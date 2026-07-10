# Time-series diagnostics for the graybox tank model, following Madsen
# "Time Series Analysis": pre-whitened cross-correlation (secs. 6.2.2,
# 9.6.1) to find the real lag structure between inputs and states, then
# residual analysis (sec. 6.6) on the fitted grey-box model's one-step
# innovations to see whether that lag structure is still missing.

source("scripts/graybox.R")

# ---- helper: AR-prewhitened lead-lag correlation --------------------------
# Fits an AR(p) filter to x (p chosen by AIC), applies the same filter to y,
# then reports cor(x[t], y[t+lag]) for lag = -max.lag..max.lag on the
# filtered series. lag > 0 means x leads y (x today predicts y in the
# future) -- i.e. "x causes y with a delay of `lag` samples".
prewhitened_leadlag <- function(x, y, max.lag=40, order.max=20) {
    ar.fit <- ar(x, order.max=order.max, aic=TRUE, method="yule-walker")
    p <- ar.fit$order
    n <- length(x)

    ar_filter <- function(z) {
        if (p == 0) return(z)
        out <- rep(NA_real_, n)
        for (t in (p+1):n) out[t] <- z[t] - sum(ar.fit$ar * z[(t-1):(t-p)])
        out
    }
    wx <- ar_filter(x)   # whitened input (~ white noise if AR fit is good)
    wy <- ar_filter(y)   # same filter applied to the output

    lags <- -max.lag:max.lag
    r <- sapply(lags, function(L) {
        if (L >= 0) {
            xx <- wx[1:(n-L)]; yy <- wy[(1+L):n]
        } else {
            xx <- wx[(1-L):n]; yy <- wy[1:(n+L)]
        }
        cor(xx, yy, use="complete.obs")
    })
    list(order=p, lag=lags, r=r, n.eff=sum(!is.na(wx) & !is.na(wy)))
}

report_leadlag <- function(name.x, name.y, res, dt=300) {
    ci <- 1.96/sqrt(res$n.eff)
    sig <- which(abs(res$r) > ci)
    cat(sprintf("\n--- %s -> %s  (AR(%d) prewhitening, 95%% CI = +-%.3f) ---\n",
                name.x, name.y, res$order, ci))
    if (length(sig) == 0) {
        cat("  no significant lags\n")
    } else {
        ord <- sig[order(-abs(res$r[sig]))][1:min(8, length(sig))]
        for (i in ord) {
            cat(sprintf("  lag %+5d samples (%+7.1f min): r = %+.3f\n",
                        res$lag[i], res$lag[i]*dt/60, res$r[i]))
        }
    }
    peak <- which.max(abs(res$r))
    cat(sprintf("  peak |r| at lag %+d samples (%+.1f min): r = %+.3f\n",
                res$lag[peak], res$lag[peak]*dt/60, res$r[peak]))
}

cat("\n############################################\n")
cat("# Step 2-3: pre-whitened input -> state CCF #\n")
cat("############################################\n")

res_pel_top <- prewhitened_leadlag(Pel, Ttop.meas)
report_leadlag("Pel", "Ttop", res_pel_top)

res_pel_bot <- prewhitened_leadlag(Pel, Tbot.meas)
report_leadlag("Pel", "Tbot", res_pel_bot)

res_qdist_top <- prewhitened_leadlag(Qdist, Ttop.meas)
report_leadlag("Qdist", "Ttop", res_qdist_top)

res_qdist_bot <- prewhitened_leadlag(Qdist, Tbot.meas)
report_leadlag("Qdist", "Tbot", res_qdist_bot)

cat("\n########################################\n")
cat("# Step 4: residual analysis of the fit #\n")
cat("########################################\n")

resid.y1 <- fit$residuals$normalized[,"y1"]   # Ttop innovations
resid.y2 <- fit$residuals$normalized[,"y2"]   # Tbot innovations

cat(sprintf("\nresidual sd  y1(Ttop)=%.3f  y2(Tbot)=%.3f  (should be ~1 if well-specified)\n",
            sd(resid.y1, na.rm=TRUE), sd(resid.y2, na.rm=TRUE)))

acf.y1 <- acf(resid.y1, lag.max=40, plot=FALSE, na.action=na.pass)
acf.y2 <- acf(resid.y2, lag.max=40, plot=FALSE, na.action=na.pass)
ci.acf <- 1.96/sqrt(sum(!is.na(resid.y1)))
cat(sprintf("\nACF(Ttop residuals) lags with |acf| > %.3f (should be none if white):\n", ci.acf))
sig1 <- which(abs(acf.y1$acf[-1]) > ci.acf)
if (length(sig1)==0) cat("  none\n") else print(data.frame(lag=sig1, acf=acf.y1$acf[-1][sig1]))

cat(sprintf("\nACF(Tbot residuals) lags with |acf| > %.3f (should be none if white):\n", ci.acf))
sig2 <- which(abs(acf.y2$acf[-1]) > ci.acf)
if (length(sig2)==0) cat("  none\n") else print(data.frame(lag=sig2, acf=acf.y2$acf[-1][sig2]))

cat("\n--- residual vs. (prewhitened) Pel cross-correlation ---\n")
res_resid1_pel <- prewhitened_leadlag(Pel, resid.y1)
report_leadlag("Pel", "Ttop residual", res_resid1_pel)
res_resid2_pel <- prewhitened_leadlag(Pel, resid.y2)
report_leadlag("Pel", "Tbot residual", res_resid2_pel)

# --- save plots for visual inspection ---
pdf("scripts/diagnostics_plots.pdf", width=8, height=10)
par(mfrow=c(3,2))
plot(res_pel_top$lag*300/60, res_pel_top$r, type="h", xlab="lag [min]", ylab="r",
     main="Pel -> Ttop (prewhitened)")
abline(h=c(-1,1)*1.96/sqrt(res_pel_top$n.eff), lty=2, col="red")
plot(res_pel_bot$lag*300/60, res_pel_bot$r, type="h", xlab="lag [min]", ylab="r",
     main="Pel -> Tbot (prewhitened)")
abline(h=c(-1,1)*1.96/sqrt(res_pel_bot$n.eff), lty=2, col="red")
plot(res_qdist_top$lag*300/60, res_qdist_top$r, type="h", xlab="lag [min]", ylab="r",
     main="Qdist -> Ttop (prewhitened)")
abline(h=c(-1,1)*1.96/sqrt(res_qdist_top$n.eff), lty=2, col="red")
plot(res_qdist_bot$lag*300/60, res_qdist_bot$r, type="h", xlab="lag [min]", ylab="r",
     main="Qdist -> Tbot (prewhitened)")
abline(h=c(-1,1)*1.96/sqrt(res_qdist_bot$n.eff), lty=2, col="red")
acf(resid.y1, lag.max=40, main="ACF: Ttop residuals", na.action=na.pass)
acf(resid.y2, lag.max=40, main="ACF: Tbot residuals", na.action=na.pass)
dev.off()

cat("\nPlots saved to scripts/diagnostics_plots.pdf\n")
