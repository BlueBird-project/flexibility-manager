library(ctsmTMB)
library(nanoparquet)

model <- ctsmTMB$new()

# State : [Ttop, Tbot]

# Ctop is fit in MJ/K (not J/K) and k, UA in kW/K (not W/K) purely to keep the
# free parameters within a few orders of magnitude of each other (~0.1-20).
# nlminb (used internally by ctsmTMB) has no automatic parameter scaling, and
# with Ctop ~ 1e6-1e7, k allowed up to 1e5, UA ~ 1e2, and sigmas ~ 1e-4-10 all
# in the same vector, its default step-size/convergence heuristics are wildly
# ill-conditioned: gradient components differ by ~10 orders of magnitude, so
# "relative convergence" triggers while some components are still enormous,
# and the optimizer walks off to whatever corner it was in when that happened
# -- reproduced directly: an un-rescaled fit here lands with Ctop and k still
# rising, a/gamma/several sigmas pinned at their box bounds, and a reported
# max gradient component of ~5.7e3 at "convergence". Rescaling collapses that
# span to ~2 orders of magnitude and removes this failure mode.
model$addSystem(
    dTtop ~ (1/(Ctop*1e6))*(a*Pel + Q_gshp*delta - Qdist - (k*1e3)*(Ttop - Tbot) - (UA*1e3)*(Ttop - Tamb))*dt + sigma_Ttop*dw,
    dTbot ~ (1/(Cbot*1e6))*(gamma*(Tdist_ret - Tbot)     + (k*1e3)*(Ttop - Tbot) - (UA*1e3)*(Tbot - Tamb))*dt + sigma_Tbot*dw
)
# Qgshp is hardcoded at the datasheet nameplate value (41100 W), not estimated:
# the GSHP (delta) is only ON 0.5% of the time in the fitting window (5
# transitions in 11 days), so there is no real information to identify it from.

model$addObs(
    y1 ~ Ttop,
    y2 ~ Tbot
)

model$setVariance(
    y1 ~ sigma_y1^2,
    y2 ~ sigma_y2^2
)

model$addInput(
    Pel,
    delta,
    Qdist,
    Tamb,
    Tdist_ret
)

# Cbot is not separately identifiable from Ctop in this data (both pin at
# whatever upper bound is given, NA std. errors -- singular Hessian), while
# the fitted coupling k comes out very high (near-total mixing) despite the
# real Ttop/Tbot correlation being weak (~0.33). Alias Cbot to Ctop instead
# of estimating a second, unconstrained capacity.
model$setAlgebraics(
    Cbot ~ Ctop
)

# Ctop in MJ/K, k and UA in kW/K (see comment above addSystem()).
#
# UA had been fixed to 0 (lower=upper=0), i.e. no ambient-loss channel at
# all. That's not a fixed nuisance parameter, it's a missing physical term:
# any net heating/cooling of Ttop that Pel/Qdist/Q_gshp don't explain then has
# nowhere to go except k*(Ttop-Tbot), which is very likely why k was being
# driven towards its (also too generous) upper bound. Restored as a free,
# bounded parameter; k's ceiling brought back down from 100 (=100000 W/K,
# essentially unconstrained/unphysical) to 2 (=2000 W/K) -- a finite-difference
# OLS regression of dTtop/dt on Pel, (Ttop-Tbot), (Ttop-Tamb), Qdist over the
# real fitting window puts k around ~0.4-0.5 (400-500 W/K), so 2 still leaves
# generous headroom without letting it run away.
model$setParameter(
    Ctop       = c(initial=1250*4184/1e6, lower=700.0*4184/1e6 ,upper=4000*4184/1e6),
    k          = c(initial=0.05, lower=0.0, upper=2.0) ,
    UA         = c(initial=0.04, lower=0.0 , upper=0.5),
    sigma_Ttop = c(initial=0.1, lower=1e-4, upper=0.1),
    sigma_Tbot = c(initial=0.1, lower=1e-4, upper=0.1),
    sigma_y1   = c(initial=0.1, lower=0.1, upper=0.1),
    sigma_y2   = c(initial=0.1, lower=0.1, upper=0.1),
    a          = c(initial=2.0, lower=1.0, upper=5.0),
    gamma      = c(initial=1.0, lower=0.0, upper=10.0),
    Q_gshp     = c(initial=41100, lower=41100, upper=41100)
)

# Note about the value of the process noise :
# sigma_x = √Δt*sigma . 
# So if we consider Δt = 300s and we consider 1 C error on 5 minutes increase
# then sigma = 1 C / √Δt = 1 / √300 = 0.04 
# In other word the upper limit 0.1 allows for sqrt(300) * 0.1 = 1.73 C of standart deviation in the discrete model 


#30,70,70

# Kalman filter initial state 
# Todo : Replace by the actual measurement 
initial.state <- list(x0=c(40.0, 30.0), p0=0.1 * diag(2))
model$setInitialState(initial.state)

# print(model)

# Load real logged input data (state measurements T_top/T_bot are intentionally
# NOT used here -- states are generated step-by-step by our own model instead)
r.seed <- 20
set.seed(r.seed)

raw <- read_parquet("data/karno-410708_raw_k0001.parquet")

# Native sampling is 5 min = 300 s, but a handful of samples are missing from
# the log (gaps up to 55 min). ctsmTMB's exact ("lkf") discretization needs a
# strictly regular time grid, so we re-index onto one and fill the resulting
# gaps rather than relying on the raw (irregular) row index.
t0 <- as.POSIXct("2026-05-13", tz="UTC")
t1 <- as.POSIXct("2026-05-25", tz="UTC")
grid <- seq(t0, t1 - 300, by=300)
window <- match(grid, raw$UTC_DateTime)   # NA where a 5-min sample is missing

# fill isolated NA gaps (missing samples, sensor dropouts) by linear
# interpolation, keeping the grid uniform. Inputs must be defined at every
# grid point (the exact/lkf discretization integrates against them), so
# they are always fully filled regardless of gap length.
fill_na <- function(x) {
    idx <- seq_along(x)
    ok  <- !is.na(x)
    approx(idx[ok], x[ok], xout=idx, rule=2)$y
}

# Observation columns (Ttop/Tbot), by contrast, are allowed to carry NA
# into ctsmTMB$estimate() -- the Kalman filter just runs open-loop (no
# measurement update) across a run of NAs, which is how ctsmTMB fits a
# single call across multiple disjoint data portions. So here we only
# bridge short dropouts by interpolation and leave gaps longer than
# `gap.limit` samples as NA, splitting the fit at those points instead of
# papering over them.
gap.limit <- 4L   # 4 * 300s = 20 min

fill_obs <- function(x, limit=gap.limit) {
    idx    <- seq_along(x)
    ok     <- !is.na(x)
    filled <- approx(idx[ok], x[ok], xout=idx, rule=2)$y
    run    <- rle(is.na(x))
    long.na <- rep(run$values & run$lengths > limit, run$lengths)
    filled[long.na] <- NA
    filled
}

Pel       <- fill_na(raw$`P_el,HP,air`[window])          # W
delta     <- fill_na(as.numeric(raw$`z_HP,geo`[window]))  # ON/OFF -> 0/1
Qdist     <- fill_na(raw$`Q̇_dist`[window]) * 1000         # kW -> W
Tamb      <- fill_na(raw$T_air[window])                   # degC
Tdist_ret <- fill_na(raw$`T_dist,ret`[window])        # degC

t.seq <- as.numeric(difftime(grid, grid[1], units="secs"))

df.obs <- data.frame(
    t         = t.seq,
    Pel       = Pel,
    delta     = delta,
    Qdist     = Qdist,
    Tamb      = Tamb,
    Tdist_ret = Tdist_ret,
    y1        = NA,   # placeholder: filled in by simulate() below (states generated by the model)
    y2        = NA
)

# "True" system, deliberately different from the setParameter initial guesses,
# used only to generate synthetic data for a non-trivial recovery test
true.pars <- c(
    Ctop       = 1250*4184/1e6,
    k          = 120.0/1e3,
    UA         = 40.0/1e3,   # heat loss to ambient -- required to keep an 11-day open-loop
                          # simulation bounded; Pel/delta are real historical signals
                          # unresponsive to the simulated tank state, so without a loss
                          # term any sustained input/output imbalance runs away
    sigma_Ttop = 0.03,
    sigma_Tbot = 0.03,
    sigma_y1   = 0.05,
    sigma_y2   = 0.05,
    a          = 3.0
)

cpp.seeds <- c(20,20)
sim <- model$simulate(data=df.obs,
                      pars=true.pars,
                      method="lkf",
                      simulation.timestep=300.0,
                      n.sims=1,
                      cpp.seeds=cpp.seeds
                     )

df.obs$y1 <- sim$observations$y1$i0
df.obs$y2 <- sim$observations$y2$i0


# --- Plot: measured states (top) + each input as its own subplot below ---
t.hours <- df.obs$t / 3600

pdf("figures/graybox_synthetic_data.pdf", width=8, height=10)
par(mfrow=c(5,1), mar=c(2,4,2,1), oma=c(3,0,2,0))

plot(t.hours, df.obs$y1, type="l", col="firebrick", lwd=1.5,
     xlab="", ylab="Temp [C]", main="Measured states: Ttop (y1) / Tbot (y2)")
lines(t.hours, df.obs$y2, col="steelblue", lwd=1.5)
legend("topright", legend=c("Ttop (y1)", "Tbot (y2)"),
       col=c("firebrick", "steelblue"), lty=1, lwd=1.5, bty="n", cex=0.8)

plot(t.hours, df.obs$Pel, type="l", col="darkorange", lwd=1.5,
     xlab="", ylab="Pel [W]", main="Electric heater input")

plot(t.hours, df.obs$delta, type="l", col="forestgreen", lwd=1.5,
     xlab="", ylab="delta [-]", main="GSHP on/off switch", ylim=c(-0.1,1.1))

plot(t.hours, df.obs$Qdist, type="l", col="purple", lwd=1.5,
     xlab="", ylab="Qdist [W]", main="District heat draw (disturbance)")

plot(t.hours, df.obs$Tamb, type="l", col="gray30", lwd=1.5,
     xlab="", ylab="Tamb [C]", main="Ambient temperature (disturbance)")

mtext("Time [h]", side=1, outer=TRUE, line=1)
par(mfrow=c(1,1))
dev.off()


# --- Fit model to the real measured Ttop / Tbot --------------------------
Ttop.meas <- fill_obs(raw$T_top[window])   # degC, NA over gaps > gap.limit
Tbot.meas <- fill_obs(raw$T_bot[window])   # degC, NA over gaps > gap.limit

# Manually excise an unreliable stretch of the log (e.g. a known sensor
# fault / maintenance window). Marking it NA -- rather than dropping rows --
# keeps the time grid regular while forcing an extra split in the fitted
# portions, exactly like a long dropout would.
excl.start <- as.POSIXct("2026-05-20 07:00:00", tz="UTC")
excl.end   <- as.POSIXct("2026-05-20 11:00:00", tz="UTC")
excl       <- grid >= excl.start & grid < excl.end
Ttop.meas[excl] <- NA
Tbot.meas[excl] <- NA

df.fit <- df.obs
df.fit$y1 <- Ttop.meas
df.fit$y2 <- Tbot.meas

# Report the contiguous data portions ctsmTMB will fit across in this one
# estimate() call (runs of non-NA observations, separated by the long
# dropouts and the manual exclusion window above).
valid <- !is.na(Ttop.meas) & !is.na(Tbot.meas)
run   <- rle(valid)
run.end   <- cumsum(run$lengths)
run.start <- run.end - run$lengths + 1
segments  <- data.frame(start=run.start, end=run.end)[run$values, ]
cat(sprintf("Fitting across %d data portion(s):\n", nrow(segments)))
for (i in seq_len(nrow(segments))) {
    cat(sprintf("  [%d] %s -> %s\n", i,
                format(grid[segments$start[i]]), format(grid[segments$end[i]])))
}

# --- Fit each data portion as an INDEPENDENT dataset (CTSM mathguide,
# sec. 1.1.2.3 "Using multiple independent data sets", docs/mathguide.pdf
# p.5) instead of one continuous Kalman filter recursion that coasts
# open-loop (no measurement update, but state still propagated by A/B)
# through the gaps between segments.
#
# ctsmTMB (the TMB rewrite we use here, not the older Matlab/R CTSM the
# mathguide documents) does not implement this feature natively -- there is
# no dataset-index / "reinitialize here" argument anywhere in its
# estimation code (checked the package source directly). What it DOES
# expose is model$likelihood(data, ...), which builds and returns the raw
# TMB negative-log-likelihood function handles (fn/gr/he) for ONE dataset,
# without running any optimizer on it. Because the joint (negative log)
# likelihood of independent datasets is just the SUM of the individual
# ones (mathguide eq. 1.23, or eq. 1.26 in its logarithmic form, with a
# flat/uniform prior on theta -- the ML special case of that MAP formula),
# we can build one such handle per segment -- each with its OWN initial
# condition read directly off the data -- and hand nlminb the *summed*
# fn/gr/he ourselves. That reproduces the mathguide formula exactly, using
# only ctsmTMB's public API, with no state or covariance carried across a
# segment boundary.
n.seg <- nrow(segments)

# Per-segment data frames + data-derived initial conditions.
# t is reset to start at 0 within each segment: ctsmTMB derives its
# internal ODE step-size from diff(data$t), and to the filter each segment
# IS a standalone dataset, so there's no reason to carry absolute clock
# time into it.
seg.data <- vector("list", n.seg)
seg.x0   <- vector("list", n.seg)
seg.p0   <- vector("list", n.seg)

for (i in seq_len(n.seg)) {
    idx <- segments$start[i]:segments$end[i]
    seg.data[[i]] <- data.frame(
        t         = t.seq[idx] - t.seq[idx[1]],
        Pel       = Pel[idx],
        delta     = delta[idx],
        Qdist     = Qdist[idx],
        Tamb      = Tamb[idx],
        Tdist_ret = Tdist_ret[idx],
        y1        = Ttop.meas[idx],
        y2        = Tbot.meas[idx]
    )
    # Initial condition = the actual measurement at this segment's first
    # (by construction, non-NA) sample -- not the state propagated/guessed
    # forward from the end of the previous segment.
    seg.x0[[i]] <- c(Ttop.meas[idx[1]], Tbot.meas[idx[1]])
    seg.p0[[i]] <- 0.1 * diag(2)
}

# One TMB negative-log-likelihood handle per segment. Each call to
# $likelihood() rebuilds an AD tape bound to that segment's own data and
# initial.state; the returned handles are self-contained (they don't share
# mutable state), so we can keep all n.seg of them around and sum them.
seg.nll <- lapply(seq_len(n.seg), function(i) {
    model$likelihood(
        data          = seg.data[[i]],
        method        = "lkf",
        initial.state = list(x0 = seg.x0[[i]], p0 = seg.p0[[i]]),
        silent        = TRUE
    )
})

# Joint objective/gradient/hessian = sum over the independent segments.
# Valid because NLL_total(theta) = sum_i NLL_i(theta) exactly -- no
# approximation here, just linearity of the sum in the mathguide formula.
joint.nll <- function(p) Reduce(`+`, lapply(seg.nll, function(h) h$fn(p)))
joint.gr  <- function(p) Reduce(`+`, lapply(seg.nll, function(h) h$gr(p)))
joint.he  <- function(p) Reduce(`+`, lapply(seg.nll, function(h) h$he(p)))

free.init  <- model$getParameters(type = "free", value = "initial")
free.lower <- model$getParameters(type = "free", value = "lower")
free.upper <- model$getParameters(type = "free", value = "upper")

# This is a black box nonlinear solver. Check the documentation 
opt <- nlminb(
    start     = free.init,
    objective = joint.nll,
    gradient  = joint.gr,
    hessian   = joint.he,
    lower     = free.lower,
    upper     = free.upper,
    control   = list(trace = 1, iter.max = 1e4, eval.max = 1e4)
)
names(opt$par) <- names(free.init)

cat(sprintf("\nJoint multi-segment fit: %s\n", opt$message))
cat(sprintf("Negative log-likelihood at optimum: %.4f (summed over %d segments)\n",
            opt$objective, n.seg))

# Fold the optimum back into a full parameter vector: fixed parameters keep
# their fixed value, free ones take the fitted value. Same shape as
# model$getParameters(value="estimate") after model$estimate(), just built
# by hand since we bypassed that method.
fitted.pars <- model$getParameters(value = "initial")
fitted.pars[names(opt$par)] <- opt$par

# Standard errors / Wald z / p-values from the summed Hessian at the
# optimum (valid for the same reason joint.he is valid: it's an exact sum).
# Hand-rolled stand-in for summary(fit), since that needs a model$estimate()
# fit object we don't have when driving nlminb ourselves.
se   <- sqrt(diag(solve(joint.he(opt$par))))
zval <- opt$par / se
pval <- 2 * pnorm(-abs(zval))
print(data.frame(estimate = opt$par, std.error = se, z.value = zval, p.value = pval))

# --- Filtered state trajectories vs. measured data, per segment ----------
# Each segment is filtered independently (its own x0/p0) with the fitted
# parameters; results are stitched onto the absolute time axis afterwards
# purely for plotting -- the filter recursion itself never crosses a
# segment boundary.
seg.filt <- lapply(seq_len(n.seg), function(i) {
    model$filter(
        data          = seg.data[[i]],
        pars          = fitted.pars,
        method        = "lkf",
        initial.state = list(x0 = seg.x0[[i]], p0 = seg.p0[[i]]),
        silent        = TRUE
    )
})

filtered.prior <- do.call(rbind, lapply(seq_len(n.seg), function(i) {
    m <- seg.filt[[i]]$states$mean$prior
    m[, "t"] <- m[, "t"] + t.seq[segments$start[i]]   # segment-local -> absolute time
    m
}))

pdf("figures/graybox_filtered_states.pdf", width=8, height=5)
par(mfrow=c(2,1), mar=c(2,4,2,1), oma=c(3,0,2,0))
plot(filtered.prior[,"t"]/3600, filtered.prior[,"Ttop"], type="l", col="firebrick",
     xlab="", ylab="Ttop [C]", main="Filtered Ttop (independent per-segment fit)")
points(t.hours, Ttop.meas, pch=".", col="gray40")
plot(filtered.prior[,"t"]/3600, filtered.prior[,"Tbot"], type="l", col="steelblue",
     xlab="", ylab="Tbot [C]", main="Filtered Tbot (independent per-segment fit)")
points(t.hours, Tbot.meas, pch=".", col="gray40")
mtext("Time [h]", side=1, outer=TRUE, line=1)
par(mfrow=c(1,1))
dev.off()

# --- Open-loop simulation with the fitted parameters, per segment --------
# Unlike the Kalman-filtered trajectory above (which is re-corrected towards
# the measurements at every step, so it tracks them closely regardless of
# parameter quality), this drives the model purely from the real inputs and
# an initial state -- a genuine visual check of how well the fitted model
# reproduces the measured dynamics on its own. Same principle as the fit
# and the filter above: each segment is simulated open-loop from its OWN
# data-derived x0, rather than one continuous simulation drifting across
# the gaps on possibly-stale state.
seg.sim <- lapply(seq_len(n.seg), function(i) {
    model$simulate(
        data                = seg.data[[i]],
        pars                = fitted.pars,
        method              = "lkf",
        simulation.timestep = 300.0,
        n.sims              = 1,
        cpp.seeds           = c(20, 20),
        initial.state       = list(x0 = seg.x0[[i]], p0 = seg.p0[[i]])
    )
})

# Full-grid vectors, NA outside the fitted segments (those stretches were
# never simulated -- there is no meaningful open-loop trajectory to show
# across a manually-excised or too-long dropout).
Ttop.sim <- rep(NA_real_, length(grid))
Tbot.sim <- rep(NA_real_, length(grid))
for (i in seq_len(n.seg)) {
    idx <- segments$start[i]:segments$end[i]
    Ttop.sim[idx] <- seg.sim[[i]]$observations$y1$i0
    Tbot.sim[idx] <- seg.sim[[i]]$observations$y2$i0
}

pdf("figures/graybox_fit_vs_measured.pdf", width=8, height=6)
par(mfrow=c(2,1), mar=c(2,4,2,1), oma=c(3,0,2,0))

# --- Top panel: Ttop ---
ylim1 <- range(c(df.fit$y1,
                  filtered.prior[,"Ttop"],
                  Ttop.sim), na.rm=TRUE)

plot(t.hours, df.fit$y1, type="l", col="gray50", lwd=1.5,
     xlab="", ylab="Ttop [C]", main="Fitted vs. measured Ttop",
     ylim=ylim1)
lines(filtered.prior[,"t"]/3600, filtered.prior[,"Ttop"], col="firebrick", lwd=1.5)
lines(t.hours, Ttop.sim, col="darkorange", lwd=1.5, lty=1)
legend("topright", legend=c("measured", "filtered", "simulated (fitted, open-loop)"),
       col=c("gray50", "firebrick", "darkorange"), lty=c(1,1,1), lwd=1.5, bty="n", cex=0.8)

# --- Bottom panel: Tbot ---
ylim2 <- range(c(df.fit$y2,
                  filtered.prior[,"Tbot"],
                  Tbot.sim), na.rm=TRUE)

plot(t.hours, df.fit$y2, type="l", col="gray50", lwd=1.5,
     xlab="", ylab="Tbot [C]", main="Fitted vs. measured Tbot",
     ylim=ylim2)
lines(filtered.prior[,"t"]/3600, filtered.prior[,"Tbot"], col="steelblue", lwd=1.5)
lines(t.hours, Tbot.sim, col="darkorange", lwd=1.5, lty=1)
legend("topright", legend=c("measured", "filtered", "simulated (fitted, open-loop)"),
       col=c("gray50", "steelblue", "darkorange"), lty=c(1,1,1), lwd=1.5, bty="n", cex=0.8)

mtext("Time [h]", side=1, outer=TRUE, line=1)
par(mfrow=c(1,1))
dev.off()