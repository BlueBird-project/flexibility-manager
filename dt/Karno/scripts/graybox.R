library(ctsmTMB)
library(nanoparquet)

model <- ctsmTMB$new()

# State : [Ttop, Tbot]

model$addSystem(
    dTtop ~ (1/Ctop)*(a*Pel + Q_gshp*delta - Qdist - k*(Ttop - Tbot) - UA*(Ttop - Tamb))*dt + sigma_Ttop*dw,
    dTbot ~ (1/Cbot)*(gamma*(Tdist_ret - Tbot)     + k*(Ttop - Tbot) - UA*(Tbot - Tamb))*dt + sigma_Tbot*dw
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

# model$setParameter(
#     Ctop       = c(initial=1250*4184, lower=700.0*4184 ,upper=4000*4184),
#     k          = c(initial=50.0, lower=0.0, upper=1000.0) ,
#     UA         = c(initial=40.0, lower=0.0 , upper=500.0),
#     sigma_Ttop = c(initial=0.1, lower=1e-4, upper=10.0),
#     sigma_Tbot = c(initial=0.1, lower=1e-4, upper=10.0),
#     sigma_y1   = c(initial=0.1, lower=1e-3, upper=0.5),
#     sigma_y2   = c(initial=0.1, lower=1e-3, upper=0.5),
#     a          = c(initial=3.0, lower=2.0, upper=4.5),
#     gamma      = c(initial=1.0, lower=0.0, upper=10.0),
#     Q_gshp     = c(initial=41100, lower=41100, upper=41100)
# )

model$setParameter(
    Ctop       = c(initial=1250*4184, lower=700.0*4184 ,upper=4000*4184),
    k          = c(initial=50.0, lower=0.0, upper=100000.0) ,
    UA         = c(initial=0.0, lower=0.0 , upper=0.0),
    sigma_Ttop = c(initial=0.1, lower=1e-4, upper=10.0),
    sigma_Tbot = c(initial=0.1, lower=1e-4, upper=10.0),
    sigma_y1   = c(initial=0.1, lower=1e-3, upper=0.5),
    sigma_y2   = c(initial=0.1, lower=1e-3, upper=0.5),
    a          = c(initial=3.0, lower=2.0, upper=4.5),
    gamma      = c(initial=1.0, lower=0.0, upper=10.0),
    Q_gshp     = c(initial=41100, lower=41100, upper=41100)
)
#30,70,70

# Kalman filter initial state 
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
    Ctop       = 1500*4184,
    k          = 120.0,
    UA         = 40.0,   # heat loss to ambient -- required to keep an 11-day open-loop
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

# initial state is always taken at t[1], regardless of where the first
# valid (non-NA) observation falls -- fall back to the fully-interpolated
# series (no gap.limit) so this is defined even if the log starts mid-gap
Ttop.full <- fill_na(raw$T_top[window])
Tbot.full <- fill_na(raw$T_bot[window])
x0.init   <- c(Ttop.full[1], Tbot.full[1])

fit <- model$estimate(
    df.fit,
    method        = "lkf",
    initial.state = list(x0=x0.init, p0=0.1 * diag(2)),
    use.hessian   = TRUE,
    control       = list(trace=1, iter.max=1e4, eval.max=1e4)
)

print(summary(fit))

# filtered state trajectories vs. measured data
pdf("figures/graybox_filtered_states.pdf", width=8, height=5)
for (i in 1:2) plot(fit, type="states", state.type="prior", print.plot=i)
dev.off()

# --- Open-loop simulation with the fitted parameters ----------------------
# Unlike the Kalman-filtered trajectory above (which is re-corrected towards
# the measurements at every step, so it tracks them closely regardless of
# parameter quality), this drives the model purely from the real inputs and
# the initial state -- a genuine visual check of how well the fitted model
# reproduces the measured dynamics on its own.
fitted.pars <- model$getParameters(value="estimate")

sim.fit <- model$simulate(
    data                = df.fit,
    pars                = fitted.pars,
    method              = "lkf",
    simulation.timestep = 300.0,
    n.sims              = 1,
    cpp.seeds           = c(20,20),
    initial.state       = list(x0=x0.init, p0=0.1 * diag(2))
)

Ttop.sim <- sim.fit$observations$y1$i0
Tbot.sim <- sim.fit$observations$y2$i0

pdf("figures/graybox_fit_vs_measured.pdf", width=8, height=6)
par(mfrow=c(2,1), mar=c(2,4,2,1), oma=c(3,0,2,0))

plot(t.hours, df.fit$y1, type="l", col="gray50", lwd=1.5,
     xlab="", ylab="Ttop [C]", main="Fitted vs. measured Ttop")
lines(fit$states$mean$prior[,"t"]/3600, fit$states$mean$prior[,"Ttop"], col="firebrick", lwd=1.5)
lines(t.hours, Ttop.sim, col="darkorange", lwd=1.5, lty=2)
legend("topright", legend=c("measured", "filtered", "simulated (fitted, open-loop)"),
       col=c("gray50", "firebrick", "darkorange"), lty=c(1,1,2), lwd=1.5, bty="n", cex=0.8)

plot(t.hours, df.fit$y2, type="l", col="gray50", lwd=1.5,
     xlab="", ylab="Tbot [C]", main="Fitted vs. measured Tbot")
lines(fit$states$mean$prior[,"t"]/3600, fit$states$mean$prior[,"Tbot"], col="steelblue", lwd=1.5)
lines(t.hours, Tbot.sim, col="darkorange", lwd=1.5, lty=2)
legend("topright", legend=c("measured", "filtered", "simulated (fitted, open-loop)"),
       col=c("gray50", "steelblue", "darkorange"), lty=c(1,1,2), lwd=1.5, bty="n", cex=0.8)

mtext("Time [h]", side=1, outer=TRUE, line=1)
par(mfrow=c(1,1))
dev.off()

