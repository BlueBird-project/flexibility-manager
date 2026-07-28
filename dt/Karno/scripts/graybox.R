library(ctsmTMB)
library(nanoparquet)
library(plotly)

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
#
# On top of that unit rescaling, Ctop/k/UA/sigma_Ttop/sigma_Tbot are
# additionally fit in LOG space (log_Ctop, log_k, log_UA, log_sigma_Ttop,
# log_sigma_Tbot below, mapped back to the physical quantity via exp() right
# here in the system equations). All five are physically constrained to be
# strictly positive, and their lower bound is 0 -- under a raw-space box
# constraint that 0 is a wall the optimizer can get pinned against (exactly
# what happened to sigma_Ttop: nlminb converged with it sitting on
# lower=1e-4, which then made solve() on the Hessian produce a negative
# diagonal entry -> NaN std. error, for that parameter *and* sigma_Tbot,
# since the inverse isn't block-diagonal and one degenerate direction
# poisons correlated entries too). Under theta = exp(phi), theta > 0 is
# automatic for ANY finite phi, so the lower bound becomes phi -> -Inf: an
# asymptote the optimizer approaches smoothly rather than a wall it slams
# into, which removes the boundary-pinning failure mode at its source
# instead of just excluding pinned parameters from the SE table after the
# fact. `a` stays in raw space -- it is fixed at exactly 0 (log(0) has no
# finite value to exponentiate back from).
model$addSystem(
    dTtop ~ (1/(exp(log_Ctop)*1e6))*(a*Pel + Q_gshp*delta - Qdist - (exp(log_k)*1e3)*(Ttop - Tbot) - (exp(log_UA)*1e3)*(Ttop - Tamb))*dt + exp(log_sigma_Ttop)*dw,
    dTbot ~ (1/(Cbot*1e6))*(Cdist*(Tdist_ret - Tbot)      + (exp(log_k)*1e3)*(Ttop - Tbot) - (exp(log_UA)*1e3)*(Tbot - Tamb))*dt + exp(log_sigma_Tbot)*dw
)
# `gamma` (heat-capacity flow rate of the district return, W/K) is no longer
# a fitted parameter -- it's measured. Qdist = Cdist*(Tdist_in - Tdist_ret)
# by definition of the heat-draw measurement, so Cdist = Qdist/dT is exact,
# and is computed as a time-varying INPUT below (Cdist), same treatment as
# Q_gshp being fixed at nameplate rather than estimated, except here it's
# genuinely time-varying rather than constant. See computation + sanity
# check near Tdist_in below: derived Cdist has median ~340 W/K, comparable
# in magnitude to k, versus the old gamma bound of [0.8, 1.2] which was off
# by ~3 orders of magnitude (missing the same *1e3-style rescale k/UA got,
# and never grounded in the actual flow measurement anyway).
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
    Tdist_ret,
    Cdist
)

# Cbot is not separately identifiable from Ctop in this data (both pin at
# whatever upper bound is given, NA std. errors -- singular Hessian), while
# the fitted coupling k comes out very high (near-total mixing) despite the
# real Ttop/Tbot correlation being weak (~0.33). Alias Cbot to Ctop instead
# of estimating a second, unconstrained capacity. exp() here maps the
# log-parameterized log_Ctop back to the physical capacity Cbot is aliased to.
model$setAlgebraics(
    Cbot ~ exp(log_Ctop)
)

# Ctop in MJ/K, k and UA in kW/K (see comment above addSystem()).
#
# UA is now genuinely free (it had been left fixed at lower=upper=0 despite
# an earlier comment here claiming it was freed -- that was a bug: the code
# never matched the comment, so the model had NO ambient-loss channel at all.
# k*(Ttop-Tbot) is a redistribution term, not a loss -- same magnitude,
# opposite sign, in both equations -- so with UA=0 the only way heat could
# leave {Ttop,Tbot} was via Qdist/Cdist, which are ~0 whenever there's no
# district flow. That left nothing to explain a multi-hour, multi-degree
# cooldown in the open-loop simulation during quiet, no-flow stretches --
# see graybox_fit_vs_measured.pdf, where the open-loop trace just sits flat.
# Upper bound of 0.1 (=100 W/K) is a generous ceiling for a ~2.5 m^2
# insulated tank (a plausible U-value of a few hundred mW/m^2K times a few
# m^2 of surface puts the real value far below this).
#
# Ctop/k/UA/sigma_Ttop/sigma_Tbot are estimated as log_* here (bounds/initial
# are log() of the physical-unit values above) and mapped back with exp()
# inside the system equations -- see the log-space comment above
# addSystem(). The lower bound in log space is -Inf: the physical lower
# bound of 0 is only ever approached asymptotically as log_* -> -Inf, never
# a box wall the optimizer can get pinned against. sigma_y1/sigma_y2/a/Q_gshp
# stay in raw space -- `a` is fixed at exactly 0 (log(0) has no finite value
# to exponentiate back from), and sigma_y1/sigma_y2/Q_gshp are fixed too, so
# log-transforming them would change nothing (nlminb never varies a fixed
# parameter regardless of its parameterization).
model$setParameter(
    log_Ctop       = c(initial=log(1250*4184/1e6), lower=log(700.0*4184/1e6), upper=log(4000*4184/1e6)),
    log_k          = c(initial=log(0.05), lower=-Inf, upper=log(2.0)),
    log_UA         = c(initial=log(0.01), lower=-Inf, upper=log(0.1)),
    log_sigma_Ttop = c(initial=log(0.1), lower=-Inf, upper=log(0.1)),
    log_sigma_Tbot = c(initial=log(0.1), lower=-Inf, upper=log(0.1)),
    sigma_y1       = c(initial=0.1, lower=0.1, upper=0.1),
    sigma_y2       = c(initial=0.1, lower=0.1, upper=0.1),
    # `a` scales Pel, which is ~0 through every quiet (both-HP-off) segment
    # used below -- it has essentially no gradient in this fit and would
    # leave the joint Hessian singular in that direction (same reason
    # Q_gshp is fixed rather than estimated). Fixed at its prior initial
    # value for now; re-estimate it separately from ASHP ON-transition
    # windows, where Pel is actually informative.
    a          = 0.0 * c(initial=2.0, lower=2.0, upper=2.0),
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
Tdist_in  <- fill_na(raw$`T_dist,in`[window])         # degC

# Heat-capacity flow rate of the district return (W/K), backed out directly
# from the heat-draw measurement instead of fitted as a free "gamma"
# parameter: Qdist = Cdist*(Tdist_in - Tdist_ret) by definition of a
# heat-power measurement, so Cdist = Qdist/dT is exact and time-varying
# (real flow isn't constant). Zeroed out during near-zero-flow stretches --
# dividing two small, noisy measurements there blows up, and physically no
# flow means no heat is carried back to Tbot regardless. On the raw log,
# excluding those stretches, this comes out to median ~340 W/K (IQR
# 227-529) -- same order of magnitude as k, and grounded in measurement
# rather than the old gamma bound of [0.8, 1.2] (off by ~3 orders of
# magnitude, and never tied to an actual flow number).
dT_dist <- Tdist_in - Tdist_ret
Cdist   <- ifelse(abs(dT_dist) > 0.5 & Qdist > 50, Qdist / dT_dist, 0)

# Raw (un-interpolated) boolean ON/OFF status for each heat pump -- distinct
# from `Pel`/`delta` above, which are fill_na'd continuous model inputs. Used
# below to select "quiet" periods where BOTH heat pumps are off, so k/UA/Ctop
# can be identified from pure tank relaxation, without the ASHP's feedback
# loop (Pel driven by Ttop crossing 32/35C) confounding them with `a`. NA
# (missing raw sample) is left as NA -- not coerced to "off" -- so a gap of
# unknown HP state excludes that stretch rather than silently assuming quiet.
z.air  <- raw$`z_HP,air`[window]
z.geo  <- raw$`z_HP,geo`[window]
hp.off <- !z.air & !z.geo
hp.off[is.na(hp.off)] <- FALSE   # unknown HP state -> not confirmed quiet, excluded below

t.seq <- as.numeric(difftime(grid, grid[1], units="secs"))

df.obs <- data.frame(
    t         = t.seq,
    Pel       = Pel,
    delta     = delta,
    Qdist     = Qdist,
    Tamb      = Tamb,
    Tdist_ret = Tdist_ret,
    Cdist     = Cdist,
    y1        = NA,   # placeholder: filled in by simulate() below (states generated by the model)
    y2        = NA
)

# "True" system, deliberately different from the setParameter initial guesses,
# used only to generate synthetic data for a non-trivial recovery test.
# Names/values here must match model's actual (log-space) parameter names --
# log_Ctop/log_k/log_UA/log_sigma_Ttop/log_sigma_Tbot are the log() of the
# physical quantities being simulated, exponentiated back inside the system
# equations.
true.pars <- c(
    log_Ctop       = log(1250*4184/1e6),
    log_k          = log(120.0/1e3),
    log_UA         = log(40.0/1e3),   # heat loss to ambient -- required to keep an 11-day open-loop
                          # simulation bounded; Pel/delta are real historical signals
                          # unresponsive to the simulated tank state, so without a loss
                          # term any sustained input/output imbalance runs away
    log_sigma_Ttop = log(0.03),
    log_sigma_Tbot = log(0.03),
    sigma_y1       = 0.05,
    sigma_y2       = 0.05,
    a              = 3.0
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

# One page per calendar day instead of the full 12-day span on one page --
# same panel layout as before, just paged through (arrow keys in any PDF
# viewer) so each page has ~24h of resolution instead of ~288h squeezed in.
day.len   <- 24
n.days    <- ceiling(max(t.hours) / day.len)

pdf("figures/graybox_synthetic_data.pdf", width=8, height=10)
for (d in seq_len(n.days) - 1) {
    idx <- which(t.hours >= d * day.len & t.hours < (d + 1) * day.len)
    if (length(idx) == 0) next

    par(mfrow=c(5,1), mar=c(2,4,2,1), oma=c(3,0,2,0))

    plot(t.hours[idx], df.obs$y1[idx], type="l", col="firebrick", lwd=1.5,
         xlab="", ylab="Temp [C]",
         main=sprintf("Measured states: Ttop (y1) / Tbot (y2) -- day %d", d + 1))
    lines(t.hours[idx], df.obs$y2[idx], col="steelblue", lwd=1.5)
    legend("topright", legend=c("Ttop (y1)", "Tbot (y2)"),
           col=c("firebrick", "steelblue"), lty=1, lwd=1.5, bty="n", cex=0.8)

    plot(t.hours[idx], df.obs$Pel[idx], type="l", col="darkorange", lwd=1.5,
         xlab="", ylab="Pel [W]", main="Electric heater input")

    plot(t.hours[idx], df.obs$delta[idx], type="l", col="forestgreen", lwd=1.5,
         xlab="", ylab="delta [-]", main="GSHP on/off switch", ylim=c(-0.1,1.1))

    plot(t.hours[idx], df.obs$Qdist[idx], type="l", col="purple", lwd=1.5,
         xlab="", ylab="Qdist [W]", main="District heat draw (disturbance)")

    plot(t.hours[idx], df.obs$Tamb[idx], type="l", col="gray30", lwd=1.5,
         xlab="", ylab="Tamb [C]", main="Ambient temperature (disturbance)")

    mtext("Time [h]", side=1, outer=TRUE, line=1)
    par(mfrow=c(1,1))
}
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
# estimate() call: runs of non-NA observations, separated by the long
# dropouts and the manual exclusion window above, AND now additionally split
# wherever either heat pump is on (hp.off above) -- so every segment handed
# to the optimizer is a genuine "quiet" (both HPs off) stretch, free of the
# ASHP feedback loop.
valid <- !is.na(Ttop.meas) & !is.na(Tbot.meas) & hp.off
run   <- rle(valid)
run.end   <- cumsum(run$lengths)
run.start <- run.end - run$lengths + 1
segments  <- data.frame(start=run.start, end=run.end)[run$values, ]

# HP-off runs are frequently just a few samples long (a brief tick between
# cycles). ctsmTMB internally checks var(diff(diff(data$t))) to decide if
# the timestep is constant -- that's a DOUBLE diff, so it needs >= 4 rows
# (n=3 gives a single-element diff(diff(t)), and var() of one value is NA,
# not 0, which crashes construct_makeADFun with a bare "missing value where
# TRUE/FALSE needed" -- confirmed by reproducing it directly against
# set_ode_timestep()/makeADFun_lkf_rtmb()). Below 4 points there's also
# essentially no decay information anyway.
min.seg.len <- 4L
segments <- segments[(segments$end - segments$start + 1) >= min.seg.len, ]

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
        Cdist     = Cdist[idx],
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

cat(opt$message, "\n")
print(rbind(par = opt$par, lower = free.lower, upper = free.upper))

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
#
# `a`/`Q_gshp` here are lower==upper ("fixed") but ctsmTMB still carries them
# in the free-parameter vector as zero-width box constraints (it only drops
# a parameter from that vector when both bounds are NA), so their row/column
# of the Hessian is structurally all-zero -- solve() on the full matrix is
# then singular by construction, regardless of how well the genuinely free
# parameters are identified. Invert only the sub-Hessian over the
# non-zero-width parameters; fixed ones get NA (SE is meaningless for them).
fixed.idx <- which(free.lower == free.upper)
free.idx  <- setdiff(seq_along(opt$par), fixed.idx)
se <- rep(NA_real_, length(opt$par))
se[free.idx] <- sqrt(diag(solve(joint.he(opt$par)[free.idx, free.idx])))
zval <- opt$par / se
pval <- 2 * pnorm(-abs(zval))
print(data.frame(estimate = opt$par, std.error = se, z.value = zval, p.value = pval))

# Map the log_* rows back to physical units: theta = exp(phi). By the delta
# method Var(theta) ~= (dtheta/dphi)^2 * Var(phi) = theta^2 * Var(phi), so
# SE(theta) ~= theta_hat * SE(phi) -- exact to first order since
# dtheta/dphi = theta for a pure exp() map. z/p are omitted here: the Wald
# test above is against phi=0 (i.e. theta=1), which isn't a physically
# meaningful null for a positive quantity like a capacity or a noise sigma.
is.log.par <- grepl("^log_", names(opt$par))
phys.estimate  <- ifelse(is.log.par, exp(opt$par), opt$par)
phys.std.error <- ifelse(is.log.par, phys.estimate * se, se)
names(phys.estimate) <- sub("^log_", "", names(opt$par))
cat("\nBack-transformed (physical-unit) parameter estimates:\n")
print(data.frame(estimate = phys.estimate, std.error = phys.std.error,
                  row.names = names(phys.estimate)))

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

# One page per calendar day (see graybox_synthetic_data.pdf above for why).
filt.t.hours <- filtered.prior[, "t"] / 3600

pdf("figures/graybox_filtered_states.pdf", width=8, height=5)
for (d in seq_len(n.days) - 1) {
    filt.idx <- which(filt.t.hours >= d * day.len & filt.t.hours < (d + 1) * day.len)
    meas.idx <- which(t.hours      >= d * day.len & t.hours      < (d + 1) * day.len)
    if (length(filt.idx) == 0 && length(meas.idx) == 0) next

    par(mfrow=c(2,1), mar=c(2,4,2,1), oma=c(3,0,2,0))
    plot(filt.t.hours[filt.idx], filtered.prior[filt.idx, "Ttop"], type="l", col="firebrick",
         xlab="", ylab="Ttop [C]",
         main=sprintf("Filtered Ttop (independent per-segment fit) -- day %d", d + 1))
    points(t.hours[meas.idx], Ttop.meas[meas.idx], pch=".", col="gray40")
    plot(filt.t.hours[filt.idx], filtered.prior[filt.idx, "Tbot"], type="l", col="steelblue",
         xlab="", ylab="Tbot [C]", main="Filtered Tbot (independent per-segment fit)")
    points(t.hours[meas.idx], Tbot.meas[meas.idx], pch=".", col="gray40")
    mtext("Time [h]", side=1, outer=TRUE, line=1)
    par(mfrow=c(1,1))
}
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

# One page per calendar day (see graybox_synthetic_data.pdf above for why).
filt.t.hours <- filtered.prior[, "t"] / 3600

pdf("figures/graybox_fit_vs_measured.pdf", width=8, height=6)
for (d in seq_len(n.days) - 1) {
    day.idx  <- which(t.hours      >= d * day.len & t.hours      < (d + 1) * day.len)
    filt.idx <- which(filt.t.hours >= d * day.len & filt.t.hours < (d + 1) * day.len)
    if (length(day.idx) == 0 && length(filt.idx) == 0) next

    par(mfrow=c(2,1), mar=c(2,4,2,1), oma=c(3,0,2,0))

    # --- Top panel: Ttop ---
    ylim1 <- range(c(df.fit$y1[day.idx],
                      filtered.prior[filt.idx,"Ttop"],
                      Ttop.sim[day.idx]), na.rm=TRUE)

    plot(t.hours[day.idx], df.fit$y1[day.idx], type="l", col="gray50", lwd=1.5,
         xlab="", ylab="Ttop [C]",
         main=sprintf("Fitted vs. measured Ttop -- day %d", d + 1),
         ylim=ylim1)
    lines(filt.t.hours[filt.idx], filtered.prior[filt.idx,"Ttop"], col="firebrick", lwd=1.5)
    lines(t.hours[day.idx], Ttop.sim[day.idx], col="darkorange", lwd=1.5, lty=1)
    legend("topright", legend=c("measured", "filtered", "simulated (fitted, open-loop)"),
           col=c("gray50", "firebrick", "darkorange"), lty=c(1,1,1), lwd=1.5, bty="n", cex=0.8)

    # --- Bottom panel: Tbot ---
    ylim2 <- range(c(df.fit$y2[day.idx],
                      filtered.prior[filt.idx,"Tbot"],
                      Tbot.sim[day.idx]), na.rm=TRUE)

    plot(t.hours[day.idx], df.fit$y2[day.idx], type="l", col="gray50", lwd=1.5,
         xlab="", ylab="Tbot [C]", main="Fitted vs. measured Tbot",
         ylim=ylim2)
    lines(filt.t.hours[filt.idx], filtered.prior[filt.idx,"Tbot"], col="steelblue", lwd=1.5)
    lines(t.hours[day.idx], Tbot.sim[day.idx], col="darkorange", lwd=1.5, lty=1)
    legend("topright", legend=c("measured", "filtered", "simulated (fitted, open-loop)"),
           col=c("gray50", "steelblue", "darkorange"), lty=c(1,1,1), lwd=1.5, bty="n", cex=0.8)

    mtext("Time [h]", side=1, outer=TRUE, line=1)
    par(mfrow=c(1,1))
}
dev.off()

# --- Interactive HTML version (box-zoom/pan/hover) of the panel above ----
# Static PDF above is kept for print/archival; this is for actually
# inspecting details (zoom into a specific dropout, hover for exact
# values) -- open figures/graybox_fit_vs_measured.html in a browser.
filt.t.hours <- filtered.prior[, "t"] / 3600

p.top <- plot_ly() %>%
    add_lines(x = t.hours, y = df.fit$y1, name = "measured",
              line = list(color = "gray50", width = 1.5)) %>%
    add_lines(x = filt.t.hours, y = filtered.prior[, "Ttop"], name = "filtered",
              line = list(color = "firebrick", width = 1.5)) %>%
    add_lines(x = t.hours, y = Ttop.sim, name = "simulated (fitted, open-loop)",
              line = list(color = "darkorange", width = 1.5)) %>%
    layout(yaxis = list(title = "Ttop [C]"),
           title = list(text = "Fitted vs. measured Ttop", x = 0.02))

p.bot <- plot_ly() %>%
    add_lines(x = t.hours, y = df.fit$y2, name = "measured", legendgroup = "measured",
              showlegend = FALSE, line = list(color = "gray50", width = 1.5)) %>%
    add_lines(x = filt.t.hours, y = filtered.prior[, "Tbot"], name = "filtered", legendgroup = "filtered",
              showlegend = FALSE, line = list(color = "steelblue", width = 1.5)) %>%
    add_lines(x = t.hours, y = Tbot.sim, name = "simulated (fitted, open-loop)", legendgroup = "simulated",
              showlegend = FALSE, line = list(color = "darkorange", width = 1.5)) %>%
    layout(yaxis = list(title = "Tbot [C]"),
           xaxis = list(title = "Time [h]"),
           title = list(text = "Fitted vs. measured Tbot", x = 0.02))

p.fit <- subplot(p.top, p.bot, nrows = 2, shareX = TRUE, titleY = TRUE, titleX = TRUE) %>%
    layout(title = "Karno grey-box fit: Ttop / Tbot", hovermode = "x unified")

htmlwidgets::saveWidget(p.fit, "figures/graybox_fit_vs_measured.html", selfcontained = TRUE)