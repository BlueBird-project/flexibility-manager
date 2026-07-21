# diagnostics.jl — reusable checks for the grey-box identification assumptions.
#
# This is NOT a plotting script and NOT a model fit: it's the set of sanity
# checks that came out of reviewing `documents/karno_mpc_greybox.pdf` against
# the actual data, specifically:
#   - is P_el a genuine exogenous input, or mostly a feedback response to T_top?
#   - is T_top/T_bot two-node coupling (kappa) actually identifiable?
#   - does the chosen window even have a live ASHP setpoint / modulation to fit?
#
# Usage:
#   include("tsa.jl")          # builds df, contiguous_time_segments, step, diffn
#   include("diagnostics.jl")
#   run_diagnostics()          # prints the full report on the currently loaded window
#
# Every function below also works standalone, so you can call e.g.
# `feedback_ccf(:T_top, :P_el)` on its own while poking around in the REPL.

using Statistics
using StatsBase
using Printf

# --------------------------------------------------------------------------
# Friendly-name -> actual DataFrame column mapping.
#
# Several raw columns contain commas (e.g. "P_el,HP,air"), which is legal as
# a Julia Symbol but easy to typo. Centralizing the mapping here means the
# rest of this file (and any REPL usage) can just say `:P_el` etc.
# Extend this dict if you need another column for a new check.
# --------------------------------------------------------------------------
const DIAG_COLS = Dict(
    :T_top   => :T_top,
    :T_bot   => :T_bot,
    :P_el    => Symbol("P_el,HP,air"),      # ASHP electrical power (measured)
    :Q_dist  => Symbol("Q̇_dist"),           # district heat load
    :T_air   => :T_air,                     # outdoor temperature
    :T_sp    => :SP_ASHP_manu,              # ASHP supply target, manual setpoint — the real commanded input.
                                             # (T_set,HP,air, used originally, turned out to be dead across almost
                                             # the whole history — see scan_window.jl notes. SP_ASHP_manu is live,
                                             # per the plant engineer; SP_ASHP_Max/Min bound it and are also logged
                                             # but not yet wired into these checks — the control logic between the
                                             # three is still unconfirmed.)
    :T_hs    => Symbol("T_HP,air"),         # ASHP supply / condenser outlet (measured response to T_sp)
    :z       => Symbol("z_HP,air"),         # ASHP on/off run state
    :nu      => Symbol("ν_HP,air"),         # ASHP compressor modulation level (%)
)

"""
    concat_segments(col; d=0)

Concatenate `col` across all `contiguous_time_segments`, differencing each
segment `d` times first (see `diffn` in tsa.jl). Segments are never bridged,
so differencing/statistics never leak across a data gap.

`col` may be a friendly key from `DIAG_COLS` or a raw column Symbol.
"""
function concat_segments(col; d::Int=0)
    actual_col = get(DIAG_COLS, col, col)
    out = Float64[]
    for s in contiguous_time_segments
        x = disallowmissing(Vector(df[s, actual_col]))
        append!(out, diffn(x, d))
    end
    return out
end

# --------------------------------------------------------------------------
# 1. Correlation overview
# --------------------------------------------------------------------------
"""
    correlation_overview(cols=collect(keys(DIAG_COLS)); d=0)

Print the pairwise correlation matrix across `cols`, concatenated over all
contiguous segments. Run with `d=0` (levels) and `d=1` (first differences)
and compare: a strong level correlation that collapses under differencing is
usually a shared-trend/non-stationarity artifact, not a real short-lag
relationship (see the T_top/T_bot PACF discussion this diagnostic grew out
of). A correlation that survives differencing is a much better candidate for
a genuine dynamical link.

A column with zero variance in the current window (e.g. a dead setpoint)
prints as NaN — that's a hard warning, not a numerical fluke.
"""
function correlation_overview(cols=collect(keys(DIAG_COLS)); d::Int=0)
    series = [concat_segments(c; d=d) for c in cols]
    M = hcat(series...)
    C = cor(M)

    println("Correlation matrix (d=$d) [", d == 0 ? "levels" : "differenced $d time(s)", "]")
    print(rpad("", 8))
    for c in cols; print(rpad(string(c), 8)); end
    println()
    for i in eachindex(cols)
        print(rpad(string(cols[i]), 8))
        for j in eachindex(cols)
            @printf("%-8.2f", C[i, j])
        end
        println()
    end
    return C
end

# --------------------------------------------------------------------------
# 2. Two-node (kappa) identifiability
# --------------------------------------------------------------------------
"""
    kappa_identifiability(colA=:T_top, colB=:T_bot)

`kappa` in the tank model is identified from how (T_top - T_bot) moves, not
from T_top or T_bot individually. If the two nodes move almost in lockstep
(std of the difference small relative to either node), kappa and the two
node capacitances become hard to separate — the two-node model degenerates
towards a single well-mixed tank. Large std(diff)/std(node) is the signature
you want to see before trusting a fitted kappa.
"""
function kappa_identifiability(colA=:T_top, colB=:T_bot)
    a = concat_segments(colA)
    b = concat_segments(colB)
    d = a .- b
    @printf("corr(%s,%s) levels       = %.3f\n", colA, colB, cor(a, b))
    da, db = concat_segments(colA; d=1), concat_segments(colB; d=1)
    @printf("corr(d%s,d%s)            = %.3f\n", colA, colB, cor(da, db))
    @printf("std(%s)=%.3f  std(%s)=%.3f  std(%s-%s)=%.3f\n", colA, std(a), colB, std(b), colA, colB, std(d))
    @printf("std(diff)/std(%s)        = %.3f  (rule of thumb: <~0.3 => weak, kappa hard to pin down)\n",
            colA, std(d) / std(a))
end

# --------------------------------------------------------------------------
# 3. Excitation check: is P_el (or any candidate input) actually driven by
#    the exogenous forecast signals, or mostly by the state it's supposed to
#    be an input to?
# --------------------------------------------------------------------------
"""
    excitation_r2(target, exogenous...; d=0)

Linear R² of `target` regressed on `exogenous` columns (e.g. weather/demand
forecasts, things you actually have at MPC decision time). Then reports the
R² again after adding `:T_top` to the regressors.

A low R² on exogenous-only, and a big jump once T_top is added, is direct
evidence that `target` (typically `:P_el`) is a feedback response to the
tank state rather than an independent excitation — exactly the closed-loop
identification risk flagged for this model.
"""
function excitation_r2(target, exogenous...; d::Int=0)
    y = concat_segments(target; d=d)
    Xs = [concat_segments(c; d=d) for c in exogenous]
    r2 = _lin_r2(y, Xs...)
    r2_with_ttop = _lin_r2(y, Xs..., concat_segments(:T_top; d=d))
    @printf("d=%d  R^2[%s ~ %s]           = %.3f\n", d, target, join(exogenous, ","), r2)
    @printf("d=%d  R^2[%s ~ %s,T_top]     = %.3f  (jump => feedback from T_top, not real excitation)\n",
            d, target, join(exogenous, ","), r2_with_ttop)
    return r2, r2_with_ttop
end

function _lin_r2(y, Xs...)
    X = hcat(ones(length(y)), Xs...)
    b = X \ y
    yhat = X * b
    ss_res = sum((y .- yhat) .^ 2)
    ss_tot = sum((y .- mean(y)) .^ 2)
    return 1 - ss_res / ss_tot
end

# --------------------------------------------------------------------------
# 4. Feedback direction: cross-correlation on differenced data
# --------------------------------------------------------------------------
"""
    feedback_ccf(a, b; d=1, maxlag=6)

Cross-correlation of `a` vs `b`, computed per-segment on the `d`-times
differenced series (see the earlier PACF discussion for why differencing
matters here: it removes the shared-trend artifact so what's left is closer
to genuine short-lag structure).

Sign convention (StatsBase.crosscor(x, y, lags)): for lag `l >= 0` this is
corr(x_t, y_{t+l}) — i.e. `x` leads `y`. So for `feedback_ccf(:T_top, :P_el)`:
  - positive lags  -> does T_top(t) predict P_el(t+l)?      (control feedback)
  - negative lags  -> does P_el(t) predict T_top(t+l), read backwards?  (plant response)
Call it both ways round (`feedback_ccf(:T_top,:P_el)` and
`feedback_ccf(:P_el,:T_top)`) if you want both directions laid out on the
positive-lag side, since the two calls are mirror images of each other.
"""
function feedback_ccf(a, b; d::Int=1, maxlag::Int=6)
    lags = -maxlag:maxlag
    acc = zeros(length(lags))
    n = 0
    for s in contiguous_time_segments
        x = diffn(disallowmissing(Vector(df[s, get(DIAG_COLS, a, a)])), d)
        y = diffn(disallowmissing(Vector(df[s, get(DIAG_COLS, b, b)])), d)
        length(x) <= maxlag + 2 && continue
        acc .+= crosscor(x, y, lags)
        n += 1
    end
    acc ./= n
    print(rpad("$a vs $b (d=$d):", 22))
    for c in acc
        @printf("%+.2f ", c)
    end
    println("  (lags $(lags[1])..$(lags[end]))")
    return collect(lags), acc
end

# --------------------------------------------------------------------------
# 5. Per-signal summary: range, spread, and — importantly — whether a
#    setpoint/actuator signal is actually alive in this window at all.
# --------------------------------------------------------------------------
"""
    signal_summary(cols=collect(keys(DIAG_COLS)))

Prints min/max/mean/std/distinct-value-count for each signal over the
currently loaded window (all contiguous segments concatenated). This is the
check that caught `T_set,HP,air` being identically 0 over the originally
inspected 2-week window: a Stage-2 / joint-model fit needs this column to
actually vary. A `std` of (numerically) zero here is a hard blocker for
anything that regresses against that column, not a modelling nuance.

Also reports the ASHP duty cycle (mean of `:z`) and the fraction of samples
where the modulation `:nu` sits strictly between 5% and 95% — i.e. genuinely
modulating rather than pinned at an on/off extreme.
"""
function signal_summary(cols=collect(keys(DIAG_COLS)))
    for c in cols
        v = concat_segments(c)
        @printf("%-10s  n=%-6d min=%-9.3f max=%-9.3f mean=%-9.3f std=%-9.4f n_unique=%d\n",
                c, length(v), minimum(v), maximum(v), mean(v), std(v),
                length(unique(round.(v, digits=4))))
    end
    z = concat_segments(:z)
    nu = concat_segments(:nu)
    p = concat_segments(:P_el)
    @printf("\nASHP duty cycle (mean z)                = %.3f\n", mean(z))
    @printf("fraction P_el < 5%% of window max        = %.3f\n", mean(p .< 0.05 * maximum(p)))
    @printf("fraction genuinely modulating (5%%<nu<95%%) = %.3f\n", mean((nu .> 5) .& (nu .< 95)))
end

# --------------------------------------------------------------------------
# Top-level report
# --------------------------------------------------------------------------
"""
    run_diagnostics()

Runs the full checklist against the currently loaded `df` / `contiguous_time_segments`
(as built by tsa.jl) and prints a report:
  1. correlation matrix, levels and differenced
  2. T_top/T_bot (kappa) identifiability
  3. is P_el excited by weather/demand, or mostly a feedback response to T_top?
  4. feedback CCF between T_top and P_el
  5. per-signal summary, incl. whether T_sp / modulation are actually alive here

Re-run this any time you change the identification window (date_time_start /
date_time_end in tsa.jl) before trusting a fit on that window.
"""
function run_diagnostics()
    println("="^70); println("1. CORRELATION — LEVELS"); println("="^70)
    correlation_overview(; d=0)

    println("\n"*"="^70); println("2. CORRELATION — DIFFERENCED (d=1)"); println("="^70)
    correlation_overview(; d=1)

    println("\n"*"="^70); println("3. T_top/T_bot COUPLING (kappa identifiability)"); println("="^70)
    kappa_identifiability()

    println("\n"*"="^70); println("4. IS P_el EXCITED, OR JUST FEEDBACK?"); println("="^70)
    for d in (0, 1)
        excitation_r2(:P_el, :Q_dist, :T_air; d=d)
    end

    println("\n"*"="^70); println("5. FEEDBACK CCF (T_top vs P_el, differenced)"); println("="^70)
    feedback_ccf(:T_top, :P_el)
    feedback_ccf(:P_el, :T_top)

    println("\n"*"="^70); println("6. SIGNAL SUMMARY (incl. is T_sp alive in this window?)"); println("="^70)
    signal_summary()
end
