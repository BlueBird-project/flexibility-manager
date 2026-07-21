# scan_window.jl — search the FULL raw parquet for a good identification window.
#
# Motivation: the window currently hard-coded in tsa.jl (2026-05-12..05-25) turned
# out to be shoulder-season (mild May weather, low district load), not winter DR
# conditions. It was originally also thought to lack a live ASHP setpoint, because
# `T_set,HP,air` is identically 0 throughout — but per the plant engineer that was
# just the wrong column: the real commanded setpoint is `SP_ASHP_manu` (bounded by
# `SP_ASHP_Max`/`SP_ASHP_Min`), which IS live in this window (see diagnostics.jl's
# signal_summary()). The exact control logic between manu/Max/Min is still
# unconfirmed with the engineer, so treat SP_ASHP_manu as provisional.
#
# This script scans the *entire* logged history (independent of tsa.jl's date
# filter) with a sliding window and scores each candidate on exactly the
# properties a good identification window needs:
#   - data completeness (few/no gaps)
#   - the ASHP setpoint T_sp actually varies (Stage 2 needs this)
#   - the compressor spends real time modulating, not just pinned on/off
#   - winter conditions (cold T_air => real heating demand, real DR value)
#
# Usage:
#   julia --project=. scan_window.jl                 # default 14-day windows, 7-day stride
#   julia --project=. -e 'include("scan_window.jl"); scan_windows(load_raw(); window=Day(21))'

using Parquet2
using DataFrames
using Dates
using Statistics
using Printf

const RAW_PATH = joinpath(@__DIR__, "data/karno-410708_raw_k0001.parquet")

"""
    load_raw(; since=nothing)

Load the raw parquet (no column subsetting), sorted by time. This is
deliberately independent of tsa.jl's `df` (which is already sliced to one
candidate window) — we need the whole history to search it.

Logging density jumps sharply around 2025-03 (roughly 1 sample/day before
that, full 5-min cadence after) — pass `since=DateTime("2025-03-01")` to
skip the effectively-unusable early stretch when scanning.
"""
function load_raw(; since::Union{DateTime,Nothing}=nothing)
    ds = Parquet2.Dataset(RAW_PATH)
    df = DataFrame(ds)
    sort!(df, :UTC_DateTime)
    isnothing(since) || (df = df[df.UTC_DateTime .>= since, :])
    return df
end

"""
    window_stats(df, t0, t1; step=Minute(5))

Compute the scoring features for the calendar window `[t0, t1)`:

- `completeness`   : actual rows / expected rows at `step` cadence — a proxy
                     for gap severity without needing to run the full
                     regularize+impute pipeline from tsa.jl for every
                     candidate window (that would be far slower).
- `sp_nonzero_frac`, `sp_std`, `sp_levels` : how alive `T_set,HP,air` is —
                     the ASHP setpoint. `sp_levels` counts distinct rounded
                     values, so a setpoint that only ever takes 1-2 values
                     is flagged even if technically "nonzero".
- `duty`           : mean on/off state (`z_HP,air`) — want this away from
                     0 and 1 (some real cycling, not permanently off/on).
- `modulating_frac`: fraction of samples with 5% < nu < 95%, i.e. genuinely
                     modulating rather than pinned at an extreme.
- `Tair_mean`, `Qdist_mean` : winter-ness / real load proxy.
"""
function window_stats(df::AbstractDataFrame, t0::DateTime, t1::DateTime; step::Period=Minute(5))
    sub = df[t0 .<= df.UTC_DateTime .< t1, :]
    n_expected = max(1, Int(round(Dates.value(Millisecond(t1 - t0)) / Dates.value(Millisecond(step)))))
    n_actual = nrow(sub)
    completeness = n_actual / n_expected

    sp = collect(skipmissing(sub[!, "SP_ASHP_manu"]))
    sp_nonzero_frac = isempty(sp) ? 0.0 : mean(sp .!= 0)
    sp_std          = isempty(sp) ? 0.0 : std(sp)
    sp_levels       = isempty(sp) ? 0   : length(unique(round.(sp, digits=1)))

    z  = collect(skipmissing(sub[!, "z_HP,air"]))
    duty = isempty(z) ? NaN : mean(z)

    nu = collect(skipmissing(sub[!, "ν_HP,air"]))
    modulating_frac = isempty(nu) ? NaN : mean(5 .< nu .< 95)

    Tair = collect(skipmissing(sub.T_air))
    Qd   = collect(skipmissing(sub[!, "Q̇_dist"]))
    Tair_mean = isempty(Tair) ? NaN : mean(Tair)
    Qdist_mean = isempty(Qd) ? NaN : mean(Qd)

    return (; t0, t1, completeness, n_actual, sp_nonzero_frac, sp_std, sp_levels,
              duty, modulating_frac, Tair_mean, Qdist_mean)
end

"""
    scan_windows(df=load_raw(); window=Day(14), stride=Day(7))

Slide a `window`-long candidate over the full time range in `df`, in steps of
`stride`, and compute `window_stats` for each. Returns a Vector of the named
tuples from `window_stats` — pass it to `top_candidates` to rank/filter.
"""
function scan_windows(df::AbstractDataFrame=load_raw(); window::Period=Day(14), stride::Period=Day(7))
    t0, t1 = extrema(df.UTC_DateTime)
    starts = t0:stride:(t1 - window)
    return [window_stats(df, s, s + window) for s in starts]
end

"""
    is_usable(w; min_completeness=0.9, min_duty=0.05, max_duty=0.95, min_sp_levels=2)

Hard gate on data quality + real compressor/setpoint excitation.

`SP_ASHP_manu` (the real ASHP setpoint, per the plant engineer — see the
module docstring for why `T_set,HP,air` was dropped) is mostly pinned at one
value through 2025, with one standout exception: late Dec 2025 - early Jan
2026, where it takes up to 12 distinct levels. `min_sp_levels=2` filters out
the "setpoint never moved" windows so what's left actually supports Stage 2
/ the joint model (§8.3/8.4); raise it if you want to require even richer
excitation. `min_duty`/`max_duty` similarly avoid windows where the ASHP is
permanently off or permanently on.
"""
function is_usable(w; min_completeness=0.9, min_duty=0.05, max_duty=0.95, min_sp_levels=2)
    return w.completeness > min_completeness &&
           min_duty < w.duty < max_duty &&
           w.sp_levels >= min_sp_levels
end

"""
    score(w)

Among windows that pass `is_usable`, rank by: more distinct setpoint levels
(the main thing that was missing with the old T_set,HP,air column — reward
it heavily), more time spent genuinely modulating (`nu` between 5-95%),
higher completeness, and colder weather (more heating demand, more
DR-relevant). Weights are heuristic — the point is to surface a shortlist to
eyeball, not to be authoritative. `modulating_frac` is NaN before the `nu`
channel starts being logged (~2026-02); treated as 0 contribution so those
windows aren't dropped, just not rewarded for modulation richness.
"""
function score(w)
    is_usable(w) || return -Inf
    mod_term = isnan(w.modulating_frac) ? 0.0 : 5 * w.modulating_frac
    return 2 * w.sp_levels + mod_term + w.completeness - 0.05 * w.Tair_mean
end

"""
    top_candidates(windows; n=10)

Print the top `n` scanned windows by `score`, plus a count of how many
windows were rejected by `is_usable` (so you know if the gate is too strict
before concluding "no good window exists").
"""
function top_candidates(windows; n::Int=10)
    scored = [(score(w), w) for w in windows]
    n_rejected = count(s -> s == -Inf, first.(scored))
    sort!(scored, by=first, rev=true)

    println("scanned $(length(windows)) windows, $(n_rejected) rejected by is_usable()")
    println()
    @printf("%-3s %-12s %-12s %-6s %-6s %-8s %-6s %-6s %-7s %-7s\n",
            "#", "start", "end", "score", "compl", "sp_frac", "sp_lv", "duty", "mod_frac", "Tair")
    for (i, (s, w)) in enumerate(scored[1:min(n, length(scored))])
        s == -Inf && continue
        @printf("%-3d %-12s %-12s %-6.2f %-6.2f %-8.2f %-6d %-6.2f %-7.2f %-7.1f\n",
                i, Dates.format(w.t0, "yyyy-mm-dd"), Dates.format(w.t1, "yyyy-mm-dd"),
                s, w.completeness, w.sp_nonzero_frac, w.sp_levels, w.duty, w.modulating_frac, w.Tair_mean)
    end
    return scored
end

# Run the default scan when executed directly (not when `include`d for its functions).
if abspath(PROGRAM_FILE) == @__FILE__
    # SP_ASHP_manu only starts being logged 2025-05-21; no point scanning before that.
    df = load_raw(; since=DateTime("2025-05-21"))
    windows = scan_windows(df; window=Day(14), stride=Day(3))
    top_candidates(windows; n=15)
end
