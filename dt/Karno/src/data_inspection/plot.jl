using Plots
using DataFrames
using Dates

include("tsa.jl")

fig_dir = joinpath(@__DIR__, "figures")
mkpath(fig_dir)

## 1. Raw time series overview (one panel per feature, gaps = missing data)
plot_cols = filter(x -> x != :UTC_DateTime, Symbol.(names(df)))

ts_panels = map(plot_cols) do col
    plot(df.UTC_DateTime, df[!, col];
         label = false, title = string(col), titlefontsize = 8,
         xrotation = 45, linewidth = 1)
end
ts_overview = plot(ts_panels...; layout = (length(ts_panels), 1),
                    size = (1000, 220 * length(ts_panels)), link = :x)
savefig(ts_overview, joinpath(fig_dir, "ts_overview.png"))

## 2. Contiguous segments highlighted on top of the raw signal
"""
  Plot a variable over time with contiguous (fully non-missing) segments
  highlighted, so you can see at a glance which stretches are usable for
  gray-box fitting.
"""
function plot_segments(df::AbstractDataFrame, col::Symbol, segs::Vector{<:UnitRange{Int}}; title::AbstractString = string(col))
    p = plot(df.UTC_DateTime, df[!, col]; label = "raw", color = :gray70,
              xrotation = 45, title = title, legend = :outertopright)
    for (i, s) in enumerate(segs)
        plot!(p, df.UTC_DateTime[s], df[s, col]; label = i == 1 ? "usable segment" : false,
              color = :steelblue, linewidth = 2)
    end
    return p
end

seg_panels = [plot_segments(df, c, contiguous_time_segments) for c in [:T_top, :T_bot, Symbol("P_el,HP,air"), :T_air]]
seg_overview = plot(seg_panels...; layout = (length(seg_panels), 1),
                     size = (1000, 250 * length(seg_panels)), link = :x)
savefig(seg_overview, joinpath(fig_dir, "segments_overview.png"))

## 3. ACF / PACF
# tsa.jl's acf_res/pacf_res only cover :T_top out to maxlag=12 (= 1h at the 5 min
# sampling step), which is far too short to see the slower thermal/diurnal dynamics
# relevant for gray-box fitting. Recompute here with a much longer horizon (24h) and
# for all the key gray-box variables, using the shortest usable segment (1008 samples
# ≈ 3.5 days) to keep the estimate well-conditioned.
acf_cols   = [:T_top, :T_bot, Symbol("P_el,HP,air"), :T_air]
maxlag_long = round(Int, Dates.Millisecond(Dates.Hour(24)) / Dates.Millisecond(step))

acf_long_res  = acf_karno(df, contiguous_time_segments, acf_cols; maxlag = maxlag_long)
pacf_long_res = pacf_karno(df, contiguous_time_segments, acf_cols; maxlag = maxlag_long)

lag_hours(lag) = lag * Dates.value(Dates.Millisecond(step)) / (1000 * 3600)

function plot_acf_panel(res::AbstractDataFrame, col::Symbol, valcol::Symbol, label::AbstractString)
    sub = res[res.variable .== string(col), :]
    p = plot(lag_hours.(sub.lag), sub[!, valcol]; seriestype = :sticks, marker = :circle,
             title = "$label - $col", xlabel = "lag (h)", ylabel = label, legend = false)
    hline!(p, [0]; color = :black, linewidth = 1)
    return p
end

acf_panels  = [plot_acf_panel(acf_long_res, c, :acf, "ACF") for c in acf_cols]
acf_overview = plot(acf_panels...; layout = (length(acf_panels), 1),
                     size = (900, 300 * length(acf_panels)))
savefig(acf_overview, joinpath(fig_dir, "acf_overview.png"))

pacf_panels  = [plot_acf_panel(pacf_long_res, c, :pacf, "PACF") for c in acf_cols]
pacf_overview = plot(pacf_panels...; layout = (length(pacf_panels), 1),
                      size = (900, 300 * length(pacf_panels)))
savefig(pacf_overview, joinpath(fig_dir, "pacf_overview.png"))

## 4. Cross-correlation: P_el,HP,air vs T_top / T_bot (raw and AR-whitened)
"""
  Overlay raw and whitened CCF for each pair on the same axes so the
  effect of removing the AR structure is visible directly.
"""
function plot_ccf_comparison(ccf_raw::AbstractDataFrame, ccf_white::AbstractDataFrame, pair::AbstractString)
    raw_sub   = ccf_raw[ccf_raw.pair .== pair, :]
    white_sub = ccf_white[ccf_white.pair .== pair, :]
    p = plot(raw_sub.lag, raw_sub.ccf; label = "raw", marker = :circle, title = pair,
             xlabel = "lag", ylabel = "ccf")
    plot!(p, white_sub.lag, white_sub.ccf; label = "AR-whitened", marker = :diamond)
    hline!(p, [0]; color = :black, linewidth = 1, label = false)
    return p
end

pairs = unique(ccc_res.pair)
ccf_panels = [plot_ccf_comparison(ccc_res, white_ccc_res, p) for p in pairs]
ccf_overview = plot(ccf_panels...; layout = (length(ccf_panels), 1),
                     size = (800, 350 * length(ccf_panels)))
savefig(ccf_overview, joinpath(fig_dir, "ccf_overview.png"))

@info "Plots saved to $fig_dir"
