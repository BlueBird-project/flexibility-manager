using Dates

using DataFrames
using Parquet2
using StatsBase
using Impute: Interpolate, impute

using Revise
using Plots

using Infiltrator

## Relevant features
feature_columns = ["UTC_DateTime","T_top","T_bot", "T_dist,in","T_dist,ret",
                   "T_set,dist,cold", "T_set,dist,mild", "T_ext,threshold",
                   "T_bot (HP air inlet)", "T_HP,air", "z_HP,air", "ν_HP,air", "P_el,HP,air", "fault_HP,air",
                   "SP_ASHP_manu", "SP_ASHP_Max", "SP_ASHP_Min",
                   "T_air", "Q̇_dist"]

feature_meaning = Dict(
    "UTC_DateTime"         => "Time Stamps",
    "T_top"                => "Tank top temperature (°C). Hot outlet zone, TT_601",
    "T_bot"                => "Tank bot temperature (°C). Hot outlet zone, TT_602",
    "T_dist,in"            => "District supply temperature, i.e. water temp leaving the plant toward the apartments (°C), TT_701",          
    "T_dist,ret"           => "District return temperature, water coming back from the network (°C), TT_702",   
    "T_set,dist,cold"      => "The two heating-curve setpoint levels for district supply temperature (°C), used depending on outdoor temp (cold-weather level vs. mild-weather level)",    
    "T_set,dist,mild"      => "The two heating-curve setpoint levels for district supply temperature (°C), used depending on outdoor temp (cold-weather level vs. mild-weather level)",     
    "T_ext,threshold"      => "Outdoor temperature threshold that switches between the cold/mild setpoint levels (°C)",       
    "T_bot (HP air inlet)" => "Water temperatrue entering the ASHP condeser (°C)",  
    "T_HP,air"             => "Water temperature leaving the ASHP condenser, going to tank top (°C)",
    "z_HP,air"             => "ASHP on/off run state (boolean)",        
    "ν_HP,air"             => "ASHP compressor modulation level (%)",       
    "P_el,HP,air"          => "ASHP compressor electrical power ([W] or [kW])",        
    "SP_ASHP_manu"         => "Manual air HP supply temperature",
    "SP_ASHP_Max"          => "Max air HP supply temperature",
    "SP_ASHP_Min"          => "Min air HP supply temperature",
    "fault_HP,air"         => "ASHP fault/alarm flage (bool)",        
    "T_air"                => "Outdoor air temperature (°C)",           
    "Q̇_dist"               => "District heat demand"
)

# Date Time selection 
date_time_start = Dates.DateTime("2026-05-12T16:05:00")
date_time_end   = Dates.DateTime("2026-05-25T15:00:00")

## Parameters
maxlag_ = 36
arwhitelag_ = 12
ndiff_ = 0 # number of times to difference each segment before computing acf/pacf/ccf

## Extract the data
ds = Parquet2.Dataset(joinpath(@__DIR__, "data/karno-410708_raw_k0001.parquet"));
df = DataFrames.DataFrame(ds)

# Extract only the relevant columns from the dataframe
df = df[date_time_start .<= df.UTC_DateTime .<= date_time_end, feature_columns]

# Start with some extra data processing for missing data points
function regularize(df::AbstractDataFrame; timecol=:UTC_DateTime, step=nothing)
    step = something(step, mode(diff(df[!, timecol])))
    t0, t1 = extrema(df[!, timecol])
    full_grid = DataFrame(; (timecol => t0:step:t1,)...)
    # left-join your data onto the full grid; missing rows become `missing`
    out = leftjoin(full_grid, df; on=timecol)
    sort!(out, timecol)
    return out, step
end

df, step = regularize(df)

# We interpolate when there is up to j missing points otherwise we split in several data sets
df = impute(df, Interpolate(limit=3))

# Find contigious segment in the dataframe 
"""
  Find contiguous row-ranges where ALL of `cols` are non-missing.
  Returns a vector of UnitRanges (row indices into df).
"""
function contiguous_segments(df::AbstractDataFrame, cols::Vector{Symbol})
    valid = [all(!ismissing, row) for row in eachrow(df[:, cols])]
    segments = UnitRange{Int}[]
    i, n = 1, nrow(df)
    while i <= n
        if valid[i]
            j = i
            while j <= n && valid[j]
                j += 1
            end
            push!(segments, i:(j-1))
            i = j
        else
            i += 1
        end
    end
    return segments
end

df_columns = filter!(x -> x != :UTC_DateTime, Symbol.(names(df))) # Columns except DateTime 
contiguous_time_segments = contiguous_segments(df, df_columns)

## Time Domain Data Analysis
"""
  Apply the discrete difference operator `diff` `n` times in a row (n=0 returns
  x unchanged). Each differencing shortens the vector by one sample.
"""
function diffn(x::AbstractVector, n::Int)
    n < 0 && throw(ArgumentError("n must be >= 0"))
    for _ in 1:n
        x = diff(x)
    end
    return x
end

"""
  Difference every column of a matrix `n` times (columns are assumed to be
  individual time series sharing the same time index).
"""
function diffn(X::AbstractMatrix, n::Int)
    n == 0 && return X
    return reduce(hcat, (diffn(view(X, :, j), n) for j in axes(X, 2)))
end

"""
  Compute the autocorrelation function as a matrix

  `ndiff` differences each segment `ndiff` times before computing the acf, e.g.
  to check whether a strong autocorrelation is genuine short-lag structure or
  just an artifact of a non-stationary (trending) signal.
"""
function acf_karno(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, cols::Vector{Symbol}; maxlag=maxlag_::Int, ndiff::Int=ndiff_)

    isempty(cols) && (cols = filter!(x -> x != :UTC_DateTime, Symbol.(names(df))))

    acc = zeros(maxlag+1, length(cols))
    for s in segs
        X = disallowmissing(Matrix(df[s, cols])) # Matrix
        X = diffn(X, ndiff)
        acc .+= StatsBase.autocor(X, 0:maxlag)   # We sum up the correlation from dataset to dataset
    end
    acc ./= length(segs)

    # Reshape the correlation matrix to a dataframe
    out = DataFrame(acc, string.(cols))
    out.lag = 0:maxlag

    return stack(out, string.(cols); variable_name=:variable, value_name=:acf)
end

# Results
acf_res    = acf_karno(df, contiguous_time_segments, [:T_top, :T_bot, Symbol("T_HP,air")]; maxlag=maxlag_);
acf_visual = unstack(acf_res, :lag, :variable, :acf);

"""
  Compute the partial autocorrelation function as matrix (df)

  `ndiff` differences each segment `ndiff` times before computing the pacf (see `acf_karno`).
"""
function pacf_karno(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, cols::Vector{Symbol}; maxlag=maxlag_::Int, method=:regression, ndiff::Int=ndiff_)

    pacc = zeros(maxlag+1, length(cols))
    for s in segs
       X = disallowmissing(Matrix(df[s, cols]))
       X = diffn(X, ndiff)
       pacc .+= StatsBase.pacf(X, 0:maxlag; method=method)
    end
    pacc ./= length(segs)

    out = DataFrame(pacc, string.(cols))
    out.lag = 0:maxlag

    return stack(out, string.(cols); variable_name=:variable, value_name=:pacf)
end

# Results
pacf_res    = pacf_karno(df, contiguous_time_segments, [:T_top]; maxlag=12);
pacf_visual = unstack(pacf_res, :lag, :variable, :pacf);

# Cross correlation
"""
  Compute the cross correlation

  `ndiff` differences each segment `ndiff` times (both `x` and `y`) before computing the ccf.

  A segment where `x` or `y` is constant after differencing (std=0) is skipped
  with a warning rather than silently polluting the average with NaNs (crosscor
  divides by sqrt(var(x)*var(y)), which is 0 for a constant series) — this
  happens e.g. when a setpoint doesn't change at all within one contiguous
  segment. The per-pair average is taken over however many segments actually
  contributed, not over `length(segs)`.
"""
function ccf_karno(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, colsx::Vector{Symbol}, colsy::Vector{Symbol}; maxlag=maxlag_::Int, ndiff::Int=ndiff_)

    lags = -maxlag:maxlag
    pairs = [(cx, cy) for cx in colsx for cy in colsy]
    ccc = zeros(length(lags), length(pairs))
    n_used = zeros(Int, length(pairs))

    for s in segs
        for (k, (cx, cy)) in enumerate(pairs)
            x = diffn(disallowmissing(df[s, cx]), ndiff)
            y = diffn(disallowmissing(df[s, cy]), ndiff)
            if iszero(std(x)) || iszero(std(y))
                @warn "ccf_karno: skipping segment $(s) for pair ($(cx),$(cy)) — constant series after differencing (std=0)"
                continue
            end
            ccc[:, k] .+= crosscor(x, y, lags)
            n_used[k] += 1
        end
    end

    for k in eachindex(n_used)
        n_used[k] == 0 && error("ccf_karno: no usable segments for pair $(pairs[k]) — every segment was constant after differencing")
        ccc[:, k] ./= n_used[k]
    end

    labels = ["$(cx)_vs_$(cy)" for (cx, cy) in pairs]
    out = DataFrame(ccc, labels)
    out.lag = collect(lags)

    return stack(out, labels; variable_name=:pair, value_name=:ccf)
end


# Filter function before computing the cross correlation 
# Todo : read and understand.... 
"""
  The idea is to
  - fit an AR model on x
  - Compute the residulas under this model (as a time serie)
  - Use the same AR model for the data y giving us again a residual time serie
  - Cross correlation between the residual time series. 
  
  p is the model order

  `ndiff` differences each segment `ndiff` times (both `x` and `y`) before fitting
  the AR model / computing the whitened ccf.

  If `x` (the `cx` column) is constant within a segment after differencing
  (std=0), `autocor` divides by zero variance and the Yule-Walker matrix `R`
  fills with NaN, which crashes `R \\ r` with a LAPACK "Infs or NaNs" error —
  this is what happens e.g. when a setpoint doesn't move at all within one
  contiguous segment (see the SP_ASHP_manu case: 2 of 3 segments were pinned
  at a single value). Such a segment is skipped (for every cy paired with
  that cx) with a warning instead of crashing. The per-pair average is taken
  over however many segments actually contributed, not over `length(segs)`.
"""
function ar_whiten_ccf(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, colsx::Vector{Symbol}, colsy::Vector{Symbol}; p::Int, maxlag=maxlag_::Int, ndiff::Int=ndiff_)

    lags = -maxlag:maxlag
    # Split on the segments first to get a meaningful AR model
    pairs = [(cx, cy) for cx in colsx for cy in colsy]
    ccc = zeros(length(lags), length(pairs))
    n_used = zeros(Int, length(pairs))

    for s in segs
        k = 1
        for cx in colsx
            x = diffn(disallowmissing(Vector(df[s, cx])), ndiff)

            if iszero(std(x))
                @warn "ar_whiten_ccf: skipping segment $(s) for $(cx) — constant after differencing (std=0), can't fit an AR($p) whitening filter"
                k += length(colsy)
                continue
            end

            ac = zeros(p+1)
            ac .+= StatsBase.autocor(x, 0:p)
            # solve Yule-Walker for AR coefficients φ
            R = [ac[abs(i-j)+1] for i in 1:p, j in 1:p]
            r = ac[2:end]
            φ = R \ r
            resid_x = similar(x, length(x)-p)
            for t in (p+1):length(x)
                resid_x[t-p] = x[t] - sum(φ[k]*x[t-k] for k in 1:p)
            end

            for cy in colsy
                y = diffn(disallowmissing(Vector(df[s, cy])), ndiff)
                resid_y = similar(y, length(y)-p)
                for t in (p+1):length(y)
                    resid_y[t-p] = y[t] - sum(φ[k]*y[t-k] for k in 1:p)
                end
                ccc[:,k] .+= crosscor(resid_x, resid_y, lags)
                n_used[k] += 1
                k += 1
            end
        end
    end

    for k in eachindex(n_used)
        n_used[k] == 0 && error("ar_whiten_ccf: no usable segments for pair $(pairs[k]) — every segment had a constant/degenerate series after differencing")
        ccc[:, k] ./= n_used[k]
    end

    labels = ["$(cx)_vs_$(cy)" for (cx, cy) in pairs]
    out = DataFrame(ccc, labels)
    out.lag = collect(lags)

    return stack(out, labels; variable_name=:pair, value_name=:ccf)
end



# Results
# ccc_res = ccf_karno(df, contiguous_time_segments, [Symbol("P_el,HP,air")], [:T_top, :T_bot]; maxlag=maxlag_);
# ccc_visual = unstack(ccc_res, :lag, :pair, :ccf);
# 
# white_ccc_res = ar_whiten_ccf(df, contiguous_time_segments, [Symbol("P_el,HP,air")], [:T_top, :T_bot]; p = arwhitelag_, maxlag=maxlag_);
# white_ccc_visual = unstack(white_ccc_res, :lag, :pair, :ccf);

ccc_res = ccf_karno(df, contiguous_time_segments, [:SP_ASHP_manu], [:T_top, :T_bot]; maxlag=maxlag_);
ccc_visual = unstack(ccc_res, :lag, :pair, :ccf);

white_ccc_res = ar_whiten_ccf(df, contiguous_time_segments, [:SP_ASHP_manu], [:T_top, :T_bot]; p = arwhitelag_, maxlag=maxlag_);
white_ccc_visual = unstack(white_ccc_res, :lag, :pair, :ccf);


## Frequency Domain data analysis

