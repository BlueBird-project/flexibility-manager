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
                   "T_bot (HP air inlet)", "T_HP,air", "z_HP,air", "ν_HP,air", "P_el,HP,air", "T_set,HP,air", "fault_HP,air",
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
    "T_bot (HP air inlet)" => "Outdoor temperature threshold that switches between the cold/mild setpoint levels (°C)",  
    "T_HP,air"             => "Water temperature leaving the ASHP condenser, going to tank top (°C)",
    "z_HP,air"             => "ASHP on/off run state (boolean)",        
    "ν_HP,air"             => "ASHP compressor modulation level (%)",       
    "P_el,HP,air"          => "ASHP compressor electrical power ([W] or [kW])",        
    "T_set,HP,air"         => "ASHP leaving-water setpoint commanded to its onboard controller (°C)",       
    "fault_HP,air"         => "ASHP fault/alarm flage (bool)",        
    "T_air"                => "Outdoor air temperature (°C)",           
    "Q̇_dist"               => "District heat demand"
)

# Date Time selection 
date_time_start = Dates.DateTime("2026-05-12T16:05:00")
date_time_end   = Dates.DateTime("2026-05-25T15:00:00")

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
  Compute the autocorrelation function as a matrix
"""
function acf_karno(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, cols::Vector{Symbol}; maxlag::Int)

    isempty(cols) && (cols = filter!(x -> x != :UTC_DateTime, Symbol.(names(df))))

    acc = zeros(maxlag+1, length(cols))
    for s in segs
        X = disallowmissing(Matrix(df[s, cols])) # Matrix 
        acc .+= StatsBase.autocor(X, 0:maxlag)   # We sum up the correlation from dataset to dataset
    end
    acc ./= length(segs)

    # Reshape the correlation matrix to a dataframe
    out = DataFrame(acc, string.(cols))
    out.lag = 0:maxlag

    return stack(out, string.(cols); variable_name=:variable, value_name=:acf)
end

# Results
acf_res    = acf_karno(df, contiguous_time_segments, [:T_top]; maxlag=12);
acf_visual = unstack(acf_res, :lag, :variable, :acf);

"""
  Compute the partial autocorrelation function as matrix (df) 
"""
function pacf_karno(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, cols::Vector{Symbol}; maxlag::Int, method=:regression)

    pacc = zeros(maxlag+1, length(cols))
    for s in segs
       X = disallowmissing(Matrix(df[s, cols]))
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
"""
function ccf_karno(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, colsx::Vector{Symbol}, colsy::Vector{Symbol}; maxlag::Int)

    lags = -maxlag:maxlag
    pairs = [(cx, cy) for cx in colsx for cy in colsy]
    ccc = zeros(length(lags), length(pairs))

    for s in segs
        for (k, (cx, cy)) in enumerate(pairs)
            x = disallowmissing(df[s, cx])
            y = disallowmissing(df[s, cy])
            ccc[:, k] .+= crosscor(x, y, lags)
        end
    end
    ccc ./= length(segs)

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
"""
function ar_whiten_ccc(df::DataFrame, segs::Vector{<:UnitRange{Int64}}, colsx::Vector{Symbol}, colsy::Vector{Symbol}; p::Int, maxlag::Int)
    
    lags = -maxlag:maxlag
    # Split on the segments first to get a meaningful AR model 
    pairs = [(cx, cy) for cx in colsx for cy in colsy]
    ccc = zeros(length(lags), length(pairs))

    for s in segs
        k = 1
        for cx in colsx
            ac = zeros(p+1)
            x = disallowmissing(Vector(df[s, cx]))
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
                y = disallowmissing(Vector(df[s, cy]))
                resid_y = similar(y, length(y)-p)
                for t in (p+1):length(y)
                    resid_y[t-p] = y[t] - sum(φ[k]*y[t-k] for k in 1:p)
                end
                ccc[:,k] .+= crosscor(resid_x, resid_y, lags) 
                k += 1
            end
        end
    end

    ccc ./= length(segs)

    labels = ["$(cx)_vs_$(cy)" for (cx, cy) in pairs]
    out = DataFrame(ccc, labels)
    out.lag = collect(lags)

    return stack(out, labels; variable_name=:pair, value_name=:ccf)
end



# Results
ccc_res = ccf_karno(df, contiguous_time_segments, [Symbol("P_el,HP,air")], [:T_top, :T_bot]; maxlag=12);
ccf_visual = unstack(ccf_res, :lag, :pair, :ccf);

white_ccf_res = ar_whiten_ccc(df, contiguous_time_segments, [Symbol("P_el,HP,air")], [:T_top, :T_bot]; p = 12, maxlag=12);
white_ccf_visual = unstack(white_ccf_res, :lag, :pair, :ccf);


## Frequency Domain data analysis

