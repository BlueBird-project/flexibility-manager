"""
    parse_OPT_output(::Ewh, oy; only_next_step::Bool=false) -> Dict{String,Any}

Parse EWH optimization results into an API-ready dictionary.
Units are read from `digital_twin["state_space"]["units"]`.
"""
function parse_OPT_output(::Ewh, oy; only_next_step::Bool=false)

    ss    = oy[:ox].digital_twin["state_space"]
    units = ss["units"]

    collapse(arr) = length(unique(arr)) == 1 ? first(arr) : join(unique(arr), "/")
    state_unit = collapse(units["states"])  # "degC"
    input_unit = collapse(units["inputs"])  # "W"

    key_map = Dict{Symbol,String}(
        :x                    => "StateTrajectory",
        :SP                   => "TempSP",
        :u                    => "CompressorPowerAllocated",
        :u_req                => "CompressorPowerRequested",
        :p1                   => "ModulatedCompressorPower",
        :p2                   => "FridgePower",
        :p3                   => "FreezerPower",
        :P_tot                => "TotalPower",
        :p_buy                => "PowerGridBuy",
        :p_sell               => "PowerGridSell",
        :PVused               => "PVPowerUsed",
        :PVcurt               => "PVPowerCurtailed",
        :δ_fridge             => "FridgeOn",
        :δ_freezer            => "FreezerOn",
        :OPT_cost             => "OPTCost",
        :OPT_status           => "OPTTerminationStatus",
        :OPT_start_date_time  => "OPTStartDateTime",
        :OPT_end_date_time    => "OPTTerminationTime",
        :OPT_elapsed_time_sec => "OPTProcessElapsedTime",
        :DT_datetime          => "DTProcessedDataTime",
        :forecasts_datetime   => "ForecastProcessedDateTime",
        :sensors_datetime     => "SensorsProcessedDateTime",
    )

    unit_map = Dict{Symbol,String}(
        :x      => state_unit,
        :SP     => state_unit,
        :u      => input_unit,
        :u_req  => input_unit,
        :p1     => input_unit,
        :p2     => input_unit,
        :p3     => input_unit,
        :P_tot  => input_unit,
        :p_buy  => input_unit,
        :p_sell => input_unit,
        :PVused => input_unit,
        :PVcurt => input_unit,
    )

    state_names = ss["state_names"]
    input_names = ss["input_names"]
    output_names = ss["output_names"]

    col_names = Dict{Symbol, Vector{String}}(
        :x     => state_names,
        :SP    => output_names,
        :u     => input_names,
        :u_req => input_names,
    )

    # Time step
    Δt = Dates.Second(round(Int, ss["sampling_time"]))
    t0 = DateTime(ZonedDateTime(oy[:DT_datetime]))
    step_time(k) = string(t0 + (k-1)*Δt)

    result = Dict{String,Any}()

    for (key, val) in oy

        key in (:o, :ox) && continue
        haskey(key_map, key) || continue

        out_key = key_map[key]
        unit    = get(unit_map, key, "")

        if val isa AbstractMatrix

            names  = get(col_names, key, nothing)
            nsteps, ncols = size(val)
            steps  = only_next_step ? 1 : nsteps
            series = Vector{Any}()
            for k in 1:steps
                col_dict = Dict{String,Any}(
                    (isnothing(names) ? string(c) : names[c]) => val[k, c] for c in 1:ncols)
                entry = Dict{String,Any}("datetime" => step_time(k), "index" => col_dict)
                isempty(unit) || (entry["units"] = unit)
                push!(series, entry)
            end
            result[out_key] = series

        elseif val isa AbstractVector

            steps = only_next_step ? 1 : length(val)
            result[out_key] = [
                let entry = Dict{String,Any}("datetime" => step_time(k), "value" => val[k])
                    isempty(unit) || (entry["units"] = unit)
                    entry
                end
                for k in 1:steps
            ]

        else
            result[out_key] = val
        end
    end

    return result
end
