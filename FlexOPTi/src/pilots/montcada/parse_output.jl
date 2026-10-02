function fill_source_datetimes!(::Montcada, oy::Dict{Symbol, Any})
    dt       = oy[:ox].digital_twin
    forecast = oy[:ox].forecast
    sensors  = oy[:ox].sensors
    oy[:DT_datetime       ], _ = find_lattest_datetime(dt["TransformedInputsTemperature"])
    oy[:forecasts_datetime], _ = find_lattest_datetime(forecast["TransformedInputsTemperature"])
    oy[:sensors_datetime  ], _ = find_lattest_datetime(sensors)
    return oy
end

"""
    parse_OPT_output(::Montcada, oy; only_next_step::Bool=false) -> Dict{String,Any}

Parse Montcada optimization results into an API-ready dictionary.
Temperatures in Kelvin, power in kW.
"""
function parse_OPT_output(::Montcada, oy; only_next_step::Bool=false)

    key_map = Dict{Symbol,String}(
        :PVused               => "PVPowerUsed",
        :DT_datetime          => "DTProcessedDataTime",
        :OPT_end_date_time    => "OPTTerminationTime",
        :OPT_start_date_time  => "OPTStartDateTime",
        :OPT_cost             => "OPTCost",
        :SP                   => "TempSP",
        :OPT_status           => "OPTTerminationStatus",
        :p_HVAC               => "HVACTotalPower",
        :PVcurt               => "PVPowerCurtailed",
        :p_grid               => "PowerGrid",
        :balance_heat         => "HeatingBalance",
        :forecasts_datetime   => "ForecastProcessedDateTime",
        :sensors_datetime     => "SensorsProcessedDateTime",
        :T                    => "AmbTemp",
        :SP_transformed       => "TransformedTempSP",
        :SP_active            => "ActiveHVAC",
        :OPT_elapsed_time_sec => "OPTProcessElapsedTime",
        :balance_cool         => "CoolingBalance",
        :Tbc                  => "Tbc",
        :Tbh                  => "Tbh",
    )

    unit_map = Dict{Symbol,String}(
        :T              => "Kelvin",
        :SP             => "Kelvin",
        :SP_transformed => "Kelvin",
        :Tbc            => "Kelvin",
        :Tbh            => "Kelvin",
        :p_HVAC         => "kW",
        :p_grid         => "kW",
        :PVused         => "kW",
        :PVcurt         => "kW",
        :balance_heat   => "kW",
        :balance_cool   => "kW",
    )

    col_mapping = Dict(
        1=>1,  2=>2,  3=>3,  4=>4,  5=>5,  6=>6,  7=>7,  8=>8,  9=>9,  10=>10,
        11=>11, 12=>12, 13=>13, 14=>14, 15=>15, 16=>16, 17=>17, 18=>18,
        19=>19, 20=>20, 21=>21,
        22=>23, 23=>24, 24=>25, 25=>26, 26=>27, 27=>28, 28=>29, 29=>30,
        30=>76, 31=>77, 32=>78, 33=>79, 34=>80,
        35=>97, 36=>98,
    )

    # Time step
    dt_hours = oy[:ox].digital_twin["DeltaTimeInHours"]
    Δt = Dates.Second(round(Int, dt_hours * 3600))
    t0 = DateTime(ZonedDateTime(oy[:DT_datetime]))
    step_time(k) = string(t0 + (k-1)*Δt)

    result = Dict{String,Any}()

    for (key, val) in oy

        key in (:o, :ox) && continue
        haskey(key_map, key) || continue

        out_key = key_map[key]
        unit    = get(unit_map, key, "")

        if val isa AbstractMatrix

            nsteps, ncols = size(val)
            steps  = only_next_step ? 1 : nsteps
            series = Vector{Any}()
            for k in 1:steps
                col_dict = Dict{String,Any}(
                    string(get(col_mapping, c, c)) => val[k, c] for c in 1:ncols)
                entry = Dict{String,Any}("datetime" => step_time(k), "space" => col_dict)
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
