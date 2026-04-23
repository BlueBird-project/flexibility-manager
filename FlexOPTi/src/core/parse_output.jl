# ---- KEY RENAME MAP ----
const KEY_MAP = Dict(
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
    :Tbh                  => "Tbh"
)

"""
    parse_OPT_output(oy; only_next_step::Bool=false) -> Dict{String,Any}

Convert raw MPC results `oy` (returned by `mpc_update`) into a
structured, API-ready dictionary with standardized output keys.

The function:
- Renames internal symbols using `KEY_MAP`,
- Expands matrices into time-indexed room dictionaries,
- Expands vectors into time series,
- Preserves scalar metadata values,
- Attaches timestamps based on `:DT_datetime` and the model time step.

# Arguments
- `oy::Dict{Symbol,Any}` — Raw optimization results.
- `only_next_step::Bool=false` — If `true`, export only the first
  MPC step; otherwise export the full horizon.

# Returns
- `Dict{String,Any}` — JSON-serializable dictionary where:
  - Matrices become arrays of `{datetime, units, space}` objects,
  - Vectors become arrays of `{datetime, value}` objects,
  - Scalars/strings are passed through unchanged.

This function prepares optimization results for API responses
or JSON export.
"""
function parse_OPT_output(oy; only_next_step::Bool=false)

    demo = oy[:o].pilot
    output_parser = parse_OPT_output(demo, oy)

    # ---- time handling ----
    # Montcada stores "DeltaTimeInHours" at the top level; EWH stores sampling_time (s) in state_space
    dt = oy[:ox].digital_twin
    dt_hours = if haskey(dt, "DeltaTimeInHours")
        dt["DeltaTimeInHours"]
    else
        dt["state_space"]["sampling_time"] / 3600.0
    end
    dt = Dates.Second(round(Int, dt_hours * 3600))

    t0 = DateTime(ZonedDateTime(oy[:DT_datetime]))
    step_time(k) = string(t0 + (k-1)*dt)

    result = Dict{String,Any}()

    for (key,val) in oy

        # skip non-exported keys
        key in (:o, :ox) && continue
        haskey(KEY_MAP,key) || continue

        out_key = KEY_MAP[key]

        # ---------------- MATRIX ----------------
        if val isa AbstractMatrix

            nsteps, nrooms = size(val)
            steps = only_next_step ? 1 : nsteps
            series = Vector{Any}()

            for k in 1:steps
                room_dict = Dict{String,Any}()

                for r in 1:nrooms
                    room_num = get(output_parser.room_idx_mapping,r,r)
                    room_dict[string(room_num)] = val[k,r]
                end

                push!(series, Dict(
                    "datetime" => step_time(k),
                    "units" => "Kelvin",
                    "space" => room_dict
                ))
            end

            result[out_key] = series

        # ---------------- VECTOR ----------------
        elseif val isa AbstractVector

            steps = only_next_step ? 1 : length(val)

            result[out_key] = [
                Dict(
                    "datetime" => step_time(k),
                    "value" => val[k]
                )
                for k in 1:steps
            ]

        # ---------------- SCALAR/STRING ----------------
        else
            result[out_key] = val
        end
    end

    return result
end

"""
    write_outputs_to_file(opt_data; file="./data/outputs/output.json")

Write parsed optimization results to a JSON file.

# Arguments
- `opt_data::Dict{String,Any}` — Output dictionary produced by
  `parse_OPT_output`.
- `file::String` — Destination file path.

The data is written using pretty-formatted JSON for readability.
"""
function write_outputs_to_file(opt_data::Dict{String, Any}; 
            file = "./data/outputs/output.json")

    open(file, "w") do io
        # JSON3.write(io, opt) # For API
        JSON3.pretty(io, opt_data)
    end

    return nothing 
end