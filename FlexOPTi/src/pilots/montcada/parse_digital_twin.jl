########################
### DATATYPES ##########
struct ColInfo
    index::Int
    name::String
    base::String
    lag::Int     
    role::Symbol
end
########################
########################
### HELPER FUNCTIONS ###
"""
    infer_role(base::AbstractString) -> Symbol

Infers the semantic role of a variable based on its base name.

The role is determined using simple naming conventions:
- Variables containing `"TempSP"` or `"SP"` are treated as setpoints.
- Variables starting with `"AmbTemp"` are treated as states.
- All other variables are treated as disturbances.

# Arguments
- `base::AbstractString` — Base name of the variable (without lag suffix).

# Returns
- `Symbol` — One of:
  - `:setpoint`
  - `:state`
  - `:disturbance`

# Examples
  infer_role("TempSP_10_l1")     :setpoint
  infer_role("AmbTemp_10_l2")    :state
  infer_role("scos1")            :disturbance
"""

function infer_role(base::AbstractString)
    if occursin("TempSP", base) || occursin("SP", base)
        return :setpoint
    elseif startswith(base, "AmbTemp") 
        return :state
    else
        return :disturbance
    end
end


"""
    parse_colname(name::String) -> (String, Int)

Parses a column name into its base variable name and lag index.

Column names may optionally end with a lag suffix of the form `_lX`,
where `X` is a non-negative integer. If no lag is present, a lag of `0`
is assumed.

# Arguments
- `name::String` — Column name, optionally including a lag suffix.

# Returns
- `base::String` — Base variable name.
- `lag::Int` — Lag index (0 if no suffix is present).

# Examples
parse_colname("AmbTemp_10_l3")   # ("AmbTemp", 3)
parse_colname("TempSP_10)        # ("TempSP", 0)
"""
function parse_colname(name::String)
    m = match(r"^(.*)_l(\d+)$", name)
    if m !== nothing
        base = m.captures[1]
        lag = parse(Int, m.captures[2])
    else
        base = name
        lag = 0
    end
    return base, lag
end


# Helper
"""
    collect_lags(cols) -> Dict{String, Vector{Int}}

Collects and groups lag indices for each base variable name.

Given a collection of column names, this function extracts lag indices
(encoded as `_lX`) and returns, for each base variable, the sorted list
of all lags that appear. Columns without an explicit lag are assigned
a lag of `0`.

If the ARX model only uses l1 and l3 for AmbTemp return "AmbTemp" => [1,3]

# Arguments
- `cols` — Iterable collection of column name strings.

# Returns
- `Dict{String, Vector{Int}}` — Dictionary mapping base variable names
  to sorted vectors of lag indices.

# Examples
  cols = ["Temp_l1", "Temp_l3", "Temp", "Power_l2"]
  collect_lags(cols)
  # Dict(
  #   "AmbTemp" => [1, 3],
  #   "TempSP" => [0, 2]
  # )
"""
function collect_lags(cols)
    result = Dict{String, Set{Int}}()

    for c in cols
        # split at the first underscore only
        parts = split(c, "_", limit=2)
        base = parts[1]

        # extract lag if "_lX" exists, otherwise 0
        lag = 0
        if occursin(r"_l\d+$", c)
            lag = parse(Int, match(r"_l(\d+)$", c).captures[1])
        end

        # create set if new
        if !haskey(result, base)
            result[base] = Set{Int}()
        end

        push!(result[base], lag)
    end

    return Dict(k => sort(collect(v)) for (k, v) in result)
end

"""
    find_index_from_datetime(
        list::AbstractVector,
        datetime::Union{String, ZonedDateTime, Nothing}
    ) -> Union{Int, Missing}

Finds the index of an entry whose `"start"` datetime matches the given value.

Each element of `list` is expected to be indexable with `"start"` and to
contain a datetime string compatible with `ZonedDateTime`.

If `datetime` is:
- `String` — it is parsed as a `ZonedDateTime`.
- `Nothing` — the last index of the list is returned.

If no matching datetime is found, `missing` is returned.

# Arguments
- `list::AbstractVector` — Collection of entries containing `"start"` fields.
- `datetime::Union{String, ZonedDateTime, Nothing}` — Datetime to search for.

# Returns
- `Int` — Index of the matching entry.
- `Missing` — If no match is found.

# Examples
  schedule = [
      Dict("start" => "2024-01-01T00:00:00+01:00"),
      Dict("start" => "2024-01-01T01:00:00+01:00")
  ]
  
  find_index_from_datetime(schedule, "2024-01-01T01:00:00+01:00") # 2
  find_index_from_datetime(schedule, nothing)                     # 2
  find_index_from_datetime(schedule, "2024-01-02T00:00:00+01:00") # missing
"""
function find_index_from_datetime(list::AbstractVector, datetime::Union{String, ZonedDateTime, Nothing})::Union{Int, Missing}

    if datetime isa String
        datetime = ZonedDateTime(datetime)
    end

    # No specification return the last index
    if datetime === nothing 
        return length(list)
    end

    # Otherwise search for closest match
    if isempty(list)
        return missing
    end
    
    # If we have a datetime, find the closest match
    closest_index = 1
    smallest_diff = typemax(Float64)
    
    for (i, entry) in enumerate(list)
        start_str = entry["start"]                          
        compute_datetime  = ZonedDateTime(start_str)                
        
        # Calculate absolute time difference
        diff = abs((compute_datetime - datetime) / Second(1))
        
        if diff < smallest_diff
            smallest_diff = diff
            closest_index = i
        end
        
        # Early exit if we find exact match
        if diff == 0
            return i
        end
    end

    t_warn = 30*60
    if smallest_diff > t_warn 
        @warn "The date used for optimization is more than $(t_warn/60) minutes off..."
    end
    
    return closest_index
end

"""
    find_datetime_index_in_digital_twin(digital_twin, datetime) -> Int or nothing

Given:
- `digital_twin`: Dict with key "TransformedInputsTemperature"
- `datetime`: String or ZonedDateTime

Returns:
- index of matching entry 
- last index if datetime === nothing
- `nothing` if not found
"""
function find_datetime_index_in_digital_twin(digital_twin::Dict{String, Any},
    datetime::Union{String, ZonedDateTime, Nothing})::Union{Int, Missing}
    
    v = digital_twin["TransformedInputsTemperature"]
    return find_index_from_datetime(v, datetime)
end
###################


"""
    parse_digital_twin(::Montcada, json_path::String) -> Dict{String, Any}

Parses and preprocesses a digital twin JSON file for MPC usage.

The function:
- Loads and converts model coefficients to numerical matrices,
- Extracts column metadata (base name, lag, role),
- Computes time-step information and start index,
- Collects lag structures used in the model.

# Arguments
- `pilot::Montcada` — Pilot identifier (used for dispatch and consistency).
- `json_path::String` — Path to the digital twin JSON file.

# Returns
- `Dict{String, Any}` — Parsed digital twin data enriched with metadata,
  coefficient matrices, lag information, and timing details.

# Examples
  dt = parse_digital_twin(pilot, "digital_twin_montcada.json")
  Δt = dt["DeltaTimeInHours"]
  lags = dt["CollectedLags"]
"""
function parse_digital_twin(::Montcada, o::O, json_path::String)::Dict{String, Any}

    compute_datetime = o.compute_datetime

    data = JSON.parsefile(json_path)

    ### Temperature ###
    colnames_T_heat = data["CoefsTempHeatingColNames"      ]
    colnames_T_cool = data["CoefsTempCoolingColNames"      ]
    colnames_E_heat = data["CoefsHVACEnergyHeatingColNames"]
    colnames_E_cool = data["CoefsHVACEnergyHeatingColNames"]

    # Process data 
    # Extract information about the columns
    heatTColInfo = ColInfo[]
    for (jcol, name) in enumerate(colnames_T_heat)
        base, lag = parse_colname(name)
        push!(heatTColInfo, ColInfo(jcol, name, base, lag, infer_role(base)))
    end
    coolTColInfo = ColInfo[]
    for (jcol, name) in enumerate(colnames_T_cool)
        base, lag = parse_colname(name)
        push!(coolTColInfo, ColInfo(jcol, name, base, lag, infer_role(base)))
    end
    heatEColInfo = ColInfo[]
    for (jcol, name) in enumerate(colnames_E_heat)
        base, lag = parse_colname(name)
        push!(heatEColInfo, ColInfo(jcol, name, base, lag, infer_role(base)))
    end
    coolEColInfo = ColInfo[]
    for (jcol, name) in enumerate(colnames_E_cool)
        base, lag = parse_colname(name)
        push!(coolEColInfo, ColInfo(jcol, name, base, lag, infer_role(base)))
    end

    coef_T_heat = data["CoefsTempHeating"];
    coef_T_heat = [Float64(coef_T_heat[i][j][1]) for i in eachindex(coef_T_heat),j in eachindex(coef_T_heat[1])];
    coef_T_cool = data["CoefsTempCooling"];
    coef_T_cool = [Float64(coef_T_cool[i][j][1]) for i in eachindex(coef_T_cool),j in eachindex(coef_T_cool[1])];
    coef_E_heat = data["CoefsHVACEnergyHeating"];
    coef_E_heat = [Float64(coef_E_heat[i][j][1]) for i in eachindex(coef_E_heat),j in eachindex(coef_E_heat[1])];
    coef_E_cool = data["CoefsHVACEnergyCooling"];
    coef_E_cool = [Float64(coef_E_cool[i][j][1]) for i in eachindex(coef_E_cool),j in eachindex(coef_E_cool[1])];

    parsed_data = Dict{typeof(first(keys(data))), Any}(data)
    # Replace the dynamics
    parsed_data["CoefsTempHeating"             ] = coef_T_heat
    parsed_data["CoefsTempCooling"             ] = coef_T_cool
    parsed_data["CoefsHVACEnergyHeating"       ] = coef_E_heat
    parsed_data["CoefsHVACEnergyCooling"       ] = coef_E_cool 
    # Add the column informations
    parsed_data["CoefsTempHeatingColInfo"      ] = heatTColInfo
    parsed_data["CoefsTempCoolingColInfo"      ] = coolTColInfo
    parsed_data["CoefsHVACEnergyHeatingColInfo"] = heatEColInfo
    parsed_data["CoefsHVACEnergyCoolingColInfo"] = coolEColInfo

    # Add the previous operation modes
    parsed_data["HVACStatus"] = data["HVACStatus"]

    # Get the starting index
    # Use provided compute_datetime 
    datetime_index = find_datetime_index_in_digital_twin(parsed_data, compute_datetime) 
    parsed_data["StartIndex"] = datetime_index

    # Add datetime metadata
    last_start = TimeZones.ZonedDateTime(parsed_data["TransformedInputsTemperature"][datetime_index]["start"])
    last_end   = TimeZones.ZonedDateTime(parsed_data["TransformedInputsTemperature"][datetime_index]["end"  ])
    parsed_data["LastStartDateTime"] = last_start 
    parsed_data["LastEndDateTime"  ] = last_end
    Δt = (last_end - last_start)
    Δt = Millisecond(Δt) / Millisecond(Hour(1))
    parsed_data["DeltaTimeInHours"] = Δt
    parsed_data["NumberRooms"] = length(parsed_data["CoefsTempCoolingRowNames"])

    # Parse the lags used in the model 
    collected_lags = collect_lags(parsed_data["CoefsTempHeatingColNames"])
    parsed_data["CollectedLags"] = collected_lags

    @info "Next time step interval : [$last_start - $last_end];"
    @info "Using Δt = $(Δt) h."

    return parsed_data

end 
