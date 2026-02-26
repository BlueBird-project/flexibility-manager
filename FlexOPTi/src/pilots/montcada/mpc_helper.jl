"""
    mpc_state_dynamics(digital_twin::Dict{String, Any}, mpc_step::Int; mode="heat")

Extracts the state-space dynamics matrices for a Model Predictive Control (MPC) problem from a digital twin.

# Arguments
- `digital_twin::Dict{String, Any}`  
  Dictionary containing the digital twin coefficients, column info, and row names for heating and cooling dynamics.

- `mpc_step::Int`  
  Current MPC step (used to determine lags for decision variables and known inputs).

- `mode::String = "heat"`  
  Mode of operation for dynamics extraction:
  - `"heat"` — extract heating model
  - `"cool"` — extract cooling model

# Returns
- `A::Matrix` — State matrix mapping previous temperature states to current states.
- `B::Matrix` — Input matrix mapping setpoints to temperature states.
- `G::Matrix` — Input matrix mapping known inputs (beyond MPC horizon) to temperature states.
- `F::Matrix` — Input matrix mapping forecasted inputs to temperature states.
- `input_map::Dict{Int,Int}` — Mapping from input column indices to vector positions.
- `forecast_map::Dict{Int,Int}` — Mapping from forecast column indices to vector positions.
- `col_names::Vector{String}` — Names of all columns in the dynamics matrix.

# Description
This function identifies decision variables, known inputs, and forecasted inputs based on their lag relative to the current MPC step. It also reorders the rows and columns to match the MPC convention and returns the state-space matrices used for predicting room temperatures.
"""
function mpc_state_dynamics(digital_twin::Dict{String, Any}, mpc_step::Int, compute_datetime::Union{String,ZonedDateTime};
                              mode="heat")

    if     mode  == "heat" 
        M        = digital_twin["CoefsTempHeating"             ]
        colInfoT = digital_twin["CoefsTempHeatingColInfo"      ]
        colInfoE = digital_twin["CoefsHVACEnergyHeatingColInfo"]
        row_name = digital_twin["CoefsTempHeatingRowNames"     ]
    elseif mode  == "cool" 
        M        = digital_twin["CoefsTempCooling"             ]
        colInfoT = digital_twin["CoefsTempCoolingColInfo"      ]
        colInfoE = digital_twin["CoefsHVACEnergyCoolingColInfo"]
        row_name = digital_twin["CoefsTempCoolingRowNames"     ]
    else
        @error "Undefined mode $(mode)."
    end


    ### Temperature model 
    decisionT        = Int[]    # Columns that will be a decision variable
    decisionSP       = Int[]    # Columns that will be a decision variable
    input_columns    = Int[]    # Columns known inputs 
    forecast_columns = Int[]    # Columns of forecasts

    for ci in colInfoT
        if ci.role == :state && ci.lag < mpc_step
            push!(decisionT, ci.index)
        elseif ci.role == :setpoint && ci.lag < mpc_step
            push!(decisionSP, ci.index)
        elseif ci.lag >= mpc_step - 1
            push!(input_columns, ci.index)
        else 
            push!(forecast_columns, ci.index)
        end
    end

    # Make sure that the dynamic is created with order convention
    col_names = getfield.(colInfoT, :name)
    # Decision Variables
    decT_names , decisionT  = reorder_labels(col_names[decisionT] , decisionT )
    decSP_names, decisionSP = reorder_labels(col_names[decisionSP], decisionSP)

    # create mapping indices
    input_map    = Dict{Int,Int}()
    forecast_map = Dict{Int,Int}()
    # Matric M column number => position in input_columns
    for (idx, j) in enumerate(input_columns)
        input_map[j]    = idx
    end
    # Matric M column number => position in forecasts_columns
    for (idx, j) in enumerate(forecast_columns)
        forecast_map[j] = idx
    end

    # Reorder base on the output order
    _, row_order = reorder_labels(row_name, Vector(1:length(row_name)))
    
    # Build Dynamics matrices 
    A = M[row_order, decisionT       ]
    B = M[row_order, decisionSP      ]
    G = M[row_order, input_columns   ]
    F = M[row_order, forecast_columns]

    return A, B, G, F,
        input_map, forecast_map,
        col_names
end

"""
    mpc_power_dynamics(digital_twin::Dict{String, Any}, mpc_step::Int; mode="heat")

Extracts HVAC power dynamics for the MPC problem from a digital twin.

# Arguments
- `digital_twin::Dict{String, Any}`  
  Dictionary containing the digital twin coefficients for energy use and temperature inputs.

- `mpc_step::Int`  
  Current MPC step. Used to extract initial conditions for temperature if `mpc_step == 1`.

- `mode::String = "heat"`  
  Mode of operation:
  - `"heat"` — extract heating power dynamics
  - `"cool"` — extract cooling power dynamics

# Returns
- `HVAC_map::Vector` — Coefficients for HVAC power contribution.
- `T_map::Vector` — Coefficients for temperature-dependent power contribution.
- `T_inputs::Vector` — Initial temperature inputs at `mpc_step`.
- `HVAC_inputs::Vector` — HVAC-related input values including intercept and number of active units.

# Description
This function maps the digital twin's HVAC energy model to the MPC step. It separates the contributions from HVAC power and room temperature changes, and handles initial conditions for temperature when starting the MPC horizon.
"""
function mpc_power_dynamics(digital_twin::Dict{String, Any}, mpc_step::Int, compute_datetime::Union{String,ZonedDateTime};
                              mode="heat")

    if mode  == "heat" 
        colInfoE = digital_twin["CoefsHVACEnergyHeatingColInfo"]
        power_linear_map = digital_twin["CoefsHVACEnergyHeating"]
    elseif mode  == "cool" 
        colInfoE = digital_twin["CoefsHVACEnergyCoolingColInfo"]
        power_linear_map = digital_twin["CoefsHVACEnergyCooling"]
    else
        @error "Undefined mode $(mode)."
    end

    # Keep track of the order 
    colnames = getfield.(colInfoE, :name)

    priority = Dict(
        "intercept"       => 0,
        "ComfTempHeating" => 1,
        "ComfTempCooling" => 2,
        "nhvac"           => 3,
    )

    sortable = map(colnames) do name
        if haskey(priority, name)
            # fixed-order items come first
            "AAA_$(priority[name])"
        elseif startswith(name, "IncTemp_")
            n = parse(Int, split(name, "_")[2])
            # IncTemp come after, preserving numeric order
            "IncTemp_$(n + 100)"
        else
            error("Unexpected column name: $name")
        end
    end

    sorted, power_linear_map = reorder_labels(sortable, power_linear_map)

    keys_order = ["intercept", "ComfTempHeating", "ComfTempCooling", "nhvac"]
    HVAC_map = Dict(zip(keys_order, power_linear_map[1:4]))
    T_map    = power_linear_map[5:end]

    # Get the inputs 
    # Initial room temperature 
    if mpc_step == 1
        T_index = find_index_from_datetime(digital_twin["TransformedInputsTemperature"], compute_datetime)
        transformed_inputs = digital_twin["TransformedInputsTemperature"][T_index]
        T0_keys = collect(filter(x -> startswith(x, "AmbTemp") && endswith(x, "l1"), keys(transformed_inputs)))
        T0_val  = get.(Ref(transformed_inputs), T0_keys, missing)
        _, T_inputs = reorder_labels(T0_keys, T0_val)
    else
           T_inputs = Int[]
    end

    hvac_index = find_index_from_datetime(digital_twin["TransformedInputsHVACEnergy"], compute_datetime)
    # HVAC related
    HVAC_info = digital_twin["TransformedInputsHVACEnergy"][hvac_index + mpc_step - 1]
    intercept = HVAC_info["intercept"      ]
    Tbh       = HVAC_info["ComfTempHeating"]
    Tbc       = HVAC_info["ComfTempCooling"]
    Trefh     = HVAC_info["tempref_heating"]
    Trefc     = HVAC_info["tempref_cooling"]
    nHVAC     = HVAC_info["nhvac"          ]
   
    HVAC_inputs = Dict("intercept" => intercept,
                       "Trefh"     => Trefh, 
                       "Trefc"     => Trefc,
                       "Tbh"       => Tbh,
                       "Tbc"       => Tbc,
                       "nhvac"     => nHVAC
    )

    return HVAC_map, T_map, 
        T_inputs, HVAC_inputs
end


"""
    mpc_build_input_vector(digital_twin::Dict{String, Any}, input_map::Dict{Int,Int}, col_names::Vector{String}, backward=0::Int)

Builds the input vector for the MPC model at a given backward step.

# Arguments
- `digital_twin::Dict{String, Any}`  
  Dictionary containing transformed input data.

- `input_map::Dict{Int,Int}`  
  Mapping from input column indices to vector positions.

- `col_names::Vector{String}`  
  Names of the columns corresponding to the inputs.

- `backward::Int = 0`  
  Number of steps behind the current MPC time to extract inputs.

# Returns
- `input_vector::Vector{Float64}` — Vector of input values for the MPC step.

# Description
This function extracts the required inputs from the digital twin and arranges them according to the `input_map`, ensuring consistency with the MPC decision vector.
"""
#TODO In the future it will all come from WP6 ?
function mpc_build_input_vector(
        digital_twin::Dict{String, Any},
        sensors::Vector{Dict{String, Any}},
        input_map::Dict{Int,Int},
        col_names::Vector{String},
        compute_datetime::Union{String,ZonedDateTime}
    )

    start_index = digital_twin["StartIndex"];

    input_dict = digital_twin["TransformedInputsTemperature"][start_index]

    Ni = length(input_map)
    input_vector = zeros(Float64, Ni)

    SP_idx = find_index_from_datetime(sensors, compute_datetime) 
    for (col_idx, mapped_pos) in input_map
        name = col_names[col_idx]

        if startswith(name, "TempSP_") 
            # Add the values from WP6 
            parts = split(name, "_")
            base = "TempSP_" * parts[2]               
            lag = length(parts) == 3 ? parse(Int, lstrip(parts[3],'l')) : 0

            SP = sensors[SP_idx-lag][base] + KELVIN_OFFSET 
            SP = transform_setpoint(SP, parts, input_dict, digital_twin)
            
            input_vector[mapped_pos] = SP

        elseif haskey(input_dict, name)
            input_vector[mapped_pos] = input_dict[name]
        else
            error("Missing key '$name' in input_dict")
        end
    end

    return input_vector
end

"""
    mpc_build_forecast_vector(forecast_json::Dict{String,Any}, forecast_map::Dict{Int,Int}, col_names::Vector{String}, mpc_step::Int)

Builds the forecast vector for the MPC model at a given step.

# Arguments
- `forecast_json::Dict{String,Any}`  
  Dictionary containing forecasted temperature inputs.

- `forecast_map::Dict{Int,Int}`  
  Mapping from forecast column indices to vector positions.

- `col_names::Vector{String}`  
  Names of the columns corresponding to forecast inputs.

- `mpc_step::Int`  
  MPC step for which the forecast vector is constructed.

# Returns
- `forecast_vector::Vector{Float64}` — Vector of forecasted input values for the MPC step.

# Description
This function converts the forecast JSON into a vector arranged according to the forecast map, ensuring the MPC receives forecasted inputs in the correct order.
"""
function mpc_build_forecast_vector(
        forecast_json::Dict{String,Any},
        forecast_map::Dict{Int,Int},
        col_names::Vector{String}, 
        mpc_step::Int)

    start_index = forecast_json["StartIndex"]

    forecast_dict = forecast_json["TransformedInputsTemperature"][start_index + mpc_step - 1]
    
    Nf = length(forecast_map)
    forecast_vector = zeros(Float64, Nf)

    for (col_idx, mapped_pos) in forecast_map
        name = col_names[col_idx]
        if haskey(forecast_dict, name)
            forecast_vector[mapped_pos] = forecast_dict[name]
        else
            error("Missing key '$name' in forecast keys")
        end
    end
    
    return forecast_vector 
end

"""
    mpc_extract_HVAC_info(digital_twin::Dict{String, Any}, backward=0::Int)

Extracts HVAC operating status and mode for rooms from the digital twin.

# Arguments
- `digital_twin::Dict{String, Any}`  
  Dictionary containing HVAC status information and room order.

- `backward::Int = 0`  
  Number of time steps behind the current MPC step to extract HVAC status.

# Returns
- `status::Vector{Bool}` — Boolean vector indicating whether each HVAC unit is active.
- `mode::Vector{Bool}` — Boolean vector indicating whether each HVAC unit is in heating/cooling mode (`true` if operating mode ≥ 3).

# Description
This function parses the HVAC status dictionary for the given step and returns structured vectors representing unit activity and mode for MPC constraints.
"""
function mpc_HVAC_info(digital_twin::Dict{String, Any}, backward=0::Int)

    # Warning the hvac with no power are already discarded in the json !

    # Helper function to extract the number
    extract_index(s) = parse(Int, split(s, "_")[end])

    # Get the room order
    room_order = digital_twin["CoefsTempHeatingRowNames"]
    # Extract the hvac
    hvac_dict  = digital_twin["HVACStatus"][end-backward]

    status = get.(Ref(hvac_dict), "PW_"     .* string.(extract_index.(room_order)), missing)
    mode   = get.(Ref(hvac_dict), "OpMode_" .* string.(extract_index.(room_order)), missing)

    return Bool.(status), Int.(mode .≤ 2) 
end


function transform_setpoint(SP, SP_part, input_dict, digital_twin)
        
    # Find the past operating modes
    op_modes = digital_twin["HVACStatus"]
    idx_op = findfirst(d ->
        ZonedDateTime(d["end"]) == digital_twin["LastEndDateTime"],
        op_modes
    )
    
    idx = parse(Int, SP_part[2])
    if length(SP_part) ≥ 3
        lag = parse(Int, lstrip(SP_part[3],'l'))
        key = "AmbTemp_$(idx)_l$lag"
        idx_op = idx_op - lag 
    else
        key = "AmbTemp_$idx"
    end

    T = input_dict[key]
    
    op_mode = op_modes[idx_op]["OpMode_$idx"]

    if (op_mode ≥ 3) & (SP - T - SENSITIVITY ≤ 0)
        return SP
    elseif (op_mode ≤ 2) & (SP - T + SENSITIVITY ≥ 0)
        return SP
    end

    return 0.0
end