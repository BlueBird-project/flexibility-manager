"""
    Deprecated
"""
function super_batch(o::O,
                     digital_twin::Dict{String, Any},
                     sensors::Vector{Dict{String, Any}},
                     forecast_json::Dict{String, Any})::BatchDynamics

    op_modes = o.op_modes
    nmodes   = length(op_modes)

    M = Vector{Matrix{Float64}}(undef, nmodes) 
    Ξ = Vector{Matrix{Float64}}(undef, nmodes) 
    Ψ = Vector{Matrix{Float64}}(undef, nmodes) 
    local ξ1, Δ 

    for (ii, op_mode) in enumerate(op_modes)
        batch = build_batch(o, digital_twin, sensors, forecast_json; op_mode=op_mode)

        M[ii] = batch.M  
        Ξ[ii] = batch.Ξ
        Ψ[ii] = batch.Ψ 
        ξ1    = batch.ξ1
        Δ     = batch.Δ 
    end

    M = cat(M...; dims=(1,2)) # Block diagonal
    Ξ = cat(Ξ...; dims= 1   ) # Block 
    Ψ = cat(Ψ...; dims= 1   ) # Block 

    return BatchDynamics(M, Ξ, Ψ, ξ1, Δ)

end


"""
    Transform a MIMO ARX model over a horizon Hu in a batch static evolution

    X  = M ⋅ ξk + Ξ ⋅ U + Ψ ⋅ Δ
    ξk := [xk, xk-1, ...., xk-N, uk-1, ..., uk-M] is the stacked ARX inputs (initial condition)
    X  := [xk+1, xk+2, ...., xk+Hu] are the stacked states (decision variable)
    U  := [uk, uk+1, ..., uHu-1]    are the stacked controllable inputs (decision variable)
    D  := [dk, dk+1, ...., dHu-1]   are the stacked disturbances (forecasts) (known numbers)

    The ARX augmented state evolves according to 
    ξk+1 = A⋅ξk + B⋅uk + G⋅dk
"""
function build_batch(o::O,
                     digital_twin::Dict{String, Any},
                     sensors::Vector{Dict{String, Any}},
                     forecast_json::Dict{String, Any};
                     op_mode=op_mode::String)

    datetime = o.compute_datetime
    Hu = o.Hu

    # Note all put in the OX structure all of it

    # Get all the necessary ingredients
    A, ξ1, C = build_ξ1(digital_twin, sensors, datetime; op_mode=op_mode)
    B        = build_u(digital_twin; op_mode=op_mode)
    G, Δ     = build_GΔ(o, digital_twin, forecast_json; op_mode=op_mode)

    # Now augment all to get a big statis description
    M = let
        M = zeros(0, size(A,2))
        CA_pow = C*A
        for ii in 1:Hu
            M = [M; CA_pow]
            CA_pow = CA_pow*A
        end
        M
    end

    Ξ = batch_B(A, B, C, Hu)
    Ψ = batch_G(A, G, C, Hu)

    return BatchDynamics(M, Ξ, Ψ, ξ1, Δ)
end

function batch_B(A::Matrix, B::Matrix, C::Matrix, Hu::Int)
    ny, nx = size(C)
    nu = size(B, 2)
    Ξ = zeros(ny * Hu, nu * Hu)

    # Pre-calculate CB, CAB, CA^2B...
    blocks = Vector{Matrix{Float64}}(undef, Hu)
    A_pow = I(nx) # A^0

    for i in 1:Hu
        blocks[i] = C * A_pow * B
        A_pow = A_pow * A # Update for next power
    end

    for row in 1:Hu
        for col in 1:row
            row_idx = (row-1)*ny + 1 : row*ny
            col_idx = (col-1)*nu + 1 : col*nu
            Ξ[row_idx, col_idx] = blocks[row - col + 1]
        end
    end

    return Ξ
end

function batch_G(A::Matrix, G::Matrix, C::Matrix, Hu::Int)
    return batch_B(A, G, C, Hu)
end




## Helper function 
function build_ξ1(digital_twin::Dict{String, Any},
                  sensors::Vector{Dict{String, Any}},
                  datetime; op_mode)
    
    # Get the data from the right mode
    M, colInfo, _, row_name = extract_data_dynamics(digital_twin, op_mode)

    lag_info = digital_twin["CollectedLags"]
    nr = digital_twin["NumberRooms"]   
    nx = length(lag_info["AmbTemp"])
    nu = length(lag_info["TempSP" ]) 

    idxT, idxu = Int[], Int[]

    for ci in colInfo
        if ci.role == :state 
            push!(idxT, ci.index)
        elseif ci.role == :setpoint && ci.lag >= 1
            push!(idxu, ci.index)
        end
    end

    # Make sure that the dynamic is created with order convention
    col_names = getfield.(colInfo, :name)
    _, idxT = reorder_labels(col_names[idxT], idxT)
    _, idxu = reorder_labels(col_names[idxu], idxu)

     # Reorder base on the output order
    _, row_order = reorder_labels(row_name, Vector(1:length(row_name)))

    # Build Dynamics matrices 
    A_left  = M[row_order, idxT]
    A_mid   = M[row_order, idxu]
    A_right = zeros(length(row_order), nr) # To account for the decision part of the setpoints

    A = [A_left A_mid A_right]

    # Fill up A for the ξ state space representation
    A = let
        T = eltype(A)
        n_blocks = nx + nu
        n_cols   = n_blocks*nr

        # 1. Handle the nx identity blocks
        for ii in 1:nx-1

            row_block = zeros(T, nr, n_cols)
            row_block[:, (ii-1)*nr+1 : ii*nr] .= I(nr)
            
            A = [A; row_block]
        end

        A = [A; zeros(T, nr, n_cols)]

        # 3. Handle the nu identity blocks (starting from the 2nd index)
        if nu-1 >= 1
            for ii in nx+1:nx+nu-1
                row_block = zeros(T, nr, n_cols)
                row_block[:, (ii-1)*nr+1 : ii*nr] .= I(nr)
                A = [A; row_block]
            end
        end

        A 
    end

    ####################
    ## Build known input aka MPC initial conditions
    datetime_index = digital_twin["StartIndex"];

    # All the variables 
    dt_var = digital_twin["TransformedInputsTemperature"][datetime_index]

    # Select only ξ and transform the setpoints u
    # First select the temperatures
    filtered_T = filter(p -> startswith(p.first, "AmbTemp"), dt_var)

    # Then proceed as before:
    T_names = collect(keys(filtered_T))
    T_val   = collect(values(filtered_T))
    _, T_val_sorted = reorder_labels(T_names, T_val)

    # Now do the same for the temperature setpoints 
    # Red from the sensors 
    # TODO : Sensors will latter come from the demo database
    sensor_datetime_idx = find_index_from_datetime(sensors, datetime) 
    excluded_SP = "TempSP_22"

    # op_modes         = digital_twin["HVACStatus"]
    # idx_op_base      = findfirst(d ->
    #     ZonedDateTime(d["end"]) == digital_twin["LastEndDateTime"],
    #     op_modes
    # )

    # u = Float64[]
    # for lag in lag_info["TempSP"][begin:end-1]
    #     sensor_var   = sensors[sensor_datetime_idx - lag]
    #     filtered_SP = filter(
    #                     p -> startswith(p.first, "TempSP") && p.first != excluded_SP,
    #                     sensor_var)

    #     SP_names     = collect(keys(filtered_SP))
    #     SP_val       = collect(values(filtered_SP))
    #     _, SP_val_sorted = reorder_labels(SP_names, SP_val)

    #     for (k, (name, SP_raw)) in enumerate(zip(SP_names, SP_val_sorted))
    #         SP   = SP_raw + KELVIN_OFFSET
    #         parts = split(name, "_")           # ["TempSP", "2"], or ["TempSP","2","l3"]
    #         idx  = parse(Int, parts[2])

    #         # SP is current step → T is from the digital_twin inputs at matching lag
    #         T_key   = "AmbTemp_$(idx)_l$(lag+1)"
    #         idx_op  = idx_op_base

    #         T       = dt_var[T_key]
    #         op_mode = op_modes[idx_op]["OpMode_$idx"]

    #         SP = if (op_mode ≥ 3) && (SP - T - SENSITIVITY ≤ 0)
    #             SP          # cooling: setpoint already below temperature → keep
    #         elseif (op_mode ≤ 2) && (SP - T + SENSITIVITY ≥ 0)
    #             SP          # heating: setpoint already above temperature → keep
    #         else
    #             0.0         # setpoint is inactive for current mode → zero out
    #         end

    #         push!(u, SP)
    #     end
    # end

    u = Float64[]
    
    for lag in lag_info["TempSP"][begin:end-1]
        sensor_var = sensors[sensor_datetime_idx - lag]
        filtered_SP = filter(p -> startswith(p.first, "TempSP") && p.first != excluded_SP, sensor_var)

        # Then proceed as before:
        SP_names = collect(keys(filtered_SP))
        SP_val   = collect(values(filtered_SP))
        _, SP_val_sorted = reorder_labels(SP_names, SP_val)

        # Transform to Kelvin 
        SP_val_sorted = SP_val_sorted .+ KELVIN_OFFSET

        append!(u, SP_val_sorted)
    end

    ξ1 = [T_val_sorted ; u; zeros(nr)]

    # Build the state selection matrix 
    # [xk+1, xk+2, ..., xk+Hu] = C⋅[ξk+1,ξk+2,...,ξk+Hu]
    C = [I(nr) zeros(nr, nr*(nx+nu-1))]

    return A, ξ1, C
end


function build_u(digital_twin::Dict{String, Any}; op_mode)

    # Get the data from the right mode
    M, colInfoT, _, row_name = extract_data_dynamics(digital_twin, op_mode)

    lag_info = digital_twin["CollectedLags"]
    nx = length(lag_info["AmbTemp"])
    nu = length(lag_info["TempSP" ])         

    idx_B1 = Int[]
    for ci in colInfoT
        if ci.role == :setpoint && ci.lag == 0
            push!(idx_B1, ci.index)
        end
    end

    col_names = getfield.(colInfoT, :name)

    _, idx_B1 = reorder_labels(col_names[idx_B1], idx_B1)
    _, row_order = reorder_labels(row_name, Vector(1:length(row_name)))

    B = M[row_order, idx_B1]

    nr = size(B, 1)
    B = [B; zeros(nr*(nx-1), nr); I(nr); zeros(nr*(nu-1), nr)]

    return B
end

# Forecasts
function build_GΔ(o::O, digital_twin::Dict{String, Any},
                    forecast_json::Dict{String::Any}; op_mode::String)
    # Get the data from the right mode
    M, colInfoT, _, row_name = extract_data_dynamics(digital_twin, op_mode)

    lag_info = digital_twin["CollectedLags"]
    nr = digital_twin["NumberRooms"]   
    nx = length(lag_info["AmbTemp"])
    nu = length(lag_info["TempSP" ])         

    forecast_map = get_forecast_map(digital_twin; op_mode=op_mode)

    disturbance_cols_selected = sort(collect(keys(forecast_map)), by=k -> forecast_map[k])

    _, row_order = reorder_labels(row_name, Vector(1:length(row_name)))

    G = M[row_order, disturbance_cols_selected];
    G = [G ; zeros(nr*(nx+nu-1),size(G,2))]

    # Build the disturbance inputs 
    Δ = Float64[]
    for k in 1:o.Hu
        append!(Δ, build_d_inputs(forecast_json, forecast_map, k; op_mode = op_mode))
    end

    return G, Δ
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
function build_d_inputs(
        forecast_json::Dict{String,Any},
        forecast_map::Dict{Int,Int},
        k::Int;
        op_mode)

    if lowercase(op_mode) == "heat"
        col_names = forecast_json["CoefsTempHeatingColNames"]
    elseif lowercase(op_mode) == "cool"
        col_names = forecast_json["CoefsTempCoolingColNames"]
    else
        @error "Unvalid op mode $mode... Choose between 'heat' or 'cool'"
    end

    datetime_index = forecast_json["StartIndex"]
    forecast_dict  = forecast_json["TransformedInputsTemperature"][datetime_index + k - 1]
    
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


function get_forecast_map(digital_twin::Dict{String, Any} ; op_mode)

    _, colInfoT, _, _ = extract_data_dynamics(digital_twin, op_mode)

    forecast_columns_idx = Int[]
    for ci in colInfoT
        if ci.role == :disturbance 
            push!(forecast_columns_idx, ci.index)
        end
    end

    forecast_map = Dict{Int,Int}()
    # Matric M column number => position in forecasts_columns
    for (idx, j) in enumerate(forecast_columns_idx)
        forecast_map[j] = idx
    end

    return forecast_map

end


## Helper 1
function extract_data_dynamics(digital_twin::Dict{String, Any},
                           op_mode)
    if op_mode  == "heat" 
        M        = digital_twin["CoefsTempHeating"             ]
        colInfoT = digital_twin["CoefsTempHeatingColInfo"      ]
        colInfoE = digital_twin["CoefsHVACEnergyHeatingColInfo"]
        row_name = digital_twin["CoefsTempHeatingRowNames"     ]
    elseif op_mode == "cool" 
        M        = digital_twin["CoefsTempCooling"             ]
        colInfoT = digital_twin["CoefsTempCoolingColInfo"      ]
        colInfoE = digital_twin["CoefsHVACEnergyCoolingColInfo"]
        row_name = digital_twin["CoefsTempCoolingRowNames"     ]
    else
        @error "Undefined mode $(op_mode)."
    end     

    return M, colInfoT, colInfoE, row_name
end