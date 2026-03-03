"""
    mpc_update_montcada(o::O, ox::OX)

Builds and solves a Model Predictive Control (MPC) optimization problem
for building temperature and HVAC power management.

# Arguments
- `o::O` — MPC options and configuration, including control horizon,
  solver selection, MILP settings, objective parameters, output file name,
  and optional start datetime.

- `ox::OX` — MPC execution context containing:
  - Digital twin model parameters,
  - Sensor measurements,
  - Forecast data,
  - Operational constraints,
  - Initial state information.

# Returns
- `oy::Dict{Symbol,Any}` — Dictionary containing MPC optimization results:

  ## Objective & Status
  - `:OPT_cost` — Optimal objective value (total energy cost).
  - `:OPT_status` — Solver termination status.
  - `:o` — MPC options used.
  - `:ox` — MPC execution context.

  ## Temperature & Setpoints
  - `:T` — Predicted room temperatures over the horizon (Hu × Nr).
  - `:SP` — Raw HVAC temperature setpoints (Hu × Nr).
  - `:SP_transformed` — Setpoints after hybrid activation logic (Hu × Nr).
  - `:SP_active` — Binary activation matrix for setpoints (Hu × Nr).

  ## Power & Energy
  - `:p_HVAC` — Total HVAC electrical power consumption.
  - `:p_grid` — Power purchased from the grid.
  - `:PVused` — PV power used.
  - `:PVcurt` — PV power curtailed.

  ## Hybrid Mode & Thermal Contributions
  - `:balance_heat` — Binary heating balance indicator per time step.
  - `:balance_cool` — Binary cooling balance indicator per time step.
  - `:Tbh` — Heating-related temperature driving term.
  - `:Tbc` — Cooling-related temperature driving term.

# Description
This function constructs and solves a full hybrid MPC optimization model
for multi-room HVAC control. The formulation includes:

- Temperature state dynamics for both heating and cooling modes,
- Hybrid HVAC mode switching (MILP formulation),
- Setpoint activation logic via Big-M constraints,
- HVAC electrical power dynamics based on thermal response,
- PV and grid power balance constraints,
- Transformer and operational bounds,
- Time-of-Use (ToU) energy cost minimization objective.

The optimization problem is solved using the selected solver,
and structured results are returned for downstream analysis,
logging, or actuation.
"""
function mpc_update(::Montcada, o::O, ox::OX)::Dict{Symbol, Any}

    digital_twin  = ox.digital_twin
    sensors_json  = ox.sensors
    forecast_json = ox.forecast

    Hu = o.Hu
    Δt = digital_twin["DeltaTimeInHours"] 
    Nr = digital_twin["NumberRooms"     ]   

    threshold = fld(Nr,2) + 1 # Majority threshold
    # Use compute_datetime from options if provided, otherwise use current time
    if o.compute_datetime !== nothing
        mpc_start_time = o.compute_datetime
    else
        # Format current time with timezone to match expected format
        mpc_start_time = ZonedDateTime(Dates.now())
    end

    # Miscelanous 
    # TODO : Change transformer limits with real values 
    transformer_lim = 1e8
    P_max_total = 1e8

    # TODO : Change the PV signal from the forecast
    PV     = 0.0 # No PV used for this model at this stage.
    p_rest = 0.0 # Rest of the building consumption 

    # TODO : Change the ToU signal
    ToU_price    = repeat([10.0], Hu)
    # ToU_price[1] = 10.50
    
    # If MILP then the operating mode is a decision variable
    hvac_status, h = mpc_HVAC_info(digital_twin)
    hvac_status = repeat(hvac_status', Hu)
    power_mode = Int.(sum(h) > threshold) |> x -> fill(x, Hu)
    h = repeat(h', Hu)

    # Init solver
    function initialize_JuMP_model(o::O)
        try
            solver = Symbol(o.solver)
            model = Model(getfield(@__MODULE__, Symbol(solver)).Optimizer)
            @info "Using solver "*o.solver*"."
            return model
        catch e
            @warn "Solver not found or failed. Defaulting to HiGHS." # exception=(e, catch_backtrace())
            model = Model(HiGHS.Optimizer)
            return model
        end
    end
    model = initialize_JuMP_model(o)

    # Asset constraints
    constraints = ox.constraints
    T_low   = constraints[:T_low  ]
    T_high  = constraints[:T_high ]
    p_low   = constraints[:p_low  ]
    p_high  = constraints[:p_high ]
    SP_low  = constraints[:SP_low ]
    SP_high = constraints[:SP_high]

    # Decision Variables 
    @variable(model, T_low  ≤      T[k=1:Nr*Hu] ≤ T_high         ) # Room temperature 
    @variable(model, p_low  ≤ p_HVAC[k=1:Hu   ] ≤ p_high         ) # Room power consumption 
    @variable(model, SP_low ≤     SP[k=1:Nr*Hu] ≤ SP_high        ) # HVAC temperature setpoint
    @variable(model, 0      ≤ p_grid[k=1:Hu   ] ≤ transformer_lim) # Power bought from the grid
    @variable(model, 0      ≤ PVused[k=1:Hu   ] ≤ PV             ) # PV used
    @variable(model, 0      ≤ PVcurt[k=1:Hu   ] ≤ PV             ) # PV curtailed

    @variable(model, bh[k=1:Hu], Bin)   # Balance cool
    @variable(model, bc[k=1:Hu], Bin)   # Balance heat

    # PV constraints and grid
    @constraint(model, PVused .+ PVcurt .== PV)
    @constraint(model, p_grid .+ PVused .== p_HVAC .+ p_rest)

    ## Transformation of the setpoints 
    M = 1000     # Big-M constraint -> TODO tune latter 
    @variable(model, 0.0 .≤ SP_transformed[1:Nr*Hu] ≤ SP_high)
    @variable(model, a[1:Nr*Hu], Bin)    # a = 1 ⇒ Tsp = Tsp | a = 0 ⇒ Tsp = 0 










    # Get the previous temperatures
    inputs = digital_twin["TransformedInputsTemperature"]
    inputs_data_idx = find_index_from_datetime(inputs, mpc_start_time)
    # Select only the 1 lag ambient temperature
    prev_temp = filter(inputs[inputs_data_idx]) do (k,v)
                    startswith(k,"AmbTemp") && endswith(k, "l1")
                end
    prev_temp = [kv[2] for kv in sort(collect(prev_temp); by = kv -> parse(Int, match(r"AmbTemp_(\d+)_l1", kv[1]).captures[1]))]
    ## Select the right domain 
    # For itereation 1 use the known previous temperature
    @constraint(model, SP[1,:] .- prev_temp .- SENSITIVITY .≤  M*h[1,:]        .+ M*(1 .- a[1,:])) # Cooling mode 
    @constraint(model, SP[1,:] .- prev_temp .+ SENSITIVITY .≥ -M*(1 .- h[1,:]) .- M*(1 .- a[1,:])) # Heating mode
    # For itereation ≥ 2 use the decision temperature
    @constraint(model, [i=2:Hu], SP[i,:] .- T[i-1,:] .- SENSITIVITY .≤  M*h[i,:]        .+ M*(1 .- a[i,:])) # Cooling mode 
    @constraint(model, [i=2:Hu], SP[i,:] .- T[i-1,:] .+ SENSITIVITY .≥ -M*(1 .- h[i,:]) .- M*(1 .- a[i,:])) # Heating mode
    
    # Select the right control law
    @constraint(model, SP_transformed .≤  a   * M         )
    @constraint(model, SP_transformed .≥ -a   * M         )
    @constraint(model, SP_transformed .≤  SP .+ (1 .-a )*M)
    @constraint(model, SP_transformed .≥  SP .- (1 .-a )*M)
    # Same with indicator constraints
    # @constraint(model,  a .⇒{SP_transformed .== SP})
    # @constraint(model, ¬a .⇒{SP_transformed .== 0})


    # Batch dynamics 
    #function dynamics_constraints!(model, ox, power_mode)
        Mh  = ox.dynamics["heat"].M
        Ξh  = ox.dynamics["heat"].Ξ
        Ψh  = ox.dynamics["heat"].Ψ
        Mc  = ox.dynamics["cool"].M
        Ξc  = ox.dynamics["cool"].Ξ
        Ψc  = ox.dynamics["cool"].Ψ
        ξ1  = ox.dynamics["heat"].ξ1
        Δ   = ox.dynamics["heat"].Δ  

        Mdyn = 1000 # Big-M
        h_vec = reshape(h', :)
        # @constraint(model, T - Ξh * SP_transformed - Mh * ξ1 - Ψh * Δ .≤   Mdyn *   h_vec      )  # Heating Mode 
        # @constraint(model, T - Ξh * SP_transformed - Mh * ξ1 - Ψh * Δ .≥ - Mdyn *   h_vec      )  # Heating Mode 
        # @constraint(model, T - Ξc * SP_transformed - Mc * ξ1 - Ψc * Δ .≤   Mdyn *  (1 .- h_vec))  # Colling Mode 
        # @constraint(model, T - Ξc * SP_transformed - Mc * ξ1 - Ψc * Δ .≥ - Mdyn *  (1 .- h_vec))  # Colling Mode

        @constraint(model, T - Ξc * SP_transformed - Mc * ξ1 - Ψc * Δ .==   0      )  # Cooling Mode 
        # Todo : We want the dynamic to evolve first according to the real mode, but then to switch all to the majority mode
        #TODO : Make sure the heating cooling mode is logical. If wrong mode for someone, change the mode

        selecta = 37
        count   = 1
        for i in 1:size(Mc, 2)
            if Mc[selecta, i] != 0
                @printf("%-5d | %-5d | %10.5f | %10.5f\n", count, i, Mc[selecta, i], ξ1[i])
                count += 1 
            end
        end

        println("-------------------------------------------")

        for i in 1:size(Ψc, 2)
            if Ψc[selecta, i] != 0
                @printf("%-5d | %-5d | %10.5f | %10.5f\n", count, i, Ψc[selecta, i], Δ[i])
                count += 1 
            end
        end

        #return nothing
    #end

    #dynamics_constraints!(model, ox, power_mode)

    # Preallocate space
    Tbh = Vector{AffExpr}(undef,Hu)
    Tbc = similar(Tbh) 

    p_heat_expr = Vector{JuMP.AffExpr}(undef, Hu)
    p_cool_expr = Vector{JuMP.AffExpr}(undef, Hu)

    ################
    ### debugging ##
    ################
    # Debug function 
    # function fake_setpoints(jsonfile::String, Hu::Int, Nr::Int)
    
    #     data = JSON.parse(read(jsonfile, String))  # Vector{Dict{String,Any}}
    
    #     target = ZonedDateTime(mpc_start_time)
    
    #     # convert the stored start string into ZonedDateTime
    #     idx = findfirst(r -> ZonedDateTime(r["start"]) == target, data)
    #     idx === nothing && error("Timestamp $(target) not found in log")
    
    #     future_sensors = data[idx:idx+Hu-1]  
    
    #     # Extract and order the temperature setpoints
    #     SP_fake = zeros(Float64, Hu, Nr)
    #     for (i,sensor_data) ∈ enumerate(future_sensors)
    #         SP_fake[i,:] = [
    #         sensor_data[k] for k in sort(
    #             filter(k -> startswith(k, "TempSP_") && k != "TempSP_22", collect(keys(sensor_data))),
    #             by = k -> parse(Int, split(k, "_")[2])
    #         )
    #         ] 
    #     end
    
    #     return SP_fake .+ KELVIN_OFFSET 
    
    # end

    # fakesensorfile = "/home/kahka/DTU/BlueBird/flexmanager/FM/data/df_predict.json"
    # SP_low  = fake_setpoints(fakesensorfile, Hu, Nr)
    # @constraint(model, SP .== SP_low) # Could be 0 or fake setpoint

    # active = [31, 33, 35];
    # # @constraint(model, [ii = 1:Nr; ii ∉ active], SP_transformed[1, ii] == SP[1, ii])
    # # @constraint(model, [ii ∈ active], SP_transformed[1, ii] == 0)

    # @constraint(model, [ii = 1:Nr; ii ∉ active], SP_transformed[2, ii] == SP[2, ii])
    # @constraint(model, [ii ∈ active], SP_transformed[2, ii] == 0)
    ################
    ################


    # MPC building loop
    for mpc_step in 1:Hu
        @info "Building MPC constraint t + $(mpc_step-1) to t + $mpc_step."

        ## Power state evolution ##
        HVAC_map_heat, ΔT_map_heat, T0_heat, HVAC_inputs_heat =
        mpc_power_dynamics(digital_twin, mpc_step, mpc_start_time; mode="heat")

        HVAC_map_cool, ΔT_map_cool, T0_cool, HVAC_inputs_cool =
        mpc_power_dynamics(digital_twin, mpc_step, mpc_start_time; mode="cool")

        # Build the delta T logic
        T_mat = reshape(T, Nr, Hu)'
        if isempty(T0_heat)
            ΔT_heat = T_mat[mpc_step, :]   .- T_mat[mpc_step-1, :]    
        else
            ΔT_heat = T_mat[mpc_step, :]   .- T0_heat
        end
        if isempty(T0_cool)
            ΔT_cool = T_mat[mpc_step-1, :] .- T_mat[mpc_step, :]
        else
            ΔT_cool = T0_cool          .- T_mat[mpc_step, :]
        end

        # Define the transformations
        # TODO : ADD Φ check if the hvac is working or not
        # TODO : If h is a decision variable this is not linear
        a_mat = reshape(a, Nr, Hu)'
        @constraint(model, [r=1:Nr], bh[mpc_step] ≥ a_mat[mpc_step,r]*h[mpc_step,r])
        @constraint(model,           bh[mpc_step] ≤ sum(a_mat[mpc_step,r]*h[mpc_step,r] for r=1:Nr))
        @constraint(model, [r=1:Nr], bc[mpc_step] ≥ a_mat[mpc_step,r]*(1-h[mpc_step,r]))
        @constraint(model,           bc[mpc_step] ≤ sum(a_mat[mpc_step,r]*(1-h[mpc_step,r]) for r=1:Nr))
        if mpc_step == 1
            Tbh[1] = HVAC_inputs_heat["Tbh"]
            Tbc[1] = HVAC_inputs_cool["Tbc"]
        else
            forecasts_idx = find_index_from_datetime(forecast_json["TransformedInputsTemperature"], mpc_start_time)
            outdoorTemperature = forecast_json["TransformedInputsTemperature"][forecasts_idx + mpc_step - 1]["outdoorTemperature"]
            Tbh[mpc_step] = bh[mpc_step] * ( HVAC_inputs_heat["Trefh"] - outdoorTemperature) 
            Tbc[mpc_step] = bc[mpc_step] * (-HVAC_inputs_cool["Trefc"] + outdoorTemperature)
        end
        
        # Power models
        p_heat_expr[mpc_step] = @expression(model,
            HVAC_map_heat["intercept"] * HVAC_inputs_heat["intercept"] +
            HVAC_map_heat["ComfTempHeating"] * Tbh[mpc_step] +
            HVAC_map_heat["ComfTempCooling"] * Tbc[mpc_step] +
            HVAC_map_heat["nhvac"]  * HVAC_inputs_heat["nhvac"] +
            sum(ΔT_map_heat   .* ΔT_heat)
        )
        p_cool_expr[mpc_step] = @expression(model,
            HVAC_map_cool["intercept"] * HVAC_inputs_heat["intercept"] +
            HVAC_map_cool["ComfTempHeating"] * Tbh[mpc_step] +
            HVAC_map_cool["ComfTempCooling"] * Tbc[mpc_step] +
            HVAC_map_cool["nhvac"]  * HVAC_inputs_heat["nhvac"] +
            sum(ΔT_map_cool   .* ΔT_cool)
        )

    end # MPC building loop

    # Power equations
    @constraint(model, power_constraint[mpc_step in 1:Hu],
        p_HVAC[mpc_step] == power_mode[mpc_step] .* p_heat_expr[mpc_step]
            + (1 - power_mode[mpc_step]) .* p_cool_expr[mpc_step]
    )

    # Objectif 
    @objective(model, Min, Δt*sum(p_grid .* ToU_price))

    # Solve
    optimize!(model)

    status = termination_status(model)

    has_solution = (termination_status(model) == MOI.OPTIMAL || 
                    termination_status(model) == MOI.LOCALLY_SOLVED ||
                    primal_status(model) == MOI.FEASIBLE_POINT)

    if has_solution
        # TODO : Rename the symbols as string and use a convention
        oy = Dict(
            :OPT_cost       => objective_value(model),
            :T              => reshape(value.(T),  Nr, Hu)',
            :SP             => reshape(value.(SP), Nr, Hu)',
            :SP_transformed => reshape(value.(SP_transformed), Nr, Hu)',
            :p_HVAC         => value.(p_HVAC),
            :p_grid         => value.(p_grid),
            :PVused         => value.(PVused),
            :PVcurt         => value.(PVcurt),
            :SP_active      => reshape(value.(a), Nr, Hu)',
            :balance_heat   => value.(bh),
            :balance_cool   => value.(bc),
            :Tbh            => value.(Tbh),
            :Tbc            => value.(Tbc),
            :OPT_status     => status, 
            :o              => o, 
            :ox             => ox
        );
    else
       @warn "Solver failed: $status. Returning NaN/Empty dict."
      
       oy = Dict(
           :OPT_cost       => NaN,
           :T              => fill(NaN, Hu, Nr),
           :SP             => fill(NaN, Hu, Nr),
           :SP_transformed => fill(NaN, Hu, Nr),
           :p_HVAC         => NaN,
           :p_grid         => NaN,
           :PVused         => NaN,
           :PVcurt         => NaN,
           :SP_active      => fill(NaN, Hu, Nr),
           :balance_heat   => fill(NaN, Hu),
           :balance_cool   => fill(NaN, Hu),
           :Tbh            => fill(NaN, Hu),
           :Tbc            => fill(NaN, Hu),
           :OPT_status     => status, 
           :o              => o, 
           :ox             => ox
       )
    end

    # Debug
    JuMP.write_to_file(model, joinpath(@__DIR__, "../../../data/")*"model_dump.lp")
    opt_output_to_file(joinpath(@__DIR__, "../../../data/")*o.output_file, oy; kelvin = false)

    return oy 

end # function,  

