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
  - `:OPT_cost` — Optimal objective value. With `o.soft_temperature == true`
    this includes the comfort-violation penalty, so it is not a monetary cost.
  - `:OPT_energy_cost` — Energy cost alone, always comparable across runs.
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

  ## Comfort Violations
  - `:T_slack` — Temperature band violation per step and room (Hu × Nr), in K.
    Compare `:T` against the bounds to see the direction of a violation.

  A zero matrix when `o.soft_temperature == false`, since the bounds are then
  enforced as hard constraints.

# Description
This function constructs and solves a full hybrid MPC optimization model
for multi-room HVAC control. The formulation includes:

- Temperature state dynamics for both heating and cooling modes,
- Hybrid HVAC mode switching (MILP formulation),
- Setpoint activation logic via Big-M constraints,
- HVAC electrical power dynamics based on thermal response,
- PV and grid power balance constraints,
- Transformer and operational bounds,
- Optional soft temperature bounds (`o.soft_temperature`), where comfort
  violations are penalised in the objective rather than forbidden,
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

    model = initialize_model(o)

    # Asset constraints
    constraints = ox.constraints
    T_low   = constraints[:T_low  ]
    T_high  = constraints[:T_high ]
    p_low   = constraints[:p_low  ]
    p_high  = constraints[:p_high ]
    SP_low  = constraints[:SP_low ]
    SP_high = constraints[:SP_high]

    # Decision Variables
    if o.soft_temperature
        # Soft comfort band: T may leave [T_low, T_high], at a price (see objective).
        # One slack per room and step — a room cannot breach both bounds at once,
        # so a single symmetric variable covers either direction. It must stay
        # per-room: a shared slack would be paid for once and then widen the band
        # for every room at every step for free.
        @info "Temperature constraints: SOFT — band [$(round(T_low, digits=2)), " *
              "$(round(T_high, digits=2))] K penalised at $(o.slack_penalty) per K⋅step."
        @variable(model,             T[k=1:Nr*Hu]                 ) # Room temperature
        @variable(model, 0      ≤    s[k=1:Nr*Hu]                 ) # Comfort violation [K]
        @constraint(model, T .≥ T_low  .- s)
        @constraint(model, T .≤ T_high .+ s)
    else
        @info "Temperature constraints: HARD — band [$(round(T_low, digits=2)), " *
              "$(round(T_high, digits=2))] K enforced; the solve fails if it cannot be met."
        @variable(model, T_low ≤     T[k=1:Nr*Hu] ≤ T_high       ) # Room temperature
    end
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

    SP_mat = reshape(SP, Nr, Hu)'
    T_mat_full = reshape(T, Nr, Hu)'
    a_mat_full = reshape(a, Nr, Hu)'

    ## Select the right domain
    # For itereation 1 use the known previous temperature
    @constraint(model, SP_mat[1,:] .- prev_temp .- SENSITIVITY .≤  M*h[1,:]        .+ M*(1 .- a_mat_full[1,:])) # Cooling mode
    @constraint(model, SP_mat[1,:] .- prev_temp .+ SENSITIVITY .≥ -M*(1 .- h[1,:]) .- M*(1 .- a_mat_full[1,:])) # Heating mode
    # For itereation ≥ 2 use the decision temperature
    @constraint(model, [i=2:Hu], SP_mat[i,:] .- T_mat_full[i-1,:] .- SENSITIVITY .≤  M*h[i,:]        .+ M*(1 .- a_mat_full[i,:])) # Cooling mode
    @constraint(model, [i=2:Hu], SP_mat[i,:] .- T_mat_full[i-1,:] .+ SENSITIVITY .≥ -M*(1 .- h[i,:]) .- M*(1 .- a_mat_full[i,:])) # Heating mode
    
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

        # selecta = 37
        # count   = 1
        # for i in 1:size(Mc, 2)
        #     if Mc[selecta, i] != 0
        #         @printf("%-5d | %-5d | %10.5f | %10.5f\n", count, i, Mc[selecta, i], ξ1[i])
        #         count += 1 
        #     end
        # end

        # println("-------------------------------------------")

        # for i in 1:size(Ψc, 2)
        #     if Ψc[selecta, i] != 0
        #         @printf("%-5d | %-5d | %10.5f | %10.5f\n", count, i, Ψc[selecta, i], Δ[i])
        #         count += 1 
        #     end
        # end

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
            HVAC_map_cool["intercept"] * HVAC_inputs_cool["intercept"] +
            HVAC_map_cool["ComfTempHeating"] * Tbh[mpc_step] +
            HVAC_map_cool["ComfTempCooling"] * Tbc[mpc_step] +
            HVAC_map_cool["nhvac"]  * HVAC_inputs_cool["nhvac"] +
            sum(ΔT_map_cool   .* ΔT_cool)
        )

    end # MPC building loop

    # Power equations
    @constraint(model, power_constraint[mpc_step in 1:Hu],
        p_HVAC[mpc_step] == power_mode[mpc_step] .* p_heat_expr[mpc_step]
            + (1 - power_mode[mpc_step]) .* p_cool_expr[mpc_step]
    )

    # Objectif
    energy_cost = @expression(model, Δt*sum(p_grid .* ToU_price))

    if o.soft_temperature
        # Exact penalty: o.slack_penalty is set well above the largest shadow
        # price of the temperature bounds (see default_code_parameter), so the
        # optimizer gains nothing by buying comfort and the solution matches the
        # hard-constrained one whenever that one exists. When it does not, the
        # band is left by the smallest amount that restores feasibility, because
        # the penalty is linear in the violation.
        @objective(model, Min, energy_cost + o.slack_penalty*sum(s))
    else
        @objective(model, Min, energy_cost)
    end

    # Solve
    optimize!(model)

    status = termination_status(model)

    has_solution = (termination_status(model) == MOI.OPTIMAL || 
                    termination_status(model) == MOI.LOCALLY_SOLVED ||
                    primal_status(model) == MOI.FEASIBLE_POINT)

    if has_solution
        # Comfort violations — zeros when the band is enforced as a hard constraint
        T_slack = o.soft_temperature ? reshape(value.(s), Nr, Hu)' : zeros(Hu, Nr)
        max_violation = maximum(T_slack)
        if max_violation > EPSILON
            # Which room and step took the worst hit — the operator needs both.
            worst_step, worst_room = Tuple(argmax(T_slack))
            n_violating = count(>(EPSILON), T_slack)
            @warn "Comfort band violated by up to $(round(max_violation, digits=3)) K " *
                  "(room $worst_room, step $worst_step); $n_violating of $(Hu*Nr) room-steps outside the band."
        elseif o.soft_temperature
            @info "Comfort band held at every room and step; no slack used."
        end

        # TODO : Rename the symbols as string and use a convention
        oy = Dict(
            :OPT_cost       => objective_value(model),
            :OPT_energy_cost => value(energy_cost),
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
            :T_slack        => T_slack,
            :OPT_status     => status,
            :o              => o, 
            :ox             => ox
        );
    else
       @warn "Solver failed: $status. Returning NaN/Empty dict."
       if status == MOI.INFEASIBLE && !o.soft_temperature
           @warn "Temperature constraints are HARD — an unreachable comfort band is a likely cause. " *
                 "Set soft_temperature = true to penalise violations instead and keep a usable solution."
       end
      
       oy = Dict(
           :OPT_cost       => NaN,
           :OPT_energy_cost => NaN,
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
           :T_slack        => fill(NaN, Hu, Nr),
           :OPT_status     => status,
           :o              => o, 
           :ox             => ox
       )
    end

    # Debug
    JuMP.write_to_file(model, joinpath(pkgdir(@__MODULE__), "data", "model_dump.lp"))
    opt_output_to_file(joinpath(@__DIR__, "../../..", "data", o.output_file), oy; kelvin = false)

    return oy 

end # function,  

