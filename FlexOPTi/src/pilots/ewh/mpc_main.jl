
function mpc_update(::Ewh, o::O, ox::OX)::Dict{Symbol, Any}

    digital_twin  = ox.digital_twin
    sensors_json  = ox.sensors
    forecast_json = ox.forecast

    Hu = o.Hu
    ss = digital_twin["state_space"] 

    Δt = ss["sampling_time"]
    nx = ss["nx"] # State size
    nu = ss["nu"] # Control inputs size
    nfreezer = 1
    nfridge  = nu - nfreezer

    # TODO : ADD PV formulation
    # TODO : Add Sensor and forecast and price signals

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

    ## Constraints Loading
    # Asset constraints
    # --- Temperatures ---
    T_fridge_diary_low = constraints[:T_fridge_diary_low]                
    T_fridge_diary_high = constraints[:T_fridge_diary_high]               
    T_fridge_finished_products_low = constraints[:T_fridge_finished_products_low]    
    T_fridge_finished_products_high = constraints[:T_fridge_finished_products_high]   
    T_fridge_meat_low = constraints[:T_fridge_meat_low]                 
    T_fridge_meat_high = constraints[:T_fridge_meat_high]                
    T_fridge_vegetables_low = constraints[:T_fridge_vegetables_low]           
    T_fridge_vegetables_high = constraints[:T_fridge_vegetables_high]          
    T_freezer_low = constraints[:T_freezer_low]                   
    T_freezer_high = constraints[:T_freezer_high]                   
    # --- Setpoints ---
    SP_fridge_diary_low = constraints[:SP_fridge_diary_low]               
    SP_fridge_diary_high = constraints[:SP_fridge_diary_high]              
    SP_fridge_finished_products_low = constraints[:SP_fridge_finished_products_low]   
    SP_fridge_finished_products_high = constraints[:SP_fridge_finished_products_high]  
    SP_fridge_meat_low = constraints[:SP_fridge_meat_low]                
    SP_fridge_meat_high = constraints[:SP_fridge_meat_high]               
    SP_fridge_vegetables_low = constraints[:SP_fridge_vegetables_low]          
    SP_fridge_vegetables_high = constraints[:SP_fridge_vegetables_high]         
    SP_freezer_low = constraints[:SP_freezer_low]                    
    SP_freezer_high = constraints[:SP_freezer_high]                   
    # --- Power ---
    p_fridge_1_low = constraints[:p_fridge_1_low]                    
    p_fridge_1_high = constraints[:p_fridge_1_high]                   
    p_fridge_2_low = constraints[:p_fridge_2_low]                    
    p_fridge_2_high = constraints[:p_fridge_2_high]                  
    p_freezer_low = constraints[:p_freezer_low]                    
    p_freezer_high = constraints[:p_freezer_high]                    

    # Stack the constraints
    # State 
    x_low  = [T_fridge_diary_low,  T_fridge_finished_products_low,  T_fridge_meat_low,  T_fridge_vegetables_low,  T_freezer_low]
    x_high = [T_fridge_diary_high, T_fridge_finished_products_high, T_fridge_meat_high, T_fridge_vegetables_high, T_freezer_high]
    # Setpoints

    ## OPT Decision Variables
    # Control related
    @variable(model, x_low .≤ x[i=1:nx,t=1:Hu] .≤ x_high) # Full State Vector
    @variable(model, u[i=1:nu,t=1:Hu]                   ) # Fridge and Freezer Power Distributed 
    @variable(model, u_req[i=1,nfridge, t=1:Hu]         ) # Fridge power requested u_req ≥ Kgain (T - SP)
    @variable(model, sp[i=1:nu,t=1:Hu]                  ) # Setpoints
    @variable(model, δ[i=1:nu,t=1:Hu], Bin              ) # Fridge and Freezer ON-OFF
    @variable(model, p_fridge_1_low .≤ p1[t=1:Hu] .≤ p_fridge_1_high) # Power modulation on compressor 1

    # Algebraic related variables 
    u_fridges = u[1:nfridges,    :]
    u_freezer = u[nfridges+1:end,:]
    δ_fridges = δ[1:nfridges,    :]
    δ_freezer = δ[nfridges+1:end,:]

    # Fridge
    p2 = δ_fridges * p_fridge_2_high
    p3 = u_freezer
    p_tot = p1 + p2 + p3

    u_req_tot = sum(u_req, dims=1) # Fridge requested power 

    # Grid and PV related
    @variable(model, 0.0      ≤ p_grid[k=1:Hu   ] ≤ transformer_lim) # Power bought from the grid
    @variable(model, 0.0      ≤ PVused[k=1:Hu   ] ≤ PV             ) # PV used
    @variable(model, 0.0      ≤ PVcurt[k=1:Hu   ] ≤ PV             ) # PV curtailed

    # Metadata for Constraints
    # TODO : Include in the O structure 
    big_M_fridge    = 1000
    big_M_freezer   = 1000
    hysteresis_band = 1.0

    ## Constraints

    # State dynamics is represented by the batch MPC constraints 
    X = vec(x)
    U = vec(u) 
    U_req = vec(u_req) 

    M   = ox.dynamics.M
    Ξ   = ox.dynamics.Ξ
    Ψ   = ox.dynamics.Ψ
    ξ1  = ox.dynamics.ξ1
    Δ   = ox.dynamics.Δ  

    @constraint(model, X - Ξ*U - M*ξ1 - Ψ*Δ .==   0)

    # Electrical Constraints
    
    # Compressors Constraints
    # TODO : Define Kgain 
    @constraints(model, U_req .≥ Kgain * (X - SP) )     # Reqested power 
    @constraints(model, x - sp - hysteresis_band .≤ big_M_freezer .* δ_freezer ) # Freezer power 
    @constraints(model, sp - hysteresis_band - x .< big_M_freezer .* (1 - δ_freezer) ) # Freezer power 
    @constraints(model, u_req_tot - p_fridge_1_high .≤ big_M_fridge .* δ_fridge    )
    @constraints(model, u_freezer .== δ_freezer * p_freezer_high)

    @constraints(model, p1 .≤ U_req)
    @constraints(model, 0.0 .≤ u_fridges .≤ u_req)


    # Objectif 
    @objective(model, Min, Δt*sum(p_tot .* ToU_price))

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

end #function mpc_update