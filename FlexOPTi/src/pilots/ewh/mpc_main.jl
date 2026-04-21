
function mpc_update(::Ewh, o::O, ox::OX)::Dict{Symbol, Any}

    digital_twin  = ox.digital_twin
    sensors_json  = ox.sensors
    forecast_json = ox.forecast

    Hu = o.Hu
    ss = digital_twin["state_space"] 

    Δt = ss["sampling_time"]

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

    # Constraints

    # OPT Decision Variables




end #function mpc_update