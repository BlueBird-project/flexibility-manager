# =============================================================================
#  EWH MPC — skeleton dispatcher + stage functions
#
#  The pipeline follows the project-wide O / OX / OY convention:
#    O  = fixed metadata / code parameters  (horizon, solver, …)
#    OX = optimization inputs               (digital twin, forecasts, constraints, dynamics)
#    OY = optimization outputs              (returned Dict)
#
#  Call order (all dispatch on ::Ewh):
#    build_variables!  →  build_constraints!  →  build_objective!
#    →  optimize!  →  package_results
# =============================================================================

# Air-temperature state index for room r.
# State ordering: (T_air, T_prod) pairs → dairy, fp, meat, veg, freezer → indices 1,3,5,7,9
air_idx(r::Int) = 2r - 1

function fill_source_datetimes!(::Ewh, oy::Dict{Symbol, Any})
    ts = oy[:ox].digital_twin["state_space"]["measure_timestamp"]
    oy[:DT_datetime       ] = ts
    oy[:forecasts_datetime] = ts
    oy[:sensors_datetime  ] = ts
    return oy
end

"""
    initialize_model(o::O) -> JuMP.Model

Try to load the solver named in `o.solver`; fall back to HiGHS on failure.
"""
function initialize_model(o::O)
    try
        solver = Symbol(o.solver)
        model  = Model(getfield(@__MODULE__, solver).Optimizer)
        @info "Using solver " * o.solver * "."
        return model
    catch e
        @warn "Solver not found or failed. Defaulting to HiGHS."
        return Model(Gurobi.Optimizer)
        # return Model(HiGHS.Optimizer)
    end
end

# -----------------------------------------------------------------------------
# Entry point
# -----------------------------------------------------------------------------

"""
    mpc_update(::Ewh, o, ox) -> Dict{Symbol,Any}

Build and solve one MPC step for the EWH pilot.
Follows the O / OX / OY convention: `o` carries metadata, `ox` carries inputs,
the returned dict is OY.
"""
function mpc_update(pilot::Ewh, o::O, ox::OX)::Dict{Symbol, Any}

    model = initialize_model(o)
    vars  = build_variables!(pilot, model, o, ox)
    build_constraints!(pilot, model, vars, o, ox)
    build_objective!(pilot, model, vars, o, ox)

    optimize!(model)

    oy = package_results(pilot, model, vars, o, ox)

    JuMP.write_to_file(model, joinpath(pkgdir(@__MODULE__), "data", "EWH", "outputs", "model_dump.lp"))
    opt_output_to_file(joinpath(pkgdir(@__MODULE__), "data", "EWH", "outputs", "output.txt"), oy; kelvin = true)

    return oy
end

# -----------------------------------------------------------------------------
# Stage 1 — decision variables + algebraic expressions
# -----------------------------------------------------------------------------

"""
    build_variables!(::Ewh, model, o, ox) -> EwhVars

Declare all JuMP variables and compute derived algebraic expressions.
Reads dimensions and bounds from `ox`; horizon from `o`.
"""
function build_variables!(::Ewh, model::JuMP.Model, o::O, ox::OX)::EwhVars

    ss      = ox.digital_twin["state_space"]
    con     = ox.constraints
    Hu      = o.Hu
    nx      = ss["nx"]        # 10: (T_air, T_prod) × 5 rooms
    nu      = ss["nu"]        # 5:  one compressor input per room
    nfridge = nu - 1          # 4

    p1_low  = con[:p1_low]
    p1_high = con[:p1_high]
    p2_high = con[:p2_high]
    p3_high = con[:p3_high]

    # State trajectory — comfort bounds applied in build_constraints!
    @variable(model, x[i=1:nx, t=1:Hu])

    # Setpoints, one per room
    @variable(model, sp[r=1:nu, t=1:Hu])

    # Fridge allocated and requested power
    @variable(model, u[r=1:nfridge, t=1:Hu]     ≥ 0.0)
    @variable(model, u_req[r=1:nfridge, t=1:Hu] ≥ 0.0)

    # Compressors
    @variable(model, p1_low ≤ p1[t=1:Hu] ≤ p1_high)
    @variable(model, δ_fridge[t=1:Hu],  Bin)
    @variable(model, δ_freezer[t=1:Hu], Bin)

    # Grid and PV — limits are OX inputs (TODO: read from ox once wired)
    grid_buy_lim  = 1e6   # B̄ [kW] — TODO: from ox.constraints or digital twin
    grid_sell_lim = 1e6   # S̄ [kW] — TODO: from ox.constraints or digital twin
    @variable(model, 0.0 ≤ p_buy[t=1:Hu]  ≤ grid_buy_lim)
    @variable(model, 0.0 ≤ p_sell[t=1:Hu] ≤ grid_sell_lim)
    @variable(model, 0.0 ≤ PVused[t=1:Hu])
    @variable(model, 0.0 ≤ PVcurt[t=1:Hu])

    # Algebraic expressions
    p2    = δ_fridge  .* p2_high
    p3    = δ_freezer .* p3_high
    P_tot = p1 .+ p2 .+ p3
    U_req = vec(sum(u_req, dims=1))

    return EwhVars(
        x, sp, u, u_req, p1, δ_fridge, δ_freezer,
        p_buy, p_sell, PVused, PVcurt,
        p2, p3, P_tot, U_req,
    )
end

# -----------------------------------------------------------------------------
# Stage 2 — constraints
# -----------------------------------------------------------------------------

"""
    build_constraints!(::Ewh, model, vars, o, ox)

Add all EWH MPC constraints to `model` in place.
"""
function build_constraints!(::Ewh, model::JuMP.Model, v::EwhVars, o::O, ox::OX)

    ss      = ox.digital_twin["state_space"]
    con     = ox.constraints
    dyn     = ox.dynamics
    Hu      = o.Hu
    nu      = ss["nu"]
    nfridge = nu - 1
    Kgain   = Float64(ss["Kgain"][1])

    T_low  = [con[:T_fridge_diary_low],
              con[:T_fridge_finished_products_low],
              con[:T_fridge_meat_low],
              con[:T_fridge_vegetables_low],
              con[:T_freezer_low]]
    T_high = [con[:T_fridge_diary_high],
              con[:T_fridge_finished_products_high],
              con[:T_fridge_meat_high],
              con[:T_fridge_vegetables_high],
              con[:T_freezer_high]]
    SP_low  = [con[:SP_fridge_diary_low],
               con[:SP_fridge_finished_products_low],
               con[:SP_fridge_meat_low],
               con[:SP_fridge_vegetables_low],
               con[:SP_freezer_low]]
    SP_high = [con[:SP_fridge_diary_high],
               con[:SP_fridge_finished_products_high],
               con[:SP_fridge_meat_high],
               con[:SP_fridge_vegetables_high],
               con[:SP_freezer_high]]

    p1_high         = con[:p1_high]
    big_M_fridge    = 1e5   # TODO: move to ox.constraints or o
    big_M_freezer   = 1e5   # TODO: move to ox.constraints or o
    hysteresis_band = 1.0   # d [°C] — TODO: move to ox.constraints or o

    # TODO: parse from ox.forecast once forecasting service is wired up
    PV_fc  = zeros(Hu)
    P_rest = zeros(Hu)

    ## Batch MPC dynamics:  X = Ξ·U_full + M·ξ1 + Ψ·Δ
    # u_full stacks fridge controls (rows 1:nfridge) and freezer power p3 (row nu)
    u_full = [v.u; reshape(v.p3, 1, Hu)]
    @constraint(model, Dynamics, vec(v.x) .== dyn.Ξ * vec(u_full) .+
                                    dyn.M * dyn.ξ1       .+
                                    dyn.Ψ * dyn.Δ)

    ## Comfort: T̲^r ≤ T_air^r ≤ T̄^r  (air-temp states only)
    @constraint(model, Confort_low[r=1:nu, t=1:Hu], T_low[r]  ≤ v.x[air_idx(r), t])
    @constraint(model, Confort_hig[r=1:nu, t=1:Hu], v.x[air_idx(r), t] ≤ T_high[r])

    ## Setpoint bounds
    @constraint(model, Setpoints_limits[r=1:nu, t=1:Hu], SP_low[r] ≤ v.sp[r, t] ≤ SP_high[r])

    ## Compressor logic

    # u^{r,req} ≥ K^gain·(T^r_air − T^{r,sp})  ∀r ∈ {1..4}
    @constraint(model, Gain[r=1:nfridge, t=1:Hu],
        v.u_req[r, t] ≥ Kgain * (v.x[air_idx(r), t] - v.sp[r, t]))

    # Freezer hysteresis big-M (room nu=5)
    @constraint(model, Big_M_freezer1[t=1:Hu],
        v.x[air_idx(nu), t] - (v.sp[nu, t] + hysteresis_band) ≤
        big_M_freezer * v.δ_freezer[t])
    @constraint(model, Big_M_freezer2[t=1:Hu],
        (v.sp[nu, t] - hysteresis_band) - v.x[air_idx(nu), t] ≤
        big_M_freezer * (1 - v.δ_freezer[t]))


    # Fridge on/off kicks in when total demand exceeds modulated limit
    @constraint(model, Big_M_fridge[t=1:Hu],
        v.U_req[t] - p1_high ≤ big_M_fridge * v.δ_fridge[t])

    # Modulated compressor bounded by total requested load
    @constraint(model, Compressor_req_bound1[t=1:Hu], v.p1[t] ≤ v.U_req[t])

    # Power allocation: Σ u^r = p1 + p2,  u^r ≤ u^{r,req}
    @constraint(model, Power_alloc_fridge[t=1:Hu], sum(v.u[:, t]) == v.p1[t] + v.p2[t])
    @constraint(model, Compressor_req_bound2[r=1:nfridge, t=1:Hu], v.u[r, t] ≤ v.u_req[r, t])

    ## Power balance
    @constraint(model, Power_balance[t=1:Hu],
        v.p_buy[t] + v.PVused[t] == v.P_tot[t] + P_rest[t])
    @constraint(model, PV_balance[t=1:Hu], v.PVused[t] + v.PVcurt[t] ≤ PV_fc[t])
    @constraint(model, Power_sell[t=1:Hu], v.p_sell[t] ≤ PV_fc[t] - v.PVused[t])


    return nothing
end

# -----------------------------------------------------------------------------
# Stage 3 — objective
# -----------------------------------------------------------------------------

"""
    build_objective!(::Ewh, model, vars, o, ox)

Minimize net energy cost (buy cost minus sell revenue) over the horizon.
"""
function build_objective!(::Ewh, model::JuMP.Model, v::EwhVars, o::O, ox::OX)

    Hu = o.Hu
    Δt = o.Δt

    ToU_buy  = ox.forecast["ToU_buy"]
    ToU_sell = ox.forecast["ToU_sell"]

    @objective(model, Min,
        Δt * sum(v.p_buy[t] * ToU_buy[t] - v.p_sell[t] * ToU_sell[t]
                 for t in 1:Hu))

    return nothing
end

# -----------------------------------------------------------------------------
# Stage 4 — result packaging  (OY)
# -----------------------------------------------------------------------------

"""
    package_results(::Ewh, model, vars, o, ox) -> Dict{Symbol,Any}

Extract solution values (or NaN fallbacks on failure) into the OY output dict.
"""
function package_results(::Ewh, model::JuMP.Model,
                         v::EwhVars, o::O, ox::OX)::Dict{Symbol, Any}

    ss           = ox.digital_twin["state_space"]
    Hu           = o.Hu
    nx           = ss["nx"]
    nu           = ss["nu"]
    nfridge      = nu - 1
    status       = termination_status(model)
    has_solution = (status == MOI.OPTIMAL        ||
                    status == MOI.LOCALLY_SOLVED  ||
                    primal_status(model) == MOI.FEASIBLE_POINT)

    if has_solution
        return Dict{Symbol, Any}(
            :OPT_cost   => objective_value(model),
            :x          => Matrix(value.(v.x)'),  # transpose to [Hu × nx]
            :SP         => Matrix(value.(v.sp)'),  # transpose to [Hu × nu] for parse_OPT_output convention
            :u          => Matrix(value.(v.u)'),    # transpose to [Hu × nfridge]
            :u_req      => Matrix(value.(v.u_req)'), # transpose to [Hu × nfridge]
            :p1         => value.(v.p1),
            :p2         => value.(v.p2),
            :p3         => value.(v.p3),
            :δ_fridge   => value.(v.δ_fridge),
            :δ_freezer  => value.(v.δ_freezer),
            :P_tot      => value.(v.P_tot),
            :p_buy      => value.(v.p_buy),
            :p_sell     => value.(v.p_sell),
            :PVused     => value.(v.PVused),
            :PVcurt     => value.(v.PVcurt),
            :OPT_status => status,
            :o          => o,
            :ox         => ox,
        )
    else
        @warn "Solver failed: $status. Returning NaN/empty dict."
        return Dict{Symbol, Any}(
            :OPT_cost   => NaN,
            :x          => fill(NaN, Hu, nx),
            :SP         => fill(NaN, Hu, nu),  # [Hu × nu] convention
            :u          => fill(NaN, Hu, nfridge),
            :u_req      => fill(NaN, Hu, nfridge),
            :p1         => fill(NaN, Hu),
            :p2         => fill(NaN, Hu),
            :p3         => fill(NaN, Hu),
            :δ_fridge   => fill(NaN, Hu),
            :δ_freezer  => fill(NaN, Hu),
            :P_tot      => fill(NaN, Hu),
            :p_buy      => fill(NaN, Hu),
            :p_sell     => fill(NaN, Hu),
            :PVused     => fill(NaN, Hu),
            :PVcurt     => fill(NaN, Hu),
            :OPT_status => status,
            :o          => o,
            :ox         => ox,
        )
    end
end
