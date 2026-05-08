# =============================================================================
#  mpc_helper.jl — Pilot-agnostic MPC utilities
#
#  Functions here are reusable across all pilots (Ewh, Montcada, Karno, …).
#  Pilot-specific overrides (e.g. apply_warm_start!) live in each pilot's file.
# =============================================================================

# Reuse a single Gurobi environment across all solves to avoid printing the
# license banner on every mpc_update call.
const _GRB_ENV = Ref{Any}(nothing)

function _grb_env()
    if isnothing(_GRB_ENV[])
        _GRB_ENV[] = Gurobi.Env()
    end
    return _GRB_ENV[]
end

"""
    initialize_model(o::O) -> JuMP.Model

Create a JuMP model for the solver named in `o.solver` and apply the relative
MIP gap tolerance from `o.mip_gap`.  Falls back to HiGHS on any load failure.
Gurobi reuses a single shared environment so the license banner prints once.
"""
function initialize_model(o::O)
    local model
    silent = lowercase(o.loglevel) != "debug"
    try
        if o.solver == "Gurobi"
            model = Model(optimizer_with_attributes(
                () -> Gurobi.Optimizer(_grb_env()),
                MOI.Silent()                  => silent,
                MOI.RelativeGapTolerance()    => o.mip_gap,
            ))
        else
            solver = Symbol(o.solver)
            model  = Model(optimizer_with_attributes(
                getfield(@__MODULE__, solver).Optimizer,
                MOI.Silent()                  => silent,
                MOI.RelativeGapTolerance()    => o.mip_gap,
            ))
        end
        @info "Using solver $(o.solver)."
    catch
        @warn "Solver '$(o.solver)' not found or failed to load. Defaulting to HiGHS."
        model = Model(optimizer_with_attributes(
            HiGHS.Optimizer,
            MOI.Silent()               => silent,
            MOI.RelativeGapTolerance() => o.mip_gap,
        ))
    end
    return model
end

# -----------------------------------------------------------------------------
# Warm-start helpers
# -----------------------------------------------------------------------------

"""
    shift_warm_start(oy, Hu) -> Dict{Symbol, Any}

Shift the previous MPC solution forward by one time step to produce a
warm-start hint for the next receding-horizon solve.
The vacated last slot is filled by repeating the previous last value.

Works on the standard OY dict keys produced by any pilot's `package_results`.
"""
function shift_warm_start(oy::Dict{Symbol, Any}, Hu::Int)::Dict{Symbol, Any}
    shift_mat(M) = vcat(M[2:end, :], M[end:end, :])   # [Hu×n]: drop row 1, repeat last
    shift_vec(v) = vcat(v[2:end],    v[end])            # [Hu]:   drop idx 1, repeat last

    return Dict{Symbol, Any}(
        :x         => shift_mat(oy[:x]),
        :sp        => shift_mat(oy[:SP]),
        :u         => shift_mat(oy[:u]),
        :p1        => shift_vec(oy[:p1]),
        :δ_fridge  => shift_vec(round.(oy[:δ_fridge])),
        :δ_freezer => shift_vec(round.(oy[:δ_freezer])),
    )
end

"""
    apply_warm_start!(pilot, vars, warm, Hu)

Default no-op.  Each pilot overrides this for its own `vars` type to set
JuMP start values from the shifted warm-start dict produced by
`shift_warm_start`.
"""
function apply_warm_start!(::AbstractBuilding, vars, warm::Dict{Symbol, Any}, Hu::Int)
    return nothing
end

# -----------------------------------------------------------------------------
# MPC loop progress display
# -----------------------------------------------------------------------------

"""
    print_mpc_progress(t, N, oy, elapsed_s; bar_width, tick_s)

After each MPC solve, animate a 🐦 bar filling left-to-right on the same
terminal line (using `\\r`), then freeze it with status/cost/time and drop
to the next line.  Each step leaves a permanent row before the next begins.

# Arguments
- `t`         : current MPC step (1-based)
- `N`         : total number of steps
- `oy`        : OY result dict (must contain `:OPT_status`, `:OPT_cost`)
- `elapsed_s` : wall-clock seconds for this solve
- `bar_width` : number of bird slots (default 20)
- `tick_s`    : delay between each bird appearing (default 0.03 s)
"""
function print_mpc_progress(t::Int, N::Int, oy::Dict{Symbol,Any}, elapsed_s::Float64)
    status   = string(oy[:OPT_status])
    cost     = oy[:OPT_cost]
    gap      = get(oy, :OPT_gap, NaN)
    cost_str = isnan(cost) ? "    N/A   " : @sprintf("%10.2f", cost)
    gap_str  = isnan(gap)  ? "  N/A  " : @sprintf("%.2f%%", gap * 100)
    println(stdout, @sprintf("MPC  %4d/%-4d  %-14s  cost=%s  gap=%s  %6.1fs",
                             t, N, status, cost_str, gap_str, elapsed_s))
    flush(stdout)
    return nothing
end
