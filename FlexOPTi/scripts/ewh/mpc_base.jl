# =============================================================================
#  mpc_base.jl — Receding-horizon MPC for the 5-zone cold room
#
#  Setup
#  -----
#  - Model  : cold_room_model_continuous.json (continuous-time state space)
#  - Δt     : 300 s (5 min) steps via ZOH discretization
#  - Horizon: Hu = 144 steps = 12 hours
#  - Sim    : 12 hours (8:00 → 20:00), 144 receding-horizon solves
#
#  True world vs MPC knowledge
#  ---------------------------
#  - External temperature: constant 20 °C (both worlds agree)
#  - Door openings: Poisson arrivals in the TRUE simulation;
#    the MPC forecasts zero door openings (perfect ignorance).
#
#  Price signal (24 h, starting at 8 am)
#  --------------------------------------
#  Expensive morning → cheap midday → moderate evening → cheap overnight
#  The 24-h window ensures the MPC can look 12 h ahead at every step.
# =============================================================================

using Pkg
Pkg.activate(joinpath(@__DIR__, "..", ".."))

using JSON
using JuMP
using LinearAlgebra
using Random
using Printf
using Plots

using HiGHS
solver_optimizer = HiGHS.Optimizer

Random.seed!(42)

# ── Simulation parameters ──────────────────────────────────────────────────

const Δt_s   = 300.0                       # time step [s]
const Δt_h   = Δt_s / 3600.0              # time step [h]
const Hu     = Int(12 * 3600 / Δt_s)      # MPC horizon: 144 steps = 12 h
const N_sim  = Hu                          # simulation length: 12 h
const T_amb      = 18.0   # constant ambient temperature [°C] (underground)
const n_rooms    = 5      # rooms: dairy, fp, meat, veg, freezer
const σ_process  = 0.05   # process noise std per step [°C], added to all states

const DT_FILE = joinpath(@__DIR__, "..", "..", "data", "EWH", "inputs",
                          "cold_room_model_continuous.json")

# ── Temperature constraints [°C] ──────────────────────────────────────────

const T_low  = [ 2.0,  2.0,  0.0,  2.0, -40.0]  # dairy, fp, meat, veg, freezer
const T_high = [ 8.0,  7.0,  9.0,  7.0, -18.0]

# Per-zone compressor power limits [W]  (continuous relaxation, no binary)
const U_max = [2500.0, 2500.0, 2500.0, 2500.0, 2700.0]  # fridges / freezer
const U_min = zeros(n_rooms)

# ── Electricity price signal ──────────────────────────────────────────────
#
#  N_price = N_sim + Hu = 288 steps = 24 h, starting at 8 am.
#  Layout (hours offset from 8 am):
#    [0,  2) h  → 0.35 €/kWh   expensive morning
#    [2,  4) h  → 0.28 €/kWh   declining
#    [4,  6) h  → 0.10 €/kWh   cheap midday
#    [6,  8) h  → 0.08 €/kWh   cheapest
#    [8, 10) h  → 0.22 €/kWh   rising
#    [10,12) h  → 0.30 €/kWh   evening peak
#    [12,16) h  → 0.12 €/kWh   falling overnight
#    [16,24) h  → 0.06 €/kWh   cheap overnight

function make_price_signal(N_price::Int, Δt_h::Float64)
    breakpoints = [  0,   2,  4,  6,   8,  10,  12,  16, 19, 24]  # hours from 8 am
    prices      = [0.35, 0.28, 0.10, 0.08, 0.22, 0.30, 0.14, 0.18 ,0.12]

    signal = zeros(N_price)
    for k in 1:N_price
        t_h = (k - 1) * Δt_h           # elapsed hours from simulation start
        seg = searchsortedlast(breakpoints, t_h, lt = ≤)
        seg = clamp(seg, 1, length(prices))
        signal[k] = prices[seg]
    end
    return signal
end

# ── ZOH discretization ────────────────────────────────────────────────────

function discretize_zoh(Ac, Bc, Ec, Δt::Float64)
    nx, nu, nd = size(Ac, 1), size(Bc, 2), size(Ec, 2)
    n = nx + nu + nd
    M = zeros(n, n)
    M[1:nx,      1:nx]           = Ac
    M[1:nx,      nx+1:nx+nu]     = Bc
    M[1:nx,      nx+nu+1:end]    = Ec
    Md = exp(M * Δt)
    return Md[1:nx, 1:nx], Md[1:nx, nx+1:nx+nu], Md[1:nx, nx+nu+1:end]
end

# ── Batch dynamics matrices ───────────────────────────────────────────────
#
#  X = M·x0 + Ξ·U + Ψ·Δ
#  X [nx·Hu],  U [nu·Hu],  Δ [nd·Hu]

function build_batch(A, B, E, Hu::Int)
    nx = size(A, 1)
    nu = size(B, 2)
    nd = size(E, 2)

    # M: [nx·Hu × nx]
    M  = zeros(nx * Hu, nx)
    Ap = copy(A)
    for t in 1:Hu
        M[(t-1)*nx+1 : t*nx, :] = Ap
        Ap = Ap * A
    end

    # Block builder for lower-triangular Toeplitz structure (used for Ξ and Ψ)
    function toeplitz_block(A, G, ncol_block)
        blocks = Vector{Matrix{Float64}}(undef, Hu)
        blocks[1] = G
        Ap = copy(A)
        for i in 2:Hu
            blocks[i] = Ap * G
            Ap = Ap * A
        end
        T = zeros(nx * Hu, ncol_block * Hu)
        for row in 1:Hu, col in 1:row
            ri = (row-1)*nx + 1 : row*nx
            ci = (col-1)*ncol_block + 1 : col*ncol_block
            T[ri, ci] = blocks[row - col + 1]
        end
        return T
    end

    Ξ = toeplitz_block(A, B, nu)
    Ψ = toeplitz_block(A, E, nd)

    return M, Ξ, Ψ
end

# ── Stacked disturbance vector ─────────────────────────────────────────────
#  At each step k: dk = [T_amb; Q_door_1; ...; Q_door_5]  (nd = 6)

function make_disturbance_vector(T_amb_vec::Vector, door_loads::Matrix, Hu::Int)
    nd = 1 + size(door_loads, 1)
    Δ  = zeros(nd * Hu)
    for k in 1:Hu
        base = (k-1) * nd
        Δ[base + 1]       = T_amb_vec[k]
        Δ[base+2 : base+nd] = door_loads[:, k]
    end
    return Δ
end

# ── Poisson door openings ─────────────────────────────────────────────────
#
#  TRUE simulation: each room independently draws n ~ Poisson(λ·Δt_min).
#  If n ≥ 1 openings occur in the 5-min window, the room receives a heat
#  load proportional to total open time (assuming each opening = 1 min):
#    Q_door = min(n, Δt_min) * door_load / Δt_min
#  The MPC forecasts d_door = 0 for all rooms at all steps.

function generate_door_schedule(N_steps::Int; λ_per_hour::Float64 = 6.0,
                                 door_load::Float64 = 4500.0,
                                 Δt_min::Float64 = 5.0)
    λ = λ_per_hour / 60.0            # openings per minute per room
    schedule = zeros(n_rooms, N_steps)
    for r in 1:n_rooms
        for t in 1:N_steps
            n_open = Poisson_approx(λ * Δt_min)
            if n_open > 0
                open_min = min(n_open, Δt_min)   # can't be open > full step
                schedule[r, t] = open_min / Δt_min * door_load
            end
        end
    end
    return schedule
end

# Poisson sample using the Knuth / inverse-transform method (no extra package needed).
# Generates k ~ Poisson(λ) by counting exponential inter-arrival times.
function Poisson_approx(λ::Float64)::Int
    L = exp(-λ)
    k = 0
    p = 1.0
    while p > L
        p *= rand()
        k += 1
    end
    return k - 1
end

# ── Single-step LP MPC solve ──────────────────────────────────────────────
#
#  Optimizes u[1:nu, 1:Hu] given x0 and forecasts.
#  Returns u_opt[:, 1] — the action applied at this step.

function solve_mpc(A, B, E, M_batch, Ξ, Ψ, x0::Vector{Float64},
                   price_slice::Vector{Float64})

    nx   = size(A, 1)
    nu   = size(B, 2)
    nd   = size(E, 2)
    air  = [1, 3, 5, 7, 9]          # air-temperature state indices per room

    # MPC forecasts: no door openings, constant ambient temperature
    T_amb_fc  = fill(T_amb, Hu)
    door_fc   = zeros(n_rooms, Hu)
    Δ_mpc     = make_disturbance_vector(T_amb_fc, door_fc, Hu)

    # Precompute the disturbance contribution (constant, not a decision variable)
    dist_contrib = Ψ * Δ_mpc              # [nx·Hu]
    x0_contrib   = M_batch * x0           # [nx·Hu]
    offset       = x0_contrib .+ dist_contrib   # [nx·Hu]

    model = Model(solver_optimizer)
    set_silent(model)

    # Decision variables: control inputs [nu × Hu]
    @variable(model, U_min[r] ≤ u[r=1:nu, t=1:Hu] ≤ U_max[r])

    # Predicted states via batch dynamics (linear expressions)
    # X = offset + Ξ·vec(u)
    # Reshape Ξ columns to match column-major vec(u): u[:,1], u[:,2], ...
    u_vec = vec(u)      # JuMP: column-major [u[1,1]; u[2,1]; ...; u[nu,1]; u[1,2]; ...]

    X_pred = offset .+ Ξ * u_vec   # [nx·Hu] AffExpr vector

    # Temperature comfort constraints for each room and time step
    for t in 1:Hu
        base = (t-1) * nx
        for r in 1:n_rooms
            xi = base + air[r]
            @constraint(model, T_low[r]  ≤ X_pred[xi])
            @constraint(model, X_pred[xi] ≤ T_high[r])
        end
    end

    # Objective: minimize energy cost over horizon
    @objective(model, Min,
        Δt_h * sum(price_slice[t] * sum(u[r, t] for r in 1:nu) for t in 1:Hu))

    optimize!(model)

    status = termination_status(model)
    if status ∈ (MOI.OPTIMAL, MOI.LOCALLY_SOLVED) || primal_status(model) == MOI.FEASIBLE_POINT
        X_pred_mat = reshape(offset .+ Ξ * value.(vec(u)), nx, Hu)  # [nx × Hu]
        return value.(u[:, 1]), value.(u), X_pred_mat, status
    else
        @warn "MPC infeasible at this step: $status — applying zero control."
        return zeros(nu), zeros(nu, Hu), fill(NaN, nx, Hu), status
    end
end

# ── Main ──────────────────────────────────────────────────────────────────

function main()

    # Load and discretize the continuous model
    data = JSON.parsefile(DT_FILE)
    ss   = data["state_space"]

    to_mat(rows) = Matrix{Float64}(reduce(hcat, rows)')
    Ac = to_mat(ss["A"])
    Bc = to_mat(ss["B"])
    Ec = to_mat(ss["E"])
    x0 = Vector{Float64}(ss["x0"])

    Ad, Bd, Ed = discretize_zoh(Ac, Bc, Ec, Δt_s)
    M_batch, Ξ, Ψ = build_batch(Ad, Bd, Ed, Hu)

    nx = size(Ad, 1)
    nu = size(Bd, 2)
    nd = size(Ed, 2)

    @info "Model: nx=$nx, nu=$nu, nd=$nd  |  Δt=$(Int(Δt_s))s, Hu=$Hu steps"

    # 24-hour price signal (starts at 8 am)
    N_price  = N_sim + Hu
    price    = make_price_signal(N_price, Δt_h)

    # TRUE door-opening schedule (Poisson, unknown to MPC)
    Δt_min        = Δt_s / 60.0
    door_schedule = generate_door_schedule(N_sim; λ_per_hour = 8.0 / 24.0,
                                            door_load = 4500.0,
                                            Δt_min = Δt_min)

    # Storage
    X_true      = zeros(nx, N_sim + 1)   # true states          [nx × (N_sim+1)]
    U_closed    = zeros(nu, N_sim)        # applied controls     [nu × N_sim]
    P_buy       = zeros(N_sim)
    Cost_arr    = zeros(N_sim)
    σ_freezer   = zeros(N_sim + 1)        # 1-σ band for freezer air temp (state 9)
    air_idx     = [1, 3, 5, 7, 9]
    freezer_r   = 5                       # freezer room index
    freezer_si  = air_idx[freezer_r]      # = 9, freezer air-temp state
    X_pred_last = fill(NaN, nx, Hu)       # predicted state trajectory from the last MPC solve
    U_pred_last = fill(NaN, nu, Hu)       # predicted control trajectory from the last MPC solve

    X_true[:, 1] = copy(x0)

    # Analytical covariance propagation: Σ_{t+1} = A·Σ_t·A' + Q
    Q_noise = (σ_process^2) * I(nx)
    Σ = zeros(nx, nx)

    @info "Starting MPC loop: $N_sim steps = $(N_sim * Δt_s / 3600) hours"

    for t in 1:N_sim

        # 1-σ for freezer from propagated covariance
        σ_freezer[t] = sqrt(max(Σ[freezer_si, freezer_si], 0.0))

        # Price slice visible to the MPC at step t
        price_slice = price[t : t + Hu - 1]

        # Solve MPC with current true state as initial condition
        u_first, u_full, X_pred, status = solve_mpc(Ad, Bd, Ed, M_batch, Ξ, Ψ,
                                                     X_true[:, t], price_slice)

        U_closed[:, t] = u_first
        X_pred_last    = X_pred    # keep updating; final value = last horizon
        U_pred_last    = u_full

        # True disturbance (MPC is blind to door openings)
        d_true = vcat(T_amb, door_schedule[:, t])

        # Propagate TRUE system with process noise
        w = σ_process * randn(nx)
        X_true[:, t+1] = Ad * X_true[:, t] .+ Bd * u_first .+ Ed * d_true .+ w

        # Propagate covariance (MPC uses perfect state feedback so Σ resets each step,
        # but we track the open-loop 1-σ envelope from t=1 for visualisation)
        Σ = Ad * Σ * Ad' + Q_noise

        P_buy[t]    = sum(u_first)
        Cost_arr[t] = Δt_h * price_slice[1] * P_buy[t]

        if t % 12 == 1
            h_off = (t - 1) * Δt_h
            @info @sprintf("t=%3d  (8am+%4.1fh)  P=%.0f W  T_freezer=%.2f°C  σ=%.3f  status=%s",
                            t, h_off, P_buy[t], X_true[freezer_si, t],
                            σ_freezer[t], string(status))
        end
    end
    σ_freezer[N_sim+1] = sqrt(max(Σ[freezer_si, freezer_si], 0.0))

    total_cost = sum(Cost_arr)
    @info @sprintf("Total energy cost: %.4f €  |  Total energy: %.2f kWh",
                    total_cost, sum(P_buy) * Δt_h / 1000)

    # ── Plots ──────────────────────────────────────────────────────────────
    #
    #  Both subplots share the same x-axis: [0, 24] hours from 8 am.
    #  Left half  [0, 12 h] : true closed-loop simulation.
    #  Right half [12, 24 h]: MPC open-loop prediction from the last solve.

    x_lim      = (0.0, N_price * Δt_h)          # full 24-h window
    sim_end_h  = N_sim * Δt_h                    # = 12 h, boundary line

    time_sim   = collect(range(0.0, step = Δt_h, length = N_sim))
    time_state = collect(range(0.0, step = Δt_h, length = N_sim + 1))
    time_price = collect(range(0.0, step = Δt_h, length = N_price))

    # MPC lookahead time axis: starts at the boundary (12 h), Hu+1 points.
    # Prepend the last true state so the prediction curve connects smoothly.
    time_pred  = collect(range(sim_end_h, step = Δt_h, length = Hu + 1))
    T_fz_pred  = vcat(X_true[freezer_si, N_sim+1], X_pred_last[freezer_si, :])
    P_fz_pred  = vcat(U_pred_last[freezer_r, 1],   U_pred_last[freezer_r, :])

    # Door-opening times for the freezer (hours from 8 am)
    door_times_freezer = time_sim[door_schedule[freezer_r, :] .> 0]

    common_kwargs = (
        framestyle = :box,
        xlims      = x_lim,
        xlabel     = "Time [h]",
        grid       = true,
        gridalpha  = 0.3,
    )

    # ── helper: add door-opening vlines with a single legend entry ─────────
    function add_door_vlines!(p)
        for (i, td) in enumerate(door_times_freezer)
            lbl = i == 1 ? "Door opening (freezer)" : ""
            vline!(p, [td]; color = :firebrick, ls = :dash, lw = 1.0, label = lbl)
        end
    end

    # ── Subplot 1: electricity price ───────────────────────────────────────
    p_price = plot(time_price, price;
                   ylabel  = "Price [€/kWh]",
                   label   = "Buy price",
                   color   = :black, lw = 2,
                   legend  = :topright,
                   title   = "Electricity price",
                   common_kwargs...)
    vspan!(p_price, [0.0, sim_end_h]; alpha = 0.08, color = :steelblue, label = "Simulation")
    vspan!(p_price, [sim_end_h, x_lim[2]]; alpha = 0.06, color = :grey, label = "MPC lookahead")
    vline!(p_price, [sim_end_h]; color = :black, ls = :dot, lw = 1.2, label = "")
    add_door_vlines!(p_price)

    # ── Subplot 2: freezer temp (left) + compressor power (right) ──────────
    T_fz = X_true[freezer_si, :]    # [N_sim+1], true trajectory
    σ_fz = σ_freezer                 # [N_sim+1]
    P_fz = U_closed[freezer_r, :]   # [N_sim]

    p_temp = plot(time_state, T_fz;
                  ribbon    = 3*σ_fz,
                  fillalpha = 0.20,
                  color     = :steelblue, lw = 2,
                  ylabel    = "Freezer air temperature [°C]",
                  label     = "T_air  ±1σ (true)",
                  legend    = :topright,
                  title     = "Freezer Temperature & compressor power",
                  common_kwargs...)

    # MPC prediction on the right half (dashed, grey)
    plot!(p_temp, time_pred, T_fz_pred;
          color = :grey, lw = 1.5, ls = :dash,
          label = "MPC prediction (last solve)")

    hline!(p_temp, [T_low[freezer_r], T_high[freezer_r]];
           color = :steelblue, ls = :dot, alpha = 0.7, label = "T bounds")
    vspan!(p_temp, [sim_end_h, x_lim[2]]; alpha = 0.06, color = :grey, label = "")
    vline!(p_temp, [sim_end_h]; color = :black, ls = :dot, lw = 1.2, label = "")
    add_door_vlines!(p_temp)

    # Right-axis: compressor power (true left half, predicted right half)
    p_pow = twinx(p_temp)
    plot!(p_pow, time_sim, P_fz;
          color   = :darkorange, lw = 1.5,
          ylabel  = "Compressor power [W]",
          label   = "Power (right axis)",
          legend  = :bottomright,
          xlims   = x_lim,
          framestyle = :box)
    plot!(p_pow, time_pred, P_fz_pred;
          color = :darkorange, lw = 1.2, ls = :dash,
          label = "", xlims = x_lim)

    combined = plot(p_price, p_temp;
                    layout        = (2, 1),
                    size          = (1000, 700),
                    left_margin   = 6Plots.mm,
                    right_margin  = 12Plots.mm,
                    bottom_margin = 5Plots.mm,
                    top_margin    = 3Plots.mm,
                    link          = :x)

    outdir = joinpath(@__DIR__, "..", "..", "data", "EWH", "outputs")
    mkpath(outdir)
    savefig(combined, joinpath(outdir, "mpc_base.png"))
    @info "Plot saved to $(joinpath(outdir, "mpc_base.png"))"

    return X_true, U_closed, price, door_schedule
end

main()
