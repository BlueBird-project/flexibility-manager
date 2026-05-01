# =============================================================================
#  mpc_base.jl — Receding-horizon MPC for the 5-zone cold room
#
#  Uses the FlexOPTi pilot infrastructure (O / OX / mpc_update) directly.
#
#  Setup
#  -----
#  - Model  : cold_room_model_continuous.json  (ZOH-discretized at run time)
#  - Δt     : 300 s (5 min)
#  - Horizon: Hu = 144 steps = 12 h
#  - Sim    : 12 h (8:00 → 20:00), 144 receding-horizon solves
#
#  True world vs MPC knowledge
#  ---------------------------
#  - Ambient temperature: constant 18 °C (underground facility)
#  - Door openings: Poisson arrivals in the TRUE system;
#    MPC forecasts zero door openings (perfect ignorance).
#  - Process noise: small Gaussian added to every true state propagation step.
# =============================================================================

using Pkg
Pkg.activate(joinpath(@__DIR__, "..", ".."))

using FlexOPTi
using JSON, LinearAlgebra, Random, Printf, Plots, TimeZones, Dates

Random.seed!(42)

# ── Paths ─────────────────────────────────────────────────────────────────

const ROOT    = joinpath(@__DIR__, "..", "..")
const DT_FILE = joinpath(ROOT, "data", "EWH", "inputs", "cold_room_model_continuous.json")
const OUT_DIR = joinpath(ROOT, "data", "EWH", "outputs")

# ── Simulation parameters ──────────────────────────────────────────────────

const Δt_s      = 300.0                    # time step [s]
const Δt_h      = Δt_s / 3600.0           # time step [h]
const Hu        = Int(12 * 3600 / Δt_s)   # 12-h horizon = 144 steps
const N_sim     = Hu                       # simulate 12 h
const T_AMB     = 18.0                     # constant ambient [°C] (underground)
const σ_process = 0.05                     # process noise std [°C] per step
const n_rooms   = 5                        # rooms: dairy, fp, meat, veg, freezer

# Freezer indices (room 5, air-temp state 9)
const freezer_r  = 5
const freezer_si = 2 * freezer_r - 1      # = 9

# ── Build the O (options) struct ───────────────────────────────────────────

const pilot = FlexOPTi.Ewh()

o = FlexOPTi.O(
    Hu,                                           # Hu
    Δt_s,                                         # Δt [s]
    false,                                        # init_condition
    pilot,                                        # pilot
    "warn",                                       # loglevel  (suppress per-solve noise)
    "console",                                    # logoutput
    "fm.log",                                     # logfile
    false,                                        # log_with_time
    "Gurobi",                                     # solver
    true,                                         # continuous_dynamo
    "output.txt",                                 # output_file
    ZonedDateTime(2026, 5, 1, 8, 0, 0, tz"UTC"),  # compute_datetime (8 am)
)

FlexOPTi.set_logging(o)

# ── Parse the digital twin once (discretizes A, B, E in-place) ────────────

digital_twin = FlexOPTi.parse_digital_twin(pilot, o, DT_FILE)
ss           = digital_twin["state_space"]
Ad = ss["A"];  Bd = ss["B"];  Ed = ss["E"]
nx = ss["nx"]; nu = ss["nu"]; nd = size(Ed, 2)
x0 = Vector{Float64}(ss["x0"])

# Constraints are fixed throughout the simulation
constraints = FlexOPTi.build_constraints(pilot)

# ── Electricity price signal (24 h from 8 am, 15-min granularity) ─────────
#
#  EU day-ahead double-peak profile with log-normal block noise.
#  key_h → clock hour,  key_p → €/MWh
#
#  00–05 h  ~45   overnight baseload
#  06–07 h  ~70   morning ramp
#  08–10 h  ~110  morning peak
#  11–13 h  ~65   midday solar dip
#  14–16 h  ~80   afternoon recovery
#  17–19 h  ~140  evening peak (highest)
#  20–22 h  ~90   post-peak decline
#  23–24 h  ~55   back to overnight

function make_price_signal(N_price::Int, Δt_h::Float64)
    key_h = Float64[0,  3,  5,  7,  8,  10, 12, 14, 16, 17, 19, 21, 23, 24]
    key_p = Float64[45, 40, 55, 85, 115, 100, 62, 72, 88, 145, 125, 88, 58, 45]

    function base_price(clock_h::Float64)
        i = searchsortedlast(key_h, clock_h)
        i = clamp(i, 1, length(key_h) - 1)
        α = (clock_h - key_h[i]) / (key_h[i+1] - key_h[i])
        return key_p[i] + α * (key_p[i+1] - key_p[i])
    end

    steps_per_block = max(1, round(Int, 0.25 / Δt_h))  # 3 steps @ 5 min
    signal = zeros(N_price)
    b, k = 0, 1
    while k <= N_price
        t_clock = mod(8.0 + b * 0.25, 24.0)
        noisy   = base_price(t_clock) * exp(0.12 * randn())
        noisy   = clamp(noisy, 10.0, 300.0) / 1000.0   # €/MWh → €/kWh
        for _ in 1:steps_per_block
            k > N_price && break
            signal[k] = noisy
            k += 1
        end
        b += 1
    end
    return signal
end

N_price = N_sim + Hu   # 24 h
price   = make_price_signal(N_price, Δt_h)

# ── Poisson door openings (TRUE world, hidden from MPC) ────────────────────
#
#  Rate: 8 openings / room / day ≈ 0.33 / hour.
#  Each opening: 1 min at 4500 W → average power = fraction × 4500 W.

function Poisson_sample(λ::Float64)::Int
    L, k, p = exp(-λ), 0, 1.0
    while p > L; p *= rand(); k += 1; end
    return k - 1
end

function generate_door_schedule(N_steps::Int; λ_per_hour = 8.0/24.0,
                                  door_load = 4500.0, Δt_min = 5.0)
    λ = λ_per_hour / 60.0
    schedule = zeros(n_rooms, N_steps)
    for r in 1:n_rooms, t in 1:N_steps
        n_open = Poisson_sample(λ * Δt_min)
        if n_open > 0
            schedule[r, t] = min(n_open, Δt_min) / Δt_min * door_load
        end
    end
    return schedule
end

door_schedule = generate_door_schedule(N_sim; Δt_min = Δt_s / 60.0)

# ── Storage ────────────────────────────────────────────────────────────────

X_true      = zeros(nx, N_sim + 1)
U_closed    = zeros(nu, N_sim)       # [nu × N_sim]: rows 1:4 fridges, row 5 freezer
σ_freezer   = zeros(N_sim + 1)
X_pred_last = fill(NaN, nx, Hu)      # last MPC predicted state trajectory [nx × Hu]
U_pred_last = fill(NaN, nu, Hu)      # last MPC predicted control trajectory [nu × Hu]

X_true[:, 1] = copy(x0)

Q_noise = (σ_process^2) * I(nx)
Σ = zeros(nx, nx)

# ── MPC loop ───────────────────────────────────────────────────────────────

@info "Starting MPC loop: $N_sim steps = $(N_sim * Δt_s / 3600) hours"

for t in 1:N_sim

    σ_freezer[t] = sqrt(max(Σ[freezer_si, freezer_si], 0.0))

    # ── Build forecast (MPC sees no door openings) ─────────────────────────
    forecast = Dict{String, Any}(
        "door_openings" => zeros(n_rooms, Hu),
        "T_ambient"     => fill(T_AMB, Hu),
        "ToU_buy"       => price[t : t + Hu - 1],
        "ToU_sell"      => zeros(Hu),
    )

    # ── Update initial condition in the digital twin ───────────────────────
    digital_twin["state_space"]["x0"] = X_true[:, t]

    # ── Build OX and solve one MPC step ───────────────────────────────────
    dynamics = FlexOPTi.build_dynamics(pilot, o, digital_twin, nothing, forecast)
    ox       = FlexOPTi.OX(digital_twin, nothing, forecast, constraints, dynamics)
    oy       = FlexOPTi.mpc_update(pilot, o, ox)

    # ── Extract first control action ───────────────────────────────────────
    # oy[:u]  → [Hu × nfridge]  fridge powers at each step
    # oy[:p3] → [Hu]            freezer power at each step (0 or p3_high)
    u_fridge_t1 = oy[:u][1, :]        # [4]  first-step fridge powers
    p_fz_t1     = oy[:p3][1]          # [1]  first-step freezer power
    u_first      = vcat(u_fridge_t1, p_fz_t1)   # [5]  full input vector

    U_closed[:, t] = u_first

    # Store full predicted trajectories from this solve (keep updating)
    global X_pred_last = Matrix(oy[:x]')      # [nx × Hu]
    nfr         = size(oy[:u], 2)
    U_pred_last[1:nfr, :] = Matrix(oy[:u]')   # [nfridge × Hu]
    U_pred_last[nu, :]    = oy[:p3]            # [Hu] freezer

    # ── Propagate TRUE system (Poisson doors + process noise) ─────────────
    d_true            = vcat(T_AMB, door_schedule[:, t])
    w                 = σ_process * randn(nx)
    X_true[:, t+1]    = Ad * X_true[:, t] .+ Bd * u_first .+ Ed * d_true .+ w

    global Σ = Ad * Σ * Ad' + Q_noise

    if t % 12 == 1
        h_off = (t - 1) * Δt_h
        @info @sprintf("t=%3d  (8am+%4.1fh)  P_fz=%.0f W  T_fz=%.2f°C  σ=%.3f  status=%s",
                        t, h_off, p_fz_t1, X_true[freezer_si, t],
                        σ_freezer[t], string(oy[:OPT_status]))
    end
end

σ_freezer[N_sim+1] = sqrt(max(Σ[freezer_si, freezer_si], 0.0))

total_energy_kwh = sum(U_closed) * Δt_h / 1000
@info @sprintf("Total compressor energy: %.1f kWh", total_energy_kwh)

# ── Plots ──────────────────────────────────────────────────────────────────
#
#  Both subplots span [0, 24] h (shared x-axis).
#  Left half  [0, 12 h] : true closed-loop simulation.
#  Right half [12, 24 h]: MPC open-loop prediction from the last solve.

x_lim     = (0.0, N_price * Δt_h)    # 24 h
sim_end_h = N_sim * Δt_h              # 12 h

time_sim   = collect(range(0.0, step = Δt_h, length = N_sim))
time_state = collect(range(0.0, step = Δt_h, length = N_sim + 1))
time_price = collect(range(0.0, step = Δt_h, length = N_price))
time_pred  = collect(range(sim_end_h, step = Δt_h, length = Hu + 1))

# Predicted freezer trajectory: prepend last true state for smooth connection
T_fz_pred = vcat(X_true[freezer_si, N_sim+1], X_pred_last[freezer_si, :])
P_fz_pred = vcat(U_pred_last[freezer_r, 1],   U_pred_last[freezer_r, :])

T_fz = X_true[freezer_si, :]
P_fz = U_closed[freezer_r, :]

door_times = time_sim[door_schedule[freezer_r, :] .> 0]

common = (framestyle = :box, xlims = x_lim,
          xlabel = "Time from 8 am [h]", grid = true, gridalpha = 0.3)

function add_door_vlines!(p)
    for (i, td) in enumerate(door_times)
        vline!(p, [td]; color = :firebrick, ls = :dash, lw = 1.0,
               label = i == 1 ? "Door opening (freezer)" : "")
    end
end

# ── Subplot 1: electricity price ───────────────────────────────────────────
p_price = plot(time_price, price;
               ylabel = "Price [€/kWh]", label = "Buy price",
               color = :black, lw = 2, legend = :topright,
               title = "Electricity price (EU day-ahead, 15 min blocks)",
               common...)
vspan!(p_price, [0.0, sim_end_h];       alpha = 0.08, color = :steelblue, label = "Simulation")
vspan!(p_price, [sim_end_h, x_lim[2]]; alpha = 0.06, color = :grey,      label = "MPC lookahead")
vline!(p_price, [sim_end_h]; color = :black, ls = :dot, lw = 1.2, label = "")
add_door_vlines!(p_price)

# ── Subplot 2: freezer temp (left) + compressor power (right) ─────────────
p_temp = plot(time_state, T_fz;
              ribbon = σ_freezer, fillalpha = 0.20,
              color = :steelblue, lw = 2,
              ylabel = "Freezer air temperature [°C]",
              label = "T_air  ±1σ (true)", legend = :bottomleft,
              title = "Freezer — temperature & compressor power",
              common...)
plot!(p_temp, time_pred, T_fz_pred;
      color = :grey, lw = 1.5, ls = :dash,
      label = "MPC prediction (last solve)")
hline!(p_temp, [-40.0, -18.0];
       color = :steelblue, ls = :dot, alpha = 0.7, label = "T bounds")
vspan!(p_temp, [sim_end_h, x_lim[2]]; alpha = 0.06, color = :grey, label = "")
vline!(p_temp, [sim_end_h]; color = :black, ls = :dot, lw = 1.2, label = "")
add_door_vlines!(p_temp)

p_pow = twinx(p_temp)
plot!(p_pow, time_sim,  P_fz;
      color = :darkorange, lw = 1.5,
      ylabel = "Compressor power [W]",
      label = "Power (right axis)", legend = :bottomright,
      xlims = x_lim, framestyle = :box)
plot!(p_pow, time_pred, P_fz_pred;
      color = :darkorange, lw = 1.2, ls = :dash,
      label = "", xlims = x_lim)

combined = plot(p_price, p_temp;
                layout = (2, 1), size = (1000, 700),
                left_margin = 6Plots.mm, right_margin = 12Plots.mm,
                bottom_margin = 5Plots.mm, top_margin = 3Plots.mm,
                link = :x)

mkpath(OUT_DIR)
savefig(combined, joinpath(OUT_DIR, "mpc_base.png"))
@info "Plot saved to $(joinpath(OUT_DIR, "mpc_base.png"))"
