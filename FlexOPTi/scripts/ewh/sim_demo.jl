# =============================================================================
#  sim_demo.jl — Closed-loop MPC simulation demo
#
#  Simulates the 5-zone cold room from yesterday noon to tomorrow midnight.
#  Runs as fast as possible (no wall-clock sleep).
#
#  Inputs:
#    - Real electricity prices from Trading Manager (yesterday noon → tomorrow midnight)
#    - Constant 18°C disturbance forecast (MPC sees this)
#    - TRUE world: Poisson door openings + process noise (hidden from MPC)
#    - Digital Twin dynamics from DT_FILE
#
#  Outputs:
#    - Setpoints written to DB (same schema as dr_controller.jl)
#    - Full simulation results saved to data/EWH/outputs/sim_demo.jld2 for plotting
#
#  Run via docker compose:
#    docker compose -f docker-compose.ewh.yml --profile sim run --rm fc-sim
#  Or locally:
#    julia --project=FlexOPTi FlexOPTi/scripts/ewh/sim_demo.jl
# =============================================================================

using Pkg
Pkg.activate(joinpath(@__DIR__, "..", ".."))

using FlexOPTi
using JSON, JSON3, JLD2, LinearAlgebra, Random, Printf, TimeZones, Dates
using SQLite, DBInterface

Random.seed!(42)

# ── Settings ──────────────────────────────────────────────────────────────────

function _load_settings()
    path = get(ENV, "SETTINGS_FILE", joinpath(@__DIR__, "../../settings.json"))
    isfile(path) || return Dict{String,Any}()
    d = JSON3.read(read(path, String))
    Dict{String,Any}(string(k) => v for (k, v) in pairs(d))
end
const SETTINGS = _load_settings()

const DB_PATH    = get(ENV, "DB_PATH",     joinpath(@__DIR__, "../../data/bb_test.db"))
const DT_FILE    = get(ENV, "DT_FILE",     joinpath(@__DIR__, "../../data/EWH/inputs/cold_room_model_continuous.json"))
const TM_BASE_URL = get(ENV, "TM_BASE_URL", get(SETTINGS, "tm_base_url", "http://localhost:9090"))
const OUT_DIR    = joinpath(@__DIR__, "../../data/EWH/outputs")

const Δt_s       = parse(Float64, get(ENV, "DELTA_T", string(get(SETTINGS, "delta_t", 900.0))))
const Hu         = parse(Int,     get(ENV, "HU",      string(get(SETTINGS, "Hu",      48))))
const Δt_h       = Δt_s / 3600.0
const T_AMB      = 18.0        # constant ambient [°C]
const σ_process  = 0.05        # process noise std [°C] per step
const n_rooms    = 5
const freezer_r  = 5
const freezer_si = 2 * freezer_r - 1   # = 9 (air-temp state index)

# ── Simulation time window ─────────────────────────────────────────────────────
#
#  Start: yesterday 12:00 UTC
#  End:   tomorrow  23:45 UTC  (tomorrow midnight - 1 step)
#  N_sim steps of Δt_s seconds

const today     = now(tz"UTC")
const t_start   = ZonedDateTime(year(today), month(today), day(today), 12, 0, 0, tz"UTC") - Day(1)
const t_end     = ZonedDateTime(year(today), month(today), day(today), 23, 45, 0, tz"UTC") + Day(1)
const N_sim     = round(Int, (t_end - t_start).value / 1000 / Δt_s)

@info "[SIM] Window: $t_start → $t_end  ($N_sim steps × $(Δt_s)s = $(round(N_sim*Δt_s/3600, digits=1))h)"

# ── Build O (options struct) ───────────────────────────────────────────────────

const pilot = FlexOPTi.Ewh()

o = FlexOPTi.O(  # not const — compute_datetime advances each step
    Hu,
    Δt_s,
    false,
    pilot,
    "warn",
    "console",
    "fm.log",
    false,
    get(SETTINGS, "solver", "HiGHS"),
    parse(Float64, string(get(SETTINGS, "mip_gap", 0.01))),
    true,     # warm_start — reuse previous solution as initial guess
    parse(Int, string(get(SETTINGS, "milp_horizon", 1))),
    true,
    "output.txt",
    t_start,
    get(SETTINGS, "market_country", "Germany"),
    false,
    TM_BASE_URL,
)

FlexOPTi.set_logging(o)

# ── Load digital twin ──────────────────────────────────────────────────────────

if !isfile(DT_FILE)
    error("[SIM] DT file not found: $DT_FILE — run dt-ewh container first.")
end

digital_twin = FlexOPTi.parse_digital_twin(pilot, o, DT_FILE)
ss = digital_twin["state_space"]
Ad = ss["A"];  Bd = ss["B"];  Ed = ss["E"]
nx = ss["nx"]; nu = ss["nu"]; nd = size(Ed, 2)
x0 = Vector{Float64}(ss["x0"])

constraints = FlexOPTi.build_constraints(pilot)

# Prices fetched internally by build_dynamics each step via TM.
# o.compute_datetime advances each step so prices align to simulated time.

# ── Poisson door openings (TRUE world, hidden from MPC) ───────────────────────

function poisson_sample(λ::Float64)::Int
    L, k, p = exp(-λ), 0, 1.0
    while p > L; p *= rand(); k += 1; end
    return k - 1
end

function generate_door_schedule(N::Int; λ_per_hour=8.0/24.0, door_load=4500.0)
    λ = λ_per_hour / 60.0
    schedule = zeros(n_rooms, N)
    Δt_min = Δt_s / 60.0
    for r in 1:n_rooms, t in 1:N
        n_open = poisson_sample(λ * Δt_min)
        n_open > 0 && (schedule[r, t] = min(n_open, Δt_min) / Δt_min * door_load)
    end
    return schedule
end

door_schedule = generate_door_schedule(N_sim)

# ── Storage ────────────────────────────────────────────────────────────────────

X_true   = zeros(nx, N_sim + 1)
U_closed = zeros(nu, N_sim)
P_series = zeros(N_sim)          # buy price [EUR/kWh] at each simulated step
X_true[:, 1] = copy(x0)
Q_noise = (σ_process^2) * I(nx)
Σ = zeros(nx, nx)

# State + input names from DT for plot labels
const state_names = get(ss, "state_names", String[])
const input_names = get(ss, "input_names", String[])

# ── DB setup ───────────────────────────────────────────────────────────────────

db = SQLite.DB(DB_PATH)
DBInterface.execute(db, "PRAGMA busy_timeout = 10000")  # wait up to 10s on lock
@info "[SIM] Connected to DB: $DB_PATH"

function db_write_setpoints(db, u, t_sim)
    ts = string(t_sim)
    n_fridge = length(u) - 1
    for r in 1:n_fridge
        DBInterface.execute(db,
            "INSERT INTO setpoints (timestamp, room_id, u, p3) VALUES (?, ?, ?, 0.0)",
            (ts, r, u[r]))
    end
    DBInterface.execute(db,
        "INSERT INTO setpoints (timestamp, room_id, u, p3) VALUES (?, ?, 0.0, ?)",
        (ts, n_rooms, u[end]))
end

# ── MPC simulation loop ────────────────────────────────────────────────────────

@info "[SIM] Starting closed-loop simulation: $N_sim steps"
oy_prev  = nothing
t_sim    = t_start   # advances by oy[:o].Δt each step

for t in 1:N_sim

    # Advance simulated clock so build_dynamics fetches prices at correct time
    o.compute_datetime = t_sim

    # MPC sees flat 18°C ambient, zero doors. Prices fetched internally from TM.
    forecast = Dict{String,Any}(
        "door_openings" => zeros(n_rooms, Hu),
        "T_ambient"     => fill(T_AMB, Hu),
        "ToU_sell"      => zeros(Hu),
    )

    digital_twin["state_space"]["x0"] = X_true[:, t]

    prices, _ = FlexOPTi.fetch_market_prices(o)
    P_series[t] = prices[1]                      # save price faced at this step
    dynamics  = FlexOPTi.build_dynamics(pilot, o, digital_twin, nothing, forecast)
    ox        = FlexOPTi.OX(digital_twin, nothing, forecast, Dict{Symbol,Any}(constraints), dynamics, prices)
    elapsed  = @elapsed oy = FlexOPTi.mpc_update(pilot, o, ox, oy_prev)

    FlexOPTi.print_mpc_progress(t, N_sim, oy, elapsed)

    u_first = vcat(oy[:u][1, :], oy[:p3][1])
    U_closed[:, t] = u_first

    db_write_setpoints(db, u_first, t_sim)

    global oy_prev = isnan(oy[:OPT_cost]) ? nothing : oy

    # Δt from the MPC output struct — stays consistent with whatever o.Δt was used
    Δt_actual = oy[:o].Δt

    # TRUE system: Poisson doors + process noise
    d_true = vcat(T_AMB, door_schedule[:, t])
    w      = σ_process * randn(nx)
    X_true[:, t+1] = Ad * X_true[:, t] .+ Bd * u_first .+ Ed * d_true .+ w
    global Σ = Ad * Σ * Ad' + Q_noise

    # Advance simulated clock using actual MPC timestep
    global t_sim = t_sim + Second(round(Int, Δt_actual))
end

total_kwh = sum(U_closed) * Δt_h / 1000
@info @sprintf("[SIM] Done. Total compressor energy: %.1f kWh", total_kwh)

# ── Save for plotting ──────────────────────────────────────────────────────────

mkpath(OUT_DIR)
out = joinpath(OUT_DIR, "sim_demo.jld2")
jldsave(out;
    X_true, U_closed, P_series, door_schedule,
    state_names, input_names,
    Δt_h, Hu, N_sim, nx, nu,
    t_start=string(t_start), t_end=string(t_end))
@info "[SIM] Results saved to $out — run mpc_plot.jl to visualise."
