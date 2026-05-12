"""
dr_controller.jl — EWH Self-Driving Flexibility Manager
========================================================
This script is the Flexibility Manager. 

Data flows (matches BlueBird_Software.excalidraw):
  SQLite DB  ──► read sensors (measurements)      ┐
  SQLite DB  ──► read forecasts (disturbances)    ├─► FlexOPTi.optimize()
  DT JSON    ──► read plant dynamics              ┘        │
  Trading Manager (HTTP) ──► fetch electricity prices ────►┘
                                                           │
  SQLite DB  ◄── write control setpoints          ◄────────┘
  Knowledge Engine picks setpoints from DB and sends to BMS.

Digital Twin retraining:
  DR Controller spawns the R/ctsmTMB estimation script as a background
  subprocess every RETRAIN_EVERY_H hours. The MPC keeps running with the
  last-known model while R trains. When R finishes, the new JSON is copied
  to DT_FILE and picked up at the next solve.

Environment variables (set in docker-compose or .env):
  DB_PATH          path to the shared SQLite database
  DB_SENSORS_TABLE   table name for sensor measurements  (★ confirm with SW team)
  DB_FORECASTS_TABLE table name for disturbance forecasts (★ confirm with SW team)
  DB_SETPOINTS_TABLE table name for control setpoints     (★ confirm with SW team)
  DT_FILE          path to digital-twin JSON (shared volume with DT container)
  DT_R_SCRIPT      path to R estimation script
  DT_R_CWD         working directory for the R script
  TM_BASE_URL      Trading Manager base URL (default: http://localhost:9090)
  RETRAIN_EVERY_H  hours between DT retrains   (default: 168 = weekly)
  HTTP_PORT        port for /health /status     (default: 8080)

★ DATABASE SCHEMA (placeholder — software team must confirm column names):
  sensors table  : timestamp TEXT, room_id INT, T_air REAL, T_product REAL
  forecasts table: timestamp TEXT, room_id INT, T_ambient REAL, door_load REAL
  setpoints table: timestamp TEXT, room_id INT, u REAL, p1 REAL, p3 REAL
"""

using Pkg
Pkg.activate(joinpath(@__DIR__, "..", ".."))

using FlexOPTi, HTTP, JSON3, SQLite, DBInterface, TimeZones, Dates, Logging, Printf

# ── Configuration from environment ────────────────────────────────────────────
const DB_PATH            = get(ENV, "DB_PATH",
                               joinpath(@__DIR__, "../../data/bb_test.db"))
const DB_SENSORS_TABLE   = get(ENV, "DB_SENSORS_TABLE",   "measurements")   # ★
const DB_FORECASTS_TABLE = get(ENV, "DB_FORECASTS_TABLE", "forecasts")       # ★
const DB_SETPOINTS_TABLE = get(ENV, "DB_SETPOINTS_TABLE", "setpoints")       # ★
const DT_FILE            = get(ENV, "DT_FILE",
                               joinpath(@__DIR__, "../../data/EWH/inputs/cold_room_model_continuous.json"))
const DT_R_SCRIPT        = get(ENV, "DT_R_SCRIPT",
                               joinpath(@__DIR__, "../../../dt/EWH_coldrooms_CSTM/ctsmTMB_parameterestimation_callable.R"))
const DT_R_CWD           = get(ENV, "DT_R_CWD",
                               joinpath(@__DIR__, "../../../dt/EWH_coldrooms_CSTM"))
const TM_BASE_URL        = get(ENV, "TM_BASE_URL",  "http://localhost:9090")
const RETRAIN_EVERY_S    = parse(Float64, get(ENV, "RETRAIN_EVERY_H", "168")) * 3600
const HTTP_PORT          = parse(Int,     get(ENV, "HTTP_PORT", "8080"))
const Δt_s               = 900.0   # MPC step [s] — 15 min
const Hu                 = 48      # horizon — 12 h
const N_ROOMS            = 5       # 4 fridges + 1 freezer

# ── Shared state (MPC loop writes, HTTP server reads) ─────────────────────────
const _status = Ref{Dict{String,Any}}(Dict("state" => "starting"))

# ══════════════════════════════════════════════════════════════════════════════
# SQLite — sensors (read) and setpoints (write)
# ══════════════════════════════════════════════════════════════════════════════

"""
Read the latest sensor state from the DB and write it as a sensors JSON file
for FlexOPTi. Returns the path to the temp file.

★ Query assumes:
  - one row per room per timestamp
  - rooms ordered 1..N_ROOMS (dairy, finished products, meat, veg, freezer)
  - state vector x0 = [T_air_1, T_prod_1, T_air_2, T_prod_2, ...] (10 elements)
  Adjust the SELECT and assembly logic once the real schema is known.
"""
function db_read_sensors(db::SQLite.DB, tmp_dir::String) :: String
    # ★ Adjust table/column names to match actual schema
    rows = DBInterface.execute(db, """
        SELECT room_id, T_air, T_product
        FROM $DB_SENSORS_TABLE
        WHERE timestamp = (SELECT MAX(timestamp) FROM $DB_SENSORS_TABLE)
        ORDER BY room_id
    """) |> (q -> map(NamedTuple, q))

    # Interleave T_air, T_product for each room → x0 length 2*N_ROOMS
    x0 = Float64[]
    if isempty(rows)
        @warn "[DB] No sensor data in '$DB_SENSORS_TABLE' — falling back to x0 from DT JSON."
        x0 = convert(Vector{Float64},
                     JSON3.read(read(DT_FILE, String))["state_space"]["x0"])
    else
        for r in rows
            push!(x0, r.T_air, r.T_product)
        end
    end

    f = joinpath(tmp_dir, "sensors.json")
    write(f, JSON3.write(Dict("x0" => x0)))
    return f
end

"""
Read the latest disturbance forecast from the DB and write it as a forecasts
JSON file for FlexOPTi. Returns the path to the temp file.

★ Adjust query to match actual schema. Expected output:
  { "T_ambient": [Float64 × Hu],
    "door_openings": [[Float64 × N_ROOMS] × Hu]  (row = step, col = room) }
"""
function db_read_forecasts(db::SQLite.DB, tmp_dir::String) :: String
    rows = DBInterface.execute(db, """
        SELECT timestamp, room_id, T_ambient, door_load
        FROM $DB_FORECASTS_TABLE
        WHERE timestamp >= datetime('now')
        ORDER BY timestamp, room_id
        LIMIT $(Hu * N_ROOMS)
    """) |> (q -> map(NamedTuple, q))

    # Fall back to flat values if DB has no forecast yet
    T_ambient    = fill(20.0, Hu)
    door_openings = zeros(Float64, N_ROOMS, Hu)

    if !isempty(rows)
        for (i, r) in enumerate(rows)
            step = div(i - 1, N_ROOMS) + 1
            room = mod(i - 1, N_ROOMS) + 1
            step > Hu && break
            T_ambient[step]          = r.T_ambient
            door_openings[room, step] = r.door_load
        end
    else
        @warn "[DB] No forecast data found — using flat fallback values."
    end

    f = joinpath(tmp_dir, "forecasts.json")
    write(f, JSON3.write(Dict(
        "T_ambient"     => T_ambient,
        "door_openings" => door_openings,
    )))
    return f
end

"""
Write the optimal setpoints u_first back to the DB so the Knowledge Engine
can pick them up and forward them to the BMS.

★ Adjust INSERT statement to match actual schema.
"""
function db_write_setpoints(db::SQLite.DB, u::Vector{Float64}, t::ZonedDateTime)
    ts = string(t)
    # u layout: [u_fridge_1, u_fridge_2, u_fridge_3, u_fridge_4] then p3 (freezer)
    n_fridge = length(u) - 1
    for r in 1:n_fridge
        DBInterface.execute(db, """
            INSERT INTO $DB_SETPOINTS_TABLE (timestamp, room_id, u, p3)
            VALUES (?, ?, ?, 0.0)
        """, (ts, r, u[r]))
    end
    # Freezer (room N_ROOMS): p3 is binary on/off power
    DBInterface.execute(db, """
        INSERT INTO $DB_SETPOINTS_TABLE (timestamp, room_id, u, p3)
        VALUES (?, ?, 0.0, ?)
    """, (ts, N_ROOMS, u[end]))
    @info "[DB] Setpoints written for $(n_fridge) fridges + 1 freezer at $ts."
end

# ══════════════════════════════════════════════════════════════════════════════
# Digital Twin — background retraining
# ══════════════════════════════════════════════════════════════════════════════

_dt_proc   = Ref{Union{Base.Process, Nothing}}(nothing)
_dt_last_s = Ref{Float64}(-Inf)

function dt_trigger_retrain(; blocking=false)
    if !isnothing(_dt_proc[]) && process_running(_dt_proc[])
        @info "[DT] Retrain already running — skipping."
        return
    end
    @info "[DT] Triggering parameter estimation $(blocking ? "(blocking)" : "(background)") …"
    cmd = Cmd(`Rscript $DT_R_SCRIPT`; dir=DT_R_CWD)
    _dt_proc[] = run(cmd; wait=blocking)
    blocking && dt_collect()
end

function dt_collect() :: Bool
    proc = _dt_proc[]
    isnothing(proc)        && return false
    process_running(proc)  && return false
    _dt_proc[] = nothing
    if !success(proc)
        @error "[DT] Retrain failed (exit code $(proc.exitcode))."
        return false
    end
    out = joinpath(DT_R_CWD, "cold_room_model_cont.json")
    if isfile(out)
        cp(out, DT_FILE; force=true)
        _dt_last_s[] = time()
        @info "[DT] Retrain finished — model updated ($(DT_FILE))."
        return true
    end
    return false
end

# ══════════════════════════════════════════════════════════════════════════════
# HTTP status server  (/health  /status)
# ══════════════════════════════════════════════════════════════════════════════

function start_status_server()
    router = HTTP.Router()
    HTTP.register!(router, "GET", "/health",
        _ -> HTTP.Response(200, "OK"))
    HTTP.register!(router, "GET", "/status",
        _ -> HTTP.Response(200,
                ["Content-Type" => "application/json"],
                JSON3.write(_status[])))
    @async HTTP.serve(router, "0.0.0.0", HTTP_PORT)
    @info "[HTTP] Status server listening on :$HTTP_PORT"
end

# ══════════════════════════════════════════════════════════════════════════════
# MPC control loop
# ══════════════════════════════════════════════════════════════════════════════

function mpc_loop(db::SQLite.DB)
    tmp_dir = mktempdir()
    @info "[MPC] Starting control loop  Δt=$(Δt_s)s  Hu=$Hu  horizon=$(Hu*Δt_s/3600)h"

    while true
        t_wall = time()

        # ── Poll for finished DT retrain; trigger next one if due ─────────────
        dt_collect()
        if time() - _dt_last_s[] ≥ RETRAIN_EVERY_S
            dt_trigger_retrain()   # non-blocking — MPC continues
        end

        # ── One MPC solve ─────────────────────────────────────────────────────
        try
            sensors_file   = nothing
            forecasts_file = nothing
            t_solve        = now(tz"UTC")

            # Prices are fetched internally by FlexOPTi from TM_BASE_URL.
            # Dynamics are read and discretised internally from DT_FILE.
            @info "[MPC] Calling FlexOPTi.optimize …"
            oy = FlexOPTi.optimize(DT_FILE, sensors_file, forecasts_file;
                pilot            = "Ewh",
                Hu               = Hu,
                Δt               = Δt_s,
                solver           = "Gurobi",
                mip_gap          = 0.01,
                milp_horizon     = 1,
                compute_datetime = t_solve,
                tm_base_url      = TM_BASE_URL,
                loglevel         = "info",
                logoutput        = "console")
            @info "[MPC] Optimize returned  status=$(oy[:OPT_status])  cost=$(oy[:OPT_cost])"

            u_first = vcat(oy[:u][1, :], oy[:p3][1])
            @info @sprintf("[MPC] u_first = %s", string(round.(u_first; digits=2)))
            db_write_setpoints(db, u_first, t_solve)

            _status[] = Dict(
                "state"   => "ok",
                "status"  => string(oy[:OPT_status]),
                "cost"    => oy[:OPT_cost],
                "u"       => u_first,
                "updated" => string(t_solve),
            )
            @info @sprintf("[MPC] cost=%.4f  status=%s", oy[:OPT_cost], oy[:OPT_status])

        catch e
            @error "[MPC] Step failed" exception=(e, catch_backtrace())
            _status[] = Dict("state"   => "error",
                             "msg"     => string(e),
                             "updated" => string(now(tz"UTC")))
        end

        # ── Sleep (★ DEMO: 1s; production: Δt_s = 900s) ──────────────────────
        elapsed = time() - t_wall
        sleep(max(0.0, 1.0 - elapsed))
    end
end

# ══════════════════════════════════════════════════════════════════════════════
# Entry point
# ══════════════════════════════════════════════════════════════════════════════

@info "[BOOT] EWH dr_controller starting …"
start_status_server()

db = SQLite.DB(DB_PATH)
@info "[BOOT] Connected to database: $DB_PATH"

@info "[BOOT] Waiting for initial Digital Twin retrain …"
dt_trigger_retrain(; blocking=true); nothing

mpc_loop(db)   # blocks forever — this is the container's main process
