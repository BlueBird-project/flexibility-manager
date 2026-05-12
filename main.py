#!/home/kahka/DTU/BlueBird/flexmanager/FM_collab/flexibility-manager/.venv/bin/python
"""
EWH Flexibility Manager — Demo / Architecture Illustration
===========================================================
Docker service map (each block → one container):

  ┌─────────────────────────────────────────────────────┐
  │              Flexibility Manager (this)             │
  │  starts services, runs control loop, triggers DT   │
  └───┬──────────────┬──────────────┬───────────────────┘
      │              │              │
      ▼              ▼              ▼
  Trading        Digital        Julia Controller
  Manager        Twin           (FlexOPTi)
  (prices)   (R/ctsmTMB)   reads DT + sensors + prices,
  ↑ already     retrain        builds & solves MILP,
    Docker       on signal      returns u_first

NOTE — FlexOPTi.optimize() is already self-contained:
  • fetches electricity prices from Trading Manager (HTTP)
  • builds door-opening / T_ambient forecasts internally
  • reads and discretises the digital-twin matrices
  • solves the MILP and returns the first optimal action

The FM's only jobs are:
  1. Start the Trading Manager.
  2. Trigger Digital Twin retraining (async, every RETRAIN_EVERY steps).
  3. Feed the current sensor state to Julia and forward u_first to actuators.

The true-plant simulator below (★ DEMO ONLY ★) replaces real sensors and
actuators. Remove it entirely when running on the pilot site.
"""

import json, logging, math, random, shutil, subprocess
from pathlib import Path
import numpy as np
from scipy.linalg import expm   # needed only for demo plant propagation

# ── Paths ──────────────────────────────────────────────────────────────────────
ROOT     = Path(__file__).parent
DT_FILE  = ROOT / "FlexOPTi/data/EWH/inputs/cold_room_model_continuous.json"
DT_DIR   = ROOT / "dt/EWH_coldrooms_CSTM"
OUT_DIR  = ROOT / "FlexOPTi/data/EWH/outputs"
TM_DIR   = Path("/home/kahka/DTU/BlueBird/TM/trading-manager/compose/local_dev")
FLEXOPTI = ROOT / "FlexOPTi"

# ── Parameters ─────────────────────────────────────────────────────────────────
Δt_s          = 900.0   # step [s] — 15 min
Hu            = 48      # MPC horizon — 12 h
N_SIM         = 96      # ★ DEMO: total steps (24 h); production loop is infinite
T_AMB         = 20.0    # ★ DEMO: ambient [°C]  (FlexOPTi uses 20.0 internally)
σ_noise       = 0.05    # ★ DEMO: plant process noise [°C]
RETRAIN_EVERY = 5       # steps between DT retrains (≈ weekly in production)

logging.basicConfig(format="%(asctime)s [%(levelname)s] %(message)s",
                    datefmt="%H:%M:%S", level=logging.INFO)
log = logging.getLogger(__name__)

# ══════════════════════════════════════════════════════════════════════════════
# SERVICE: Trading Manager  (already Docker — no Python logic needed)
# ══════════════════════════════════════════════════════════════════════════════

def trading_manager_start():
    log.info("[TM] Starting Trading Manager …")
    subprocess.run(
        ["docker", "compose", "-p", "local",
         "-f", str(TM_DIR / "compose.yaml"),
         "--env-file", str(TM_DIR / ".env"), "start"],
        check=False, capture_output=True,
    )

# ══════════════════════════════════════════════════════════════════════════════
# SERVICE: Digital Twin  (R / ctsmTMB — will be Docker)
#   FM → trigger retrain (non-blocking, MPC keeps running with current model)
#   FM ← new matrices arrive in DT_FILE when done
# ══════════════════════════════════════════════════════════════════════════════

_dt_proc     = None
_dt_at_step  = None

def dt_trigger_retrain(step: int):
    global _dt_proc, _dt_at_step
    if _dt_proc is not None and _dt_proc.poll() is None:
        log.info("[DT] Retrain already running — skipping.")
        return
    log.info("[DT] Triggering retrain in background (step %d) …", step)
    _dt_proc    = subprocess.Popen(
        ["Rscript", str(DT_DIR / "ctsmTMB_parameterestimation_callable.R")],
        cwd=str(DT_DIR), stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
    )
    _dt_at_step = step

def dt_collect(blocking: bool = False) -> bool:
    """Check (or wait) for a finished retrain. Returns True if model was updated."""
    global _dt_proc
    if _dt_proc is None:
        return False
    if blocking:
        _dt_proc.wait()
    elif _dt_proc.poll() is None:
        return False
    if _dt_proc.returncode != 0:
        log.error("[DT] Retrain failed:\n%s", _dt_proc.stderr.read().decode())
        _dt_proc = None
        return False
    out = DT_DIR / "cold_room_model_cont.json"
    if out.exists():
        shutil.copy(out, DT_FILE)
        log.info("[DT] Retrain finished — model updated.")
    _dt_proc = None
    return True

# ══════════════════════════════════════════════════════════════════════════════
# SERVICE: Julia Controller  (FlexOPTi — will be Docker)
#   FM → sensors JSON  +  DT file path
#   FM ← u_first (first optimal action vector)
#
#   Internally FlexOPTi handles everything else:
#     parse_digital_twin  →  ZOH discretisation of A, B, E
#     parse_forecasts     →  door-opening schedule, T_ambient forecast
#     fetch_market_prices →  day-ahead prices from Trading Manager (HTTP)
#     mpc_update          →  builds & solves MILP
#     parse_output        →  packages result dict
# ══════════════════════════════════════════════════════════════════════════════

_JL_EXPR = """\
using FlexOPTi, JSON, TimeZones, Dates
oy = FlexOPTi.optimize(ARGS[1], ARGS[2];
    pilot="Ewh", Hu=parse(Int,ARGS[4]), Δt=parse(Float64,ARGS[5]),
    loglevel="warn", logoutput="console",
    solver="HiGHS", mip_gap=0.01, milp_horizon=parse(Int,ARGS[4]),
    compute_datetime=now(tz"UTC"))
result = Dict("u_first"=>vcat(oy[:u][1,:],oy[:p3][1]),
              "cost"=>oy[:OPT_cost],"status"=>string(oy[:OPT_status]))
open(ARGS[3],"w") do io; JSON.print(io,result); end
"""

def jc_solve(x_sensor: np.ndarray, tmp: Path) -> np.ndarray | None:
    """Send sensor state → Julia Controller; receive first optimal action u."""
    sens = tmp / "sensors.json"
    out  = tmp / "mpc_out.json"
    sens.write_text(json.dumps({"x0": x_sensor.tolist()}))
    r = subprocess.run(
        ["julia", f"--project={FLEXOPTI}", "-e", _JL_EXPR,
         "--", str(DT_FILE), str(sens), str(out), str(Hu), str(Δt_s)],
        capture_output=True, text=True)
    if r.returncode != 0:
        log.error("[JC] FlexOPTi failed:\n%s", r.stderr)
        return None
    res = json.loads(out.read_text())
    log.info("[JC] status=%-20s  cost=%.4f", res["status"], res["cost"])
    return np.array(res["u_first"])

# ══════════════════════════════════════════════════════════════════════════════
# ★ DEMO ONLY — true-plant simulator (replaces real sensors + actuators)
#   On the pilot site: read x from sensor API, send u to actuator API instead.
# ══════════════════════════════════════════════════════════════════════════════

def _demo_load_plant():
    """Discretise continuous matrices from DT_FILE via ZOH for plant simulation."""
    ss = json.loads(DT_FILE.read_text())["state_space"]
    Ac, Bc, Ec = map(np.array, [ss["A"], ss["B"], ss["E"]])
    nx, nu, nd  = Ac.shape[0], Bc.shape[1], Ec.shape[1]
    M = np.zeros((nx + nu + nd,) * 2)
    M[:nx, :nx]      = Ac
    M[:nx, nx:nx+nu] = Bc
    M[:nx, nx+nu:]   = Ec
    Md = expm(M * Δt_s)
    return (Md[:nx, :nx], Md[:nx, nx:nx+nu], Md[:nx, nx+nu:],
            np.array(ss["x0"]), nx, nu)

def _demo_door_schedule(n_rooms, n_steps, door_w=4500.0, rate_per_day=8):
    """Poisson door-opening heat loads (hidden from MPC — FlexOPTi builds its own forecast)."""
    lam = rate_per_day / 24 / 60 * (Δt_s / 60)
    sched = np.zeros((n_rooms, n_steps))
    for rm in range(n_rooms):
        for t in range(n_steps):
            L, k, p = math.exp(-lam), 0, 1.0
            while p > L: p *= random.random(); k += 1
            if k - 1 > 0: sched[rm, t] = door_w
    return sched

# ══════════════════════════════════════════════════════════════════════════════
# FLEXIBILITY MANAGER — orchestration loop
# ══════════════════════════════════════════════════════════════════════════════

def main():
    random.seed(2); np.random.seed(2)
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    tmp = OUT_DIR / "tmp"; tmp.mkdir(exist_ok=True)

    # Start services
    trading_manager_start()
    dt_trigger_retrain(step=0)
    dt_collect(blocking=True)          # wait for initial model before first solve

    # ★ DEMO: load discretised plant matrices and generate hidden disturbances
    Ad, Bd, Ed, x0, nx, nu = _demo_load_plant()
    doors = _demo_door_schedule(n_rooms=5, n_steps=N_SIM)
    X = np.zeros((nx, N_SIM + 1)); X[:, 0] = x0
    U = np.zeros((nu, N_SIM))

    for t in range(N_SIM):

        # Poll for finished background retrain; reload plant matrices if updated
        if dt_collect():
            Ad, Bd, Ed, _, nx, nu = _demo_load_plant()   # ★ DEMO only

        # Trigger periodic retrain (non-blocking)
        if t > 0 and t % RETRAIN_EVERY == 0 and _dt_at_step != t:
            dt_trigger_retrain(step=t)

        # ── Read sensors (★ DEMO: use simulated state; production: HTTP call) ──
        x_sensor = X[:, t]

        # ── Call Julia Controller ─────────────────────────────────────────────
        log.info("[FM] Step %d/%d", t + 1, N_SIM)
        u = jc_solve(x_sensor, tmp)
        if u is None:
            u = np.zeros(nu)       # safe fallback
        U[:, t] = u

        # ── Apply u to true plant (★ DEMO only; production: send u to actuators)
        d = np.concatenate([[T_AMB], doors[:, t]])
        X[:, t + 1] = Ad @ X[:, t] + Bd @ u + Ed @ d + σ_noise * np.random.randn(nx)

    log.info("[FM] Done.  Total energy: %.1f kWh", U.sum() * Δt_s / 3600 / 1000)
    (OUT_DIR / "demo_results.json").write_text(
        json.dumps({"X_true": X.tolist(), "U": U.tolist()}, indent=2))


if __name__ == "__main__":
    main()
