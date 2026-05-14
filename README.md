# BlueBird Flexibility Manager

Demand-response control system for local energy assets across EU partner sites. Each asset type is a **pilot** (EWH cold rooms, building HVAC, water works, district heating). The system runs economic Model Predictive Control on a configurable cadence, writes optimal setpoints to a shared database, and publishes them to the Building Management System via a Knowledge Engine.

---

## Architecture

```
┌─────────────────┐   dynamics JSON    ┌──────────────────────┐
│  Digital Twin   │ ─────────────────► │                      │
│  (R/ctsmTMB)    │   shared volume    │  Flexibility         │
└─────────────────┘                    │  Controller          │
                                       │  (Julia/FlexOPTi)    │
┌─────────────────┐   DB forecasts     │                      │
│  Disturbances   │ ─────────────────► │  · reads dynamics    │
│  Forecast       │                    │  · reads forecasts   │
└─────────────────┘                    │  · fetches prices    │
                                       │  · writes setpoints  │
┌─────────────────┐   HTTP prices      │                      │
│  Trading        │ ─────────────────► │                      │
│  Manager (ext.) │                    └──────────────────────┘
└─────────────────┘                             │
                                                │ setpoints
┌─────────────────┐   shared DB        ┌────────▼─────────────┐
│  Control Room   │ ◄────────────────► │  Database (SQLite)   │
│  (dashboard)    │                    └──────────────────────┘
└─────────────────┘
```

**Services**

| Service | Language | Role |
|---------|----------|------|
| `fc` | Julia (FlexOPTi) | MPC loop — reads dynamics + forecasts + prices, writes setpoints |
| `dummy-forecast` | Python | Writes synthetic disturbance forecasts to DB on cadence |
| `dt-ewh` | R (ctsmTMB) | Parameter estimation; writes dynamics JSON to shared volume |
| Trading Manager | External | Fetches day-ahead electricity prices from ENTSOE |
| Database | SQLite | Shared time-series store (measurements, forecasts, setpoints, prices) |
| `control-room` | Python (Flask) | Operator dashboard — monitors system, queries KPIs |

**Data flow**

- **Shared volume** — Digital Twin writes `cold_room_model_continuous.json`; FC reads it at every iteration.
- **Database** — everything time-indexed: measurements, disturbance forecasts, setpoints.
- **HTTP** — FC fetches electricity prices from the Trading Manager. Falls back to file cache if TM is unreachable.

**Pilot dispatch** — `pilot` is passed as a string at the API boundary (`"Ewh"`, `"Montcada"`, …). Resolved once to a Julia type; `multiple dispatch` handles the rest. No string-matching inside core logic.

---

## Quick Start (Docker)

### Prerequisites

- Docker + Docker Compose v2
- Copy `.env.example` to `.env`

```bash
cp .env.example .env
```

### Run the EWH pilot

```bash
docker compose -f docker-compose.ewh.yml up --build
```

This starts three services:

| Service | Port | What it does |
|---------|------|--------------|
| `dummy-forecast` | 5000 | Initialises DB schema, writes synthetic forecasts every 15 min |
| `fc` | 8080 | Runs MPC loop (HiGHS solver, EWH pilot) |
| `control-room` | 9000 | Operator dashboard |

The FC completes its first MPC iteration within ~30 s of startup (pre-baked Digital Twin model, no blocking retrain).

### Run in background

```bash
docker compose -f docker-compose.ewh.yml up --build -d
docker compose -f docker-compose.ewh.yml logs -f fc
```

### Stop

```bash
docker compose -f docker-compose.ewh.yml down
```

### Trigger a Digital Twin retrain (optional)

The DT container is gated behind the `retrain` profile and runs once then exits.
Trigger it manually when you want updated model parameters:

```bash
docker compose -f docker-compose.ewh.yml run --rm dt-ewh
```

The FC picks up the new `cold_room_model_continuous.json` from the shared volume at the next iteration.

---

## Configuration

**`FlexOPTi/settings.json`** — operator-tunable parameters. Edit and rebuild (or bind-mount) to change behaviour without touching code.

```json
{
  "pilot":          "Ewh",
  "solver":         "HiGHS",
  "Hu":             48,
  "delta_t":        900.0,
  "retrain_every_h": 168,
  "market_country": "Germany",
  "tm_base_url":    "http://tm-service:9090",
  "http_port":      8080,
  "mip_gap":        0.01,
  "milp_horizon":   1
}
```

**`.env`** — secrets and environment-specific values. All `settings.json` fields can be overridden via environment variables (see `.env.example`).

---

## Control Room

Open **http://localhost:9000** in a browser for the live dashboard. It auto-refreshes every 15 s and shows:

- Service health (FC, dummy-forecast, control-room)
- FC optimization status (last cost, solver status, last setpoint)
- Recent setpoints from the database

### REST API

All endpoints return JSON (except `/` which returns HTML).

**Service health**

```bash
# This service
curl http://localhost:9000/health

# FC
curl http://localhost:8080/health

# Dummy forecast
curl http://localhost:5000/health

# All services at once
curl http://localhost:9000/services
```

**System status** — FC optimization state + last DB setpoints

```bash
curl http://localhost:9000/status | python3 -m json.tool
```

```json
{
  "fc": {
    "state": "ok",
    "status": "OPTIMAL",
    "cost": 0.1234,
    "u": [0.8, 0.6, 0.9, 0.7, 1.0],
    "updated": "2026-05-14T13:55:56+00:00"
  },
  "last_setpoints": [
    {"id": 42, "timestamp": "2026-05-14 13:55:56", "room_id": 1, "u": 0.8, "p3": 0.0},
    ...
  ],
  "db_last_write": "2026-05-14 13:55:56"
}
```

**KPIs and metrics**

```bash
curl http://localhost:9000/metrics | python3 -m json.tool
```

Returns: total setpoints written, total forecasts in DB, total measurements, recent setpoints table, latest sensor readings.

**Raw DB tables**

```bash
# Last 50 setpoints
curl http://localhost:9000/setpoints

# Upcoming forecast horizon
curl http://localhost:9000/forecasts
```

**FC detailed status** (direct)

```bash
curl http://localhost:8080/status | python3 -m json.tool
```

---

## Connecting the Trading Manager

If the Trading Manager is running, set `TM_BASE_URL` in `.env`:

```bash
TM_BASE_URL=http://tm-service:9090
```

The FC will query the TM for day-ahead prices at each iteration. It discovers the correct `market_id` from the TM registry by matching `market_country` (default: `"Germany"`). If the TM is unreachable, the FC falls back to a file cache (valid for 36 h), then to flat-zero prices with a logged warning.

To start the Trading Manager alongside the FC:

```bash
# From the TM repo
cd ~/DTU/BlueBird/TM/trading-manager/compose/local_dev
docker compose up -d

# Then start the FC, pointing at the TM
TM_BASE_URL=http://localhost:9090 docker compose -f docker-compose.ewh.yml up --build
```

---

## Development (without Docker)

### FlexOPTi (Julia)

```bash
cd FlexOPTi
julia --project=.
```

```julia
julia> using FlexOPTi
julia> ?optimize          # full API docs
```

Run a single MPC iteration:

```julia
oy = FlexOPTi.optimize(
    "data/EWH/inputs/cold_room_model_continuous.json",
    nothing, nothing;
    pilot    = "Ewh",
    Hu       = 48,
    solver   = "HiGHS",
    loglevel = "info",
)
```

Run the full self-driving controller loop (reads from `.env` or defaults):

```bash
julia --project=FlexOPTi FlexOPTi/scripts/ewh/dr_controller.jl
```

Run tests:

```bash
julia --project=FlexOPTi -e 'using Pkg; Pkg.test()'
```

### Dummy forecast (Python)

```bash
cd forecasting/dummy
DB_PATH=FlexOPTi/data/bb.db python dummy_forecast.py
```

### Control room (Python)

```bash
cd control_room
pip install flask requests
DB_PATH=FlexOPTi/data/bb.db FC_URL=http://localhost:8080 python main.py
# Open http://localhost:9000
```

---

## Project structure

```
flexibility-manager/
├── FlexOPTi/                  Julia optimization library
│   ├── src/
│   │   ├── FlexOPTi.jl        Module entry point
│   │   ├── core/
│   │   │   ├── types.jl       O / OX / OY structs + pilot dispatch
│   │   │   ├── fm_optimize.jl Public API entry point
│   │   │   ├── fetch_prices.jl Trading Manager client + cache
│   │   │   └── ...
│   │   └── pilots/
│   │       ├── ewh/           EWH cold-room pilot
│   │       └── montcada/      Montcada building pilot
│   ├── scripts/
│   │   └── ewh/
│   │       ├── dr_controller.jl  Self-driving MPC loop (container entrypoint)
│   │       └── mpc_base.jl       Standalone simulation reference
│   ├── data/EWH/inputs/       Pre-baked Digital Twin JSON
│   ├── settings.json          Operator config (pilot, solver, horizon, …)
│   └── Dockerfile
├── dt/
│   └── EWH_coldrooms_CSTM/    R parameter estimation scripts + Dockerfile
├── forecasting/
│   └── dummy/                 Dummy forecast service + Dockerfile
├── control_room/              Flask dashboard + Dockerfile
├── db/
│   └── init.sql               Complete DB schema
├── docker-compose.ewh.yml     EWH pilot compose file
└── .env.example               Environment variable reference
```

---

## Adding a new pilot

1. Add a type to `FlexOPTi/src/core/types.jl`
2. Add pilot methods (parse_digital_twin, parse_forecasts, mpc_update, …) under `src/pilots/<name>/`
3. Add its Digital Twin scripts under `dt/<name>/`
4. Create `docker-compose.<name>.yml` — only that pilot's DT container, FC is shared
