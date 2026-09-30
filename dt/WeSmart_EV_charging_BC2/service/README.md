# Live inference service

A long-running HTTP service that answers one charging decision whenever it's asked, for as long as
the container runs. It has no clock and no timer of its own — it does nothing until a request
arrives, and does nothing else after replying to it. The charger controller decides *when* to call
it (typically on a 15-minute tick, or immediately when a new car arrives) and keeps no state on the
service's side: every request must carry the full current picture, because nothing is remembered
between calls.

Two engines sit behind the same API:

| `ENGINE` | what answers `/decide` | needs |
|---|---|---|
| **`mpc`** *(default)* | a MILP solved per request (`src/mpc/`) | pulp + highspy |
| `dqn` | the legacy trained network (`saved_models/<run>/`) | torch + a checkpoint |

The MPC is the shipped engine: the problem is fully observable (prices published, departures
declared), so planning is provably optimal where the trained policy reached ~62% of the same saving.
`ENGINE=dqn` is kept only for comparison.

## Build & run

```
docker build -f Dockerfile.mpc -t ev-mpc:v1 .

# EV only
docker run -d -p 8080:8080 --restart unless-stopped --name ev-mpc    -e ENGINE=mpc ev-mpc:v1
# EV + PV, and EV + PV + building load (same image; see pv_forecast below)
docker run -d -p 8081:8080 --restart unless-stopped --name ev-mpc-pv -e ENGINE=mpc -e MPC_HAS_PV=1 ev-mpc:v1
```

No model files, no training, ~320 MB. Locally, without Docker:

```
ENGINE=mpc MPC_HAS_PV=1 python -m service.inference_server
```

Configuration (all optional): `MPC_HAS_PV`, `MPC_PRICE_QUARTERS` (96), `MPC_PV_QUARTERS` (96),
`MPC_DEFAULT_POWER_KW` (9.0), `MPC_SITE_MAX_POWER_KW` (unset), `MPC_TIME_LIMIT_S` (10),
plus `HOST` / `PORT`.

## API

### `GET /health`
Returns the configuration, so a caller can self-check before wiring up the real feed:
```json
{
  "status": "ok",
  "engine": "mpc",
  "station_ids": null,
  "power_kw": 9.0,
  "prices_required": 96,
  "has_pv": true,
  "pv_required": 96,
  "interval_minutes": 15,
  "site_max_power_kw": null,
  "time_limit_s": 10.0
}
```
`station_ids: null` means any station ID is accepted — the MPC is not tied to a fixed set, so a
fourth charger needs no redeploy. (`ENGINE=dqn` reports its trained station IDs instead, plus the
model-specific fields.)

### `POST /decide`
One decision for one 15-minute interval.

**Request:**
```json
{
  "timestamp": "2026-09-27T10:45:00",
  "stations": [
    {"station_id": 1, "present": 1, "remaining_kwh": 12.4, "hours_to_departure": 6.0},
    {"station_id": 2, "present": 0},
    {"station_id": 3, "present": 1, "remaining_kwh": 4.0, "hours_to_departure": 1.0, "max_power_kw": 11.0}
  ],
  "prices": [0.142, 0.138, "... 96 values ..."],
  "pv_forecast": [0.0, 0.0, "... 96 values, only when has_pv ..."]
}
```

| Field | Meaning |
|---|---|
| `timestamp` | Local wall-clock time of the interval being decided (same convention as the prices). It need **not** be on a 15-minute boundary: a call at 10:38 plans and decides the 10:38–10:45 remainder exactly, so you can trigger on a car arriving as well as on the tick. |
| `stations` | One entry per station you want a decision for. Any integer `station_id`. A `present: 0` station may omit the rest. |
| `stations[].remaining_kwh` | Energy the EV still needs, from telemetry. |
| `stations[].hours_to_departure` | Time until it leaves, as declared by the driver. |
| `stations[].max_power_kw` | Optional per-station charging power; defaults to `power_kw` from `/health`. |
| `prices` | €/kWh, consecutive **quarter-hour** values starting at `timestamp`, exactly `prices_required` of them. Belgian day-ahead prices are hourly — repeat each hourly value 4×. Beyond the supplied window the last price is held flat. |
| `pv_forecast` | kWh of solar available for EV charging per quarter-hour, same length and convention, required when `/health` reports `has_pv: true` and rejected otherwise. For the **building-load** setup send production **minus** consumption, floored at 0 — that is the only difference between EV+PV and EV+PV+load. Beyond the supplied window solar is assumed 0. |

**Response:**
```json
{
  "timestamp": "2026-09-27T10:45:00",
  "decisions": [
    {"station_id": 1, "charge": 0},
    {"station_id": 2, "charge": 0},
    {"station_id": 3, "charge": 1}
  ]
}
```

A station reported `present: 0`, or one that needs no more energy, always comes back `charge: 0`.

**Errors** are `400` with `{"error": "..."}` for anything wrong with the request (wrong array
length, unparseable timestamp, missing or unexpected `pv_forecast`, malformed JSON) and `500` for
anything unexpected (logged with a traceback). A bad request never takes the process down.

## Design notes

- **One MPC step per request.** `mpc_decide()` plans from `timestamp` to the last departure, then
  returns only the first interval; the rest is deliberately discarded. The 15-minute loop lives in
  the caller, and that is what makes it receding-horizon: each call sees updated remaining energy,
  new arrivals, changed departures and revised prices.
- **Why `mpc_decide` and not `solve_horizon`.** Both give the same first-interval decision, but the
  wrapper owns the "commit one interval" contract and is the single place where future state
  (a battery's charge level) will be handled. `solve_horizon` stays the offline/analysis entry point.
- **An EV that cannot be filled** has its requirement capped at what is physically deliverable, so
  it charges flat out instead of making the whole site's problem infeasible.
- **No solution at all** (e.g. a site power cap that cannot be met) falls back to charging every
  connected EV, and logs a warning — the service never returns an empty decision set.
- **The MPC path imports neither torch nor numpy.** `torch`, the network and `state_builder` are
  imported lazily inside `ModelBundle`, which is what keeps the MPC image at ~320 MB.
- `service/errors.py` holds the shared validation error so the MPC path doesn't need
  `state_builder.py` (and therefore numpy) at all.

## Legacy DQN engine

With `ENGINE=dqn` the service loads `MODEL_DIR` (default `saved_models/EV`) and runs the trained
network. `service/state_builder.py` deliberately duplicates the training-time state construction
(`src/env/`) rather than importing it, so the two can silently drift;
`python service/test_state_parity.py` asserts they still agree and must be run after touching
either side. That engine's request format differs (it wants `prices_required` = 23 by default and no
per-station power), which is why `/health` is the right way to discover what a given container expects.
