# Live inference service

A long-running HTTP service wrapping a trained checkpoint. Loads the model once at startup, then
answers one decision request per 15-minute interval for as long as the container runs. No pandas,
no dataset, no training code — the image only needs `numpy` + `torch` (CPU).

## Build & run

```
docker build -t ev-dqn-inference .
docker run -p 8080:8080 -v "$(pwd)/saved_models/EV_PV_Cons:/models:ro" ev-dqn-inference
```

or with the included compose file (defaults to `saved_models/EV`; override with `MODEL_DIR_HOST`):

```
MODEL_DIR_HOST=./saved_models/EV_PV_Cons docker compose up --build
```

`MODEL_DIR` inside the container must point at a folder with `q_state_dict.pth` + `metadata.json` (both
written by `python main.py train`) — mount it read-only rather than baking weights into the image, so
retraining only needs a container restart, not a rebuild.

Config is via environment variables: `MODEL_DIR` (default `/models`), `HOST` (default `0.0.0.0`), `PORT`
(default `8080`).

## API

### `GET /health`
Returns the loaded model's configuration, so a caller can self-check before wiring up the real feed:
```json
{
  "status": "ok",
  "station_ids": [1, 2],
  "power_kw": 9.0,
  "price_horizon": 4,
  "has_pv": true,
  "pv_horizon": 4,
  "interval_minutes": 15,
  "observation_size": 18
}
```

### `POST /decide`
One decision for one 15-minute interval.

**Request:**
```json
{
  "timestamp": "2026-08-31T14:30:00",
  "stations": [
    {"station_id": 1, "present": 1, "remaining_kwh": 12.4, "hours_to_departure": 2.5},
    {"station_id": 2, "present": 0}
  ],
  "prices": [0.142, 0.138, 0.130, 0.125],
  "pv_forecast": [1.8, 2.1, 2.4, 2.6]
}
```

| Field | Meaning |
|---|---|
| `timestamp` | Local wall-clock time (same convention as the training data — no timezone conversion applied) of the interval this decision is for. |
| `stations` | One entry per station the model was trained on — see `/health` for the expected `station_ids` (order doesn't matter, matched by id). A `present: 0` station may omit `remaining_kwh`/`hours_to_departure`. |
| `prices` | €/kWh, **forward-looking**, length must equal `price_horizon` from `/health`. `prices[0]` is the price for `timestamp` itself. Belgian day-ahead prices are hourly — repeat each hourly value 4× to fill the quarter-hours it covers before sending. |
| `pv_forecast` | Net kWh available for EV charging (production minus building consumption, already floored at 0) per quarter-hour, forward-looking, length must equal `pv_horizon`. Omit entirely if `/health` reports `has_pv: false`; required if `true`. |

**Response:**
```json
{
  "timestamp": "2026-08-31T14:30:00",
  "decisions": [
    {"station_id": 1, "charge": 1},
    {"station_id": 2, "charge": 0}
  ]
}
```

A station reported `present: 0` always comes back `charge: 0` — the network was never trained to produce a
meaningful decision for an empty station (training ignores the action bit there too), so the service doesn't
forward whatever it happened to output.

**Errors** are `400` with `{"error": "..."}` for anything wrong with the request (wrong array length, unknown
station id, missing PV forecast, malformed JSON, ...) and `500` for anything unexpected (logged with a full
traceback). A bad request never takes the process down.

## Design notes

- **`state_builder.py` duplicates training logic on purpose.** Training derives `remaining_kwh`/
  `hours_to_departure` from a sessions dataframe walked forward in time; the live service gets them handed
  directly by charger telemetry. Sharing code wasn't possible without dragging `pandas`/`EnergyEnv` into the
  serving image. `test_state_parity.py` (not part of the image — dev-only) builds a synthetic `EnergyEnv` and
  asserts the two produce byte-identical vectors; **run it after touching either side.**
- **Retraining changes the contract.** `price_horizon`, `pv_horizon`, station set, and normalisation constants
  all come from `metadata.json`. A checkpoint trained with different horizons than the caller is sending will
  be rejected with a clear `400`/startup error rather than silently misinterpreting the input — see the
  `observation_size` cross-check in `StateBuilder.from_metadata`.
- **PV forecast accuracy is a real train/serve gap.** Training reads PV "forecast" straight off historical
  production (perfect foresight); a real weather-driven forecast has error the model never saw. This is why
  the horizon defaults to a short 1 hour — keep it in mind if charging decisions look PV-overconfident in
  production.
