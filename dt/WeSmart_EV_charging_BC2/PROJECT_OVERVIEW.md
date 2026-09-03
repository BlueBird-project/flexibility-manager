# EV Charging Scheduler — Project Overview

## 1. What this project does

A reinforcement-learning agent (Deep Q-Network) decides, every 15 minutes, whether each EV
charging station should charge or not — with the goal of minimizing electricity cost while
still fully charging every EV before it departs.

It was trained on real charging-session data (WeSmart, 2 stations, 2025) and real Belgian
day-ahead electricity prices (Belpex/ELEXYS).

**Status: EV-only.** The codebase also supports netting on-site solar (PV) production against
building consumption, but that part has not been dockerized yet — everything below describes
the EV-only model that exists today.

## 2. How it's delivered

A trained model plus a small HTTP server, packaged as a single Docker image. The image runs
forever as a container; a real charger controller calls it once per 15-minute interval with
current conditions and gets back a charge / no-charge decision for each station. No Python or
ML knowledge is needed to *run* it — just Docker.

## 3. What you receive, and how to run it locally

You'll be handed a single file: a `.tar` (roughly 275 MB) exported from a self-contained Docker
image — the trained model's weights are baked in, so nothing else needs to be sent alongside it.

**To run it on your own machine:**

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/) (Windows, Mac, or
   Linux) if you don't already have it, and make sure it's running.
2. Load the image from the file (run once, from wherever the `.tar` was saved):
   ```
   docker load -i ev-dqn-ev-v1.tar
   ```
3. Start it as a background service, listening on port 8080:
   ```
   docker run -d -p 8080:8080 --restart unless-stopped --name ev-dqn ev-dqn-inference:ev-v1
   ```
4. Confirm it's up:
   ```
   docker ps
   ```
   should list a container named `ev-dqn`. Then open `http://localhost:8080/health` in a
   browser (or `curl http://localhost:8080/health`) — it should return the model's
   configuration as JSON (see the API section below).

From there it's a normal always-on local service: send it `POST http://localhost:8080/decide`
requests as described in the API section, and it answers immediately (a network this size runs
in well under a millisecond on CPU — no GPU needed).

To stop it: `docker stop ev-dqn`. To start it again later: `docker start ev-dqn` (no need to
`docker run` again — it remembers the settings).

## 4. The API

### `GET /health`

Returns the model's own configuration (station IDs it knows about, price horizon, power
rating) so an integrator can sanity-check their setup before going live.

### `POST /decide`

**Request:**

```json
{
  "timestamp": "2026-08-31T14:30:00",
  "stations": [
    {"station_id": 1, "present": 1, "remaining_kwh": 12.4, "hours_to_departure": 2.5},
    {"station_id": 2, "present": 0}
  ],
  "prices": [0.142, 0.138, 0.130, 0.125]
}
```

| Field | Meaning |
|---|---|
| `timestamp` | Local wall-clock time (no timezone conversion) for the interval this decision covers |
| `stations[].station_id` | Must match the station IDs the model was trained on — `1` and `2` for this model |
| `stations[].present` | Whether an EV is currently plugged in there |
| `stations[].remaining_kwh` | Energy that EV still needs before it leaves (omit if not present) |
| `stations[].hours_to_departure` | Time left until it leaves (omit if not present) |
| `prices` | EUR/kWh, **forward-looking**: `prices[0]` is the price for `timestamp` itself, `prices[1]` is 15 minutes later, etc. Exactly 4 values for this model |

There is no `pv_forecast` field for this model — that field exists in the API for PV-enabled
models but is unused here.

**Response:**

```json
{"timestamp": "2026-08-31T14:30:00", "decisions": [{"station_id": 1, "charge": 1}, {"station_id": 2, "charge": 0}]}
```

`charge: 1` or `0` per station, for the interval that just started. The controller applies it
and calls again 15 minutes later with fresh numbers. A station reported `present: 0` always
comes back `charge: 0`.

## 5. What happens inside, per request

1. **Build the state vector** (14 numbers for this model):
   - 4 forward-looking prices
   - 2 time-of-day values (`sin`/`cos` of time-of-day, so midnight and 23:59 look "close" to
     the network instead of maximally far apart)
   - 4 numbers per station: `present`, normalized `remaining_kwh`, normalized
     `hours_to_departure`, and a derived `urgency = remaining_kwh / (hours_to_departure ×
     9 kW)` — how close the EV is to needing every remaining hour at full charging power just
     to finish in time.
2. **Feed it through the network**: a small 2-layer MLP (64 → 64 hidden units) that outputs 2
   scores per station — one for "don't charge," one for "charge." This is a DQN value network,
   not a classifier: each score estimates the long-run cost of that choice, and the higher one
   wins. Each station's decision is made independently, so the approach scales to any number of
   stations without the number of possible actions exploding.
3. **Pick the higher-scoring action per station**, force it to 0 for any absent station, return
   the result.

## 6. How it was trained

- Simulated 15-minute steps over real 2025 session and price data, split chronologically 70/30
  per station into train and test sets.
- Reward each step: negative of `price × kWh drawn`, minus a shaping penalty if a station is
  falling behind its charging schedule, minus a larger penalty at departure for any energy left
  undelivered.
- Trained with standard DQN (target network, replay buffer of 10,000 transitions, batch size
  64, epsilon decayed from full exploration to greedy over 350,000 steps).
- **Result on held-out test data:** essentially zero unmet energy (6,658.02 kWh required,
  6,658.02 kWh delivered) at **€529.62** total electricity cost, versus **€611.83** for an
  "always charge immediately" baseline — about **13% cheaper for the same service level**.

## 7. Limitation

If the model is ever retrained with different input
features, this file needs to be updated by hand to match.
