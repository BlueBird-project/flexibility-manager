# EV Charging Scheduler — Project Overview

## 1. What this project does

A reinforcement-learning agent (Deep Q-Network) decides, every 15 minutes, whether each EV
charging station should charge or not — with the goal of minimizing electricity cost while
still fully charging every EV before it departs.

It was trained on real charging-session data (WeSmart, 2 stations, 2025) and real Belgian
day-ahead electricity prices (Belpex/ELEXYS).

Three setups are trained and can each be run as a service, all from the same code and the same
Dockerfile:

| setup | model folder | uses |
|---|---|---|
| **EV only** | `saved_models/EV/` | charging sessions + prices |
| **EV + PV** | `saved_models/EV_PV/` | + on-site solar production forecast |
| **EV + PV + building load** | `saved_models/EV_PV_Cons/` | + solar net of the building's own consumption |

## 2. How it's delivered

A trained model plus a small HTTP server, packaged as a Docker image (one image per setup, all
built from the same `Dockerfile.release`). The image runs
forever as a container; a real charger controller calls it once per 15-minute interval with
current conditions and gets back a charge / no-charge decision for each station. No Python or
ML knowledge is needed to *run* it — just Docker.

## 3. What you receive, and how to run it locally

You'll be given access to this Git repository — no separate file to receive or transfer. The
trained models' weights (`saved_models/EV/`, `EV_PV/`, `EV_PV_Cons/`) are committed in the repo, so
cloning it is enough to build a self-contained Docker image locally; nothing else needs to be sent
alongside it.

**To run it on your own machine:**

1. Install Docker Desktop if you don't already have it, and make sure it's running.
2. Install Git if you don't already have it.
3. Clone the repository and move into it:
   ```
   git clone https://github.com/BlueBird-project/flexibility-manager.git
   cd flexibility-manager/dt/WeSmart_EV_charging_BC2
   ```
4. Build the image for the setup you want. `MODEL_RUN` picks which model folder gets baked in:

   | setup | build command |
   |---|---|
   | EV only | `docker build -f Dockerfile.release --build-arg MODEL_RUN=EV -t ev-dqn-inference:ev-v1 .` |
   | EV + PV | `docker build -f Dockerfile.release --build-arg MODEL_RUN=EV_PV -t ev-dqn-inference:ev-pv-v1 .` |
   | EV + PV + load | `docker build -f Dockerfile.release --build-arg MODEL_RUN=EV_PV_Cons -t ev-dqn-inference:ev-pv-cons-v1 .` |

5. Start it as a background service. Each container needs its own name and host port, so several
   setups can run side by side:

   | setup | run command | URL |
   |---|---|---|
   | EV only | `docker run -d -p 8080:8080 --restart unless-stopped --name ev-dqn ev-dqn-inference:ev-v1` | `http://localhost:8080` |
   | EV + PV | `docker run -d -p 8081:8080 --restart unless-stopped --name ev-dqn-pv ev-dqn-inference:ev-pv-v1` | `http://localhost:8081` |
   | EV + PV + load | `docker run -d -p 8082:8080 --restart unless-stopped --name ev-dqn-pv-cons ev-dqn-inference:ev-pv-cons-v1` | `http://localhost:8082` |

6. Confirm it's up:
   ```
   docker ps
   ```
   should list the container. Then open `<URL>/health` in a browser (or `curl <URL>/health`) — it
   should return the model's configuration as JSON (see the API section below). A PV model reports
   `"has_pv": true`.

From there it's a normal always-on local service: send it `POST <URL>/decide` requests as described
in the API section, and it answers immediately (a network this size runs in well under a
millisecond on CPU — no GPU needed). `--restart unless-stopped` brings it back after a reboot or
crash.

To stop it: `docker stop <name>`. To start it again later: `docker start <name>` (no need to
`docker run` again — it remembers the settings). To pick up a retrained model later, `git pull`,
then `docker rm -f <name>` and repeat steps 4–5.

## 4. The API

### `GET /health`

Returns the model's own configuration (station IDs it knows about, price horizon, power
rating, and `deadline_guard_margin`) so an integrator can sanity-check their setup before
going live.

### `POST /decide`

**Request:**

```json
{
  "timestamp": "2026-08-31T14:30:00",
  "stations": [
    {"station_id": 1, "present": 1, "remaining_kwh": 12.4, "hours_to_departure": 2.5},
    {"station_id": 2, "present": 0}
  ],
  "prices": [0.142, 0.138, 0.130, 0.125, 0.121, 0.119, 0.118, 0.120, 0.124, 0.131, 0.140, 0.152,
             0.165, 0.171, 0.176, 0.180, 0.178, 0.170, 0.161, 0.150, 0.141, 0.135, 0.130]
}
```

| Field | Meaning |
|---|---|
| `timestamp` | Local wall-clock time (no timezone conversion) for the interval this decision covers |
| `stations[].station_id` | Must match the station IDs the model was trained on — `1` and `2` for these models |
| `stations[].present` | Whether an EV is currently plugged in there |
| `stations[].remaining_kwh` | Energy that EV still needs before it leaves (omit if not present) |
| `stations[].hours_to_departure` | Time left until it leaves (omit if not present) |
| `prices` | EUR/kWh, **forward-looking quarter-hour prices**: `prices[0]` is the price for `timestamp` itself, `prices[1]` is 15 minutes later, etc. Exactly **23 values** (the next 5 h 45 min) — `/health` reports this as `prices_required`. Send the plain day-ahead prices; the service averages and normalises them. |
| `pv_forecast` | **PV models only** (`/health` reports `has_pv: true`): forecast kWh of solar available for EV charging in each of the next 4 quarter-hours, same forward-looking convention as `prices`. For the EV + PV + load model, send solar production **minus** building consumption, floored at 0. Required for PV models, rejected by the EV-only model. |

For a PV model, the same request just adds one line:

```json
  "pv_forecast": [1.8, 2.1, 2.4, 2.6]
```

**Response:**

```json
{"timestamp": "2026-08-31T14:30:00", "decisions": [{"station_id": 1, "charge": 1}, {"station_id": 2, "charge": 0}]}
```

`charge: 1` or `0` per station, for the interval that just started. The controller applies it
and calls again 15 minutes later with fresh numbers. A station reported `present: 0` always
comes back `charge: 0`.

## 5. What happens inside, per request

1. **Build the state vector** (24 numbers for EV only, 28 with PV — every one scaled to 0…1 or −1…1):
   - 12 prices covering the next ~6 hours: the current quarter-hour price, then the average of each
     following half hour. They are scaled **within those 12** — 0 is the cheapest moment in the window,
     1 the most expensive — because the decision is about *when* to charge, and absolute price levels
     shift a lot between seasons
   - 2 price context values: the current price level (relative to typical prices) and how big the
     gap between cheapest and dearest is, so a nearly flat price curve isn't mistaken for a big
     opportunity
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
3. **Pick the higher-scoring action per station.**
4. **Apply the deadline guard.** If a station can no longer meet its deadline — that is, if
   `remaining_kwh > hours_to_departure × 9 kW × 0.96` — the decision is overridden to `charge: 1`
   regardless of what the network said. This is a deterministic safety floor, not part of the
   network; see the note in section 7 for why it exists. `/health` reports the margin as
   `deadline_guard_margin`.
5. **Force `charge: 0` for any absent station**, and return the result.

## 6. How it was trained

- Simulated 15-minute steps over real 2025 session and price data, split chronologically per
  station: **55% train / 15% validation / 30% test**.
- Reward each step: negative of `price × kWh drawn`, minus a shaping penalty if a station is
  falling behind its charging schedule, minus a larger penalty at departure for any energy left
  undelivered.
- Trained with standard DQN (target network, replay buffer of 10,000 transitions, batch size
  64, epsilon decayed from full exploration to greedy over 350,000 steps).
- The greedy policy is scored on the validation split after every episode and **the
  best-scoring weights are kept** — not the last episode's, which vary widely run to run.

> **Note:** the results below were measured with the previous input state (4 raw prices, 1 hour
> ahead). The price inputs have since changed to 12 window-scaled prices (~6 hours ahead); these
> numbers will be updated once the models are retrained and re-measured over several seeds.

**Results on held-out test data (6,658 kWh of demand), averaged over 15 random seeds**, against
an "always charge immediately" baseline:

| configuration | electricity cost | baseline | saving | energy not delivered |
|---|---|---|---|---|
| EV only     | €524.91 | €595.07 | **11.8%** | 3.47 kWh avg, 9.02 kWh worst seed |
| EV + PV     | €433.70 | €476.31 | **8.9%**  | 1.27 kWh avg, 9.55 kWh worst seed |
| EV + PV + building load | €456.47 | €507.88 | **10.1%** | 2.24 kWh avg, 5.48 kWh worst seed |

Worst case across all 45 runs is 9.55 kWh undelivered — 0.14% of demand — with no run exceeding
that in any configuration.

**Single numbers from a single training run are not meaningful here.** Identical settings vary
enormously by seed: an earlier EV+PV configuration produced anywhere from 0.03 to 69.91 kWh
undelivered depending only on the random seed. Quote averages over seeds, never one run.

## 7. Limitations

- **The deadline guard is doing real work.** The RL policy on its own occasionally strands an EV
  badly — in seed sweeps, 2 of 5 EV+PV runs left 38–70 kWh undelivered, costing far more in
  penalties than the entire electricity bill. The cause is a handful of unusually large/long
  sessions (one is 185 kWh over 33.5 h) that occur only in the test period; nothing in the
  training or validation data resembles them, and no hyperparameter setting tested fixed it
  reliably. The guard bounds that risk deterministically. Do not disable it
  (`--no-deadline-guard`) on a real site.
- **Train and test are different seasons.** One year of data split chronologically means training
  is Jan–Jul and testing is Oct–Dec, and the test period has roughly 4× less solar relative to
  demand. Results are therefore a cross-season generalisation test. More years of data is the
  real fix for the point above.
- If the model is retrained with different input features, this file needs updating by hand to
  match.
