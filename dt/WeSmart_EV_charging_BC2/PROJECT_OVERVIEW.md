# EV Charging Scheduler — Project Overview

## 1. What this project does

It decides, for each EV charging station, whether to charge or not — minimising the electricity
bill while still fully charging every EV before it departs.

The decision is made by a **model-predictive controller (MPC)**. It has no clock of its own: it sits
idle until asked, then plans the whole remaining stay of every connected car as a small optimisation
problem and commits only the interval starting right now. A caller triggers it on a 15-minute tick,
or sooner if a car arrives, and gets an up-to-date decision either way.

It was developed on real charging-session data (WeSmart, **3 stations**, 2025) and real Belgian
day-ahead electricity prices (Belpex/ELEXYS). Three setups are supported:

| setup | uses |
|---|---|
| **EV only** | charging sessions + electricity prices |
| **EV + PV** | + on-site solar production |
| **EV + PV + building load** | + solar net of the building's own consumption |

## 2. Results

Measured on the held-out test period (433 sessions, 14,625 kWh) against charging every car
immediately on arrival, which is what happens with no scheduler:

| setup | baseline cost | **MPC cost** | **saving** | energy not delivered |
|---|---|---|---|---|
| EV only | €1,319.47 | **€1,085.75** | **−17.7%** | **0.00 kWh** |
| EV + PV | €942.59 | **€710.30** | **−24.6%** | **0.00 kWh** |
| EV + PV + building load | €1,025.75 | **€787.45** | **−23.2%** | **0.00 kWh** |

Every EV leaves fully charged in all three setups. With solar, the share of on-site production
used by the cars rises from 28% to 32%.

**The MPC is provably optimal for this problem.** Solving each session exactly with whole-interval
decisions gives €1,085.62 — the same figure the rolling controller reaches with a long enough
look-ahead. A theoretical bound of €1,081.08 exists but assumes chargers can modulate power
continuously; at fixed-power on/off charging it is unreachable, and the €4.54 difference is simply
what on/off costs versus dimming.

### Why not reinforcement learning

Earlier versions used a Deep Q-Network. It worked — about 11% cheaper than the baseline — but it
captured only **62%** of the saving the optimiser achieves, left a little energy undelivered, and
its result moved by several percent from one random seed to the next.

The reason is structural: prices are published in advance and departure times are declared, so the
problem is fully observable and deterministic. Under those conditions planning beats learning —
there is nothing to learn that cannot simply be computed. The RL code is still in the repository
(`main.py`, `src/agent/`, `src/env/`) and the analysis stands, but the shipped scheduler is the MPC.

That trade-off would change if prices became **forecasts** rather than a published schedule: a plan
built on wrong numbers degrades, and that is where a learned, reactive policy could earn its place
again. The MPC already re-plans every 15 minutes, which absorbs much of that error.

## 3. How it's delivered

A single Docker image containing the optimiser and a small HTTP server. The container runs forever,
waiting — it never does anything until called. A charger controller calls it with the current
conditions, whenever a decision is needed, and gets back a charge / no-charge decision per station.

There are **no model files and no training step** — nothing is baked in, so **one image serves all
three setups**, chosen by an environment variable at run time. The image is ~320 MB.

## 4. What you receive, and how to run it locally

You'll be given access to this Git repository — nothing else needs to be sent.

1. Install [Docker Desktop](https://www.docker.com/products/docker-desktop/) and make sure it's running.
2. Install [Git](https://git-scm.com/downloads).
3. Clone and enter the project:
   ```
   git clone https://github.com/BlueBird-project/flexibility-manager.git
   cd flexibility-manager/dt/WeSmart_EV_charging_BC2
   ```
4. Build the image once:
   ```
   docker build -f Dockerfile.mpc -t ev-mpc:v1 .
   ```
5. Start the setup you want. Several can run side by side on different ports:

   | setup | command | URL |
   |---|---|---|
   | EV only | `docker run -d -p 8080:8080 --restart unless-stopped --name ev-mpc -e ENGINE=mpc ev-mpc:v1` | `http://localhost:8080` |
   | EV + PV *(and + building load)* | `docker run -d -p 8081:8080 --restart unless-stopped --name ev-mpc-pv -e ENGINE=mpc -e MPC_HAS_PV=1 ev-mpc:v1` | `http://localhost:8081` |

   EV+PV and EV+PV+building-load use the **same container**: for the latter, send solar **minus**
   building consumption (floored at 0) in the `pv_forecast` field.

6. Check it:
   ```
   docker ps
   ```
   should list the container as `healthy`. Open `<URL>/health` — it returns the configuration as
   JSON (see section 5).

From there it is a normal always-on service: it does nothing on its own and only answers when
called. Send `POST <URL>/decide` whenever a decision is needed — on your own 15-minute tick, or
immediately when a car plugs in. `--restart unless-stopped` brings it back after a crash or a reboot.

To stop: `docker stop ev-mpc`. To start again: `docker start ev-mpc`. To pick up code changes:
`git pull`, then rebuild and recreate the container.

### Settings (all optional)

| variable | default | meaning |
|---|---|---|
| `ENGINE` | `mpc` | `mpc`, or `dqn` to run the legacy trained model instead |
| `MPC_HAS_PV` | `0` | `1` to require a `pv_forecast` in every request |
| `MPC_PRICE_QUARTERS` | `96` | how many quarter-hour prices a request must carry (96 = 24 h) |
| `MPC_PV_QUARTERS` | `96` | same for `pv_forecast` |
| `MPC_DEFAULT_POWER_KW` | `9.0` | charging power when a station doesn't state its own |
| `MPC_SITE_MAX_POWER_KW` | unset | cap on total power across all chargers |
| `MPC_TIME_LIMIT_S` | `10` | solver time limit per request |

## 5. The API

### `GET /health`

Returns the configuration, so an integrator can check their setup before going live:

```json
{"status": "ok", "engine": "mpc", "station_ids": null, "power_kw": 9.0,
 "prices_required": 96, "has_pv": true, "pv_required": 96,
 "interval_minutes": 15, "site_max_power_kw": null, "time_limit_s": 10.0}
```

`station_ids: null` means any station ID is accepted — adding a fourth charger needs no change here.

### `POST /decide`

**Request:**

```json
{
  "timestamp": "2026-09-27T10:45:00",
  "stations": [
    {"station_id": 1, "present": 1, "remaining_kwh": 12.4, "hours_to_departure": 6.0},
    {"station_id": 2, "present": 0},
    {"station_id": 3, "present": 1, "remaining_kwh": 4.0, "hours_to_departure": 1.0, "max_power_kw": 11.0}
  ],
  "prices": [0.142, 0.138, "... 96 values in total ..."],
  "pv_forecast": [0.0, 0.0, "... 96 values, only when has_pv ..."]
}
```

| field | meaning |
|---|---|
| `timestamp` | local wall-clock time of the interval being decided. Need not be on a 15-minute boundary — a call at 10:38 decides the 10:38–10:45 remainder correctly |
| `stations[].station_id` | any integer |
| `stations[].present` | whether an EV is plugged in |
| `stations[].remaining_kwh` | energy that EV still needs (omit if not present) |
| `stations[].hours_to_departure` | time left until it leaves (omit if not present) |
| `stations[].max_power_kw` | **optional**, per-station charging power; defaults to `power_kw` from `/health` |
| `prices` | EUR/kWh, consecutive **quarter-hour** values starting at `timestamp`. Exactly `prices_required` of them. Day-ahead prices are hourly — repeat each value 4× |
| `pv_forecast` | **PV setups only**: kWh of solar available for charging per quarter-hour, same length and convention. For the building-load setup, send solar **minus** consumption, floored at 0 |

**Response:**

```json
{"timestamp": "2026-09-27T10:45:00",
 "decisions": [{"station_id": 1, "charge": 0}, {"station_id": 2, "charge": 0}, {"station_id": 3, "charge": 1}]}
```

`charge: 1` or `0` per station, for the interval that just started. The controller applies it, then
calls again whenever the next decision is needed — on its own 15-minute tick, or sooner if a car
arrives or a station's numbers change — with the then-current `remaining_kwh` and
`hours_to_departure`. The service holds no state between calls; nothing is remembered unless it's in
the request. A station reported `present: 0` always comes back `charge: 0`.

**Errors** are `400` with `{"error": "..."}` for anything wrong with the request (wrong array
length, unparseable timestamp, missing `pv_forecast`, malformed JSON) and `500` for anything
unexpected. Either way the service stays up; one bad request never takes it down.

## 6. What happens inside, per request

1. **Check the message** — timestamp, stations, and exactly the required number of prices. Anything
   missing or the wrong length gets a `400` explaining what was wrong.
2. **Turn the prices into a timeline**: "this is the price at 10:45, at 11:00, …" for the next 24 h.
3. **Turn each occupied station into a car to plan for**: energy still needed, time left, charging
   power. Empty stations and already-full cars are set aside and get "don't charge".
4. **If no car needs energy, stop** and answer "don't charge" for everything — no solving needed.
5. **Plan every car's whole stay at once.** Find the cheapest set of 15-minute blocks that still
   fills every car in time. Where there is solar, those blocks are free, so they get used first. If
   one car physically cannot be filled, it charges flat out and doesn't spoil the plan for the others.
6. **Keep only the next 15 minutes** of that plan and discard the rest — it was only needed to know
   whether charging *now* is a good idea.
7. **Reply** with charge / don't charge per station.

The whole call takes about 0.2 s, of which the planning itself is ~5 ms.

Re-planning from scratch every interval is what makes it robust: the remaining energy comes from
live telemetry, new arrivals appear immediately, a driver changing their departure time is picked
up, and revised prices are used as soon as they arrive.

## 7. Limitations

- **Prices and departure times are assumed known.** Day-ahead prices are published 11–35 h ahead,
  so that holds today; declared departures come from the driver and may be wrong. A car leaving
  earlier than declared is the main real-world risk, since the plan spends its slack.
- **Forecast prices are not yet modelled.** When prices become forecasts, plan quality drops with
  forecast error. Re-planning every 15 minutes absorbs part of that, but it has not been measured.
- **Solar beyond the supplied forecast is assumed zero**, and prices beyond the supplied window are
  held flat — deliberately conservative, never counting on data it wasn't given.
- **Charging is on/off at a fixed power.** Modulating power would save a further ~0.4%.
- **No battery yet.** The solver is structured to take one as an optional input later.
- **The site power cap is implemented but unused** (no field in the API yet); it matters once a
  battery shares the grid connection.
- Sessions are treated independently, so nothing prevents all three chargers running at once.
