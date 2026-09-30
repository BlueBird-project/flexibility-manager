# EV Charging Scheduling — MPC (with a reinforcement-learning predecessor)

Smart charging for a site of EV chargers: decide per station whether to charge, so that the
electricity bill is minimised and every EV still leaves fully charged. Built on real session data
provided by the use case (WeSmart, **3 stations**, 2025) and real Belgian day-ahead prices.

The scheduler is a **model-predictive controller (MPC)**: it holds no clock or state of its own, and
answers whenever it's asked — which a caller does on a 15-minute tick, or immediately when a new car
arrives. Each answer re-plans every connected car's remaining stay from scratch as one small MILP,
commits only the interval that's starting now, and discards the rest. It optionally accounts for
on-site PV production, and for PV net of the building's own consumption.

A **Deep Q-Network** came first and is still in the repository. It is no longer the shipped
scheduler — see [Why MPC and not RL](#why-mpc-and-not-rl).

- `src/mpc/` — the optimiser (`solve_horizon`) and the MPC step (`mpc_decide`)
- `service/` — the always-on HTTP service, dockerised, `ENGINE=mpc` (or `dqn` for the legacy model)
- `run_mpc.py` — run the MPC over the dataset and score it against the no-scheduler baseline
- `main.py`, `src/env/`, `src/agent/` — the RL environment, agent and training CLI (legacy)

## Results

Test period, 433 sessions, 14,625 kWh. Baseline = charge every car immediately on arrival.

| setup | baseline | **MPC** | saving | energy not delivered |
|---|---|---|---|---|
| EV only | €1,319.47 | **€1,085.75** | **−17.7%** | **0.00 kWh** |
| EV + PV | €942.59 | **€710.30** | **−24.6%** | **0.00 kWh** |
| EV + PV + building load | €1,025.75 | **€787.45** | **−23.2%** | **0.00 kWh** |

The MPC is **optimal for on/off charging**: solving each session exactly with whole-interval
decisions also gives €1,085.62, which the rolling controller matches given a long enough look-ahead.
A €1,081.08 bound exists but assumes continuously modulated power, which these chargers don't do.

## Quick start

```
python -m venv .venv
.venv\Scripts\activate          # Windows
pip install -r requirements.txt
```

Run the MPC over the dataset:

```
python run_mpc.py                                                     # EV only
python run_mpc.py --pv src/env/dataset/solar_production.csv           # EV + PV
python run_mpc.py --pv src/env/dataset/solar_production.csv \
                  --consumption "src/env/dataset/common_areas_consumption(in).csv"
```

It prints cost, energy delivered and unmet energy next to the baseline, and writes CSVs to
`outputs/mpc/<setup>/`. Useful flags: `--split {train,val,test,all}`, `--horizon-hours` (default 24),
`--max-steps`, `--site-max-power-kw`, `--time-limit`.

Run it as a service (see [service/README.md](service/README.md) for the full API):

```
docker build -f Dockerfile.mpc -t ev-mpc:v1 .
docker run -d -p 8080:8080 --restart unless-stopped --name ev-mpc -e ENGINE=mpc ev-mpc:v1
```

`pulp` requires Python ≥ 3.12 (the image uses 3.12-slim).

## How the MPC works

Two layers, in `src/mpc/mpc.py`:

- **`solve_horizon(evs, prices, now, site_max_power_kw=None, pv_forecast=None)`** — the optimiser.
  One MILP over a 15-minute grid from `now` to the last departure, split also at each arrival and
  departure so partial intervals are exact. One binary variable per EV per interval (charge at full
  power or not), locked to 0 outside that EV's connection window. Minimises Σ price × energy bought.
  Each EV must receive its required energy; where a PV forecast is given, only the grid part of each
  interval is paid for. Optional site-wide power cap. Solved with PuLP + HiGHS.
- **`mpc_decide(...)`** — one receding-horizon step: calls `solve_horizon` and returns only the
  decision for the interval starting at `now`. The caller commits that, then calls again at the next
  trigger (a 15-minute tick, or a car arriving) with the then-current cars.

Both `pv_forecast` and `site_max_power_kw` are optional; with both `None` the problem reduces exactly
to EV-only. **EV+PV and EV+PV+building-load are the same code path** — the caller sends solar minus
building consumption, floored at 0.

An EV that cannot physically be filled in its remaining time has its requirement capped at what is
deliverable, so it charges flat out rather than making the whole site's problem infeasible.

### The datasets

`wesmart_ev_sessions_3stations.csv` (default) is built from the raw charger logs by
`src/env/dataset/build_sessions.py`: a session is one contiguous run of `Connected == 1`, with
`req_kwh` the energy charged during it. Sessions are kept only if the car is plugged in for at least
**1.15×** the time needed to deliver its energy at 9 kW — below that there is no flexibility to
schedule. That leaves **1,442 of 1,586** sessions (48,050 kWh). The 2-station file
`wesmart_ev_sessions.csv` is still there; select it with `--sessions`.

Charging power is a constant **9 kW** per station, from the raw logs (per-session medians of 9.13,
9.61 and 8.86 kW). Prices come from [ELEXYS](https://www.elexys.be/insights/spot-belpex) at
15-minute resolution; solar and building consumption are separate 15-minute series.

## Why MPC and not RL

The DQN worked: about **11% cheaper** than the baseline. But it captured only **62%** of the saving
the optimiser reaches, left a little energy undelivered, and its result varied by several percent
between random seeds.

The cause is structural, not a tuning failure. Prices are published up to 35 hours ahead and
departure times are declared, so the problem is **fully observable and deterministic** — and in that
setting planning beats learning, because there is nothing to learn that cannot be computed directly.
Extensive tuning (reward scale, discount, replay buffer, network size, price-window length, penalty
sweeps over hundreds of runs) moved the result by less than the seed-to-seed noise.

This would change if prices became **forecasts** instead of a published schedule: a committed plan
built on wrong numbers degrades, while a reactive policy can adapt. The MPC already re-plans every
15 minutes, which absorbs much of that error, so any future RL work should be measured against MPC
under forecast error rather than against the no-scheduler baseline.

### Running the legacy RL

Still functional and unchanged:

```
python main.py train                                    # 3 chargers, EV only
python main.py train --pv src/env/dataset/solar_production.csv
python main.py test
```

Models and reports go to `saved_models/3_chargers/<setup>/` and `outputs/3_chargers/<setup>/`. The
agent is a DQN whose network emits one independent binary decision per station (a VDN-style
factorisation rather than a `2^n` joint action space); training holds out a validation split,
keeps the best-scoring weights, clips state features to the range seen in training, and applies a
deadline guard that forces charging when a car can no longer finish. `--help` documents the flags.

For serving that model instead of the MPC, set `ENGINE=dqn`; note its request format differs
(23 prices by default) — `/health` reports what a given container expects.
