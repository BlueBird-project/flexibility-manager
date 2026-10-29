# EWH - BC7 - EV Charging Scheduling with MPC
This folder decides **when and how fast to charge electric vehicles (EVs)** at the EWH charging station
(BC7), so that charging happens when electricity is cheapest, every car still gets its energy before
it leaves, and the site never draws more than its power limit.

It does this with **MPC (Model Predictive Control)**: an optimiser that computes the cheapest
charging plan for all cars, and re-plans whenever something changes.

The folder contains:
- **The MPC solver** ([src/mpc/mpc.py](src/mpc/mpc.py)), used by both programs below
- **MPC over a historical dataset** ([run_mpc_dataset.py](run_mpc_dataset.py)): computes the best
  possible charging for every past session and its total cost
- **The running-forever service** ([run_mpc_service.py](run_mpc_service.py)): runs in Docker, talks
  to the station over MQTT and sends a charging plan for every car
- The earlier **Reinforcement Learning (DQN)** approach, kept for reference (see the end of this page)

## Why MPC instead of Reinforcement Learning
Reinforcement learning is useful when you *don't know* how the system behaves: the agent has to learn
it by trial and error. Here we **know everything we need** to compute the best decision directly:

| Input | Where it comes from |
|---|---|
| when each car arrives | the station tells us (session `started`) |
| when it leaves | the driver's survey answer (stay 12 h or 24 h) |
| how much energy it needs | the survey: battery level (`soc`) and battery size (`capacity`) |
| how fast it can charge | the connector maximum (22 kW), then measured from the meter |
| electricity prices | day-ahead prices, known in advance (trading manager) |
| site limit | fixed (30 kW for the 4 connectors) |

With all inputs known, the problem "charge every car enough, at minimum cost, within the limit" can be
**solved exactly** as an optimisation problem (a MILP, solved with [PuLP](https://coin-or.github.io/pulp/)
+ HiGHS). Compared to the DQN this means:
- **The best plan, not an approximation**: the solver finds the cheapest plan that is possible.
- **Hard guarantees**: the energy each car needs and the site limit are rules the plan must follow,
  not penalties the agent may decide to accept.
- **No training, no tuning**: no neural network, no reward/penalty weights, no saved models; a new
  site limit or price market is just a different number.
- **Explainable**: every plan can be checked by hand (prices, energy, limit).

The "Predictive Control" part: the future is not fully known (a new car can arrive, a car can be
slower than expected), so the plan is **recomputed at every event and every 15 minutes** with the
latest information. Only the next part of the plan really matters; the rest is updated as we go.

On the historical dataset (209 sessions, 11 kW per car), MPC costs **357.22 EUR**, against
**539.18 EUR** when every car simply charges as soon as it arrives until it is full (**34% cheaper**),
with every car fully charged. With a 22 kW shared site limit it costs 358.43 EUR.

# How to run the project

### Install Python
First, Python is required. Download Python from the **OFFICIAL WEBSITE**.

### Create and activate a virtual environment
From the folder `dt/EWH_EV_charging_BC7`, create it:

```python -m venv .venv```

and activate it (Windows):

```.venv\Scripts\activate```

or (macOS/Linux):

```source .venv/bin/activate```

### Install dependencies
```pip install -r requirements.txt```

## 1. MPC over the historical dataset
```python run_mpc_dataset.py --site-cap-kw 22```

This command:
- loads the EV sessions ([ewh_cleaned_rtu.csv](src/env/dataset/ewh_cleaned_rtu.csv)) and the
  15-minute prices ([price_two_years_15min.xlsx](src/env/dataset/price_two_years_15min.xlsx))
- groups sessions that overlap in time (they share the site limit) and solves each group exactly
- prints the total cost, plus checks: groups not solved optimally, cars that did not get all their
  energy, and moments the site limit was exceeded (all should be 0)

Optional arguments:
~~~
--site-cap-kw KW      shared power limit of all stations together (default: no limit)
--compare-no-cap      also solve without the limit and print the cost difference
--sessions PATH       (default = src/env/dataset/ewh_cleaned_rtu.csv)
--price PATH          (default = src/env/dataset/price_two_years_15min.xlsx)
~~~
Each car charges at full power or not at all in each 15-minute interval; sessions without a
`power_kw` column are assumed to charge at 11 kW.

## 2. The running-forever service (MQTT)
[run_mpc_service.py](run_mpc_service.py) is the real controller for the Hotel Prinz-Luitpold-Bad
station (`plb`, connectors c1-c4). It follows the document *"bluebird – MQTT Interface for
fm_charging (IMEC)"*:
- it **listens** on the bluebird MQTT broker: cars starting/stopping, the driver's survey answer,
  meter readings, connector status, and the station's answers to our plans
- it **sends** one charging plan per flexible car (a kW limit for every quarter-hour until it
  leaves), a baseline (expected energy without optimisation), and its online/offline status
- it gets the prices from the trading manager

Start it with Docker (after putting the MQTT password in `secrets/mqtt_password.txt`):

```docker compose up -d --build```

**Everything about the service** (how it decides step by step, all messages, settings, logs,
troubleshooting) is explained in plain language in **[MPC_SERVICE.md](MPC_SERVICE.md)**.

# The earlier approach: Reinforcement Learning (DQN)
Before the MPC, this folder used a **Deep Q-Network** agent
([DQN paper](https://doi.org/10.48550/arXiv.1312.5602)) that learned, per 15-minute interval and per
station, whether to charge (1) or not (0). The code is still here:
- [src/env/env.py](src/env/env.py): the simulated charging environment (4 stations, sessions, prices,
  optional shared power limit)
- [src/agent/dqn_agent.py](src/agent/dqn_agent.py): the DQN agent, with a neural network
  ([NN.py](src/agent/helpers/NN/NN.py)) and a replay buffer
  ([replay_buffer.py](src/agent/helpers/replay_buffer/replay_buffer.py))
- [main.py](main.py): command line to train and test it

The agent's reward was the charging cost plus penalties for cars leaving without enough energy. It
had to learn the balance between saving money and charging enough, which depends on the penalty
weights; MPC removes that trade-off by making the energy a hard rule.

To train and test it:
```
python main.py train      # trains on 70% of the dataset, saves the model in saved_models/
python main.py test       # evaluates on the rest, compares with always-charge, writes CSVs to outputs/
```
Both accept `--sessions PATH` and `--price PATH`; `train` also `--verbose yes/no` and
`--save-model yes/no`; `test` also `--q PATH`, `--qtarget PATH`, `--verbose yes/no` and `--save-prefix PATH`.
