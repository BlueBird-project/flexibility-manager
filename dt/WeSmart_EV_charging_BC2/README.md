# EWH - BC7 - EV Charging Scheduling with Reinforcement Learning
This repository implements a **Reinforcement Learning** system for smart charging Electric Vehicles (EVs), using
real-world session data provided by the Use-Case and a real price dataset.

The project includes:

- A custom, **component-based EV charging environment** (EV load, optional PV production)
- A **Deep Q-Network (DQN)** agent
- A command line interface for training and testing the agent on different parts of dataset

## Environment
The custom environment models **multiple charging stations** and **electric vehicle sessions**.
The default dataset ([wesmart_ev_sessions.csv](src/env/dataset/wesmart_ev_sessions.csv)) covers **2 stations**
over 2025; the older BC7 dataset ([ewh_cleaned_rtu.csv](src/env/dataset/ewh_cleaned_rtu.csv)) is also included and
can be selected with `--sessions`. The historical price dataset
[price_two_years_15min.xlsx](src/env/dataset/price_two_years_15min.xlsx) used in this version is obtained from
[ELEXYS](https://www.elexys.be/insights/spot-belpex), showing a price for each 15-minute interval.

The EV session dataset contains the following columns:
- **arrival**: timestamp arrival of the EV
- **departure**: timestamp departure of the EV
- **station_id**: the ID of the station where the EV is located
- **req_kwh**: the amount of energy required by the EV

### Charging power
The simulator charges at a constant **9 kW** per station. This is derived from the raw charger logs
(`ev_charger_EVSE0*_with_connected.csv`): the per-session median is 8.86 kW and 9.27 kW for the two stations,
with a mode of 9 kW. The same figure is used by the session feasibility filter, so the environment never keeps a
session it cannot physically serve. Override both with `--power`.

### What the agent controls
At each 15-minute interval, the agent outputs an action for each charging station, which is either to charge (1)
or not (0) at the next timestep. With the default dataset the agent therefore takes 2 actions per timestep.

## Goal
The goal of the agent is to:
- **minimize electricity cost**, by charging the EVs at low prices
- **respect EV energy requirements**, by providing the EVs a good amount of energy

## Used Framework: DQN
We use a DQN agent ([DQN scientific paper](https://doi.org/10.48550/arXiv.1312.5602))
([DQN easy explanation](https://medium.com/data-science/reinforcement-learning-explained-visually-part-5-deep-q-networks-step-by-step-5a5317197f4b)).
The agent uses a neural network ([NN.py](src/agent/helpers/NN/NN.py)) to output the best action for each timestep.
A replay buffer ([replay_buffer.py](src/agent/helpers/replay_buffer/replay_buffer.py)) stores past experiences.
The agent does exploration and exploitation, with a decaying epsilon value.

The network emits `2 x n_stations` values reshaped to `(n_stations, 2)`, so each station gets an independent
binary decision instead of the agent enumerating a `2^n_stations` joint action space. Q(s,a) is the sum of the
per-station values and the target is the sum of per-station maxima — a value-decomposition (VDN-style)
factorisation of DQN.

During each timestep:
1. The agent observes the environment state
2. Chooses an action vector [0/1] per station
3. Receives cost-based reward
4. Stores the transition in the replay buffer
5. The stored transitions are used to train the neural network, able to make a better decision

### State representation
At every decision step, the environment returns a flattened numeric vector containing:
1. Price signal: `--price-horizon` (M, default 12) **forward-looking** price values spaced
   `--price-step-minutes` apart (default 30), i.e. about 6 h ahead. The first value is the exact price of the
   interval about to be decided; each later one is the average of the quarter-hours in its 30-min block.
   Belgian day-ahead prices are published a day ahead, so this is a known schedule the live service can
   supply exactly, not a forecast with error. With the default `--price-encoding window` these are
   **min-max scaled within the window** (0 = cheapest of the 12, 1 = most expensive; a flat window is 0.5),
   because when to charge depends on how a price compares with what is coming, and absolute levels shift a
   lot between seasons. Two more values keep the absolute information: the current **price level**
   (price / 99th percentile of training prices, clipped to [-1, 1]) and the window's **spread** (max - min,
   same scale, clipped to [0, 1]), so a nearly flat night is not mistaken for a big opportunity.
2. Time of day encoding: for representing the time, sine and cosine encoding are used
(sin(2π·time) and cos(2π·time))
3. For each station, a block of 4 values: EV present (0/1), remaining kWh, hours to departure,
and urgency (remaining energy / max possible charge time). Remaining kWh and hours are divided by the
training split's maxima and **clipped to [0, 1]**; urgency is clipped at 2 and rescaled to [0, 1]. Without
the clip, a session larger than anything in training pushes the network outside the range it ever saw
(the test split has one at 1.5x), and urgency is otherwise unbounded.
4. If PV is attached, `--pv-horizon` (N, default 4) more values: **forward-looking** net PV kWh available for
   charging, same convention, divided by the historical maximum and clipped to [0, 1]. Unlike prices, this
   genuinely is a forecast at inference time — training reads it straight off the historical production
   series (perfect foresight), so keep the horizon short to limit how much a real forecast's error can
   differ from what the model saw during training.

**Every input is bounded** to [0, 1] or [-1, 1]; `service/test_state_parity.py` asserts it.

The state will look like this (defaults, one PV component attached — 28 values; 24 without PV):
~~~
[
 p_0, p_1, ..., p_11,                          # 12 window-scaled prices, 30 min apart   [0, 1]
 price_level, price_spread,                    #                                  [-1, 1], [0, 1]
 sin_time, cos_time,                           #                                         [-1, 1]
 [present_1, remaining_1, hours_left_1, urgency_1],   #                                   [0, 1]
 [present_2, remaining_2, hours_left_2, urgency_2],
 pv_t, pv_t+1, pv_t+2, pv_t+3                  # only when --pv is used                  [0, 1]
]
~~~

The state returned by `reset()`/`step()` describes the interval **one step ahead** — exactly the interval the
next action applies to. `M` and `N` are set at training time (`--price-horizon`/`--pv-horizon`) and must match
what the live service in [service/](service/) is fed — see [service/README.md](service/README.md).

### Reward Function
The reward is constructed from:
- Charging cost: how much we pay for charging EVs (negative reward)
- Penalty for departing EVs with some missing energy (`--penalty-per-kwh` per kWh + `--fixed-penalty` flat)
- Penalty for being behind schedule, charged **every interval** an EV cannot still be finished in time
  (`--progress-penalty`, default `fixed-penalty / 2`), to help the agent learn faster

This combination encourages the agent to get minimum costs, minimum unmet energy, and respect charging deadlines.
Note the behind-schedule term compounds over a session, so it can dominate the cost term — set
`--progress-penalty 0` to train against the terminal penalty alone.

### Deadline guard
On top of the network's choice, any station that can no longer meet its deadline is forced to charge:

~~~
remaining_kwh > hours_to_departure * power_kw * 0.96   ->   charge = 1
~~~

It is applied during training and evaluation alike (so the network learns with it), saved in `metadata.json`,
and applied identically by the live service. It exists because the RL policy alone occasionally strands an EV
on rare, unusually large sessions that neither the training nor the validation data contain an example of;
in seed sweeps 2 of 5 EV+PV runs left 38–70 kWh undelivered. With the guard, 45 runs across all three setups
never exceeded 9.6 kWh undelivered, at equal or better cost. Disable with `--no-deadline-guard` (not advised).

## How to run the project
Follow this guide to run the project.
### Install Python
First, python is required. Download python from the **OFFICIAL WEBSITE**.

### Create and activate a virtual environment
The virtual environment is used to install the required packages. For creating it, run from command line:

```python -m venv .venv```

### Activate the virtual environment
To activate the virtual environment, run from command line (WINDOWS):

```.venv\Scripts\activate```

or from macOS/Linux:

```source .venv/bin/activate```

### Install dependencies
To install the required packages and dependencies, run:

```pip install -r requirements.txt```

`openpyxl` is included because the default price dataset is an `.xlsx` file.

### Train a new model
For training the model, by using the default datasets saved in the folder, run:

```python main.py train```

This command will:
- Load the dataset and drop sessions that cannot be served at 9 kW
- Split it per station, chronologically: 55% train, 15% validation, 30% test
- Train the DQN, scoring the greedy policy on the validation split after every episode and keeping the
  **best-scoring weights** (not simply the last episode's)
- Save the trained model in [saved_models](saved_models), together with a `metadata.json`
- Evaluate on the test split and against the always-charge baseline

The same command works for every setup; add `--pv` and/or `--consumption` for EV+PV and EV+PV+building load.

### Choosing a model to ship
A single training run is one draw from a wide distribution — identical settings can differ by several
percent in cost depending only on `--seed`. To pick a model, train a few seeds, compare the
`Restored best-validation weights from episode N (VAL reward X)` line each prints, and keep the seed with
the **highest VAL reward**. Select on validation, never on the test numbers. Training is deterministic per
seed, so re-running the winner reproduces it exactly. The committed checkpoints were chosen this way from
15 seeds each.

### Test the model
To test the model, by using the models saved in [saved_models](saved_models), run:

```python main.py test```

This command will:
- Load the default models from [saved_models](saved_models)
- Restore the normalisation constants from `metadata.json` so the network sees the same input scale it trained on
- Run evaluation
- Generate per-EV CSVs in the [outputs](outputs) folder
- Generate a comparison wrt the always-charge baseline, saving it in [outputs](outputs)

`python main.py test` now reproduces the evaluation printed at the end of `python main.py train` exactly.

### Options

Shared by both subcommands:

| Flag | Default | Meaning |
|---|---|---|
| `--sessions PATH` | `src/env/dataset/wesmart_ev_sessions.csv` | EV session dataset |
| `--price PATH` | `src/env/dataset/price_two_years_15min.xlsx` | Price dataset (`.xlsx` or `.csv`) |
| `--pv PATH` | none | PV production dataset; adds the PV component |
| `--consumption PATH` | none | Building consumption, netted off PV (requires `--pv`) |
| `--power FLOAT` | `9.0` | Charging power in kW (filter **and** simulator) |
| `--slack FLOAT` | `1.5` | Feasibility slack for the session filter |
| `--penalty-per-kwh FLOAT` | `5.0` | € per kWh unmet at departure |
| `--fixed-penalty FLOAT` | `10.0` | Flat € for an undercharged departure |
| `--progress-penalty FLOAT` | `fixed-penalty / 2` | € per interval while behind schedule; `0` disables |
| `--price-horizon INT` | `12` | Number of forward-looking price values in the state (M) |
| `--price-step-minutes INT` | `30` | Spacing of those values, a multiple of 15 (e.g. `15`, `30`, `60`) |
| `--price-encoding window/raw` | `window` | `window`: min-max within the window + level + spread; `raw`: EUR/kWh as-is (pre-change) |
| `--pv-horizon INT` | `4` | Forward-looking PV forecast steps in the state (N); must match the live service |
| `--seed INT` | `1` | Random seed |
| `--val-size FLOAT` | `0.15` | Validation fraction used to pick the best weights; `0` keeps the last episode. Use the same value for `train` and `test` |
| `--no-deadline-guard` | off | Disable the deadline guard (not advised) |
| `--legacy-features` | off | Disable feature clipping (only to reproduce pre-fix checkpoints) |
| `--verbose yes/no` | `yes` | Detailed printing |

`train` only:

| Flag | Default | Meaning |
|---|---|---|
| `--num_episodes INT` | `50` | Training episodes |
| `--save-model yes/no` | `yes` | Write checkpoints |
| `--val-every INT` | `1` | Evaluate on the validation split every N episodes |

For `test`, the price settings, clipping and the guard are all taken from the checkpoint's `metadata.json`,
not from these flags, so a model is always evaluated the way it was trained — including checkpoints from
before the price change, which still use 4 raw prices.

`test` only:

| Flag | Default | Meaning |
|---|---|---|
| `--q PATH` | `saved_models/<run>/q_state_dict.pth` | Q-network state dict |
| `--qtarget PATH` | `saved_models/<run>/q_target_state_dict.pth` | Target-network state dict |
| `--metadata PATH` | next to `--q` | `metadata.json` with the training normalisation constants |
| `--allow-unsafe-load` | off | Permit legacy pickled-module checkpoints (executes code from the file) |
| `--save-prefix PATH` | `outputs/<run>/test` | Prefix for the evaluation CSVs |

### Output folders
Model and output directories are namespaced by the attached components: `EV`, `EV_PV`, `EV_PV_Cons`. Train and
test with the same `--pv`/`--consumption` combination so the default checkpoint paths line up.

## Live deployment
A trained checkpoint (`saved_models/<run>/`) can be served as a long-running HTTP service, dockerized, for a
real charger controller to call once per 15-minute interval. See [service/README.md](service/README.md) for
the request/response format and how to build and run the container.
