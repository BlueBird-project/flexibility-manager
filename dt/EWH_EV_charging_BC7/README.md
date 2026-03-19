# EWH - BC7 - EV Charging Scheduling with Reinforcement Learning 
This repository implements a **Reinforcement Learning** system for smart charging Electric Vehicles (EVs), using 
real-world session data provided by the Use-Case and a real price dataset.

The project includes:

- A custom **EV charging environment**
- A **Deep Q-Network (DQN)** agent
- A command line interface for training and testing the agent on different parts of dataset

## Environment
The custom environment models **multiple charging stations**, with four stations, and **electric vehicle sessions**. 
The model is trained and tested based on historical data sessions from BC7 
([ewh_cleaned_rtu.csv](src/env/dataset/ewh_cleaned_rtu.csv)), after being cleaned and preprocessed, while the 
historical price dataset [price_two_years_15min.xlsx](src/env/dataset/price_two_years_15min.xlsx)] used in this version 
is obtained by [ELEXYS](https://www.elexys.be/insights/spot-belpex), showing a price for each 15-minute interval.

The EV session dataset contains the following columns:
- **arrival**: timestamp arrival of the EV
- **departure**: timestamp departure of the EV
- **station_id**: the ID of the station where the EV is located
- **req_kwh**: the amount of energy required by the EV

### What the agent controls
At each 15-minute interval, the agent outputs an action for each charging station, which is either to charge (1) 
or not (0) at the next timestep. Since the business case is using 4 stations, the agent will take 4 actions at each timestep.

## Goal
The goal of the agent is to:
- **minimize electricity cost**, by charging the EVs at low prices
- **respect EV energy requirements**, by providing the EVs a good amount of energy

## Used Framework: DQN
We use a standard DQN agent ([DQN scientific paper](https://doi.org/10.48550/arXiv.1312.5602))
([DQN easy explanation](https://medium.com/data-science/reinforcement-learning-explained-visually-part-5-deep-q-networks-step-by-step-5a5317197f4b)).
The agent will use a neural network ([NN.py](src/agent/helpers/NN/NN.py)) to output the best action for each timestep.
A replay buffer ([replay_buffer.py](src/agent/helpers/replay_buffer/replay_buffer.py)) is used for storing past experiences.
The agent will do exploration and exploitation, with a decaying epsilon value.

During each timestep:
1. The agent observes the environment state
2. Chooses an action vector [0/1] per station
3. Receives cost-based reward
4. Stores the transition in the replay buffer
5. The stored transitions are used to train the neural network, able to make a better decision

### State representation
At every decision step, the environment returns a flattened numeric vector containing:
1. Price signal: price at next timestep, and a window of N previous prices (e.g. 6)
2. Time of day encoding: for representing the time, sine and cosine encoding are used 
(sin(2π·time) and cos(2π·time))
3. For each station, a block of 4 values: EV present (0/1), remaining kWh, hours to departure, 
and urgency (remaining energy / max possible charge time)

The state will look like this:
~~~
[
 next_price,
 prev_price_1, prev_price_2, ..., prev_price_k,
 sin_time, cos_time,
 [present_1, remaining_1, hours_left_1, urgency_1],
 [present_2, remaining_2, hours_left_2, urgency_2],
 ...
]
~~~

### Reward Function
The reward is constructed from:
- Charging cost: how much we pay for charging EVs (negative reward)
- Penalty for departing EVs with some missing energy 
- Penalty for impossibility of fully charging an EV in a specific timestep, to help the agent learn faster

This combination will encourage the agent to get minimum costs, minimum unmet energy, and respect charging deadlines.


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

### Train a new model
For training the model, by using the default datasets saved in the folder, run:

```python main.py train```

This command will:
- Load the dataset 
- Take the 70% of the dataset for training the DQN 
- Save the trained model in [saved_models](saved_models).

You can also specify these optional arguments:
1. The EV session dataset path:
~~~ 
--sessions PATH
(default = src/env/dataset/ewh_cleaned_rtu.csv)
~~~
2. The price dataset path:
~~~
--prices PATH
(default = src/env/dataset/price_two_years_15min.xlsx)
~~~
3. Enable/ disable detailed printing:
~~~
--verbose: yes/no (default = yes)
~~~
4. Save the model:
~~~
--save_model: yes/no (default = yes)
~~~
   

### Test the model
To test the model, by using the models saved in [saved_models](saved_models), run:

```python main.py test```

This command will:
- Load the default models from [saved_models](saved_models)
- Run evaluation
- Generate per-EV CSVs in [outputs](outputs) folder
- Generate a comparison wrt always-charge baseline, saving it in [outputs](outputs)

You can also specify these optional arguments:
1. The EV session dataset path:
~~~ 
--sessions PATH 
(default = src/env/dataset/ewh_cleaned_rtu.csv)
~~~
2. The price dataset path:
~~~
--prices PATH 
(default = src/env/dataset/price_two_years_15min.xlsx)
~~~
3. Q-network path for the DQN:
~~~
--q PATH
(default = saved_models/q_state_dict.pth)
~~~
4. Q-target-network path for the DQN:
~~~
--qtarget PATH
(default = saved_models/qtarget_state_dict.pth)
~~~
5. Enable/disable printing:
~~~
--verbose: yes/no (default = yes)
~~~
6. Save the evaluation CSVs. If not provided, it won't save. If provided, it will save three files in the folder:
~~~
--save-prefix PATH
~~~

