Aquí tienes la traducción al inglés, conservando la estructura Markdown, las tablas y los bloques de código. Al traducir documentación técnica en Markdown, es buena práctica mantener intacto el formato y no alterar el contenido dentro de bloques de código salvo que sea texto descriptivo claramente no ejecutable. [docs.github](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-and-highlighting-code-blocks)

# Detailed Analysis — BlueBird Flexibility Manager


## 1. Overview


The **BlueBird Flexibility Manager** project is a **demand-response** system for controlling local energy assets at European partner sites. Each type of asset is called a **pilot** (EWH cold rooms, Montcada building HVAC, Enlion water works, district heating). The system runs economic **Model Predictive Control (MPC)** at a configurable cadence, writes optimal setpoints to a shared database, and publishes them to the Building Management System (BMS) through a Knowledge Engine.


***


## 2. General Architecture


```
┌─────────────────┐   dynamics JSON     ┌──────────────────────┐
│  Digital Twin   │ ─────────────────► │                      │
│  (R/ctsmTMB)    │   shared volume     │  Flexibility        │
└─────────────────┘                     │  Controller         │
                                        │  (Julia/FlexOPTi)   │
┌─────────────────┐   DB forecasts      │                      │
│  Disturbances   │ ─────────────────► │  · reads dynamics    │
│  Forecast       │                     │  · reads forecasts   │
└─────────────────┘                     │  · gets prices       │
                                        │  · writes setpoints  │
┌─────────────────┐   HTTP prices       │                      │
│  Trading        │ ─────────────────► │                      │
│  Manager (ext.) │                     └──────────────────────┘
└─────────────────┘                                  │
                                                     │ setpoints
┌─────────────────┐   shared DB          ┌────────▼─────────────┐
│  Control Room   │ ◄────────────────► │  Database (SQLite)   │
│  (dashboard)    │                     └──────────────────────┘
└─────────────────┘
```


### 2.1 Docker Services


| Service | Language | Port | Profile | Role |
|----------|----------|--------|--------|-----|
| `dummy-forecast` | Python 3.12 | 5000 | _(always)_ | Initializes DB schema; writes synthetic forecasts every 15 min |
| `fc` | Julia 1.12 (FlexOPTi) | 8080 | _(always)_ | MPC loop — reads dynamics + forecasts + prices, writes setpoints |
| `control-room` | Python 3.12 (Flask) | 9000 | _(always)_ | Operator dashboard — monitors the system and KPIs |
| `dt-ewh` | R 4.4.2 (ctsmTMB) | — | `retrain` | Digital Twin parameter estimation (single run) |
| `fc-sim` | Julia 1.12 | — | `sim` | Closed-loop MPC simulation (demo) |
| Trading Manager | External | 9090 | — | Day-ahead electricity prices (ENTSOE) |


### 2.2 Networks and Volumes


- **Docker networks:**
  - `flexibility-manager_default` — internal network between services
  - `local_tm-net` — external network for communication with the Trading Manager
- **Shared volumes:**
  - `db-data` → `/data/bb.db` (shared SQLite database)
  - `dt-volume` → JSON model exchange between Digital Twin and FC


### 2.3 Data Flow


1. **`dummy-forecast` → DB:** Writes disturbance forecasts (ambient temperature, door loads) every 15 minutes.
2. **`fc` ← DB:** Reads measurements (initial conditions) and disturbance forecasts.
3. **`fc` → DB:** Writes optimal cooling setpoints.
4. **`dt-ewh` → `dt-volume`:** Writes `cold_room_model_continuous.json` with estimated parameters.
5. **`fc` ← `dt-volume`:** Reads the dynamic model from the Digital Twin at each iteration.
6. **`fc` → Trading Manager:** Gets day-ahead electricity prices via HTTP.
7. **`control-room` ← DB / fc:** Reads system state for visualization.


***


## 3. System Configuration


### 3.1 Environment Variables (`.env`)


| Variable | Default value | Description |
|----------|-------------------|-------------|
| `DB_PATH` | `/data/bb.db` | Path to the SQLite database |
| `PILOT` | `Ewh` | Active pilot (`Ewh`, `Montcada`) |
| `SOLVER` | `HiGHS` | MILP optimization solver |
| `HU` | `48` | Control horizon (steps) |
| `DELTA_T` | `900.0` | Time step (seconds) → 15 min |
| `RETRAIN_EVERY_H` | `168` | Weekly DT retraining (hours) |
| `MIP_GAP` | `0.01` | Solver gap tolerance (1%) |
| `HTTP_PORT` | `8080` | FC status server port |
| `TM_BASE_URL` | `http://tm-service:9090` | Trading Manager URL |
| `MARKET_COUNTRY` | `Germany` | Country for price query |
| `CADENCE_S` | `900` | Forecast writing interval (s) |
| `HORIZON_STEPS` | `48` | Number of steps per forecast |
| `T_AMBIENT` | `18.0` | Constant ambient temperature (°C) |
| `N_ROOMS` | `5` | Number of cold rooms |


### 3.2 FC Settings (`FlexOPTi/settings.json`)


```json
{
  "pilot":           "Ewh",
  "solver":          "HiGHS",
  "Hu":              48,
  "delta_t":         900.0,
  "retrain_every_h": 168,
  "market_country":  "Germany",
  "tm_base_url":     "http://tm-service:9090",
  "http_port":       8080,
  "mip_gap":         0.01,
  "milp_horizon":    1
}
```


- **`Hu=48`**: 48 steps × 900s = **12 hours** optimization horizon.
- **`milp_horizon=1`**: Only the first step is binary (the rest is relaxed to ). [deepl](https://www.deepl.com/en/translator/q/en/markdown/es/rebaja/71e26c33)
- **`mip_gap=0.01`**: The solver stops when it reaches a 1% optimality gap.


***


## 4. Database (`db/init.sql`)


### 4.1 Main Tables


#### `measurements` — BMS sensor measurements


| Column | Type | Description |
|---------|------|-------------|
| `id` | INTEGER PK | Identifier |
| `timestamp` | TEXT | Timestamp |
| `room_id` | INTEGER | Cold room ID (1-5) |
| `T_air` | REAL | Air temperature [°C] |
| `T_product` | REAL | Product temperature [°C] |


#### `disturbance_forecasts` — Disturbance forecasts


| Column | Type | Description |
|---------|------|-------------|
| `id` | INTEGER PK | Identifier |
| `timestamp` | TEXT | Timestamp |
| `room_id` | INTEGER | Cold room ID (1-5) |
| `T_ambient` | REAL | Ambient temperature [°C] |
| `door_load` | REAL | Door thermal load [W] |


#### `setpoints` — Controller setpoints


| Column | Type | Description |
|---------|------|-------------|
| `id` | INTEGER PK | Identifier |
| `timestamp` | TEXT | Timestamp |
| `room_id` | INTEGER | Cold room ID (1-5) |
| `u` | REAL | Cooling power [W] |
| `p3` | REAL | Freezer power [W] |


#### Additional market tables


| Table | Purpose |
|-------|-----------|
| `assets` | Energy assets (with location and type) |
| `price_tag` | Price tags by country and market type |
| `price` | Electricity prices [EUR/kWh] by timestamp |
| `weather` | Weather variables by asset |
| `forecasts` | Generic forecasts with confidence intervals (`yhat`, `yhat_lower`, `yhat_upper`) |


### 4.2 Initial Data (Seed)


Five cold rooms are preloaded:


| Room | T_air (°C) | T_product (°C) | Use |
|--------|------------|-----------------|-----|
| 1 | 4.5 | 4.8 | Dairy |
| 2 | 2.1 | 2.3 | Finished products |
| 3 | 3.8 | 4.0 | Meat |
| 4 | 5.2 | 5.5 | Vegetables |
| 5 | -18.3 | -18.0 | Freezer |


***


## 5. FlexOPTi — Flexibility Controller (Julia)


### 5.1 Module Structure


```
FlexOPTi/
├── Project.toml              # Julia dependencies
├── Manifest.toml
├── settings.json             # Operational configuration
├── Dockerfile
├── src/
│   ├── FlexOPTi.jl           # Main module
│   ├── logging.jl            # Log configuration
│   ├── print/
│   │   └── print_output.jl   # Output formatting
│   ├── core/
│   │   ├── types.jl           # Type definitions
│   │   ├── parse_digital_twin.jl  # DT JSON parser
│   │   ├── parse_sensors.jl       # Initial conditions
│   │   ├── parse_forecasts.jl     # Disturbance forecasts
│   │   ├── fetch_prices.jl        # Trading Manager prices
│   │   ├── build_constraints.jl   # MPC constraints
│   │   ├── mpc_update.jl          # MPC optimization
│   │   ├── mpc_helper.jl          # Helper functions
│   │   ├── fm_optimize.jl         # Main API: optimize()
│   │   ├── utils.jl               # Utilities
│   │   └── parse_output.jl        # Result parsing
│   └── pilots/
│       ├── ewh/                   # EWH pilot implementation
│       │   ├── Ewh.jl
│       │   ├── parse_digital_twin.jl
│       │   ├── parse_forecasts.jl
│       │   ├── constraints.jl
│       │   ├── batch_mpc.jl
│       │   ├── mpc_types.jl
│       │   └── mpc_main.jl
│       └── montcada/              # Montcada pilot implementation
│           └── Montcada.jl
├── scripts/
│   └── ewh/
│       ├── dr_controller.jl       # Main MPC loop (production)
│       ├── sim_demo.jl            # Closed-loop simulation
│       ├── sim_plot.jl            # Simulation plots
│       ├── mpc_base.jl            # Standalone MPC demo
│       ├── dt.jl                  # DT utilities
│       ├── ewh.jl                 # EWH utilities
│       └── mpc_plot.jl            # Plot utilities
├── data/
│   ├── EWH/inputs/                # Input data
│   └── cache/                     # Price cache
└── test/                          # Tests
```


### 5.2 Main Dependencies


| Package | Use |
|---------|-----|
| `JuMP` | Mathematical optimization modeling |
| `HiGHS` | MILP solver (Mixed-Integer Linear Programming) |
| `HTTP` | HTTP client (queries to the Trading Manager) |
| `JSON3` | JSON parsing |
| `SQLite` | Database access |
| `TimeZones` | Time zone handling |
| `JLD2` | Binary serialization of JuMP data |
| `LoggingExtras` | Advanced logging |


### 5.3 Type System (Multiple Dispatch)


```julia
abstract type AbstractBuilding end
abstract type AbstractHeatBuilding <: AbstractBuilding end
abstract type AbstractElecBuilding <: AbstractBuilding end


struct Montcada <: AbstractElecBuilding end
struct Ewh      <: AbstractElecBuilding end
struct Karno    <: AbstractHeatBuilding end
```


The pilot is passed as a string in the API (`"Ewh"`, `"Montcada"`) and resolved once into a Julia type. From that point on, Julia’s **multiple dispatch** automatically handles the pilot-specific logic without using string comparisons.


### 5.4 Key Data Structures


#### `O` — Configuration options (mutable)


| Field | Type | Description |
|-------|------|-------------|
| `Hu` | Int | Control horizon [steps] |
| `Δt` | Float64 | Time step [seconds] |
| `pilot` | AbstractBuilding | Resolved pilot type |
| `solver` | String | `"HiGHS"` or `"Gurobi"` |
| `mip_gap` | Float64 | Relative MIP gap |
| `warm_start` | Bool | Reuse previous solution |
| `milp_horizon` | Int | Steps with binary variables |
| `market_country` | String | Country for prices |
| `tm_base_url` | String | Trading Manager URL |


#### `OX` — MPC input data


| Field | Type | Description |
|-------|------|-------------|
| `digital_twin` | Dict | Model structure + dynamics |
| `sensors` | Vector{Dict} | Initial conditions (measurements) |
| `forecast` | Dict | Disturbance predictions |
| `constraints` | Dict | System constraints |
| `dynamics` | BatchDynamics | Discretized state-space |
| `prices` | Vector{Float64} | Day-ahead prices [EUR/Wh] |


#### `BatchDynamics` — Batch MPC formulation


```
X = M·ξ₁ + Ξ·U + Ψ·Δ
```


| Field | Description |
|-------|-------------|
| `M` | State mapping matrix |
| `Ξ` | Control mapping matrix |
| `Ψ` | Disturbance mapping matrix |
| `ξ₁` | Initial condition vector |
| `Δ` | Stacked disturbance vector |


### 5.5 Main Function: `optimize()`


```julia
function optimize(
    digital_twin_file::String,
    sensors_file::String,
    forecasts_file::String;
    pilot::String,
    Hu::Int = 1,
    solver::String = "HiGHS",
    ...
)::Dict{Symbol, Any}
```


**Execution flow:**


1. Loads configuration from `settings.json`
2. Overrides values with the received arguments
3. Parses the Digital Twin JSON (model structure + identified dynamics)
4. Parses sensor JSON (current measurements → initial conditions `x₀`)
5. Parses forecast JSON (ambient temperature, door openings for `Hu` steps)
6. Gets day-ahead prices from the Trading Manager (or uses cache)
7. Builds batch dynamics by discretization (Zero-Order Hold at `Δt`)
8. Dispatches to pilot-specific `mpc_update()` via multiple dispatch
9. Returns a dictionary with: objective value [EUR], optimal setpoints `u[1..Hu]`, predicted states `x[1..Hu]`, solver status, computational metadata


### 5.6 Price Retrieval (`fetch_prices.jl`)


The system queries electricity prices from the Trading Manager with a cascading fallback mechanism:


| Quality level | Source | Description |
|------------------|--------|-------------|
| `:live` | Trading Manager | Fresh prices obtained in real time |
| `:cached` | Local file | TM unreachable; on-disk cache is used |
| `:fallback` | Previous cache | Previous day’s prices are used |
| `:unavailable` | Zeros | No data; optimization is unreliable |


**Key functions:**


- `_get_market_id(tm_base, country)` — Finds market ID by country
- `_query_tm_prices(tm_base, market_id)` — Queries prices via REST
- `_snap_to_slot(dt)` — Quantizes to a 15-minute slot
- `_align_prices(prices_dict, t0, Hu, Δt)` — Builds the horizon price vector
- Local cache in `FlexOPTi/data/cache/prices_<country>.json` (maximum 36 hours)


### 5.7 EWH Pilot — Cold Rooms


**Description of the physical system:**


- **5 rooms:** dairy, finished products, meat, vegetables, freezer
- **2 compressors:** P1 (modulated, rooms 1-4), P2 (on/off, support)
- **1 freezer compressor:** P3 (on/off, room 5)
- **State vector:** 10 states (T_air, T_product per room)
- **Control vector:** `u ∈ [0, P1_max]` for rooms 1-4; `u [codecademy](https://www.codecademy.com/resources/docs/markdown/code-blocks) ∈ {0, P3_max}` for the freezer
- **Constraints:** setpoint limits (±ΔT from nominal), power limits, ramp rates


### 5.8 Main Production Loop (`dr_controller.jl`)


```
START
  │
  ├── Start HTTP status server (:8080)
  ├── Connect to SQLite DB
  ├── Verify existence of the DT file
  │
  └── MPC LOOP (every Δt_s seconds):
        │
        ├── 1. Read sensors from DB → x₀
        ├── 2. Read forecasts from DB → disturbances
        ├── 3. Get prices from the Trading Manager (or cache)
        ├── 4. Call FlexOPTi.optimize(dt_file, sensors, forecasts, ...)
        ├── 5. Extract u_first [1..N_ROOMS] from the result
        ├── 6. Write setpoints to DB
        ├── 7. Check DT retraining timer
        │      └── If due: launch Rscript in subprocess
        └── 8. Update HTTP server status
```


**DB functions:**


| Function | Description |
|---------|-------------|
| `db_read_sensors(db, tmp_dir)` | SELECT latest timestamp → interleaved `x₀` vector `[T_air_1, T_prod_1, ...]` |
| `db_read_forecasts(db, tmp_dir)` | SELECT `timestamp >= NOW()` → next `Hu` steps of T_ambient and door openings |
| `db_write_setpoints(db, u, t)` | INSERT setpoints by room; rooms 1-4 use `u`, room 5 uses `p3` |
| `dt_trigger_retrain()` | Launches `Rscript` in background; copies output to the shared volume |


***


## 6. Digital Twin (`dt/`)


### 6.1 EWH_coldrooms_CSTM — Parameter Estimation (R/ctsmTMB)


**Technology:** R 4.4.2 with the `ctsmTMB` library (Continuous-Time Stochastic Modelling using TMB).


**Physical model (per room):**


$$\frac{dT_{a,i}}{dt} = k_{1,i}(T_{amb} - T_{a,i}) + k_{2,i}(T_{p,i} - T_{a,i}) - k_3 \cdot u_i + k_{4,i} \cdot d_i \cdot 1000 + \sigma_a \cdot w_i$$


$$\frac{dT_{p,i}}{dt} = k_{5,i}(T_{a,i} - T_{p,i}) + \sigma_p \cdot w_{p,i}$$


Where:


| Parameter | Meaning |
|-----------|-------------|
| \(k_{1,i} = UA_i / C_{a,i}\) | Wall heat loss rate |
| \(k_{2,i} = UA_{ap,i} / C_{a,i}\) | Air-product coupling rate |
| \(k_3 = COP / C_a\) | Cooling efficiency |
| \(k_{4,i} = Q_{door} / (1000 \cdot C_a)\) | Door load effect |
| \(k_{5,i} = UA_{ap,i} / C_{p,i}\) | Product heat recovery |
| \(\sigma_a, \sigma_p\) | Process noise (stochastic) |
| \(w_i, w_{p,i}\) | Wiener processes (Brownian motion) |


**Training data:** `simulation_data_8day_noise_real_door_opening.csv` (8 days of simulation with noise and real door openings).


**Process:**


1. **Data preparation:** Map observations (temperatures), inputs (cooling powers), and disturbances (door openings).
2. **Train/test split:** First 164 hours for training, the rest for test.
3. **ctsmTMB model definition:** Stochastic differential equations for the 5 rooms.
4. **Estimation:** Maximum likelihood estimation of parameters.
5. **Output:** JSON with continuous and discretized state-space matrices.


**Output JSON (`cold_room_model_continuous.json`):**


```json
{
  "state_space": {
    "A": [...],       // Continuous A matrix
    "B": [...],       // B matrix (control input)
    "E": [...],       // E matrix (disturbances)
    "x0": [...]       // Initial conditions
  },
  "discretization": {
    "delta_t": 900.0, // Sampling time [s]
    "Ad": [...],      // Discretized A
    "Bd": [...],      // Discretized B
    "Ed": [...]       // Discretized E
  },
  "parameters": {
    "k1": [...],      // Heat loss rates [W/K]
    "k2": [...],      // Air-product coupling rates
    "k3": 2.5,        // COP
    "k4": [...],      // Door load rates
    "k5": [...]       // Heat recovery rates
  }
}
```


### 6.2 EWH_coldrooms — Python Simulation


Simulator of the physical system of the 5 cold rooms used to generate training data for the Digital Twin.


**Discrete state-space model:**


$$x[k+1] = A_d \cdot x[k] + B_d \cdot u[k] + E_d \cdot d[k]$$


Where:
- \(x = [T_1, T_2, T_3, T_4, T_5]\) — room temperatures
- \(u = [u_1, u_2, u_3, u_4, u_5]\) — cooling powers
- \(d = [T_{amb,1}, Q_{door,1}, T_{amb,2}, Q_{door,2}, ...]\) — disturbances


**Exact discretization** via matrix exponential:


$$M_d = e^{M \cdot \Delta t}, \quad M = \begin{bmatrix} A & B & E \\ 0 & 0 & 0 \\ 0 & 0 & 0 \end{bmatrix}$$


**Physical parameters:**


| Parameter | Value | Description |
|-----------|-------|-------------|
| `m` |  kg | Thermal masses |
| `cv` |  J/kg·K | Specific heats |
| `UA` |  W/K | Wall conductances |
| `COP` | 2.5 | Coefficient of performance |
| `P1_max` | 2000 W | Primary compressor |
| `P2_max` | 500 W | Secondary compressor |
| `P3_max` | 2000 W | Freezer compressor |


**Simulated control logic:**


- **Rooms 1-4:** Proportional control with power allocation. If demand exceeds `P1_max`, P2 is activated and allocated proportionally.
- **Freezer:** Hysteresis control with a ±0.5°C band.
- **Disturbances:** Door openings modeled as Poisson arrivals.


### 6.3 EWH_EV_charging_BC7 — Electric Vehicle Charging (RL)


**Deep Q-Network (DQN)** agent for optimal scheduling of electric vehicle charging.


| Component | Detail |
|------------|---------|
| **Technology** | PyTorch 2.7.0 |
| **Agent** | `DQNAgent` |
| **Environment** | `EVChargingEnv` |
| **Reward** | \(-\text{PENALTY\_PER\_KWH} \times E_{charged} - \text{FIXED\_PENALTY}\) if infeasible |
| **Action** | Charging rate \([0, P_{max}]\) with \(P_{max} = 11\) kW |
| **Episodes** | 50 |
| **Data split** | 70% train, 30% test (by station, chronological) |


### 6.4 BlueBird-multizone-modelling — Montcada Building


Thermal dynamics models for the Montcada pilot (multi-zone HVAC building):


- `DT_dynamics_estimator.R` — Dynamics estimator
- `DT_training.R` — Model training
- `DT_predict.R` — Prediction
- `HourlyCalendarEnergyModel.R` — Calendar-based energy model
- `HourlyHVACEnergyModel.R` — HVAC energy model
- `HourlyTemperatureModel.R` — Temperature model


### 6.5 Enlion — Water Works


**Linear regression with lag-24** model for water storage tank dynamics:


$$\text{tank\_level}(t) = \sum_i w_i \cdot \text{feature}(t - \text{lag}_i) + \text{intercept}$$


**Process:**
1. Synthetic data generation (`waterworks-simulated-data-generator.py`)
2. Model training (`prediction_linear_regression.py`)
3. Export of weights to CSV (`linear_reg_lag24_weights.csv`)


***


## 7. Forecasting System (`forecasting/`)


### 7.1 Dummy Forecast Service


Synthetic forecast service for development and testing.


**Behavior:**


- Initializes the DB schema at startup (CREATE TABLE if it does not exist + seed data).
- Every 15 minutes (configurable via `CADENCE_S`), writes 48 forecast steps (12h) to the `disturbance_forecasts` table.
- All forecasts use constant ambient temperature (18°C) and door load 0.
- Exposes `/health` endpoint on port 5000.


### 7.2 Forecasting API (NeuralForecast / Chronos)


REST API based on **FastAPI** for time series model inference.


| Component | Technology |
|------------|------------|
| Framework | FastAPI |
| Models | NeuralForecast 2.0.1, Amazon Chronos |
| ML Framework | PyTorch 2.2.0 (CPU) |
| Tracking | CodeCarbon (CO₂ emissions) |


**Main endpoint:**


```
POST /predict/
Body: { ds: [timestamps], unique_id: [ids], y: [values] }
Response: { yhat: [...], yhat_lower: [...], yhat_upper: [...], ds: [...], unique_id: [...] }
```


### 7.3 Streamlit Application


Web dashboard for data visualization and inference execution:


- **"View Data" page:** Historical data visualization
- **"Inference" page:** Real-time prediction execution


### 7.4 Prefect Pipelines


Scheduled data pipelines (Prefect 3.4.15) for:


- Periodic model training
- Scheduled inference execution
- Synthetic data generation


### 7.5 FAST Framework


Modular forecasting framework organized into 4 phases:


| Phase | Name | Purpose |
|------|--------|-----------|
| 1 | **Forge** | Data preparation |
| 2 | **Augur** | Model training |
| 3 | **Surveil** | Monitoring and anomaly detection |
| 4 | **Translate** | Output transformation |


***


## 8. Control Room — Operator Dashboard


### 8.1 REST Endpoints


| Method | Route | Description |
|--------|------|-------------|
| GET | `/` | HTML dashboard with auto-refresh every 15s |
| GET | `/health` | Service health check |
| GET | `/status` | FC status + latest setpoints |
| GET | `/services` | Health ping of all services |
| GET | `/metrics` | KPIs (setpoint, forecast, measurement counters) |
| GET | `/setpoints` | Latest N setpoints from DB (JSON) |
| GET | `/forecasts` | Latest N forecasts from DB |
| POST | `/clear` | Clear DB tables (with confirmation) |


### 8.2 HTML Dashboard


The dashboard displays in real time:


1. **Service status:** Health traffic light for `fc`, `dummy-forecast`, `control-room`
2. **FC status:** Optimization status (OPTIMAL/INFEASIBLE), cost [EUR], current setpoints `u[1..5]`
3. **Recent setpoints:** Table with the last 5 written setpoints
4. **KPIs:** Total setpoints, forecasts, and measurements in the DB
5. **DB management:** Buttons to clear individual tables or the entire DB


***


## 9. API Summary — Endpoints by Service


| Service | Port | Endpoints |
|----------|--------|-----------|
| `dummy-forecast` | 5000 | `GET /health` |
| `fc` (FlexOPTi) | 8080 | `GET /health`, `GET /status` |
| `control-room` | 9000 | `GET /`, `/health`, `/status`, `/services`, `/metrics`, `/setpoints`, `/forecasts`, `POST /clear` |
| Trading Manager (ext.) | 9090 | `GET /api/market`, `GET /api/market/{id}/offerinfo`, `GET /api/offer/{id}` |


***


## 10. Deployment


### Docker Commands


```bash
# Build and start main services
docker compose -f docker-compose.ewh.yml up --build


# Start in background
docker compose -f docker-compose.ewh.yml up --build -d


# Digital Twin retraining (single run)
docker compose -f docker-compose.ewh.yml --profile retrain run --rm dt-ewh


# Closed-loop MPC simulation (demo)
docker compose -f docker-compose.ewh.yml stop fc
docker compose -f docker-compose.ewh.yml --profile sim run --rm fc-sim


# View logs
docker compose -f docker-compose.ewh.yml logs -f fc
docker compose -f docker-compose.ewh.yml logs -f control-room


# Stop everything
docker compose -f docker-compose.ewh.yml down
```


### Prerequisites


- Docker + Docker Compose v2
- Copy `.env.example` to `.env`
- Run DT retraining before the first FC startup (to generate `cold_room_model_continuous.json` in the shared volume)


***


## 11. Technology Summary


| Component | Technology | Version |
|------------|------------|---------|
| MPC Controller | Julia + JuMP + HiGHS | Julia 1.12 |
| Digital Twin (EWH) | R + ctsmTMB | R 4.4.2 |
| Digital Twin (EV) | Python + PyTorch (DQN) | PyTorch 2.7.0 |
| Physical simulator | Python + NumPy + SciPy | Python 3.12 |
| Dashboard | Python + Flask | Flask 3.1.1 |
| Forecasting API | Python + FastAPI + NeuralForecast | NeuralForecast 2.0.1 |
| Forecasting UI | Python + Streamlit | — |
| Pipelines | Python + Prefect | Prefect 3.4.15 |
| Database | SQLite | — |
| Containers | Docker + Docker Compose | v2 |