Aquí tienes la traducción al inglés, manteniendo la estructura Markdown y el contenido técnico sin alterar el significado. Los principios de traducción de Markdown suelen preservar encabezados, listas, tablas y bloques de código mientras se traduce solo el texto natural, que es lo que he aplicado aquí. [doc.openl](https://doc.openl.io/translate/md/spanish)

# Roadmap: Generalization of the Flexibility Manager


## Objective


Transform the current Flexibility Manager —which works only for specific pilots (EWH cold rooms, Montcada HVAC)— into a **generic framework** where any new pilot is defined exclusively through **configuration** (JSON/YAML), without touching Julia, Python, or R code.


The end user should be able to tell the system:
- What **assets** it has (cold rooms, heat pumps, EV chargers, water tanks…).
- How many **zones** it manages and which **state variables** each one has.
- Which **control inputs** exist (compressor powers, valve setpoints…).
- Which **disturbances** affect the system (ambient temperature, door opening, solar irradiance, occupancy…).
- What the **operational limits** are for temperature, power, ramping, etc.
- Which **dynamic model** describes the plant (state-space, ARX, or another).
- How it connects to the **electricity market** and which **cost function** to minimize.


And the system will automatically generate the optimal setpoints.


***


## Diagnosis: What is pilot-coupled today


### Summary of dependencies by layer


| Layer | Files | What is hardcoded |
|------|----------|----------------------|
| **Julia Types** | `core/types.jl` | `Ewh`, `Montcada`, `Karno` as concrete structs. A pilot cannot be added without editing this file. |
| **Constraints** | `pilots/ewh/constraints.jl` | Zone names (`dairy`, `meat`, `freezer`), temperature ranges by zone, power limits per compressor — all literal in the code. |
| **MPC Variables** | `pilots/ewh/mpc_types.jl` | `EwhVars` with specific fields: `δ_fridge`, `δ_freezer`, `p1`, `p2`, `p3`, `PVused`, `PVcurt`. |
| **MPC Formulation** | `pilots/ewh/mpc_main.jl` | Objective formula, hysteresis constraints, Big-M, proportional compressor allocation, power balance — all coupled to the EWH topology. |
| **Batch Dynamics** | `pilots/ewh/batch_mpc.jl` | `batch_Δ()` stacks disturbances in the order `[T_amb, door_1..5]` — assumes 6 disturbances/step. |
| **Parsers** | `pilots/ewh/parse_*.jl` | The DT, sensor, and forecast JSON format assumes the EWH schema. Montcada has completely different parsers (ARX vs state-space). |
| **Output** | `pilots/ewh/parse_output.jl` | Output keys (`CompressorPower`, `FridgePower`, `FreezerOn`) and units (`°C`, `W`) hardcoded. Montcada uses different keys and units (`HVACTotalPower`, `K`, `kW`). |
| **Controller** | `scripts/ewh/dr_controller.jl` | `N_ROOMS=5`, DB schema (`T_air`, `T_product`, `door_load`, `u`, `p3`), `u_first = vcat(oy[:u][1,:], oy[:p3] [doc.openl](https://doc.openl.io/translate/md/spanish))`. |
| **DB** | `db/init.sql` | Columns (`T_air`, `T_product`, `door_load`, `u`, `p3`) specific to cold rooms. |
| **Forecast** | `forecasting/dummy/dummy_forecast.py` | `N_ROOMS=5`, `T_AMBIENT=18`, `door_load=0` hardcoded. |
| **Dashboard** | `control_room/main.py` | Displays EWH columns (`u`, `p3`, `room_id`). |
| **Docker** | `docker-compose.ewh.yml` | Service `dt-ewh` points to a pilot-specific R script. |


### Map of hardcoded constants


```
┌─────────────────────────────────────────────────────────────────┐
│                    HARDCODED VALUES TODAY                      │
├─────────────────────────────────────────────────────────────────┤
│ N_ROOMS = 5                                  (dr_controller.jl) │
│ nx = 10  (5 zones × 2 states)                (mpc_main.jl)      │
│ nu = 5   (4 fridges + 1 freezer)             (mpc_main.jl)      │
│ nd = 6   (1 T_amb + 5 door_loads)            (batch_mpc.jl)     │
│ nfridge = nu - 1 = 4                         (mpc_main.jl)      │
│ air_idx(r) = 2r - 1                          (mpc_main.jl)      │
│ T_dairy ∈ [2, 8] °C                          (constraints.jl)   │
│ T_freezer ∈ [-40, -18] °C                    (constraints.jl)   │
│ p1_max = 3300 kW, p2_max = 2900 kW           (constraints.jl)   │
│ p3_max = 2700 kW                             (constraints.jl)   │
│ hysteresis_band = 1.0 °C                     (mpc_main.jl)      │
│ big_M = 1e5                                  (mpc_main.jl)      │
│ Kgain from DT JSON                           (mpc_main.jl)      │
│ door_open_load = 4500 W                      (parse_forecasts.jl)│
│ DB cols: T_air, T_product, door_load         (init.sql)         │
│ DB cols: u, p3                               (init.sql)         │
│ u_first = vcat(oy[:u][1,:], oy[:p3] [doc.openl](https://doc.openl.io/translate/md/spanish))      (dr_controller.jl) │
└─────────────────────────────────────────────────────────────────┘
```


***


## Proposed Architecture


### Design Principle


> **Everything that is currently pilot-specific code becomes declarative configuration.** The generic MPC kernel reads the configuration, builds the optimization model dynamically, and writes the results with semantic metadata.


### New flow


```
                  ┌──────────────────────────────┐
                  │  pilot_config.json           │
                  │  (zones, states, controls,   │
                  │   disturbances, limits,      │
                  │   topology, cost function)   │
                  └──────────────┬───────────────┘
                                 │
                                 ▼
┌──────────────┐     ┌────────────────────────┐     ┌──────────────┐
│ Digital Twin │────►│  Generic MPC Engine    │◄────│ Trading Mgr  │
│ (any format) │     │                        │     └──────────────┘
└──────────────┘     │  1. Reads pilot_config │     ┌──────────────┐
                     │  2. Parses generic DT  │◄────│ Forecast Svc │
┌──────────────┐     │  3. Reads sensors      │     └──────────────┘
│ Sensors DB   │────►│  4. Reads forecasts    │
└──────────────┘     │  5. Builds JuMP        │
                     │  6. Solves             │
                     │  7. Writes setpoints   │
                     └────────────┬───────────┘
                                  │
                                  ▼
                     ┌────────────────────────┐
                     │  generic setpoints     │
                     │  (timestamp, zone_id,  │
                     │   control_name, value) │
                     └────────────────────────┘
```


***


## Detailed Changes by Component


### 1. Pilot Configuration (`pilot_config.json`)


**This is the most important change.** Create a single JSON/YAML file that completely describes the pilot. The system does not need to know whether it is "EWH" or "Montcada" — it only reads the configuration.


```jsonc
{
  "pilot_id": "ewh_cold_rooms",
  "pilot_name": "EWH Cold Rooms",
  "description": "5 cold storage rooms with shared compressor infrastructure",


  // ─── ZONES ────────────────────────────────────────────────────────
  "zones": [
    {
      "id": "dairy",
      "label": "Dairy Room",
      "states": [
        { "name": "T_air",     "unit": "°C", "initial": 4.5 },
        { "name": "T_product", "unit": "°C", "initial": 4.8 }
      ],
      "constraints": {
        "T_air":     { "min": 2.0, "max": 8.0 },
        "T_product": { "min": 1.0, "max": 10.0 }
      },
      "setpoint": {
        "state": "T_air",
        "min": 2.0,
        "max": 8.0,
        "initial": 4.5
      }
    },
    {
      "id": "freezer",
      "label": "Freezer",
      "states": [
        { "name": "T_air",     "unit": "°C", "initial": -18.3 },
        { "name": "T_product", "unit": "°C", "initial": -18.0 }
      ],
      "constraints": {
        "T_air": { "min": -40.0, "max": -18.0 }
      },
      "setpoint": {
        "state": "T_air",
        "min": -40.0,
        "max": -18.0,
        "initial": -20.0
      }
    }
    // ... more zones
  ],


  // ─── CONTROL INPUTS ───────────────────────────────────────────────
  "controls": [
    {
      "id": "p1",
      "label": "Modulated compressor (cold rooms)",
      "type": "continuous",
      "unit": "W",
      "min": 0.0,
      "max": 3300.0,
      "zones": ["dairy", "finished_products", "meat", "vegetables"]
    },
    {
      "id": "p2",
      "label": "On/off compressor (cold rooms)",
      "type": "binary",
      "unit": "W",
      "on_value": 2900.0,
      "zones": ["dairy", "finished_products", "meat", "vegetables"]
    },
    {
      "id": "p3",
      "label": "Freezer compressor",
      "type": "binary",
      "unit": "W",
      "on_value": 2700.0,
      "zones": ["freezer"]
    }
  ],


  // ─── DISTURBANCES ─────────────────────────────────────────────────
  "disturbances": [
    {
      "id": "T_ambient",
      "label": "Ambient temperature",
      "unit": "°C",
      "scope": "global",
      "fallback_value": 20.0
    },
    {
      "id": "door_load",
      "label": "Door thermal load",
      "unit": "W",
      "scope": "per_zone",
      "fallback_value": 0.0
    }
  ],


  // ─── DYNAMIC MODEL ────────────────────────────────────────────────
  "dynamics": {
    "type": "state_space",
    // "type": "arx",           ← for Montcada
    // "type": "neural",        ← for future ML models
    "discretization": "zoh",
    "dt": 900.0,
    "source": "digital_twin_file",
    "state_vector_layout": [
      // Defines how the state vector is mapped to zones
      { "zone": "dairy",              "state": "T_air",     "index": 0 },
      { "zone": "dairy",              "state": "T_product", "index": 1 },
      { "zone": "finished_products",  "state": "T_air",     "index": 2 },
      { "zone": "finished_products",  "state": "T_product", "index": 3 },
      { "zone": "meat",               "state": "T_air",     "index": 4 },
      { "zone": "meat",               "state": "T_product", "index": 5 },
      { "zone": "vegetables",         "state": "T_air",     "index": 6 },
      { "zone": "vegetables",         "state": "T_product", "index": 7 },
      { "zone": "freezer",            "state": "T_air",     "index": 8 },
      { "zone": "freezer",            "state": "T_product", "index": 9 }
    ],
    "control_vector_layout": [
      { "control": "u_allocated", "zone": "dairy",              "index": 0 },
      { "control": "u_allocated", "zone": "finished_products",  "index": 1 },
      { "control": "u_allocated", "zone": "meat",               "index": 2 },
      { "control": "u_allocated", "zone": "vegetables",         "index": 3 },
      { "control": "p3",          "zone": "freezer",            "index": 4 }
    ],
    "disturbance_vector_layout": [
      { "disturbance": "T_ambient", "scope": "global",  "index": 0 },
      { "disturbance": "door_load", "zone": "dairy",    "index": 1 },
      { "disturbance": "door_load", "zone": "finished_products", "index": 2 },
      { "disturbance": "door_load", "zone": "meat",     "index": 3 },
      { "disturbance": "door_load", "zone": "vegetables","index": 4 },
      { "disturbance": "door_load", "zone": "freezer",  "index": 5 }
    ]
  },


  // ─── POWER TOPOLOGY ───────────────────────────────────────────────
  "power_topology": {
    "has_grid": true,
    "has_pv": false,
    "has_battery": false,
    "grid_buy_limit": 1e6,
    "grid_sell_limit": 1e6,
    "compressor_groups": [
      {
        "id": "fridge_group",
        "type": "shared_modulated_plus_binary",
        "modulated_control": "p1",
        "binary_control": "p2",
        "zones": ["dairy", "finished_products", "meat", "vegetables"],
        "allocation_mode": "proportional",
        "gain": "from_dt"
      },
      {
        "id": "freezer_group",
        "type": "hysteresis_binary",
        "binary_control": "p3",
        "zones": ["freezer"],
        "hysteresis_band": 1.0,
        "big_M": 1e5
      }
    ]
  },


  // ─── OBJECTIVE FUNCTION ───────────────────────────────────────────
  "objective": {
    "type": "minimize_energy_cost",
    "components": [
      { "term": "grid_buy",  "sign": "+", "price_source": "trading_manager" },
      { "term": "grid_sell", "sign": "-", "price_source": "trading_manager" }
    ]
  },


  // ─── MARKET ───────────────────────────────────────────────────────
  "market": {
    "country": "Germany",
    "tm_base_url": "http://tm-service:9090"
  },


  // ─── MPC ──────────────────────────────────────────────────────────
  "mpc": {
    "horizon_steps": 48,
    "dt_seconds": 900.0,
    "solver": "HiGHS",
    "mip_gap": 0.01,
    "milp_horizon": 1,
    "warm_start": false
  },


  // ─── DIGITAL TWIN RETRAINING ──────────────────────────────────────
  "retraining": {
    "enabled": true,
    "cadence_hours": 168,
    "script": "/dt/EWH_coldrooms_CSTM/ctsmTMB_parameterestimation_callable.R",
    "working_dir": "/dt/EWH_coldrooms_CSTM",
    "output_file": "cold_room_model_cont.json"
  },


  // ─── DATABASE ─────────────────────────────────────────────────────
  "database": {
    "sensors_table": "measurements",
    "forecasts_table": "disturbance_forecasts",
    "setpoints_table": "setpoints"
  }
}
```


**Example for a different pilot (building HVAC):**


```jsonc
{
  "pilot_id": "montcada_hvac",
  "zones": [
    {
      "id": "zone_10",
      "states": [{ "name": "T_room", "unit": "K", "initial": 295.15 }],
      "constraints": { "T_room": { "min": 294.15, "max": 303.15 } },
      "setpoint": { "state": "T_room", "min": 292.15, "max": 303.15 }
    }
    // ... 37 more zones
  ],
  "controls": [
    { "id": "sp_hvac", "type": "continuous", "unit": "K", "min": 288, "max": 308, "zones": ["all"] }
  ],
  "disturbances": [
    { "id": "T_outdoor", "unit": "K", "scope": "global" },
    { "id": "occupancy", "unit": "persons", "scope": "per_zone" },
    { "id": "solar_irradiance", "unit": "W/m2", "scope": "global" }
  ],
  "dynamics": {
    "type": "arx",
    "lags": { "T_room": [1,2,3], "sp_hvac": [0,1,2], "T_outdoor": [1,2,3] }
  },
  "power_topology": {
    "has_grid": true,
    "compressor_groups": [
      {
        "id": "hvac",
        "type": "mode_switching",
        "modes": ["heating", "cooling"],
        "binary_mode_selection": true
      }
    ]
  }
}
```


***


### 2. FlexOPTi — Generic MPC Engine


#### 2.1 Remove concrete pilot types


**File:** `FlexOPTi/src/core/types.jl`


**Today:**
```julia
struct Montcada <: AbstractElecBuilding end
struct Ewh      <: AbstractElecBuilding end
struct Karno    <: AbstractHeatBuilding end
```


**Proposal:** Replace with a configurable struct that loads its definition from `pilot_config.json`:


```julia
struct GenericPilot <: AbstractBuilding
    config::PilotConfig  # parsed from pilot_config.json
end


struct PilotConfig
    id::String
    zones::Vector{ZoneConfig}
    controls::Vector{ControlConfig}
    disturbances::Vector{DisturbanceConfig}
    dynamics_type::Symbol              # :state_space | :arx | :neural
    state_layout::Vector{StateMapping}
    control_layout::Vector{ControlMapping}
    disturbance_layout::Vector{DisturbanceMapping}
    power_topology::PowerTopologyConfig
    objective::ObjectiveConfig
    constraints_dict::Dict{Symbol, Any}
end


struct ZoneConfig
    id::String
    label::String
    states::Vector{StateConfig}
    constraints::Dict{String, BoundPair}
    setpoint::Union{SetpointConfig, Nothing}
end


struct ControlConfig
    id::String
    type::Symbol   # :continuous | :binary
    unit::String
    min::Float64
    max::Float64
    on_value::Union{Float64, Nothing}  # for binary controls
    zones::Vector{String}
end
```


#### 2.2 Generic Digital Twin parser


**Current file:** `pilots/ewh/parse_digital_twin.jl` (expects state-space) and `pilots/montcada/parse_digital_twin.jl` (expects ARX).


**Proposal:** A parser that dispatches according to `dynamics.type` in the config:


```julia
function parse_digital_twin(pilot::GenericPilot, o::O, file::String)
    raw = JSON3.read(read(file, String))


    if pilot.config.dynamics_type == :state_space
        return parse_state_space(raw, pilot.config)
    elseif pilot.config.dynamics_type == :arx
        return parse_arx(raw, pilot.config)
    else
        error("Dynamics type $(pilot.config.dynamics_type) not supported")
    end
end
```


The DT JSON remains free-form (each pilot can have its own structure), but the parser knows what to expect thanks to `dynamics.type`.


**Dimensional validation:** When parsing, the system must verify that:
- `size(A) == (nx, nx)` where `nx = length(state_vector_layout)`
- `size(B) == (nx, nu)` where `nu = length(control_vector_layout)`
- `size(E) == (nx, nd)` where `nd = length(disturbance_vector_layout)`


#### 2.3 Generic construction of constraints


**Current file:** `pilots/ewh/constraints.jl` with EWH constants.


**Proposal:** Read limits directly from `pilot_config.json`:


```julia
function build_constraints(pilot::GenericPilot)
    config = pilot.config
    constraints = Dict{Symbol, Any}()


    for zone in config.zones
        for (state_name, bounds) in zone.constraints
            constraints[Symbol("$(zone.id)_$(state_name)_low")]  = bounds.min
            constraints[Symbol("$(zone.id)_$(state_name)_high")] = bounds.max
        end
        if !isnothing(zone.setpoint)
            constraints[Symbol("SP_$(zone.id)_low")]  = zone.setpoint.min
            constraints[Symbol("SP_$(zone.id)_high")] = zone.setpoint.max
        end
    end


    for ctrl in config.controls
        constraints[Symbol("$(ctrl.id)_low")]  = ctrl.min
        constraints[Symbol("$(ctrl.id)_high")] = ctrl.max
    end


    return constraints
end
```


#### 2.4 Generic MPC engine (the most complex change)


**Current file:** `pilots/ewh/mpc_main.jl` with EWH-specific variables, constraints, and objective.


**Proposal:** Build the JuMP model dynamically from the configuration.


##### Variables


```julia
function build_variables!(pilot::GenericPilot, model, o, ox)
    cfg = pilot.config
    Hu  = o.Hu
    nx  = length(cfg.state_layout)
    nz  = length(cfg.zones)


    # States: always present
    @variable(model, x[1:nx, 1:Hu])


    # Setpoints: one per zone that has a defined setpoint
    sp_zones = filter(z -> !isnothing(z.setpoint), cfg.zones)
    @variable(model, sp[1:length(sp_zones), 1:Hu])


    # Controls: according to type
    vars = Dict{String, Any}()
    for ctrl in cfg.controls
        if ctrl.type == :continuous
            vars[ctrl.id] = @variable(model, [1:Hu],
                lower_bound = ctrl.min, upper_bound = ctrl.max,
                base_name = ctrl.id)
        elseif ctrl.type == :binary
            milp_steps = clamp(o.milp_horizon, 0, Hu)
            v = @variable(model, [1:Hu], base_name = ctrl.id)
            for t in 1:milp_steps; set_binary(v[t]); end
            for t in milp_steps+1:Hu
                set_lower_bound(v[t], 0.0)
                set_upper_bound(v[t], 1.0)
            end
            vars[ctrl.id] = v
        end
    end


    # Grid (if applicable)
    if cfg.power_topology.has_grid
        vars["p_buy"]  = @variable(model, [1:Hu], lower_bound=0.0, base_name="p_buy")
        vars["p_sell"] = @variable(model, [1:Hu], lower_bound=0.0, base_name="p_sell")
    end


    return GenericVars(x, sp, vars, sp_zones)
end
```


##### Constraints


```julia
function build_constraints!(pilot::GenericPilot, model, vars, o, ox)
    cfg = pilot.config
    con = ox.constraints
    dyn = ox.dynamics
    Hu  = o.Hu


    # 1. DYNAMICS: X = Ξ·U + M·ξ₁ + Ψ·Δ
    u_full = build_control_vector(cfg, vars, Hu)
    @constraint(model, vec(vars.x) .== dyn.Ξ * vec(u_full) .+
                                         dyn.M * dyn.ξ1    .+
                                         dyn.Ψ * dyn.Δ)


    # 2. COMFORT: for each zone, bound the observed state
    for (zi, zone) in enumerate(cfg.zones)
        for (state_name, bounds) in zone.constraints
            state_idx = find_state_index(cfg, zone.id, state_name)
            @constraint(model, [t=1:Hu], bounds.min ≤ vars.x[state_idx, t])
            @constraint(model, [t=1:Hu], vars.x[state_idx, t] ≤ bounds.max)
        end
    end


    # 3. SETPOINTS: bound setpoints by zone
    for (si, zone) in enumerate(vars.sp_zones)
        sp_low  = zone.setpoint.min
        sp_high = zone.setpoint.max
        @constraint(model, [t=1:Hu], sp_low ≤ vars.sp[si, t] ≤ sp_high)
    end


    # 4. POWER TOPOLOGY: according to config
    for group in cfg.power_topology.compressor_groups
        if group.type == :shared_modulated_plus_binary
            build_shared_compressor_constraints!(model, vars, group, cfg, Hu)
        elseif group.type == :hysteresis_binary
            build_hysteresis_constraints!(model, vars, group, cfg, Hu)
        elseif group.type == :mode_switching
            build_mode_switching_constraints!(model, vars, group, cfg, Hu)
        end
    end


    # 5. POWER BALANCE (if there is a grid)
    if cfg.power_topology.has_grid
        build_power_balance!(model, vars, cfg, Hu)
    end
end
```


##### Objective


```julia
function build_objective!(pilot::GenericPilot, model, vars, o, ox)
    cfg = pilot.config
    Hu  = o.Hu
    Δt  = o.Δt


    cost = AffExpr(0.0)


    for component in cfg.objective.components
        price = resolve_price(component.price_source, ox)
        v     = vars.controls[component.term]
        sign  = component.sign == "+" ? 1.0 : -1.0
        for t in 1:Hu
            add_to_expression!(cost, sign * Δt * v[t] * price[t])
        end
    end


    @objective(model, Min, cost)
end
```


#### 2.5 Generic Batch Dynamics


**Current file:** `pilots/ewh/batch_mpc.jl` with hardcoded `batch_Δ()`.


**Proposal:** A generic `batch_Δ` that uses `disturbance_vector_layout`:


```julia
function batch_Δ(pilot::GenericPilot, forecasts::Dict, Hu::Int)
    layout = pilot.config.disturbance_layout
    nd = length(layout)
    Δ = Vector{Float64}(undef, nd * Hu)


    for k in 1:Hu
        for (i, mapping) in enumerate(layout)
            idx = (k - 1) * nd + i
            if mapping.scope == :global
                Δ[idx] = forecasts[mapping.disturbance][k]
            else  # per_zone
                Δ[idx] = forecasts[mapping.disturbance][mapping.zone][k]
            end
        end
    end


    return Δ
end
```


#### 2.6 Generic output


**Current file:** `pilots/ewh/parse_output.jl` with EWH keys.


**Proposal:** `package_results` returns a dictionary with keys derived from the config:


```julia
function package_results(pilot::GenericPilot, model, vars, o, ox)
    result = Dict{Symbol, Any}(
        :OPT_cost   => objective_value(model),
        :OPT_status => termination_status(model),
        :x          => Matrix(value.(vars.x)'),   # [Hu × nx]
    )


    # Setpoints
    if !isempty(vars.sp_zones)
        result[:SP] = Matrix(value.(vars.sp)')
    end


    # Controls with their real names
    for (name, var) in vars.controls
        result[Symbol(name)] = value.(var)
    end


    return result
end
```


***


### 3. Generic Production Controller (`dr_controller.jl`)


**Today:** `N_ROOMS=5`, hardcoded DB schema, `u_first = vcat(oy[:u][1,:], oy[:p3] [doc.openl](https://doc.openl.io/translate/md/spanish))`.


**Proposal:** A controller that reads the pilot config and adapts the queries.


#### 3.1 Generic sensor reading


```julia
function db_read_sensors(db, pilot_config, tmp_dir)
    layout = pilot_config.state_layout
    nz     = length(pilot_config.zones)


    # Build query dynamically according to the defined states
    state_columns = unique(m.state for m in layout)
    cols = join(state_columns, ", ")


    rows = DBInterface.execute(db, """
        SELECT zone_id, $cols
        FROM $(pilot_config.database.sensors_table)
        WHERE timestamp = (SELECT MAX(timestamp) FROM $(pilot_config.database.sensors_table))
        ORDER BY zone_id
    """)


    # Assemble x0 according to state_vector_layout
    x0 = zeros(length(layout))
    for row in rows
        for mapping in layout
            if mapping.zone == row.zone_id
                x0[mapping.index + 1] = getproperty(row, Symbol(mapping.state))
            end
        end
    end


    f = joinpath(tmp_dir, "sensors.json")
    write(f, JSON3.write(Dict("x0" => x0)))
    return f
end
```


#### 3.2 Generic setpoint writing


```julia
function db_write_setpoints(db, pilot_config, oy, t_solve)
    ctrl_layout = pilot_config.control_layout


    for mapping in ctrl_layout
        value = oy[Symbol(mapping.control)] [doc.openl](https://doc.openl.io/translate/md/spanish)  # first step
        DBInterface.execute(db, """
            INSERT INTO $(pilot_config.database.setpoints_table)
                (timestamp, zone_id, control_name, value)
            VALUES (?, ?, ?, ?)
        """, (string(t_solve), mapping.zone, mapping.control, value))
    end
end
```


***


### 4. Generic Database


**Today:** Columns `T_air`, `T_product`, `door_load`, `u`, `p3` hardcoded.


**Proposal:** EAV (Entity-Attribute-Value) schema or generic columns:


```sql
-- ═══ GENERIC SCHEMA ═══


-- Sensor measurements (any variable, any zone)
CREATE TABLE measurements (
    id        INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT    NOT NULL,
    zone_id   TEXT    NOT NULL,    -- "dairy", "zone_10", etc.
    var_name  TEXT    NOT NULL,    -- "T_air", "T_product", "humidity", etc.
    value     REAL    NOT NULL,
    unit      TEXT
);
CREATE INDEX idx_meas_ts ON measurements(timestamp);


-- Disturbance forecasts (generic)
CREATE TABLE disturbance_forecasts (
    id        INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT    NOT NULL,
    zone_id   TEXT,                -- NULL for global disturbances
    var_name  TEXT    NOT NULL,    -- "T_ambient", "door_load", "occupancy"
    value     REAL    NOT NULL,
    unit      TEXT
);
CREATE INDEX idx_fc_ts ON disturbance_forecasts(timestamp);


-- Setpoints written by the controller (generic)
CREATE TABLE setpoints (
    id           INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp    TEXT    NOT NULL,
    zone_id      TEXT    NOT NULL,
    control_name TEXT    NOT NULL, -- "u_allocated", "p3", "sp_hvac"
    value        REAL    NOT NULL,
    unit         TEXT
);
CREATE INDEX idx_sp_ts ON setpoints(timestamp);


-- Pilot metadata (optional, for traceability)
CREATE TABLE pilot_meta (
    key   TEXT PRIMARY KEY,
    value TEXT
);
```


**Advantages:**
- Any pilot uses the same tables.
- The schema does not need to be altered when adding a new pilot.
- It can be filtered by `zone_id` and `var_name`.


**Alternative (less flexible but faster):** Keep fixed but parameterized columns:


```sql
CREATE TABLE measurements (
    id        INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp TEXT NOT NULL,
    zone_id   TEXT NOT NULL,
    state_1   REAL,   -- real name mapped in pilot_config
    state_2   REAL,
    state_3   REAL
);
```


The EAV option is recommended for maximum flexibility.


***


### 5. Generic Forecast Service


**Today:** `dummy_forecast.py` writes `T_ambient=18`, `door_load=0` for 5 rooms.


**Proposal:** The service reads the pilot configuration to know which disturbances to write:


```python
# Reads pilot_config.json
config = json.load(open("/app/pilot_config.json"))


def write_forecasts(conn, config):
    now = datetime.now(timezone.utc)
    disturbances = config["disturbances"]
    zones = config["zones"]


    for step in range(HORIZON_STEPS):
        ts = now + timedelta(seconds=step * DELTA_T_S)
        for dist in disturbances:
            if dist["scope"] == "global":
                conn.execute(
                    "INSERT INTO disturbance_forecasts "
                    "(timestamp, zone_id, var_name, value, unit) "
                    "VALUES (?, NULL, ?, ?, ?)",
                    (ts.isoformat(), dist["id"], dist["fallback_value"], dist["unit"])
                )
            elif dist["scope"] == "per_zone":
                for zone in zones:
                    conn.execute(
                        "INSERT INTO disturbance_forecasts "
                        "(timestamp, zone_id, var_name, value, unit) "
                        "VALUES (?, ?, ?, ?, ?)",
                        (ts.isoformat(), zone["id"], dist["id"],
                         dist["fallback_value"], dist["unit"])
                    )
```


***


### 6. Generic Control Room


**Today:** The dashboard shows EWH columns (`u`, `p3`, `room_id`).


**Proposal:** The dashboard reads `pilot_config.json` to build the view dynamically:


```python
@app.route("/")
def dashboard():
    config = load_pilot_config()
    zones = config["zones"]
    controls = config["controls"]


    # Build setpoints table dynamically
    setpoints = query_db(f"""
        SELECT timestamp, zone_id, control_name, value
        FROM setpoints ORDER BY id DESC LIMIT {len(zones) * 5}
    """)


    return render_template("dashboard.html",
        pilot_name=config["pilot_name"],
        zones=zones,
        controls=controls,
        setpoints=setpoints)
```


***


### 7. Generic Docker Compose


**Today:** `docker-compose.ewh.yml` with EWH-specific services.


**Proposal:** A single `docker-compose.yml` parameterized with `PILOT_CONFIG`:


```yaml
services:
  fc:
    build: ./FlexOPTi
    environment:
      - PILOT_CONFIG=/app/configs/${PILOT_ID:-ewh}/pilot_config.json
      - DB_PATH=/data/bb.db
    volumes:
      - db-data:/data
      - dt-volume:/dt-volume
      - ./configs:/app/configs:ro


  forecast:
    build: ./forecasting/dummy
    environment:
      - PILOT_CONFIG=/app/configs/${PILOT_ID:-ewh}/pilot_config.json
      - DB_PATH=/data/bb.db
    volumes:
      - db-data:/data
      - ./configs:/app/configs:ro


  control-room:
    build: ./control_room
    environment:
      - PILOT_CONFIG=/app/configs/${PILOT_ID:-ewh}/pilot_config.json
    volumes:
      - db-data:/data
      - ./configs:/app/configs:ro


  dt:
    build:
      context: ./dt
      dockerfile: ${DT_DOCKERFILE:-EWH_coldrooms_CSTM/Dockerfile}
    profiles: [retrain]
    volumes:
      - dt-volume:/dt-volume
```


Usage:


```bash
# EWH pilot
PILOT_ID=ewh docker compose up --build


# Montcada pilot
PILOT_ID=montcada docker compose up --build


# New pilot (water works)
PILOT_ID=enlion docker compose up --build
```


***


## Phased Implementation Plan


### Phase 1 — Config-Driven Constraints (low effort, high impact)


**Objective:** Constraints (temperatures, powers, setpoints) must come from a file, not code.


| Task | File |
|-------|---------|
| Create `pilot_config.json` for EWH with the proposed structure | `configs/ewh/pilot_config.json` |
| Create function `load_pilot_config(path)` in Julia | `core/pilot_config.jl` |
| Rewrite `build_constraints()` to read from config | `core/build_constraints.jl` |
| Existing pilots continue to work with no changes in MPC | — |


**Validation:** Existing tests pass with the same numerical results.


### Phase 2 — Generic DB schema + parameterized controller (medium effort)


| Task | File |
|-------|---------|
| Migrate `init.sql` to the generic EAV schema | `db/init_generic.sql` |
| Rewrite `db_read_sensors()` and `db_write_setpoints()` using config | `scripts/dr_controller.jl` |
| Rewrite `dummy_forecast.py` to read disturbances from config | `forecasting/dummy/dummy_forecast.py` |
| Adapt Control Room to read config | `control_room/main.py` |


### Phase 3 — GenericPilot + dynamic MPC (high effort, structural change)


| Task | File |
|-------|---------|
| Replace `Ewh`/`Montcada` structs with `GenericPilot` | `core/types.jl` |
| Generic DT parser with dispatch by `dynamics.type` | `core/parse_digital_twin.jl` |
| Dynamic `build_variables!()` from config | `core/mpc_builder.jl` |
| Generic `build_constraints!()` with topology blocks | `core/mpc_builder.jl` |
| Generic `build_objective!()` from config | `core/mpc_builder.jl` |
| Generic `batch_Δ()` using layout | `core/batch_mpc.jl` |
| Generic `package_results()` | `core/parse_output.jl` |
| Keep EWH/Montcada pilots as example configs | `configs/ewh/`, `configs/montcada/` |


### Phase 4 — Generic Docker + onboarding of new pilots


| Task | File |
|-------|---------|
| Unify docker-compose with `PILOT_ID` | `docker-compose.yml` |
| Document new pilot onboarding process | `docs/new_pilot_guide.md` |
| Create empty `pilot_config.json` template | `configs/template/pilot_config.json` |
| Config validator (JSON Schema) | `tools/validate_config.py` |


***


## Reusable Power Topology Blocks


The generic MPC needs a catalog of **constraint patterns** selected from the config. These are the ones that cover the current pilots and foreseeable future ones:


| Block | Constraint Pattern | Pilots |
|--------|-------------------------|---------|
| `shared_modulated_plus_binary` | Shared modulated compressor (P1) + on/off (P2) across zones, with proportional allocation via Kgain. `Σu = P1 + δ·P2_max`, `u_r ≤ u_r_req`, `u_r_req ≥ Kgain·(T - SP)` | EWH (rooms 1-4) |
| `hysteresis_binary` | On/off with Big-M hysteresis: `T - (SP + d) ≤ M·δ`, `(SP - d) - T ≤ M·(1-δ)` | EWH (freezer) |
| `mode_switching` | Two models (heating/cooling), binary selection `bh + bc ≤ 1`, conditional dynamics | Montcada |
| `continuous_setpoint` | Continuous setpoint `SP ∈ [SPmin, SPmax]` directly as control input | Generic |
| `ev_charging` | Charging rate `P ∈ [0, Pmax]`, accumulated energy constraint `Σ P·Δt ≥ E_required`, deadline | EV Charging |
| `tank_level` | Mass balance `L[k+1] = L[k] + (Qin - Qout)·Δt`, `Lmin ≤ L ≤ Lmax` | Enlion |
| `battery_soc` | `SOC[k+1] = SOC[k] + η_ch·Pch - Pdis/η_dis`, `SOCmin ≤ SOC ≤ SOCmax` | Future |


Each block is implemented as an independent Julia function and invoked according to the config:


```julia
# Block registry
const TOPOLOGY_BLOCKS = Dict{Symbol, Function}(
    :shared_modulated_plus_binary => build_shared_compressor_constraints!,
    :hysteresis_binary            => build_hysteresis_constraints!,
    :mode_switching               => build_mode_switching_constraints!,
    :continuous_setpoint          => build_continuous_setpoint_constraints!,
    :ev_charging                  => build_ev_charging_constraints!,
    :tank_level                   => build_tank_level_constraints!,
    :battery_soc                  => build_battery_soc_constraints!,
)


# In build_constraints!:
for group in cfg.power_topology.compressor_groups
    block_fn = TOPOLOGY_BLOCKS[Symbol(group.type)]
    block_fn(model, vars, group, cfg, Hu)
end
```


***


## Validation Checklist


Once the generalization is completed, verify that:


- [ ] EWH produces **exactly the same setpoints** as before (numerical regression).
- [ ] A new pilot can be defined **only with `pilot_config.json`**, without touching Julia.
- [ ] The DB schema works for **any combination** of zones and variables.
- [ ] The dashboard displays the zones and controls of the active pilot **dynamically**.
- [ ] The dummy forecast writes the **correct disturbances** according to config.
- [ ] It is possible to switch pilots **with an environment variable** (`PILOT_ID`).
- [ ] There is a **JSON Schema validator** that rejects invalid configs before startup.
- [ ] The MPC **unit tests** are parameterized with example configs.