# FlexOPTi

FlexOPTi is a flexibility optimization package written in [Julia](https://julialang.org/), developed as part of the Blue Bird project. It runs a one-step Model Predictive Control (MPC) optimization for a selected building pilot.

**Requires Julia >= 1.11** — download and install from https://julialang.org/downloads/

---

## Julia Usage

> **All commands and paths in this README assume you are inside the `FlexOPTi`
> folder** (the one containing this file), unless stated otherwise.

```bash
cd FlexOPTi
```

Activate and load the package:

```julia
$ julia
julia> import Pkg
julia> Pkg.activate(".")
julia> Pkg.instantiate()      # first time only — installs dependencies
julia> using FlexOPTi
```

To get help on any function:

```julia
julia> ?
help?> optimize
```

### Running an optimization

```julia
# Required input files (JSON format)
dt_file       = "data/montcada/inputs/dynamics_estimator_results.json"  # model structure & identified dynamics
sensors_file  = "data/montcada/inputs/df_predict.json"                  # current measurements / initial conditions
forecast_file = "data/montcada/inputs/dynamics_estimator_results.json"  # disturbance predictions

# Run the optimization — returns a Dict{Symbol,Any}
oy = FlexOPTi.optimize(
    dt_file,
    sensors_file,
    forecast_file;
    pilot          = "Montcada",   # required — "Montcada" or "Ewh" (case-sensitive)
    Hu             = 4,            # control horizon (future timesteps)
    solver         = "HiGHS",      # "HiGHS" (default) or "Gurobi"
    market_country = nothing,      # nothing → dummy 1.0 EUR/kWh price
)
```

`optimize` returns the **live results dictionary** `oy::Dict{Symbol,Any}`, not a
file. Work with it directly in Julia — no serialization involved:

```julia
julia> oy[:OPT_status]      # OPTIMAL  (a MathOptInterface.TerminationStatusCode)
julia> oy[:OPT_cost]        # 1.0513324338460874e6
julia> oy[:p_HVAC]          # [35386.28, 26231.65, 18397.58, 25117.73]  — HVAC power per step [kW]
julia> size(oy[:T])         # (4, 36)  — full temperature state matrix
```

Two keys carry the full run context:

| key | contents |
|---|---|
| `oy[:o]` | the resolved options `O` (`Hu`, `Δt`, `solver`, `pilot`, prices config, …) |
| `oy[:ox]` | the inputs `OX` (digital twin, sensors, forecasts, constraints, dynamics, prices) |

```julia
julia> oy[:o].Hu          # 4
julia> oy[:o].solver      # "HiGHS"
julia> oy[:ox].prices     # price vector actually used
```

### Exporting results to JSON

Serialization is a **separate, optional step**. `parse_OPT_output` converts `oy`
into a JSON-serializable `Dict{String,Any}` with renamed keys and units;
`write_outputs_to_file` writes that to disk.

```julia
# Extract the pilot type for efficient multiple dispatch
pilot = oy[:o].pilot

# Parse results into a JSON-serializable dictionary
#   only_next_step = true   → only the first MPC step
#   only_next_step = false  → the full horizon (default)
json_data = FlexOPTi.parse_OPT_output(pilot, oy; only_next_step = false)

# Write to a JSON file
FlexOPTi.write_outputs_to_file(json_data; file = "result.json")
```

> **Note** — `parse_OPT_output` drops `:o` and `:ox`. Use `oy` directly if you
> need the options or the inputs.

---

## Python Usage

**PyFlexOPTi** (`python/pyflexopti.py`) is the Python wrapper around the FlexOPTi
Julia package. It runs FlexOPTi as a subprocess, so no Julia/Python bridge
(PyCall, PyJulia) is required.

Requirements:

- a working `julia` on your `PATH` (Julia >= 1.11)
- **Python 3.7+ — no `pip install` needed** (standard library only)

### Step 1 — Install the Julia dependencies (once)

```bash
julia --project=. -e "import Pkg; Pkg.instantiate()"
```

### Step 2 — Call it from Python

```python
import sys
sys.path.insert(0, "python")   # or add FlexOPTi/python to PYTHONPATH

from pyflexopti import optimize

oy = optimize(
    dt_file        = "path/to/digital_twin.json",  # model structure & identified dynamics
    sensors_file   = "path/to/sensors.json",       # current measurements / initial conditions
    forecast_file  = "path/to/forecasts.json",     # disturbance predictions (weather, occupancy...)
    pilot          = "Montcada",                   # required — "Montcada" or "Ewh" (case-sensitive)
    Hu             = 24,                           # control horizon (future timesteps)
    solver         = "HiGHS",                      # "HiGHS" (default) or "Gurobi"
    market_country = "Spain",                      # None → dummy 1.0 EUR/kWh price
    output_file    = "result.json",                # optional — also keep the JSON on disk
)

print(oy["OPTTerminationStatus"])   # "OPTIMAL"
print(oy["OPTCost"])                # 1051332.43...
print(oy["HVACTotalPower"])         # setpoints with datetime + units
```

`optimize` returns a Python `dict`, already deserialized — the equivalent of
Julia's `parse_OPT_output(pilot, oy)`.

Useful extra arguments:

| kwarg | Effect |
|---|---|
| `output_file` | Also write the JSON to this path (omit → discarded after reading) |
| `only_next_step` | `True` → only the first MPC step instead of the full horizon |
| `capture_output` | `True` → hide Julia's logs (shown in the error message on failure) |
| `julia` | Path to the `julia` executable, if not on `PATH` |

Any other keyword is forwarded to the Julia `optimize` (see the table below).

### Runnable example

Using the Montcada sample data shipped with the repository:

```python
import sys
sys.path.insert(0, "python")
from pyflexopti import optimize

oy = optimize(
    dt_file       = "data/montcada/inputs/dynamics_estimator_results.json",
    sensors_file  = "data/montcada/inputs/df_predict.json",
    forecast_file = "data/montcada/inputs/dynamics_estimator_results.json",
    pilot         = "Montcada",
    Hu            = 2,
    market_country = None,
    compute_datetime = "2025-07-15T17:00:00+00:00",   # sample data covers July 2025
)
print(oy["OPTTerminationStatus"])   # OPTIMAL
```

> **Note** — the bundled sample data only covers **July 2025**. You must pass
> `compute_datetime` inside that window, otherwise the MPC horizon runs past the
> end of the forecast series and the run fails with a `BoundsError`. With your
> own up-to-date data you can omit it and the current UTC time is used.

### Command-line usage

The same runner works from any language, or directly from a shell. Write a JSON
config and pass it to `scripts/run_optimize.jl`:

```json
{
  "dt_file":          "data/montcada/inputs/dynamics_estimator_results.json",
  "sensors_file":     "data/montcada/inputs/df_predict.json",
  "forecast_file":    "data/montcada/inputs/dynamics_estimator_results.json",
  "pilot":            "Montcada",
  "Hu":               2,
  "market_country":   null,
  "compute_datetime": "2025-07-15T17:00:00+00:00",
  "output_file":      "result.json"
}
```

```bash
julia --project=. scripts/run_optimize.jl config.json
```

Required keys are `dt_file`, `sensors_file`, `forecast_file`, `pilot` and
`output_file`; every other key is forwarded to `optimize` as a keyword argument.

> **Startup cost** — each call pays Julia's start-up and compilation latency
> (a few seconds). This is negligible at typical MPC cadences (`delta_t` is
> 900 s by default). If you need many rapid calls, run FlexOPTi as a long-lived
> HTTP service instead (see `scripts/ewh/dr_controller.jl`).

### Keyword arguments reference for `optimize`

| kwarg | Type | Default | Description |
|---|---|---|---|
| `pilot` | `String` | **required** | Building/pilot name: `"Montcada"` or `"Ewh"` (case-sensitive) |
| `Hu` | `Int` | `24` | Control horizon (future timesteps) |
| `Δt` | `Float64` | `900.0` | Sampling time in seconds |
| `init_condition` | `Bool` | `false` | Enforce special initial-condition handling |
| `solver` | `String` | `"HiGHS"` | LP/MILP solver (`"HiGHS"` or `"Gurobi"`) |
| `mip_gap` | `Float64` | `1e-4` | Relative MIP gap tolerance (e.g. `0.01` = 1%) |
| `milp_horizon` | `Int` | `1` | Steps with binary constraints; `0` = full LP, `Hu` = full MILP |
| `warm_start` | `Bool` | `false` | Reuse a previous solution as initial guess |
| `continuous_dynamo` | `Bool` | `true` | Use continuous (`true`) or discrete (`false`) dynamics |
| `compute_datetime` | `String` \| `ZonedDateTime` | current UTC | Start time of the MPC horizon, e.g. `"2025-07-15T17:00:00+00:00"` |
| `market_country` | `String` \| `None` | `None` | Country for day-ahead prices; `None` → dummy 1.0 EUR/kWh |
| `variable_Hu` | `Bool` | `false` | Shrink `Hu` to the number of published price slots |
| `tm_base_url` | `String` | `"http://localhost:9090"` | Trading Manager service URL |
| `loglevel` | `String` | `"info"` | Log verbosity: `"debug"`, `"info"`, `"warn"`, `"error"` |
| `logoutput` | `String` | `"combined"` | Log destination: `"console"`, `"file"`, `"combined"` |
| `logfile` | `String` | `"fm.log"` | Log file name (used when `logoutput` includes `"file"`) |
| `log_with_time` | `Bool` | `true` | Prepend timestamps to log entries |
| `output_file` | `String` | `"output.txt"` | Filename for the raw (non-JSON) result export |

The Python wrapper additionally accepts `only_next_step` (`bool`, default
`False`) to export only the first MPC step instead of the full horizon.

---

## Troubleshooting

### Missing `LibGit2` or `LibCURL` errors

On some OS, Julia's bundled stdlib binaries for `LibGit2` and `LibCURL` are not pre-built. If you see errors mentioning these packages, rebuild them from within Julia before running FlexOPTi:

```julia
$ julia
julia> import Pkg
julia> Pkg.build("LibGit2")
julia> Pkg.add("LibCURL")
```

### `BoundsError: attempt to access N-element Vector`

The MPC horizon extends past the end of your forecast series. Either shorten
`Hu`, or set `compute_datetime` to a time covered by your data. With the bundled
sample data this means a timestamp inside July 2025 (see the runnable example
above).

### `julia: command not found` (from Python)

The wrapper invokes `julia` from your `PATH`. If Julia is installed elsewhere,
pass the full path explicitly:

```python
optimize(..., julia=r"C:\Users\you\AppData\Local\Programs\Julia-1.11\bin\julia.exe")
```

### Connection errors to the Trading Manager

If no Trading Manager is reachable, run with `market_country=None`. FlexOPTi
then falls back to a dummy price of 1.0 EUR/kWh, which is fine for testing the
pipeline but not for meaningful cost optimization.

---

## Running Tests

```julia
$ julia
julia> import Pkg
julia> Pkg.activate(".")
julia> Pkg.test()
```
