# FlexOPTi

FlexOPTi is a flexibility optimization package written in [Julia](https://julialang.org/), developed as part of the Blue Bird project. It runs a one-step Model Predictive Control (MPC) optimization for a selected building pilot.

**Requires Julia >= 1.11** — download and install from https://julialang.org/downloads/

---

## Julia Usage

Activate and load the package from within the `flexibility_manager` folder:

```julia
$ julia
julia> import Pkg
julia> Pkg.activate("FlexOPTi")
julia> using FlexOPTi
```

To get help on any function:

```julia
julia> ?
help?> optimize
```

---

## Python Usage

### Step 1 — Install Julia dependencies

Launch Julia and install the required packages:

```julia
$ julia
julia> import Pkg
julia> Pkg.add("PyCall")
```

### Step 2 — Install PyJulia

```bash
pip install julia
```

### Step 3 — Configure PyJulia and load FlexOPTi

```python
import julia

# Only needed once, if Julia was not already configured for PyJulia
julia.install()

from julia import Pkg
Pkg.activate(".")
Pkg.develop(path="FlexOPTi")

from julia import FlexOPTi
```

### Step 4 — Run an optimization

```python
# Required input files (JSON format)
dt_file       = "path/to/digital_twin.json"   # model structure and identified dynamics
sensors_file  = "path/to/sensors.json"         # current measurements / initial conditions
forecast_file = "path/to/forecasts.json"       # disturbance predictions (weather, occupancy...)

# Run the optimization
out_dic = FlexOPTi.optimize(
    dt_file,
    sensors_file,
    forecast_file,
    pilot       = "Montcada",   # required — name of the building/pilot
    Hu          = 4,            # control horizon (number of future timesteps), default: 1
    solver      = "HiGHS",      # LP/MILP solver: "HiGHS" (default) or "Gurobi"
    loglevel    = "info",       # "debug" | "info" | "warn" | "error"  (default: "info")
    logoutput   = "combined",   # "console" | "file" | "combined"       (default: "combined")
    logfile     = "fm.log",     # log file name when file logging is active (default: "fm.log")
    log_with_time = True,       # prepend timestamps to log entries      (default: True)
    output_file = "output.txt", # filename for raw result export         (default: "output.txt")
    # compute_datetime = ...    # ZonedDateTime for the MPC start time; defaults to current UTC time
)

# Parse results into a JSON-serializable dictionary
#   only_next_step=True  → export only the first MPC step
#   only_next_step=False → export the full horizon (default)
json_data = FlexOPTi.parse_OPT_output(out_dic, only_next_step=False)

# Write results to a JSON file
FlexOPTi.write_outputs_to_file(json_data, file="./data/outputs/output.json")
```

### Keyword arguments reference for `optimize`

| kwarg | Type | Default | Description |
|---|---|---|---|
| `pilot` | `String` | **required** | Building/pilot name (e.g. `"Montcada"`) |
| `Hu` | `Int` | `1` | Control horizon (future timesteps) |
| `init_condition` | `Bool` | `false` | Enforce special initial-condition handling |
| `solver` | `String` | `"HiGHS"` | LP/MILP solver (`"HiGHS"` or `"Gurobi"`) |
| `compute_datetime` | `ZonedDateTime` | current UTC | Start time of the MPC horizon |
| `loglevel` | `String` | `"info"` | Log verbosity: `"debug"`, `"info"`, `"warn"`, `"error"` |
| `logoutput` | `String` | `"combined"` | Log destination: `"console"`, `"file"`, `"combined"` |
| `logfile` | `String` | `"fm.log"` | Log file name (used when `logoutput` includes `"file"`) |
| `log_with_time` | `Bool` | `true` | Prepend timestamps to log entries |
| `output_file` | `String` | `"output.txt"` | Filename for raw result export |

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

---

## Running Tests

```julia
$ julia
julia> import Pkg
julia> Pkg.activate(".")
julia> Pkg.test()
```
