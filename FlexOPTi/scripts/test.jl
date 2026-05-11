## Imports
import Pkg
Pkg.activate(".")

using Revise
using Infiltrator
using Logging
using Debugger

using FlexOPTi

## Test
dt_file = joinpath(@__DIR__, "../data/montcada/inputs/dynamics_estimator_results.json");
sensors_file = joinpath(@__DIR__, "../data/montcada/inputs/df_predict.json");
forecast_file = joinpath(@__DIR__, "../data/montcada/inputs/dynamics_estimator_results.json");
 
## optimize 
oy = optimize(dt_file, sensors_file, forecast_file; loglevel="info", Hu = 2, pilot = "Montcada", solver = "Gurobi", compute_datetime = "2025-07-15T17:00:00+00:00");

## Save outputs to json 
opt_data = FlexOPTi.parse_OPT_output(oy)
FlexOPTi.write_outputs_to_file(opt_data; file=joinpath(@__DIR__, "../data/montcada/outputs/output.json"))
