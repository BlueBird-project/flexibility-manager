## Imports
import Pkg
Pkg.activate(".")

using Revise
using Infiltrator
using Logging
using Debugger

using FlexOPTi

## Test
dt_file = joinpath("/home/kahka/DTU/BlueBird/flexmanager/FlexOPTi/data/dynamics_estimator_results.json");
sensors_file = joinpath("/home/kahka/DTU/BlueBird/flexmanager/FlexOPTi/data/df_predict_2.json");
forecast_file = joinpath("/home/kahka/DTU/BlueBird/flexmanager/FlexOPTi/data/dynamics_estimator_results.json");
 
## optimize 
oy = optimize(dt_file, sensors_file, forecast_file; loglevel="info", Hu = 2, pilot = "Montcada", solver = "Gurobi", compute_datetime = "2025-07-15T17:00:00+00:00");
