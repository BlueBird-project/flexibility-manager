## Imports
import Pkg
Pkg.activate(".")

using Revise
using Infiltrator
using Logging
using Debugger

using FlexOPTi

## Test
dt_file       = joinpath(pwd(), "data", "EWH", "inputs", "cold_room_model.json");
sensors_file  = nothing
forecast_file = nothing
 
## optimize 
oy = optimize(dt_file, sensors_file, forecast_file; loglevel="info", Hu = 100, pilot = "Ewh", solver = "Gurobi");
