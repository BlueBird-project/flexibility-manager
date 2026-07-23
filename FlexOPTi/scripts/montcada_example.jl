## Imports
import Pkg
Pkg.activate(@__DIR__)

using Revise
using Infiltrator
using Logging
using Debugger

using FlexOPTi

Debugger.breakpoint(FlexOPTi.fetch_market_prices)

## Test
dt_file       = joinpath(@__DIR__,"../data/montcada/inputs/dynamics_estimator_results.json");
sensors_file  = joinpath(@__DIR__,"../data/montcada/inputs/df_predict.json");
forecast_file = joinpath(@__DIR__,"../data/montcada/inputs/dynamics_estimator_results.json");

## optimize 
oy = FlexOPTi.optimize(dt_file, sensors_file, forecast_file; loglevel="info", Hu = 2, pilot = "Montcada", solver = "Gurobi", compute_datetime = "2025-07-15T17:00:00+00:00", market_country=nothing);
