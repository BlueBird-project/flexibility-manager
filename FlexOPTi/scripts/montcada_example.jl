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
dt_file       = joinpath(@__DIR__,"../data/montcada/inputs/dt_file.json");
sensors_file  = joinpath(@__DIR__,"../data/montcada/inputs/sensors_file.json");
forecast_file = joinpath(@__DIR__,"../data/montcada/inputs/forecasts_file.json");

## optimize 
oy = FlexOPTi.optimize(dt_file, sensors_file, forecast_file;
                       pilot = "Montcada",
                       Hu = 3,
                       solver = "HiGHS",
                       market_country = nothing,
                       output_file = "res_montcada_tmp.json",
                       compute_datetime = "2026-09-10T09:00:00+00:00",
                       soft_temperature = true
                      );
