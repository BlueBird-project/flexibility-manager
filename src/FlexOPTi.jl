"""
FlexOPTi — Flexbility Manager Optimization Engine

Public API:
    optimize(...)
    parse_output(...)
    set_logging(...)

  - Use Julia help function for details 
    e.g. help?> optimize

All other functions are internal and may change without notice.
"""
module FlexOPTi

# Dependencies
using Printf
using JSON, JSON3
using JuMP
using HiGHS, Gurobi
using SparseArrays
using Symbolics 
using Logging, LoggingExtras
using Dates, TimeZones
using Debugger
using Plots

#Debug stage
using Infiltrator
using Ipopt

# Global

# Types
include(joinpath(@__DIR__, "core", "types.jl"))
# Logging
include(joinpath(@__DIR__, "logging.jl"))
# Miscelanous
include(joinpath(@__DIR__, "print", "print_output.jl"))
# MPC and optimization
include(joinpath(@__DIR__, "core", "parse_digital_twin.jl"))
include(joinpath(@__DIR__, "core", "parse_sensors.jl"))
include(joinpath(@__DIR__, "core", "parse_forecasts.jl"))
include(joinpath(@__DIR__, "core", "build_constraints.jl"))
include(joinpath(@__DIR__, "core", "mpc_update.jl"))
include(joinpath(@__DIR__, "core", "fm_optimize.jl"))
include(joinpath(@__DIR__, "core", "utils.jl"))
include(joinpath(@__DIR__, "core", "parse_output.jl"))

# Montcada specific
include(joinpath(@__DIR__, "pilots", "montcada", "Montcada.jl"))

# Plots 
include(joinpath(@__DIR__, "core", "plots", "plot_state.jl"))

# API
export optimize , # Core MPC function 
	parse_output, # Write the output of optimize to a file
       set_logging   # For logging type and level

end # module 