# Latter to become a Module so that the different pilots don't see the common points

const KELVIN_OFFSET    = 273.15
const SENSITIVITY      = 0.5 # C  
const EPSILON          = 1e8*eps()

include(joinpath(@__DIR__, "parse_digital_twin.jl"))
include(joinpath(@__DIR__, "parse_sensors.jl"))
include(joinpath(@__DIR__, "parse_forecasts.jl"))
include(joinpath(@__DIR__, "build_constraints.jl"))
include(joinpath(@__DIR__, "build_dynamics.jl"))
include(joinpath(@__DIR__, "mpc_helper.jl"))
include(joinpath(@__DIR__, "mpc_update.jl"))
include(joinpath(@__DIR__, "parse_output.jl"))

# module Montcada
resolve_building(::Val{:Montcada}) = Montcada()

# end