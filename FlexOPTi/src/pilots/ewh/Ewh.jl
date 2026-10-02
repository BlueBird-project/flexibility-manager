# Latter to become a Module so that the different pilots don't see the common points

include(joinpath(@__DIR__, "parse_digital_twin.jl"))
include(joinpath(@__DIR__, "parse_forecasts.jl"))
include(joinpath(@__DIR__, "constraints.jl"))
include(joinpath(@__DIR__, "batch_mpc.jl"))
include(joinpath(@__DIR__, "mpc_types.jl"))
include(joinpath(@__DIR__, "mpc_main.jl"))
include(joinpath(@__DIR__, "parse_output.jl"))

# module Ewh 
resolve_building(::Val{:Ewh}) = Ewh()

# end