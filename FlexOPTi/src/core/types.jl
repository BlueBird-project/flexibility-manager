# TODO : Consider 3 structures MP, MPX, MPY with 
# TODO   respectively the code params, the input (forecasts, digital twin, trading),
# TODO   and the optimization output (control action, state prediction, etc)

# Abstract types for multiple dispatch
abstract type AbstractBuilding end
abstract type AbstractHeatBuilding <: AbstractBuilding end
abstract type AbstractElecBuilding <: AbstractBuilding end

# Define Each pilot 
# Each parameters specific to the pilot could be defined here 
struct Montcada <: AbstractElecBuilding end 
struct Ewh      <: AbstractElecBuilding end
struct Karno    <: AbstractHeatBuilding end 

"""
resolve_building(name::String) -> AbstractBuilding

This function is extended by building modules to create the correct building type.
"""
function resolve_building(name::String)
    resolve_building(Val(Symbol(name)))
end

function resolve_building(::Val{name}) where name
    error("Building '$name' not registered")
end


"""
mutable struct O
        
    O(Hu::Int, init_condition::Bool, MILP::Bool, pilot::String)

Define code parameters

    - Hu::Int : MPC control horizon 
    - init_condition::Bool : Use explicitely initial conditions
pilot::String : Pilot string identification 
                "EWH", "Montcada", ...
"""
mutable struct O
    Hu::Int
    init_condition::Bool
    # name::String
    pilot::Union{String, AbstractBuilding, Nothing}

    loglevel::String
    logoutput::String
    logfile::String
    log_with_time::Bool
    solver::String

    output_file::String
    compute_datetime::Union{String, ZonedDateTime, Nothing}
end 

"""
    Used for batch MPC formulation

    X = M⋅ξ1 + Ξ⋅U + Ψ⋅Δ
    where ξ1 are the initial condition and Δ a stack of the disturbances (forecasts)
    return BatchDynamics(M, Ξ, Ψ, C, ξ1, Δ)
"""
struct BatchDynamics
   M::Matrix{Float64}   # State mapping   
   Ξ::Matrix{Float64}   # Control mapping
   Ψ::Matrix{Float64}   # Disturbance mapping
   ξ1::Vector{Float64}  # Initial conditions
   Δ::Vector{Float64}   # Disturbances / Forecasts
end

"""
mutable struct OX

    OX(digital_twin::Vector{String, Any}, forecast::Vector{String, Any}, constraints::MPConstraints, x0::Vector{Real})

    digital_twin::Vector{String, Any}  : digital_twin to be obtained from digital_twin API
    sensors::Vector{Dict{String, Any}} : Sensor file 
    forecast::Vector{String, Any} : forecasts to be obtained from forecasts API
    constraints::MPConstraints : MPC optimization constraints
"""
struct OX
    digital_twin::Dict{String, Any}
    sensors::Union{Vector{Dict{String, Any}}, Nothing}
    forecast::Union{Dict{String, Any}, Nothing}
    constraints::Dict{Symbol, Any}
    dynamics::Any
end

"""
    Depreciated. 
"""
struct OY
    output_dic::Dict{Symbol, Any} 
end
