function mpc_update(pilot::AbstractBuilding, o::O, ox::OX,
                    oy_prev::Union{Dict{Symbol,Any}, Nothing} = nothing)::Dict{Symbol, Any}
    @warn "No default call for mpc_update. Return empty"
    return Dict()
end