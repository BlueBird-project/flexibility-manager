function build_constraints(pilot::AbstractBuilding)::Dict{Symbol, Any}

    @warn "Default contraint method called. Return unconstrained problem."

    return Dict()

end