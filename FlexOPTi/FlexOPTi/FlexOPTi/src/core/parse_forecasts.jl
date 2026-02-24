function parse_forecasts(pilot::AbstractBuilding, o::O, forecasts_file::String)::Dict{String, Any}

    @warn "No default forecast file. Return empty Dictionary."

    return Dict()

end