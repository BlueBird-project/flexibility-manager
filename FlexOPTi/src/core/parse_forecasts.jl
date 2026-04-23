function parse_forecasts(pilot::AbstractBuilding, o::O, forecasts_file::Union{Nothing, String})

    @warn "No pilot-specific parse_forecasts defined for $(typeof(pilot)). Returning nothing."
    return nothing

end
