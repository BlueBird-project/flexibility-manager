function parse_sensors(pilot::AbstractBuilding, sensor_file::Union{Nothing, String})
    # No sensor file is a normal case (e.g. Ewh); warn only if a file is given and ignored
    sensor_file === nothing || @warn "No pilot-specific parse_sensors defined for $(typeof(pilot)). Ignoring $sensor_file."
    return nothing

end
