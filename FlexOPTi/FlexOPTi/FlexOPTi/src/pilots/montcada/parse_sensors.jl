function parse_sensors(::Montcada, sensor_file::String)::Vector{Dict{String,Any}}

    data = JSON.parse(read(sensor_file, String))  # Vector{Dict{String,Any}}

    # target = digital_twin["LastStartDateTime"]::ZonedDateTime

    # convert the stored start string into ZonedDateTime
    # idx = findfirst(r -> ZonedDateTime(r["start"]) == target, data)
    # idx === nothing && error("Timestamp $(target) not found in log")

    # # compute required backward window
    # maxlag = maximum(maximum.(values(digital_twin["CollectedLags"])))
    # start_idx = max(1, idx - maxlag)

    # return data[start_idx:idx]  
    return data
end
