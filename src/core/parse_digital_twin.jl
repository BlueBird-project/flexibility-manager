function parse_digital_twin(pilot::AbstractBuilding, o::O, digital_twin_file::String)::Dict{String, Any}

    @warn "No default parser for the digital twin. Return empty."

    return Dict()

end # function 