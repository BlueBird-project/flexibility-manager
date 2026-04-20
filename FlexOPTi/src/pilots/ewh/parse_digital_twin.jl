
function parse_digital_twin(::Ewh, o::O, json_path::String)::Dict{String, Any}

    data = JSON.parsefile(json_path)
    parsed_data = data

    return parsed_data
end