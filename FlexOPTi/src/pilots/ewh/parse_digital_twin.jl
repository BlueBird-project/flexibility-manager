
function parse_digital_twin(::Ewh, o::O, json_path::String)::Dict{String, Any}

    data = JSON.parsefile(json_path)
    ss   = data["state_space"]

    # JSON gives nested arrays; convert to Matrix{Float64} for linear algebra
    to_matrix(rows) = Matrix{Float64}(reduce(hcat, rows)')
    ss["A"]  = to_matrix(ss["A"])
    ss["B"]  = to_matrix(ss["B"])
    ss["E"]  = to_matrix(ss["E"])
    ss["C"]  = to_matrix(ss["C"])
    ss["x0"] = Vector{Float64}(ss["x0"])

    return data
end