
function discretize_zoh(Ac, Bc, Ec, Δt::Float64)
    nx, nu, nd = size(Ac, 1), size(Bc, 2), size(Ec, 2)
    total_dim  = nx + nu + nd
    M          = zeros(total_dim, total_dim)
    M[1:nx, 1:nx]              = Ac
    M[1:nx, nx+1:nx+nu]        = Bc
    M[1:nx, nx+nu+1:total_dim] = Ec
    Md = exp(M * Δt)
    return Md[1:nx, 1:nx], Md[1:nx, nx+1:nx+nu], Md[1:nx, nx+nu+1:total_dim]
end

function parse_digital_twin(::Ewh, o::O, json_path::String)::Dict{String, Any}

    data = JSON.parsefile(json_path)
    ss   = data["state_space"]

    to_matrix(rows) = Matrix{Float64}(reduce(hcat, rows)')
    ss["A"]  = to_matrix(ss["A"])
    ss["B"]  = to_matrix(ss["B"])
    ss["E"]  = to_matrix(ss["E"])
    ss["C"]  = to_matrix(ss["C"])
    ss["x0"] = Vector{Float64}(ss["x0"])

    if o.continuous_dynamo
        ss["A"], ss["B"], ss["E"] = discretize_zoh(ss["A"], ss["B"], ss["E"], o.Δt)
    else
        o.Δt = Float64(ss["sampling_time"])
    end

    return data
end