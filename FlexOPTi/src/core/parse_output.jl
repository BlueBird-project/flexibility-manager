"""
    parse_OPT_output(::AbstractBuilding, oy; only_next_step::Bool=false) -> Dict{String,Any}

Default fallback: dump everything in `oy` as-is, skipping internal keys `:o` and `:ox`.
Values are converted to strings if not directly JSON-serializable.
Pilot-specific methods override this with proper key renaming and units.
"""
function parse_OPT_output(::AbstractBuilding, oy; only_next_step::Bool=false)
    result = Dict{String,Any}()
    for (key, val) in oy
        key in (:o, :ox) && continue
        result[string(key)] = val
    end
    return result
end

"""
    write_outputs_to_file(opt_data::Dict{String,Any}; file="./data/outputs/output.json")

Write a parsed output dictionary to a JSON file.
"""
function write_outputs_to_file(opt_data::Dict{String,Any};
            file = "./data/outputs/output.json")
    open(file, "w") do io
        JSON3.pretty(io, opt_data)
    end
    return nothing
end
