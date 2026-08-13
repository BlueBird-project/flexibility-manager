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
    _sort_key(k::AbstractString)

Sort key for dict keys: purely-numeric keys (e.g. column indices like
`"2"`, `"10"`) sort numerically so `"2"` comes before `"10"`; all other
keys sort alphabetically. Returns a `Tuple` so both kinds can be compared
(numeric keys always sort before non-numeric ones within the same dict).
"""
function _sort_key(k::AbstractString)
    n = tryparse(Int, k)
    return n === nothing ? (1, 0, k) : (0, n, "")
end

"""
    _ordered(val)

Recursively convert `Dict`s into `NamedTuple`s with keys sorted via
`_sort_key` (alphabetically, except purely-numeric keys sort numerically).
`Dict` does not preserve insertion order, but `NamedTuple` does (and
`JSON3` serializes it in that order), so this gives a deterministic key
order at every nesting level when the result is serialized.
"""
_ordered(val) = val
_ordered(val::AbstractDict) =
    (; (Symbol(k) => _ordered(v) for (k, v) in sort(collect(val); by = p -> _sort_key(first(p))))...)
_ordered(val::AbstractVector) = [_ordered(v) for v in val]

"""
    write_outputs_to_file(opt_data::Dict{String,Any}; file="./data/outputs/output.json")

Write a parsed output dictionary to a JSON file.

`Dict` does not preserve insertion order, so the keys are recursively
sorted alphabetically (including nested dicts, e.g. per-timestep entries)
before serialization to get a deterministic key order in the output file.
"""
function write_outputs_to_file(opt_data::Dict{String,Any};
            file = joinpath(@__DIR__, "outputs", "oy.json"))
    sorted = _ordered(opt_data)
    mkpath(dirname(file))
    open(file, "w") do io
        JSON3.pretty(io, sorted)
        @info "Writing MPC control output in $(file)"
    end
    return nothing
end
