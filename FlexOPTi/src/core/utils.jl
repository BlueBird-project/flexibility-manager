"""
    reorder_labels(labels, values)

Reorders a list of labels and their associated values according to the numeric indices in the labels.

# Arguments
- `labels::Vector{String}` — Labels in the form `name_index[_lag]`.
- `values::Vector` — Values associated with each label.

# Returns
- `labels::Vector{String}` — Reordered labels.
- `values::Vector` — Values reordered according to numeric indices extracted from labels.

# Description
Sorts labels by the numeric index and optional lag, and reorders the corresponding values. Useful for maintaining consistent column/row ordering in MPC matrices.
"""
function reorder_labels(labels, values)

    function parse_generic_label(label)
        m = match(r"(.+?)_(\d+)(?:_l(\d+))?$", label)
        mainnum = parse(Int, m.captures[2])
        level   = isnothing(m.captures[3]) ? 0 : parse(Int, m.captures[3])
        return mainnum, level
    end

    order = sortperm(labels; by = lbl -> parse_generic_label(lbl))
    return labels[order], values[order]
end






function find_lattest_datetime(list::AbstractArray)
    
    index    = length(list[end])
    datetime = list[end]["start"]

    return datetime, index
end