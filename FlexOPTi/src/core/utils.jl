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
function reorder_labels_old(labels, values)

    function parse_generic_label(label)
        m = match(r"(.+?)_(\d+)(?:_l(\d+))?$", label)
        mainnum = parse(Int, m.captures[2])
        level   = isnothing(m.captures[3]) ? 0 : parse(Int, m.captures[3])
        return mainnum, level
    end

    order = sortperm(labels; by = lbl -> parse_generic_label(lbl))

    if isnothing(value)
        return labels[order], nothing
    else
        return labels[order], values[order]
    end
end


function reorder_labels(labels, values)

    function parse_generic_label(label)
        # Match "Name_RoomNumber_lLag"
        # Example: "TempSP_98_l1" -> Name="TempSP", Room=98, Lag=1
        m = match(r"(.+?)_(\d+)(?:_l(\d+))?$", label)
        
        # If no lag is found (e.g., "AmbTemp_98"), it's Lag 0
        room_num = parse(Int, m.captures[2])
        lag      = isnothing(m.captures[3]) ? 0 : parse(Int, m.captures[3])
        
        # PRIMARY SORT: Lag (0, 1, 2...)
        # SECONDARY SORT: Room Number (1, 2, 3...)
        return (lag, room_num)
    end

    order = sortperm(labels; by = lbl -> parse_generic_label(lbl))

    if isnothing(values)
        return labels[order], nothing
    else
        return labels[order], values[order]
    end
end


"""
    find_index_from_datetime(
        list::AbstractVector,
        datetime::Union{String, ZonedDateTime, Nothing}
    ) -> Union{Int, Missing}

Finds the index of an entry whose `"start"` datetime matches the given value.

Each element of `list` is expected to be indexable with `"start"` and to
contain a datetime string compatible with `ZonedDateTime`.

If `datetime` is:
- `String` — it is parsed as a `ZonedDateTime`.
- `Nothing` — the last index of the list is returned.

If no matching datetime is found, `missing` is returned.

# Arguments
- `list::AbstractVector` — Collection of entries containing `"start"` fields.
- `datetime::Union{String, ZonedDateTime, Nothing}` — Datetime to search for.

# Returns
- `Int` — Index of the matching entry.
- `Missing` — If no match is found.

# Examples
  schedule = [
      Dict("start" => "2024-01-01T00:00:00+01:00"),
      Dict("start" => "2024-01-01T01:00:00+01:00")
  ]
  
  find_index_from_datetime(schedule, "2024-01-01T01:00:00+01:00") # 2
  find_index_from_datetime(schedule, nothing)                     # 2
  find_index_from_datetime(schedule, "2024-01-02T00:00:00+01:00") # missing
"""
function find_index_from_datetime(list::AbstractVector, datetime::Union{String, ZonedDateTime, Nothing})::Union{Int, Missing}

    if datetime isa String
        datetime = ZonedDateTime(datetime)
    end

    # No specification return the last index
    if datetime === nothing 
        return length(list)
    end

    # Otherwise search for closest match
    if isempty(list)
        return missing
    end
    
    # If we have a datetime, find the closest match
    closest_index = 1
    smallest_diff = typemax(Float64)
    
    for (i, entry) in enumerate(list)
        start_str = entry["start"]                          
        compute_datetime  = ZonedDateTime(start_str)                
        
        # Calculate absolute time difference
        diff = abs((compute_datetime - datetime) / Second(1))
        
        if diff < smallest_diff
            smallest_diff = diff
            closest_index = i
        end
        
        # Early exit if we find exact match
        if diff == 0
            return i
        end
    end

    t_warn = 30*60
    if smallest_diff > t_warn 
        @warn "The date used for optimization is more than $(t_warn/60) minutes off..."
    end
    
    return closest_index
end





function find_lattest_datetime(list::AbstractArray)
    
    index    = length(list[end])
    datetime = list[end]["start"]

    return datetime, index
end