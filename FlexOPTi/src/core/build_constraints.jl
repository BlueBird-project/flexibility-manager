function build_constraints(pilot::AbstractBuilding)::Dict{Symbol, Any}

    @warn "Default contraint method called. Return unconstrained problem."

    return Dict()

end

"""
    safety_limits(pilot) -> Dict{Symbol, Tuple{Float64, Float64}}

Hard safety envelope of a pilot, in °C: `constraint => (min, max)`.
No caller can set a constraint outside it. Pilots extend this method;
the fallback has no limits.
"""
safety_limits(::AbstractBuilding) = Dict{Symbol, Tuple{Float64, Float64}}()

"""
    enforce_safety_limits!(pilot, constraints)

Clamp, in place, every constraint outside the pilot's `safety_limits` to the
nearest limit, with a warning. Then throw an `ArgumentError` if a
`<name>_low` bound is above its `<name>_high`: clamping cannot tell which of
the two was meant. Call it on the constraints in °C, before converting them
to Kelvin.
"""
function enforce_safety_limits!(pilot::AbstractBuilding, constraints::AbstractDict)
    name = nameof(typeof(pilot))

    for (k, (lo, hi)) in safety_limits(pilot)
        v = constraints[k]
        lo ≤ v ≤ hi && continue
        constraints[k] = clamp(v, lo, hi)
        @warn "$name: $k = $v °C is outside the safety range [$lo, $hi] °C; " *
              "clamped to $(constraints[k]) °C."
    end

    for (k, v) in constraints
        s = string(k)
        endswith(s, "_low") || continue
        k_high = Symbol(chop(s; tail = 4) * "_high")
        haskey(constraints, k_high) || continue
        v ≤ constraints[k_high] || throw(ArgumentError(
            "$name: $k = $v is above $k_high = $(constraints[k_high])."))
    end

    return nothing
end
