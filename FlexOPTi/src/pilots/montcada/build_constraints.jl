"""
    build_constraints(::Montcada; to_kelvin=true, T_low=21.0, T_high=30.0,
                      p_low=0.0, p_high=Inf, SP_low=19.0, SP_high=30.0)

    - to_kelvin::Bool : convert temperatures to kelvin if true
    - T_low, T_high   : room temperature band [°C]
    - p_low, p_high   : total HVAC power bounds
    - SP_low, SP_high : HVAC setpoint bounds [°C]

Build Montcada constraints, optionally overriding default values via keyword
arguments. Every keyword can also be passed directly to `optimize`.
Each value is a scalar applied to all rooms over the whole horizon.
Values outside `safety_limits(::Montcada)` (15–30 °C) are clamped to the
limit with a warning; a `_low` above its `_high` throws an `ArgumentError`.

Returns constraints as a Dict with temperatures converted to Kelvin.
"""
# TODO : Possibility to overwrite the constraints from an API ?
# TODO : Different constraints per room / per time step ?
# TODO : Check the real values from the datasheet
function build_constraints(pilot::Montcada;
    to_kelvin::Bool = true,
    T_low::Real   = 21.0,
    T_high::Real  = 30.0,
    p_low::Real   = 0.0,
    p_high::Real  = Inf,
    SP_low::Real  = 19.0,
    SP_high::Real = 30.0
)
    constraints = Dict{Symbol, Any}(
        :T_low   => T_low ,
        :T_high  => T_high,
        :p_low   => p_low ,
        :p_high  => p_high,
        :SP_low  => SP_low,
        :SP_high => SP_high
    )

    # Clamped to the safety limits in °C, before any unit conversion
    enforce_safety_limits!(pilot, constraints)

    if to_kelvin
        for k in (:T_low, :T_high, :SP_low, :SP_high)
            constraints[k] += KELVIN_OFFSET
        end
    end

    return constraints
end # function

# Safety envelope [°C]: comfort band and setpoints must stay within 15–30 °C.
safety_limits(::Montcada) = Dict(
    :T_low   => (15.0, 30.0),
    :T_high  => (15.0, 30.0),
    :SP_low  => (15.0, 30.0),
    :SP_high => (15.0, 30.0),
)


