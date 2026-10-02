"""
    montcada_build_constraints(; to_kelvin=true, T_low_celsius=15, T_high_celsius=25, p_low_kw=0.0, p_high_kw=2.0)

    - to_kelvin::Bool : convert to kelvin if true

Build Montcada constraints, optionally overriding default values via keyword arguments.

Returns constraints as a NamedTuple with temperatures converted to Kelvin.
"""
# TODO : Possibility to overwrite the constraints from an API ?
# TODO : Different constraints per room ?
# TODO : Check the real values from the datasheet
function build_constraints(::Montcada; 
    to_kelvin::Bool = true,
    T_low::Real   = 21.0, 
    T_high::Real  = 30.0,
    p_low::Real   = 0.0, 
    p_high::Real  = Inf, 
    SP_low::Real  = 19.0, 
    SP_high::Real = 30.0
)
    # Helper function remains internal
    _to_kelvin(x::Real) = x + KELVIN_OFFSET 
    
    constraints = Dict(
        :T_low   => to_kelvin ? _to_kelvin(T_low ) : T_low , 
        :T_high  => to_kelvin ? _to_kelvin(T_high) : T_high, 
        :p_low   => p_low , 
        :p_high  => p_high,
        :SP_low  => to_kelvin ? _to_kelvin(SP_low ) : SP_low,
        :SP_high => to_kelvin ? _to_kelvin(SP_high) : SP_high
    )

    return constraints
end # function 


