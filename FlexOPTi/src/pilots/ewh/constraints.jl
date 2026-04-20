"""
    montcada_build_constraints(; to_kelvin=true, T_low_celsius=15, T_high_celsius=25, p_low_kw=0.0, p_high_kw=2.0)

    - to_kelvin::Bool : convert to kelvin if true

Build EWH constraints, optionally overriding default values via keyword arguments.

Returns constraints as a NamedTuple with temperatures converted to Kelvin.
"""
# TODO : Possibility to overwrite the constraints from an API ?
# TODO : Different constraints per room ?
# TODO : Check the real values from the datasheet
function build_constraints(::Ewh; 
    to_kelvin::Bool       = true,
    T_fridge_low::Real    = 0.0, 
    T_fridge_high::Real   = 0.0, 
    T_freezer_low::Real   = 0.0, 
    T_freezer_high::Real  = 0.0, 
    SP_fridge_low::Real   = 0.0, 
    SP_fridge_high::Real  = 0.0, 
    SP_freezer_low::Real  = 0.0,
    SP_freezer_high::Real = 0.0,
    p_fridge_low::Real    = 0.0,
    p_fridge_high::Real   = 0.0,
    p_freezer_low::Real   = Inf,
    p_freezer_high::Real  = Inf,
)
    # Helper function remains internal
    _to_kelvin(x::Real) = x + KELVIN_OFFSET 
    
    constraints = Dict(
        :T_fridge_low    => to_kelvin ? _to_kelvin(T_fridge_low   ) : T_fridge_low   ,
        :T_fridge_high   => to_kelvin ? _to_kelvin(T_fridge_high  ) : T_fridge_high  ,
        :T_freezer_low   => to_kelvin ? _to_kelvin(T_freezer_low  ) : T_freezer_low  ,
        :T_freezer_high  => to_kelvin ? _to_kelvin(T_freezer_high ) : T_freezer_high ,
        :SP_fridge_low   => to_kelvin ? _to_kelvin(SP_fridge_low  ) : SP_fridge_low  ,
        :SP_fridge_high  => to_kelvin ? _to_kelvin(SP_fridge_high ) : SP_fridge_high ,
        :SP_freezer_low  => to_kelvin ? _to_kelvin(SP_freezer_low ) : SP_freezer_low ,
        :SP_freezer_high => to_kelvin ? _to_kelvin(SP_freezer_high) : SP_freezer_high,
        :p_fridge_low    => p_fridge_low  ,
        :p_fridge_high   => p_fridge_high ,
        :p_freezer_low   => p_freezer_low ,
        :p_freezer_high  => p_freezer_high,
    )

    return constraints
end # function 
