"""
    build_constraints(::Ewh; kwargs...) -> Dict{Symbol, Float64}

Build MPC constraints for an Electric Water Heater (EWH) device, grouped by zone
(dairy, finished products, meat, vegetables, freezer). All temperature parameters
are in °C and optionally converted to Kelvin.

# Keyword Arguments
| Parameter                             | Default | Description                               |
|---------------------------------------|---------|-------------------------------------------|
| `to_kelvin::Bool`                     | `true`  | Convert temperature values to Kelvin      |
| `T_fridge_diary_low/high`             | `0.0`   | Dairy fridge temperature bounds [°C]      |
| `T_fridge_finished_products_low/high` | `0.0`   | Finished products temperature bounds [°C] |
| `T_fridge_meat_low/high`              | `0.0`   | Meat fridge temperature bounds [°C]       |
| `T_fridge_vegetables_low/high`        | `0.0`   | Vegetables fridge temperature bounds [°C] |
| `T_freezer_low/high`                  | `0.0`   | Freezer temperature bounds [°C]           |
| `SP_fridge_diary_low/high`            | `0.0`   | Dairy fridge setpoint bounds [°C]         |
| `SP_fridge_finished_products_low/high`| `0.0`   | Finished products setpoint bounds [°C]    |
| `SP_fridge_meat_low/high`             | `0.0`   | Meat fridge setpoint bounds [°C]          |
| `SP_fridge_vegetables_low/high`       | `0.0`   | Vegetables fridge setpoint bounds [°C]    |
| `SP_freezer_low/high`                 | `0.0`   | Freezer setpoint bounds [°C]              |
| `p_fridge_diary_low/high`             | `0.0`   | Dairy fridge power bounds [kW]            |
| `p_fridge_finished_products_low/high` | `0.0`   | Finished products power bounds [kW]       |
| `p_fridge_meat_low/high`              | `0.0`   | Meat fridge power bounds [kW]             |
| `p_fridge_vegetables_low/high`        | `0.0`   | Vegetables fridge power bounds [kW]       |
| `p_freezer_low/high`                  | `0.0`   | Freezer power bounds [kW]                 |

# Returns
`Dict{Symbol, Float64}` mapping constraint names to their values.

# TODO
- Possibility to overwrite the constraints from an API?
- Different constraints per room?
- Check the real values from the datasheet
"""
function build_constraints(::Ewh;
    to_kelvin::Bool                            = false,   # digital twin states are in °C
    # --- Temperatures [°C] ---
    T_fridge_diary_low::Float64                = 2.0,
    T_fridge_diary_high::Float64               = 8.0,
    T_fridge_finished_products_low::Float64    = 2.0,
    T_fridge_finished_products_high::Float64   = 7.0,
    T_fridge_meat_low::Float64                 = 0.0,
    T_fridge_meat_high::Float64                = 9.0,
    T_fridge_vegetables_low::Float64           = 2.0,
    T_fridge_vegetables_high::Float64          = 7.0,
    T_freezer_low::Float64                     = -40.0,
    T_freezer_high::Float64                    = -18.0,
    # --- Setpoints [°C] ---
    SP_fridge_diary_low::Float64               = 2.0,
    SP_fridge_diary_high::Float64              = 8.0,
    SP_fridge_finished_products_low::Float64   = 2.0,
    SP_fridge_finished_products_high::Float64  = 7.0,
    SP_fridge_meat_low::Float64                = 2.0,
    SP_fridge_meat_high::Float64               = 5.0,
    SP_fridge_vegetables_low::Float64          = 2.0,
    SP_fridge_vegetables_high::Float64         = 7.0,
    SP_freezer_low::Float64                    = -40.0,
    SP_freezer_high::Float64                   = -18.0,
    # --- Power [kW] ---
    p1_low::Float64                            = 0.0, # Fridges
    p1_high::Float64                           = 5000.0, # Fridges
    p2_low::Float64                            = 0.0, # Fridges
    p2_high::Float64                           = 5000.0, # Fridges 
    p3_low::Float64                            = 0.0, # Freezer 
    p3_high::Float64                           = 5000.0, # Freezer
)

    _to_kelvin(x::Float64) = to_kelvin ? x + KELVIN_OFFSET : x

    return Dict{Symbol, Float64}(
        # --- Temperatures ---
        :T_fridge_diary_low                => _to_kelvin(T_fridge_diary_low),
        :T_fridge_diary_high               => _to_kelvin(T_fridge_diary_high),
        :T_fridge_finished_products_low    => _to_kelvin(T_fridge_finished_products_low),
        :T_fridge_finished_products_high   => _to_kelvin(T_fridge_finished_products_high),
        :T_fridge_meat_low                 => _to_kelvin(T_fridge_meat_low),
        :T_fridge_meat_high                => _to_kelvin(T_fridge_meat_high),
        :T_fridge_vegetables_low           => _to_kelvin(T_fridge_vegetables_low),
        :T_fridge_vegetables_high          => _to_kelvin(T_fridge_vegetables_high),
        :T_freezer_low                     => _to_kelvin(T_freezer_low),
        :T_freezer_high                    => _to_kelvin(T_freezer_high),
        # --- Setpoints ---
        :SP_fridge_diary_low               => _to_kelvin(SP_fridge_diary_low),
        :SP_fridge_diary_high              => _to_kelvin(SP_fridge_diary_high),
        :SP_fridge_finished_products_low   => _to_kelvin(SP_fridge_finished_products_low),
        :SP_fridge_finished_products_high  => _to_kelvin(SP_fridge_finished_products_high),
        :SP_fridge_meat_low                => _to_kelvin(SP_fridge_meat_low),
        :SP_fridge_meat_high               => _to_kelvin(SP_fridge_meat_high),
        :SP_fridge_vegetables_low          => _to_kelvin(SP_fridge_vegetables_low),
        :SP_fridge_vegetables_high         => _to_kelvin(SP_fridge_vegetables_high),
        :SP_freezer_low                    => _to_kelvin(SP_freezer_low),
        :SP_freezer_high                   => _to_kelvin(SP_freezer_high),
        # --- Power ---
        :p1_low                            => p1_low,
        :p1_high                           => p1_high,
        :p2_low                            => p2_low,
        :p2_high                           => p2_high,
        :p3_low                            => p3_low,
        :p3_high                           => p3_high,
    )
end