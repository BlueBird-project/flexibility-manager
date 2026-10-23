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
function build_constraints(pilot::Ewh;
    to_kelvin::Bool                            = false,   # digital twin states are in °C
    # --- Temperatures [°C] ---
    T_fridge_diary_low::Real                   = 2.0,
    T_fridge_diary_high::Real                  = 8.0,
    T_fridge_finished_products_low::Real       = 2.0,
    T_fridge_finished_products_high::Real      = 7.0,
    T_fridge_meat_low::Real                    = 0.0,
    T_fridge_meat_high::Real                   = 9.0,
    T_fridge_vegetables_low::Real              = 2.0,
    T_fridge_vegetables_high::Real             = 7.0,
    T_freezer_low::Real                        = -40.0,
    T_freezer_high::Real                       = -18.0,
    # --- Setpoints [°C] ---
    SP_fridge_diary_low::Real                  = 2.0,
    SP_fridge_diary_high::Real                 = 8.0,
    SP_fridge_finished_products_low::Real      = 2.0,
    SP_fridge_finished_products_high::Real     = 7.0,
    SP_fridge_meat_low::Real                   = 2.0,
    SP_fridge_meat_high::Real                  = 5.0,
    SP_fridge_vegetables_low::Real             = 2.0,
    SP_fridge_vegetables_high::Real            = 7.0,
    SP_freezer_low::Real                       = -40.0,
    SP_freezer_high::Real                      = -18.0,
    # --- Power [kW] ---
    p1_low::Real                               = 0.0, # Fridges
    p1_high::Real                              = 3300.0, # Fridges
    p2_low::Real                               = 0.0, # Fridges
    p2_high::Real                              = 2900.0, # Fridges 
    p3_low::Real                               = 0.0, # Freezer 
    p3_high::Real                              = 2700.0, # Freezer
)

    constraints = Dict{Symbol, Float64}(
        # --- Temperatures ---
        :T_fridge_diary_low                => T_fridge_diary_low,
        :T_fridge_diary_high               => T_fridge_diary_high,
        :T_fridge_finished_products_low    => T_fridge_finished_products_low,
        :T_fridge_finished_products_high   => T_fridge_finished_products_high,
        :T_fridge_meat_low                 => T_fridge_meat_low,
        :T_fridge_meat_high                => T_fridge_meat_high,
        :T_fridge_vegetables_low           => T_fridge_vegetables_low,
        :T_fridge_vegetables_high          => T_fridge_vegetables_high,
        :T_freezer_low                     => T_freezer_low,
        :T_freezer_high                    => T_freezer_high,
        # --- Setpoints ---
        :SP_fridge_diary_low               => SP_fridge_diary_low,
        :SP_fridge_diary_high              => SP_fridge_diary_high,
        :SP_fridge_finished_products_low   => SP_fridge_finished_products_low,
        :SP_fridge_finished_products_high  => SP_fridge_finished_products_high,
        :SP_fridge_meat_low                => SP_fridge_meat_low,
        :SP_fridge_meat_high               => SP_fridge_meat_high,
        :SP_fridge_vegetables_low          => SP_fridge_vegetables_low,
        :SP_fridge_vegetables_high         => SP_fridge_vegetables_high,
        :SP_freezer_low                    => SP_freezer_low,
        :SP_freezer_high                   => SP_freezer_high,
        # --- Power ---
        :p1_low                            => p1_low,
        :p1_high                           => p1_high,
        :p2_low                            => p2_low,
        :p2_high                           => p2_high,
        :p3_low                            => p3_low,
        :p3_high                           => p3_high,
    )

    # Clamped to the safety limits in °C, before any unit conversion
    enforce_safety_limits!(pilot, constraints)

    if to_kelvin
        for k in keys(constraints)
            startswith(string(k), r"T_|SP_") && (constraints[k] += KELVIN_OFFSET)
        end
    end

    return constraints
end

# Safety envelope [°C]: a frozen-goods freezer must stay at or below -18 °C.
# TODO : Fridge limits (food-safety range) still to be confirmed with the site.
safety_limits(::Ewh) = Dict(
    :T_freezer_low   => (-Inf, -18.0),
    :T_freezer_high  => (-Inf, -18.0),
    :SP_freezer_low  => (-Inf, -18.0),
    :SP_freezer_high => (-Inf, -18.0),
)