# const KELVIN_OFFSET = 273.15

"""
    plot_T_SP(oy; rooms=1:1)

Plot temperature (T) and setpoint (SP) over time.

- Top subplot: T
- Bottom subplot: SP
- If multiple rooms are given, they are overlaid in each subplot.
"""
function plot_state(oy::Dict{Symbol, Any}; rooms=1:1)

    # Extract state and action
    T  = oy[:T]  .- KELVIN_OFFSET
    SP = oy[:SP] .- KELVIN_OFFSET

    # Normalize rooms input
    rooms = isa(rooms, Integer) ? [rooms] : collect(rooms)

    time = 1:size(T, 1)

    # Create 2 subplots (T on top, SP below)
    plt = plot(
        layout = (2, 1),
        size = (900, 500),
        link = :x
    )

    # ---- Temperature subplot ----
    for r in rooms
        plot!(
            plt[1],
            time, T[:, r],
            label = "Room $r",
            lw = 2
        )
    end
    ylabel!(plt[1], "Temperature [K]")
    title!(plt[1], "Temperature")
    plot!(plt[1], grid = true)

    # ---- Setpoint subplot ----
    for r in rooms
        plot!(
            plt[2],
            time, SP[:, r],
            label = "Room $r",
            lw = 2,
            ls = :dash
        )
    end
    ylabel!(plt[2], "Setpoint [K]")
    title!(plt[2], "Setpoint")
    plot!(plt[2], grid = true)

    xlabel!(plt[2], "Time step")

    return plt
end
