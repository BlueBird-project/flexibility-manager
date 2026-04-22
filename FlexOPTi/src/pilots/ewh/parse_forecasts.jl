"""
    parse_forecasts(::Ewh, o, forecasts_file) -> Dict{String, Any}

Build a dummy door-opening disturbance forecast for the EWH pilot.
Until a real forecasting service is wired up, door openings are simulated
stochastically: 11–17 openings per room per day, uniformly distributed in
the 06:00–23:00 window, each lasting 1 minute with d = 4500 [W] heat load.

The returned dict contains:
  "door_openings" => Matrix{Float64}(n_rooms × Hu)  — disturbance per step per room
  "ToU_buy"       => Vector{Float64}(Hu)             — placeholder flat price [€/kWh]
  "ToU_sell"      => Vector{Float64}(Hu)             — placeholder flat sell price [€/kWh]
"""
function parse_forecasts(::Ewh, o::O, forecasts_file::String)::Dict{String, Any}

    Hu              = o.Hu
    t0              = o.compute_datetime
    n_rooms         = 5          # 4 fridges + 1 freezer
    door_open_load  = 4500.0     # [W] heat input when door is open
    openings_low    = 11         # min daily door openings per room
    openings_high   = 17         # max daily door openings per room
    active_start    = 6          # hour: 06:00
    active_end      = 23         # hour: 23:00 (exclusive)

    # Build minute-resolution open/close schedule per room for enough days to cover Hu steps
    # then sample at the model time step (Δt may be > 1 min, so we average)
    n_days = max(2, ceil(Int, Hu / (24 * 60)) + 1)

    # Full minute-resolution schedule: n_rooms × (n_days * 24 * 60)
    n_minutes = n_days * 24 * 60
    schedule  = zeros(Float64, n_rooms, n_minutes)

    for r in 1:n_rooms
        for d in 0:n_days-1
            day_start     = d * 24 * 60
            start_min     = day_start + active_start * 60
            end_min       = day_start + active_end   * 60   # exclusive
            possible      = collect(start_min:end_min-1)
            n_open        = rand(openings_low:openings_high)
            chosen        = randperm(length(possible))[1:n_open]
            for m in chosen
                schedule[r, m + 1] = door_open_load   # 1-indexed
            end
        end
    end

    # Aggregate from minute resolution to model time steps.
    # Δt is read from the digital twin at solve time; here we assume 1 step = 1 minute.
    # build_batch will use this matrix directly, so keep it at minute resolution
    # and let the caller slice the first Hu columns.
    door_openings = schedule[:, 1:min(Hu, n_minutes)]

    # Pad with zeros if Hu > generated minutes (shouldn't happen)
    if size(door_openings, 2) < Hu
        door_openings = hcat(door_openings,
                             zeros(Float64, n_rooms, Hu - size(door_openings, 2)))
    end

    return Dict{String, Any}(
        "door_openings" => door_openings,   # [n_rooms × Hu]
        "ToU_buy"       => ones(Float64, Hu),
        "ToU_sell"      => zeros(Float64, Hu),
    )
end
