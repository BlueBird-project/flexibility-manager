# =============================================================================
#  sim_plot.jl — Plot closed-loop simulation results from sim_demo.jl
#
#  Reads data/EWH/outputs/sim_demo.jld2 (produced by sim_demo.jl).
#
#  Layout (4 stacked subplots, shared x-axis = hours from t_start):
#    1. Electricity price (step plot)
#    2. Fridge room air temperatures (rooms 1–4)
#    3. Freezer air temperature
#    4. Compressor powers (all 5 inputs)
# =============================================================================

using JLD2, Plots, Printf, Dates, TimeZones

const ROOT    = joinpath(@__DIR__, "..", "..")
const OUT_DIR = joinpath(ROOT, "data", "EWH", "outputs")

# ── Load saved simulation data ─────────────────────────────────────────────

results_file = joinpath(OUT_DIR, "sim_demo.jld2")
isfile(results_file) || error("No results file found at $results_file — run sim_demo.jl first.")

d = load(results_file)

X_true        = d["X_true"]
U_closed      = d["U_closed"]
P_series      = d["P_series"]
door_schedule = d["door_schedule"]
state_names   = d["state_names"]
input_names   = d["input_names"]

Δt_h_   = d["Δt_h"]
N_sim_  = d["N_sim"]
t_start = d["t_start"]
t_end   = d["t_end"]

# State indexing (matches cold_room_model_continuous.json):
#   air-temp states at odd indices: 1,3,5,7 = fridges 1..4, 9 = freezer
#   compressor inputs: 1..4 = fridges, 5 = freezer
const fridge_air_si = [1, 3, 5, 7]
const freezer_si    = 9
const fridge_u      = [1, 2, 3, 4]
const freezer_u     = 5

_short(s::AbstractString) = replace(s, "T_air_fridge_" => "", "T_air_" => "",
                                       "P_compressor_fridge_" => "", "P_compressor_" => "")

# ── Time axes (absolute DateTime) ──────────────────────────────────────────

t0_dt      = DateTime(ZonedDateTime(t_start), UTC)
Δt_sec     = round(Int, Δt_h_ * 3600)
step_ms    = Millisecond(Δt_sec * 1000)
time_sim   = [t0_dt + (i-1) * step_ms for i in 1:N_sim_]
time_state = [t0_dt + (i-1) * step_ms for i in 1:(N_sim_ + 1)]
x_lim      = (time_state[1], time_state[end])

# Tick every 8 h, anchored to 00:00 of t_start's day
tick_start = DateTime(Date(t0_dt))
xticks_dt  = filter(t -> x_lim[1] ≤ t ≤ x_lim[2],
                    collect(tick_start:Hour(8):time_state[end]))
xticks_lbl = Dates.format.(xticks_dt, "mm-dd HH:MM")

total_energy_kwh = sum(U_closed) * Δt_h_ / 1000
println(@sprintf("Total compressor energy: %.1f kWh", total_energy_kwh))
println("Window: $t_start → $t_end")

common = (framestyle = :box, xlims = x_lim, grid = true, gridalpha = 0.3,
          xticks = (xticks_dt, xticks_lbl), xrotation = 30,
          background_color_legend = :white, foreground_color_legend = :black,
          legend_font_pointsize = 8)

# ── 1. Price (step plot) ───────────────────────────────────────────────────

p_price = plot(time_sim, P_series .* 1000;     # EUR/Wh → EUR/kWh
               seriestype = :steppost,
               color = :black, lw = 1.8,
               ylabel = "Price [€/kWh]",
               label  = "Buy price",
               title  = "Electricity price (day-ahead)",
               legend = :topright,
               common...)

# ── 2. Fridge room air temperatures ────────────────────────────────────────

p_fridge = plot(; ylabel = "Air temp [°C]",
                  title  = "Fridge rooms (air temperature)",
                  legend = :topright, common...)
for (i, si) in enumerate(fridge_air_si)
    lbl = isempty(state_names) ? "fridge $i" : _short(state_names[si])
    plot!(p_fridge, time_state, X_true[si, :]; lw = 1.5, label = lbl)
end

# ── 3. Freezer air temperature ─────────────────────────────────────────────

p_freezer = plot(time_state, X_true[freezer_si, :];
                 color = :steelblue, lw = 2,
                 ylabel = "Air temp [°C]",
                 title  = "Freezer (air temperature)",
                 label  = isempty(state_names) ? "freezer" : _short(state_names[freezer_si]),
                 legend = :topright,
                 common...)
hline!(p_freezer, [-40.0, -18.0]; color = :steelblue, ls = :dot, alpha = 0.7,
       label = "T bounds")

# ── 4. All compressor powers ───────────────────────────────────────────────

p_power = plot(; ylabel = "Power [W]",
                 xlabel = "Time (UTC)",
                 title  = "Room Powers",
                 legend = :topright, common...)
for r in 1:size(U_closed, 1)
    lbl = isempty(input_names) ? "u$r" : _short(input_names[r])
    plot!(p_power, time_sim, U_closed[r, :];
          seriestype = :steppost, lw = 1.4, label = lbl)
end

# ── Combine and save ───────────────────────────────────────────────────────

combined = plot(p_price, p_fridge, p_freezer, p_power;
                layout = (4, 1), size = (1100, 1100),
                left_margin = 8Plots.mm, right_margin = 8Plots.mm,
                bottom_margin = 4Plots.mm, top_margin = 3Plots.mm,
                link = :x)

out_file = joinpath(OUT_DIR, "sim_demo.png")
mkpath(OUT_DIR)
savefig(combined, out_file)
println("Plot saved to $out_file")
