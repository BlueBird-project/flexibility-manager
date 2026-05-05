# =============================================================================
#  mpc_plot.jl — Re-plot MPC results from saved data (no re-solve needed)
#
#  Run after mpc_base.jl has produced data/EWH/outputs/mpc_results.jld2.
#  Edit only this file to tweak colours, labels, layout, etc.
# =============================================================================

# using Pkg
# Pkg.activate(joinpath(@__DIR__, "..", ".."))

using JLD2, Plots, Printf

const ROOT    = joinpath(@__DIR__, "..", "..")
const OUT_DIR = joinpath(ROOT, "data", "EWH", "outputs")

# ── Load saved simulation data ─────────────────────────────────────────────

results_file = joinpath(OUT_DIR, "mpc_results.jld2")
isfile(results_file) || error("No results file found at $results_file — run mpc_base.jl first.")

d = load(results_file)

X_true      = d["X_true"]
U_closed    = d["U_closed"]
X_pred_last = d["X_pred_last"]
U_pred_last = d["U_pred_last"]
price       = d["price"]
door_schedule = d["door_schedule"]
σ_freezer   = d["σ_freezer"]

Δt_h_      = d["Δt_h"]
Hu_        = d["Hu"]
N_sim_     = d["N_sim"]
N_price_   = d["N_price"]
freezer_r_  = d["freezer_r"]
freezer_si_ = d["freezer_si"]

# ── Build time axes ────────────────────────────────────────────────────────

x_lim     = (0.0, N_price_ * Δt_h_)
sim_end_h = N_sim_ * Δt_h_

time_sim   = collect(range(0.0, step = Δt_h_, length = N_sim_))
time_state = collect(range(0.0, step = Δt_h_, length = N_sim_ + 1))
time_price = collect(range(0.0, step = Δt_h_, length = N_price_))
time_pred  = collect(range(sim_end_h, step = Δt_h_, length = Hu_ + 1))

# ── Signals ────────────────────────────────────────────────────────────────

T_fz_pred = vcat(X_true[freezer_si_, N_sim_+1], X_pred_last[freezer_si_, :])
P_fz_pred = vcat(U_pred_last[freezer_r_, 1],    U_pred_last[freezer_r_, :])

T_fz = X_true[freezer_si_, :]
P_fz = U_closed[freezer_r_, :]

door_times = time_sim[door_schedule[freezer_r_, :] .> 0]

total_energy_kwh = sum(U_closed) * Δt_h_ / 1000
println(@sprintf("Total compressor energy: %.1f kWh", total_energy_kwh))

# ── Plot helpers ───────────────────────────────────────────────────────────

common = (framestyle = :box, xlims = x_lim,
          grid = true, gridalpha = 0.3)

function add_door_vlines!(p)
    for (i, td) in enumerate(door_times)
        vline!(p, [td]; color = :firebrick, ls = :solid, lw = 1.5,
               label = i == 1 ? "Door opening (freezer)" : "")
    end
end

# ── Subplot 1: electricity price ───────────────────────────────────────────

p_price = plot(time_price, price;
               ylabel = "Price [€/kWh]", label = "Buy price",
               color = :black, lw = 2, legend = :topright,
               background_color_legend = :white,
               foreground_color_legend = :black,
               title = "Electricity price (EU day-ahead, 15 min blocks)",
               common...)
vspan!(p_price, [0.0, sim_end_h];       alpha = 0.08, color = :steelblue, label = "Simulation")
vspan!(p_price, [sim_end_h, x_lim[2]]; alpha = 0.06, color = :grey,      label = "MPC lookahead")
vline!(p_price, [sim_end_h]; color = :black, ls = :dot, lw = 1.2, label = "")
add_door_vlines!(p_price)

# ── Subplot 2: freezer temp (left) + compressor power (right) ─────────────

p_temp = plot(time_state, T_fz;
              ribbon = σ_freezer, fillalpha = 0.20,
              color = :steelblue, lw = 2,
              ylabel = "Freezer air temperature [°C]",
              label = "", legend = false,
              title = "Freezer — temperature & compressor power",
              xlabel = "Time from 8 am [h]",
              common...)
plot!(p_temp, time_pred, T_fz_pred;
      color = :grey, lw = 1.5, ls = :dash, label = "")
hline!(p_temp, [-40.0, -18.0];
       color = :steelblue, ls = :dot, alpha = 0.7, label = "")
vspan!(p_temp, [sim_end_h, x_lim[2]]; alpha = 0.06, color = :grey, label = "")
vline!(p_temp, [sim_end_h]; color = :black, ls = :dot, lw = 1.2, label = "")
add_door_vlines!(p_temp)

p_pow = twinx(p_temp)
plot!(p_pow, time_sim,  P_fz;
      color = :darkorange, lw = 1.5,
      ylabel = "Compressor power [W]",
      label = "Power (right axis)",
      legend = :bottomright,
      background_color_legend = :white,
      foreground_color_legend = :black,
      legend_font_pointsize = 9,
      xlims = x_lim, framestyle = :box)
plot!(p_pow, time_pred, P_fz_pred;
      color = :darkorange, lw = 1.2, ls = :dash,
      label = "Power MPC prediction", xlims = x_lim)

# Dummy series on p_pow (top layer) for temperature labels
plot!(p_pow, [NaN], [NaN]; color = :steelblue, lw = 2,               label = "T_air ±1σ (true)")
plot!(p_pow, [NaN], [NaN]; color = :grey,      lw = 1.5, ls = :dash, label = "T_air MPC prediction")
plot!(p_pow, [NaN], [NaN]; color = :steelblue, lw = 1.5, ls = :dot,  label = "T bounds")
plot!(p_pow, [NaN], [NaN]; color = :firebrick, lw = 1.5,             label = "Door opening (freezer)")

# ── Combine and save ───────────────────────────────────────────────────────

combined = plot(p_price, p_temp;
                layout = (2, 1), size = (1000, 750),
                left_margin = 6Plots.mm, right_margin = 12Plots.mm,
                bottom_margin = 5Plots.mm, top_margin = 3Plots.mm,
                link = :x)

out_file = joinpath(OUT_DIR, "mpc_base.png")
mkpath(OUT_DIR)
savefig(combined, out_file)
println("Plot saved to $out_file")
