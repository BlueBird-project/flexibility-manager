## Simulate the EWH cold-room linear state-space model for 100 time steps
##
##   x[k+1] = A*x[k] + B*u[k] + E*d[k]
##   y[k]   = C*x[k]
##
## System dimensions (from cold_room_model.json):
##   nx = 10 states  : [T_air, T_prod] pairs for dairy, finishedProducts, meat, vegetables, freezer
##   nu = 5  inputs  : P_compressor for each of the 5 zones [W]
##   nd = 6  disturbances : T_ambient [degC], Q_door for each of the 5 zones [W]
##   ny = 5  outputs : T_air for each zone [degC]
##
## --- INITIAL STATE (x0 from digital twin) ---
##   T_air_fridge_dairy              =  3.0 degC
##   T_prod_fridge_dairy             =  3.0 degC
##   T_air_fridge_finishedProducts   =  4.0 degC
##   T_prod_fridge_finishedProducts  =  4.0 degC
##   T_air_fridge_meat               =  3.0 degC
##   T_prod_fridge_meat              =  3.0 degC
##   T_air_fridge_vegetables         =  5.0 degC
##   T_prod_fridge_vegetables        =  5.0 degC
##   T_air_freezer                   = -19.0 degC
##   T_prod_freezer                  = -19.0 degC
##
## --- INPUTS u [W] ---
##   Steps from 0 W to 2500 W at the midpoint (k = 51)
##
## --- CONSTANT DISTURBANCES d ---
##   T_ambient        = 20.0 degC
##   Q_door_dairy     =  0.0 W   (no door openings)
##   Q_door_finished  =  0.0 W
##   Q_door_meat      =  0.0 W
##   Q_door_vegetables=  0.0 W
##   Q_door_freezer   =  0.0 W

import Pkg
Pkg.activate(".")

using JSON
using LinearAlgebra
using Printf
using Plots
using Infiltrator

## Load model
json_path = joinpath(pwd(), "data", "EWH", "inputs", "cold_room_model.json")
data = JSON.parsefile(json_path)
ss   = data["state_space"]

to_matrix(rows) = Matrix{Float64}(reduce(hcat, rows)')

A  = to_matrix(ss["A"])
B  = to_matrix(ss["B"])
E  = to_matrix(ss["E"])
C  = to_matrix(ss["C"])
x0 = Vector{Float64}(ss["x0"])

state_names       = ss["state_names"]
input_names       = ss["input_names"]
disturbance_names = ss["disturbance_names"]
output_names      = ss["output_names"]

nx = ss["nx"]
nu = ss["nu"]
nd = ss["nd"]
ny = ss["ny"]
N  = 100  # simulation horizon [timesteps]
Δt = ss["sampling_time"]  # 60 s

## Disturbance vector (constant)
d = vcat(fill(20.0, N)', zeros(nd - 1, N))  # T_ambient = 20 degC, no door openings
d[3,20] = 4500
d[3,60] = 4500
d[4,35] = 4500

## Time-varying compressor power [W]
# Zero for the first half, 2500 W for the second half (step at k = N÷2 + 1)
U = zeros(N, nu)
U[(N÷2 + 1):end, begin:end-1] .= 700.0
U[(N÷2 + 1):end, 3          ] .= 1000.0
U[(N÷2 + 1):end, 4          ] .= 1200.0
U[(N÷2 + 1):end, end        ] .= 2700.0


## Simulate
X = zeros(N + 1, nx)
Y = zeros(N,     ny)
X[1, :] = x0

for k in 1:N
    X[k+1, :] = A * X[k, :] + B * U[k, :] + E * d[:,k]
    Y[k, :]   = C * X[k, :]
end

## Print summary
println("=" ^ 60)
println("Simulation: $(N) steps  |  Δt = $(Δt) s  |  total = $(N * Δt / 60) min")
println()
println("Inputs (time-varying square wave) [W]  — min / max:")
for i in 1:nu
    @printf("  %-45s = %.0f / %.0f W\n", input_names[i], minimum(U[:, i]), maximum(U[:, i]))
end
println()
println("Disturbances (constant):")
@printf("  %-45s = %.1f degC\n", disturbance_names[1], d[1])
for i in 2:nd
    @printf("  %-45s = %.1f W\n", disturbance_names[i], d[i])
end
println()
println("Final state at step $(N):")
for i in 1:nx
    @printf("  %-45s = %+.4f degC\n", state_names[i], X[N+1, i])
end
println()
println("Final outputs (air temperatures) at step $(N):")
for i in 1:ny
    @printf("  %-45s = %+.4f degC\n", output_names[i], Y[N, i])
end
println("=" ^ 60)

## Expose results for interactive inspection
println("\nVariables available: X [$(N+1)×$(nx)], Y [$(N)×$(ny)], u, d")

## Load MPC state trajectory from output.txt
# Format per line: [k, j]  value  repeated — k=timestep (1..N), j=state index (1..nx)
# Each line covers one state column j for all timesteps.
# MPC x[k, j] is the state at the END of step k, i.e. after applying u[k].
# This aligns with our X[k+1, j] (X is 1-indexed from the initial condition).
output_path = joinpath(pwd(), "data", "EWH", "outputs", "output.txt")
X_mpc = fill(NaN, N, nx)
open(output_path) do f
    in_x_section = false
    for line in eachline(f)
        if occursin("================ x ================", line)
            in_x_section = true
            continue
        end
        if in_x_section
            startswith(line, "====") && break
            isempty(strip(line)) && continue
            for m in eachmatch(r"\[(\d+),\s*(\d+)\]\s*([-\d.]+)", line)
                k, j, v = parse(Int, m[1]), parse(Int, m[2]), parse(Float64, m[3])
                if k <= N && j <= nx
                    X_mpc[k, j] = v
                end
            end
        end
    end
end
# MPC time axis: step k corresponds to t = k * Δt/60 min
t_mpc = (1:N) .* (Δt / 60)

## Plots
t_min  = (0:N)   .* (Δt / 60)   # state time axis [min]
t_minu = (1:N)   .* (Δt / 60)   # input time axis [min]

# Zone data: (air_idx, prod_idx, input_idx, T_low, T_high, label)
zones = [
    (1, 2,  1,  2.0,   8.0,  "Fridge Dairy"),
    (3, 4,  2,  2.0,   7.0,  "Fridge Finished Products"),
    (5, 6,  3,  0.0,   9.0,  "Fridge Meat"),
    (7, 8,  4,  2.0,   7.0,  "Fridge Vegetables"),
    (9, 10, 5, -40.0, -18.0, "Freezer"),
]

state_plots = []
power_plots = []

for (i_air, i_prod, i_u, T_low, T_high, label) in zones

    # --- Temperature subplot ---
    p_T = plot(t_min, X[:, i_air];
        label     = "T_air (sim)",
        xlabel    = "Time [min]",
        ylabel    = "Temp [°C]",
        title     = label,
        linewidth = 2,
        legend    = :topright,
        color     = :steelblue,
    )
    plot!(p_T, t_min, X[:, i_prod];
        label     = "T_prod (sim)",
        linewidth = 2,
        linestyle = :dash,
        color     = :darkorange,
    )
    # MPC trajectories — overlaid for comparison
    plot!(p_T, t_mpc, X_mpc[:, i_air];
        label     = "T_air (MPC)",
        linewidth = 2,
        linestyle = :dot,
        color     = :steelblue,
    )
    plot!(p_T, t_mpc, X_mpc[:, i_prod];
        label     = "T_prod (MPC)",
        linewidth = 2,
        linestyle = :dashdot,
        color     = :darkorange,
    )
    hline!(p_T, [T_low, T_high];
        label     = ["T_low" "T_high"],
        linestyle = :dot,
        linewidth = 1,
        color     = [:red :red],
    )
    push!(state_plots, p_T)

    # --- Power subplot ---
    p_u = plot(t_minu, U[:, i_u];
        label     = "P_compressor",
        xlabel    = "Time [min]",
        ylabel    = "Power [W]",
        title     = label * " — compressor",
        linewidth = 2,
        legend    = false,
        color     = :seagreen,
        seriestype = :steppost,
    )
    push!(power_plots, p_u)
end

fig_states = plot(state_plots...;
    layout     = (3, 2),
    size       = (1100, 950),
    plot_title = "Cold-room temperatures  (T_amb = 20 °C)",
)

fig_power = plot(power_plots...;
    layout     = (3, 2),
    size       = (1100, 950),
    plot_title = "Compressor power schedule",
)

savefig(fig_states, joinpath(pwd(), "scripts", "ewh", "dt_temperatures.png"))
savefig(fig_power,  joinpath(pwd(), "scripts", "ewh", "dt_power.png"))
println("Plots saved → scripts/ewh/dt_temperatures.png  &  dt_power.png")
display(fig_states)
display(fig_power)
