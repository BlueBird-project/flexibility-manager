"""
    build_dynamics(::Ewh, o, digital_twin, sensors, forecasts) -> BatchDynamics

Build batch dynamics for the EWH pilot (single operating mode).
"""
function build_dynamics(::Ewh, o::O,
                        digital_twin::Dict{String, Any},
                        sensors::Vector{Dict{String, Any}},
                        forecasts::Dict{String, Any})
    build_batch(o, digital_twin, sensors, forecasts)
end


"""
    Transform a MIMO ARX model over a horizon Hu in a batch static evolution

    X  = M ⋅ xk + Ξ ⋅ U + Ψ ⋅ Δ
    X  := [xk+1, xk+2, ...., xk+Hu] are the stacked states (decision variable)
    U  := [uk, uk+1, ..., uHu-1]    are the stacked controllable inputs (decision variable)
    D  := [dk, dk+1, ...., dHu-1]   are the stacked disturbances (forecasts) (known numbers)

    The  state evolves according to 
    xk+1 = A⋅xk + B⋅uk + G⋅dk
"""
function build_batch(o::O,
                     digital_twin::Dict{String, Any},
                     sensors::Vector{Dict{String, Any}},
                     forecast_json::Dict{String, Any})

    datetime = o.compute_datetime
    Hu = o.Hu

    # Note all put in the OX structure all of it

    # Extract state-space information
    ss = digital_twin["state_space"]

    # Get all the necessary ingredients
    A  = ss["A" ]
    x1 = ss["x1"]
    B  = ss["B" ]
    E  = ss["E" ]

    # Add forecasted disturbances
    # Δ = get_forecasts()

    # Now augment all to get a big statis description
    M = let
        M = zeros(0, size(A,2))
        A_pow = A 
        for _ in 1:Hu
            M = [M; A_pow]
            A_pow = A_pow*A
        end
        M
    end

    Ξ = batch_B(A, B, Hu)
    Ψ = batch_E(A, E, Hu)

    return BatchDynamics(M, Ξ, Ψ, x1, Δ)
end


function batch_B(A::Matrix, B::Matrix, Hu::Int)
    ny, nx = size(A)
    nu = size(B, 2)
    Ξ = zeros(ny * Hu, nu * Hu)

    # Pre-calculate B, AB, A^2B...
    blocks = Vector{Matrix{Float64}}(undef, Hu)
    A_pow = I(nx) # A^0

    for i in 1:Hu
        blocks[i] = A_pow * B
        A_pow = A_pow * A # Update for next power
    end

    for row in 1:Hu
        for col in 1:row
            row_idx = (row-1)*ny + 1 : row*ny
            col_idx = (col-1)*nu + 1 : col*nu
            Ξ[row_idx, col_idx] = blocks[row - col + 1]
        end
    end

    return Ξ
end

function batch_E(A::Matrix, G::Matrix, Hu::Int)
    return batch_B(A, G, Hu)
end