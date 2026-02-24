"""
    One step MPC
    dt::DT         - The Digital Twin with the asset dynamics 
    f::Forecasts   - External disturbance including the electricity price 
    c::Constraints - Constraints on the assets power and confort. There must 
                     be exactly 1 constraint per state 
"""
function mpc_update_ewh(dt::DT, f::Forecasts, c::MPConstraints, P::Params)

    verbose = P.verbose

    Hu = P.Hu                 # Control Horizon
    nx = length(dt.x[:,1])    # State dimension
    nu = length(dt.B[1,:])    # Action dimension
    nd = length(f.d[:,1] )    # Disturbance dimension

    model = Model(HiGHS.Optimizer)

    @variable(model, u[k=1:Hu-1, 1:nu])  # Action
    @variable(model, x[k=1:Hu  , 1:nx])  # State 

    # Confort limits
    @constraint(model, state_operation[k in 2:Hu, ii in 1:nx],
                c.x_low[ii] <= x[k,ii] <= c.x_high[ii])

    # Operation limits
    @constraint(model, actuation_limits[k in 1:Hu-1, ii in 1:nu],
                c.u_low[ii] <= u[k,ii] <= c.u_high[ii])

    # The dynamics is considered as exact and with no noise, this could be different
    # if doing stochastic MPC (SMPC) 
    @constraint(model, initial_condition[ii=1:nx], x[1,ii] == dt.x[ii, end] )
    @constraint(model, dynamics[k in 2:Hu, ii in 1:nx],
                x[k,ii] == sum(dt.A[ii,j]*  x[k-1,j] for j in 1:nx)
                         + sum(dt.B[ii,j]*  u[k-1,j] for j in 1:nu)
                         + sum(dt.F[ii,j]*f.d[j,k-1] for j in 1:nd)
    )

    elec_price = f.d[end, 1:Hu]

    @objective(model, Min, sum(elec_price[k] * x[k, end] for k in 1:Hu))

    (verbose > 1) && print(model)
    (verbose < 2) && set_silent(model) 
    optimize!(model)                     

    return value.(u)
end 