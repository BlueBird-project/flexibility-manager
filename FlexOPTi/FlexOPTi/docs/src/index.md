# FM Documentation 

```@docs
    optimize(A, B, F, x, d=0 ; x_low=nothing, x_high=nothing, u_low=nothing, u_high=nothing, kwargs...)

    mpc_update(dt::DT, f::Forecasts, c::MPConstraints, P::Params)

    DT
    Forecasts
    MPConstraints
    Params
```