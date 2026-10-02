"""
    EwhVars

All JuMP decision variables and algebraic expressions for the EWH MPC.
Mutable so that derived expressions can be patched between stages if needed.
Nothing here duplicates O or OX — those are passed directly through the pipeline.
"""
mutable struct EwhVars
    # --- Decision variables ---
    x::Any          # [nx × Hu]  full state trajectory
    sp::Any         # [nu × Hu]  temperature setpoints (one per room)
    u::Any          # [nfridge × Hu]  fridge allocated power
    u_req::Any      # [nfridge × Hu]  fridge requested power
    p1::Any         # [Hu]  modulated compressor power
    δ_fridge::Any   # [Hu]  fridge on/off compressor (Bin)
    δ_freezer::Any  # [Hu]  freezer on/off compressor (Bin)
    p_buy::Any      # [Hu]  power bought from grid  (P↓)
    p_sell::Any     # [Hu]  power sold to grid      (P↑)
    PVused::Any     # [Hu]  PV used for self-consumption
    PVcurt::Any     # [Hu]  PV curtailed

    # --- Algebraic expressions (derived in build_variables!) ---
    p2::Any         # [Hu]  δ_fridge  .* p2_high
    p3::Any         # [Hu]  δ_freezer .* p3_high
    P_tot::Any      # [Hu]  p1 + p2 + p3
    U_req::Any      # [Hu]  Σ u_req over fridges
end
