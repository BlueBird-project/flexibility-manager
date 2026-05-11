# =============================================================================
#  Market price fetching with file-based cache and graceful fallback
#
#  Call order inside optimize():
#    fetch_market_prices(o) -> (prices_eur_kwh::Vector{Float64}, quality::Symbol)
#
#  quality values (stored in oy[:prices_quality]):
#    :live       — fresh from Trading Manager
#    :cached     — TM unreachable, using file cache from a previous call
#    :fallback   — cache also stale/missing; prices extrapolated from prior day
#    :unavailable — no data at all; flat 0 price used (optimization result unreliable)
# =============================================================================

const PRICE_SLOT_MINUTES  = 15
const EUR_MWH_TO_EUR_WH   = 1 / 1_000_000.0   # power in W, time in s → cost in EUR

# ── Low-level TM helpers (same logic as query_prices.jl standalone script) ───

_to_ms(dt::DateTime) = Int64(round(datetime2unix(dt) * 1000))
_from_ms(ms::Int64)  = unix2datetime(ms / 1000)

"""
    _query_tm_prices(tm_base, market_id) -> Dict{DateTime, Float64}

Query the Trading Manager for all available day-ahead prices.
Returns a Dict mapping slot-start UTC DateTime → price in EUR/kWh.
Throws on HTTP or parse errors — callers should catch.
"""
function _query_tm_prices(tm_base::String, market_id::Int)::Dict{DateTime, Float64}
    date_from = today() - Day(2)
    date_to   = today() + Day(2)
    ts_from   = _to_ms(DateTime(date_from))
    ts_to     = _to_ms(DateTime(date_to) + Day(1))

    @info "Querying TM: market $market_id, sessions $date_from → $date_to"
    resp     = HTTP.get("$tm_base/api/market/$market_id/offerinfo",
                        query = ["ts_from" => string(ts_from),
                                 "ts_to"   => string(ts_to)])
    sessions = sort(collect(JSON3.read(resp.body)); by = s -> (s.date_str, s.sequence))
    @info "  Found $(length(sessions)) offer session(s)"

    prices = Dict{DateTime, Float64}()
    for s in sessions
        offer_resp = HTTP.get("$tm_base/api/offer/$(Int(s.offer_id))")
        raw = JSON3.read(offer_resp.body)
        isempty(raw) && continue
        session_start = _from_ms(Int64(first(raw).ts))
        for slot in raw
            dt  = session_start + Minute(PRICE_SLOT_MINUTES * (Int(slot.isp_start) - 1))
            prices[dt] = Float64(slot.cost_mwh) * EUR_MWH_TO_EUR_WH
        end
    end
    return prices
end

# ── File cache ────────────────────────────────────────────────────────────────

function _cache_path(market_id::Int)
    dir = joinpath(pkgdir(@__MODULE__), "data", "cache")
    isdir(dir) || mkpath(dir)
    joinpath(dir, "prices_market_$(market_id).json")
end

function _save_cache(market_id::Int, prices::Dict{DateTime, Float64})
    payload = Dict(
        "fetched_at" => string(now(UTC)),
        "market_id"  => market_id,
        "prices"     => Dict(string(k) => v for (k, v) in prices),
    )
    path = _cache_path(market_id)
    open(path, "w") do io
        JSON.print(io, payload)
    end
    @info "Price cache written to $path ($(length(prices)) slots)"
end

const CACHE_MAX_AGE_HOURS = 36

function _load_cache(market_id::Int)::Union{Dict{DateTime, Float64}, Nothing}
    path = _cache_path(market_id)
    isfile(path) || return nothing
    raw = JSON.parsefile(path)
    fetched_at = DateTime(raw["fetched_at"][1:19])  # trim timezone suffix if present
    age_hours  = (now(UTC) - fetched_at) / Hour(1)
    if age_hours > CACHE_MAX_AGE_HOURS
        @warn "Price cache is $(round(age_hours, digits=1)) h old (limit: $(CACHE_MAX_AGE_HOURS) h) — treating as expired"
        return nothing
    end
    prices = Dict{DateTime, Float64}()
    for (k, v) in raw["prices"]
        prices[DateTime(k)] = Float64(v)
    end
    return prices
end

# ── Horizon alignment ─────────────────────────────────────────────────────────

"""
    _snap_to_slot(dt::DateTime) -> DateTime

Floor `dt` to the nearest 15-minute price slot boundary (UTC).
"""
function _snap_to_slot(dt::DateTime)::DateTime
    m = Dates.minute(dt)
    snapped_m = (m ÷ PRICE_SLOT_MINUTES) * PRICE_SLOT_MINUTES
    DateTime(Dates.year(dt), Dates.month(dt), Dates.day(dt),
             Dates.hour(dt), snapped_m, 0)
end

"""
    _align_prices(prices_dict, t0, Hu, Δt_sec) -> Vector{Float64}

Build a length-Hu price vector from `prices_dict`, starting at `t0`.
Each MPC step `k` maps to the 15-min slot containing `t0 + (k-1)*Δt`.
Missing slots are filled with the previous day's price, then the last known
price, then zero (in order of preference).
"""
function _align_prices(prices_dict::Dict{DateTime, Float64},
                       t0::DateTime, Hu::Int, Δt_sec::Float64)::Vector{Float64}
    result = Vector{Float64}(undef, Hu)
    last_known = 0.0
    for k in 1:Hu
        step_time = t0 + Millisecond(round(Int, (k - 1) * Δt_sec * 1000))
        slot      = _snap_to_slot(step_time)
        if haskey(prices_dict, slot)
            result[k] = prices_dict[slot]
            last_known = result[k]
        else
            # try same slot from previous day
            prev_day_slot = slot - Day(1)
            if haskey(prices_dict, prev_day_slot)
                result[k] = prices_dict[prev_day_slot]
                last_known = result[k]
            else
                result[k] = last_known   # constant extrapolation
            end
        end
    end
    return result
end

# ── Variable Hu ───────────────────────────────────────────────────────────────

"""
    _available_slots(prices_dict, t0, Δt_sec) -> Int

Count consecutive MPC steps from `t0` that have a price (directly or via
previous-day proxy). Used when `o.variable_Hu = true`.
Returns at least 1.
"""
function _available_slots(prices_dict::Dict{DateTime, Float64},
                           t0::DateTime, Δt_sec::Float64)::Int
    count = 0
    k = 1
    while true
        step_time = t0 + Millisecond(round(Int, (k - 1) * Δt_sec * 1000))
        slot      = _snap_to_slot(step_time)
        has_price = haskey(prices_dict, slot) || haskey(prices_dict, slot - Day(1))
        has_price || break
        count = k
        k += 1
        # don't look beyond 48 h (no day-ahead beyond D+1)
        step_time > t0 + Hour(48) && break
    end
    return max(count, 1)
end

# ── Public entry point ────────────────────────────────────────────────────────

"""
    fetch_market_prices(o::O) -> (prices::Vector{Float64}, quality::Symbol)

Fetch day-ahead electricity prices for the MPC horizon defined by `o`.

Returns a length-`o.Hu` vector of buy prices in EUR/Wh (one value per MPC
step, snapped to the containing 15-min price slot) and a quality symbol:
(Unit: EUR/Wh matches the W power variables and s time step in the objective
 Δt[s] × p[W] × price[EUR/Wh] = EUR)
  :live        — fresh from the Trading Manager
  :cached      — TM unreachable, values from the on-disk cache
  :fallback    — cache also unavailable; previous-day proxy or constant 0
  :unavailable — no prices anywhere; vector is all zeros

If `o.variable_Hu == true`, `o.Hu` is updated in-place to the number of
published slots available from `o.compute_datetime`.
"""
function fetch_market_prices(o::O)::Tuple{Vector{Float64}, Symbol}
    t0      = _resolve_t0(o.compute_datetime)
    Δt_sec  = o.Δt

    @info "Resolving market prices for horizon t0=$(t0) UTC, Hu=$(o.Hu), Δt=$(o.Δt) s"
    prices_dict, quality = _get_prices_dict(o)

    if o.variable_Hu && !isempty(prices_dict)
        o.Hu = _available_slots(prices_dict, t0, Δt_sec)
        @info "variable_Hu: horizon set to $(o.Hu) steps ($(round(o.Hu * Δt_sec / 3600, digits=1)) h)"
    end

    prices_vec = _align_prices(prices_dict, t0, o.Hu, Δt_sec)
    @info "Price vector aligned: Hu=$(o.Hu) steps, " *
          "min=$(round(minimum(prices_vec), digits=4)) " *
          "max=$(round(maximum(prices_vec), digits=4)) EUR/kWh " *
          "(quality: $quality)"
    return prices_vec, quality
end

# ── Internal helpers ──────────────────────────────────────────────────────────

function _resolve_t0(compute_datetime)::DateTime
    if compute_datetime isa ZonedDateTime
        return DateTime(compute_datetime, UTC)
    elseif compute_datetime isa String
        return DateTime(compute_datetime)
    else
        return now(UTC)
    end
end

function _get_prices_dict(o::O)::Tuple{Dict{DateTime, Float64}, Symbol}
    # 1. Try live TM query
    try
        prices_dict = _query_tm_prices(o.tm_base_url, o.market_id)
        if !isempty(prices_dict)
            _save_cache(o.market_id, prices_dict)
            @info "Market prices fetched live from Trading Manager ($(length(prices_dict)) slots)"
            return prices_dict, :live
        end
    catch e
        @warn "Trading Manager unreachable: $e — trying file cache"
    end

    # 2. Fall back to file cache
    cached = _load_cache(o.market_id)
    if !isnothing(cached) && !isempty(cached)
        @warn "Using cached market prices (TM unavailable) — $(length(cached)) slots"
        return cached, :cached
    end

    # 3. No data at all
    @error "No market prices available (TM down, no cache). Optimization cost signal is unreliable."
    return Dict{DateTime, Float64}(), :unavailable
end
