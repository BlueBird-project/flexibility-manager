using HTTP, JSON3, Dates, Printf

const TM_BASE = "http://localhost:11001"

# ── helpers ────────────────────────────────────────────────────────────────────

"""Convert a Julia DateTime to a Unix timestamp in milliseconds (Int64)."""
to_ms(dt::DateTime) = Int64(round(datetime2unix(dt) * 1000))

"""Convert a Unix timestamp in milliseconds to a UTC DateTime."""
from_ms(ms::Int64) = unix2datetime(ms / 1000)

# ── API calls ──────────────────────────────────────────────────────────────────

"""
    get_markets() -> Vector

Return the list of all markets registered in the Trading Manager.
Each element has fields: market_id, market_name, market_type, market_location.
"""
function get_markets()
    resp = HTTP.get("$TM_BASE/api/market")
    return JSON3.read(resp.body)
end

"""
    find_market_id(markets, country) -> Int or nothing

Find market_id by matching country string against market_location (URI or name).
"""
function find_market_id(markets, country::String)
    for m in markets
        if occursin(country, string(m.market_location))
            return Int(m.market_id)
        end
    end
    return nothing
end

"""
    get_prices(market_id; date_from, date_to) -> Vector{NamedTuple}

Return all 15-minute price slots for a market in [date_from, date_to].
Returns a sorted Vector of NamedTuples:
  (datetime_utc, datetime_cet, price_eur_mwh)

Uses /api/market/{id}/offer?ts_from=...&ts_to=... directly.
Each returned item's `ts` field is the slot's own start time in Unix ms.
`datetime_cet` uses CEST (UTC+2); change cet_offset to Hour(1) for CET (winter).
"""
function get_prices(market_id::Int;
                    date_from::Date = today() - Day(2),
                    date_to::Date   = today() + Day(1))
    ts_from = to_ms(DateTime(date_from))
    ts_to   = to_ms(DateTime(date_to) + Day(1))   # include end of last day
    resp = HTTP.get("$TM_BASE/api/market/$market_id/offer",
                    query = ["ts_from" => string(ts_from),
                             "ts_to"   => string(ts_to)])
    raw = JSON3.read(resp.body)
    isempty(raw) && return NamedTuple[]

    cet_offset = Hour(2)   # CEST (summer); use Hour(1) for CET (winter)

    # Multiple sequences per slot — keep only the latest (highest sequence number)
    deduped = Dict{Int64, Any}()
    for s in raw
        ts = Int64(s.ts)
        seq = parse(Int, string(s.sequence))
        if !haskey(deduped, ts) || seq > deduped[ts][1]
            deduped[ts] = (seq, s)
        end
    end

    result = map(values(deduped)) do (_, s)
        dt_utc = from_ms(Int64(s.ts))
        dt_cet = dt_utc + cet_offset
        (datetime_utc  = dt_utc,
         datetime_cet  = dt_cet,
         price_eur_mwh = Float64(s.cost))
    end
    return sort(result; by = r -> r.datetime_utc)
end

# ── main ───────────────────────────────────────────────────────────────────────

markets = get_markets()
println("=== Available markets ===")
for m in markets
    println("  id=$(m.market_id)  $(m.market_location)  $(m.market_type)")
end
println()

market_id = find_market_id(markets, "Germany")
if isnothing(market_id)
    println("No Germany market found. Check market_location values above and update find_market_id().")
    exit(1)
end

# ── Query Germany day-ahead prices ────────────────────────────────────────────
# Change date_from / date_to to any range you need (max 180 days).
# Day-ahead prices are published ~noon the day before delivery.

date_from = today() - Day(2)
date_to   = today() + Day(1)

println("=== Germany prices ($date_from → $date_to), market_id=$market_id ===")
prices = get_prices(market_id; date_from, date_to)

if isempty(prices)
    println("  No prices found. The data acquisition job may not have run yet.")
    println("  Trigger it with:")
    println("    curl -X POST '$TM_BASE/api/market/country/job/Germany" *
            "?ts_from=$(to_ms(DateTime(date_from)))&ts_to=$(to_ms(DateTime(date_to) + Day(1)))&override_running_job=false' -d ''")
    println("  Then check progress with:")
    println("    curl '$TM_BASE/api/market/country/job/state'")
else
    println("  datetime CET (UTC+2)   |  EUR/MWh")
    println("  -----------------------|----------")
    for p in prices
        @printf("  %s  |  %7.2f\n",
                Dates.format(p.datetime_cet, "yyyy-mm-dd HH:MM"),
                p.price_eur_mwh)
    end

    println()
    @printf("  min=%.2f  max=%.2f  mean=%.2f  EUR/MWh  (%d slots)\n",
        minimum(p.price_eur_mwh for p in prices),
        maximum(p.price_eur_mwh for p in prices),
        sum(p.price_eur_mwh for p in prices) / length(prices),
        length(prices))

    # ── Build Dict{DateTime, Float64} for MPC lookup ──────────────────────────
    # Key = slot start UTC, value = EUR/MWh
    prices_dict = Dict{DateTime, Float64}(p.datetime_utc => p.price_eur_mwh for p in prices)

    println()
    println("=== Dict usage example ===")
    example_dt = DateTime(today(), Time(10, 0, 0))   # 10:00 UTC today
    if haskey(prices_dict, example_dt)
        @printf("  Price at %s UTC = %.2f EUR/MWh\n", example_dt, prices_dict[example_dt])
    else
        println("  (example_dt not in range — adjust example_dt as needed)")
    end

    # Sorted vector of (DateTime, price) — useful for MPC horizon iteration
    prices_vec = sort(collect(prices_dict))   # Vector{Pair{DateTime, Float64}}
    println("  First 3 slots in horizon:")
    for (dt, price) in first(prices_vec, 3)
        @printf("    %s UTC  →  %.2f EUR/MWh\n", dt, price)
    end
end
