using HTTP, JSON3, Dates, Printf

const TM_BASE = "http://localhost:9090"
const GERMANY_MARKET_ID = 4

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
    get_offer_sessions(market_id; date_from, date_to) -> Vector

Return offer session metadata for a market between two dates (inclusive).
Each session covers one trading day.  Fields include:
  offer_id   – unique session ID, pass to get_prices()
  date_str   – "YYYY-MM-DD" (local CET date of the delivery day)
  sequence   – revision number (higher = more recent publication)
  isp_unit   – slot length in minutes (15 for Germany)
  isp_len    – number of slots in the session (96 = full 24 h at 15-min resolution)
  ts         – Unix ms of session start (22:00 UTC = midnight CET of delivery day)
"""
function get_offer_sessions(market_id::Int;
                             date_from::Date = today() - Day(7),
                             date_to::Date   = today() + Day(2))
    ts_from = to_ms(DateTime(date_from))
    ts_to   = to_ms(DateTime(date_to) + Day(1))   # include end of last day
    resp = HTTP.get("$TM_BASE/api/market/$market_id/offerinfo",
                    query = ["ts_from" => string(ts_from),
                             "ts_to"   => string(ts_to)])
    sessions = JSON3.read(resp.body)
    # Sort by date and sequence so the latest revision is last
    return sort(collect(sessions); by = s -> (s.date_str, s.sequence))
end

"""
    get_prices(offer_id) -> Vector{NamedTuple}

Return the 15-minute price slots for one offer session.
Returns a sorted Vector of NamedTuples:
  (slot, datetime_utc, datetime_cet, price_eur_mwh)

`slot` runs 1–96 for a full day.
`datetime_cet` is the wall-clock time in Central European Time (UTC+1 winter, UTC+2 summer).
The session `ts` is the anchor: slot 1 starts at ts, slot 2 at ts+15 min, etc.
"""
function get_prices(offer_id::Int)
    resp = HTTP.get("$TM_BASE/api/offer/$offer_id")
    raw  = JSON3.read(resp.body)
    isempty(raw) && return NamedTuple[]

    session_start_utc = from_ms(Int64(first(raw).ts))
    cet_offset        = Hour(2)   # CEST (summer); use Hour(1) for CET (winter)

    result = map(raw) do s
        slot_offset   = Minute(15 * (Int(s.isp_start) - 1))
        dt_utc        = session_start_utc + slot_offset
        dt_cet        = dt_utc + cet_offset
        (slot          = Int(s.isp_start),
         datetime_utc  = dt_utc,
         datetime_cet  = dt_cet,
         price_eur_mwh = Float64(s.cost_mwh))
    end
    return sort(result; by = r -> r.slot)
end

# ── main ───────────────────────────────────────────────────────────────────────

println("=== Available markets ===")
for m in get_markets()
    println("  id=$(m.market_id)  $(m.market_location)  $(m.market_type)")
end
println()

# ── Query Germany day-ahead prices ────────────────────────────────────────────
# Change date_from / date_to to any range you need.
# Day-ahead prices are published ~noon the day before delivery.
# The system holds the last 5 days + today's published prices.

date_from = today() - Day(2)
date_to   = today() + Day(1)

println("=== Germany offer sessions ($date_from → $date_to) ===")
sessions = get_offer_sessions(GERMANY_MARKET_ID; date_from, date_to)

if isempty(sessions)
    println("  No sessions found.  Try a wider date range.")
else
    for s in sessions
        println("  offer_id=$(s.offer_id)  date=$(s.date_str)  " *
                "revision=$(s.sequence)  slots=$(s.isp_len)  " *
                "slot_len=$(s.isp_unit) min")
    end
    println()

    # ── Build a Dict{DateTime, Float64} across ALL sessions in the window ─────
    # Key   = slot start time in UTC (use datetime_cet if you prefer local time)
    # Value = price in EUR/MWh
    # Using a Dict means you can look up any slot instantly:  prices_dict[dt]
    # Later sessions (higher sequence) overwrite earlier ones for the same slot,
    # so you always end up with the most recent published price for each slot.
    prices_dict = Dict{DateTime, Float64}()

    for s in sessions
        for p in get_prices(Int(s.offer_id))
            prices_dict[p.datetime_utc] = p.price_eur_mwh
        end
    end

    # ── Display ───────────────────────────────────────────────────────────────
    latest = last(sessions)
    println("=== Prices for $(latest.date_str) (offer_id=$(latest.offer_id), " *
            "revision $(latest.sequence)) ===")
    println("  datetime CET (UTC+2)   |  EUR/MWh")
    println("  -----------------------|----------")

    prices = get_prices(Int(latest.offer_id))
    for p in prices
        @printf("  %s  |  %7.2f\n",
                Dates.format(p.datetime_cet, "yyyy-mm-dd HH:MM"),
                p.price_eur_mwh)
    end

    println()
    isempty(prices) || @printf("  min=%.2f  max=%.2f  mean=%.2f  EUR/MWh\n",
        minimum(p.price_eur_mwh for p in prices),
        maximum(p.price_eur_mwh for p in prices),
        sum(p.price_eur_mwh for p in prices) / length(prices))

    # ── How to use the Dict ───────────────────────────────────────────────────
    println()
    println("=== Dict usage example ===")
    example_dt = DateTime(2026, 5, 11, 10, 0, 0)   # 10:00 UTC = 12:00 CEST
    if haskey(prices_dict, example_dt)
        @printf("  Price at %s UTC = %.2f EUR/MWh\n", example_dt, prices_dict[example_dt])
    end

    # Sorted vector of (DateTime, price) — useful for MPC horizon iteration
    prices_vec = sort(collect(prices_dict))   # Vector{Pair{DateTime, Float64}}
    println("  First 3 slots in horizon:")
    for (dt, price) in first(prices_vec, 3)
        @printf("    %s UTC  →  %.2f EUR/MWh\n", dt, price)
    end
end
