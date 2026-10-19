#!/usr/bin/env python3
"""
Run the MPC (src/mpc/mpc.py) over an entire sessions dataset under a shared site power cap.

Since the cap couples all stations together, sessions only need to be solved jointly when
their [arrival, departure) windows overlap in time -- so sessions are grouped into connected
components by overlap (any station), and each group is solved with one solve_horizon() call.
Non-overlapping sessions end up in their own group of 1. This gives the same result a full
tick-by-tick receding-horizon simulation would (nothing changes between arrivals in this
deterministic, fully-known-in-advance setting), without the wasted repeat solves.

Usage:
    python run_mpc_dataset.py
    python run_mpc_dataset.py --site-cap-kw 22 --sessions <csv> --price <xlsx>
    python run_mpc_dataset.py --site-cap-kw 22 --compare-no-cap
"""
import argparse
import os
import sys
import time

ROOT = os.path.dirname(os.path.abspath(__file__))
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

import pandas as pd

from src.mpc.mpc import EVSession, solve_horizon

DEFAULT_SESSIONS = "src/env/dataset/ewh_cleaned_rtu.csv"
DEFAULT_PRICE = "src/env/dataset/price_two_years_15min.xlsx"
DEFAULT_POWER_KW = 11  # for sessions without their own power_kw column


def load_sessions_csv(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    df["arrival"] = pd.to_datetime(df["arrival"])
    df["departure"] = pd.to_datetime(df["departure"])
    if "power_kw" not in df.columns:
        df["power_kw"] = DEFAULT_POWER_KW
    # drop sessions too short to plausibly deliver their energy
    mask = (df["departure"] - df["arrival"]).dt.total_seconds() / 3600 >= 1.5 * df["req_kwh"] / df["power_kw"]
    return df[mask].sort_values("arrival").reset_index(drop=True)


def load_prices(price_path: str):
    price_df = pd.read_excel(price_path)
    price_df["price"] = price_df["price"] / 1000.0  # EUR/MWh -> EUR/kWh
    price_df["date"] = pd.to_datetime(price_df["timestamp"] + " " + price_df["Tijd"], dayfirst=True)
    price_df = price_df.sort_values("date").reset_index(drop=True)
    return price_df


def group_by_overlap(sessions_df: pd.DataFrame) -> list[list[dict]]:
    """Connected components of sessions whose [arrival, departure) windows overlap, across all stations."""
    rows = sessions_df.sort_values("arrival").to_dict("records")
    groups, current, current_end = [], [], None
    for r in rows:
        if current and r["arrival"] < current_end:
            current.append(r)
            current_end = max(current_end, r["departure"])
        else:
            if current:
                groups.append(current)
            current, current_end = [r], r["departure"]
    if current:
        groups.append(current)
    return groups


def group_to_evs(group: list[dict]) -> list[EVSession]:
    return [
        EVSession(
            ev_id=f"{r['ChargingPoint']}_{r['arrival']}",
            charger_id=str(r["station_id"]),
            arrival_time=r["arrival"].to_pydatetime(),
            departure_time=r["departure"].to_pydatetime(),
            energy_needed_kwh=float(r["req_kwh"]),
            max_charging_power_kw=float(r["power_kw"]),
        )
        for r in group
    ]


def solve_dataset(groups: list[list[dict]], price_map: dict, site_max_power_kw: float | None) -> dict:
    """Solve every group with one solve_horizon() call each; return aggregate stats + per-group results."""
    t0 = time.perf_counter()
    total_cost = 0.0
    non_optimal, under_delivered, cap_violations = [], [], []
    per_group = []

    for group in groups:
        evs = group_to_evs(group)
        now = min(ev.arrival_time for ev in evs)
        result = solve_horizon(evs, price_map, now, site_max_power_kw=site_max_power_kw)
        per_group.append((evs, result))

        if result["status"] != "optimal":
            non_optimal.append((evs, result["status"]))
            continue
        total_cost += result["cost"]

        kwh_per_slot = {ev.ev_id: ev.max_charging_power_kw * 0.25 for ev in evs}
        for ev in evs:
            delivered = sum(kwh_per_slot[ev.ev_id] * p[ev.ev_id] for p in result["profile"])
            if delivered < ev.energy_needed_kwh - 1e-6:
                under_delivered.append((ev.ev_id, delivered, ev.energy_needed_kwh))

        if site_max_power_kw is not None:
            power_by_id = {ev.ev_id: ev.max_charging_power_kw for ev in evs}
            for p in result["profile"]:
                total_power = sum(power_by_id[k] * v for k, v in p.items() if k not in ("start", "end"))
                if total_power > site_max_power_kw + 1e-6:
                    cap_violations.append((p["start"], total_power))

    return {
        "elapsed_s": time.perf_counter() - t0,
        "total_cost": round(total_cost, 4),
        "non_optimal": non_optimal,
        "under_delivered": under_delivered,
        "cap_violations": cap_violations,
        "per_group": per_group,
    }


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--sessions", default=DEFAULT_SESSIONS, help=f"Sessions CSV (default: {DEFAULT_SESSIONS})")
    parser.add_argument("--price", default=DEFAULT_PRICE, help=f"Price XLSX (default: {DEFAULT_PRICE})")
    parser.add_argument("--site-cap-kw", type=float, default=None, help="Shared power cap across all stations (kW). Omit for no cap.")
    parser.add_argument("--compare-no-cap", action="store_true", help="Also solve without the cap and report the cost difference.")
    args = parser.parse_args(argv)

    sessions_df = load_sessions_csv(args.sessions)
    price_df = load_prices(args.price)
    p_min, p_max = price_df["date"].min(), price_df["date"].max()
    sessions_df = sessions_df[(sessions_df["arrival"] >= p_min) & (sessions_df["departure"] <= p_max)].reset_index(drop=True)
    price_map = price_df.set_index("date")["price"].to_dict()

    print(f"{len(sessions_df)} sessions within price coverage ({p_min} -> {p_max})")

    groups = group_by_overlap(sessions_df)
    sizes = pd.Series([len(g) for g in groups])
    print(f"\n{len(groups)} connected-component groups by time overlap:")
    print(sizes.value_counts().sort_index().to_string())

    print(f"\nsolving with site_max_power_kw = {args.site_cap_kw}")
    res = solve_dataset(groups, price_map, args.site_cap_kw)
    print(f"solved {len(groups)} groups in {res['elapsed_s']:.2f}s")
    print(f"non-optimal groups: {len(res['non_optimal'])}")
    print(f"under-delivered EVs: {len(res['under_delivered'])}")
    print(f"cap violations: {len(res['cap_violations'])}")
    print(f"TOTAL COST: {res['total_cost']} EUR")

    if args.compare_no_cap and args.site_cap_kw is not None:
        res_nocap = solve_dataset(groups, price_map, None)
        diff = round(res["total_cost"] - res_nocap["total_cost"], 4)
        pct = round((res["total_cost"] / res_nocap["total_cost"] - 1) * 100, 2) if res_nocap["total_cost"] else 0.0
        print(f"\nTOTAL COST with no cap: {res_nocap['total_cost']} EUR")
        print(f"cost increase from the {args.site_cap_kw}kW cap: {diff} EUR ({pct}%)")

    return 0 if not (res["non_optimal"] or res["under_delivered"] or res["cap_violations"]) else 1


if __name__ == "__main__":
    raise SystemExit(main())
