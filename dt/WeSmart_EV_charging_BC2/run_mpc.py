#!/usr/bin/env python3
"""
Run the MPC over the sessions dataset and score it against the always-charge baseline.

Drives the same `EnergyEnv` the RL training uses, so cost, delivered energy and unmet energy
are computed by exactly the same accounting — the numbers here are directly comparable to the
`EVAL` / `RB` lines from `python main.py train`.

Each 15-minute interval it calls `mpc_decide()` with the EVs currently connected (their
remaining kWh and time left), the next `--horizon-hours` of prices and, when enabled, the PV
forecast. Only the decision for the current interval is applied — that is the receding-horizon
loop, one solve per interval, exactly as the live service does per request.

    python run_mpc.py                                    # EV only, test split
    python run_mpc.py --pv src/env/dataset/solar_production.csv
    python run_mpc.py --pv src/env/dataset/solar_production.csv \
                      --consumption "src/env/dataset/common_areas_consumption(in).csv"
    python run_mpc.py --split val --horizon-hours 12     # shorter look-ahead
    python run_mpc.py --max-steps 400                    # quick smoke run

No model, no torch: this path only needs pandas + pulp.
"""
from __future__ import annotations

import argparse
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from main import (DEFAULT_OUTPUT_DIR, POWER_KW, TEST_SIZE, VAL_SIZE, load_consumption_dataset,  # noqa: E402
                 load_price, load_pv_dataset, load_sessions_csv, make_env, split_df)
from src.env.pv_component import PVComponent  # noqa: E402
from src.mpc.mpc import SLOT, EVSession, mpc_decide  # noqa: E402


def run_episode(env, policy, collect_log: bool = True) -> dict:
    """One pass over the data under `policy(env, now) -> action array`, with env accounting."""
    env.reset()
    info_history, done = [], False
    while not done:
        now = env.current_time + env.interval_td      # the interval about to be decided
        action = policy(env, now)
        _, _, done, info = env.step(action)
        info_history.append(info)
    metrics = {type(c).__name__: c.get_episode_metrics(info_history) for c in env.components}
    logs = env.get_episode_logs() if collect_log else {}
    return {"metrics": metrics, "logs": logs}


def always_charge(env, now):
    return np.ones(env.action_size, dtype=int)


class MpcPolicy:
    """Calls mpc_decide() once per interval with what a live caller would send."""

    def __init__(self, horizon_hours: float, site_max_power_kw: float | None,
                 time_limit_s: float, power_kw: float):
        self.quarters = int(round(horizon_hours * 4))
        self.site_max_power_kw = site_max_power_kw
        self.time_limit_s = time_limit_s
        self.power_kw = power_kw
        self.solves = 0
        self.solve_seconds = 0.0
        self.no_solution = 0

    def __call__(self, env, now):
        ev = env.components[0]
        pv_comp = next((c for c in env.components if isinstance(c, PVComponent)), None)
        action = np.zeros(env.action_size, dtype=int)

        sessions = ev._sessions_at(now)  # noqa: SLF001 - same access the parity test uses
        evs = []
        for s_idx, sess in sessions.items():
            remaining = (ev.remaining_kwh[s_idx] if ev._active_ids.get(s_idx) == sess.sid  # noqa: SLF001
                         else sess.need_kwh)
            hours_left = (sess.departure - now).total_seconds() / 3600
            if remaining <= 1e-9 or hours_left <= 0:
                continue
            evs.append((s_idx, EVSession(ev_id=str(s_idx), charger_id=str(s_idx),
                                        arrival_time=now, departure_time=sess.departure,
                                        energy_needed_kwh=float(remaining),
                                        max_charging_power_kw=self.power_kw)))
        if not evs:
            return action

        slots = [now + k * SLOT for k in range(self.quarters)]
        prices = dict(zip(slots, env._prices(slots)))  # noqa: SLF001
        pv = ({s: pv_comp._get_net_kwh(s) for s in slots} if pv_comp is not None else None)  # noqa: SLF001

        t0 = time.perf_counter()
        result = mpc_decide([e for _, e in evs], prices, now,
                            site_max_power_kw=self.site_max_power_kw,
                            pv_forecast=pv, time_limit_s=self.time_limit_s)
        self.solve_seconds += time.perf_counter() - t0
        self.solves += 1

        decisions = result.get("decisions") or {}
        if not decisions:      # no solution at all: fall back to charging what needs energy
            self.no_solution += 1
            for s_idx, _ in evs:
                action[s_idx] = 1
            return action
        for ev_id, value in decisions.items():
            action[int(ev_id)] = int(value)
        return action


def report(label: str, metrics: dict, baseline: dict | None = None) -> None:
    ev = metrics.get("EVComponent", {})
    req, cost = ev.get("required_kwh", 0.0), ev.get("cost_eur", 0.0)
    unmet = ev.get("unmet_kwh", 0.0)
    line = (f"{label:14s} cost EUR {cost:9.2f} | unmet {unmet:8.2f} kWh "
            f"({100 * unmet / req if req else 0:5.2f}%) | delivered {ev.get('delivered_kwh', 0.0):9.2f} kWh")
    if baseline:
        b = baseline.get("EVComponent", {}).get("cost_eur", 0.0)
        line += f" | vs always-charge {100 * (cost / b - 1) if b else 0:+6.2f}%"
    print(line)
    pv = metrics.get("PVComponent")
    if pv:
        print(f"{'':14s} PV produced {pv.get('produced_kwh', 0.0):8.0f} kWh | "
              f"used by EVs {pv.get('used_ev_kwh', 0.0):8.0f} kWh | self-rate {pv.get('self_rate', 0.0):.2f}")


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--sessions", default=None)
    ap.add_argument("--price", default=None)
    ap.add_argument("--pv", default=None, help="PV dataset; enables the PV term")
    ap.add_argument("--consumption", default=None, help="building load, netted off PV (needs --pv)")
    ap.add_argument("--split", default="test", choices=["train", "val", "test", "all"])
    ap.add_argument("--horizon-hours", type=float, default=24.0,
                    help="how far ahead the MPC is given prices/PV (default: 24)")
    ap.add_argument("--site-max-power-kw", type=float, default=None)
    ap.add_argument("--time-limit", type=float, default=10.0, help="solver limit per interval")
    ap.add_argument("--power", type=float, default=POWER_KW)
    ap.add_argument("--out", default=None, help="folder for the CSVs (default: outputs/mpc/<setup>)")
    ap.add_argument("--skip-baseline", action="store_true")
    args = ap.parse_args()

    from main import DEFAULT_PRICE, DEFAULT_SESSIONS  # noqa: PLC0415
    sessions = load_sessions_csv(args.sessions or DEFAULT_SESSIONS, args.power)
    price_df = load_price(args.price or DEFAULT_PRICE)
    pv_df = load_pv_dataset(args.pv) if args.pv else None
    cons_df = load_consumption_dataset(args.consumption) if args.consumption else None
    if cons_df is not None and pv_df is None:
        print("[WARN] --consumption has no effect without --pv")

    train_df, val_df, test_df = split_df(sessions, test_size=TEST_SIZE, eval_size=VAL_SIZE)
    chosen = {"train": train_df, "val": val_df, "test": test_df, "all": sessions}[args.split]
    setup = "EV" + ("_PV" if pv_df is not None else "") + ("_Cons" if cons_df is not None else "")
    print(f"MPC on the {args.split} split: {len(chosen)} sessions, "
          f"{chosen['req_kwh'].sum():.0f} kWh | setup {setup} | "
          f"horizon {args.horizon_hours:g} h | power {args.power:g} kW")

    env = make_env(price_df, train_df, pv_df, cons_df, power_kw=args.power, verbose=False)
    env.reload_data(price_df, chosen)

    policy = MpcPolicy(args.horizon_hours, args.site_max_power_kw, args.time_limit, args.power)
    t0 = time.time()
    mpc_out = run_episode(env, policy)
    print(f"\n{policy.solves} solves in {policy.solve_seconds:.1f}s "
          f"({1000 * policy.solve_seconds / max(policy.solves, 1):.0f} ms each), "
          f"{time.time() - t0:.0f}s total"
          + (f" | {policy.no_solution} intervals with no solution (charged everything)"
             if policy.no_solution else ""))

    baseline = None
    if not args.skip_baseline:
        env.reload_data(price_df, chosen)
        baseline = run_episode(env, always_charge, collect_log=False)["metrics"]
        report("always-charge", baseline)
    report("MPC", mpc_out["metrics"], baseline)

    out_dir = Path(args.out) if args.out else Path(DEFAULT_OUTPUT_DIR) / "mpc" / setup
    out_dir.mkdir(parents=True, exist_ok=True)
    rows = []
    for label, m in (("MPC", mpc_out["metrics"]), ("ALWAYS", (baseline or {}))):
        ev = m.get("EVComponent", {})
        if ev:
            rows.append({"mode": label, "energy_required_kwh": ev.get("required_kwh"),
                         "delivered_kwh": ev.get("delivered_kwh"), "unmet_kwh": ev.get("unmet_kwh"),
                         "energy_cost_eur": ev.get("cost_eur")})
    pd.DataFrame(rows).to_csv(out_dir / "summary_compare.csv", index=False)
    for name, df in (mpc_out["logs"] or {}).items():
        if df is not None:
            df.to_csv(out_dir / f"mpc_{name}_per_ev.csv", index=False)
    print(f"\nwritten to {out_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
