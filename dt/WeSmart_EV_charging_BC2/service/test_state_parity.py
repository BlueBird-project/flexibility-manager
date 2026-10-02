#!/usr/bin/env python3
"""
Guards against service/state_builder.py silently drifting from the training
env it duplicates. Builds a tiny synthetic EnergyEnv (real EVComponent +
PVComponent), takes one step, and asserts StateBuilder.build() reproduces
EnergyEnv's own normalised state vector exactly for the same raw inputs.

Not part of the Docker image (needs pandas/EnergyEnv, which the service
itself does not). Run manually after touching either side:
    python service/test_state_parity.py
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(ROOT))

from src.env.base_env import EnergyEnv  # noqa: E402
from src.env.ev_component import EVComponent  # noqa: E402
from src.env.pv_component import PVComponent  # noqa: E402
from service.state_builder import StateBuilder, apply_deadline_guard  # noqa: E402
from main import make_deadline_guard  # noqa: E402


def _metadata_for(env: EnergyEnv) -> dict:
    return {
        "observation_size": env.observation_size,
        "action_size": env.action_size,
        "price_horizon": env.price_horizon,
        "interval_minutes": env.interval_minutes,
        "norm_state": env.get_norm_state(),
    }


def _telemetry(env: EnergyEnv):
    """Extract the raw inputs a live caller would send for the interval about to be decided."""
    t = env.current_time + env.interval_td
    # a live caller sends plain consecutive quarter-hour prices; averaging and
    # normalisation happen on each side independently
    prices = env._prices([t + k * env.interval_td for k in range(env.prices_required)])  # noqa: SLF001

    ev = env.components[0]
    ev_sessions = ev._sessions_at(t)  # noqa: SLF001
    stations = []
    for s_idx in range(ev.n_stations):
        sess = ev_sessions.get(s_idx)
        entry = {"station_id": ev.station_ids[s_idx]}
        if sess is not None:
            remaining = ev.remaining_kwh[s_idx] if ev._active_ids.get(s_idx) == sess.sid else sess.need_kwh  # noqa: SLF001
            entry.update(
                present=1,
                remaining_kwh=float(remaining),
                hours_to_departure=max((sess.departure - t).total_seconds() / 3600, 0.0),
            )
        else:
            entry["present"] = 0
        stations.append(entry)

    pv_forecast = None
    if len(env.components) > 1:
        pv = env.components[1]
        pv_forecast = [pv._get_net_kwh(t + k * pv.interval_td) for k in range(pv.forecast_horizon)]  # noqa: SLF001

    return t, stations, prices, pv_forecast


def check(label: str, env: EnergyEnv, steps: int = 12, expect_clipping: bool = False) -> None:
    """Compare env vs StateBuilder at every interval of a short rollout.

    Holding every action at 0 lets `remaining_kwh` stay high while `hours_left`
    shrinks, which is what drives urgency past its clip — the branch that a
    reset()-only check never reaches.
    """
    print(f"[{label}] ", end="")
    env.reset()
    builder = StateBuilder.from_metadata(_metadata_for(env))
    ev = env.components[0]
    n_compared = 0
    saw_clip = False
    nonlocal_guard_fired = [False]

    done = False
    for _ in range(steps):
        if done:
            break
        expected = env.normalize_state(env._build_state())  # noqa: SLF001
        t, stations, prices, pv_forecast = _telemetry(env)

        # did any station actually land outside the training box this interval?
        for st in stations:
            if st.get("present"):
                if (st["remaining_kwh"] / ev.norm_max_kwh > 1.0
                        or st["hours_to_departure"] / ev.norm_max_hours > 1.0
                        or st["remaining_kwh"] / (st["hours_to_departure"] * ev.power_kw + 1e-6) > ev.urgency_clip):
                    saw_clip = True

        actual = builder.build(t.to_pydatetime(), stations, prices, pv_forecast)
        assert actual.shape == expected.shape, f"shape mismatch: {actual.shape} vs {expected.shape}"
        np.testing.assert_allclose(actual, expected, rtol=1e-5, atol=1e-6,
                                   err_msg=f"{label}: mismatch at {t}")
        if env.price_encoding == "window":
            # every input must be normalised: prices/EV/PV in [0, 1], level and sin/cos in [-1, 1]
            assert expected.min() >= -1 - 1e-6 and expected.max() <= 1 + 1e-6, (
                f"{label}: unnormalised input at {t}: min {expected.min():.3f}, max {expected.max():.3f}")

        # the guard is duplicated across the same seam as the state vector, so check
        # both implementations agree on every action pattern for this state
        env_guard = make_deadline_guard(env)
        margin = ev.feasibility_margin
        for pattern in range(2 ** env.action_size):
            base = np.array([(pattern >> b) & 1 for b in range(env.action_size)], dtype=np.int64)
            a_env = env_guard(expected, base.copy())
            a_svc = apply_deadline_guard(actual, base.copy().tolist(), builder, margin)
            assert list(a_env) == list(a_svc), (
                f"{label}: guard disagrees at {t} for {base.tolist()}: {list(a_env)} vs {list(a_svc)}")
            if list(a_env) != base.tolist():
                nonlocal_guard_fired[0] = True

        n_compared += 1
        _, _, done, _ = env.step(np.zeros(env.action_size, dtype=int))

    if expect_clipping:
        assert saw_clip, f"{label}: scenario never left the training box, clip path untested"
    print(f"PASS ({n_compared} intervals x {env.observation_size} values"
          f"{', incl. inputs outside the training box' if saw_clip else ''}"
          f"{', guard fired' if nonlocal_guard_fired[0] else ''})")


def make_sessions() -> pd.DataFrame:
    return pd.DataFrame([
        {"arrival": "2025-01-01 08:00", "departure": "2025-01-01 12:00", "req_kwh": 10.0, "station_id": 1},
        {"arrival": "2025-01-01 09:00", "departure": "2025-01-01 11:00", "req_kwh": 5.0, "station_id": 2},
    ])


def make_prices() -> pd.DataFrame:
    idx = pd.date_range("2025-01-01 00:00", "2025-01-02 00:00", freq="15min")
    return pd.DataFrame({
        "timestamp": idx.strftime("%d-%m-%Y"),
        "Tijd": idx.strftime("%H:%M"),
        "price": np.linspace(0.05, 0.35, len(idx)),
    })


def make_pv() -> pd.DataFrame:
    idx = pd.date_range("2025-01-01 00:00", "2025-01-02 00:00", freq="15min")
    return pd.DataFrame({
        "Timestamp": idx,
        "Production_kWh": np.clip(np.sin(np.linspace(0, np.pi, len(idx))), 0, None) * 5,
    })


def make_oversized_sessions() -> pd.DataFrame:
    """A session bigger and longer than anything the component was built on.

    This is the real test split's situation: evaluation reloads sessions but keeps
    the training normalisation constants, so remaining/hours can exceed 1.0.
    """
    return pd.DataFrame([
        {"arrival": "2025-01-01 06:00", "departure": "2025-01-01 23:00", "req_kwh": 90.0, "station_id": 1},
        {"arrival": "2025-01-01 07:00", "departure": "2025-01-01 10:00", "req_kwh": 25.0, "station_id": 2},
    ])


def make_flat_prices() -> pd.DataFrame:
    """Constant price: the window has zero spread, which must not divide by zero."""
    p = make_prices()
    p["price"] = 0.11
    return p


def make_volatile_prices() -> pd.DataFrame:
    """Negative prices and spikes beyond the scale, so level/spread clipping is exercised."""
    p = make_prices()
    n = len(p)
    p["price"] = np.where(np.arange(n) % 7 == 0, 0.9, np.linspace(-0.4, 0.3, n))
    return p


# the current default state: 12 price values, 30 min apart, window-normalised
NEW = dict(price_horizon=12, price_step_minutes=30, price_encoding="window")
# the pre-change state older checkpoints were trained on: 4 raw quarter-hour prices
LEGACY = dict(price_horizon=4, price_step_minutes=15, price_encoding="raw")


def main() -> None:
    sessions = make_sessions()
    prices = make_prices()
    pv = make_pv()

    def ev(clip=True):
        return EVComponent(sessions, power_kw=9.0, clip_features=clip)

    check("EV only", EnergyEnv(prices, components=[ev()], **NEW))
    check("EV + PV", EnergyEnv(prices, components=[ev(), PVComponent(pv, forecast_horizon=4, clip_features=True)], **NEW))
    check("EV + PV, 8 hourly prices", EnergyEnv(prices, components=[ev(), PVComponent(pv, forecast_horizon=4, clip_features=True)],
                                                price_horizon=8, price_step_minutes=60, price_encoding="window"))
    check("EV only, flat prices", EnergyEnv(make_flat_prices(), components=[ev()], **NEW))
    check("EV + PV, negative prices and spikes", EnergyEnv(make_volatile_prices(), components=[ev(), PVComponent(pv, forecast_horizon=4, clip_features=True)], **NEW))

    # pre-change checkpoints (raw prices, unclipped) must still be served exactly as trained
    check("EV only (legacy prices + unclipped)", EnergyEnv(prices, components=[ev(clip=False)], **LEGACY))
    check("EV + PV (legacy prices)", EnergyEnv(prices, components=[ev(), PVComponent(pv, forecast_horizon=4)], **LEGACY))

    # the case the clipping and guard exist for: normalisation frozen on small sessions,
    # then evaluated on a session that leaves the box in every direction at once
    env4 = EnergyEnv(prices, components=[ev(), PVComponent(pv, forecast_horizon=4, clip_features=True)], **NEW)
    env4.reload_data(prices, make_oversized_sessions())
    check("EV + PV (out-of-distribution sessions)", env4, steps=40, expect_clipping=True)

    env5 = EnergyEnv(prices, components=[ev(clip=False)], **LEGACY)
    env5.reload_data(prices, make_oversized_sessions())
    check("EV only (legacy, out-of-distribution)", env5, steps=40)

    print("\nAll parity checks passed.")


if __name__ == "__main__":
    main()
