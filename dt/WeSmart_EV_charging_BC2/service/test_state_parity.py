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
from service.state_builder import StateBuilder  # noqa: E402


def _metadata_for(env: EnergyEnv) -> dict:
    return {
        "observation_size": env.observation_size,
        "action_size": env.action_size,
        "price_horizon": env.price_horizon,
        "interval_minutes": env.interval_minutes,
        "norm_state": env.get_norm_state(),
    }


def check(label: str, env: EnergyEnv) -> None:
    print(f"[{label}] ", end="")
    env.reset()
    raw = env._build_state()  # noqa: SLF001 - test-only access to internals
    expected = env.normalize_state(raw)

    t = env.current_time + env.interval_td
    prices = env._prices([t + k * env.interval_td for k in range(env.price_horizon)])  # noqa: SLF001

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

    builder = StateBuilder.from_metadata(_metadata_for(env))
    actual = builder.build(t.to_pydatetime(), stations, prices, pv_forecast)

    assert actual.shape == expected.shape, f"shape mismatch: {actual.shape} vs {expected.shape}"
    np.testing.assert_allclose(actual, expected, rtol=1e-5, atol=1e-6)
    print(f"PASS ({len(expected)} values match)")


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


def main() -> None:
    sessions = make_sessions()
    prices = make_prices()
    pv = make_pv()

    ev = EVComponent(sessions, power_kw=9.0)
    check("EV only", EnergyEnv(prices, components=[ev], price_horizon=4))

    ev2 = EVComponent(sessions, power_kw=9.0)
    pv_comp = PVComponent(pv, forecast_horizon=4)
    check("EV + PV", EnergyEnv(prices, components=[ev2, pv_comp], price_horizon=4))

    print("\nAll parity checks passed.")


if __name__ == "__main__":
    main()
