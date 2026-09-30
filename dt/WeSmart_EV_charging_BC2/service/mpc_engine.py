"""MPC engine behind the same HTTP contract as the trained-model engine.

`MpcBundle` is a drop-in alternative to `ModelBundle`: same `decide(payload)` and `health()`,
so `inference_server.py`'s parsing, validation and error handling are shared and the request
and response formats are identical to the RL service. The only visible differences are how
many forward-looking values a request carries (see `/health`: `prices_required`,
`pv_required`) and `engine: "mpc"`.

Per call it runs ONE receding-horizon MPC step via `mpc_decide()`: plan from `now` to the last
departure, commit only the interval starting at `now`, discard the rest. The 15-minute loop is
the caller's (a tick, or a car arriving mid-interval — both are fine, the grid is built from
`now`, so a call at 10:38 decides the 10:38-10:45 stub correctly).

Stateless, like the RL engine: every request carries the full current picture
(`remaining_kwh`, `hours_to_departure` per station) and nothing is remembered between calls.

Configuration comes from the environment, so no trained model is needed:
    MPC_PRICE_QUARTERS      how many consecutive 15-min prices a request must send (default 96 = 24 h)
    MPC_PV_QUARTERS         same for the PV forecast (default 96); ignored unless MPC_HAS_PV
    MPC_HAS_PV              1 to require a `pv_forecast` (net of building consumption)
    MPC_DEFAULT_POWER_KW    charging power when a station does not state its own (default 9.0)
    MPC_SITE_MAX_POWER_KW   optional cap on total power across all EVs (unset = no cap)
    MPC_TIME_LIMIT_S        solver time limit per call (default 10)
"""
from __future__ import annotations

import logging
import os
from datetime import datetime, timedelta

from service.errors import StateBuilderError
from src.mpc.mpc import SLOT, EVSession, mpc_decide, slot_start

INTERVAL_MINUTES = 15


def _env_float(name: str, default: float | None) -> float | None:
    raw = os.environ.get(name)
    return default if raw is None or raw == "" else float(raw)


def _env_int(name: str, default: int) -> int:
    raw = os.environ.get(name)
    return default if raw is None or raw == "" else int(raw)


class MpcBundle:
    """Answers /decide by solving one MPC step. No model, no weights, no torch."""

    def __init__(self, price_quarters: int = 96, pv_quarters: int = 96, has_pv: bool = False,
                 default_power_kw: float = 9.0, site_max_power_kw: float | None = None,
                 time_limit_s: float = 10.0):
        self.price_quarters = price_quarters
        self.pv_quarters = pv_quarters
        self.has_pv = has_pv
        self.default_power_kw = default_power_kw
        self.site_max_power_kw = site_max_power_kw
        self.time_limit_s = time_limit_s

    @classmethod
    def from_env(cls) -> "MpcBundle":
        return cls(price_quarters=_env_int("MPC_PRICE_QUARTERS", 96),
                   pv_quarters=_env_int("MPC_PV_QUARTERS", 96),
                   has_pv=os.environ.get("MPC_HAS_PV", "0").lower() in ("1", "true", "yes"),
                   default_power_kw=_env_float("MPC_DEFAULT_POWER_KW", 9.0),
                   site_max_power_kw=_env_float("MPC_SITE_MAX_POWER_KW", None),
                   time_limit_s=_env_float("MPC_TIME_LIMIT_S", 10.0))

    # -- request handling ---------------------------------------------------

    def decide(self, payload: dict) -> dict:
        if "timestamp" not in payload:
            raise StateBuilderError("'timestamp' is required")
        ts_raw = payload["timestamp"]
        try:
            now = datetime.fromisoformat(ts_raw)
        except (TypeError, ValueError) as e:
            raise StateBuilderError(f"'timestamp' must be ISO-8601: {e}") from e

        stations = payload.get("stations")
        if not isinstance(stations, list) or not stations:
            raise StateBuilderError("'stations' must be a non-empty list")

        prices = self._series(payload.get("prices"), "prices", self.price_quarters, now)
        pv = None
        if self.has_pv:
            pv = self._series(payload.get("pv_forecast"), "pv_forecast", self.pv_quarters, now)
        elif payload.get("pv_forecast"):
            raise StateBuilderError("this service is configured without PV; drop 'pv_forecast'")

        evs, ids = [], []
        for i, st in enumerate(stations):
            if "station_id" not in st:
                raise StateBuilderError(f"stations[{i}] is missing 'station_id'")
            try:
                sid = int(st["station_id"])
            except (TypeError, ValueError):
                raise StateBuilderError(f"stations[{i}].station_id must be an integer")
            ids.append(sid)
            if not bool(st.get("present", False)):
                continue
            try:
                remaining = float(st.get("remaining_kwh", 0.0))
                hours_left = float(st.get("hours_to_departure", 0.0))
                power = float(st.get("max_power_kw", self.default_power_kw))
            except (TypeError, ValueError):
                raise StateBuilderError(
                    f"station_id {sid}: remaining_kwh / hours_to_departure / max_power_kw must be numbers")
            if remaining < 0 or hours_left < 0 or power <= 0:
                raise StateBuilderError(
                    f"station_id {sid}: remaining_kwh and hours_to_departure must be >= 0, max_power_kw > 0")
            # nothing to deliver, or no time left: no decision to make
            if remaining <= 0 or hours_left <= 0:
                continue
            evs.append(EVSession(ev_id=str(sid), charger_id=str(sid), arrival_time=now,
                                 departure_time=now + timedelta(hours=hours_left),
                                 energy_needed_kwh=remaining, max_charging_power_kw=power))

        charge = {sid: 0 for sid in ids}
        if evs:  # no connected EV that needs energy -> nothing to solve, everything stays 0
            result = mpc_decide(evs, prices, now, site_max_power_kw=self.site_max_power_kw,
                                pv_forecast=pv, time_limit_s=self.time_limit_s)
            decisions = result.get("decisions") or {}
            if not decisions:
                # no solution at all (e.g. a site cap makes it infeasible): charge everything
                # that needs energy rather than returning nothing
                logging.warning("MPC returned no decision (status=%s); charging all connected EVs",
                                result.get("status"))
                for ev in evs:
                    charge[int(ev.ev_id)] = 1
            else:
                for ev_id, value in decisions.items():
                    charge[int(ev_id)] = int(value)

        return {"timestamp": ts_raw,
                "decisions": [{"station_id": sid, "charge": charge[sid]} for sid in ids]}

    def _series(self, values, field: str, count: int, now: datetime) -> dict:
        """Turn a list of consecutive 15-min values starting at `now` into a slot-keyed mapping."""
        if values is None:
            raise StateBuilderError(f"'{field}' with {count} values is required")
        if not isinstance(values, list):
            raise StateBuilderError(f"'{field}' must be a list")
        if len(values) != count:
            raise StateBuilderError(
                f"'{field}' must have exactly {count} consecutive quarter-hour values "
                f"starting at 'timestamp' (got {len(values)})")
        try:
            floats = [float(v) for v in values]
        except (TypeError, ValueError) as e:
            raise StateBuilderError(f"'{field}' must all be numbers: {e}") from e
        first = slot_start(now)
        return {first + k * SLOT: v for k, v in enumerate(floats)}

    def health(self) -> dict:
        return {
            "status": "ok",
            "engine": "mpc",
            "station_ids": None,          # station-agnostic: any id may be sent
            "power_kw": self.default_power_kw,
            "prices_required": self.price_quarters,
            "has_pv": self.has_pv,
            "pv_required": self.pv_quarters if self.has_pv else 0,
            "interval_minutes": INTERVAL_MINUTES,
            "site_max_power_kw": self.site_max_power_kw,
            "time_limit_s": self.time_limit_s,
        }
