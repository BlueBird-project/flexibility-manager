"""
Builds the exact normalised state vector the trained network expects, from raw
telemetry/forecast values — no pandas, no EnergyEnv, no dataset access.

This intentionally duplicates logic from src/env/base_env.py, ev_component.py
and pv_component.py rather than reusing them, because the live inputs are
fundamentally different in shape: training derives remaining_kwh/hours_left
from a sessions dataframe walked forward in time, while a live deployment gets
them handed directly by charger telemetry. Being a lightweight, pandas-free
duplicate is also what keeps the serving Docker image small.

Because it duplicates rather than shares code, it CAN drift from the training
side silently. If you touch any of:
  - src/env/base_env.py:EnergyEnv._build_state / observation_size
  - src/env/ev_component.py:EVComponent.get_state_features / normalize_state_slice
  - src/env/pv_component.py:PVComponent.get_state_features
update this file to match, then run `python service/test_state_parity.py` to
confirm the two are still identical on a synthetic scenario.
"""
from __future__ import annotations

import math
from dataclasses import dataclass, field
from datetime import datetime

import numpy as np


class StateBuilderError(ValueError):
    """Raised for any malformed or out-of-contract request payload."""


def encode_prices(quarter_prices, horizon, ratio, encoding, scale):
    """Duplicate of src/env/base_env.encode_prices — see there for the rationale."""
    q = [float(p) for p in quarter_prices]
    values = [q[0]] + [sum(q[1 + (k - 1) * ratio: 1 + k * ratio]) / ratio for k in range(1, horizon)]
    if encoding == "raw":
        return values
    lo, hi = min(values), max(values)
    span = hi - lo
    window = [0.5] * horizon if span < 1e-9 else [(v - lo) / span for v in values]
    level = max(-1.0, min(1.0, q[0] / scale))
    spread = max(0.0, min(1.0, span / scale))
    return window + [level, spread]


def apply_deadline_guard(state, actions, builder, margin):
    """
    Force charge on any station that can no longer meet its deadline.

    Duplicates main.make_deadline_guard() the same way this module duplicates the
    env's state construction, and for the same reason. Both read only the state
    vector, so the rule is `urgency > margin` in the station's feature block, where
    urgency is rescaled by `urgency_clip` when the checkpoint was trained clipped.
    Covered by service/test_state_parity.py.
    """
    if margin is None:
        return actions
    prefix = builder.state_prefix
    scale = builder.urgency_clip if builder.clip_features else 1.0
    for i in range(len(builder.station_ids)):
        if state[prefix + 4 * i] > 0.5 and state[prefix + 4 * i + 3] * scale > margin:
            actions[i] = 1
    return actions


@dataclass
class StateBuilder:
    """
    Read-only config derived from a checkpoint's metadata.json. Contains no
    tensors/torch state, so it can be constructed and unit-tested without
    loading a model.
    """

    price_horizon: int
    station_ids: list[int]
    power_kw: float
    norm_max_kwh: float
    norm_max_hours: float
    has_pv: bool = False
    pv_horizon: int = 0
    pv_norm_max_kwh: float | None = None
    interval_minutes: int = 15
    # Keep the EV features inside the [0, 1] box the training split defined. Absent
    # from pre-fix metadata.json, which was trained unclipped — hence default False,
    # so an old checkpoint keeps being served exactly the inputs it was trained on.
    clip_features: bool = False
    urgency_clip: float = 2.0
    # Price block (EnergyEnv.encode_prices). Absent from older metadata.json, whose
    # models took `price_horizon` raw quarter-hour prices — hence these defaults.
    price_encoding: str = "raw"
    price_step_minutes: int = 15
    price_scale: float = 1.0
    pv_clip_features: bool = False

    @property
    def prices_required(self) -> int:
        """Consecutive quarter-hour prices a request must send (mirrors EnergyEnv.prices_required)."""
        return 1 + (self.price_horizon - 1) * (self.price_step_minutes // self.interval_minutes)

    @property
    def state_prefix(self) -> int:
        # mirrors EnergyEnv.state_prefix: price features (+ level, spread) + sin/cos time
        return self.price_horizon + (2 if self.price_encoding == "window" else 0) + 2

    @property
    def observation_size(self) -> int:
        # mirrors EnergyEnv.observation_size: price block + time + EV block + PV block
        return self.state_prefix + 4 * len(self.station_ids) + (self.pv_horizon if self.has_pv else 0)

    @classmethod
    def from_metadata(cls, meta: dict) -> "StateBuilder":
        try:
            ev_state = meta["norm_state"]["EVComponent"]
            builder = cls(
                price_horizon=int(meta["price_horizon"]),
                station_ids=[int(s) for s in ev_state["station_ids"]],
                power_kw=float(ev_state["power_kw"]),
                norm_max_kwh=float(ev_state["norm_max_kwh"]),
                norm_max_hours=float(ev_state["norm_max_hours"]),
                interval_minutes=int(meta.get("interval_minutes", 15)),
                clip_features=bool(ev_state.get("clip_features", False)),
                urgency_clip=float(ev_state.get("urgency_clip", 2.0)),
            )
            env_state = meta["norm_state"].get("EnergyEnv")
            if env_state is not None:
                builder.price_encoding = str(env_state["price_encoding"])
                builder.price_horizon = int(env_state["price_horizon"])
                builder.price_step_minutes = int(env_state["price_step_minutes"])
                builder.price_scale = float(env_state["price_scale"])
            pv_state = meta["norm_state"].get("PVComponent")
            if pv_state is not None:
                builder.has_pv = True
                builder.pv_horizon = int(pv_state["forecast_horizon"])
                builder.pv_norm_max_kwh = float(pv_state["norm_max_kwh"])
                builder.pv_clip_features = bool(pv_state.get("clip_features", False))
        except KeyError as e:
            raise StateBuilderError(f"metadata.json is missing expected key: {e}") from e

        if builder.observation_size != int(meta["observation_size"]):
            raise StateBuilderError(
                f"metadata.json is inconsistent: computed observation_size="
                f"{builder.observation_size} but metadata says {meta['observation_size']}"
            )
        return builder

    # -- request parsing ---------------------------------------------------

    def build(self, timestamp: datetime, stations: list[dict], prices: list[float],
              pv_forecast: list[float] | None) -> np.ndarray:
        if len(prices) != self.prices_required:
            raise StateBuilderError(
                f"'prices' must have exactly {self.prices_required} consecutive quarter-hour "
                f"values starting at 'timestamp' (got {len(prices)})"
            )
        if self.has_pv:
            if pv_forecast is None:
                raise StateBuilderError(
                    f"this model was trained with PV; 'pv_forecast' with {self.pv_horizon} "
                    f"values is required"
                )
            if len(pv_forecast) != self.pv_horizon:
                raise StateBuilderError(
                    f"'pv_forecast' must have exactly {self.pv_horizon} values (got {len(pv_forecast)})"
                )
        elif pv_forecast:
            raise StateBuilderError("this model has no PV component; drop 'pv_forecast'")

        try:
            prices_f = encode_prices(prices, self.price_horizon,
                                     self.price_step_minutes // self.interval_minutes,
                                     self.price_encoding, self.price_scale)
        except (TypeError, ValueError) as e:
            raise StateBuilderError(f"'prices' must all be numbers: {e}") from e

        frac = (timestamp.hour * 60 + timestamp.minute) / 1440.0
        time_enc = [math.sin(2 * math.pi * frac), math.cos(2 * math.pi * frac)]

        by_id = self._index_stations(stations)
        unknown = sorted(set(by_id) - set(self.station_ids))
        if unknown:
            raise StateBuilderError(
                f"'stations' has unrecognised station_id(s) {unknown}; this model only knows "
                f"{self.station_ids}"
            )

        ev_feats: list[float] = []
        for sid in self.station_ids:
            st = by_id.get(sid)
            if st is None:
                raise StateBuilderError(
                    f"'stations' is missing station_id {sid} (expected all of {self.station_ids})"
                )
            ev_feats.extend(self._station_features(sid, st))

        pv_feats: list[float] = []
        if self.has_pv:
            try:
                pv_feats = [float(v) / self.pv_norm_max_kwh for v in pv_forecast]
                if self.pv_clip_features:
                    pv_feats = [min(v, 1.0) for v in pv_feats]
            except (TypeError, ValueError) as e:
                raise StateBuilderError(f"'pv_forecast' must all be numbers: {e}") from e

        return np.array(prices_f + time_enc + ev_feats + pv_feats, dtype=np.float32)

    def _index_stations(self, stations: list[dict]) -> dict[int, dict]:
        by_id: dict[int, dict] = {}
        for i, s in enumerate(stations):
            if "station_id" not in s:
                raise StateBuilderError(f"stations[{i}] is missing 'station_id'")
            try:
                sid = int(s["station_id"])
            except (TypeError, ValueError):
                raise StateBuilderError(f"stations[{i}].station_id must be an integer")
            by_id[sid] = s
        return by_id

    def _station_features(self, sid: int, st: dict) -> list[float]:
        # Mirrors EVComponent.get_state_features + normalize_state_slice exactly:
        # urgency is computed from RAW remaining/hours, and only remaining/hours
        # (not urgency, not present) are divided by the normalisation constants.
        present = bool(st.get("present", False))
        if not present:
            return [0.0, 0.0, 0.0, 0.0]
        try:
            remaining = float(st.get("remaining_kwh", 0.0))
            hours_left = float(st.get("hours_to_departure", 0.0))
        except (TypeError, ValueError):
            raise StateBuilderError(f"station_id {sid}: remaining_kwh/hours_to_departure must be numbers")
        if remaining < 0 or hours_left < 0:
            raise StateBuilderError(f"station_id {sid}: remaining_kwh and hours_to_departure must be >= 0")

        urgency = remaining / (hours_left * self.power_kw + 1e-6)
        remaining_n = remaining / self.norm_max_kwh
        hours_n = hours_left / self.norm_max_hours
        if self.clip_features:
            remaining_n = min(remaining_n, 1.0)
            hours_n = min(hours_n, 1.0)
            urgency = min(urgency, self.urgency_clip) / self.urgency_clip
        return [1.0, remaining_n, hours_n, urgency]
