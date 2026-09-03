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

    @property
    def observation_size(self) -> int:
        # mirrors EnergyEnv.observation_size: price_horizon + 2 (time) + EV block + PV block
        return self.price_horizon + 2 + 4 * len(self.station_ids) + (self.pv_horizon if self.has_pv else 0)

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
            )
            pv_state = meta["norm_state"].get("PVComponent")
            if pv_state is not None:
                builder.has_pv = True
                builder.pv_horizon = int(pv_state["forecast_horizon"])
                builder.pv_norm_max_kwh = float(pv_state["norm_max_kwh"])
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
        if len(prices) != self.price_horizon:
            raise StateBuilderError(
                f"'prices' must have exactly {self.price_horizon} values (got {len(prices)})"
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
            prices_f = [float(p) for p in prices]
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
        return [1.0, remaining / self.norm_max_kwh, hours_left / self.norm_max_hours, urgency]
