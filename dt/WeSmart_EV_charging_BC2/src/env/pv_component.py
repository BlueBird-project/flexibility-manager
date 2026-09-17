import numpy as np
import pandas as pd

from src.env.base_env import BaseComponent


def _parse_timestamps(series: pd.Series, label: str) -> pd.Series:
    """
    Parse a timestamp column without guessing wrong on ambiguous day/month order.

    The datasets in this repo mix ISO (`2025-01-01 00:00:00`, solar) and
    day-first (`01/01/2025 00:00`, building consumption), so try the day-first
    format explicitly before falling back to inference.
    """
    for fmt in ("%d/%m/%Y %H:%M", "%d/%m/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"):
        parsed = pd.to_datetime(series, format=fmt, errors="coerce")
        if parsed.notna().all():
            return parsed
    parsed = pd.to_datetime(series, errors="coerce")
    if parsed.isna().any():
        raise ValueError(f"Could not parse {int(parsed.isna().sum())} timestamps in {label}.")
    return parsed


class PVComponent(BaseComponent):
    """
    PV (solar) production as a pluggable source component.

    PV energy is always self-consumed automatically — no action needed.
    The env's produce phase calls produce() to report available kWh into the
    shared context; EVComponent draws from it and writes back what it used.

    State features (forecast_horizon values, normalised):
        [pv(t), pv(t+1), ..., pv(t+forecast_horizon-1)]
    where t is the interval about to be decided. During training this is read
    straight off the historical production series — i.e. training assumes a
    perfect-foresight forecast. A live deployment feeds an actual weather-driven
    forecast here instead, which will have real error the model never saw during
    training; keep the horizon short (this defaults to 1 hour) to limit how much
    that mismatch can bite.

    Dataset must have:
        Timestamp       — datetime column (sep=";")
        Production_kWh  — energy produced per 15-min interval in kWh
    """

    def __init__(
        self,
        pv_df: pd.DataFrame,
        consumption_df: pd.DataFrame = None,
        interval_minutes: int = 15,
        forecast_horizon: int = 4,
        time_col: str = "Timestamp",
        production_col: str = "Production_kWh",
        consumption_col: str = "Consumption_kWh",
        clip_features: bool = False,
    ):
        """
        clip_features: cap the normalised forecast at 1.0. norm_max_kwh is the maximum
            of the historical series, so training never exceeds 1 — but a live forecast
            can, which would put the network outside the range it was trained on (the
            same failure EVComponent.clip_features guards against). Persisted in
            metadata; absent (pre-change checkpoints) means unclipped.
        """
        self.clip_features = clip_features
        pv_df = pv_df.copy()
        if time_col not in pv_df.columns or production_col not in pv_df.columns:
            raise KeyError(f"pv_df must have '{time_col}' and '{production_col}' columns; got {list(pv_df.columns)}")
        pv_df[time_col] = _parse_timestamps(pv_df[time_col], "pv_df")
        pv_df = pv_df.sort_values(time_col).reset_index(drop=True)

        self.interval_minutes = interval_minutes
        self.interval_td = pd.Timedelta(minutes=interval_minutes)
        self.forecast_horizon = forecast_horizon

        pv_df["_t"] = pv_df[time_col].dt.floor(f"{interval_minutes}min")
        series = pv_df.set_index("_t")[production_col].astype(float)
        self._pv_series = series[~series.index.duplicated(keep="last")].sort_index()

        # optional building consumption — net available = max(0, pv - consumption)
        if consumption_df is not None:
            consumption_df = consumption_df.copy()
            if time_col not in consumption_df.columns or consumption_col not in consumption_df.columns:
                raise KeyError(
                    f"consumption_df must have '{time_col}' and '{consumption_col}' columns; "
                    f"got {list(consumption_df.columns)}"
                )
            consumption_df[time_col] = _parse_timestamps(consumption_df[time_col], "consumption_df")
            consumption_df["_t"] = consumption_df[time_col].dt.floor(f"{interval_minutes}min")
            cseries = consumption_df.set_index("_t")[consumption_col].astype(float)
            self._consumption_series = cseries[~cseries.index.duplicated(keep="last")].sort_index()
        else:
            self._consumption_series = None

        # O(1) lookups instead of a try/except .at[] per call
        net = self._pv_series
        if self._consumption_series is not None:
            net = (net - self._consumption_series.reindex(net.index, fill_value=0.0)).clip(lower=0.0)
        self._net_series = net

        self.norm_max_kwh = float(net.max()) if len(net) and float(net.max()) > 0 else 1.0

    # ── BaseComponent ─────────────────────────────────────────────────────────

    @property
    def n_actions(self) -> int:
        return 0  # PV is automatic — no agent decision needed

    @property
    def n_state_features(self) -> int:
        return self.forecast_horizon

    @property
    def data_start(self) -> pd.Timestamp:
        return self._pv_series.index.min()

    @property
    def data_end(self) -> pd.Timestamp:
        return self._pv_series.index.max()

    def reset(self, t_start: pd.Timestamp) -> None:
        pass  # stateless

    def sync(self, t: pd.Timestamp) -> None:
        pass  # stateless

    # ── skip-empty ────────────────────────────────────────────────────────────
    # PV takes no actions and nothing can consume its output when every load is
    # idle, so it must not keep the episode alive on its own. Inheriting the
    # default is_active_at() -> True disables skipping for the whole env: an
    # EV+PV episode then steps through the entire price range (70k intervals,
    # including a year with no PV or session data) instead of the ~11k intervals
    # where an EV is actually plugged in, flooding the replay buffer with
    # zero-reward transitions.

    def is_active_at(self, t: pd.Timestamp, interval_td: pd.Timedelta) -> bool:
        return False

    def next_active_from(self, t: pd.Timestamp, t_max: pd.Timestamp, interval_td: pd.Timedelta):
        return None

    # ── produce phase — called before apply_actions ───────────────────────────

    def produce(self, t: pd.Timestamp) -> dict:
        """Report net available PV energy (production - building consumption) into shared context."""
        return {"pv_available_kwh": self._get_net_kwh(t)}

    # ── state ─────────────────────────────────────────────────────────────────

    def get_state_features(self, t: pd.Timestamp) -> list[float]:
        feats = [
            self._get_net_kwh(t + k * self.interval_td) / self.norm_max_kwh
            for k in range(self.forecast_horizon)
        ]
        return [min(v, 1.0) for v in feats] if self.clip_features else feats

    # ── no-ops (PV has no actions, no departures) ─────────────────────────────

    def apply_actions(self, actions: np.ndarray, t: pd.Timestamp, price: float, shared: dict) -> tuple[float, dict]:
        return 0.0, {
            "pv_available_kwh":  self._get_net_kwh(t),   # gross available, before EV draw
            "pv_produced_kwh":   self._get_kwh(t),
            "pv_consumed_kwh":   self._get_consumption_kwh(t),
        }

    def check_departures(self, t: pd.Timestamp, t_end: pd.Timestamp) -> tuple[float, dict]:
        return 0.0, {}

    # ── metrics ───────────────────────────────────────────────────────────────

    def get_episode_metrics(self, info_history: list[dict]) -> dict:
        me = type(self).__name__
        produced  = sum(float(i.get(me, {}).get("pv_produced_kwh",  0.0)) for i in info_history)
        consumed  = sum(float(i.get(me, {}).get("pv_consumed_kwh",  0.0)) for i in info_history)
        available = sum(float(i.get(me, {}).get("pv_available_kwh", 0.0)) for i in info_history)
        # what the loads actually took comes through the shared channel, so this
        # stays correct no matter which load components are attached
        used      = sum(float(i.get("shared", {}).get("pv_used_kwh", 0.0)) for i in info_history)
        return {
            "produced_kwh":   produced,
            "building_kwh":   consumed,
            "available_kwh":  available,
            "used_ev_kwh":    used,
            "exported_kwh":   max(0.0, available - used),
            "self_rate":      used / available if available > 0 else 0.0,
        }

    def get_norm_state(self) -> dict:
        return {
            "norm_max_kwh": float(self.norm_max_kwh),
            "forecast_horizon": int(self.forecast_horizon),
            "clip_features": bool(self.clip_features),
        }

    def set_norm_state(self, state: dict) -> None:
        if "norm_max_kwh" in state:
            self.norm_max_kwh = float(state["norm_max_kwh"])
        self.clip_features = bool(state.get("clip_features", False))
        saved_horizon = state.get("forecast_horizon")
        if saved_horizon is not None and int(saved_horizon) != self.forecast_horizon:
            raise ValueError(
                f"Checkpoint was trained with PV forecast_horizon={saved_horizon} but this "
                f"env uses {self.forecast_horizon}; the state vector would not line up."
            )

    # ── internals ─────────────────────────────────────────────────────────────

    def _get_kwh(self, t: pd.Timestamp) -> float:
        v = self._pv_series.get(self._floor(t))
        return 0.0 if v is None else float(v)

    def _get_consumption_kwh(self, t: pd.Timestamp) -> float:
        if self._consumption_series is None:
            return 0.0
        v = self._consumption_series.get(self._floor(t))
        return 0.0 if v is None else float(v)

    def _get_net_kwh(self, t: pd.Timestamp) -> float:
        v = self._net_series.get(self._floor(t))
        return 0.0 if v is None else max(0.0, float(v))

    def _floor(self, t: pd.Timestamp) -> pd.Timestamp:
        m = t.hour * 60 + t.minute
        return t.normalize() + pd.Timedelta(minutes=(m // self.interval_minutes) * self.interval_minutes)
