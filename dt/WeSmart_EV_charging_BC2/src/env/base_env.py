import numpy as np
import pandas as pd
from abc import ABC, abstractmethod
from typing import Optional


# ──────────────────────────────────────────────────────────────────────────────
#  Contract every device plugin must satisfy
# ──────────────────────────────────────────────────────────────────────────────

class BaseComponent(ABC):

    @property
    @abstractmethod
    def n_actions(self) -> int:
        """How many actions this component needs per step."""

    @property
    @abstractmethod
    def n_state_features(self) -> int:
        """How many floats this component adds to the state vector."""

    @abstractmethod
    def reset(self, t_start: pd.Timestamp) -> None:
        """Reset internal state at the start of an episode."""

    @abstractmethod
    def sync(self, t: pd.Timestamp) -> None:
        """Called after time advances — update bookkeeping (arrivals, etc.)."""

    @abstractmethod
    def get_state_features(self, t: pd.Timestamp) -> list[float]:
        """Return a flat list of n_state_features floats describing interval t.

        NOTE: t is the *look-ahead* interval (current_time + one interval), so it
        may not have been sync()'d yet. Implementations must derive their features
        from the data for t rather than assuming runtime state is current.
        """

    @abstractmethod
    def apply_actions(self, actions: np.ndarray, t: pd.Timestamp, price: float, shared: dict) -> tuple[float, dict]:
        """Apply this component's action slice. Return (reward_contribution, info).

        shared: dict populated by produce() calls — read available energy from it,
        and write back what you consumed so later components and the episode
        metrics can see it. This is the only supported channel for cross-component
        values; the returned info dict is namespaced per component and is not
        visible to other components.
        """

    @abstractmethod
    def check_departures(self, t: pd.Timestamp, t_end: pd.Timestamp) -> tuple[float, dict]:
        """Check departures / expirations. Return (reward_contribution, info)."""

    # Optional — override for source components (PV, battery discharging, ...)
    def produce(self, t: pd.Timestamp) -> dict:
        """
        Report energy available from this component at time t.
        Called before apply_actions so loads can read supply from shared context.
        Example: PV returns {"pv_available_kwh": 2.1}
        """
        return {}

    # Optional — override only if your component can be idle
    def is_active_at(self, t: pd.Timestamp, interval_td: pd.Timedelta) -> bool:
        return True

    def next_active_from(self, t: pd.Timestamp, t_max: pd.Timestamp, interval_td: pd.Timedelta) -> Optional[pd.Timestamp]:
        return t

    def on_skip(self, t: pd.Timestamp, t_end: pd.Timestamp) -> float:
        return 0.0

    def normalize_state_slice(self, state_slice: np.ndarray) -> np.ndarray:
        return state_slice

    def get_episode_log(self) -> Optional[pd.DataFrame]:
        return None

    def get_episode_metrics(self, info_history: list[dict]) -> dict:
        """
        Return a flat dict of scalar metrics for this component.
        Called at the end of each episode for printing/logging.

        info_history is a list of per-step info dicts shaped as
        {"<ComponentClassName>": {...}, ..., "shared": {...}} — read your own
        namespace with info.get(type(self).__name__, {}).

        Default: empty (component contributes nothing to the printout).
        """
        return {}

    # Optional — override to persist/restore normalisation constants alongside a
    # checkpoint, so evaluation scales inputs exactly the way training did.
    def get_norm_state(self) -> dict:
        return {}

    def set_norm_state(self, state: dict) -> None:
        pass


# ──────────────────────────────────────────────────────────────────────────────
#  Core environment — orchestrates time, prices, and attached components
# ──────────────────────────────────────────────────────────────────────────────

class EnergyEnv:
    """
    Modular RL environment. Attach any combination of BaseComponent devices.

    Usage:
        ev  = EVComponent(sessions_df, ...)
        env = EnergyEnv(price_df, components=[ev])
        state = env.reset()
        state, reward, done, info = env.step(actions)

    Timing contract
    ---------------
    The state returned by reset()/step() describes the interval
    ``current_time + interval``, which is exactly the interval the *next*
    step()'s actions will be applied to. Skipping idle stretches preserves this
    alignment: the env lands one interval *before* the next active interval so
    that interval still receives an action.
    """

    def __init__(
        self,
        price_df: pd.DataFrame,
        components: list[BaseComponent],
        interval_minutes: int = 15,
        price_horizon: int = 4,
        skip_empty: bool = True,
    ):
        """
        price_horizon: number of forward-looking price values in the state, starting
            at the interval about to be decided (t, t+1, ..., t+price_horizon-1).
            Belgian day-ahead prices are published a day ahead, so this is a known
            schedule, not a forecast — a live deployment can supply it exactly.
        """
        self.components = components
        self.interval_minutes = interval_minutes
        self.interval_td = pd.Timedelta(minutes=interval_minutes)
        self.price_horizon = price_horizon
        self.skip_empty = skip_empty

        # precompute action / state slices for each component
        self._action_slices, self._state_slices = [], []
        a = s = 0
        for c in components:
            self._action_slices.append(slice(a, a + c.n_actions));  a += c.n_actions
            self._state_slices.append(slice(s, s + c.n_state_features)); s += c.n_state_features

        self._load_prices(price_df)
        self.current_time: Optional[pd.Timestamp] = None
        self.done = False

    # ── price helpers ─────────────────────────────────────────────────────────

    def _load_prices(self, price_df: pd.DataFrame) -> None:
        price_df = price_df.copy()

        if "date" in price_df.columns:
            dates = pd.to_datetime(price_df["date"])
        elif "timestamp" in price_df.columns and "Tijd" in price_df.columns:
            dates = pd.to_datetime(
                price_df["timestamp"].astype(str).str.strip() + " " + price_df["Tijd"].astype(str).str.strip(),
                dayfirst=True,
            )
        elif "timestamp" in price_df.columns:
            dates = pd.to_datetime(price_df["timestamp"])
        else:
            raise KeyError("price_df needs a 'date' column, or 'timestamp' (+ optional 'Tijd').")

        if dates.isna().any():
            raise ValueError(f"price_df has {int(dates.isna().sum())} unparseable timestamps.")
        if "price" not in price_df.columns:
            raise KeyError("price_df must have a 'price' column.")

        price_df["date"] = dates
        price_df = (
            price_df.sort_values("date")
            .drop_duplicates(subset="date", keep="last")
            .reset_index(drop=True)
        )

        self.price_df = price_df
        self.price_series = price_df.set_index("date")["price"]

        # O(log n) lookup arrays — _price() is called several times per step, so a
        # boolean mask over the whole series here dominates the runtime.
        self._price_times = self.price_series.index.to_numpy(dtype="datetime64[ns]")
        self._price_values = self.price_series.to_numpy(dtype=np.float64)

        self.t_min = self._floor(price_df["date"].min())
        self.t_max = price_df["date"].max()

    def _floor(self, t: pd.Timestamp) -> pd.Timestamp:
        m = t.hour * 60 + t.minute
        return t.normalize() + pd.Timedelta(minutes=(m // self.interval_minutes) * self.interval_minutes)

    def _price(self, t: pd.Timestamp) -> float:
        """Most recent price at or before t (first price if t precedes the series)."""
        i = int(np.searchsorted(self._price_times, np.datetime64(t, "ns"), side="right")) - 1
        return float(self._price_values[i if i >= 0 else 0])

    def _prices(self, times: list[pd.Timestamp]) -> list[float]:
        """Vectorised _price() for several timestamps at once."""
        query = np.array([np.datetime64(t, "ns") for t in times], dtype="datetime64[ns]")
        idx = np.searchsorted(self._price_times, query, side="right") - 1
        np.clip(idx, 0, None, out=idx)
        return self._price_values[idx].tolist()

    # ── sizes ─────────────────────────────────────────────────────────────────

    @property
    def observation_size(self) -> int:
        return self.price_horizon + 2 + sum(c.n_state_features for c in self.components)

    @property
    def action_size(self) -> int:
        return sum(c.n_actions for c in self.components)

    # ── normalisation state (persisted alongside checkpoints) ─────────────────

    def get_norm_state(self) -> dict:
        """Collect every component's normalisation constants, keyed by class name."""
        return {type(c).__name__: c.get_norm_state() for c in self.components}

    def set_norm_state(self, state: dict) -> None:
        """Restore normalisation constants captured by get_norm_state()."""
        for c in self.components:
            payload = (state or {}).get(type(c).__name__)
            if payload:
                c.set_norm_state(payload)

    # ── state ─────────────────────────────────────────────────────────────────

    def _build_state(self) -> np.ndarray:
        t = self.current_time + self.interval_td
        prices = self._prices([t + k * self.interval_td for k in range(self.price_horizon)])
        frac = (t.hour * 60 + t.minute) / 1440
        time_enc = [np.sin(2 * np.pi * frac), np.cos(2 * np.pi * frac)]
        feats = []
        for c in self.components:
            feats.extend(c.get_state_features(t))
        return np.array(prices + time_enc + feats, dtype=np.float32)

    # ── reset / step ──────────────────────────────────────────────────────────

    def reset(self, start_date=None) -> np.ndarray:
        """Start a new episode.

        start_date: optional timestamp to start from (floored to the interval
        grid). Defaults to the beginning of the price series.
        """
        if start_date is None:
            self.current_time = self.t_min
        else:
            ts = self._floor(pd.Timestamp(start_date))
            if not (self.t_min <= ts <= self.t_max):
                raise ValueError(
                    f"start_date {ts} is outside the price range {self.t_min} .. {self.t_max}"
                )
            self.current_time = ts

        self.done = False
        for c in self.components:
            c.reset(self.current_time)
        if self.skip_empty:
            self._skip()
        return self._build_state()

    def step(self, actions: np.ndarray) -> tuple[np.ndarray, float, bool, dict]:
        assert not self.done, "Episode finished — call reset()."
        actions = np.asarray(actions, dtype=np.int32)

        self.current_time += self.interval_td
        if self.current_time > self.t_max:
            self.done = True
            return self._build_state(), 0.0, True, {}

        for c in self.components:
            c.sync(self.current_time)

        price = self._price(self.current_time)
        t_end = self.current_time + self.interval_td
        reward, info = 0.0, {}

        # 1. produce phase — sources report available energy
        shared = {}
        for c in self.components:
            shared.update(c.produce(self.current_time))

        # 2. action phase — loads consume, reading (and writing back to) shared
        for c, sl in zip(self.components, self._action_slices):
            r, i = c.apply_actions(actions[sl], self.current_time, price, shared)
            reward += r
            info.setdefault(type(c).__name__, {}).update(i)

        # 3. departure phase
        for c in self.components:
            r, i = c.check_departures(self.current_time, t_end)
            reward += r
            info.setdefault(type(c).__name__, {}).update(i)

        # cross-component values live in shared, exposed once under a fixed key
        info["shared"] = dict(shared)
        info["price"] = price

        if self.skip_empty:
            reward += self._skip()

        if self.current_time > self.t_max:
            self.done = True

        return self._build_state(), reward, self.done, info

    # ── skip-empty ────────────────────────────────────────────────────────────

    def _skip(self) -> float:
        """
        Fast-forward over stretches where no component is active.

        Checks the interval the *next* action would apply to
        (``current_time + interval``) and lands one interval before the next
        active one, so the first active interval after a gap still gets an
        action instead of being stepped over.
        """
        penalty = 0.0
        while True:
            t_next = self.current_time + self.interval_td
            if t_next > self.t_max:
                self.done = True
                break
            if any(c.is_active_at(t_next, self.interval_td) for c in self.components):
                break

            # nothing to do in t_next — settle anything that expired, then jump
            for c in self.components:
                penalty += c.on_skip(t_next, t_next + self.interval_td)

            candidates = [
                c.next_active_from(t_next + self.interval_td, self.t_max, self.interval_td)
                for c in self.components
            ]
            valid = [x for x in candidates if x is not None]
            target = min(valid) if valid else None
            if target is None or target > self.t_max:
                self.current_time = self.t_max + self.interval_td
                self.done = True
                break

            # land one interval early so `target` itself is acted on next step;
            # target >= t_next + interval guarantees forward progress
            self.current_time = target - self.interval_td
            for c in self.components:
                c.sync(self.current_time)
        return penalty

    # ── helpers ───────────────────────────────────────────────────────────────

    def normalize_state(self, state: np.ndarray) -> np.ndarray:
        out = state.copy()
        prefix = self.price_horizon + 2
        for c, sl in zip(self.components, self._state_slices):
            out[prefix + sl.start: prefix + sl.stop] = c.normalize_state_slice(out[prefix + sl.start: prefix + sl.stop])
        return out

    def reload_data(self, price_df: pd.DataFrame, sessions_df: pd.DataFrame = None, start_date=None) -> np.ndarray:
        """Swap in new price/session data.

        Normalisation constants are deliberately left untouched — a model must
        keep seeing inputs on the scale it was trained with.
        """
        self._load_prices(price_df)
        if sessions_df is not None:
            for c in self.components:
                if hasattr(c, "reload_sessions"):
                    c.reload_sessions(sessions_df)
        return self.reset(start_date=start_date)

    def get_episode_logs(self) -> dict:
        return {type(c).__name__: c.get_episode_log() for c in self.components if c.get_episode_log() is not None}

    def __repr__(self) -> str:
        return (f"EnergyEnv(components=[{', '.join(type(c).__name__ for c in self.components)}], "
                f"obs={self.observation_size}, actions={self.action_size})")
