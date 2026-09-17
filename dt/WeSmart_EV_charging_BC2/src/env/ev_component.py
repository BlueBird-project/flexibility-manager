import numpy as np
import pandas as pd
from typing import NamedTuple, Optional

from src.env.base_env import BaseComponent


class Session(NamedTuple):
    """Lightweight view of one charging session (replaces per-step DataFrame row lookups)."""
    sid: int
    arrival: pd.Timestamp
    departure: pd.Timestamp
    need_kwh: float


# urgency = remaining_kwh / (hours_left * power_kw): 1.0 means "must charge at full
# power every remaining interval", >1 means already infeasible. Every control-relevant
# distinction lives below 2.0, but the raw ratio is unbounded as hours_left -> 0 (it hits
# 16 on train and 21 on test under an exploring policy), which puts a single feature 20x
# above the 0..1 scale of every other input. Clip there and rescale to the unit box.
URGENCY_CLIP = 2.0


class EVComponent(BaseComponent):
    """
    EV charging logic as a pluggable component.

    State features per station (4 values):
        ev_present, remaining_kwh, hours_until_departure, urgency

    Action per station:
        0 = don't charge,  1 = charge
    """

    def __init__(
        self,
        sessions_df: pd.DataFrame,
        power_kw: float = 9.0,
        interval_minutes: int = 15,
        penalty_per_kwh: float = 5.0,
        fixed_penalty: float = 10.0,
        progress_penalty: float | None = None,
        feasibility_margin: float = 0.96,
        full_tolerance: float = 0.04,
        clip_features: bool = True,
    ):
        """
        power_kw:            constant charging rate used for every station.
        penalty_per_kwh:     € per kWh still missing when an EV departs.
        fixed_penalty:       flat € charged on top of an undercharged departure.
        progress_penalty:    € charged *every interval* while an EV is already
                             behind schedule (shaping). Defaults to
                             fixed_penalty / 2. Note this compounds over the
                             session — set it to 0.0 to train on the terminal
                             penalty alone.
        feasibility_margin:  fraction of the theoretical max charge rate assumed
                             usable when deciding whether a session is still on
                             track.
        full_tolerance:      fraction of the requirement that may remain at
                             departure without counting as undercharged.
        clip_features:       keep the normalised state features inside the [0, 1]
                             box the training split defines. Without it a session
                             larger or longer than anything in training (the test
                             split has one at 1.54x norm_max_kwh and 1.51x
                             norm_max_hours) pushes the network outside the domain
                             it ever saw, where a ReLU MLP extrapolates freely.
                             Set False only to reproduce pre-fix checkpoints.
        """
        self.power_kw = power_kw
        self.interval_minutes = interval_minutes
        self.interval_td = pd.Timedelta(minutes=interval_minutes)
        self.penalty_per_kwh = penalty_per_kwh
        self.fixed_penalty = fixed_penalty
        self.progress_penalty = fixed_penalty / 2 if progress_penalty is None else progress_penalty
        self.feasibility_margin = feasibility_margin
        self.full_tolerance = full_tolerance
        self.clip_features = clip_features
        self.urgency_clip = URGENCY_CLIP

        self._set_sessions(sessions_df)

        # normalisation constants are captured from the data the component is
        # first built on (i.e. the training split) and then frozen — see
        # get_norm_state()/set_norm_state().
        self.norm_max_kwh = float(self._need_series.max()) or 1.0
        self.norm_max_hours = float(
            (self.sessions_df["departure"] - self.sessions_df["arrival"]).dt.total_seconds().max() / 3600
        ) or 1.0

        # runtime (set in reset)
        self.remaining_kwh = np.zeros(self.n_stations)
        self.cumulative_cost = np.zeros(self.n_stations)
        self._active_ids: dict = {}
        self._session_acc: dict = {}
        self._step_log: list[dict] = []

    # ── session indexing ──────────────────────────────────────────────────────

    def _set_sessions(self, sessions_df: pd.DataFrame) -> None:
        sessions_df = sessions_df.copy()
        sessions_df["arrival"] = pd.to_datetime(sessions_df["arrival"])
        sessions_df["departure"] = pd.to_datetime(sessions_df["departure"])
        self.sessions_df = sessions_df.sort_values("arrival").reset_index(drop=True)

        need_col = self._need_col()
        self._need_series = self.sessions_df[need_col].astype(float)

        self.station_ids = sorted(self.sessions_df["station_id"].unique())
        self.n_stations = len(self.station_ids)
        self.station_idx = {sid: i for i, sid in enumerate(self.station_ids)}

        self._build_index()

    def _build_index(self) -> None:
        """
        Per-station sorted arrays so _sessions_at() is a binary search instead of
        a full DataFrame filter + iterrows() on every call (it runs 3-4x/step).
        """
        self._idx_arr, self._idx_dep, self._idx_maxdep, self._idx_sess = [], [], [], []
        need_col = self._need_col()
        for sid in self.station_ids:
            blk = self.sessions_df[self.sessions_df["station_id"] == sid].sort_values("arrival")
            arr = blk["arrival"].to_numpy(dtype="datetime64[ns]")
            dep = blk["departure"].to_numpy(dtype="datetime64[ns]")
            self._idx_arr.append(arr)
            self._idx_dep.append(dep)
            # running max of departure, so a backward scan can stop as soon as no
            # earlier session could possibly still be present
            self._idx_maxdep.append(np.maximum.accumulate(dep) if len(dep) else dep)
            self._idx_sess.append([
                Session(int(i), a, d, float(n))
                for i, a, d, n in zip(blk.index, blk["arrival"], blk["departure"], blk[need_col].astype(float))
            ])
        self._cache: dict = {}

    def _sessions_at(self, t: pd.Timestamp) -> dict:
        """{station_idx: Session} for every station occupied during [t, t+interval)."""
        key = t.value
        hit = self._cache.get(key)
        if hit is not None:
            return hit

        t64 = np.datetime64(t, "ns")
        tend64 = np.datetime64(t + self.interval_td, "ns")
        result = {}
        for s_idx in range(self.n_stations):
            arr, dep, maxdep = self._idx_arr[s_idx], self._idx_dep[s_idx], self._idx_maxdep[s_idx]
            j = int(np.searchsorted(arr, tend64, side="left")) - 1  # last arrival < t_end
            while j >= 0:
                if dep[j] > t64:
                    result[s_idx] = self._idx_sess[s_idx][j]  # latest arrival wins, as before
                    break
                if maxdep[j] <= t64:
                    break  # nothing earlier can still be present
                j -= 1

        if len(self._cache) > 8:
            self._cache.clear()
        self._cache[key] = result
        return result

    # ── BaseComponent ─────────────────────────────────────────────────────────

    @property
    def n_actions(self) -> int:
        return self.n_stations

    @property
    def n_state_features(self) -> int:
        return 4 * self.n_stations

    def reset(self, t_start: pd.Timestamp) -> None:
        self.remaining_kwh = np.zeros(self.n_stations, dtype=np.float64)
        self.cumulative_cost = np.zeros(self.n_stations, dtype=np.float64)
        self._active_ids = {}
        self._session_acc = {}
        self._step_log = []
        self.sync(t_start)

    def sync(self, t: pd.Timestamp) -> None:
        for s_idx, sess in self._sessions_at(t).items():
            if self._active_ids.get(s_idx) != sess.sid:
                self.remaining_kwh[s_idx] = sess.need_kwh
                self._active_ids[s_idx] = sess.sid

    def get_state_features(self, t: pd.Timestamp) -> list[float]:
        sessions = self._sessions_at(t)
        feats = []
        for s_idx in range(self.n_stations):
            sess = sessions.get(s_idx)
            if sess is None:
                feats.extend([0.0, 0.0, 0.0, 0.0])
                continue
            # t is the look-ahead interval and may not be sync()'d yet: an EV that
            # arrives in t has no runtime state, so fall back to its full
            # requirement instead of reading a stale/zero remaining_kwh.
            remaining = (
                self.remaining_kwh[s_idx]
                if self._active_ids.get(s_idx) == sess.sid
                else sess.need_kwh
            )
            hours_left = max((sess.departure - t).total_seconds() / 3600, 0.0)
            urgency = remaining / (hours_left * self.power_kw + 1e-6)
            feats.extend([1.0, float(remaining), hours_left, urgency])
        return feats

    def apply_actions(self, actions: np.ndarray, t: pd.Timestamp, price: float, shared: dict) -> tuple[float, dict]:
        sessions = self._sessions_at(t)
        reward = 0.0
        info = {"ev_cost": np.zeros(self.n_stations), "ev_delivered": np.zeros(self.n_stations)}

        pv_available = float(shared.get("pv_available_kwh", 0.0))

        # pass 1 — how much each station would draw this interval
        energy = np.zeros(self.n_stations)
        for s_idx in range(self.n_stations):
            sess = sessions.get(s_idx)
            if sess is not None and actions[s_idx] == 1:
                energy[s_idx] = min(self._max_energy(sess, t), self.remaining_kwh[s_idx])

        # pass 2 — split available PV in proportion to draw. Allocating greedily by
        # station index would give the same totals but make station 0 look
        # systematically cheaper in the per-station logs.
        total_energy = float(energy.sum())
        pv_share = min(1.0, pv_available / total_energy) if total_energy > 1e-12 else 0.0

        pv_used_total = 0.0
        for s_idx in range(self.n_stations):
            sess = sessions.get(s_idx)
            charging = sess is not None and actions[s_idx] == 1
            if charging:
                e = energy[s_idx]
                self.remaining_kwh[s_idx] -= e
                pv_used = e * pv_share
                pv_used_total += pv_used
                grid_energy = e - pv_used
                cost = price * grid_energy
                self.cumulative_cost[s_idx] += cost
                reward -= cost
                info["ev_cost"][s_idx] = cost
                info["ev_delivered"][s_idx] = e
                self._acc(sess.sid, delivered=e, cost=cost)
            else:
                e = pv_used = grid_energy = cost = 0.0

            self._step_log.append({
                "timestamp":   t,
                "station_idx": s_idx,
                "station_id":  self.station_ids[s_idx],
                "action":      int(actions[s_idx]) if sess is not None else 0,
                "ev_present":  int(sess is not None),
                "price":       round(price, 6),
                "energy_kwh":  round(e, 4),
                "pv_used_kwh": round(pv_used, 4),
                "grid_kwh":    round(grid_energy, 4),
                "cost_eur":    round(cost, 4),
            })

        # report consumption back through the shared channel so sources (and their
        # episode metrics) can see what was actually taken
        shared["pv_available_kwh"] = pv_available - pv_used_total
        shared["pv_used_kwh"] = shared.get("pv_used_kwh", 0.0) + pv_used_total

        info["pv_used_kwh"] = pv_used_total
        info["pv_remaining_kwh"] = pv_available - pv_used_total
        return reward, info

    def check_departures(self, t: pd.Timestamp, t_end: pd.Timestamp) -> tuple[float, dict]:
        sessions = self._sessions_at(t)
        reward = 0.0
        info = {"ev_penalties": np.zeros(self.n_stations)}
        for s_idx in range(self.n_stations):
            sess = sessions.get(s_idx)
            if sess is None:
                continue
            # behind-schedule shaping penalty (charged every interval it applies)
            time_left_h = max((sess.departure - t_end).total_seconds() / 3600, 0)
            behind = self.remaining_kwh[s_idx] > time_left_h * self.power_kw * self.feasibility_margin
            if self.progress_penalty and behind and sess.departure > t_end:
                reward -= self.progress_penalty
                info["ev_penalties"][s_idx] += self.progress_penalty
                self._acc(sess.sid, penalty=self.progress_penalty)
            # departure penalty
            if sess.departure <= t_end:
                remaining = self.remaining_kwh[s_idx]
                if remaining > sess.need_kwh * self.full_tolerance:
                    pen = self.penalty_per_kwh * remaining + self.fixed_penalty
                    reward -= pen
                    info["ev_penalties"][s_idx] += pen
                    self._acc(sess.sid, penalty=pen)
                self.remaining_kwh[s_idx] = 0.0
        return reward, info

    # ── skip-empty ────────────────────────────────────────────────────────────

    def is_active_at(self, t: pd.Timestamp, interval_td: pd.Timedelta) -> bool:
        return bool(self._sessions_at(t))

    def next_active_from(self, t: pd.Timestamp, t_max: pd.Timestamp, interval_td: pd.Timedelta) -> Optional[pd.Timestamp]:
        future = self.sessions_df[self.sessions_df["departure"] > t]
        if future.empty:
            return None
        earliest = future["arrival"].min()
        candidate = max(t, self._floor(earliest))
        return candidate if candidate <= t_max else None

    def on_skip(self, t: pd.Timestamp, t_end: pd.Timestamp) -> float:
        penalty = 0.0
        for s_idx in range(self.n_stations):
            if self.remaining_kwh[s_idx] > 1e-6:
                sid = self._active_ids.get(s_idx)
                if sid is not None and self.sessions_df.loc[sid, "departure"] <= t_end:
                    penalty -= self.penalty_per_kwh * self.remaining_kwh[s_idx] + self.fixed_penalty
                    self.remaining_kwh[s_idx] = 0.0
                    self._active_ids.pop(s_idx, None)
        return penalty

    # ── normalisation ─────────────────────────────────────────────────────────

    def normalize_state_slice(self, state_slice: np.ndarray) -> np.ndarray:
        out = state_slice.copy()
        for i in range(self.n_stations):
            out[4 * i + 1] /= self.norm_max_kwh    # remaining_kwh
            out[4 * i + 2] /= self.norm_max_hours  # hours_left
            if self.clip_features:
                # norm_max_* are the train split's maxima, so these are <=1 by
                # construction on train but not on any other split.
                out[4 * i + 1] = min(out[4 * i + 1], 1.0)
                out[4 * i + 2] = min(out[4 * i + 2], 1.0)
                out[4 * i + 3] = min(out[4 * i + 3], self.urgency_clip) / self.urgency_clip
        return out

    def get_norm_state(self) -> dict:
        return {
            "norm_max_kwh": float(self.norm_max_kwh),
            "norm_max_hours": float(self.norm_max_hours),
            "power_kw": float(self.power_kw),
            "station_ids": [int(s) for s in self.station_ids],
            "clip_features": bool(self.clip_features),
            "urgency_clip": float(self.urgency_clip),
        }

    def set_norm_state(self, state: dict) -> None:
        if "norm_max_kwh" in state:
            self.norm_max_kwh = float(state["norm_max_kwh"])
        if "norm_max_hours" in state:
            self.norm_max_hours = float(state["norm_max_hours"])
        # absent in pre-fix checkpoints, which were trained unclipped
        self.clip_features = bool(state.get("clip_features", False))
        self.urgency_clip = float(state.get("urgency_clip", URGENCY_CLIP))
        saved_stations = state.get("station_ids")
        if saved_stations is not None and [int(s) for s in self.station_ids] != list(saved_stations):
            raise ValueError(
                f"Checkpoint was trained on stations {saved_stations} but this dataset has "
                f"{[int(s) for s in self.station_ids]}; the action space would not line up."
            )
        saved_power = state.get("power_kw")
        if saved_power is not None and abs(float(saved_power) - self.power_kw) > 1e-9:
            print(
                f"[WARN] Checkpoint was trained with power_kw={saved_power} but the env uses "
                f"{self.power_kw}. Results will not be comparable."
            )

    # ── episode log ───────────────────────────────────────────────────────────

    def get_episode_log(self) -> pd.DataFrame:
        need_col = self._need_col()
        rows = []
        for sid in self.sessions_df.index:
            sess = self.sessions_df.loc[sid]
            acc = self._session_acc.get(sid, {})
            delivered = acc.get("delivered", 0.0)
            cost = acc.get("cost", 0.0)
            needed = sess[need_col]
            rows.append({
                "session_id": sid,
                "station_id": sess["station_id"],
                "arrival": sess["arrival"],
                "departure": sess["departure"],
                "energy_needed_kwh": needed,
                "energy_delivered_kwh": delivered,
                "unmet_kwh": max(needed - delivered, 0.0),
                "pct_met": 100.0 * delivered / needed if needed > 0 else 0.0,
                "energy_cost_eur": cost,
                "penalty_eur": acc.get("penalty", 0.0),
                "avg_price_eur_per_kwh": cost / delivered if delivered > 0 else 0.0,
            })
        return pd.DataFrame(rows).sort_values(["station_id", "arrival"]).reset_index(drop=True)

    def get_episode_metrics(self, info_history: list[dict]) -> dict:
        me = type(self).__name__
        total_required = float(self._need_series.sum())
        delivered = sum(float(np.sum(i.get(me, {}).get("ev_delivered", 0.0))) for i in info_history)
        cost      = sum(float(np.sum(i.get(me, {}).get("ev_cost",      0.0))) for i in info_history)
        penalty   = sum(float(np.sum(i.get(me, {}).get("ev_penalties", 0.0))) for i in info_history)
        return {
            "required_kwh":   total_required,
            "delivered_kwh":  delivered,
            "unmet_kwh":      max(0.0, total_required - delivered),
            "cost_eur":       cost,
            "penalty_eur":    penalty,
            "reward_cost":    -cost,
            "reward_penalty": -penalty,
            "n_sessions":     len(self.sessions_df),
        }

    def get_step_log(self) -> pd.DataFrame:
        """Per-step log: one row per station per interval. Collected during evaluate()."""
        return pd.DataFrame(self._step_log)

    def reload_sessions(self, sessions_df: pd.DataFrame) -> None:
        """Swap in a different set of sessions, keeping normalisation constants."""
        previous = list(self.station_ids)
        self._set_sessions(sessions_df)
        if self.station_ids != previous:
            raise ValueError(
                f"reload_sessions changed the station set from {previous} to {self.station_ids}; "
                "the action space and any trained model would no longer line up."
            )

    # ── internals ─────────────────────────────────────────────────────────────

    def _need_col(self) -> str:
        if "req_kwh" in self.sessions_df.columns:
            return "req_kwh"
        if "kwh_needed" in self.sessions_df.columns:
            return "kwh_needed"
        raise KeyError("sessions_df must have 'req_kwh' or 'kwh_needed'.")

    def _max_energy(self, sess: Session, t: pd.Timestamp) -> float:
        eff_start = max(t, sess.arrival)
        eff_end = min(t + self.interval_td, sess.departure)
        if eff_end <= eff_start:
            return 0.0
        frac = (eff_end - eff_start).total_seconds() / self.interval_td.total_seconds()
        return self.power_kw * (self.interval_minutes / 60.0) * frac

    def _floor(self, t: pd.Timestamp) -> pd.Timestamp:
        m = t.hour * 60 + t.minute
        return t.normalize() + pd.Timedelta(minutes=(m // self.interval_minutes) * self.interval_minutes)

    def _acc(self, sid, delivered=0.0, cost=0.0, penalty=0.0) -> None:
        a = self._session_acc.get(sid, {"delivered": 0.0, "cost": 0.0, "penalty": 0.0})
        a["delivered"] += delivered
        a["cost"] += cost
        a["penalty"] += penalty
        self._session_acc[sid] = a
