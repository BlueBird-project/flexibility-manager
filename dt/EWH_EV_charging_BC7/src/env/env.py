import numpy as np
import pandas as pd
from typing import Optional


class EVChargingEnv:
    """
    EV Charging Scheduling Environment.

    Controls multiple charging stations, deciding at each time interval
    whether to charge (1) or not (0) each station.

    Args:
        price_df: DataFrame with columns ['date', 'price'].
        sessions_df: DataFrame with columns ['arrival', 'departure', 'kwh_needed', 'station_id'].
        power_kw: Charging power in kW (same for every station).
        interval_minutes: Decision interval length in minutes (e.g. 15).
        price_window: Number of previous price steps to include in state.
        penalty_per_kwh: € penalty per remaining kWh when EV departs undercharged.
        fixed_penalty: Fixed € penalty when EV departs undercharged.
        skip_empty: If True, reset/step skip forward to the next interval
                     where at least one EV is present.
    """

    def __init__(
            self,
            price_df: pd.DataFrame,
            sessions_df: pd.DataFrame,
            power_kw: float = 9.0,
            interval_minutes: int = 15,
            price_window: int = 3,
            penalty_per_kwh: float = 5.0,
            fixed_penalty: float = 10.0,
            skip_empty: bool = True,
    ):
        # ---- store params ------------------------------------------------
        self.norm_max_hours = (sessions_df["departure"] - sessions_df["arrival"]).dt.total_seconds().max() / 3600
        self.norm_max_kwh = sessions_df["req_kwh"].max()
        self.power_kw = power_kw
        self.interval_minutes = interval_minutes
        self.interval_td = pd.Timedelta(minutes=interval_minutes)
        self.price_window = price_window
        self.penalty_per_kwh = penalty_per_kwh
        self.fixed_penalty = fixed_penalty
        self.skip_empty = skip_empty
        self._session_acc = {}

        # ---- price data ---------------------------------------------------
        price_df = price_df.copy()
        price_df['date'] = pd.to_datetime(
            price_df['timestamp'] + ' ' + price_df['Tijd'], dayfirst=True
        )
        price_df["date"] = pd.to_datetime(price_df["date"])
        price_df = price_df.sort_values("date").reset_index(drop=True)
        self.price_df = price_df
        # build a Series indexed by aligned timestamps for fast lookup
        self.price_series = price_df.set_index("date")["price"]

        # ---- session data -------------------------------------------------
        sessions_df = sessions_df.copy()
        sessions_df["arrival"] = pd.to_datetime(sessions_df["arrival"])
        sessions_df["departure"] = pd.to_datetime(sessions_df["departure"])
        self.sessions_df = sessions_df.sort_values("arrival").reset_index(drop=True)

        # ---- stations -----------------------------------------------------
        self.station_ids = sorted(sessions_df["station_id"].unique())
        self.n_stations = len(self.station_ids)
        # map station_id -> positional index (0, 1, …)
        self.station_idx = {sid: i for i, sid in enumerate(self.station_ids)}

        # ---- time boundaries ----------------------------------------------
        self.t_min = self._floor_time(price_df["date"].min())
        self.t_max = price_df["date"].max()

        # ---- per‑station bookkeeping (set in reset) -----------------------
        self.current_time: Optional[pd.Timestamp] = None
        # remaining energy each station's current EV still needs (kWh)
        self.remaining_kwh: Optional[np.ndarray] = None
        # cumulative cost per station (positive = money spent)
        self.cumulative_cost: Optional[np.ndarray] = None
        # keep a *working* copy of sessions so we can track remaining energy
        self._active_sessions: Optional[dict] = None  # station_idx -> session row
        self.done = False

    # ------------------------------------------------------------------
    #  Time helpers
    # ------------------------------------------------------------------
    def _floor_time(self, t: pd.Timestamp) -> pd.Timestamp:
        """Floor a timestamp to the nearest interval boundary."""
        minutes_since_midnight = t.hour * 60 + t.minute
        floored_minutes = (minutes_since_midnight // self.interval_minutes) * self.interval_minutes
        return t.normalize() + pd.Timedelta(minutes=floored_minutes)

    def _get_price(self, t: pd.Timestamp) -> float:
        """Return the price for the interval starting at *t*.

        Uses the last known price at or before *t* (forward‑fill logic).
        """
        mask = self.price_series.index <= t
        if mask.any():
            return float(self.price_series.loc[mask].iloc[-1])
        # before any price data – return first available price
        return float(self.price_series.iloc[0])

    # ------------------------------------------------------------------
    #  Session lookup helpers
    # ------------------------------------------------------------------

    def _sessions_at(self, t: pd.Timestamp) -> dict:
        """Return dict  station_idx -> session (Series) for EVs present at *t*.

        An EV is present if  arrival <= t + interval  AND  departure > t,
        i.e. it overlaps the interval [t, t + interval).
        """
        t_end = t + self.interval_td
        df = self.sessions_df
        present = df[(df["arrival"] < t_end) & (df["departure"] > t)]
        result = {}
        for _, row in present.iterrows():
            idx = self.station_idx[row["station_id"]]
            # if multiple sessions per station overlap somehow, keep latest arrival
            if idx not in result or row["arrival"] > result[idx]["arrival"]:
                result[idx] = row
        return result

    def _any_vehicle_from(self, t: pd.Timestamp) -> Optional[pd.Timestamp]:
        """Find the first interval >= *t* where at least one EV is present.

        Returns the aligned interval start, or None if no future EVs exist.
        """
        future = self.sessions_df[self.sessions_df["departure"] > t]
        if future.empty:
            return None
        # earliest arrival still relevant
        earliest_arrival = future["arrival"].min()
        candidate = max(t, self._floor_time(earliest_arrival))
        # make sure it's >= t
        if candidate < t:
            candidate = t
        return candidate

    # ------------------------------------------------------------------
    #  Effective energy delivered in one interval
    # ------------------------------------------------------------------
    def _effective_energy(self, session: pd.Series, t: pd.Timestamp) -> float:
        """Max kWh that can be delivered in interval [t, t+interval)
        considering partial overlaps at arrival / departure.
        """
        interval_start = t
        interval_end = t + self.interval_td

        # effective overlap
        eff_start = max(interval_start, session["arrival"])
        eff_end = min(interval_end, session["departure"])

        if eff_end <= eff_start:
            return 0.0

        fraction = (eff_end - eff_start).total_seconds() / self.interval_td.total_seconds()
        return self.power_kw * (self.interval_minutes / 60.0) * fraction

    # ------------------------------------------------------------------
    #  State construction
    # ------------------------------------------------------------------
    def _build_state(self) -> np.ndarray:
        """
        State vector layout:
          [price_t+1,
           price_t, price_t-1, …, price_t-(window-1),   (price_window values)
           sin(time), cos(time),
           --- for each station (ordered by station index) ---
           ev_present (0/1), remaining_kwh, hours_until_departure]
        """
        t = self.current_time + self.interval_td

        # --- prices ---
        next_price = self._get_price(t)
        prev_prices = []
        for k in range(1, self.price_window + 1):
            prev_prices.append(self._get_price(t - k * self.interval_td))

        # --- time encoding (fraction of day) ---
        minutes_of_day = t.hour * 60 + t.minute
        frac = minutes_of_day / (24 * 60)
        sin_time = np.sin(2 * np.pi * frac)
        cos_time = np.cos(2 * np.pi * frac)

        # --- per‑station features ---
        station_feats = []
        sessions = self._sessions_at(t)
        for s_idx in range(self.n_stations):
            if s_idx in sessions:
                sess = sessions[s_idx]
                ev_present = 1.0
                remaining = self.remaining_kwh[s_idx]
                hours_left = max(
                    (sess["departure"] - t).total_seconds() / 3600.0, 0.0
                )
                max_possible = hours_left * self.power_kw
                urgency = remaining / (max_possible + 1e-6)

            else:
                ev_present = 0.0
                remaining = 0.0
                hours_left = 0.0
                urgency = 0.0
            station_feats.extend([ev_present, remaining, hours_left, urgency])

        state = [next_price] + prev_prices + [sin_time, cos_time] + station_feats
        return np.array(state, dtype=np.float32)

    # ------------------------------------------------------------------
    #  State size helper
    # ------------------------------------------------------------------
    @property
    def observation_size(self) -> int:
        # 1 (next price) + price_window + 2 (sin, cos) + 3 * n_stations
        return 1 + self.price_window + 2 + 4 * self.n_stations

    @property
    def action_size(self) -> int:
        """Number of actions (one binary action per station)."""
        return self.n_stations

    # ------------------------------------------------------------------
    #  reset
    # ------------------------------------------------------------------
    def reset(self) -> np.ndarray:
        """Reset the environment to the beginning of the dataset.

        Returns:
            Initial state vector.
        """
        self.current_time = self._floor_time(self.price_df["date"].min())
        self.done = False

        # initialise per‑station tracking
        self.remaining_kwh = np.zeros(self.n_stations, dtype=np.float64)
        self.cumulative_cost = np.zeros(self.n_stations, dtype=np.float64)
        self._active_session_ids = {}  # station_idx -> session index in self.sessions_df
        self._session_acc = {}
        # load initial sessions & their energy needs
        self._sync_sessions()

        # optionally skip to first interval with at least one EV
        if self.skip_empty:
            self._skip_to_next_vehicle()

        return self._build_state()

    # ------------------------------------------------------------------
    #  step
    # ------------------------------------------------------------------

    # ... existing code ...
    def step(self, actions: np.ndarray):
        """
        Execute one interval step.

        Semantics: the agent sits at time t, sees price at t+1 in the state,
        and decides whether to charge at t+1. step() advances to t+1, applies
        the action there, checks departures at end of t+1, then returns the
        new state (which shows price at t+2).

        Args:
            actions: array of shape (n_stations,) with 0 or 1 per station.

        Returns:
            state (np.ndarray), reward (float), done (bool), info (dict)
        """
        assert not self.done, "Environment is done. Call reset()."
        actions = np.asarray(actions, dtype=np.int32)
        assert actions.shape == (self.n_stations,), (
            f"Expected actions shape ({self.n_stations},), got {actions.shape}"
        )

        # ---- 1. Advance time: action applies to the NEXT interval --------
        self.current_time = self.current_time + self.interval_td

        if self.current_time > self.t_max:
            self.done = True
            return self._build_state(), 0.0, self.done, {
                "charging_cost": np.zeros(self.n_stations),
                "penalties": np.zeros(self.n_stations),
                "energy_delivered": np.zeros(self.n_stations),
            }

        # ---- 2. Sync arrivals at the new current time --------------------
        self._sync_sessions()

        t = self.current_time  # now = t+1 (execution interval)
        price = self._get_price(t)  # price at t+1, the one agent saw
        sessions = self._sessions_at(t)

        reward = 0.0
        info = {
            "charging_cost": np.zeros(self.n_stations),
            "penalties": np.zeros(self.n_stations),
            "energy_delivered": np.zeros(self.n_stations),
        }

        # ---- 3. Apply charging actions at t+1 ----------------------------
        for s_idx in range(self.n_stations):
            if s_idx not in sessions:
                continue  # no EV → action is irrelevant
            if actions[s_idx] == 1:
                sess = sessions[s_idx]
                max_energy = self._effective_energy(sess, t)
                energy = min(max_energy, self.remaining_kwh[s_idx])
                self.remaining_kwh[s_idx] -= energy
                cost = price * energy
                self.cumulative_cost[s_idx] += cost
                reward -= cost
                info["charging_cost"][s_idx] = cost
                info["energy_delivered"][s_idx] = energy

                sid = sess.name
                acc = self._session_acc.get(sid, {"delivered": 0.0, "cost": 0.0, "penalty": 0.0})
                acc["delivered"] += float(energy)
                acc["cost"] += float(cost)
                self._session_acc[sid] = acc

        margin = 96/100
        # ---- 4. Check departures at end of interval [t, t+Δ) -------------
        t_end = t + self.interval_td
        for s_idx in range(self.n_stations):
            if s_idx not in sessions:
                continue
            sess = sessions[s_idx]
            time_left_h = max((sess['departure'] - t_end).total_seconds() / 3600, 0)
            max_possible = time_left_h * self.power_kw * margin
            penalize_past = 0
            # if its impossible to fully charge (considering a margin) the EV, penalty
            if self.remaining_kwh[s_idx] > max_possible and sess["departure"] > t_end:
                pen = self.fixed_penalty / 2
                reward -= pen
                info["penalties"][s_idx] += pen

                sid = sess.name
                acc = self._session_acc.get(sid, {"delivered": 0.0, "cost": 0.0, "penalty": 0.0})
                acc["penalty"] += float(pen)
                self._session_acc[sid] = acc

            # if vehicle departing, so if departure bigger than the next interval
            if sess["departure"] <= t_end:
                # EV is leaving
                remaining = self.remaining_kwh[s_idx]
                # if there is energy left at departure, give penalty
                if remaining > sess["req_kwh"] * (1 - margin):
                    pen = self.penalty_per_kwh * remaining + self.fixed_penalty
                    reward -= pen
                    info["penalties"][s_idx] += pen

                    sid = sess.name
                    acc = self._session_acc.get(sid, {"delivered": 0.0, "cost": 0.0, "penalty": 0.0})
                    acc["penalty"] += float(pen)
                    self._session_acc[sid] = acc

                # clear station
                self.remaining_kwh[s_idx] = 0.0

        # ---- 5. Skip empty intervals if configured -----------------------
        if self.skip_empty:
            skipped_penalties = self._skip_to_next_vehicle()
            reward += skipped_penalties

        if self.current_time > self.t_max:
            self.done = True

        # ---- 6. Return new state (shows price at t+2, EV info at t+1) ---
        return self._build_state(), reward, self.done, info

    # ------------------------------------------------------------------
    #  Internal helpers
    # ------------------------------------------------------------------
    def _sync_sessions(self):
        """Update remaining_kwh for newly arrived EVs at current_time."""
        sessions = self._sessions_at(self.current_time)
        for s_idx, sess in sessions.items():
            sess_id = sess.name  # index in sessions_df
            prev = self._active_session_ids.get(s_idx)
            if prev != sess_id:
                # new EV at this station
                self.remaining_kwh[s_idx] = sess["req_kwh"]
                self._active_session_ids[s_idx] = sess_id

    def _skip_to_next_vehicle(self) -> float:
        """Advance current_time until at least one EV is present.

        While skipping, if any EV departs with remaining energy a penalty
        is applied (they were never charged because no vehicle was flagged –
        this is a safety net).

        Returns accumulated penalty (negative contribution to reward).
        """
        penalty_total = 0.0

        while True:
            sessions = self._sessions_at(self.current_time)
            if sessions:
                break

            # before advancing, check departures and penalise
            t_end = self.current_time + self.interval_td
            for s_idx in range(self.n_stations):
                if self.remaining_kwh[s_idx] > 1e-6:
                    # check if the session that owned this energy departs now
                    sid = self._active_session_ids.get(s_idx)
                    if sid is not None:
                        sess = self.sessions_df.loc[sid]
                        if sess["departure"] <= t_end:
                            pen = (
                                    self.penalty_per_kwh * self.remaining_kwh[s_idx]
                                    + self.fixed_penalty
                            )
                            penalty_total -= pen
                            self.remaining_kwh[s_idx] = 0.0
                            self._active_session_ids.pop(s_idx, None)

            # find next interval with an EV
            candidate = self._any_vehicle_from(self.current_time + self.interval_td)
            if candidate is None or candidate > self.t_max:
                self.current_time = self.t_max + self.interval_td
                self.done = True
                break
            self.current_time = candidate
            self._sync_sessions()

        return penalty_total

    def get_session_log_df(self) -> pd.DataFrame:
        """
        Return a per-EV session DataFrame with concrete metrics.
        Columns: session_id, station_id, arrival, departure, energy_needed_kwh,
                 energy_delivered_kwh, unmet_kwh, pct_met, energy_cost_eur,
                 penalty_eur, avg_price_eur_per_kwh
        """
        # base info from sessions_df in the order of original indexing
        df = self.sessions_df.copy()
        df = df.assign(session_id=df.index)
        # ensure column names
        need_col = 'req_kwh' if 'req_kwh' in df.columns else ('kwh_needed' if 'kwh_needed' in df.columns else None)
        if need_col is None:
            raise KeyError("sessions_df must contain 'req_kwh' or 'kwh_needed'.")
        # accumulate
        delivered = []
        cost = []
        penalty = []
        for sid in df.index:
            acc = self._session_acc.get(sid, {"delivered": 0.0, "cost": 0.0, "penalty": 0.0})
            delivered.append(float(acc.get("delivered", 0.0)))
            cost.append(float(acc.get("cost", 0.0)))
            penalty.append(float(acc.get("penalty", 0.0)))
        df_out = pd.DataFrame({
            'session_id': df['session_id'].values,
            'station_id': df['station_id'].values,
            'arrival': pd.to_datetime(df['arrival']).values,
            'departure': pd.to_datetime(df['departure']).values,
            'energy_needed_kwh': df[need_col].astype(float).values,
            'energy_delivered_kwh': delivered,
            'energy_cost_eur': cost,
            'penalty_eur': penalty,
        })
        df_out['unmet_kwh'] = (df_out['energy_needed_kwh'] - df_out['energy_delivered_kwh']).clip(lower=0.0)
        df_out['pct_met'] = (100.0 * df_out['energy_delivered_kwh'] / df_out['energy_needed_kwh']).where(
            df_out['energy_needed_kwh'] > 0, 0.0)
        df_out['avg_price_eur_per_kwh'] = (df_out['energy_cost_eur'] / df_out['energy_delivered_kwh']).where(
            df_out['energy_delivered_kwh'] > 0, 0.0)
        # nice sorting
        return df_out.sort_values(['station_id', 'arrival']).reset_index(drop=True)

    # ------------------------------------------------------------------
    #  Nice repr
    # ------------------------------------------------------------------
    def __repr__(self) -> str:
        return (
            f"EVChargingEnv(stations={self.n_stations}, "
            f"power={self.power_kw}kW, "
            f"interval={self.interval_minutes}min, "
            f"price_window={self.price_window}, "
            f"state_size={self.observation_size}, "
            f"action_size={self.action_size})"
        )

    def normalize_state(self, state: np.ndarray) -> np.ndarray:

        norm_state = state.copy()

        base = 1 + self.price_window + 2  # next_price + price_window + sin, cos
        station_feats_num = 4
        for i in range(self.n_stations):
            pos_present = base + station_feats_num * i
            pos_remaining = base + station_feats_num * i + 1
            pos_till_dep = base + station_feats_num * i + 2

            remaining = norm_state[pos_remaining]
            t_until = norm_state[pos_till_dep]

            # Normalize ONLY those two fields
            norm_state[pos_remaining] = remaining / self.norm_max_kwh
            norm_state[pos_till_dep] = t_until / self.norm_max_hours

        return norm_state

    def reload_data(self, price_df, sessions_df):
        """
        Swap the sessions dataset while keeping everything else identical.
        Price dataset is reused exactly as provided at env construction.
        No checks, no constraints — same structure, just new sessions.
        """

        # --- Store sessions ---
        sessions_df = sessions_df.copy()
        sessions_df["arrival"] = pd.to_datetime(sessions_df["arrival"])
        sessions_df["departure"] = pd.to_datetime(sessions_df["departure"])
        sessions_df = sessions_df.sort_values("arrival").reset_index(drop=True)
        self.sessions_df = sessions_df

        # # --- Recompute normalization constants ---
        # self.norm_max_hours = (
        #         (self.sessions_df["departure"] - self.sessions_df["arrival"])
        #         .dt.total_seconds()
        #         .max() / 3600
        # )

        # Accept both names used in your project
        if "req_kwh" in self.sessions_df.columns:
            need_col = "req_kwh"
        else:
            need_col = "kwh_needed"

        # self.norm_max_kwh = self.sessions_df[need_col].max()

        # # --- Rebuild station structure ---
        # self.station_ids = sorted(self.sessions_df["station_id"].unique())
        # self.n_stations = len(self.station_ids)
        # self.station_idx = {sid: i for i, sid in enumerate(self.station_ids)}

        # --- Price data remains the same, but rebuild series/index ---
        price_df = price_df.copy()
        price_df["date"] = pd.to_datetime(
            price_df["timestamp"] + " " + price_df["Tijd"], dayfirst=True
        )
        price_df = price_df.sort_values("date").reset_index(drop=True)
        self.price_df = price_df
        self.price_series = price_df.set_index("date")["price"]

        # --- Recompute time boundaries ---
        self.t_min = self._floor_time(price_df["date"].min())
        self.t_max = price_df["date"].max()

        # --- Reset environment state completely ---
        return self.reset()
# ======================================================================
#  Quick demo / smoke test
# ======================================================================
# if __name__ == "__main__":
#     # --- synthetic price data (every 15 min for one day) ---------------
#     timestamps = pd.date_range("2025-06-01 00:00", "2025-06-01 23:45", freq="15min")
#     np.random.seed(42)
#     prices = 0.10 + 0.05 * np.sin(np.linspace(0, 2 * np.pi, len(timestamps))) + \
#              np.random.normal(0, 0.005, len(timestamps))
#     price_df = pd.DataFrame({"date": timestamps, "price": prices})
#
#     # --- synthetic EV sessions -----------------------------------------
#     sessions_df = pd.DataFrame([
#         {
#             "arrival": pd.Timestamp("2025-06-01 08:18"),
#             "departure": pd.Timestamp("2025-06-01 12:00"),
#             "kwh_needed": 30.0,
#             "station_id": "S1",
#         },
#         {
#             "arrival": pd.Timestamp("2025-06-01 09:05"),
#             "departure": pd.Timestamp("2025-06-01 11:30"),
#             "kwh_needed": 20.0,
#             "station_id": "S2",
#         },
#         {
#             "arrival": pd.Timestamp("2025-06-01 13:00"),
#             "departure": pd.Timestamp("2025-06-01 17:00"),
#             "kwh_needed": 40.0,
#             "station_id": "S1",
#         },
#     ])
#
#     env = EVChargingEnv(
#         price_df=price_df,
#         sessions_df=sessions_df,
#         power_kw=7.0,
#         interval_minutes=15,
#         price_window=4,
#         penalty_per_kwh=5.0,
#         fixed_penalty=10.0,
#         skip_empty=True,
#     )
#     print(env)
#     print(f"State size : {env.state_size}")
#     print(f"Action size: {env.action_size}")
#     print()
#
#     state = env.reset()
#     print(f"t={env.current_time}  state={np.round(state, 3)}")
#
#     total_reward = 0.0
#     steps = 0
#     while not env.done:
#         # simple policy: always charge if EV is present
#         actions = np.ones(env.action_size, dtype=int)
#         state, reward, done, info = env.step(actions)
#         total_reward += reward
#         steps += 1
#         print(
#             f"t={env.current_time}  reward={reward:+.4f}  "
#             f"cost={info['charging_cost']}  pen={info['penalties']}  "
#             f"remaining={env.remaining_kwh}"
#         )
#
#     print(f"\nDone in {steps} steps.  Total reward: {total_reward:.4f}")
#     print(f"Cumulative cost per station: {env.cumulative_cost}")
