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
        sessions_df: DataFrame with columns ['arrival', 'departure', 'req_kwh',
                     'station_id'], plus an optional per-session 'power_kw'
                     column -- each EV can charge at its own rate (e.g. a
                     standard 5/7/9/11/22 kW rating). Sessions without a
                     'power_kw' value (including the whole column being
                     absent, e.g. an older dataset) fall back to
                     `default_power_kw`.
        default_power_kw: Charging power (kW) used for any session that
                     doesn't specify its own power_kw.
        interval_minutes: Decision interval length in minutes (e.g. 15).
        price_window: Number of previous price steps to include in state.
        penalty_per_kwh: € penalty per remaining kWh when EV departs undercharged.
        fixed_penalty: Fixed € penalty when EV departs undercharged.
        skip_empty: If True, reset/step skip forward to the next interval
                     where at least one EV is present.
        power_limit_kw: Optional shared power cap (kW) across all stations for a
                     single interval. Charging stays binary per station (each
                     station draws its own session's power_kw or 0 -- no
                     partial power), so at most as many stations as fit
                     their combined power_kw under the cap can charge at
                     once. If more stations request charging (action 1)
                     than the cap allows, the most urgent ones (least
                     achievable charge remaining relative to time left) win
                     priority, filled greedily until the cap is reached; the
                     rest are curtailed for that interval as if their action
                     had been 0. None (default) disables the cap.
    """

    # Ceiling on the "urgency" station feature (see _build_state) -- unlike
    # every other feature, it's a raw ratio with no denominator floor, so
    # it isn't bounded like the rest of the state without this cap.
    URGENCY_CAP = 5.0

    def __init__(
            self,
            price_df: pd.DataFrame,
            sessions_df: pd.DataFrame,
            default_power_kw: float = 9.0,
            interval_minutes: int = 15,
            price_window: int = 3,
            penalty_per_kwh: float = 5.0,
            fixed_penalty: float = 10.0,
            skip_empty: bool = True,
            power_limit_kw: Optional[float] = None,
    ):
        # ---- store params ------------------------------------------------
        self.norm_max_hours = (sessions_df["departure"] - sessions_df["arrival"]).dt.total_seconds().max() / 3600
        self.norm_max_kwh = sessions_df["req_kwh"].max()
        self.default_power_kw = default_power_kw
        self.norm_max_power_kw = self._power_column(sessions_df).max()
        self.interval_minutes = interval_minutes
        self.interval_td = pd.Timedelta(minutes=interval_minutes)
        self.price_window = price_window
        self.penalty_per_kwh = penalty_per_kwh
        self.fixed_penalty = fixed_penalty
        self.skip_empty = skip_empty
        self.power_limit_kw = power_limit_kw
        self._session_acc = {}
        self._sessions_at_cache = None

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
        self.norm_max_price = float(self.price_series.max())

        # ---- session data -------------------------------------------------
        sessions_df = sessions_df.copy()
        sessions_df["arrival"] = pd.to_datetime(sessions_df["arrival"])
        sessions_df["departure"] = pd.to_datetime(sessions_df["departure"])
        sessions_df["power_kw"] = self._power_column(sessions_df)
        self.sessions_df = sessions_df.sort_values("arrival").reset_index(drop=True)

        # ---- stations -----------------------------------------------------
        self.station_ids = sorted(sessions_df["station_id"].unique())
        self.n_stations = len(self.station_ids)
        # map station_id -> positional index (0, 1, …)
        self.station_idx = {sid: i for i, sid in enumerate(self.station_ids)}

        # ---- time boundaries ----------------------------------------------
        self.t_min = self._floor_time(price_df["date"].min())
        self.t_max = price_df["date"].max()
        self._build_price_grid()

        # ---- per‑station bookkeeping (set in reset) -----------------------
        self.current_time: Optional[pd.Timestamp] = None
        # remaining energy each station's current EV still needs (kWh)
        self.remaining_kwh: Optional[np.ndarray] = None
        # cumulative cost per station (positive = money spent)
        self.cumulative_cost: Optional[np.ndarray] = None
        # keep a *working* copy of sessions so we can track remaining energy
        self._active_sessions: Optional[dict] = None  # station_idx -> session row
        self.done = False
        self.printer()

    def _power_column(self, sessions_df: pd.DataFrame) -> pd.Series:
        """Per-session charging power (kW), falling back to
        `default_power_kw` for any session (or whole dataset) that doesn't
        specify its own -- keeps older sessions_df's without a power_kw
        column working unchanged."""
        if "power_kw" in sessions_df.columns:
            return sessions_df["power_kw"].fillna(self.default_power_kw).astype(float)
        return pd.Series(self.default_power_kw, index=sessions_df.index, dtype=float)

    def printer(self):
        # Assuming 'env' is already initialized (e.g., env = EVChargingEnv(...))
        print("\n" + "=" * 60)
        print("  EV CHARGING ENVIRONMENT OBSERVATION VECTOR BREAKDOWN")
        print("=" * 60)
        print(f"Total State Vector Size: {self.observation_size} dimensions")
        print("-" * 60)

        # 1. Price Breakdown
        print(f"1. MARKET PRICES ({self.price_window + 1} dimensions, each min-max "
              f"normalized WITHIN this window -- 0=cheapest visible, 1=priciest):")
        print(f"   • Index [0]      : Target Price (t+1)")
        for k in range(1, self.price_window + 1):
            print(f"   • Index [{k}]      : Future Horizon Price (t+1+{k})")
        idx_wmin = self.price_window + 1
        idx_wmax = idx_wmin + 1
        print(f"   • Index [{idx_wmin}]      : Window Min Price (raw EUR/kWh, this window's price level)")
        print(f"   • Index [{idx_wmax}]      : Window Max Price (raw EUR/kWh, this window's price level)")

        # 2. Time Breakdown
        idx_sin = idx_wmax + 1
        idx_cos = idx_sin + 1
        print(f"\n2. TIME ENCODING (2 dimensions):")
        print(f"   • Index [{idx_sin}]      : Sin Component of Time (t+1 fraction of day)")
        print(f"   • Index [{idx_cos}]      : Cos Component of Time (t+1 fraction of day)")

        # 3. Station Breakdown (Simplified)
        print(f"\n3. EV CHARGING STATIONS ({8 * self.n_stations} dimensions total):")
        print(f"   There are {self.n_stations} total stations registered: {self.station_ids}")
        print(f"   Each station adds 8 features sequentially (starting at Index {idx_cos + 1}):")
        print(f"     - Feature 1 : EV Present Flag (0.0 or 1.0)")
        print(f"     - Feature 2 : Remaining Energy Needed (kWh)")
        print(f"     - Feature 3 : Hours Until Departure")
        print(f"     - Feature 4 : Urgency Coefficient (Remaining kWh / Max Possible kWh)")
        print(f"     - Feature 5 : Charging Power for this EV's session (kW, 0 if absent)")
        print(f"     - Feature 6 : Price Percentile Until Departure (fraction of remaining "
              f"slots strictly cheaper than now -- 0=cheapest, ~1=priciest; percentile, not "
              f"min-max, so it's robust to one-off price spikes in the window)")
        print(f"     - Feature 7 : Min Price Remaining Before Departure (raw EUR/kWh)")
        print(f"     - Feature 8 : Should-Charge-Greedy Flag (1.0 if now is one of the cheapest "
              f"remaining slots the exact-optimal algorithm would pick to meet this EV's "
              f"remaining energy need, 0.0 otherwise)")

        print("-" * 60)
        print(f"Action Size: {self.action_size} dimensions (Binary decisions per station)")
        if self.power_limit_kw is not None:
            print(f"Shared power cap: {self.power_limit_kw} kW total across stations "
                  f"(per-session power now varies, so the number of stations that fit "
                  f"depends on which EVs are active)")
        print("=" * 60 + "\n")
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
        Relies on price_series' index being sorted ascending (guaranteed at
        construction) to binary-search instead of scanning the full index.
        """
        pos = self.price_series.index.searchsorted(t, side="right") - 1
        if pos >= 0:
            return float(self.price_series.iloc[pos])
        # before any price data – return first available price
        return float(self.price_series.iloc[0])

    def _build_price_grid(self) -> None:
        """Precompute prices on a fixed interval_minutes grid starting at
        t_min, one entry per interval_td step, using the exact same
        forward-fill rule as _get_price(). Every time value _build_state()
        ever asks about (current_time and t_min are both floored/advanced
        in whole interval_td steps) lands exactly on this grid, so a slice
        of this array is bit-identical to calling _get_price() at each of
        those times individually -- but as ONE vectorized numpy op instead
        of up to ~97 separate Python-level calls (each its own Timestamp
        add + binary search) per station per step. That loop, needed for
        price_rank_remaining/min_price_remaining (see _build_state()), was
        the actual bottleneck once price_window widened and per-station
        lookahead was added -- confirmed: a screening sweep config went
        from a few minutes to ~3 hours after those features were added.
        """
        n_grid = int(np.ceil((self.t_max - self.t_min) / self.interval_td)) + 2
        grid_times = self.t_min + np.arange(n_grid) * self.interval_td
        pos = self.price_series.index.searchsorted(grid_times, side="right") - 1
        pos = np.clip(pos, 0, len(self.price_series) - 1)
        self._price_grid = self.price_series.values[pos].astype(np.float64)

    def _price_slice(self, t: pd.Timestamp, n_steps: int) -> np.ndarray:
        """Vectorized equivalent of
        [self._get_price(t + k*interval_td) for k in range(n_steps)].
        Indices outside the grid clip to the first/last known price, same
        edge behavior as _get_price().
        """
        start = int(round((t - self.t_min) / self.interval_td))
        idx = np.clip(np.arange(start, start + n_steps), 0, len(self._price_grid) - 1)
        return self._price_grid[idx]

    # ------------------------------------------------------------------
    #  Session lookup helpers
    # ------------------------------------------------------------------

    def _sessions_at(self, t: pd.Timestamp) -> dict:
        """Return dict  station_idx -> session (Series) for EVs present at *t*.

        An EV is present if  arrival <= t + interval  AND  departure > t,
        i.e. it overlaps the interval [t, t + interval).

        Single-entry memoized on t: every step() call asks this for the same
        current_time 2-3 times in a row (once directly, once via
        _sync_sessions(), once more via _skip_to_next_vehicle()'s first
        check), each redoing a full pandas filter over every session. Since
        the return value is a pure function of t and self.sessions_df, and
        t only ever advances, caching the single most recent (t, result)
        pair is exact -- not an approximation -- as long as it's invalidated
        whenever sessions_df changes (see reset()/reload_data()).
        """
        if self._sessions_at_cache is not None and self._sessions_at_cache[0] == t:
            return self._sessions_at_cache[1]
        t_end = t + self.interval_td
        df = self.sessions_df
        present = df[(df["arrival"] < t_end) & (df["departure"] > t)]
        result = {}
        for _, row in present.iterrows():
            idx = self.station_idx[row["station_id"]]
            # if multiple sessions per station overlap somehow, keep latest arrival
            if idx not in result or row["arrival"] > result[idx]["arrival"]:
                result[idx] = row
        self._sessions_at_cache = (t, result)
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
        return session["power_kw"] * (self.interval_minutes / 60.0) * fraction

    # ------------------------------------------------------------------
    #  Shared power cap
    # ------------------------------------------------------------------
    def _select_charging_stations(self, t: pd.Timestamp, sessions: dict, requested: list) -> set:
        """Given stations that requested to charge this interval, return the
        subset actually allowed under the shared power_limit_kw cap.

        Each session now draws its own power_kw (no longer uniform), so
        "how many fit" isn't a simple count -- rank requests by urgency
        (highest remaining_kwh relative to what's still achievable before
        departure) and greedily admit them in that order while their
        summed power_kw stays within the cap. A less urgent but
        lower-power session can end up admitted where a more urgent,
        higher-power one wouldn't fit; that's intentional -- skipping a
        request that doesn't fit and moving on to the next-most-urgent one
        that does makes better use of the shared capacity than leaving it
        idle.
        """
        if self.power_limit_kw is None or not requested:
            return set(requested)

        def urgency(s_idx: int) -> float:
            sess = sessions[s_idx]
            hours_left = max((sess["departure"] - t).total_seconds() / 3600.0, 0.0)
            max_possible = hours_left * sess["power_kw"]
            return self.remaining_kwh[s_idx] / (max_possible + 1e-6)

        ranked = sorted(requested, key=urgency, reverse=True)
        allowed = set()
        total_power = 0.0
        for s_idx in ranked:
            p = sessions[s_idx]["power_kw"]
            if total_power + p <= self.power_limit_kw + 1e-9:
                allowed.add(s_idx)
                total_power += p
        return allowed

    # ------------------------------------------------------------------
    #  State construction
    # ------------------------------------------------------------------
    def _build_state(self) -> np.ndarray:
        """
        State vector layout:
          [price_t+1, price_t+2, …, price_t+1+window,   (price_window+1 values,
             each min-max normalized WITHIN this window: 0 = cheapest price
             currently visible, 1 = priciest -- tells the agent directly
             whether "now" is relatively cheap without it having to learn
             that from 13 raw numbers itself),
           window_min, window_max,                       (raw €/kWh, so the
             *level*/spread of this window isn't lost to the within-window
             normalization above -- normalize_state() scales these two)
           sin(time), cos(time),
           --- for each station (ordered by station index) ---
           ev_present (0/1), remaining_kwh, hours_until_departure, urgency,
           power_kw (this session's own charging rate, 0 if absent),
           price_rank_remaining, min_price_remaining, should_charge_greedy
           (see below)]

        These three exist because the shared price_window (a few hours) is
        far shorter than a session's dwell time (up to 24h), so the fixed
        window alone can't tell a station whether a cheaper slot exists
        later today, before ITS OWN departure. Since prices are fully known
        (this is offline/backtest data, same assumption used to compute the
        exact-optimal cost baseline), we compute these directly instead of
        making the network infer them via long-horizon bootstrapping
        through a window it can't see past:
          - price_rank_remaining: PERCENTILE rank (fraction of remaining
            slots strictly cheaper than now) against all prices between now
            and THIS station's departure. Percentile, not min-max, so one
            outlier price spike in the window doesn't compress everything
            else toward 0 (min-max was the original, weaker version of
            this feature -- percentile is robust to that skew).
          - min_price_remaining: that same window's raw minimum price
            (€/kWh) -- normalize_state() scales this one.
          - should_charge_greedy: 1.0 if "now" is one of the cheapest
            remaining slots that the exact-optimal algorithm (sort by
            price, take the cheapest until req_kwh is met -- the same
            algorithm used to compute the ceiling savings figure we
            compare against) would pick for this station, 0.0 otherwise.
            Hands the agent the optimal decision rule as an input feature,
            not the outcome of the episode -- it still has to learn to
            weigh it against curtailment risk, urgency, etc.
        """
        t = self.current_time + self.interval_td

        # --- prices: min-max normalized within the visible window ---
        window_prices = self._price_slice(t, self.price_window + 1)
        window_min = float(window_prices.min())
        window_max = float(window_prices.max())
        spread = window_max - window_min
        norm_prices = ((window_prices - window_min) / (spread + 1e-6)).tolist()

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
                power_kw = sess["power_kw"]
                hours_left = max(
                    (sess["departure"] - t).total_seconds() / 3600.0, 0.0
                )
                max_possible = hours_left * power_kw
                # Unlike every other station feature (all bounded to
                # roughly [-1,1] or [0,1]), this ratio is unbounded and
                # blows up as hours_left -> 0 for any undercharged EV --
                # confirmed empirically up to ~6000 right before departure.
                # Every undercharged session passes through that moment,
                # so it's not a rare edge case, and it fed values orders of
                # magnitude larger than every other input straight into
                # the network. Capped at URGENCY_CAP: >1.0 already means
                # "can't fully finish even at max power", so the network
                # loses no real signal by not distinguishing "5x infeasible"
                # from "6000x infeasible".
                urgency = min(remaining / (max_possible + 1e-6), self.URGENCY_CAP)

                # --- price info spanning THIS station's own remaining
                #     dwell time, not the shared (much shorter) window ---
                steps_left = max(0, int(hours_left * 60 / self.interval_minutes))
                remaining_prices = self._price_slice(t, steps_left + 1)
                min_remaining = float(remaining_prices.min())
                current_price = float(remaining_prices[0])
                # Percentile rank, NOT min-max rank: min-max compresses
                # almost everything near 0 whenever there's a single price
                # spike in the window (common with real electricity
                # prices), making "top 10% cheapest" indistinguishable from
                # "top 40% cheapest" to the network. Percentile (fraction
                # of remaining slots strictly cheaper than now) is robust
                # to that skew and is what actually matters for a
                # sort-and-take-cheapest optimal policy.
                price_rank_remaining = float(np.mean(remaining_prices < current_price))
                min_price_remaining = min_remaining

                # --- literal greedy-optimal recommendation ---
                # The exact-optimal policy (computed separately as our
                # ceiling metric) sorts this station's remaining slots by
                # price and takes the cheapest ones until req_kwh is met.
                # Compute that same threshold here and tell the agent
                # directly whether THIS slot would be one of the ones the
                # optimal algorithm picks -- handing it the decision rule
                # as an input, not the outcome of the whole episode. Still
                # has to learn to trust/combine it with curtailment risk,
                # urgency, etc., not just copy it.
                interval_h = self.interval_minutes / 60.0
                max_per_slot = power_kw * interval_h
                k_needed = int(np.ceil(remaining / (max_per_slot + 1e-9)))
                k_needed = max(1, min(k_needed, len(remaining_prices)))
                threshold_price = float(np.partition(remaining_prices, k_needed - 1)[k_needed - 1])
                should_charge_greedy = 1.0 if current_price <= threshold_price + 1e-9 else 0.0

            else:
                ev_present = 0.0
                remaining = 0.0
                hours_left = 0.0
                urgency = 0.0
                power_kw = 0.0
                price_rank_remaining = 0.0
                min_price_remaining = 0.0
                should_charge_greedy = 0.0
            station_feats.extend([
                ev_present, remaining, hours_left, urgency, power_kw,
                price_rank_remaining, min_price_remaining, should_charge_greedy,
            ])

        state = norm_prices + [window_min, window_max] + [sin_time, cos_time] + station_feats
        return np.array(state, dtype=np.float32)

    # ------------------------------------------------------------------
    #  State size helper
    # ------------------------------------------------------------------
    @property
    def observation_size(self) -> int:
        # (price_window+1) normalized prices + 2 (window_min, window_max)
        # + 2 (sin, cos) + 8 * n_stations
        return (self.price_window + 1) + 2 + 2 + 8 * self.n_stations

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
        self._sessions_at_cache = None
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
                "curtailed": np.zeros(self.n_stations, dtype=bool),
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
            "curtailed": np.zeros(self.n_stations, dtype=bool),
        }

        # ---- 3. Apply charging actions at t+1, subject to the shared -----
        #         power cap (excess requests are curtailed, i.e. treated as 0)
        requested = [
            s_idx for s_idx in range(self.n_stations)
            if s_idx in sessions and actions[s_idx] == 1
        ]
        allowed = self._select_charging_stations(t, sessions, requested)

        for s_idx in requested:
            if s_idx not in allowed:
                info["curtailed"][s_idx] = True
                continue
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
            max_possible = time_left_h * sess["power_kw"] * margin
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
            # per-station array now (positive = penalty magnitude, same sign
            # convention as info["penalties"] elsewhere) -- kept attributed
            # to the station it belongs to instead of collapsing to one
            # scalar, so callers building a per-station reward vector (see
            # dqn_agent.py train()) see it correctly, not folded away.
            skipped_penalties = self._skip_to_next_vehicle()
            reward -= float(skipped_penalties.sum())
            info["penalties"] += skipped_penalties

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

    def _skip_to_next_vehicle(self) -> np.ndarray:
        """Advance current_time until at least one EV is present.

        While skipping, if any EV departs with remaining energy a penalty
        is applied (they were never charged because no vehicle was flagged –
        this is a safety net).

        Returns a per-station array of penalty magnitudes (positive =
        penalty, same sign convention as info["penalties"] -- kept
        per-station rather than collapsed to one scalar so callers can
        still attribute it to the right station's reward).
        """
        penalty_total = np.zeros(self.n_stations)

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
                            penalty_total[s_idx] += pen
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
        Columns: session_id, station_id, arrival, departure, power_kw,
                 energy_needed_kwh, energy_delivered_kwh, unmet_kwh, pct_met,
                 energy_cost_eur, penalty_eur, avg_price_eur_per_kwh
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
            'power_kw': df['power_kw'].astype(float).values,
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
            f"default_power={self.default_power_kw}kW, "
            f"interval={self.interval_minutes}min, "
            f"price_window={self.price_window}, "
            f"state_size={self.observation_size}, "
            f"action_size={self.action_size})"
        )

    def normalize_state(self, state: np.ndarray) -> np.ndarray:

        norm_state = state.copy()

        # The (price_window+1) price slots are already normalized within
        # their own window by _build_state() -- only window_min/window_max
        # (raw €/kWh) still need scaling here.
        pos_window_min = self.price_window + 1
        pos_window_max = self.price_window + 2
        norm_state[pos_window_min] = norm_state[pos_window_min] / self.norm_max_price
        norm_state[pos_window_max] = norm_state[pos_window_max] / self.norm_max_price

        base = (self.price_window + 1) + 2 + 2  # prices + window min/max + sin, cos
        station_feats_num = 8  # should_charge_greedy (8th) is already 0/1, stays raw
        for i in range(self.n_stations):
            pos_remaining = base + station_feats_num * i + 1
            pos_till_dep = base + station_feats_num * i + 2
            pos_power = base + station_feats_num * i + 4
            pos_min_price_remaining = base + station_feats_num * i + 6

            remaining = norm_state[pos_remaining]
            t_until = norm_state[pos_till_dep]
            power = norm_state[pos_power]
            min_price_remaining = norm_state[pos_min_price_remaining]

            # Normalize ONLY these fields (see class docstring / CLAUDE.md:
            # presence/urgency/sin/cos/price_rank_remaining stay raw --
            # price_rank_remaining is already a 0-1 min-max ratio, same as
            # the window prices in _build_state())
            norm_state[pos_remaining] = remaining / self.norm_max_kwh
            norm_state[pos_till_dep] = t_until / self.norm_max_hours
            norm_state[pos_power] = power / self.norm_max_power_kw
            norm_state[pos_min_price_remaining] = min_price_remaining / self.norm_max_price

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
        # power_kw stays per-session; norm_max_power_kw is deliberately NOT
        # recomputed here, same reasoning as norm_max_hours/norm_max_kwh
        # below -- train and test must share one normalization scale.
        sessions_df["power_kw"] = self._power_column(sessions_df)
        sessions_df = sessions_df.sort_values("arrival").reset_index(drop=True)
        self.sessions_df = sessions_df
        # sessions_df just changed -- a cached _sessions_at() result from
        # before this swap would silently answer with the OLD dataset's
        # sessions if left in place, so it must be invalidated here.
        self._sessions_at_cache = None

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
        self._build_price_grid()

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
