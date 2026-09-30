"""MPC for EV charging: per EV and interval, charge at full power or not, at minimum energy cost.

Two layers:
- `solve_horizon()` is the optimizer core: one MILP solve of the full look-ahead plan for the EVs
  and prices given to it. On its own this is just optimization, not MPC.
- `mpc_decide()` is the actual receding-horizon MPC step: it calls `solve_horizon()` but only
  returns the decision for the *current* interval (the one starting at `now`). The caller commits
  that decision, then calls `mpc_decide()` again at the next trigger (a 15-min tick, or a new EV
  arriving) with the then-current set of connected EVs -- discarding the rest of the old plan each
  time. That repeated call-and-commit loop is what makes it MPC.

All datetimes (EV times, price keys, `now`) must use one convention; prefer UTC, since naive local time breaks on DST days.
"""
from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Mapping, Optional, Sequence

import pulp

SLOT = timedelta(minutes=15)


@dataclass(frozen=True)
class EVSession:
    ev_id: str
    charger_id: str
    arrival_time: datetime
    departure_time: datetime
    energy_needed_kwh: float
    max_charging_power_kw: float


@dataclass(frozen=True)
class Interval:
    start_time: datetime
    end_time: datetime

    @property
    def duration_seconds(self) -> float:
        return (self.end_time - self.start_time).total_seconds()

    @property
    def duration_hours(self) -> float:
        return self.duration_seconds / 3600.0


def slot_start(t: datetime) -> datetime:
    return t - timedelta(minutes=t.minute % 15, seconds=t.second, microseconds=t.microsecond)


def _price_at(prices: Mapping[datetime, float]):
    """Price for a 15-min slot, held flat outside the supplied range.

    The horizon runs to the last departure, which can be further ahead than the prices the
    caller has (day-ahead prices reach 11-35 h; stays reach 38 h). Holding the last known
    price flat keeps the solve well-defined instead of raising KeyError.
    """
    if not prices:
        raise ValueError("prices must not be empty")
    ordered = sorted(prices)

    def value(slot: datetime) -> float:
        if slot in prices:
            return float(prices[slot])
        earlier = [s for s in ordered if s <= slot]
        return float(prices[earlier[-1]] if earlier else prices[ordered[0]])

    return value


def _pv_at(pv_forecast: Optional[Mapping[datetime, float]]):
    """Net PV energy (kWh) available in a 15-min slot; 0 outside the supplied range.

    Unknown means "assume no sun", so a plan never relies on production it was not told
    about. `None` disables PV entirely.
    """
    if not pv_forecast:
        return lambda slot: 0.0
    return lambda slot: max(0.0, float(pv_forecast.get(slot, 0.0)))


def build_time_grid(start: datetime, end: datetime, breakpoints: Sequence[datetime] = ()) -> list[Interval]:
    """Split [start, end) at every 15-min clock boundary and at every breakpoint inside it."""
    points = {start, end}
    t = slot_start(start) + SLOT
    while t < end:
        points.add(t)
        t += SLOT
    points.update(b for b in breakpoints if start < b < end)
    points = sorted(points)
    return [Interval(a, b) for a, b in zip(points, points[1:])]


def solve_horizon(
        evs: Sequence[EVSession],
        prices: Mapping[datetime, float],
        now: datetime,
        site_max_power_kw: Optional[float] = None,
        pv_forecast: Optional[Mapping[datetime, float]] = None,
        time_limit_s: float = 10.0,
) -> dict:
    """Optimizer core (not MPC by itself -- see module docstring). Plans charging from `now` until
    the last departure in one MILP solve.

    prices:       price per kWh, keyed by 15-min slot start. Held flat past the last key.
    pv_forecast:  optional net PV energy in kWh per 15-min slot (production minus any building
                  consumption, floored at 0 -- the caller nets it, so EV+PV and EV+PV+building
                  load are the same problem here). PV energy is free, so only the grid part of
                  each interval is paid for. None = no PV, and then the objective reduces to
                  exactly the EV-only one.
    site_max_power_kw: optional cap on the total power drawn across all EVs in any interval.

    An EV that cannot be filled within its remaining window has its requirement capped at what
    is physically deliverable, so one impossible EV charges flat out instead of making the whole
    site's problem infeasible.
    """
    evs = [ev for ev in evs if ev.departure_time > now]
    if not evs:
        return {"status": "optimal", "profile": [], "cost": 0.0}

    grid = build_time_grid(now, max(ev.departure_time for ev in evs),
                           [t for ev in evs for t in (ev.arrival_time, ev.departure_time)])
    price_of, pv_of = _price_at(prices), _pv_at(pv_forecast)
    price = [price_of(slot_start(iv.start_time)) for iv in grid]
    # PV is given per 15-min slot; a grid interval can be shorter where a breakpoint splits it
    pv = [pv_of(slot_start(iv.start_time)) * (iv.duration_seconds / SLOT.total_seconds())
          for iv in grid]

    E, T = range(len(evs)), range(len(grid))
    kwh = {(e, t): evs[e].max_charging_power_kw * grid[t].duration_hours for e in E for t in T}
    connected = {(e, t): int(evs[e].arrival_time <= grid[t].start_time
                             and grid[t].end_time <= evs[e].departure_time) for e in E for t in T}

    prob = pulp.LpProblem("ev_charging_mpc", pulp.LpMinimize)
    # binary charge decision, forced to 0 outside the EV's connection window
    x = {(e, t): prob.add_variable(f"x_{e}_{t}", lowBound=0, cat=pulp.LpInteger,
                                   upBound=connected[e, t])
         for e in E for t in T}

    if pv_forecast:
        # grid_t = max(0, drawn_t - pv_t): PV covers what it can, the rest is bought
        g = {t: prob.add_variable(f"g_{t}", lowBound=0) for t in T}
        prob += pulp.lpSum(price[t] * g[t] for t in T)
        for t in T:
            prob += g[t] >= pulp.lpSum(kwh[e, t] * x[e, t] for e in E) - pv[t]
    else:
        prob += pulp.lpSum(kwh[e, t] * price[t] * x[e, t] for e in E for t in T)

    for e in E:
        deliverable = sum(kwh[e, t] * connected[e, t] for t in T)
        prob += pulp.lpSum(kwh[e, t] * x[e, t] for t in T) >= min(evs[e].energy_needed_kwh,
                                                                  deliverable)
    if site_max_power_kw is not None:
        for t in T:
            prob += pulp.lpSum(evs[e].max_charging_power_kw * x[e, t] for e in E) <= site_max_power_kw

    stats = prob.solve(pulp.HiGHS(msg=False, timeLimit=time_limit_s, gapRel=0))
    status = stats.status.name.lower()  # optimal / infeasible / timelimit
    if not stats.has_solution:
        return {"status": status, "profile": [], "cost": None}

    plan = {k: round(v.value() or 0) for k, v in x.items()}
    drawn = [sum(kwh[e, t] * plan[e, t] for e in E) for t in T]
    cost = sum(price[t] * max(0.0, drawn[t] - pv[t]) for t in T)
    return {
        "status": status,
        "profile": [
            {"start": iv.start_time.isoformat(), "end": iv.end_time.isoformat(),
             **{evs[e].ev_id: plan[e, t] for e in E}}
            for t, iv in enumerate(grid)
        ],
        "cost": round(cost, 4),
    }


def mpc_decide(
        evs: Sequence[EVSession],
        prices: Mapping[datetime, float],
        now: datetime,
        site_max_power_kw: Optional[float] = None,
        pv_forecast: Optional[Mapping[datetime, float]] = None,
        time_limit_s: float = 10.0,
) -> dict:
    """One receding-horizon MPC step. Re-solves the full look-ahead plan via `solve_horizon()` but
    returns only the decision for the current interval (the one starting at `now`) -- that's what
    the caller should actually commit. Call this again at the next trigger (next 15-min tick, or a
    new EV arriving) with the then-current set of connected EVs; the rest of this call's plan is
    provisional and gets recomputed then, not carried over.
    """
    result = solve_horizon(evs, prices, now, site_max_power_kw, pv_forecast, time_limit_s)
    if not result["profile"]:
        return {"status": result["status"], "start": None, "end": None, "decisions": {}}

    current = result["profile"][0]
    return {
        "status": result["status"],
        "start": current["start"],
        "end": current["end"],
        "decisions": {k: v for k, v in current.items() if k not in ("start", "end")},
    }
