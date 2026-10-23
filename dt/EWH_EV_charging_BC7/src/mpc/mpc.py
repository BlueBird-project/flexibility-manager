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
    energy_needed_kwh: float  # into the battery
    max_charging_power_kw: float  # drawn from the grid
    efficiency: float = 1.0  # battery kWh gained per grid kWh


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
        time_limit_s: float = 10.0,
) -> dict:
    """Optimizer core (not MPC by itself -- see module docstring). Plans charging from `now` until
    the last departure in one MILP solve. `prices`: price per kWh, keyed by 15-min slot start."""
    evs = [ev for ev in evs if ev.departure_time > now]
    if not evs:
        return {"status": "optimal", "profile": [], "cost": 0.0}

    grid = build_time_grid(now, max(ev.departure_time for ev in evs),
                           [t for ev in evs for t in (ev.arrival_time, ev.departure_time)])
    price = [float(prices[slot_start(iv.start_time)]) for iv in grid]

    E, T = range(len(evs)), range(len(grid))
    kwh = {(e, t): evs[e].max_charging_power_kw * grid[t].duration_hours for e in E for t in T}

    prob = pulp.LpProblem("ev_charging_mpc", pulp.LpMinimize)
    # binary charge decision, forced to 0 outside the EV's connection window
    x = {(e, t): prob.add_variable(f"x_{e}_{t}", lowBound=0, cat=pulp.LpInteger,
                                   upBound=int(evs[e].arrival_time <= grid[t].start_time
                                               and grid[t].end_time <= evs[e].departure_time))
         for e in E for t in T}

    prob += pulp.lpSum(kwh[e, t] * price[t] * x[e, t] for e in E for t in T)
    for e in E:
        prob += pulp.lpSum(kwh[e, t] * evs[e].efficiency * x[e, t] for t in T) >= evs[e].energy_needed_kwh
    if site_max_power_kw is not None:
        for t in T:
            prob += pulp.lpSum(evs[e].max_charging_power_kw * x[e, t] for e in E) <= site_max_power_kw

    stats = prob.solve(pulp.HiGHS(msg=False, timeLimit=time_limit_s, gapRel=0))
    status = stats.status.name.lower()  # optimal / infeasible / timelimit
    if not stats.has_solution:
        return {"status": status, "profile": [], "cost": None}

    plan = {k: round(v.value() or 0) for k, v in x.items()}
    return {
        "status": status,
        "profile": [
            {"start": iv.start_time.isoformat(), "end": iv.end_time.isoformat(),
             **{evs[e].ev_id: plan[e, t] for e in E}}
            for t, iv in enumerate(grid)
        ],
        "cost": round(sum(kwh[e, t] * price[t] * plan[e, t] for e in E for t in T), 4),
    }


SLOT_H = 0.25
_SHORTFALL_EUR_PER_KWH = 1000.0  # missing energy is allowed only as a last resort
_BIG = 1000.0


@dataclass(frozen=True)
class PlanCar:
    power_kw: float  # max power the car can take
    energy_kwh: float  # still to deliver, measured at the meter
    first_slot: int  # first slot index it may be planned in
    end_slot: int  # first slot index after its stay (exclusive)


def plan_charging(
        cars: Sequence[PlanCar],
        uncontrolled_kw: Sequence[Sequence[float]],
        prices: Sequence[float],
        site_limit_kw: float,
        time_limit_s: float = 10.0,
) -> Optional[list[list[float]]]:
    """kW limit per car per 15-min slot, minimising energy cost (used by the MQTT service).

    `uncontrolled_kw[t]`: power of every uncontrolled car charging in slot t. Site rule: if the
    powers of all cars charging in a slot exceed `site_limit_kw`, each of them gets limit / N
    (N = cars charging in that slot). Energy that can't fit is a heavily penalised shortfall, so
    the plan always exists and gives the car as much as possible.
    """
    prob = pulp.LpProblem("ev_plan", pulp.LpMinimize)
    p, z = {}, {}
    for e, car in enumerate(cars):
        for t in range(car.first_slot, car.end_slot):
            z[e, t] = prob.add_variable(f"z_{e}_{t}", cat=pulp.LpBinary)
            p[e, t] = prob.add_variable(f"p_{e}_{t}", lowBound=0, upBound=car.power_kw)
            prob += p[e, t] <= car.power_kw * z[e, t]
    short = [prob.add_variable(f"short_{e}", lowBound=0) for e in range(len(cars))]

    # + 1e-6/slot: among equally priced slots, charge earlier (more time left to correct the plan)
    prob += (pulp.lpSum((prices[t] + 1e-6 * t) * SLOT_H * v for (_, t), v in p.items())
             + _SHORTFALL_EUR_PER_KWH * pulp.lpSum(short))
    for e, car in enumerate(cars):
        prob += pulp.lpSum(p[e, t] * SLOT_H for t in range(car.first_slot, car.end_slot)) + short[e] >= car.energy_kwh

    for t in range(len(prices)):
        on = [e for e in range(len(cars)) if (e, t) in z]
        if not on:
            continue
        m = len(uncontrolled_kw[t])
        split = prob.add_variable(f"split_{t}", cat=pulp.LpBinary)  # 1 = limit reached in this slot
        prob += (pulp.lpSum(cars[e].power_kw * z[e, t] for e in on) + sum(uncontrolled_kw[t])
                 <= site_limit_kw + _BIG * split)
        # y[n] = 1 <=> n cars charge in slot t (uncontrolled ones always do)
        y = {n: prob.add_variable(f"y_{t}_{n}", cat=pulp.LpBinary) for n in range(m, m + len(on) + 1)}
        prob += pulp.lpSum(y.values()) == 1
        prob += pulp.lpSum(z[e, t] for e in on) == pulp.lpSum((n - m) * y[n] for n in y)
        for e in on:
            for n in y:
                if n > 0:
                    prob += p[e, t] <= site_limit_kw / n + _BIG * (1 - y[n]) + _BIG * (1 - split)

    stats = prob.solve(pulp.HiGHS(msg=False, timeLimit=time_limit_s, gapRel=0))
    if not stats.has_solution:
        return None
    return [[max(0.0, round(p[e, t].value() or 0.0, 2)) if (e, t) in p else 0.0 for t in range(len(prices))]
            for e in range(len(cars))]


def mpc_decide(
        evs: Sequence[EVSession],
        prices: Mapping[datetime, float],
        now: datetime,
        site_max_power_kw: Optional[float] = None,
        time_limit_s: float = 10.0,
) -> dict:
    """One receding-horizon MPC step. Re-solves the full look-ahead plan via `solve_horizon()` but
    returns only the decision for the current interval (the one starting at `now`) -- that's what
    the caller should actually commit. Call this again at the next trigger (next 15-min tick, or a
    new EV arriving) with the then-current set of connected EVs; the rest of this call's plan is
    provisional and gets recomputed then, not carried over.
    """
    result = solve_horizon(evs, prices, now, site_max_power_kw, time_limit_s)
    if not result["profile"]:
        return {"status": result["status"], "start": None, "end": None, "decisions": {}}

    current = result["profile"][0]
    return {
        "status": result["status"],
        "start": current["start"],
        "end": current["end"],
        "decisions": {k: v for k, v in current.items() if k not in ("start", "end")},
    }
