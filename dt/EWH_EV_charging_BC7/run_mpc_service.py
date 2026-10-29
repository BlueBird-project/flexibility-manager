#!/usr/bin/env python3
"""fm_charging: long-running MQTT service that plans EV charging at the plb charging station.

It listens to the bluebird broker (sessions, user decisions, meter readings, connector status,
plan acknowledgements), keeps a table of the cars connected to connectors c1..c4, and publishes
one charging plan (kW limit per quarter hour) per controllable car plus a charging baseline.
Interface: "bluebird - MQTT Interface for fm_charging (IMEC)". Prices: trading manager REST API.

Configuration (environment variables):
    MQTT_HOST           default sndbx.ewhindelang.de
    MQTT_PORT           default 8883 (TLS)
    MQTT_USERNAME       default fm_charging
    MQTT_PASSWORD_FILE  default /run/secrets/mqtt_password (file containing only the password)
    MQTT_CLIENT_ID      default fm_charging-1 (keep it stable)
    PRICE_API_BASE      default http://localhost:11001 (trading manager entsoe-service)
    PRICE_MARKET_ID     default 2 (Germany)
    SITE_LIMIT_KW       default 30, power limit shared by the four connectors
    DEFAULT_POWER_KW    default 22, assumed car power until measured
    BAU_HOURS           default 10, assumed stay of cars we may not control
    EFFICIENCY          default 0.95, battery kWh gained per kWh at the meter
    MPC_TIME_LIMIT_S    default 10, solver time limit per plan
"""
from __future__ import annotations

import json
import logging
import math
import os
import re
import signal
import sys
import threading
import uuid
from collections import OrderedDict
from dataclasses import dataclass, field
from datetime import datetime, timedelta, timezone
from pathlib import Path

import paho.mqtt.client as mqtt
import requests

ROOT = Path(__file__).resolve().parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from src.mpc.mpc import SLOT, PlanCar, plan_charging  # noqa: E402

UTC = timezone.utc
BUILDING = "plb"
CONNECTORS = (1, 2, 3, 4)
FLEX_HOURS = {"flex_12h": 12, "flex_24h": 24}
STATUS_TOPIC = f"bluebird/{BUILDING}/status/fm_charging"
BASELINE_ID = "plb_cs_energy_bl"
ACK_TIMEOUT_S = 60


def _env(name: str, default: str) -> str:
    return os.environ.get(name) or default


PRICE_API_BASE = _env("PRICE_API_BASE", "http://localhost:11001")
PRICE_MARKET_ID = int(_env("PRICE_MARKET_ID", "2"))
SITE_LIMIT_KW = float(_env("SITE_LIMIT_KW", "30"))
DEFAULT_POWER_KW = float(_env("DEFAULT_POWER_KW", "22"))
BAU_HOURS = float(_env("BAU_HOURS", "10"))
EFFICIENCY = float(_env("EFFICIENCY", "0.95"))
TIME_LIMIT_S = float(_env("MPC_TIME_LIMIT_S", "10"))

SUBSCRIPTIONS = [
    f"bluebird/{BUILDING}/charging_sessions/ocpp_manager/+",
    f"bluebird/{BUILDING}/user_events/ui/+",
] + [t for n in CONNECTORS for t in (
    f"bluebird/{BUILDING}/measurements/ocpp_manager/plb_cs_c{n}_energy",
    f"bluebird/{BUILDING}/measurements/ocpp_manager/plb_cs_c{n}_power",
    f"bluebird/{BUILDING}/states/ocpp_manager/plb_cs_c{n}_status",
    f"bluebird/{BUILDING}/command_acks/+/plb_cs_c{n}_power_op",
)]


# ---------- time helpers (everything is timezone-aware UTC) ----------

def parse_ts(s: str) -> datetime:
    return datetime.fromisoformat(s.replace("Z", "+00:00")).astimezone(UTC)


def iso(dt: datetime, ms: bool = False) -> str:
    return dt.astimezone(UTC).isoformat(timespec="milliseconds" if ms else "seconds").replace("+00:00", "Z")


def floor_q(dt: datetime) -> datetime:
    return dt.replace(minute=dt.minute // 15 * 15, second=0, microsecond=0)


def ceil_q(dt: datetime) -> datetime:
    f = floor_q(dt)
    return f if f == dt else f + SLOT


def overlap_h(a0: datetime, a1: datetime, b0: datetime, b1: datetime) -> float:
    return max(0.0, (min(a1, b1) - max(a0, b0)).total_seconds() / 3600)


# ---------- prices ----------

def fetch_prices(start: datetime, end: datetime) -> dict[datetime, float]:
    """EUR/kWh per 15-min slot start in [start, end), from the trading manager.

    The API's `ts` is the END of each quarter hour; when two sequences exist for a slot, sequence 1
    wins. Slots not published yet (day-ahead runs out) get the price of the same quarter hour on
    the most recent earlier day that has one.
    """
    ms = lambda dt: int(dt.timestamp() * 1000)  # noqa: E731
    url = (f"{PRICE_API_BASE}/api/market/{PRICE_MARKET_ID}/offer"
           f"?ts_from={ms(start - timedelta(days=2))}&ts_to={ms(end)}")
    response = requests.get(url, timeout=10)
    response.raise_for_status()

    known: dict[datetime, float] = {}
    for r in response.json():
        if r.get("ts") is None or r.get("cost") is None:
            continue
        slot = datetime.fromtimestamp(int(r["ts"]) / 1000, UTC) - SLOT
        if str(r.get("sequence")) == "1" or slot not in known:
            known[slot] = float(r["cost"]) / 1000.0  # EUR/MWh -> EUR/kWh

    prices, t = {}, start
    while t < end:
        same_time = (t - timedelta(days=d) for d in range(8))
        price = next((known[x] for x in same_time if x in known), None)
        if price is None:
            raise RuntimeError(f"no price for {iso(t)} or the same time in the past week ({url})")
        prices[t] = price
        t += SLOT
    return prices


# ---------- one charging session ----------

@dataclass
class Session:
    connector: int
    transaction_id: int
    arrival: datetime
    flex: str | None = None  # None: no user decision yet; "skip", "noflex", "flex_12h", "flex_24h"
    soc: float | None = None
    capacity: float | None = None
    status: str = ""
    meter_first: tuple[datetime, float] | None = None
    meter_last: tuple[datetime, float] | None = None
    power_kw: float = DEFAULT_POWER_KW
    full: bool = False
    plans: list[dict] = field(default_factory=list)  # every plan we sent, oldest first

    @property
    def controllable(self) -> bool:
        return self.flex in FLEX_HOURS and self.soc is not None and self.capacity is not None

    @property
    def departure(self) -> datetime:
        return self.arrival + timedelta(hours=FLEX_HOURS[self.flex] if self.controllable else BAU_HOURS)

    def need_kwh(self) -> float:
        """Energy at the meter for a full battery; infinite if the survey gave no soc/capacity."""
        if self.soc is None or self.capacity is None:
            return math.inf
        return max(0.0, (100.0 - self.soc) / 100.0 * self.capacity / EFFICIENCY)

    def delivered_kwh(self) -> float:
        return self.meter_last[1] - self.meter_first[1] if self.meter_first else 0.0

    def allowed_kw(self, t: datetime) -> float:
        """Power the station lets this car take at t: our latest accepted plan, else uncontrolled."""
        for plan in reversed(self.plans):
            if plan["accepted"] is False or plan["first"] > t:
                continue
            return plan["slots"].get(floor_q(t), 0.0) if t < plan["valid_until"] else DEFAULT_POWER_KW
        return DEFAULT_POWER_KW


# ---------- the service ----------

class FmCharging:
    def __init__(self, publish):
        self.publish = publish  # publish(topic, payload_dict, retain=False)
        self.sessions: dict[int, Session] = {}
        self.pending_ui: OrderedDict[int, dict] = OrderedDict()  # user decisions that came before 'started'
        self.seen: OrderedDict[tuple, None] = OrderedDict()  # duplicate detection (QoS 1)
        self.last_created: dict[str, datetime] = {}
        self.pending_acks: dict[str, threading.Timer] = {}
        self.lock = threading.Lock()

    # ----- incoming messages -----

    def on_message(self, topic: str, payload: bytes) -> None:
        try:
            msg = json.loads(payload)
            parts = topic.split("/")
            if not isinstance(msg, dict) or msg.get("v") != 1 or len(parts) != 5 or msg.get("id") != parts[4]:
                logging.warning("ignored malformed message on %s: %r", topic, payload[:300])
                return
            kind = parts[2]
            match = re.match(r"plb_cs_c([1-4])_", msg["id"])
            if not match:
                return
            connector = int(match.group(1))
            with self.lock:
                if self._duplicate(kind, msg):
                    return
                handler = {"charging_sessions": self._on_session, "user_events": self._on_user_event,
                           "measurements": self._on_measurement, "states": self._on_status,
                           "command_acks": self._on_ack}[kind]
                handler(msg, connector)
        except Exception:
            logging.exception("error handling message on %s: %r", topic, payload[:300])

    def _duplicate(self, kind: str, msg: dict) -> bool:
        key = {"charging_sessions": lambda: (msg["id"], msg["transaction_id"], msg["event"],
                                             msg["ts"] if msg["event"] == "issue" else None),
               "user_events": lambda: ("ui", msg["transaction_id"]),
               "command_acks": lambda: ("ack", msg["command_id"])}.get(kind, lambda: (msg["id"], msg["ts"]))()
        if key in self.seen:
            return True
        self.seen[key] = None
        while len(self.seen) > 10000:
            self.seen.popitem(last=False)
        return False

    def _on_session(self, msg: dict, c: int) -> None:
        event, tid, ts = msg["event"], msg["transaction_id"], parse_ts(msg["ts"])
        current = self.sessions.get(c)
        if event == "started":
            if current and current.transaction_id != tid:
                logging.warning("c%d: transaction %s ended without 'stopped'", c, current.transaction_id)
            session = Session(c, tid, ts)
            ui = self.pending_ui.pop(tid, None)
            if ui:
                self._apply_user_event(session, ui)
            self.sessions[c] = session
            logging.info("c%d: transaction %s started at %s", c, tid, iso(ts))
            self.replan(baseline=True)
        elif event == "stopped":
            if current and current.transaction_id == tid:
                del self.sessions[c]
                logging.info("c%d: transaction %s stopped (%s)", c, tid, msg.get("reason"))
                self.replan(baseline=True)
        elif event == "issue":
            logging.warning("c%d: transaction %s issue %s %s", c, tid, msg.get("issue"), msg.get("info", ""))

    def _on_user_event(self, msg: dict, c: int) -> None:
        session = self.sessions.get(c)
        if session and session.transaction_id == msg["transaction_id"]:
            self._apply_user_event(session, msg)
            self.replan(baseline=True)
        else:  # 'started' not here yet (it can arrive late)
            self.pending_ui[msg["transaction_id"]] = msg
            while len(self.pending_ui) > 100:
                self.pending_ui.popitem(last=False)

    def _apply_user_event(self, s: Session, msg: dict) -> None:
        s.flex = "skip" if msg["skip"] else msg.get("flex")
        s.soc, s.capacity = msg.get("soc"), msg.get("capacity")
        if s.flex in FLEX_HOURS and not s.controllable:
            logging.error("c%d: transaction %s has %s but no soc/capacity -- not controlled",
                          s.connector, s.transaction_id, s.flex)
        logging.info("c%d: transaction %s user decision %s (soc=%s, capacity=%s) -> %s",
                     s.connector, s.transaction_id, s.flex, s.soc, s.capacity,
                     "controllable" if s.controllable else "uncontrolled")

    def _on_measurement(self, msg: dict, c: int) -> None:
        s = self.sessions.get(c)
        if not s:
            return
        ts, value = parse_ts(msg["ts"]), float(msg["value"])
        if msg["id"].endswith("_power"):
            kw = self._station_kw(s, ts)
            if self._learn_power(s, value, (kw, 1.0 if kw > 0 else 0.0), may_finish=False):
                self.replan()
            return
        if s.meter_first is None:
            s.meter_first = s.meter_last = (ts, value)
            return
        prev_ts, prev_value = s.meter_last
        if ts <= prev_ts:
            return
        before = s.need_kwh() - s.delivered_kwh()  # still needed at the start of this interval
        s.meter_last = (ts, value)
        allowed_kw, allowed_h = self._allowed_avg(s, prev_ts, ts)
        if allowed_h > 0:
            measured_kw = (value - prev_value) / allowed_h  # average while it was allowed to charge
            self._learn_power(s, measured_kw, (allowed_kw, allowed_h),
                              may_finish=before <= allowed_kw * allowed_h + 0.5)  # 0.5 kWh: meter/soc slack
        logging.info("c%d: meter %.3f kWh, %.3f kWh since %s", c, value, value - prev_value, iso(prev_ts))
        self.replan()

    def _station_kw(self, s: Session, t: datetime) -> float:
        """What the station lets s take at t: its plan (or uncontrolled), cut to limit / N when the
        cars together want more than the site limit."""
        kw = s.allowed_kw(t)
        wanted = [min(x.power_kw, x.allowed_kw(t)) for x in self.sessions.values() if not x.full and x.arrival <= t]
        wanted = [w for w in wanted if w > 0]
        if sum(wanted) > SITE_LIMIT_KW:
            kw = min(kw, SITE_LIMIT_KW / len(wanted))
        return kw

    def _allowed_avg(self, s: Session, t0: datetime, t1: datetime) -> tuple[float, float]:
        """(average kW the station allowed while > 0, hours it allowed > 0) over [t0, t1]."""
        kwh, hours, t = 0.0, 0.0, t0
        while t < t1:
            nxt = min(floor_q(t) + SLOT, t1)
            kw = self._station_kw(s, t)
            if kw > 0:
                h = (nxt - t).total_seconds() / 3600
                kwh, hours = kwh + kw * h, hours + h
            t = nxt
        return (kwh / hours if hours else 0.0), hours

    def _remaining_kwh(self, s: Session, until: datetime) -> float:
        """Still to deliver after `until`: need - metered so far - expected until `until`."""
        if s.full:
            return 0.0
        since = s.meter_last[0] if s.meter_last else s.arrival
        expected, t = 0.0, since
        while t < until:
            nxt = min(floor_q(t) + SLOT, until)
            expected += min(s.power_kw, self._station_kw(s, t)) * (nxt - t).total_seconds() / 3600
            t = nxt
        return max(0.0, s.need_kwh() - s.delivered_kwh() - expected)

    def _learn_power(self, s: Session, measured_kw: float, allowed: tuple[float, float], may_finish: bool) -> bool:
        """Car took clearly less than allowed: that is its real power (or it is full). True if changed.
        `may_finish`: the car could have filled up during the interval, so a low value says nothing
        about its power."""
        allowed_kw, allowed_h = allowed
        if allowed_h <= 0 or measured_kw >= 0.9 * allowed_kw:
            return False
        if measured_kw < 0.5:
            if s.status == "suspended_ev" and not s.full:
                s.full = True
                logging.info("c%d: car takes no power while allowed (suspended_ev) -> treated as full", s.connector)
                return True
            return False
        if may_finish or abs(measured_kw - s.power_kw) < 0.1:
            return False
        logging.info("c%d: car power %.2f -> %.2f kW (allowed %.2f)", s.connector, s.power_kw, measured_kw, allowed_kw)
        s.power_kw = round(measured_kw, 2)
        return True

    def _on_status(self, msg: dict, c: int) -> None:
        s = self.sessions.get(c)
        if s:
            s.status = msg["value"]
        logging.info("c%d: status %s %s", c, msg["value"], msg.get("error_code", ""))

    def _on_ack(self, msg: dict, c: int) -> None:
        timer = self.pending_acks.pop(msg["command_id"], None)
        if timer:
            timer.cancel()
        for s in self.sessions.values():
            for plan in s.plans:
                if plan["command_id"] == msg["command_id"]:
                    plan["accepted"] = msg["result"] == "accepted"
        log = logging.info if msg["result"] == "accepted" else logging.warning
        log("c%d: plan %s %s %s", c, msg["command_id"], msg["result"], msg.get("info", ""))

    def _ack_missing(self, command_id: str) -> None:
        with self.lock:
            if self.pending_acks.pop(command_id, None):
                logging.warning("no ack for plan %s after %d s -- probably quarantined", command_id, ACK_TIMEOUT_S)

    # ----- planning -----

    def tick(self) -> None:
        with self.lock:
            self.replan()

    def replan(self, baseline: bool = False) -> None:
        now = datetime.now(UTC)
        if baseline:
            self._publish_baseline(now)

        first = ceil_q(now)  # decisions only for full quarter hours from the next one on
        ctrl = [s for s in self.sessions.values() if s.controllable and floor_q(s.departure) > first]
        if not ctrl:
            return
        end = max(floor_q(s.departure) for s in ctrl)
        slots = [first + i * SLOT for i in range((end - first) // SLOT)]
        try:
            prices = fetch_prices(first, end)
        except Exception as e:
            logging.error("no prices (%s) -- plans not updated", e)
            return

        cars = [PlanCar(s.power_kw, self._remaining_kwh(s, first), 0, (floor_q(s.departure) - first) // SLOT)
                for s in ctrl]
        uncontrolled = [s for s in self.sessions.values() if not s.controllable and not s.full]
        uncontrolled_kw = [[s.power_kw for s in uncontrolled if overlap_h(s.arrival, s.departure, t, t + SLOT) > 0]
                           for t in slots]
        result = plan_charging(cars, uncontrolled_kw, [prices[t] for t in slots], SITE_LIMIT_KW, TIME_LIMIT_S)
        if result is None:
            logging.error("solver found no plan -- plans not updated")
            return

        for s, car, kws in zip(ctrl, cars, result):
            schedule = dict(zip(slots[:car.end_slot], kws[:car.end_slot]))
            self._publish_plan_if_changed(s, schedule, first, slots[0] + car.end_slot * SLOT, now)

    def _publish_plan_if_changed(self, s: Session, schedule: dict, first: datetime,
                                 valid_until: datetime, now: datetime) -> None:
        last = next((p for p in reversed(s.plans) if p["accepted"] is not False), None)
        if last and last["valid_until"] == valid_until and all(
                abs(s.allowed_kw(t) - kw) < 0.05 for t, kw in schedule.items()):
            return
        values, prev = [], None
        for t, kw in schedule.items():
            if kw != prev:
                values.append({"ts": iso(t), "value": float(kw)})
                prev = kw
        pid = f"plb_cs_c{s.connector}_power_op"
        command_id = str(uuid.uuid4())
        self.publish(f"bluebird/{BUILDING}/commands/fm_charging/{pid}", {
            "v": 1, "command_id": command_id, "id": pid, "created": iso(self._created(pid, now), ms=True),
            "valid_until": iso(valid_until), "transaction_id": s.transaction_id, "values": values})
        s.plans.append({"command_id": command_id, "first": first, "valid_until": valid_until,
                        "slots": schedule, "accepted": None})
        timer = threading.Timer(ACK_TIMEOUT_S, self._ack_missing, [command_id])
        timer.daemon = True
        timer.start()
        self.pending_acks[command_id] = timer

    def _created(self, data_id: str, now: datetime) -> datetime:
        """'created' must be later than the previous version of the same id."""
        created = now.replace(microsecond=now.microsecond // 1000 * 1000)
        if data_id in self.last_created and created <= self.last_created[data_id]:
            created = self.last_created[data_id] + timedelta(milliseconds=1)
        self.last_created[data_id] = created
        return created

    def _publish_baseline(self, now: datetime) -> None:
        """Expected kWh per quarter hour without optimisation: every car charges from its arrival
        at its power until its battery is full (or its whole stay if the need is unknown)."""
        start = floor_q(now)
        sessions = list(self.sessions.values())
        end = max([ceil_q(s.departure) for s in sessions] + [start + SLOT])
        left = {s.connector: s.need_kwh() for s in sessions}
        t = min([floor_q(s.arrival) for s in sessions] + [start])
        values = []
        while t < end:
            charging = [s for s in sessions
                        if left[s.connector] > 1e-9 and overlap_h(s.arrival, s.departure, t, t + SLOT) > 0]
            limit_reached = sum(s.power_kw for s in charging) > SITE_LIMIT_KW
            total = 0.0
            for s in charging:
                kw = min(s.power_kw, SITE_LIMIT_KW / len(charging)) if limit_reached else s.power_kw
                kwh = min(left[s.connector], kw * overlap_h(s.arrival, s.departure, t, t + SLOT))
                left[s.connector] -= kwh
                total += kwh
            if t >= start:
                values.append({"ts": iso(t), "value": round(total, 3)})
            t += SLOT
        self.publish(f"bluebird/{BUILDING}/forecasts/fm_charging/{BASELINE_ID}", {
            "v": 1, "id": BASELINE_ID, "created": iso(self._created(BASELINE_ID, now), ms=True), "values": values})


# ---------- MQTT wiring ----------

def main() -> None:
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s", stream=sys.stdout)
    host = _env("MQTT_HOST", "sndbx.ewhindelang.de")
    port = int(_env("MQTT_PORT", "8883"))
    password = Path(_env("MQTT_PASSWORD_FILE", "/run/secrets/mqtt_password")).read_text().strip()

    client = mqtt.Client(mqtt.CallbackAPIVersion.VERSION2, client_id=_env("MQTT_CLIENT_ID", "fm_charging-1"),
                         protocol=mqtt.MQTTv311)
    client.username_pw_set(_env("MQTT_USERNAME", "fm_charging"), password)
    client.tls_set()  # system CA store, TLS >= 1.2; the server certificate is not pinned
    offline = json.dumps({"v": 1, "value": "offline"})
    client.will_set(STATUS_TOPIC, offline, qos=1, retain=True)

    def publish(topic: str, payload: dict, retain: bool = False) -> None:
        client.publish(topic, json.dumps(payload), qos=1, retain=retain)
        logging.info("sent %s %s", topic, json.dumps(payload))

    fm = FmCharging(publish)

    def on_connect(client, userdata, flags, reason_code, properties):
        if reason_code.is_failure:
            logging.error("MQTT connection refused: %s", reason_code)
            return
        client.subscribe([(t, 1) for t in SUBSCRIPTIONS])
        publish(STATUS_TOPIC, {"v": 1, "value": "online"}, retain=True)
        logging.info("connected to %s:%d, subscribed to %d topics", host, port, len(SUBSCRIPTIONS))

    client.on_connect = on_connect
    client.on_message = lambda c, u, m: fm.on_message(m.topic, m.payload)
    client.on_disconnect = lambda c, u, f, rc, p: logging.warning("MQTT disconnected (%s), reconnecting", rc)
    client.on_connect_fail = lambda c, u: logging.warning("MQTT connection to %s:%d failed, retrying", host, port)
    logging.info("site limit %.1f kW, default power %.1f kW, BAU stay %.1f h, efficiency %.2f, prices %s market %d",
                 SITE_LIMIT_KW, DEFAULT_POWER_KW, BAU_HOURS, EFFICIENCY, PRICE_API_BASE, PRICE_MARKET_ID)
    client.connect_async(host, port, keepalive=60)
    client.loop_start()  # network thread, reconnects automatically

    stop = threading.Event()
    signal.signal(signal.SIGTERM, lambda *_: stop.set())
    signal.signal(signal.SIGINT, lambda *_: stop.set())
    while True:  # re-plan at every quarter hour, a few seconds after :00/:15/:30/:45
        now = datetime.now(UTC)
        if stop.wait((ceil_q(now + timedelta(seconds=1)) - now).total_seconds() + 3):
            break
        fm.tick()

    client.publish(STATUS_TOPIC, offline, qos=1, retain=True).wait_for_publish(timeout=5)
    client.disconnect()
    client.loop_stop()


if __name__ == "__main__":
    main()
