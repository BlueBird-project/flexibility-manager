"""
Dummy Disturbances Forecast service.

Writes synthetic disturbance forecasts to the database on a configurable cadence.
Interface-compatible with the real NeuralForecast service: writes rows to the
`disturbance_forecasts` table with columns (timestamp, room_id, T_ambient, door_load).

Reference pattern: FlexOPTi/scripts/ewh/mpc_base.jl (constant 18°C ambient, zero door load).

Environment variables:
  DB_PATH              path to the SQLite database file
  DB_FORECASTS_TABLE   table name for disturbance forecasts (default: disturbance_forecasts)
  CADENCE_S            seconds between forecast updates (default: 900)
  HORIZON_STEPS        number of forecast steps to write (default: 48)
  DELTA_T_S            seconds per step (default: 900)
  N_ROOMS              number of rooms (default: 5)
  T_AMBIENT            constant ambient temperature in °C (default: 18.0)
"""

import os
import sqlite3
import time
import logging
import threading
from datetime import datetime, timedelta, timezone
from http.server import BaseHTTPRequestHandler, HTTPServer
import json as _json

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [DUMMY-FORECAST] %(levelname)s %(message)s",
)
log = logging.getLogger(__name__)

DB_PATH            = os.getenv("DB_PATH", "/data/bb.db")
FORECASTS_TABLE    = os.getenv("DB_FORECASTS_TABLE", "disturbance_forecasts")
HEALTH_PORT        = int(os.getenv("HEALTH_PORT", "5000"))
CADENCE_S          = int(os.getenv("CADENCE_S", "900"))
HORIZON_STEPS      = int(os.getenv("HORIZON_STEPS", "48"))
DELTA_T_S          = int(os.getenv("DELTA_T_S", "900"))
N_ROOMS            = int(os.getenv("N_ROOMS", "5"))
T_AMBIENT          = float(os.getenv("T_AMBIENT", "18.0"))


SCHEMA_SQL = """
CREATE TABLE IF NOT EXISTS measurements (
    id        INTEGER PRIMARY KEY,
    timestamp TEXT    NOT NULL,
    room_id   INTEGER NOT NULL,
    T_air     REAL    NOT NULL,
    T_product REAL    NOT NULL
);

CREATE TABLE IF NOT EXISTS disturbance_forecasts (
    id        INTEGER PRIMARY KEY,
    timestamp TEXT    NOT NULL,
    room_id   INTEGER NOT NULL,
    T_ambient REAL    NOT NULL,
    door_load REAL    NOT NULL DEFAULT 0.0
);

CREATE TABLE IF NOT EXISTS setpoints (
    id        INTEGER PRIMARY KEY,
    timestamp TEXT    NOT NULL,
    room_id   INTEGER NOT NULL,
    u         REAL    NOT NULL,
    p3        REAL    NOT NULL DEFAULT 0.0
);
"""

SEED_SQL = """
INSERT OR IGNORE INTO measurements (id, timestamp, room_id, T_air, T_product) VALUES
    (1, datetime('now'), 1,  4.5,   4.8),
    (2, datetime('now'), 2,  2.1,   2.3),
    (3, datetime('now'), 3,  3.8,   4.0),
    (4, datetime('now'), 4,  5.2,   5.5),
    (5, datetime('now'), 5, -18.3, -18.0);
"""


def init_schema(conn: sqlite3.Connection) -> None:
    conn.executescript(SCHEMA_SQL)
    conn.executescript(SEED_SQL)
    conn.commit()
    log.info("Schema initialised.")


def wait_for_db(path: str, retries: int = 30, interval: float = 2.0) -> sqlite3.Connection:
    for attempt in range(1, retries + 1):
        try:
            conn = sqlite3.connect(path)
            conn.execute("SELECT 1")
            log.info("Connected to database: %s", path)
            return conn
        except Exception as exc:
            log.warning("DB not ready (attempt %d/%d): %s", attempt, retries, exc)
            time.sleep(interval)
    raise RuntimeError(f"Cannot connect to database at {path} after {retries} attempts")


def write_forecasts(conn: sqlite3.Connection) -> None:
    now = datetime.now(timezone.utc)
    rows = []
    for step in range(HORIZON_STEPS):
        ts = now + timedelta(seconds=step * DELTA_T_S)
        ts_str = ts.strftime("%Y-%m-%d %H:%M:%S")
        for room in range(1, N_ROOMS + 1):
            rows.append((ts_str, room, T_AMBIENT, 0.0))

    conn.executemany(
        f"INSERT INTO {FORECASTS_TABLE} (timestamp, room_id, T_ambient, door_load) "
        "VALUES (?, ?, ?, ?)",
        rows,
    )
    conn.commit()
    log.info(
        "Wrote %d forecast rows (%d steps × %d rooms, T_ambient=%.1f°C)",
        len(rows), HORIZON_STEPS, N_ROOMS, T_AMBIENT,
    )


_health_state: dict = {"status": "starting", "service": "dummy-forecast"}
_start_time = time.monotonic()


class _HealthHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path in ("/health", "/"):
            body = _json.dumps({
                **_health_state,
                "uptime_s": round(time.monotonic() - _start_time, 1),
            }).encode()
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(body)
        else:
            self.send_response(404)
            self.end_headers()

    def log_message(self, *args):
        pass  # suppress access log noise


def _start_health_server() -> None:
    server = HTTPServer(("0.0.0.0", HEALTH_PORT), _HealthHandler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    log.info("Health endpoint listening on :%d/health", HEALTH_PORT)


def main() -> None:
    log.info(
        "Starting dummy forecast service  cadence=%ds  horizon=%d steps  T_amb=%.1f°C",
        CADENCE_S, HORIZON_STEPS, T_AMBIENT,
    )
    _start_health_server()
    conn = wait_for_db(DB_PATH)
    init_schema(conn)
    _health_state["status"] = "ok"
    while True:
        t0 = time.monotonic()
        try:
            write_forecasts(conn)
        except Exception as exc:
            log.error("Failed to write forecasts: %s", exc)
            conn = wait_for_db(DB_PATH)
        elapsed = time.monotonic() - t0
        sleep_s = max(0.0, CADENCE_S - elapsed)
        log.info("Next update in %.0fs", sleep_s)
        time.sleep(sleep_s)


if __name__ == "__main__":
    main()
