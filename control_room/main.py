"""
Control Room — BlueBird operator dashboard.

REST API + HTML dashboard for monitoring and operating the system.

Endpoints:
  GET  /             HTML dashboard
  GET  /health       JSON health of this service
  GET  /status       Aggregate: FC status + last DB setpoints
  GET  /metrics      KPIs: recent setpoints, optimization costs, forecast stats
  GET  /services     Ping health endpoints of all services
  GET  /setpoints    Last N setpoints from DB (JSON)
  GET  /forecasts    Last N forecast rows from DB (JSON)

Environment variables:
  DB_PATH            path to SQLite database
  FC_URL             Flexibility Controller base URL (default: http://fc:8080)
  FORECAST_URL       Dummy forecast base URL (default: http://dummy-forecast:5000)
  PORT               port to listen on (default: 9000)
"""

import os
import sqlite3
import json
import time
import logging
from datetime import datetime, timezone
from typing import Any

import requests
from flask import Flask, jsonify, Response, request

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [CONTROL-ROOM] %(levelname)s %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
log = logging.getLogger(__name__)
logging.getLogger("werkzeug").setLevel(logging.ERROR)

app = Flask(__name__)

DB_PATH       = os.getenv("DB_PATH",      "/data/bb.db")
FC_URL        = os.getenv("FC_URL",       "http://fc:8080")
FORECAST_URL  = os.getenv("FORECAST_URL", "http://dummy-forecast:5000")
PORT          = int(os.getenv("PORT", "9000"))
SERVICE_NAME  = "control-room"

START_TIME = time.monotonic()


# ── DB helpers ────────────────────────────────────────────────────────────────

def _db() -> sqlite3.Connection:
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn


def _query(sql: str, params=()) -> list[dict]:
    try:
        with _db() as conn:
            rows = conn.execute(sql, params).fetchall()
            return [dict(r) for r in rows]
    except Exception as exc:
        log.warning("DB query failed: %s", exc)
        return []


# ── Service health helpers ────────────────────────────────────────────────────

def _ping(url: str, timeout: float = 8.0) -> dict:
    try:
        r = requests.get(url, timeout=timeout)
        return {"ok": r.status_code == 200, "status_code": r.status_code,
                "body": r.json() if r.headers.get("content-type", "").startswith("application/json") else r.text}
    except Exception as exc:
        return {"ok": False, "error": str(exc)}


# ── Routes ────────────────────────────────────────────────────────────────────

@app.route("/health")
def health():
    return jsonify({
        "status": "ok",
        "service": SERVICE_NAME,
        "uptime_s": round(time.monotonic() - START_TIME, 1),
        "timestamp": datetime.now(timezone.utc).isoformat(),
    })


@app.route("/status")
def status():
    fc = _ping(f"{FC_URL}/status")
    last_setpoints = _query(
        "SELECT * FROM setpoints ORDER BY timestamp DESC LIMIT 5"
    )
    last_error = _query(
        "SELECT MAX(timestamp) as ts FROM setpoints"
    )
    return jsonify({
        "fc": fc.get("body") if fc["ok"] else {"error": fc.get("error")},
        "last_setpoints": last_setpoints,
        "db_last_write": last_error[0]["ts"] if last_error else None,
    })


@app.route("/services")
def services():
    results = {
        "fc":              _ping(f"{FC_URL}/health"),
        "dummy-forecast":  _ping(f"{FORECAST_URL}/health"),
        "control-room":    {"ok": True, "status_code": 200},
    }
    all_ok = all(v["ok"] for v in results.values())
    return jsonify({"all_ok": all_ok, "services": results})


@app.route("/metrics")
def metrics():
    setpoints_count = _query("SELECT COUNT(*) as n FROM setpoints")
    recent_setpoints = _query(
        "SELECT timestamp, room_id, u, p3 FROM setpoints ORDER BY timestamp DESC LIMIT 20"
    )
    forecast_count = _query("SELECT COUNT(*) as n FROM disturbance_forecasts")
    measurement_count = _query("SELECT COUNT(*) as n FROM measurements")
    latest_measurement = _query(
        "SELECT * FROM measurements ORDER BY timestamp DESC LIMIT 5"
    )
    return jsonify({
        "setpoints_total": setpoints_count[0]["n"] if setpoints_count else 0,
        "forecasts_total": forecast_count[0]["n"] if forecast_count else 0,
        "measurements_total": measurement_count[0]["n"] if measurement_count else 0,
        "recent_setpoints": recent_setpoints,
        "latest_measurements": latest_measurement,
    })


@app.route("/setpoints")
def setpoints():
    rows = _query(
        "SELECT * FROM setpoints ORDER BY timestamp DESC LIMIT 50"
    )
    return jsonify(rows)


CLEARABLE_TABLES = ["setpoints", "measurements", "disturbance_forecasts"]


@app.route("/clear", methods=["POST"])
def clear():
    table = request.json.get("table") if request.json else None
    if table and table not in CLEARABLE_TABLES:
        return jsonify({"error": f"unknown table '{table}'"}), 400
    tables = [table] if table else CLEARABLE_TABLES
    deleted = {}
    try:
        with _db() as conn:
            for t in tables:
                n = conn.execute(f"SELECT COUNT(*) FROM {t}").fetchone()[0]
                conn.execute(f"DELETE FROM {t}")
                deleted[t] = n
        log.info("Cleared tables: %s", deleted)
        return jsonify({"cleared": deleted})
    except Exception as exc:
        log.error("Clear failed: %s", exc)
        return jsonify({"error": str(exc)}), 500


@app.route("/forecasts")
def forecasts():
    rows = _query(
        "SELECT * FROM disturbance_forecasts ORDER BY timestamp ASC LIMIT 50"
    )
    return jsonify(rows)


@app.route("/")
def dashboard():
    html = """<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="refresh" content="15">
<title>BlueBird Control Room</title>
<style>
  body { font-family: monospace; background: #111; color: #eee; margin: 2rem; }
  h1 { color: #4af; }
  h2 { color: #8cf; margin-top: 2rem; border-bottom: 1px solid #333; padding-bottom: 0.3rem; }
  .ok   { color: #4f4; }
  .err  { color: #f44; }
  .warn { color: #fa4; }
  table { border-collapse: collapse; width: 100%; margin-top: 0.5rem; }
  th { background: #222; text-align: left; padding: 6px 12px; color: #8cf; }
  td { padding: 5px 12px; border-bottom: 1px solid #222; }
  tr:hover td { background: #1a1a1a; }
  .card { background: #181818; border: 1px solid #2a2a2a; border-radius: 6px;
          padding: 1rem; margin: 0.5rem 0; }
  a { color: #4af; }
  .badge-ok  { background:#1a3a1a; color:#4f4; border-radius:4px; padding:2px 8px; }
  .badge-err { background:#3a1a1a; color:#f44; border-radius:4px; padding:2px 8px; }
</style>
</head>
<body>
<h1>🐦 BlueBird Control Room</h1>
<p style="color:#888">Auto-refreshes every 15 s &nbsp;|&nbsp;
  <a href="/health">health</a> &nbsp;
  <a href="/status">status</a> &nbsp;
  <a href="/metrics">metrics</a> &nbsp;
  <a href="/services">services</a> &nbsp;
  <a href="/setpoints">setpoints</a> &nbsp;
  <a href="/forecasts">forecasts</a>
</p>

<h2>Database</h2>
<div class="card">
  <button onclick="clearTable('setpoints')"    style="margin:4px;padding:6px 14px;background:#2a1a1a;color:#f44;border:1px solid #f44;border-radius:4px;cursor:pointer">Clear setpoints</button>
  <button onclick="clearTable('measurements')" style="margin:4px;padding:6px 14px;background:#2a1a1a;color:#f44;border:1px solid #f44;border-radius:4px;cursor:pointer">Clear measurements</button>
  <button onclick="clearTable('disturbance_forecasts')" style="margin:4px;padding:6px 14px;background:#2a1a1a;color:#f44;border:1px solid #f44;border-radius:4px;cursor:pointer">Clear forecasts</button>
  <button onclick="clearTable(null)"           style="margin:4px;padding:6px 14px;background:#3a0000;color:#f44;border:2px solid #f44;border-radius:4px;cursor:pointer;font-weight:bold">⚠ Clear ALL</button>
  <span id="clear-msg" style="margin-left:12px;color:#fa4"></span>
</div>

<h2>Services</h2>
<div id="services-placeholder" class="card">Loading…</div>

<h2>Flexibility Controller</h2>
<div id="fc-placeholder" class="card">Loading…</div>

<h2>Recent Setpoints (DB)</h2>
<div id="setpoints-placeholder" class="card">Loading…</div>

<script>
async function clearTable(table) {
  const label = table || 'ALL tables';
  if (!confirm(`Delete all rows from ${label}?`)) return;
  const body = table ? JSON.stringify({table}) : '{}';
  const r = await fetch('/clear', {method:'POST', headers:{'Content-Type':'application/json'}, body});
  const j = await r.json();
  const msg = j.error ? `Error: ${j.error}` : 'Cleared: ' + JSON.stringify(j.cleared);
  document.getElementById('clear-msg').innerText = msg;
  setTimeout(() => document.getElementById('clear-msg').innerText = '', 5000);
}

async function load(url) {
  try { return await (await fetch(url)).json(); }
  catch(e) { return {error: String(e)}; }
}

function badge(ok) {
  return ok ? '<span class="badge-ok">OK</span>' : '<span class="badge-err">DOWN</span>';
}

async function refresh() {
  // Services
  const sv = await load('/services');
  let shtml = '<table><tr><th>Service</th><th>Status</th><th>Detail</th></tr>';
  for (const [name, info] of Object.entries(sv.services || {})) {
    shtml += `<tr><td>${name}</td><td>${badge(info.ok)}</td><td>${info.error || info.status_code || ''}</td></tr>`;
  }
  shtml += '</table>';
  document.getElementById('services-placeholder').innerHTML = shtml;

  // FC status
  const st = await load('/status');
  const fc = st.fc || {};
  let fhtml = '<table>';
  for (const [k, v] of Object.entries(fc)) {
    fhtml += `<tr><td style="color:#8cf;width:180px">${k}</td><td>${JSON.stringify(v)}</td></tr>`;
  }
  if (st.db_last_write) fhtml += `<tr><td style="color:#8cf">db_last_write</td><td>${st.db_last_write}</td></tr>`;
  fhtml += '</table>';
  document.getElementById('fc-placeholder').innerHTML = fhtml || '<span class="warn">No data</span>';

  // Setpoints
  const sp = st.last_setpoints || [];
  if (sp.length === 0) {
    document.getElementById('setpoints-placeholder').innerHTML = '<span class="warn">No setpoints written yet.</span>';
  } else {
    let thtml = '<table><tr>';
    Object.keys(sp[0]).forEach(k => thtml += `<th>${k}</th>`);
    thtml += '</tr>';
    sp.forEach(row => {
      thtml += '<tr>';
      Object.values(row).forEach(v => thtml += `<td>${v}</td>`);
      thtml += '</tr>';
    });
    thtml += '</table>';
    document.getElementById('setpoints-placeholder').innerHTML = thtml;
  }
}

refresh();
setInterval(refresh, 15000);
</script>
</body>
</html>"""
    return Response(html, mimetype="text/html")


if __name__ == "__main__":
    log.info("Control Room starting on port %d", PORT)
    app.run(host="0.0.0.0", port=PORT, debug=False)
