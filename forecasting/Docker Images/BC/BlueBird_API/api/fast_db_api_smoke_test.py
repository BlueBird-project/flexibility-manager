"""Smoke test for fast_db_api (run the server first: uvicorn fast_db_api:app --port 8001, or the DB container).

  python fast_db_api_smoke_test.py [model] [base_url]

Checks: /health, empty-database behaviour (/status state 'empty_database', /forecast 409; only when the DB is empty),
energy_values-only pushes (weather/price rejected with 422 under the default config), /load_model, forecast with lower/upper,
and reading the stored forecast back. It writes a throw-away asset ('smoke-asset') into the API's database.
"""
import math
import sys
import time

import pandas as pd
import requests

MODEL = sys.argv[1] if len(sys.argv) > 1 else "chronos2"
BASE = (sys.argv[2] if len(sys.argv) > 2 else "http://127.0.0.1:8001").rstrip("/")


def call(method, path, **kw):
    r = requests.request(method, BASE + path, timeout=600, **kw)
    print(f"{method} {path} -> {r.status_code}")
    return r


def check(cond, msg):
    print(("OK   " if cond else "FAIL ") + msg)
    if not cond:
        sys.exit(1)


check(call("GET", "/health").status_code == 200, "health")
status = call("GET", "/status").json()
print([(m["model"], m["status"]) for m in status["models"]])
if status["db"].get("empty"):
    check(status["state"] == "empty_database", "/status reports empty_database")
    r = call("POST", "/forecast", json={"model": MODEL})
    check(r.status_code == 409 and "empty database" in r.json()["detail"], "/forecast on empty DB -> 409 'empty database'")
else:
    print("DB already has data: skipping the empty-database checks")

r = call("POST", "/push_data", json={"weather": [{"ds": "2026-01-01 00:00:00", "asset_id": 1, "weather_var": "t", "value": 1}]})
print("weather push ->", r.status_code, "(422 expected when push_tables=[energy_values])")

end = pd.Timestamp.now().floor("15min")
idx = pd.date_range(end - pd.Timedelta(days=3), end, freq="15min")
rows = [{"ds": str(t), "asset": "smoke-asset", "value": 10 + 5 * math.sin(2 * math.pi * (t.hour + t.minute / 60) / 24)} for t in idx]
r = call("POST", "/push_data", json={"create_missing_assets": True, "energy_values": rows})
check(r.status_code == 200 and r.json()["tables"]["energy_values"]["rejected"] == 0, "energy_values push")

r = call("POST", "/forecast", json={"assets": ["smoke-asset"], "model": MODEL})
print("forecast before load ->", r.status_code, r.json().get("detail"))  # 409 unless already loaded
call("POST", "/load_model", json={"model": MODEL})
for _ in range(600):
    state = next(m for m in call("GET", "/status").json()["models"] if m["model"] == MODEL)
    print(state["status"], state["load_seconds"])
    if state["status"] in ("ready", "failed"):
        break
    time.sleep(5)
check(state["status"] == "ready", f"model loaded ({state['error']})")

r = call("POST", "/forecast", json={"assets": ["smoke-asset"], "model": MODEL, "horizon": 8})
out = r.json()
check(r.status_code == 200 and len(out["yhat"]) == 8, "forecast returns 8 points")
print({k: (v[:3] if isinstance(v, list) else v) for k, v in out.items() if k != "assets"})
latest = call("GET", "/forecasts", params={"asset": "smoke-asset"}).json()
check(len(latest["ds"]) >= 8, "stored forecast read back")
print("lower/upper sample:", latest["yhat_lower"][:2], latest["yhat_upper"][:2])
print("Smoke test passed.")
