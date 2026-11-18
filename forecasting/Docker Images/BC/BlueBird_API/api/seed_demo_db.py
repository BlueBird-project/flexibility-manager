"""Creates a demo SQLite database for fast_db_api from the existing BC1 CSVs (PV and energy consumption).

  python seed_demo_db.py [--db fast_db.sqlite] [--tail-days 30] [--weather --lat 50.38 --lon 4.88]

Uses the exact schema in db_forecast_schema.sql. PV: data/pv/BC1*/pv_data.csv (ds, unique_id, y).
Energy: data/energy_consumption/BC1*/*.csv, target = `delta` only (cumulative meter columns are ignored).
--weather downloads Open-Meteo archive (history) + forecast (future) for the demo location (needs internet).
Only the last --tail-days of each series are loaded to keep the demo small. Nothing here trains or forecasts.
"""
import argparse
import sqlite3
from pathlib import Path

import pandas as pd

APP_DIR = Path(__file__).resolve().parent
ROOT = APP_DIR.parent
TS = "%Y-%m-%d %H:%M:%S"
WEATHER_VARS = ["temperature_2m", "shortwave_radiation", "cloud_cover", "wind_speed_10m", "relative_humidity_2m"]


def first_file(pattern: str) -> Path:
    files = sorted(ROOT.glob(pattern))
    if not files:
        raise SystemExit(f"No file matches {pattern} under {ROOT}")
    return files[0]


def tail(df: pd.DataFrame, days: float) -> pd.DataFrame:
    return df[df["ds"] >= df["ds"].max() - pd.Timedelta(days=days)]


def fetch_weather(lat: float, lon: float, start: pd.Timestamp, end: pd.Timestamp, past_days: int):
    import requests

    base = {"latitude": lat, "longitude": lon, "hourly": ",".join(WEATHER_VARS), "timezone": "Europe/Brussels"}
    archive = requests.get("https://archive-api.open-meteo.com/v1/archive", timeout=60, params={
        **base, "start_date": start.date().isoformat(), "end_date": end.date().isoformat()}).json()
    future = requests.get("https://api.open-meteo.com/v1/forecast", timeout=60, params={**base, "forecast_days": 3, "past_days": 0}).json()
    frames = []
    for payload, is_forecast in ((archive, 0), (future, 1)):
        if "hourly" not in payload:
            continue
        wide = pd.DataFrame(payload["hourly"]).rename(columns={"time": "ds"})
        long = wide.melt("ds", var_name="weather_var", value_name="value").dropna()
        long["forecast"] = is_forecast
        frames.append(long)
    return pd.concat(frames, ignore_index=True)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--db", default=str(APP_DIR / "fast_db.sqlite"))
    ap.add_argument("--schema", default=str(APP_DIR / "db_forecast_schema.sql"))
    ap.add_argument("--tail-days", type=float, default=30)
    ap.add_argument("--weather", action="store_true")
    ap.add_argument("--lat", type=float, default=50.38)
    ap.add_argument("--lon", type=float, default=4.88)
    args = ap.parse_args()

    pv = pd.read_csv(first_file("data/pv/BC1*/pv_data.csv"), parse_dates=["ds"])
    en = pd.read_csv(first_file("data/energy_consumption/BC1*/*.csv"), parse_dates=["time"]).rename(columns={"time": "ds", "delta": "y"})
    series = {
        ("Karno-PV", "pv"): tail(pv[["ds", "y"]].dropna(), args.tail_days),
        ("Karno-Heating", "energy_consumption"): tail(en[["ds", "y"]].dropna(), args.tail_days),
    }

    conn = sqlite3.connect(args.db)
    conn.executescript(Path(args.schema).read_text(encoding="utf-8"))
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("INSERT OR IGNORE INTO price_tag(id, country, type) VALUES (1, 'BE', 'day-ahead')")
    for name, kind in {n: t for n, t in series}.items():
        conn.execute("INSERT INTO assets(name, location_id, type, price_tag_id) SELECT ?, 1, ?, 1 WHERE NOT EXISTS (SELECT 1 FROM assets WHERE name=?)", (name, kind, name))
    for (name, _), df in series.items():
        asset_id = conn.execute("SELECT asset_id FROM assets WHERE name=?", (name,)).fetchone()[0]
        rows = [(t.strftime(TS), asset_id, float(v)) for t, v in zip(df["ds"], df["y"])]
        conn.executemany("INSERT INTO energy_values(ds, asset_id, value) VALUES (?,?,?) ON CONFLICT(ds, asset_id) DO UPDATE SET value=excluded.value", rows)
        print(f"{name}: {len(rows)} values, last {rows[-1][0]}")
        if args.weather:
            w = fetch_weather(args.lat, args.lon, df["ds"].min().normalize(), df["ds"].max().normalize(), 0)
            conn.execute("DELETE FROM weather WHERE asset_id=?", (asset_id,))
            conn.executemany("INSERT INTO weather(ds, asset_id, weather_var, value, forecast) VALUES (?,?,?,?,?)",
                             [(str(r.ds).replace("T", " ") + ":00", asset_id, r.weather_var, float(r.value), int(r.forecast)) for r in w.itertuples()])
            print(f"  weather rows: {len(w)}")
    conn.commit()
    conn.close()
    print(f"Database ready: {args.db}")


if __name__ == "__main__":
    main()
