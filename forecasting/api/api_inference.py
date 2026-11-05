"""FastAPI service for PriceFM+ price forecasts (FAST Forge/Translate pipeline).

  uvicorn api_inference:app --host 0.0.0.0 --port 8000

  POST /forecast/production  {"regions": ["BE", "ES"], "days": 2}
  POST /forecast/backtest    {"regions": ["BE"], "start_date": "20260201", "end_date": "20260202"}
  GET  /health

Both return {"ds": [...], "region_name": [...], "yhat": [...]} (ds in UTC, yhat in EUR/MWh);
backtest also returns "actual" (observed price or null).
ENTSO-E key: "token" in credentials_entsoe.json (path override: ENTSOE_CREDENTIALS_FILE).
Data/weights folder: PRICEFM_WORKDIR (default ~/PriceFM_Plus_forecast).
"""

import json
import os
import threading
from datetime import datetime, timedelta, timezone
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field

import price_inference as core

APP_DIR = core.APP_DIR
MAX_BACKTEST_DAYS = int(os.environ.get("MAX_BACKTEST_DAYS", 62))
MAX_PRODUCTION_DAYS = int(os.environ.get("MAX_PRODUCTION_DAYS", 7))
# The Forge/Translate change the process working directory, so only one run at a time.
RUN_LOCK = threading.Lock()


def entsoe_key():
    path = Path(os.environ.get("ENTSOE_CREDENTIALS_FILE", APP_DIR / "credentials_entsoe.json"))
    if not path.is_file():
        raise HTTPException(500, f"Missing ENTSO-E credentials file: {path}")
    try:
        return json.loads(path.read_text())["token"]
    except (KeyError, ValueError) as exc:
        raise HTTPException(500, f"{path.name} must be JSON with a 'token' field") from exc


def settings():
    workdir = Path(os.environ.get("PRICEFM_WORKDIR", Path.home() / "PriceFM_Plus_forecast"))
    workdir = workdir.expanduser().resolve()
    workdir.mkdir(parents=True, exist_ok=True)
    return SimpleNamespace(workdir=str(workdir), history_days=10)


class ProductionRequest(BaseModel):
    regions: list[str] = Field(min_length=1)
    days: int = Field(default=1, ge=1)


class BacktestRequest(BaseModel):
    regions: list[str] = Field(min_length=1)
    start_date: str = Field(description="YYYYMMDD")
    end_date: str = Field(description="YYYYMMDD")


def parse_day(text):
    try:
        return datetime.strptime(text, "%Y%m%d").date()
    except ValueError:
        raise HTTPException(400, f"Invalid date '{text}': use YYYYMMDD")


def observed_prices(workdir, regions, first, last):
    """Observed day-ahead prices (natively reported slots only) as {(region, ds): price}."""
    prices = {}
    for region in regions:
        path = Path(workdir) / "Data" / "Processed" / "per_region_feature_interp" / f"{region}_DayAheadPrice.csv"
        if not path.is_file():
            continue
        frame = pd.read_csv(path, usecols=["timestamp_utc", "value", "source_observed"])
        frame = frame[frame["source_observed"].astype(bool)]
        ds = pd.to_datetime(frame["timestamp_utc"], utc=True).dt.tz_localize(None)
        keep = (ds >= pd.Timestamp(first)) & (ds < pd.Timestamp(last) + pd.Timedelta(days=1))
        prices.update({(region, t): v for t, v in zip(ds[keep], frame["value"][keep])})
    return prices


def run_days(days, regions, with_actual=False):
    regions = list(dict.fromkeys(regions))
    key, args = entsoe_key(), settings()
    frames, failed = [], []
    with RUN_LOCK:
        for day in days:
            try:
                frames.append(core.forecast_day(day, regions, args, key))
            except Exception as exc:
                failed.append(f"{day}: {exc!r}")
    if not frames:
        raise HTTPException(500, f"No forecasts produced. {'; '.join(failed)}")
    result = pd.concat(frames, ignore_index=True).sort_values(["region_name", "ds"])
    response = {"ds": result["ds"].dt.strftime("%Y-%m-%d %H:%M:%S").tolist(),
                "region_name": result["region_name"].tolist(),
                "yhat": result["yhat"].tolist(),
                "failed_days": failed}
    if with_actual:
        prices = observed_prices(args.workdir, regions, min(days), max(days))
        response["actual"] = [prices.get((r, t)) for r, t in zip(result["region_name"], result["ds"])]
    return response


app = FastAPI(title="PriceFM+ Price Forecast API", version="1.0.0")


@app.get("/health")
def health():
    entsoe_key()
    return {"status": "ok"}


@app.post("/forecast/production")
def production(request: ProductionRequest):
    if request.days > MAX_PRODUCTION_DAYS:
        raise HTTPException(400, f"days must be at most {MAX_PRODUCTION_DAYS}")
    first = datetime.now(timezone.utc).date() + timedelta(days=1)
    return run_days([first + timedelta(days=i) for i in range(request.days)], request.regions)


@app.post("/forecast/backtest")
def backtest(request: BacktestRequest):
    start, end = parse_day(request.start_date), parse_day(request.end_date)
    if end < start:
        raise HTTPException(400, "end_date is before start_date")
    count = (end - start).days + 1
    if count > MAX_BACKTEST_DAYS:
        raise HTTPException(400, f"Range too long: {count} days (max {MAX_BACKTEST_DAYS})")
    return run_days([start + timedelta(days=i) for i in range(count)], request.regions, with_actual=True)
