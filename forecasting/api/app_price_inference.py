"""PriceFM+ inference API (FAST schema): Forge -> loaded Translate. Runs inside the Docker image.

  uvicorn app_price_inference:app --host 0.0.0.0 --port 8000

  POST /model/load        start loading the model in the background (downloads data/weights)
  GET  /model/status      not_loaded | loading | ready | failed
  POST /forecast/production  {"regions": ["BE", "ES"], "days": 2}
  POST /forecast/backtest    {"regions": ["BE"], "start_date": "20260201", "end_date": "20260202"}
  GET  /health

Loading and predicting have separate time budgets:
  * load runs in a background thread with no HTTP timeout; poll /model/status. With
    AUTO_LOAD_MODEL=1 (default) it starts when the server starts.
  * forecasts return 503 until the model is ready, and 504 after INFERENCE_TIMEOUT_SECONDS
    (the run itself cannot be killed: it finishes in the background and later requests wait for it).
Settings (environment): PRICEFM_WORKDIR (data/weights folder, mount it as a volume),
ENTSOE_CREDENTIALS_FILE (JSON with "token"), AUTO_LOAD_MODEL, INFERENCE_TIMEOUT_SECONDS,
MAX_PRODUCTION_DAYS, MAX_BACKTEST_DAYS, HISTORY_DAYS.
Responses: ds (UTC), region_name, yhat, p10, p90 in EUR/MWh; backtest adds "actual" (or null).
"""

import asyncio
import json
import os
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from contextlib import asynccontextmanager
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field

import db_forecast
import price_inference as core
from api_inference import observed_prices
from fast_inference import ClassBuilder

APP_DIR = Path(__file__).resolve().parent
WORKDIR = Path(os.environ.get("PRICEFM_WORKDIR", Path.home() / "PriceFM_Plus_forecast")).expanduser().resolve()
CREDENTIALS = Path(os.environ.get("ENTSOE_CREDENTIALS_FILE", APP_DIR / "credentials_entsoe.json"))
INFERENCE_TIMEOUT = float(os.environ.get("INFERENCE_TIMEOUT_SECONDS", 1800))
MAX_PRODUCTION_DAYS = int(os.environ.get("MAX_PRODUCTION_DAYS", 7))
MAX_BACKTEST_DAYS = int(os.environ.get("MAX_BACKTEST_DAYS", 62))
HISTORY_DAYS = int(os.environ.get("HISTORY_DAYS", 10))
AUTO_LOAD = os.environ.get("AUTO_LOAD_MODEL", "1").lower() in ("1", "true", "yes")

# Forge/Translate change the process working directory: a single worker serialises load and inference.
EXECUTOR = ThreadPoolExecutor(max_workers=1)
STATE_LOCK = threading.Lock()
STATE = {"status": "not_loaded", "error": None, "started_at": None, "finished_at": None, "translate": None}


def entsoe_key():
    if not CREDENTIALS.is_file():
        raise HTTPException(500, f"Missing ENTSO-E credentials file: {CREDENTIALS}")
    try:
        return json.loads(CREDENTIALS.read_text())["token"]
    except (KeyError, ValueError) as exc:
        raise HTTPException(500, f"{CREDENTIALS.name} must be JSON with a 'token' field") from exc


def _load_model():
    try:
        WORKDIR.mkdir(parents=True, exist_ok=True)
        translate = ClassBuilder(str(APP_DIR), core.TRANSLATES[0], "translate")
        translate.load(model_path=str(WORKDIR))
        with STATE_LOCK:
            STATE.update(status="ready", translate=translate, error=None, finished_at=time.time())
    except Exception as exc:
        with STATE_LOCK:
            STATE.update(status="failed", translate=None, error=repr(exc), finished_at=time.time())


def start_load(force=False):
    """Queue the model load; returns the status. No-op if already loading/ready (unless force)."""
    with STATE_LOCK:
        if STATE["status"] == "loading" or (STATE["status"] == "ready" and not force):
            return STATE["status"]
        STATE.update(status="loading", error=None, started_at=time.time(), finished_at=None)
    EXECUTOR.submit(_load_model)
    return "loading"


def status_payload():
    with STATE_LOCK:
        started, finished = STATE["started_at"], STATE["finished_at"]
        return {"status": STATE["status"], "error": STATE["error"],
                "seconds": round((finished or time.time()) - started, 1) if started else None}


@asynccontextmanager
async def lifespan(app):
    if AUTO_LOAD:
        start_load()
    yield


app = FastAPI(title="PriceFM+ Price Inference API (FAST)", version="1.0.0", lifespan=lifespan)


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


def clean_regions(regions):
    regions = list(dict.fromkeys(r.strip().upper() for r in regions))
    unknown = [r for r in regions if r not in db_forecast.REGIONS]
    if unknown:
        raise HTTPException(400, f"Unknown regions {unknown}; valid: {sorted(db_forecast.REGIONS)}")
    return regions


def forecast_day(translate, day, regions, key):
    """One FAST run: Forge on history and future frames, then the already loaded Translate."""
    config = {"workdir": str(WORKDIR), "forecast_date": day.isoformat(),
              "entsoe_api_key": key, "history_days": HISTORY_DAYS}
    history = core.seed_frame(regions, pd.Timestamp(day))
    future = pd.concat([pd.DataFrame({"ds": pd.date_range(history["ds"].max(), periods=core.HORIZON + 1, freq="15min"),
                                      "unique_id": r, "y": None}) for r in regions], ignore_index=True)
    forge = ClassBuilder(str(APP_DIR), core.FORGES[0], "forge")
    history = forge.process(history, config_process=config)
    future = forge.process(future, config_process=config)
    forecast = translate.predict(data=history, horizon=core.HORIZON, future_df=future)
    return forecast.rename(columns={"unique_id": "region_name"})[["ds", "region_name", "yhat", "p10", "p90"]]


def run_days(days, regions, key, with_actual):
    with STATE_LOCK:
        translate = STATE["translate"]
    frames, failed = [], []
    for day in days:
        try:
            frames.append(forecast_day(translate, day, regions, key))
        except Exception as exc:
            failed.append(f"{day}: {exc!r}")
    if not frames:
        return {"error": "No forecasts produced. " + "; ".join(failed)}
    result = pd.concat(frames, ignore_index=True)
    result["ds"] = pd.to_datetime(result["ds"])
    result = result.sort_values(["region_name", "ds"])
    response = {"ds": result["ds"].dt.strftime("%Y-%m-%d %H:%M:%S").tolist(),
                "region_name": result["region_name"].tolist(),
                "yhat": result["yhat"].tolist(), "p10": result["p10"].tolist(), "p90": result["p90"].tolist(),
                "failed_days": failed}
    if with_actual:
        prices = observed_prices(str(WORKDIR), regions, min(days), max(days))
        response["actual"] = [prices.get((r, t)) for r, t in zip(result["region_name"], result["ds"])]
    return response


async def predict(days, regions, with_actual=False):
    with STATE_LOCK:
        status = STATE["status"]
    if status != "ready":
        raise HTTPException(503, f"Model is {status}. POST /model/load and poll /model/status.")
    key = entsoe_key()
    loop = asyncio.get_running_loop()
    try:
        response = await asyncio.wait_for(
            loop.run_in_executor(EXECUTOR, run_days, days, regions, key, with_actual), INFERENCE_TIMEOUT)
    except asyncio.TimeoutError:
        raise HTTPException(504, f"Inference exceeded {INFERENCE_TIMEOUT:.0f}s; it keeps running in the "
                                 "background and the next request waits for it")
    if "error" in response:
        raise HTTPException(500, response["error"])
    return response


@app.get("/health")
def health():
    return {"status": "ok", "model": status_payload()["status"]}


@app.post("/model/load", status_code=202)
def load_model(force: bool = False):
    start_load(force)
    return status_payload()


@app.get("/model/status")
def model_status():
    return status_payload()


@app.post("/forecast/production")
async def production(request: ProductionRequest):
    if request.days > MAX_PRODUCTION_DAYS:
        raise HTTPException(400, f"days must be at most {MAX_PRODUCTION_DAYS}")
    first = datetime.now(timezone.utc).date() + timedelta(days=1)
    return await predict([first + timedelta(days=i) for i in range(request.days)], clean_regions(request.regions))


@app.post("/forecast/backtest")
async def backtest(request: BacktestRequest):
    start, end = parse_day(request.start_date), parse_day(request.end_date)
    if end < start:
        raise HTTPException(400, "end_date is before start_date")
    count = (end - start).days + 1
    if count > MAX_BACKTEST_DAYS:
        raise HTTPException(400, f"Range too long: {count} days (max {MAX_BACKTEST_DAYS})")
    return await predict([start + timedelta(days=i) for i in range(count)],
                         clean_regions(request.regions), with_actual=True)
