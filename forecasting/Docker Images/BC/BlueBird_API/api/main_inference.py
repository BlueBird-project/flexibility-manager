from __future__ import annotations

import importlib.util
import json
import os
import threading
import time
from contextlib import asynccontextmanager
from datetime import datetime
from pathlib import Path
from typing import Any, Literal

import pandas as pd
from fastapi import Body, FastAPI, HTTPException, Response
from pydantic import BaseModel, Field, model_validator

from .fast_inference import ClassBuilder, ModelInference, load_data, run_backtest, run_production


APP_DIR = Path(__file__).resolve().parent
CONFIG_PATH = APP_DIR / "api_config.json"
CONFIG_ENV_VAR = "FAST_API_CONFIG"
REQUIRED_COLUMNS = {"ds", "unique_id", "y"}


DAY_AHEAD_BODY_EXAMPLES = {
    "inline_data": {
        "summary": "Day-ahead with inline data dictionary",
        "description": "Provide historical points directly in the request body.",
        "value": {
            "horizon_span": 4,
            "data": {
                "ds": [
                    "2026-10-20 00:00:00",
                    "2026-10-20 00:15:00",
                    "2026-10-20 00:30:00",
                    "2026-10-20 00:45:00",
                ],
                "unique_id": ["asset_1", "asset_1", "asset_1", "asset_1"],
                "y": [100.0, 101.5, 99.8, 102.1],
            },
        },
    },
    "csv_input": {
        "summary": "Day-ahead with CSV file path",
        "description": "Provide a path to a CSV/feather/xlsx/xls file visible to the API process.",
        "value": {
            "horizon_span": 96,
            "csv_file": "./my_input_name.csv",
        },
    },
}


BATCH_BODY_EXAMPLES = {
    "inline_data": {
        "summary": "Batch with inline data dictionary",
        "description": "Simulate repeated day-ahead runs between start_date and end_date.",
        "value": {
            "start_date": "2026-10-01",
            "end_date": "2026-10-07",
            "horizon_span": 4,
            "data": {
                "ds": [
                    "2026-10-20 00:00:00",
                    "2026-10-20 00:15:00",
                    "2026-10-20 00:30:00",
                    "2026-10-20 00:45:00",
                ],
                "unique_id": ["asset_1", "asset_1", "asset_1", "asset_1"],
                "y": [100.0, 101.5, 99.8, 102.1],
            },
        },
    },
    "csv_input": {
        "summary": "Batch with CSV file path",
        "description": "Use a file path and run historical simulation over a date range.",
        "value": {
            "start_date": "2026-10-01",
            "end_date": "2026-10-07",
            "horizon_span": 96,
            "csv_file": "./my_input_name.csv",
        },
    },
}


class InputPayload(BaseModel):
    data: dict[str, list[Any]] | None = None
    csv_file: str | None = None

    @model_validator(mode="after")
    def validate_input_source(self) -> "InputPayload":
        has_data = self.data is not None
        has_csv = self.csv_file is not None
        if has_data == has_csv:
            raise ValueError("Provide exactly one input source: either 'data' or 'csv_file'.")
        return self


class DayAheadRequest(InputPayload):
    horizon_span: int | None = Field(default=None, ge=1)
    model: str | None = Field(default=None, description="chronos2 | timesfm3 | timestral (default: first configured)")


class BatchRequest(InputPayload):
    start_date: str
    end_date: str
    horizon_span: int | None = Field(default=None, ge=1)
    model: str | None = Field(default=None, description="chronos2 | timesfm3 | timestral (default: first configured)")


class ApiConfig(BaseModel):
    path_fast: str | None = None
    forges: list[str]
    translate: list[str]
    config_files: dict[str, list[dict[str, Any]]]
    default_horizon_span: int = Field(default=96, ge=1)
    default_model: str | None = None
    preload: list[str] = Field(default_factory=list, description="models loaded in the background at startup")
    interval_quantiles: list[float] = Field(default_factory=lambda: [0.1, 0.9], description="[lower, upper] quantiles -> yhat_lower/yhat_upper")
    clip_lower: float | None = Field(default=None, description="optional floor for yhat/yhat_lower/yhat_upper (e.g. 0 for PV/energy)")


def resolve_config_path() -> Path:
    custom_config = os.getenv(CONFIG_ENV_VAR)
    if not custom_config:
        return CONFIG_PATH

    candidate = Path(custom_config)
    if not candidate.is_absolute():
        candidate = (APP_DIR / candidate).resolve()

    return candidate


def load_api_config() -> ApiConfig:
    config_path = resolve_config_path()

    if not config_path.exists():
        raise RuntimeError(
            f"Missing API config file: {config_path}. "
            f"Set {CONFIG_ENV_VAR} to an existing JSON file or create api_config.json."
        )

    with config_path.open("r", encoding="utf-8") as config_file:
        raw_config = json.load(config_file)

    config = ApiConfig.model_validate(raw_config)

    if "forge" not in config.config_files or "translate" not in config.config_files:
        raise RuntimeError("api_config.json must define config_files.forge and config_files.translate.")

    return config


def resolve_path_fast(config: ApiConfig) -> str:
    if config.path_fast is None:
        return str(APP_DIR.parent / "FAST")

    candidate = Path(config.path_fast)

    if not candidate.is_absolute():
        candidate = (APP_DIR / candidate).resolve()

    return str(candidate)


def load_input_dataframe(payload: InputPayload) -> pd.DataFrame:
    if payload.csv_file is not None:
        csv_path = Path(payload.csv_file)
        if not csv_path.is_absolute():
            csv_path = (APP_DIR / csv_path).resolve()
        if not csv_path.exists():
            raise HTTPException(status_code=400, detail=f"csv_file does not exist: {csv_path}")
        df = load_data(filename=str(csv_path))
    else:
        assert payload.data is not None
        try:
            df = pd.DataFrame(payload.data)
        except Exception as exc:
            raise HTTPException(status_code=400, detail=f"Invalid data payload: {exc}") from exc

        missing = REQUIRED_COLUMNS.difference(df.columns)
        if missing:
            missing_cols = ", ".join(sorted(missing))
            raise HTTPException(status_code=400, detail=f"Missing required columns: {missing_cols}")

        try:
            df["ds"] = pd.to_datetime(df["ds"])
        except Exception as exc:
            raise HTTPException(status_code=400, detail=f"Column 'ds' cannot be parsed as datetime: {exc}") from exc

        df = df.sort_values(["unique_id", "ds"])

    missing = REQUIRED_COLUMNS.difference(df.columns)
    if missing:
        missing_cols = ", ".join(sorted(missing))
        raise HTTPException(status_code=400, detail=f"Missing required columns after load: {missing_cols}")

    return df


# translate file name -> public model key, package that must be importable, covariate support
MODEL_INFO = {
    "chronos_2": {"key": "chronos2", "package": "chronos", "covariates": True, "default_path": "amazon/chronos-2"},
    "timesfm_3": {"key": "timesfm3", "package": "timesfm3", "covariates": "optional (predict_args.covariate_cols)",
                  "default_path": "google/timesfm-3.0-pytorch"},
    "timestral": {"key": "timestral", "package": "timestral", "covariates": "optional (predict_args.covariate_cols)",
                  "default_path": "mistralai/Timestral-209M"},
}
_INSTANCES: dict[str, Any] = {}
_LOAD_LOCKS: dict[str, threading.Lock] = {}
_PREDICT_LOCKS: dict[str, threading.Lock] = {}
_REGISTRY_LOCK = threading.Lock()


def configured_models(config: ApiConfig) -> dict[str, dict[str, Any]]:
    """{model key: {translate, config}} from the translate list zipped with config_files.translate."""
    models: dict[str, dict[str, Any]] = {}
    for i, name in enumerate(config.translate):
        cfg = dict(config.config_files["translate"][i]) if i < len(config.config_files["translate"]) else {}
        key = MODEL_INFO.get(name, {}).get("key", name)
        models[key] = {"translate": name, "config": cfg}
    return models


def select_model(config: ApiConfig, requested: str | None) -> str:
    models = configured_models(config)
    key = requested or config.default_model or next(iter(models))
    if key not in models:
        raise HTTPException(status_code=400, detail=f"Unknown model '{key}'. Configured models: {sorted(models)}")
    return key


def weights_present(model_path: str | None) -> bool | None:
    if not model_path:
        return None
    if Path(model_path).exists():
        return True
    try:
        from huggingface_hub import try_to_load_from_cache
        return isinstance(try_to_load_from_cache(model_path, "config.json"), str)
    except Exception:
        return None  # unknown (huggingface_hub missing)


_STATE: dict[str, dict[str, Any]] = {}


def _set_state(key: str, **fields) -> None:
    with _REGISTRY_LOCK:
        _STATE.setdefault(key, {"status": "not_loaded", "error": None, "started_at": None, "finished_at": None,
                                "load_seconds": None, "device": None}).update(fields)


def get_instance(key: str, config: ApiConfig, path_fast: str):
    """Loads each model once (lazily, thread-safe) and reuses it for every request."""
    with _REGISTRY_LOCK:
        lock = _LOAD_LOCKS.setdefault(key, threading.Lock())
        _PREDICT_LOCKS.setdefault(key, threading.Lock())
    if key in _INSTANCES:
        return _INSTANCES[key]
    entry = configured_models(config)[key]
    with lock:
        if key not in _INSTANCES:
            info = MODEL_INFO.get(entry["translate"], {})
            cfg = entry["config"]
            started = time.time()
            _set_state(key, status="loading", error=None, started_at=started, finished_at=None, load_seconds=None,
                       device=cfg.get("load_args", {}).get("device") or "library default")
            try:
                if info.get("package") and importlib.util.find_spec(info["package"]) is None:
                    raise HTTPException(status_code=503, detail=f"Model '{key}' needs the Python package '{info['package']}' (see README/description.md).")
                try:
                    instance = ClassBuilder(path_fast, entry["translate"], "translate")
                    instance.load(model_path=cfg.get("model_path") or info.get("default_path"), **cfg.get("load_args", {}))
                except Exception as exc:
                    raise HTTPException(status_code=503, detail=f"Could not load model '{key}': {type(exc).__name__}: {exc}") from exc
            except HTTPException as exc:
                _set_state(key, status="failed", error=str(exc.detail), finished_at=time.time())
                raise
            _INSTANCES[key] = instance
            done = time.time()
            _set_state(key, status="ready", error=None, finished_at=done, load_seconds=round(done - started, 2))
    return _INSTANCES[key]


def start_load(key: str, config: ApiConfig, path_fast: str, force: bool = False) -> None:
    """Loads a model in a background thread (poll GET /status). No-op if already ready/loading unless force."""
    with _REGISTRY_LOCK:
        if _STATE.get(key, {}).get("status") == "loading" or (key in _INSTANCES and not force):
            return
        if force:
            _INSTANCES.pop(key, None)
        _STATE.setdefault(key, {}).update(status="loading", error=None, started_at=time.time(), finished_at=None, load_seconds=None)

    def _bg():
        try:
            get_instance(key, config, path_fast)
        except Exception:
            pass  # the failure is recorded in the state

    threading.Thread(target=_bg, name=f"load-{key}", daemon=True).start()


def model_status(key: str, config: ApiConfig) -> dict[str, Any]:
    entry = configured_models(config)[key]
    info = MODEL_INFO.get(entry["translate"], {})
    path = entry["config"].get("model_path") or info.get("default_path")
    with _REGISTRY_LOCK:
        state = dict(_STATE.get(key, {}))
    status = "ready" if key in _INSTANCES else state.get("status", "not_loaded")
    if status == "ready" and key not in _INSTANCES:
        status = "not_loaded"
    return {
        "model": key,
        "status": status,  # not_loaded | loading | ready | failed
        "ready": key in _INSTANCES,
        "error": state.get("error"),
        "load_seconds": state.get("load_seconds") if status != "loading" else round(time.time() - state["started_at"], 1),
        "device": state.get("device") or entry["config"].get("load_args", {}).get("device") or "library default",
        "package_installed": importlib.util.find_spec(info["package"]) is not None if info.get("package") else None,
        "weights_cached": weights_present(path),
    }


def build_pipeline(config: ApiConfig, horizon_span: int, model: str, path_fast: str) -> ModelInference:
    entry = configured_models(config)[model]
    instance = get_instance(model, config, path_fast)
    # covariates are only forwarded when the model supports them; otherwise the call is univariate
    return ModelInference(
        forges=config.forges,
        translate=[entry["translate"]],
        config_forges=config.config_files["forge"],
        config_translate=[{"predict_args": {"quantiles": config.interval_quantiles, **entry["config"].get("predict_args", {})}}],
        horizon=horizon_span,
        translate_instances={entry["translate"]: instance},
    )


def format_forecast_response(forecast_df: pd.DataFrame, model: str | None = None, clip_lower: float | None = None) -> dict[str, Any]:
    required_output = ["ds", "unique_id", "yhat"]
    missing_output = [col for col in required_output if col not in forecast_df.columns]
    if missing_output:
        missing_cols = ", ".join(missing_output)
        raise HTTPException(
            status_code=500,
            detail=f"Forecast output is missing expected columns: {missing_cols}",
        )

    response_df = forecast_df[required_output].copy()
    response_df["ds"] = pd.to_datetime(response_df["ds"]).dt.strftime("%Y-%m-%d %H:%M:%S")
    # lower/upper are extra fields: None when the model gives no quantiles (e.g. Timestral)
    for col in ("yhat_lower", "yhat_upper"):
        response_df[col] = pd.to_numeric(forecast_df[col], errors="coerce") if col in forecast_df.columns else float("nan")
    if clip_lower is not None:
        response_df[["yhat", "yhat_lower", "yhat_upper"]] = response_df[["yhat", "yhat_lower", "yhat_upper"]].clip(lower=clip_lower)

    def values(col):
        return [None if pd.isna(v) else float(v) for v in response_df[col]]

    return {
        "ds": response_df["ds"].tolist(),
        "unique_id": response_df["unique_id"].tolist(),
        "yhat": response_df["yhat"].tolist(),
        "yhat_lower": values("yhat_lower"),
        "yhat_upper": values("yhat_upper"),
        "model": model,
    }


def run_forecast(mode: Literal["day-ahead", "batch"], payload: InputPayload, start_date: str | None = None, end_date: str | None = None, horizon_span: int | None = None, model: str | None = None) -> dict[str, Any]:
    config = load_api_config()
    selected_horizon = horizon_span or config.default_horizon_span
    path_fast = resolve_path_fast(config)

    data_df = load_input_dataframe(payload)
    model_key = select_model(config, model)
    pipeline = build_pipeline(config, selected_horizon, model_key, path_fast)

    with _PREDICT_LOCKS[model_key]:
        return _run(mode, pipeline, data_df, path_fast, start_date, end_date, model_key, config.clip_lower)


def _run(mode, pipeline, data_df, path_fast, start_date, end_date, model_key, clip_lower=None):
    if mode == "day-ahead":
        forecast_df = run_production(pipeline=pipeline, data=data_df, path_fast=path_fast)
    else:
        if start_date is None or end_date is None:
            raise HTTPException(status_code=400, detail="start_date and end_date are required for batch mode.")

        try:
            datetime.strptime(start_date, "%Y-%m-%d")
            datetime.strptime(end_date, "%Y-%m-%d")
        except ValueError as exc:
            raise HTTPException(status_code=400, detail="start_date and end_date must use YYYY-MM-DD format.") from exc

        forecast_df = run_backtest(
            pipeline=pipeline,
            data=data_df,
            path_fast=path_fast,
            start_date=start_date,
            end_date=end_date,
        )

    return format_forecast_response(forecast_df, model_key, clip_lower)


@asynccontextmanager
async def lifespan(_app):
    config = load_api_config()
    for key in config.preload:
        if key in configured_models(config):
            start_load(key, config, resolve_path_fast(config))
    yield


app = FastAPI(title="FAST Forecast API", version="1.2.0", lifespan=lifespan)


class LoadModelRequest(BaseModel):
    model: str | None = Field(default=None, description="chronos2 | timesfm3 | timestral (default: the configured default)")
    wait: bool = Field(default=False, description="true: block until loaded (200); false: load in the background (202) and poll GET /status")
    force: bool = Field(default=False, description="reload even if already loaded")


@app.get("/health")
def healthcheck() -> dict[str, str]:
    load_api_config()
    return {"status": "ok"}


@app.get("/models")
def list_models() -> dict[str, Any]:
    config = load_api_config()
    default = select_model(config, None)
    out = []
    for key, entry in configured_models(config).items():
        info = MODEL_INFO.get(entry["translate"], {})
        path = entry["config"].get("model_path") or info.get("default_path")
        out.append({
            "model": key,
            "translate": entry["translate"],
            "model_path": path,
            "default": key == default,
            "package": info.get("package"),
            "package_installed": importlib.util.find_spec(info["package"]) is not None if info.get("package") else None,
            "weights_cached": weights_present(path),
            "loaded": key in _INSTANCES,
            "covariates": info.get("covariates", "unknown"),
        })
    return {"default_model": default, "models": out}


@app.post("/load_model")
def load_model(response: Response, request: LoadModelRequest | None = None) -> dict[str, Any]:
    """Loads a model into the cache so the first forecast is fast. Poll GET /status while it loads."""
    request = request or LoadModelRequest()
    config = load_api_config()
    key = select_model(config, request.model)
    path_fast = resolve_path_fast(config)
    if request.wait:
        if request.force:
            _INSTANCES.pop(key, None)
        get_instance(key, config, path_fast)
    else:
        start_load(key, config, path_fast, request.force)
        response.status_code = 202
    return model_status(key, config)


@app.get("/status")
def status() -> dict[str, Any]:
    config = load_api_config()
    default = select_model(config, None)
    models = [model_status(key, config) for key in configured_models(config)]
    return {"default_model": default, "ready": any(m["ready"] for m in models if m["model"] == default), "models": models}


@app.post("/forecast/day-ahead")
def forecast_day_ahead(
    request: DayAheadRequest = Body(..., examples=DAY_AHEAD_BODY_EXAMPLES)
) -> dict[str, Any]:
    return run_forecast(mode="day-ahead", payload=request, horizon_span=request.horizon_span, model=request.model)


@app.post("/forecast/batch")
def forecast_batch(
    request: BatchRequest = Body(..., examples=BATCH_BODY_EXAMPLES)
) -> dict[str, Any]:
    return run_forecast(
        model=request.model,
        mode="batch",
        payload=request,
        start_date=request.start_date,
        end_date=request.end_date,
        horizon_span=request.horizon_span,
    )
