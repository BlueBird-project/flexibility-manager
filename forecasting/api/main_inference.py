from __future__ import annotations

import json
import os
from datetime import datetime
from pathlib import Path
from typing import Any, Literal

import pandas as pd
from fastapi import Body, FastAPI, HTTPException
from pydantic import BaseModel, Field, model_validator

from .fast_inference import ModelInference, load_data, run_backtest, run_production


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


class BatchRequest(InputPayload):
    start_date: str
    end_date: str
    horizon_span: int | None = Field(default=None, ge=1)


class ApiConfig(BaseModel):
    path_fast: str | None = None
    forges: list[str]
    translate: list[str]
    config_files: dict[str, list[dict[str, Any]]]
    default_horizon_span: int = Field(default=96, ge=1)


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


def build_pipeline(config: ApiConfig, horizon_span: int) -> ModelInference:
    return ModelInference(
        forges=config.forges,
        translate=config.translate,
        config_forges=config.config_files["forge"],
        config_translate=config.config_files["translate"],
        horizon=horizon_span,
    )


def format_forecast_response(forecast_df: pd.DataFrame) -> dict[str, list[Any]]:
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

    return {
        "ds": response_df["ds"].tolist(),
        "unique_id": response_df["unique_id"].tolist(),
        "yhat": response_df["yhat"].tolist(),
    }


def run_forecast(mode: Literal["day-ahead", "batch"], payload: InputPayload, start_date: str | None = None, end_date: str | None = None, horizon_span: int | None = None) -> dict[str, list[Any]]:
    config = load_api_config()
    selected_horizon = horizon_span or config.default_horizon_span
    path_fast = resolve_path_fast(config)

    data_df = load_input_dataframe(payload)
    pipeline = build_pipeline(config, selected_horizon)

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

    return format_forecast_response(forecast_df)


app = FastAPI(title="FAST Forecast API", version="1.0.0")


@app.get("/health")
def healthcheck() -> dict[str, str]:
    load_api_config()
    return {"status": "ok"}


@app.post("/forecast/day-ahead")
def forecast_day_ahead(
    request: DayAheadRequest = Body(..., examples=DAY_AHEAD_BODY_EXAMPLES)
) -> dict[str, list[Any]]:
    return run_forecast(mode="day-ahead", payload=request, horizon_span=request.horizon_span)


@app.post("/forecast/batch")
def forecast_batch(
    request: BatchRequest = Body(..., examples=BATCH_BODY_EXAMPLES)
) -> dict[str, list[Any]]:
    return run_forecast(
        mode="batch",
        payload=request,
        start_date=request.start_date,
        end_date=request.end_date,
        horizon_span=request.horizon_span,
    )
