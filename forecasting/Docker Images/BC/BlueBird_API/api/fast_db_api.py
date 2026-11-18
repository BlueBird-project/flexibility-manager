"""FAST DB API: a two-way street between a SQLite3 database and the FAST forecasters.

  uvicorn fast_db_api:app --port 8001          (from this folder)   or   uvicorn api.fast_db_api:app (from the parent)

  POST /load_model   load a model (chronos2 | timesfm3 | timestral) into memory so /forecast is fast
  GET  /status       model readiness (not_loaded | loading | ready | failed), load time, device, DB state
  POST /push_data    write energy_values / weather / price (and assets, price tags, variable names) into the DB
  POST /forecast     read the latest values from the DB, forecast with the loaded model, store them in `forecasts`
  GET  /forecasts    latest stored forecast per asset
  GET  /health

The SQLite file is created from db_forecast_schema.sql (used exactly as given) when missing. Settings live in
fast_db_api_config.json (override with FAST_DB_API_CONFIG); the DB path can be overridden with FAST_DB_PATH.
Model loading/caching reuses main_inference (the Chronos-2 API), so both APIs share the same machinery.
"""
from __future__ import annotations

import json
import math
import os
import sqlite3
import sys
import threading
import time
from contextlib import asynccontextmanager, contextmanager
from datetime import datetime, timezone
from pathlib import Path
from statistics import NormalDist
from typing import Any

import numpy as np
import pandas as pd
from fastapi import FastAPI, HTTPException, Query, Response
from pydantic import BaseModel, Field

try:
    from . import main_inference as core
except ImportError:  # run as `uvicorn fast_db_api:app` from this folder
    sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
    from api import main_inference as core

APP_DIR = Path(__file__).resolve().parent
CONFIG_PATH = APP_DIR / "fast_db_api_config.json"
CONFIG_ENV_VAR = "FAST_DB_API_CONFIG"
DB_ENV_VAR = "FAST_DB_PATH"
TABLES = ["weather", "forecasts", "forecast_var_names", "energy_values", "assets", "price", "price_tag"]
TS_FORMAT = "%Y-%m-%d %H:%M:%S"


# ==========================================================
# CONFIG
# ==========================================================

class UseCaseConfig(BaseModel):
    var_name: str = "forecast"
    clip_lower: float | None = Field(default=None, description="floor for yhat/lower/upper (e.g. 0 for PV or consumption); null = no clipping")
    weather_vars: list[str] = Field(default_factory=list, description="empty = every variable found in the weather table")
    forges: list[dict[str, Any]] = Field(default_factory=list, description="[{name, config}] FAST forge steps (anomaly handling, interpolation)")


class DbApiConfig(BaseModel):
    path_fast: str | None = None
    db_path: str = "fast_db.sqlite"
    schema_file: str = "db_forecast_schema.sql"
    models: list[dict[str, Any]]
    default_model: str | None = None
    preload: list[str] = Field(default_factory=list)
    horizon: int = Field(default=96, ge=1)
    freq: str = "15min"
    history_days: float = Field(default=10, gt=0)
    min_history_points: int = Field(default=96, ge=2)
    interval_quantiles: list[float] = Field(default_factory=lambda: [0.1, 0.9])
    interval_fallback: str = "seasonal_residual"  # seasonal_residual | none (when the model gives no quantiles)
    use_weather: bool = Field(default=False, description="read weather covariates from the `weather` table (off by default; that table cannot be fed when push_tables excludes it)")
    push_tables: list[str] = Field(default_factory=lambda: ["assets", "price_tags", "forecast_var_names", "energy_values", "weather", "price"],
                                   description="payload sections /push_data accepts; the DB image restricts this to [\"energy_values\"]")
    require_data: bool = Field(default=True, description="/forecast answers 409 'empty database' when energy_values has no rows")
    weather_models: list[str] = Field(default_factory=lambda: ["chronos2"])
    weather_ffill_steps: int = 4
    auto_create_assets: bool = False
    default_asset_type: str = "generic"
    default_use_case: str = "default"
    data_timezone: str = "UTC"
    forecast_timestamp_tz: str = "UTC"
    price_ds_format: str = "text"  # text | epoch (price.ds is INTEGER in the schema)
    sqlite_timeout_seconds: float = 30
    max_rows_per_request: int = 200000
    stale_after_hours: float = 48
    locations: dict[str, dict[str, Any]] = Field(default_factory=dict, description="location_id or asset name -> {lat, lon, tz} for night_zero")
    use_cases: dict[str, UseCaseConfig] = Field(default_factory=lambda: {"default": UseCaseConfig()})


def load_config() -> DbApiConfig:
    path = Path(os.environ.get(CONFIG_ENV_VAR, CONFIG_PATH))
    if not path.is_absolute():
        path = APP_DIR / path
    try:
        cfg = DbApiConfig(**json.loads(path.read_text(encoding="utf-8")))
    except Exception as exc:
        raise HTTPException(status_code=500, detail=f"Invalid DB API config {path.name}: {exc}") from exc
    if cfg.default_use_case not in cfg.use_cases:
        raise HTTPException(status_code=500, detail=f"default_use_case '{cfg.default_use_case}' is not defined in use_cases")
    if len(cfg.interval_quantiles) != 2 or not all(0 < q < 1 for q in cfg.interval_quantiles):
        raise HTTPException(status_code=500, detail="interval_quantiles must be [lower, upper] within (0, 1)")
    return cfg


def core_config(cfg: DbApiConfig) -> core.ApiConfig:
    """Same model registry as the Chronos-2 API (translate list + per-model config)."""
    return core.ApiConfig(
        path_fast=cfg.path_fast, forges=[], translate=[m["translate"] for m in cfg.models],
        config_files={"forge": [], "translate": [{k: v for k, v in m.items() if k != "translate"} for m in cfg.models]},
        default_model=cfg.default_model, interval_quantiles=cfg.interval_quantiles,
    )


# ==========================================================
# SQLITE
# ==========================================================

_DB_READY: set[str] = set()
_DB_LOCK = threading.Lock()


def db_file(cfg: DbApiConfig) -> Path:
    path = Path(os.environ.get(DB_ENV_VAR, cfg.db_path))
    return path if path.is_absolute() else (APP_DIR / path)


def ensure_db(cfg: DbApiConfig) -> None:
    """Creates the file/tables from the schema file if missing (the DDL is IF NOT EXISTS, so it is safe to re-run)."""
    path = db_file(cfg)
    with _DB_LOCK:
        if str(path) in _DB_READY:
            return
        schema = Path(cfg.schema_file)
        schema = schema if schema.is_absolute() else APP_DIR / schema
        path.parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(path, timeout=cfg.sqlite_timeout_seconds, isolation_level=None)
        try:
            conn.execute("PRAGMA journal_mode=WAL")
            existing = {r[0] for r in conn.execute("SELECT name FROM sqlite_master WHERE type='table'")}
            if not set(TABLES) <= existing:
                conn.executescript(schema.read_text(encoding="utf-8"))
        finally:
            conn.close()
        _DB_READY.add(str(path))


@contextmanager
def db(cfg: DbApiConfig, write: bool = False):
    """Per-request connection (WAL, busy timeout, FK on). Write requests run in one IMMEDIATE transaction."""
    ensure_db(cfg)
    conn = sqlite3.connect(db_file(cfg), timeout=cfg.sqlite_timeout_seconds, isolation_level=None)
    conn.row_factory = sqlite3.Row
    conn.execute(f"PRAGMA busy_timeout={int(cfg.sqlite_timeout_seconds * 1000)}")
    conn.execute("PRAGMA foreign_keys=ON")
    try:
        if write:
            conn.execute("BEGIN IMMEDIATE")
        yield conn
        if write:
            conn.execute("COMMIT")
    except Exception:
        if conn.in_transaction:
            conn.execute("ROLLBACK")
        raise
    finally:
        conn.close()


def norm_ds(value: Any, cfg: DbApiConfig) -> str:
    """Timestamps are stored as 'YYYY-MM-DD HH:MM:SS' naive local time; tz-aware input is converted to data_timezone."""
    if value is None or isinstance(value, bool):
        raise ValueError("missing ds")
    ts = pd.Timestamp(value)
    if pd.isna(ts):
        raise ValueError("invalid ds")
    if ts.tzinfo is not None:
        ts = ts.tz_convert(cfg.data_timezone).tz_localize(None)
    return ts.strftime(TS_FORMAT)


def finite(value: Any, what: str = "value") -> float:
    if value is None or isinstance(value, bool):
        raise ValueError(f"{what} is missing")
    number = float(value)
    if not math.isfinite(number):
        raise ValueError(f"{what} is not finite")
    return number


def chunks(items: list, size: int = 400):
    for i in range(0, len(items), size):
        yield items[i:i + size]


# ==========================================================
# PUSH DATA
# ==========================================================

class PushRequest(BaseModel):
    create_missing_assets: bool | None = Field(default=None, description="create unknown assets / price tags by name (default: config auto_create_assets)")
    default_asset_type: str | None = Field(default=None, description="type for auto-created assets (free text; maps to a use_cases key, else default_use_case)")
    price_tags: list[dict[str, Any]] = Field(default_factory=list, description="[{country, type}] get-or-create")
    assets: list[dict[str, Any]] = Field(default_factory=list, description="[{name, type, location_id, price_tag_id | price_tag:{country,type}, asset_id?}] upsert by asset_id or name")
    forecast_var_names: list[dict[str, Any]] = Field(default_factory=list, description="[{var_name, type}] get-or-create")
    energy_values: list[dict[str, Any]] = Field(default_factory=list, description="[{ds, asset | asset_id, value}] upsert on (ds, asset_id)")
    weather: list[dict[str, Any]] = Field(default_factory=list, description="[{ds, asset | asset_id, weather_var, value, forecast}]")
    price: list[dict[str, Any]] = Field(default_factory=list, description="[{ds, price_tag_id | price_tag:{country,type}, price}]")


class Report:
    def __init__(self):
        self.tables: dict[str, dict[str, int]] = {}
        self.examples: list[dict[str, Any]] = []

    def count(self, table: str, field: str, n: int = 1):
        self.tables.setdefault(table, {"inserted": 0, "updated": 0, "rejected": 0})[field] += n

    def reject(self, table: str, row: Any, reason: str):
        self.count(table, "rejected")
        if len(self.examples) < 20:
            self.examples.append({"table": table, "row": row, "reason": reason})


class Resolver:
    """Resolves assets / price tags by id or name inside the push transaction, creating them only when allowed."""

    def __init__(self, conn, create: bool, asset_type: str, report: Report):
        self.conn, self.create, self.asset_type, self.report = conn, create, asset_type, report
        self.assets_by_id = {r["asset_id"]: r["name"] for r in conn.execute("SELECT asset_id, name FROM assets")}
        self.assets_by_name = {v: k for k, v in self.assets_by_id.items() if v is not None}

    def asset(self, row: dict) -> int:
        asset_id, name = row.get("asset_id"), row.get("asset")
        if asset_id is not None:
            if int(asset_id) not in self.assets_by_id:
                if not self.create:
                    raise ValueError(f"unknown asset_id {asset_id} (set create_missing_assets to create it)")
                self.conn.execute("INSERT INTO assets(asset_id, name, type) VALUES (?, ?, ?)", (int(asset_id), name, self.asset_type))
                self.assets_by_id[int(asset_id)] = name
                if name is not None:
                    self.assets_by_name[str(name)] = int(asset_id)
                self.report.count("assets", "inserted")
            return int(asset_id)
        if not name:
            raise ValueError("asset or asset_id is required")
        name = str(name)
        if name not in self.assets_by_name:
            if not self.create:
                raise ValueError(f"unknown asset '{name}' (set create_missing_assets to create it)")
            cur = self.conn.execute("INSERT INTO assets(name, type) VALUES (?, ?)", (name, self.asset_type))
            self.assets_by_id[cur.lastrowid], self.assets_by_name[name] = name, cur.lastrowid
            self.report.count("assets", "inserted")
        return self.assets_by_name[name]

    def price_tag(self, ref: dict | None, tag_id: Any = None, force_create: bool = False) -> int:
        if tag_id is not None:
            if self.conn.execute("SELECT 1 FROM price_tag WHERE id=?", (int(tag_id),)).fetchone() is None:
                raise ValueError(f"unknown price_tag_id {tag_id}")
            return int(tag_id)
        if not ref or not ref.get("country") or not ref.get("type"):
            raise ValueError("price_tag_id or price_tag {country, type} is required")
        found = self.conn.execute("SELECT id FROM price_tag WHERE country=? AND type=?", (ref["country"], ref["type"])).fetchone()
        if found:
            return found["id"]
        if not (self.create or force_create):
            raise ValueError(f"unknown price tag {ref} (set create_missing_assets to create it)")
        self.report.count("price_tag", "inserted")
        return self.conn.execute("INSERT INTO price_tag(country, type) VALUES (?, ?)", (ref["country"], ref["type"])).lastrowid


def push_catalog(conn, req: PushRequest, res: Resolver, rep: Report):
    for row in req.price_tags:
        try:
            ref = {"country": row.get("country"), "type": row.get("type")}
            known = conn.execute("SELECT 1 FROM price_tag WHERE country=? AND type=?", (ref["country"], ref["type"])).fetchone()
            res.price_tag(ref, force_create=True)  # listing a tag in `price_tags` is an explicit creation request
            if known:
                rep.count("price_tag", "updated", 0)
        except ValueError as exc:
            rep.reject("price_tag", row, str(exc))
    for row in req.forecast_var_names:
        name = row.get("var_name")
        if not name:
            rep.reject("forecast_var_names", row, "var_name is required")
        elif conn.execute("SELECT 1 FROM forecast_var_names WHERE var_name=?", (name,)).fetchone() is None:
            conn.execute("INSERT INTO forecast_var_names(var_name, type) VALUES (?, ?)", (name, row.get("type")))
            rep.count("forecast_var_names", "inserted")
    for row in req.assets:
        try:
            name = row.get("name")
            if not name and row.get("asset_id") is None:
                raise ValueError("name or asset_id is required")
            tag = row.get("price_tag_id")
            if tag is not None or row.get("price_tag"):
                tag = res.price_tag(row.get("price_tag"), tag, force_create=True)
            existing = row.get("asset_id") if row.get("asset_id") is not None else res.assets_by_name.get(name)
            fields = {k: row[k] for k in ("name", "location_id", "type") if k in row}
            if tag is not None:
                fields["price_tag_id"] = tag
            if existing is not None and int(existing) in res.assets_by_id:
                if fields:
                    conn.execute(f"UPDATE assets SET {', '.join(f'{k}=?' for k in fields)} WHERE asset_id=?", [*fields.values(), int(existing)])
                if "name" in fields:
                    res.assets_by_name.pop(res.assets_by_id[int(existing)], None)
                    res.assets_by_id[int(existing)] = fields["name"]
                    res.assets_by_name[fields["name"]] = int(existing)
                rep.count("assets", "updated")
            else:
                fields.setdefault("type", res.asset_type)
                if row.get("asset_id") is not None:
                    fields["asset_id"] = int(row["asset_id"])
                cur = conn.execute(f"INSERT INTO assets({', '.join(fields)}) VALUES ({', '.join('?' * len(fields))})", list(fields.values()))
                res.assets_by_id[cur.lastrowid], res.assets_by_name[name] = name, cur.lastrowid
                rep.count("assets", "inserted")
        except (ValueError, TypeError, sqlite3.Error) as exc:
            rep.reject("assets", row, str(exc))


def push_energy(conn, rows: list[dict], res: Resolver, rep: Report, cfg: DbApiConfig):
    valid: dict[tuple[str, int], float] = {}
    for row in rows:
        try:
            key = (norm_ds(row.get("ds"), cfg), res.asset(row))
            valid[key] = finite(row.get("value"))
        except (ValueError, TypeError) as exc:
            rep.reject("energy_values", row, str(exc))
    existing: set[tuple[str, int]] = set()
    by_asset: dict[int, list[str]] = {}
    for ds, aid in valid:
        by_asset.setdefault(aid, []).append(ds)
    for aid, stamps in by_asset.items():
        for part in chunks(stamps):
            q = f"SELECT ds FROM energy_values WHERE asset_id=? AND ds IN ({','.join('?' * len(part))})"
            existing.update((r["ds"], aid) for r in conn.execute(q, [aid, *part]))
    conn.executemany(
        "INSERT INTO energy_values(ds, asset_id, value) VALUES (?, ?, ?) ON CONFLICT(ds, asset_id) DO UPDATE SET value=excluded.value",
        [(ds, aid, v) for (ds, aid), v in valid.items()])
    rep.count("energy_values", "updated", len(existing))
    rep.count("energy_values", "inserted", len(valid) - len(existing))


def push_weather(conn, rows: list[dict], res: Resolver, rep: Report, cfg: DbApiConfig):
    valid: dict[tuple, float] = {}
    for row in rows:
        try:
            var = row.get("weather_var")
            if not var or not isinstance(var, str):
                raise ValueError("weather_var is required")
            valid[(norm_ds(row.get("ds"), cfg), res.asset(row), var, int(bool(row.get("forecast", False))))] = finite(row.get("value"))
        except (ValueError, TypeError) as exc:
            rep.reject("weather", row, str(exc))
    # the schema has no UNIQUE key on weather: emulate an upsert on (ds, asset_id, weather_var, forecast)
    existing: dict[tuple, int] = {}
    groups: dict[tuple, list[str]] = {}
    for ds, aid, var, fc in valid:
        groups.setdefault((aid, var, fc), []).append(ds)
    for (aid, var, fc), stamps in groups.items():
        for part in chunks(stamps):
            q = ("SELECT id_weather, ds FROM weather WHERE asset_id=? AND weather_var=? AND COALESCE(forecast,0)=? "
                 f"AND ds IN ({','.join('?' * len(part))})")
            for r in conn.execute(q, [aid, var, fc, *part]):
                existing[(r["ds"], aid, var, fc)] = r["id_weather"]
    upd = [(v, existing[k]) for k, v in valid.items() if k in existing]
    ins = [(k[0], k[1], k[2], v, k[3]) for k, v in valid.items() if k not in existing]
    conn.executemany("UPDATE weather SET value=? WHERE id_weather=?", upd)
    conn.executemany("INSERT INTO weather(ds, asset_id, weather_var, value, forecast) VALUES (?, ?, ?, ?, ?)", ins)
    rep.count("weather", "updated", len(upd))
    rep.count("weather", "inserted", len(ins))


def push_price(conn, rows: list[dict], res: Resolver, rep: Report, cfg: DbApiConfig):
    valid: dict[tuple, float] = {}
    for row in rows:
        try:
            ref = row.get("price_tag") or {"country": row.get("country"), "type": row.get("type")}
            tag = res.price_tag(ref, row.get("price_tag_id"))
            ds = norm_ds(row.get("ds"), cfg)
            if cfg.price_ds_format == "epoch":
                ds = int(pd.Timestamp(ds).tz_localize(cfg.data_timezone, ambiguous=True, nonexistent="shift_forward").timestamp())
            valid[(ds, tag)] = finite(row.get("price"), "price")
        except (ValueError, TypeError) as exc:
            rep.reject("price", row, str(exc))
    existing: dict[tuple, int] = {}
    groups: dict[int, list] = {}
    for ds, tag in valid:
        groups.setdefault(tag, []).append(ds)
    for tag, stamps in groups.items():
        for part in chunks(stamps):
            q = f"SELECT id, ds FROM price WHERE price_tag_id=? AND ds IN ({','.join('?' * len(part))})"
            for r in conn.execute(q, [tag, *part]):
                existing[(r["ds"], tag)] = r["id"]
    upd = [(v, existing[k]) for k, v in valid.items() if k in existing]
    ins = [(k[0], k[1], v) for k, v in valid.items() if k not in existing]
    conn.executemany("UPDATE price SET price=? WHERE id=?", upd)
    conn.executemany("INSERT INTO price(ds, price_tag_id, price) VALUES (?, ?, ?)", ins)
    rep.count("price", "updated", len(upd))
    rep.count("price", "inserted", len(ins))


# ==========================================================
# FORECAST
# ==========================================================

class ForecastRequest(BaseModel):
    assets: list[str | int] | None = Field(default=None, description="asset names or ids (default: every asset with energy_values)")
    model: str | None = Field(default=None, description="chronos2 | timesfm3 | timestral (default: configured default)")
    horizon: int | None = Field(default=None, ge=1, description="steps (default: config horizon, 96 x 15 min)")
    history_days: float | None = Field(default=None, gt=0, description="history window ending at each asset's last value")
    use_weather: bool | None = None
    quantiles: list[float] | None = Field(default=None, description="[lower, upper] quantiles, default config interval_quantiles")
    store: bool = Field(default=True, description="write the forecasts into the `forecasts` table")
    auto_load: bool = Field(default=False, description="load the model on demand instead of answering 409")


_FORGES: dict[str, Any] = {}


def get_forge(path_fast: str, name: str):
    if name not in _FORGES:
        _FORGES[name] = core.ClassBuilder(path_fast, name, "forge")
    return _FORGES[name]


def require_model(key: str, ccfg: core.ApiConfig, path_fast: str, auto_load: bool):
    info = core.model_status(key, ccfg)
    if info["ready"]:
        return core._INSTANCES[key]
    if auto_load:
        return core.get_instance(key, ccfg, path_fast)
    if info["status"] == "loading":
        raise HTTPException(status_code=503, detail=f"Model '{key}' is still loading; poll GET /status.")
    if info["status"] == "failed":
        raise HTTPException(status_code=503, detail=f"Model '{key}' failed to load: {info['error']}")
    raise HTTPException(status_code=409, detail=f"Model '{key}' is not loaded. POST /load_model first (or send auto_load=true).")


def regularize(df: pd.DataFrame, freq: str) -> pd.DataFrame:
    """Guarantees a regular grid (the forges normally do this already)."""
    g = df.sort_values("ds").drop_duplicates("ds", keep="last").set_index("ds")
    g = g.reindex(pd.date_range(g.index.min(), g.index.max(), freq=freq))
    g["y"] = g["y"].interpolate(limit_direction="both")
    g["unique_id"] = df["unique_id"].iloc[0]
    g.index.name = "ds"
    return g.reset_index()


def build_weather(conn, asset_id: int, hist: pd.DataFrame, fut_index: pd.DatetimeIndex, cfg: DbApiConfig, allowed: list[str]):
    """Wide weather frame over history+future. History prefers actuals, the future needs forecast rows.
    Variables without complete future coverage (or <50% history coverage) are dropped and reported."""
    start, end = hist["ds"].min(), fut_index.max()
    rows = pd.DataFrame([dict(r) for r in conn.execute(
        "SELECT ds, weather_var, value, COALESCE(forecast,0) AS forecast FROM weather WHERE asset_id=? AND ds>=? AND ds<=?",
        (asset_id, start.strftime(TS_FORMAT), end.strftime(TS_FORMAT)))])
    if rows.empty:
        return None, [], []
    rows["ds"] = pd.to_datetime(rows["ds"])
    if allowed:
        rows = rows[rows["weather_var"].isin(allowed)]
    last = hist["ds"].max()
    rows["preferred"] = np.where(rows["ds"] <= last, rows["forecast"] == 0, rows["forecast"] == 1)
    rows = rows.sort_values("preferred").drop_duplicates(["ds", "weather_var"], keep="last")
    wide = rows.pivot(index="ds", columns="weather_var", values="value")
    grid = pd.date_range(start, end, freq=cfg.freq)
    wide = wide.reindex(wide.index.union(grid)).sort_index().interpolate(method="time", limit_area="inside")
    wide = wide.ffill(limit=cfg.weather_ffill_steps).reindex(grid)
    keep, dropped = [], []
    for col in wide.columns:
        full_future = wide.loc[fut_index, col].notna().all()
        hist_cov = wide.loc[wide.index <= last, col].notna().mean()
        (keep if full_future and hist_cov >= 0.5 else dropped).append(col)
    return wide[keep], keep, dropped


def interval_fallback(y: pd.Series, fcst: pd.DataFrame, cfg: DbApiConfig, quantiles: list[float]):
    """Seasonal-naive residual spread (y_t - y_{t-1 day}) as a symmetric-normal interval around yhat."""
    season = int(pd.Timedelta(days=1) / pd.Timedelta(cfg.freq))
    sigma = (y - y.shift(season)).std()
    if not np.isfinite(sigma):
        return None
    nd = NormalDist()
    return fcst["yhat"] + nd.inv_cdf(min(quantiles)) * sigma, fcst["yhat"] + nd.inv_cdf(max(quantiles)) * sigma


def forecast_asset(conn, asset, key, instance, cfg, ccfg, path_fast, horizon, history_days, use_weather, quantiles):
    """One asset: DB history -> FAST forges -> (weather) -> loaded translator. Returns (frame, info)."""
    asset_id, name = asset["asset_id"], asset["name"] or f"asset_{asset['asset_id']}"
    use_case = asset["type"] if asset["type"] in cfg.use_cases else cfg.default_use_case
    uc = cfg.use_cases[use_case]
    info: dict[str, Any] = {"use_case": use_case, "warnings": []}
    last = conn.execute("SELECT MAX(ds) AS m FROM energy_values WHERE asset_id=?", (asset_id,)).fetchone()["m"]
    if last is None:
        raise ValueError("no energy_values for this asset")
    last_ts = pd.Timestamp(last)
    since = (last_ts - pd.Timedelta(days=history_days)).strftime(TS_FORMAT)
    raw = pd.DataFrame([dict(r) for r in conn.execute(
        "SELECT ds, value AS y FROM energy_values WHERE asset_id=? AND ds>=? ORDER BY ds", (asset_id, since))])
    raw["ds"] = pd.to_datetime(raw["ds"])
    raw["unique_id"] = str(asset_id)
    raw = raw.dropna(subset=["y"])
    if len(raw) < cfg.min_history_points:
        raise ValueError(f"only {len(raw)} values in the last {history_days:g} days (min_history_points={cfg.min_history_points})")

    loc = cfg.locations.get(name) or cfg.locations.get(str(asset["location_id"]))
    meta = {"coords": [loc["lat"], loc["lon"]], "tz": loc.get("tz", cfg.data_timezone)} if loc and "lat" in loc else {"tz": cfg.data_timezone}
    hist = raw
    for step in uc.forges:
        try:
            hist = get_forge(path_fast, step["name"]).process(hist, config_process={**step.get("config", {}), "_asset": meta})
        except Exception as exc:  # same behaviour as ModelInference: report and continue
            info["warnings"].append(f"forge {step['name']} failed: {type(exc).__name__}: {exc}")
    hist = regularize(hist[["ds", "unique_id", "y"]], cfg.freq)
    last_ds = hist["ds"].max()
    fut_index = pd.date_range(last_ds, periods=horizon + 1, freq=cfg.freq)
    future = pd.DataFrame({"ds": fut_index, "unique_id": str(asset_id), "y": None})

    predict_args: dict[str, Any] = {"quantiles": quantiles}
    info["weather_vars"] = []
    if use_weather and key in cfg.weather_models:
        wide, keep, dropped = build_weather(conn, asset_id, hist, fut_index, cfg, uc.weather_vars)
        if dropped:
            info["warnings"].append(f"weather variables dropped (incomplete future forecast or sparse history): {dropped}")
        if keep:
            hist = hist.merge(wide[keep].reset_index(names="ds"), on="ds", how="left")
            future = future.merge(wide[keep].reset_index(names="ds"), on="ds", how="left")
            info["weather_vars"] = keep
            if core.MODEL_INFO.get(core.configured_models(ccfg)[key]["translate"], {}).get("covariates") is not True:
                predict_args["covariate_cols"] = keep
    predict_args = {**core.configured_models(ccfg)[key]["config"].get("predict_args", {}), **predict_args}

    with core._PREDICT_LOCKS[key]:
        fc = instance.predict(data=hist, horizon=horizon, future_df=future, **predict_args)
    fc = fc.rename(columns={"item_id": "unique_id"}).copy()
    fc["ds"] = pd.to_datetime(fc["ds"])
    fc = fc.sort_values("ds").reset_index(drop=True)
    for col in ("yhat_lower", "yhat_upper"):
        if col not in fc.columns:
            fc[col] = np.nan
    info["interval_source"] = "model_quantiles"
    if fc[["yhat_lower", "yhat_upper"]].isna().any(axis=None):
        info["interval_source"] = "none"
        if cfg.interval_fallback == "seasonal_residual":
            bounds = interval_fallback(hist["y"], fc, cfg, quantiles)
            if bounds is not None:
                fc["yhat_lower"], fc["yhat_upper"] = bounds
                info["interval_source"] = "seasonal_residual"
    if uc.clip_lower is not None:
        fc[["yhat", "yhat_lower", "yhat_upper"]] = fc[["yhat", "yhat_lower", "yhat_upper"]].clip(lower=uc.clip_lower)
    fc["yhat_lower"] = np.fmin(fc["yhat_lower"], fc["yhat"])
    fc["yhat_upper"] = np.fmax(fc["yhat_upper"], fc["yhat"])
    stale_hours = (pd.Timestamp.now(tz=cfg.data_timezone).tz_localize(None) - last_ts).total_seconds() / 3600
    if stale_hours > cfg.stale_after_hours:
        info["warnings"].append(f"last value is {stale_hours:.0f} h old (stale_after_hours={cfg.stale_after_hours:g})")
    info["last_value_ds"] = last_ts.strftime(TS_FORMAT)
    return fc[["ds", "yhat", "yhat_lower", "yhat_upper"]], info


def get_var_id(conn, use_case: str, uc: UseCaseConfig) -> int:
    row = conn.execute("SELECT id FROM forecast_var_names WHERE var_name=?", (uc.var_name,)).fetchone()
    if row:
        return row["id"]
    return conn.execute("INSERT INTO forecast_var_names(var_name, type) VALUES (?, ?)", (uc.var_name, use_case)).lastrowid


def pick_assets(conn, refs) -> list[dict]:
    rows = [dict(r) for r in conn.execute("SELECT asset_id, name, type, location_id FROM assets ORDER BY asset_id")]
    if refs is None:
        have = {r["asset_id"] for r in conn.execute("SELECT DISTINCT asset_id FROM energy_values")}
        return [r for r in rows if r["asset_id"] in have]
    out, missing = [], []
    for ref in refs:
        match = next((r for r in rows if r["name"] == str(ref)), None) or next(
            (r for r in rows if str(r["asset_id"]) == str(ref)), None)
        (out if match else missing).append(match or ref)
    if missing:
        raise HTTPException(status_code=404, detail=f"Unknown assets: {missing}")
    return out


# ==========================================================
# APP
# ==========================================================

@asynccontextmanager
async def lifespan(_app):
    cfg = load_config()
    ensure_db(cfg)
    ccfg = core_config(cfg)
    for key in cfg.preload:
        if key in core.configured_models(ccfg):
            core.start_load(key, ccfg, core.resolve_path_fast(ccfg))
    yield


app = FastAPI(title="FAST DB API", version="1.0.0", lifespan=lifespan)


class LoadRequest(BaseModel):
    model: str | None = None
    device: str | None = Field(default=None, description="cpu | cuda | mps (overrides the configured load_args.device)")
    horizon: int | None = Field(default=None, ge=1, description="default forecast horizon (steps) used by /forecast from now on")
    wait: bool = False
    force: bool = False


_RUNTIME: dict[str, Any] = {"horizon": None}


@app.get("/health")
def health() -> dict[str, str]:
    load_config()
    return {"status": "ok"}


@app.post("/load_model")
def load_model(response: Response, request: LoadRequest | None = None) -> dict[str, Any]:
    request = request or LoadRequest()
    cfg = load_config()
    ccfg = core_config(cfg)
    key = core.select_model(ccfg, request.model)
    if request.device:
        models = [dict(m) for m in cfg.models]
        for m, name in zip(models, ccfg.translate):
            if core.MODEL_INFO.get(name, {}).get("key", name) == key:
                m["load_args"] = {**m.get("load_args", {}), "device": request.device}
        ccfg = core_config(cfg.model_copy(update={"models": models}))
        if key in core._INSTANCES:
            request.force = True
    if request.horizon:
        _RUNTIME["horizon"] = request.horizon
    path_fast = core.resolve_path_fast(ccfg)
    if request.wait:
        if request.force:
            core._INSTANCES.pop(key, None)
        core.get_instance(key, ccfg, path_fast)
    else:
        core.start_load(key, ccfg, path_fast, request.force)
        response.status_code = 202
    return {**core.model_status(key, ccfg), "horizon": _RUNTIME["horizon"] or cfg.horizon}


@app.get("/status")
def status() -> dict[str, Any]:
    cfg = load_config()
    ccfg = core_config(cfg)
    default = core.select_model(ccfg, None)
    models = [core.model_status(key, ccfg) for key in core.configured_models(ccfg)]
    out: dict[str, Any] = {
        "default_model": default,
        "ready": any(m["ready"] for m in models if m["model"] == default),
        "horizon": _RUNTIME["horizon"] or cfg.horizon,
        "models": models,
        "db": {"path": str(db_file(cfg)), "reachable": False, "assets": []},
    }
    try:
        with db(cfg) as conn:
            now = pd.Timestamp.now(tz=cfg.data_timezone).tz_localize(None)
            rows = conn.execute(
                "SELECT a.asset_id, a.name, a.type, MAX(e.ds) AS last_ds, COUNT(e.id) AS n FROM assets a "
                "LEFT JOIN energy_values e ON e.asset_id=a.asset_id GROUP BY a.asset_id ORDER BY a.asset_id").fetchall()
            for r in rows:
                age = (now - pd.Timestamp(r["last_ds"])).total_seconds() / 3600 if r["last_ds"] else None
                out["db"]["assets"].append({"asset_id": r["asset_id"], "name": r["name"], "type": r["type"], "last_ds": r["last_ds"],
                                            "rows": r["n"], "stale": age is None or age > cfg.stale_after_hours})
            total = conn.execute("SELECT COUNT(*) AS n FROM energy_values").fetchone()["n"]
            out["db"]["reachable"] = True
            out["db"]["energy_values_rows"] = total
            out["db"]["empty"] = total == 0
    except Exception as exc:
        out["db"]["error"] = f"{type(exc).__name__}: {exc}"
    if not out["db"]["reachable"]:
        out["state"] = "db_unreachable"
    elif out["db"]["empty"]:
        out["state"], out["error"] = "empty_database", "empty database: push data to energy_values first (POST /push_data)"
    elif not out["ready"]:
        out["state"] = "model_not_loaded"
    else:
        out["state"] = "ready"
    return out


@app.post("/push_data")
def push_data(request: PushRequest) -> dict[str, Any]:
    cfg = load_config()
    total = sum(len(getattr(request, f)) for f in ("price_tags", "assets", "forecast_var_names", "energy_values", "weather", "price"))
    if total > cfg.max_rows_per_request:
        raise HTTPException(status_code=413, detail=f"{total} rows exceed max_rows_per_request={cfg.max_rows_per_request}")
    sections = {"assets": request.assets, "price_tags": request.price_tags, "forecast_var_names": request.forecast_var_names,
                "energy_values": request.energy_values, "weather": request.weather, "price": request.price}
    blocked = [k for k, rows in sections.items() if rows and k not in cfg.push_tables]
    if blocked:
        raise HTTPException(status_code=422, detail=f"/push_data accepts only {cfg.push_tables} in this configuration; got rows for {blocked}")
    create = cfg.auto_create_assets if request.create_missing_assets is None else request.create_missing_assets
    rep = Report()
    try:
        with db(cfg, write=True) as conn:
            res = Resolver(conn, create, request.default_asset_type or cfg.default_asset_type, rep)
            push_catalog(conn, request, res, rep)
            push_energy(conn, request.energy_values, res, rep, cfg)
            if "weather" in cfg.push_tables:
                push_weather(conn, request.weather, res, rep, cfg)
            if "price" in cfg.push_tables:
                push_price(conn, request.price, res, rep, cfg)
    except sqlite3.OperationalError as exc:
        raise HTTPException(status_code=503, detail=f"Database busy or unavailable: {exc}") from exc
    return {"status": "ok", "tables": rep.tables, "rejected_examples": rep.examples}


@app.post("/forecast")
def forecast(request: ForecastRequest) -> dict[str, Any]:
    t0 = time.time()
    cfg = load_config()
    ccfg = core_config(cfg)
    key = core.select_model(ccfg, request.model)
    path_fast = core.resolve_path_fast(ccfg)
    quantiles = request.quantiles or cfg.interval_quantiles
    if len(quantiles) != 2 or not all(0 < q < 1 for q in quantiles):
        raise HTTPException(status_code=400, detail="quantiles must be [lower, upper] within (0, 1)")
    if cfg.require_data:
        with db(cfg) as conn:
            if conn.execute("SELECT 1 FROM energy_values LIMIT 1").fetchone() is None:
                raise HTTPException(status_code=409, detail="empty database: push data to energy_values first (POST /push_data)")
    instance = require_model(key, ccfg, path_fast, request.auto_load)
    horizon = request.horizon or _RUNTIME["horizon"] or cfg.horizon
    history_days = request.history_days or cfg.history_days
    use_weather = cfg.use_weather if request.use_weather is None else request.use_weather
    made = pd.Timestamp.now(tz=cfg.forecast_timestamp_tz).tz_localize(None).strftime(TS_FORMAT)

    cols: dict[str, list] = {k: [] for k in ("ds", "asset_id", "asset", "yhat", "yhat_lower", "yhat_upper")}
    per_asset, skipped, pending = [], [], []
    with db(cfg) as conn:  # read phase: no write lock is held while the model runs
        for asset in pick_assets(conn, request.assets):
            label = asset["name"] or asset["asset_id"]
            try:
                fc, info = forecast_asset(conn, asset, key, instance, cfg, ccfg, path_fast, horizon, history_days, use_weather, quantiles)
            except ValueError as exc:
                skipped.append({"asset": label, "reason": str(exc)})
                continue
            except HTTPException:
                raise
            except Exception as exc:
                skipped.append({"asset": label, "reason": f"{type(exc).__name__}: {exc}"})
                continue
            stamps = fc["ds"].dt.strftime(TS_FORMAT).tolist()
            vals = {c: [None if pd.isna(v) else float(v) for v in fc[c]] for c in ("yhat", "yhat_lower", "yhat_upper")}
            cols["ds"] += stamps
            cols["asset_id"] += [asset["asset_id"]] * len(fc)
            cols["asset"] += [label] * len(fc)
            for c in vals:
                cols[c] += vals[c]
            per_asset.append({"asset": label, "asset_id": asset["asset_id"], **info})
            pending.append((asset["asset_id"], info["use_case"], stamps, vals))
    if not pending and skipped and all(x["reason"].startswith("no energy_values") for x in skipped):
        raise HTTPException(status_code=409, detail="empty database for the requested assets: push data to energy_values first (%s)"
                            % ", ".join(str(x["asset"]) for x in skipped))
    stored = 0
    if request.store and pending:
        try:
            with db(cfg, write=True) as conn:
                for asset_id, use_case, stamps, vals in pending:
                    var_id = get_var_id(conn, use_case, cfg.use_cases[use_case])
                    conn.executemany(
                        "INSERT INTO forecasts(ds, asset_id, var_id, yhat, yhat_lower, yhat_upper, ds_forecast, model_name) VALUES (?,?,?,?,?,?,?,?)",
                        [(s, asset_id, var_id, a, lo, hi, made, key)
                         for s, a, lo, hi in zip(stamps, vals["yhat"], vals["yhat_lower"], vals["yhat_upper"])])
                    stored += len(stamps)
        except sqlite3.OperationalError as exc:
            raise HTTPException(status_code=503, detail=f"Forecast computed but could not be stored (database busy): {exc}") from exc
    return {**cols, "model": key, "ds_forecast": made, "horizon": horizon, "interval_quantiles": quantiles,
            "stored_rows": stored, "assets": per_asset, "skipped": skipped, "seconds": round(time.time() - t0, 2)}


@app.get("/forecasts")
def latest_forecasts(asset: str | None = Query(default=None, description="asset name or id"),
                     model: str | None = Query(default=None)) -> dict[str, Any]:
    """Latest stored forecast run per asset (and model)."""
    cfg = load_config()
    with db(cfg) as conn:
        sql = ("SELECT f.ds, f.asset_id, a.name AS asset, v.var_name, f.yhat, f.yhat_lower, f.yhat_upper, f.ds_forecast, f.model_name "
               "FROM forecasts f LEFT JOIN assets a ON a.asset_id=f.asset_id LEFT JOIN forecast_var_names v ON v.id=f.var_id "
               "WHERE f.ds_forecast = (SELECT MAX(f2.ds_forecast) FROM forecasts f2 WHERE f2.asset_id=f.asset_id "
               "AND (? IS NULL OR f2.model_name=?))")
        params: list[Any] = [model, model]
        if model:
            sql += " AND f.model_name=?"
            params.append(model)
        if asset:
            sql += " AND (a.name=? OR CAST(f.asset_id AS TEXT)=?)"
            params += [asset, asset]
        rows = conn.execute(sql + " ORDER BY f.asset_id, f.ds", params).fetchall()
    keys = ["ds", "asset_id", "asset", "var_name", "yhat", "yhat_lower", "yhat_upper", "ds_forecast", "model_name"]
    return {k: [r[k] for r in rows] for k in keys}
