"""SQLite access for the price pipeline (schema: db_forecast_test.sql).

Tables used: price_tag, assets, price, forecasts, forecast_var_names.
"""

import json
import os
import sqlite3
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd

PRICE_TAG_TYPE = os.environ.get("PRICE_TAG_TYPE", "Intraday")  # matched with LIKE '<type>%'
FORECAST_VAR_NAME = os.environ.get("FORECAST_VAR_NAME", "price")
MODEL_NAME = os.environ.get("FORECAST_MODEL_NAME", "PriceFM_Plus")
DS_FORMAT = "%Y-%m-%d %H:%M:%S"

# price_tag.country -> PriceFM bidding zone. Codes (e.g. "BE") are accepted as they are.
# Multi-zone countries default to one zone; override with COUNTRY_REGION_MAP='{"Italy": "IT_CNOR"}'.
COUNTRY_REGION = {
    "austria": "AT", "belgium": "BE", "bulgaria": "BG", "switzerland": "CH", "czechia": "CZ",
    "czech republic": "CZ", "germany": "DE_LU", "luxembourg": "DE_LU", "denmark": "DK_1",
    "estonia": "EE", "spain": "ES", "espana": "ES", "españa": "ES", "finland": "FI",
    "france": "FR", "greece": "GR", "croatia": "HR", "hungary": "HU", "italy": "IT_NORD",
    "lithuania": "LT", "latvia": "LV", "netherlands": "NL", "norway": "NO_1", "poland": "PL",
    "portugal": "PT", "romania": "RO", "serbia": "RS", "sweden": "SE_3", "slovenia": "SI",
    "slovakia": "SK",
}
REGIONS = {"AT", "BE", "BG", "CH", "CZ", "DE_LU", "DK_1", "DK_2", "EE", "ES", "FI", "FR", "GR", "HR",
           "HU", "IT_CALA", "IT_CNOR", "IT_CSUD", "IT_NORD", "IT_SARD", "IT_SICI", "IT_SUD", "LT",
           "LV", "NL", "NO_1", "NO_2", "NO_3", "NO_4", "NO_5", "PL", "PT", "RO", "RS", "SE_1",
           "SE_2", "SE_3", "SE_4", "SI", "SK"}


def country_to_region(country):
    if not country:
        return None
    text = str(country).strip()
    extra = {k.lower(): v for k, v in json.loads(os.environ.get("COUNTRY_REGION_MAP", "{}")).items()}
    if text.upper() in REGIONS:
        return text.upper()
    return extra.get(text.lower()) or COUNTRY_REGION.get(text.lower())


SCHEMA_FILE = os.environ.get("FORECAST_SCHEMA", "db_forecast_test.sql")


def find_schema():
    """Schema file: FORECAST_SCHEMA path, else the current folder, else next to this script."""
    given = Path(SCHEMA_FILE).expanduser()
    candidates = [given] if given.is_absolute() else [Path.cwd() / given, Path(__file__).resolve().parent / given]
    return next((p for p in candidates if p.is_file()), None)


DEFAULT_TAG_TYPE = "Intraday-15min"
DEFAULT_COUNTRIES = [
    "Austria", "Belgium", "Bulgaria", "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France",
    "Germany", "Greece", "Hungary", "Italy", "Latvia", "Lithuania", "Netherlands", "Norway", "Poland",
    "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
]


def seed_defaults(conn):
    """Default rows of a new database: one Intraday-15min price tag per country."""
    conn.executemany("INSERT INTO price_tag (country, type) VALUES (?, ?)",
                     [(country, DEFAULT_TAG_TYPE) for country in DEFAULT_COUNTRIES])


PRICE_FORECAST_DDL = """
CREATE TABLE IF NOT EXISTS "price_forecast" (
    "id" INTEGER PRIMARY KEY NOT NULL,
    "ds" INTEGER,
    "price_tag_id" INTEGER,
    "yhat" REAL,
    "yhat_lower" REAL,
    "yhat_upper" REAL,
    "ds_forecast" INTEGER,
    "model_name" TEXT,
    FOREIGN KEY("price_tag_id") REFERENCES "price_tag"("id")
);
"""


def ensure_price_forecast(conn):
    """price_forecast is linked to a price_tag (shared by many assets); created if the schema lacks it."""
    conn.executescript(PRICE_FORECAST_DDL)


def connect(db_path):
    path = Path(db_path).expanduser().resolve()
    if not path.is_file():
        schema = find_schema()
        if schema is None:
            raise FileNotFoundError(f"Database not found: {path} and no schema file ({SCHEMA_FILE}) to create it")
        path.parent.mkdir(parents=True, exist_ok=True)
        conn = sqlite3.connect(path)
        try:
            conn.executescript(schema.read_text(encoding="utf-8"))
            seed_defaults(conn)
            ensure_price_forecast(conn)
            conn.commit()
        except Exception:
            conn.close()
            path.unlink(missing_ok=True)
            raise
        conn.execute("PRAGMA foreign_keys = ON")
        return conn
    conn = sqlite3.connect(path)
    ensure_price_forecast(conn)
    conn.execute("PRAGMA foreign_keys = ON")
    return conn


def _ds_is_epoch(conn, table):
    row = conn.execute(f"SELECT typeof(ds) FROM {table} WHERE ds IS NOT NULL LIMIT 1").fetchone()
    return bool(row) and row[0] == "integer"


def _ds_value(ts, epoch):
    ts = pd.Timestamp(ts)
    return int(ts.tz_localize("UTC").timestamp()) if epoch else ts.strftime(DS_FORMAT)


def get_forecast_var_id(conn, name=FORECAST_VAR_NAME):
    row = conn.execute("SELECT id FROM forecast_var_names WHERE var_name = ?", (name,)).fetchone()
    if row:
        return row[0]
    raise LookupError(f"'{name}' not found in forecast_var_names; add it first")


def get_targets(conn):
    """Assets with a price tag of type 'Intraday...' whose country maps to a PriceFM zone.

    Returns a DataFrame: asset_id, price_tag_id, country, region.
    """
    rows = conn.execute(
        """SELECT a.asset_id, t.id, t.country FROM assets a
           JOIN price_tag t ON t.id = a.price_tag_id WHERE t.type LIKE ?""",
        (f"{PRICE_TAG_TYPE}%",)).fetchall()
    frame = pd.DataFrame(rows, columns=["asset_id", "price_tag_id", "country"])
    frame["region"] = frame["country"].map(country_to_region)
    unmapped = sorted(frame.loc[frame["region"].isna(), "country"].dropna().unique())
    if unmapped:
        print(f"WARNING countries without PriceFM zone (skipped): {unmapped}")
    return frame.dropna(subset=["region"]).reset_index(drop=True)


def get_price_tags(conn):
    """One price tag per country (price_tag.id) of type 'Intraday...': DataFrame id, country, region."""
    rows = conn.execute("SELECT id, country FROM price_tag WHERE type LIKE ? ORDER BY id",
                        (f"{PRICE_TAG_TYPE}%",)).fetchall()
    frame = pd.DataFrame(rows, columns=["price_tag_id", "country"])
    frame["region"] = frame["country"].map(country_to_region)
    return frame.dropna(subset=["region"]).drop_duplicates("country").reset_index(drop=True)


def save_prices(conn, prices):
    """prices: DataFrame price_tag_id, ds (naive UTC), price. Replaces existing rows of those slots."""
    if prices.empty:
        return 0
    epoch = _ds_is_epoch(conn, "price")
    rows = [(int(r.price_tag_id), _ds_value(r.ds, epoch), float(r.price)) for r in prices.itertuples()]
    with conn:
        conn.executemany("DELETE FROM price WHERE price_tag_id = ? AND ds = ?",
                         [(t, d) for t, d, _ in rows])
        conn.executemany("INSERT INTO price (ds, price_tag_id, price) VALUES (?, ?, ?)",
                         [(d, t, p) for t, d, p in rows])
    return len(rows)


def save_forecasts(conn, forecasts, var_id, model_name=MODEL_NAME, created_at=None):
    """forecasts: DataFrame asset_id, ds, yhat, yhat_lower, yhat_upper. Replaces same asset/var/model/slot."""
    if forecasts.empty:
        return 0
    epoch = _ds_is_epoch(conn, "forecasts")
    created = _ds_value(created_at or datetime.now(timezone.utc).replace(tzinfo=None), epoch)
    rows = [(_ds_value(r.ds, epoch), int(r.asset_id), var_id, float(r.yhat),
             None if pd.isna(r.yhat_lower) else float(r.yhat_lower),
             None if pd.isna(r.yhat_upper) else float(r.yhat_upper), created, model_name)
            for r in forecasts.itertuples()]
    with conn:
        conn.executemany("DELETE FROM forecasts WHERE ds = ? AND asset_id = ? AND var_id = ? AND model_name = ?",
                         [(r[0], r[1], r[2], r[7]) for r in rows])
        conn.executemany(
            """INSERT INTO forecasts (ds, asset_id, var_id, yhat, yhat_lower, yhat_upper, ds_forecast, model_name)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?)""", rows)
    return len(rows)


def save_price_forecasts(conn, forecasts, model_name=MODEL_NAME, created_at=None):
    """forecasts: DataFrame price_tag_id, ds, yhat, yhat_lower, yhat_upper -> price_forecast.
    Replaces the same price_tag/model/slot."""
    if forecasts.empty:
        return 0
    epoch = _ds_is_epoch(conn, "price_forecast")
    created = _ds_value(created_at or datetime.now(timezone.utc).replace(tzinfo=None), epoch)
    rows = [(_ds_value(r.ds, epoch), int(r.price_tag_id), float(r.yhat),
             None if pd.isna(r.yhat_lower) else float(r.yhat_lower),
             None if pd.isna(r.yhat_upper) else float(r.yhat_upper), created, model_name)
            for r in forecasts.itertuples()]
    with conn:
        conn.executemany(
            "DELETE FROM price_forecast WHERE ds = ? AND price_tag_id = ? AND model_name = ?",
            [(r[0], r[1], r[6]) for r in rows])
        conn.executemany(
            """INSERT INTO price_forecast (ds, price_tag_id, yhat, yhat_lower, yhat_upper, ds_forecast, model_name)
               VALUES (?, ?, ?, ?, ?, ?, ?)""", rows)
    return len(rows)
