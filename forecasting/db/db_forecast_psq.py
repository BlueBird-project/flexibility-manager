"""PostgreSQL access for the price pipeline (schema: db_forecast_pg.sql). Same functions as db_forecast.py.

The connection string comes from FORECAST_DB_URL, e.g. postgresql://user:password@localhost:5432/pricefm.
connect() creates the database if it does not exist (needs a role with CREATEDB), applies the schema and
seeds one Intraday-15min price tag per country the first time. Nothing is touched if the tables exist.
"""

import os
import time
from datetime import datetime, timezone
from pathlib import Path

import pandas as pd
import psycopg
from psycopg import sql
from psycopg.conninfo import conninfo_to_dict, make_conninfo

from db_forecast import (COUNTRY_REGION, DEFAULT_COUNTRIES, DEFAULT_TAG_TYPE, MODEL_NAME,  # noqa: F401
                         PRICE_TAG_TYPE, REGIONS, country_to_region)

DB_URL = os.environ.get("FORECAST_DB_URL", "")
SCHEMA_FILE = os.environ.get("FORECAST_SCHEMA_PG", "db_forecast_pg.sql")
ADMIN_DB = os.environ.get("FORECAST_ADMIN_DB", "postgres")
CONNECT_RETRY_SECONDS = float(os.environ.get("FORECAST_DB_CONNECT_RETRY_SECONDS", 30))
BOOTSTRAP_LOCK = 727_001  # advisory lock id: concurrent starters bootstrap one at a time


def find_schema():
    """Schema file: FORECAST_SCHEMA_PG path, else the current folder, else next to this script."""
    given = Path(SCHEMA_FILE).expanduser()
    candidates = [given] if given.is_absolute() else [Path.cwd() / given, Path(__file__).resolve().parent / given]
    return next((p for p in candidates if p.is_file()), None)


def _open(dsn, **kwargs):
    """psycopg.connect retrying while the server is still starting (connection refused)."""
    deadline = time.monotonic() + CONNECT_RETRY_SECONDS
    while True:
        try:
            return psycopg.connect(dsn, **kwargs)
        except psycopg.OperationalError:
            if time.monotonic() >= deadline:
                raise
            time.sleep(2)


def _tables_exist(conn):
    return conn.execute("SELECT to_regclass('public.price_forecast') IS NOT NULL "
                        "AND to_regclass('public.price') IS NOT NULL").fetchone()[0]


def _create_database(dsn, name):
    admin = make_conninfo(dsn, dbname=ADMIN_DB)
    with _open(admin, autocommit=True) as conn:
        if not conn.execute("SELECT 1 FROM pg_database WHERE datname = %s", (name,)).fetchone():
            conn.execute(sql.SQL("CREATE DATABASE {}").format(sql.Identifier(name)))
            print(f"[db_forecast_psq] created database '{name}'")


def seed_defaults(conn):
    """Default rows of a new database: one Intraday-15min price tag per country."""
    conn.cursor().executemany(
        "INSERT INTO price_tag (country, type) VALUES (%s, %s) ON CONFLICT (country, type) DO NOTHING",
        [(country, DEFAULT_TAG_TYPE) for country in DEFAULT_COUNTRIES])


def connect(dsn=None):
    """Open the database, creating it, the tables and the default tags if they are missing."""
    dsn = dsn or DB_URL
    if not dsn:
        raise ValueError("No PostgreSQL connection string: set FORECAST_DB_URL or pass --db-url")
    name = conninfo_to_dict(dsn).get("dbname")
    if not name:
        raise ValueError("The connection string must name the database (.../pricefm)")
    try:
        conn = _open(dsn, autocommit=True)
    except psycopg.OperationalError as exc:
        if f'"{name}" does not exist' not in str(exc):
            raise
        conn = None
    if conn is None:
        if find_schema() is None:
            raise FileNotFoundError(f"Database '{name}' not found and no schema file ({SCHEMA_FILE}) to create it")
        _create_database(dsn, name)
        conn = _open(dsn, autocommit=True)

    if not _tables_exist(conn):
        schema = find_schema()
        if schema is None:
            conn.close()
            raise FileNotFoundError(f"Tables missing in '{name}' and no schema file ({SCHEMA_FILE}) to create them")
        conn.autocommit = True
        conn.execute("SELECT pg_advisory_lock(%s)", (BOOTSTRAP_LOCK,))
        try:
            if not _tables_exist(conn):
                conn.execute(schema.read_text(encoding="utf-8"))
                conn.autocommit = False
                seed_defaults(conn)
                conn.commit()
                print(f"[db_forecast_psq] schema applied and default price tags created in '{name}'")
        finally:
            conn.autocommit = True
            conn.execute("SELECT pg_advisory_unlock(%s)", (BOOTSTRAP_LOCK,))
    conn.autocommit = False
    if conn.execute("SELECT count(*) FROM price_tag").fetchone()[0] == 0:
        seed_defaults(conn)
        conn.commit()
        print(f"[db_forecast_psq] price_tag was empty: default price tags created in '{name}'")
    return conn


def get_price_tags(conn):
    """One price tag per country (price_tag.id) of type 'Intraday...': DataFrame price_tag_id, country, region."""
    rows = conn.execute("SELECT id, country FROM price_tag WHERE type ILIKE %s ORDER BY id",
                        (f"{PRICE_TAG_TYPE}%",)).fetchall()
    frame = pd.DataFrame(rows, columns=["price_tag_id", "country"])
    frame["region"] = frame["country"].map(country_to_region)
    return frame.dropna(subset=["region"]).drop_duplicates("country").reset_index(drop=True)


def _naive(ts):
    ts = pd.Timestamp(ts)
    return (ts.tz_convert("UTC").tz_localize(None) if ts.tzinfo else ts).to_pydatetime()


def _num(value):
    return None if value is None or pd.isna(value) else float(value)


def save_prices(conn, prices):
    """prices: DataFrame price_tag_id, ds (UTC), price. Inserts or updates by (price_tag_id, ds)."""
    if prices.empty:
        return 0
    rows = [(_naive(r.ds), int(r.price_tag_id), float(r.price)) for r in prices.itertuples()]
    conn.cursor().executemany(
        """INSERT INTO price (ds, price_tag_id, price) VALUES (%s, %s, %s)
           ON CONFLICT (price_tag_id, ds) DO UPDATE SET price = EXCLUDED.price""", rows)
    conn.commit()
    return len(rows)


def save_price_forecasts(conn, forecasts, model_name=MODEL_NAME, created_at=None):
    """forecasts: DataFrame price_tag_id, ds, yhat, yhat_lower, yhat_upper -> price_forecast.
    Inserts or updates by (price_tag_id, model_name, ds)."""
    if forecasts.empty:
        return 0
    created = _naive(created_at or datetime.now(timezone.utc))
    rows = [(_naive(r.ds), int(r.price_tag_id), float(r.yhat), _num(r.yhat_lower), _num(r.yhat_upper),
             created, model_name) for r in forecasts.itertuples()]
    conn.cursor().executemany(
        """INSERT INTO price_forecast (ds, price_tag_id, yhat, yhat_lower, yhat_upper, ds_forecast, model_name)
           VALUES (%s, %s, %s, %s, %s, %s, %s)
           ON CONFLICT (price_tag_id, model_name, ds) DO UPDATE SET
               yhat = EXCLUDED.yhat, yhat_lower = EXCLUDED.yhat_lower,
               yhat_upper = EXCLUDED.yhat_upper, ds_forecast = EXCLUDED.ds_forecast""", rows)
    conn.commit()
    return len(rows)
