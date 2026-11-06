-- PostgreSQL version of db_forecast_test.sql (+ price_forecast). ds columns are UTC timestamps.
-- Run: psql -U postgres -d pricefm -f db_forecast_pg.sql   (safe to rerun)

BEGIN;

CREATE TABLE IF NOT EXISTS price_tag (
    id      BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    country TEXT,
    type    TEXT,
    UNIQUE (country, type)
);

CREATE TABLE IF NOT EXISTS assets (
    asset_id     BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    name         TEXT,
    location_id  BIGINT,
    type         TEXT,
    price_tag_id BIGINT REFERENCES price_tag (id)
);

CREATE TABLE IF NOT EXISTS forecast_var_names (
    id       BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    var_name TEXT,
    type     TEXT
);

CREATE TABLE IF NOT EXISTS weather (
    id_weather  BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ds          TIMESTAMP,
    asset_id    BIGINT REFERENCES assets (asset_id),
    weather_var TEXT,
    value       DOUBLE PRECISION,
    forecast    BOOLEAN
);

CREATE TABLE IF NOT EXISTS forecasts (
    id          BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ds          TIMESTAMP,
    asset_id    BIGINT REFERENCES assets (asset_id),
    var_id      BIGINT REFERENCES forecast_var_names (id),
    yhat        DOUBLE PRECISION,
    yhat_lower  DOUBLE PRECISION,
    yhat_upper  DOUBLE PRECISION,
    ds_forecast TIMESTAMP,
    model_name  TEXT
);

CREATE TABLE IF NOT EXISTS energy_values (
    id       BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ds       TIMESTAMP,
    asset_id BIGINT,
    value    DOUBLE PRECISION,
    UNIQUE (ds, asset_id)
);

CREATE TABLE IF NOT EXISTS price (
    id           BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ds           TIMESTAMP,
    price_tag_id BIGINT REFERENCES price_tag (id),
    price        DOUBLE PRECISION,
    UNIQUE (price_tag_id, ds)
);

CREATE TABLE IF NOT EXISTS price_forecast (
    id           BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    ds           TIMESTAMP,
    price_tag_id BIGINT REFERENCES price_tag (id),
    yhat         DOUBLE PRECISION,
    yhat_lower   DOUBLE PRECISION,
    yhat_upper   DOUBLE PRECISION,
    ds_forecast  TIMESTAMP,
    model_name   TEXT,
    UNIQUE (price_tag_id, model_name, ds)
);

COMMIT;
