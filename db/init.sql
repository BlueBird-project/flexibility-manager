-- BlueBird shared database schema
-- Engine-agnostic DDL (compatible with SQLite and PostgreSQL)

-- Sensor measurements written by the plant / BMS
CREATE TABLE IF NOT EXISTS measurements (
    id        INTEGER PRIMARY KEY,
    timestamp TEXT    NOT NULL,
    room_id   INTEGER NOT NULL,
    T_air     REAL    NOT NULL,
    T_product REAL    NOT NULL
);

-- Disturbance forecasts written by the Disturbances Forecast service
CREATE TABLE IF NOT EXISTS disturbance_forecasts (
    id        INTEGER PRIMARY KEY,
    timestamp TEXT    NOT NULL,
    room_id   INTEGER NOT NULL,
    T_ambient REAL    NOT NULL,
    door_load REAL    NOT NULL DEFAULT 0.0
);

-- Control setpoints written by the Flexibility Controller, read by the Knowledge Engine
CREATE TABLE IF NOT EXISTS setpoints (
    id        INTEGER PRIMARY KEY,
    timestamp TEXT    NOT NULL,
    room_id   INTEGER NOT NULL,
    u         REAL    NOT NULL,
    p3        REAL    NOT NULL DEFAULT 0.0
);

-- NeuralForecast outputs (existing schema from forecasting service)
CREATE TABLE IF NOT EXISTS assets (
    asset_id     INTEGER PRIMARY KEY NOT NULL,
    name         TEXT,
    location_id  INTEGER,
    type         TEXT,
    price_tag_id INTEGER,
    FOREIGN KEY (price_tag_id) REFERENCES price_tag (id)
);

CREATE TABLE IF NOT EXISTS price_tag (
    id      INTEGER PRIMARY KEY NOT NULL,
    country TEXT,
    type    TEXT
);

CREATE TABLE IF NOT EXISTS price (
    id           INTEGER PRIMARY KEY NOT NULL,
    ds           TEXT,
    price_tag_id INTEGER,
    price        REAL,
    FOREIGN KEY (price_tag_id) REFERENCES price_tag (id)
);

CREATE TABLE IF NOT EXISTS weather (
    id_weather  INTEGER PRIMARY KEY NOT NULL,
    ds          TEXT,
    asset_id    INTEGER,
    weather_var TEXT,
    value       REAL,
    forecast    INTEGER,
    FOREIGN KEY (asset_id) REFERENCES assets (asset_id)
);

CREATE TABLE IF NOT EXISTS forecasts (
    id           INTEGER PRIMARY KEY NOT NULL,
    ds           TEXT,
    asset_id     INTEGER,
    var_name     TEXT,
    yhat         REAL,
    yhat_lower   REAL,
    yhat_upper   REAL,
    ds_forecast  TEXT,
    model_name   TEXT,
    FOREIGN KEY (asset_id) REFERENCES assets (asset_id)
);

-- Seed measurements: 5 cold rooms at typical operating temperatures
-- (dairy, finished products, meat, vegetables, freezer)
INSERT OR IGNORE INTO measurements (id, timestamp, room_id, T_air, T_product) VALUES
    (1, datetime('now'), 1, 4.5,  4.8),
    (2, datetime('now'), 2, 2.1,  2.3),
    (3, datetime('now'), 3, 3.8,  4.0),
    (4, datetime('now'), 4, 5.2,  5.5),
    (5, datetime('now'), 5, -18.3, -18.0);
