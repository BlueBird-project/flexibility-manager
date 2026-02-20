-- SQLite database export
PRAGMA foreign_keys = ON;

BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "weather" (
    "id_weather" INTEGER PRIMARY KEY NOT NULL,
    "ds" TEXT,
    "asset_id" INTEGER,
    "weather_var" TEXT,
    "value" FLOAT,
    "forecast" BOOL,
    FOREIGN KEY("asset_id") REFERENCES "assets"("asset_id")
);


CREATE TABLE IF NOT EXISTS "forecasts" (
    "id" INTEGER PRIMARY KEY NOT NULL,
    "ds" TEXT,
    "asset_id" INTEGER,
    "var_name" INTEGER,
    "yhat" FLOAT,
    "yhat_lower" FLOAT,
    "yhat_upper" INTEGER,
    "ds_forecast" TEXT,
    "model_name" TEXT,
    FOREIGN KEY("asset_id") REFERENCES "assets"("asset_id")
);


CREATE TABLE IF NOT EXISTS "assets" (
    "asset_id" INTEGER PRIMARY KEY NOT NULL,
    "name" TEXT,
    "location_id" INTEGER,
    "type" TEXT,
    "price_tag_id" INTEGER,
    FOREIGN KEY("price_tag_id") REFERENCES "price_tag"("id")
);


CREATE TABLE IF NOT EXISTS "price" (
    "id" INTEGER PRIMARY KEY NOT NULL,
    "ds" INTEGER,
    "price_tag_id" INTEGER,
    "price" INTEGER,
    FOREIGN KEY("price_tag_id") REFERENCES "price_tag"("id")
);


CREATE TABLE IF NOT EXISTS "price_tag" (
    "id" INTEGER PRIMARY KEY NOT NULL,
    "country" TEXT,
    "type" TEXT
);


COMMIT;
