"""Prefect flow: daily PriceFM+ forecast + real prices into the SQLite database (no API).

  pip install prefect
  python prefect_price_flow.py                 # serve with a daily schedule (13:00 UTC)
  python prefect_price_flow.py --once          # run now, for tomorrow
  python prefect_price_flow.py --once --date 20261029 --days 2

FAST modules stay data-to-data; saving is a separate step (db_forecast.py). Per target day:
  1. Forge execution (pricefm_data): input frame (no real input) -> processed frame with the
     observed prices in `y`;                                      -> saved in `price`
  2. Translate (pricefm_plus): processed frame -> forecast (ds, unique_id, yhat, p10, p90)
                                                                  -> saved in `price_forecast`
Zones come from the price_tags (type 'Intraday...') whose country maps to a PriceFM zone;
forecasts go to table price_forecast (per price_tag).

Settings (environment): FORECAST_DB (default db_forecast_test.db), PRICEFM_WORKDIR,
PRICE_TAG_TYPE, FORECAST_MODEL_NAME, COUNTRY_REGION_MAP, FLOW_CRON.
The PriceFM+ model uses actuals up to 12:00 UTC of the issue day, hence the 13:00 UTC schedule.
"""

import argparse
import os
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd
from prefect import flow, get_run_logger, task
from prefect.artifacts import create_table_artifact

APP_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(APP_DIR))

import db_forecast as db  # noqa: E402
import price_inference as core  # noqa: E402
from fast_inference import ClassBuilder  # noqa: E402

DB_PATH = os.environ.get("FORECAST_DB", str(APP_DIR / "db_forecast_test.db"))
WORKDIR = os.environ.get("PRICEFM_WORKDIR", str(Path.home() / "PriceFM_Plus_forecast"))
CRON = os.environ.get("FLOW_CRON", "0 13 * * *")
PRICE_BACKFILL_DAYS = int(os.environ.get("PRICE_BACKFILL_DAYS", 3))


@task
def load_targets(db_path: str):
    conn = db.connect(db_path)
    try:
        return db.get_price_tags(conn)
    finally:
        conn.close()


def input_frame(regions, day, backfill_days=PRICE_BACKFILL_DAYS):
    """Forge input: empty quarter-hour grid per zone (backfill days + target day); y is unknown."""
    ds = pd.date_range(pd.Timestamp(day) - pd.Timedelta(days=backfill_days),
                       pd.Timestamp(day) + pd.Timedelta(days=1), freq="15min", inclusive="left")
    return pd.DataFrame([{"unique_id": r, "ds": t, "y": None} for r in regions for t in ds])


@task(retries=2, retry_delay_seconds=300, timeout_seconds=6 * 3600)
def run_forge(day, regions, workdir: str, backfill_days: int = PRICE_BACKFILL_DAYS):
    """Forge execution: downloads/updates the data; returns the frame with observed prices in y."""
    Path(workdir).mkdir(parents=True, exist_ok=True)
    forge = ClassBuilder(str(APP_DIR), core.FORGES[0], "forge")
    return forge.process(input_frame(regions, day, backfill_days), config_process={
        "workdir": workdir, "forecast_date": day.isoformat(),
        "entsoe_api_key": core.load_entsoe_key(), "history_days": max(10, backfill_days + 7)})

@task(retries=1, retry_delay_seconds=60, timeout_seconds=6 * 3600)
def run_translate(processed, day, workdir: str):
    """Translate: forecast of the target day from the Forge output."""
    start = pd.Timestamp(day)
    history = processed[processed["ds"] < start]
    future = processed[processed["ds"] >= start]
    translate = ClassBuilder(str(APP_DIR), core.TRANSLATES[0], "translate")
    translate.load(model_path=workdir)
    forecast = translate.predict(data=history, horizon=core.HORIZON, future_df=future)
    if forecast["yhat"].isna().all():
        raise RuntimeError(f"Empty forecast for {day}")
    return forecast

@task
def save_real_prices(db_path: str, processed, tags):
    """Observed prices (Forge output, y) -> price table, per price_tag_id."""
    observed = processed.dropna(subset=["y"]).rename(columns={"y": "price", "region": "tag_region"})
    observed = observed.merge(tags[["price_tag_id", "region"]], left_on="tag_region", right_on="region")
    conn = db.connect(db_path)
    try:
        return db.save_prices(conn, observed[["price_tag_id", "ds", "price"]])
    finally:
        conn.close()

@task
def save_forecasts(db_path: str, forecast, tags):
    """Translate output -> price_forecast table, one set of rows per price_tag (country)."""
    frame = forecast.rename(columns={"unique_id": "region", "p10": "yhat_lower", "p90": "yhat_upper"})
    for column in ("yhat_lower", "yhat_upper"):
        if column not in frame:
            frame[column] = None
    frame = frame.merge(tags[["price_tag_id", "region"]], on="region")
    conn = db.connect(db_path)
    try:
        return db.save_price_forecasts(conn, frame)
    finally:
        conn.close()

@task
def publish_forecast(forecast, day):
    """Forecast as a Prefect table artifact (key forecast-YYYYMMDD)."""
    table = forecast.assign(ds=forecast["ds"].astype(str)).round(3).to_dict("records")
    create_table_artifact(key=f"forecast-{day:%Y%m%d}", table=table,
                          description=f"PriceFM+ forecast for {day} (ds, unique_id, yhat, p10, p90)")


@flow(name="price-forecast-daily", log_prints=True)
def price_forecast_flow(db_path: str = DB_PATH, workdir: str = WORKDIR,
                        first_day: str | None = None, days: int = 1, regions: list[str] | None = None,
                        backfill_days: int = PRICE_BACKFILL_DAYS):
    """first_day 'YYYYMMDD' (UTC, default tomorrow); days = consecutive target days."""
    log = get_run_logger()
    tags = load_targets(db_path)
    if tags.empty:
        log.warning("No Intraday price tags with a mapped PriceFM zone; nothing to do")
        return
    if regions is None:
        regions = sorted(set(tags["region"]))
    else:
        for region in regions:
            if region in tags["region"].values:
                continue
            else:
                regions.remove(region)
                log.warning(f"Region {region} has no corresponding price tag")
    first = (datetime.strptime(first_day, "%Y%m%d").date() if first_day
             else datetime.now(timezone.utc).date() + timedelta(days=1))
    target_days = [first + timedelta(days=i) for i in range(days)]
    workdir = str(Path(workdir).expanduser().resolve())
    db_path = str(Path(db_path).expanduser().resolve())

    failed = []
    for day in target_days:
        try:
            processed = run_forge(day, regions, workdir, backfill_days)
            n_pr = save_real_prices(db_path, processed, tags)
            forecast = run_translate(processed, day, workdir)
            publish_forecast(forecast, day)
            n_fc = save_forecasts(db_path, forecast, tags)
            log.info(f"{day}: saved {n_fc} forecast rows and {n_pr} price rows ({len(regions)} zones)")
        except Exception as exc:
            failed.append(day)
            log.error(f"{day} failed: {exc!r}")
    if failed:
        raise RuntimeError(f"Failed days: {', '.join(map(str, failed))}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--once", action="store_true", help="run now instead of serving the schedule")
    parser.add_argument("--date", help="first target day YYYYMMDD (default tomorrow)")
    parser.add_argument("--days", type=int, default=1)
    parser.add_argument("--backfill-days", type=int, default=PRICE_BACKFILL_DAYS,
                        help="days of real prices saved before each target day")
    parser.add_argument("--db", default=DB_PATH)
    parser.add_argument("--workdir", default=WORKDIR)
    parser.add_argument("--regions", "-r", nargs="+", help="list of regions to process", default=[])
    args = parser.parse_args()

    if len(args.regions) == 0:
        args.regions = None

    if args.once:
        price_forecast_flow(db_path=args.db, workdir=args.workdir, first_day=args.date, days=args.days, regions=args.regions,
                            backfill_days=args.backfill_days)
    else:
        price_forecast_flow.serve(name="price-forecast-daily", cron=CRON,
                                  parameters={"db_path": args.db, "workdir": args.workdir, "regions": args.regions,
                                              "backfill_days": args.backfill_days})
