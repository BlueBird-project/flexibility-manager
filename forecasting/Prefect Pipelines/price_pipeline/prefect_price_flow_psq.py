"""Prefect flow: daily PriceFM+ forecast + real prices into PostgreSQL (no API).

  python prefect_price_flow_psq.py                 # serve with a daily schedule (default 13:00 UTC)
  python prefect_price_flow_psq.py --once          # run now, for tomorrow
  python prefect_price_flow_psq.py --once --date 20261106 --days 2

Same steps as price_inference_psq.py (Forge -> `price`, Translate -> `price_forecast`), as Prefect tasks.
Settings (environment): FORECAST_DB_URL, PRICEFM_WORKDIR, FLOW_CRON (default "0 13 * * *"),
FLOW_TIMEZONE (default UTC), PRICE_BACKFILL_DAYS, ENTSOE_API_TOKEN or ENTSOE_CREDENTIALS_FILE.
The PriceFM+ model needs actuals up to 12:00 UTC of the issue day, hence the 13:00 UTC schedule.
"""

import argparse
import json
import os
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path
from types import SimpleNamespace

from prefect import flow, get_run_logger, task
from prefect.artifacts import create_table_artifact

APP_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(APP_DIR))

import db_forecast_psq as db  # noqa: E402
import price_inference_psq as core  # noqa: E402

WORKDIR = os.environ.get("PRICEFM_WORKDIR", str(Path.home() / "PriceFM_Plus_forecast"))
CRON = os.environ.get("FLOW_CRON", "0 13 * * *")
TIMEZONE = os.environ.get("FLOW_TIMEZONE", "UTC")
BACKFILL_DAYS = core.BACKFILL_DAYS


def entsoe_key():
    if os.environ.get("ENTSOE_API_TOKEN"):
        return os.environ["ENTSOE_API_TOKEN"]
    path = os.environ.get("ENTSOE_CREDENTIALS_FILE")
    if path and Path(path).is_file():
        return json.loads(Path(path).read_text())["token"]
    return core.load_entsoe_key()


@task(retries=5, retry_delay_seconds=30)
def load_targets():
    """Opens the database (creating it, the tables and the tags if missing); returns the price tags."""
    conn = db.connect()
    try:
        return db.get_price_tags(conn)
    finally:
        conn.close()


@task(retries=2, retry_delay_seconds=300, timeout_seconds=6 * 3600)
def run_forge(day, regions, args):
    return core.run_forge(day, regions, args, entsoe_key())


@task(retries=1, retry_delay_seconds=60, timeout_seconds=6 * 3600)
def run_translate(processed, day, args):
    return core.run_translate(processed, day, args)


@task
def save_real_prices(processed, tags):
    conn = db.connect()
    try:
        return core.save_real_prices(conn, processed, tags)
    finally:
        conn.close()


@task
def save_forecasts(forecast, tags):
    conn = db.connect()
    try:
        return core.save_forecasts(conn, forecast, tags)
    finally:
        conn.close()


@task
def publish_forecast(forecast, day):
    table = forecast.assign(ds=forecast["ds"].astype(str)).round(3).to_dict("records")
    create_table_artifact(key=f"forecast-{day:%Y%m%d}", table=table,
                          description=f"PriceFM+ forecast for {day} (ds, unique_id, yhat, p10, p90)")


@flow(name="price-forecast-daily-psq", log_prints=True)
def price_forecast_flow(workdir: str = WORKDIR, first_day: str | None = None, days: int = 1,
                        regions: list[str] | None = None, backfill_days: int = BACKFILL_DAYS):
    """first_day 'YYYYMMDD' (UTC, default tomorrow); days = consecutive target days."""
    log = get_run_logger()
    tags = load_targets()
    if regions:
        unknown = [r for r in regions if r not in set(tags["region"])]
        if unknown:
            log.warning(f"Regions without a price tag are skipped: {unknown}")
        tags = tags[tags["region"].isin(regions)]
    if tags.empty:
        log.warning("No Intraday price tags with a mapped PriceFM zone; nothing to do")
        return
    regions = sorted(set(tags["region"]))
    first = (datetime.strptime(first_day, "%Y%m%d").date() if first_day
             else datetime.now(timezone.utc).date() + timedelta(days=1))
    workdir = str(Path(workdir).expanduser().resolve())
    Path(workdir).mkdir(parents=True, exist_ok=True)
    args = SimpleNamespace(workdir=workdir, backfill_days=backfill_days, history_days=10)

    failed = []
    for day in (first + timedelta(days=i) for i in range(days)):
        try:
            processed = run_forge(day, regions, args)
            n_pr = save_real_prices(processed, tags)
            forecast = run_translate(processed, day, args)
            publish_forecast(forecast, day)
            n_fc = save_forecasts(forecast, tags)
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
    parser.add_argument("--backfill-days", type=int, default=BACKFILL_DAYS,
                        help="days of real prices saved before each target day")
    parser.add_argument("--workdir", default=WORKDIR)
    parser.add_argument("--regions", "-r", nargs="+", default=None, help="zones to process (default: all tags)")
    args = parser.parse_args()

    if args.once:
        price_forecast_flow(workdir=args.workdir, first_day=args.date, days=args.days,
                            regions=args.regions, backfill_days=args.backfill_days)
    else:
        from prefect.schedules import Cron
        price_forecast_flow.serve(
            name="price-forecast-daily-psq", schedule=Cron(CRON, timezone=TIMEZONE),
            parameters={"workdir": args.workdir, "regions": args.regions,
                        "backfill_days": args.backfill_days})
