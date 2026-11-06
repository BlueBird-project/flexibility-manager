"""Price forecasts with PriceFM+ saved in PostgreSQL (same CLI as price_inference.py).

  python price_inference_psq.py -r BE ES -m production
  python price_inference_psq.py -r BE ES -m backtest -d 20260201 20260202

For each target day: checks that the database exists (creates it, the tables and the price tags if not),
runs the Forge (downloads the data), saves the observed prices in `price`, runs the Translate and saves
the forecast in `price_forecast`. A CSV (ds, region_name, yhat) is also written to the current folder.
Connection: FORECAST_DB_URL or --db-url, e.g. postgresql://user:password@localhost:5432/pricefm
(the role needs CREATEDB to create the database). Reruns are idempotent (upserts).
"""

import argparse
import os
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd

APP_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(APP_DIR))

import db_forecast_psq as db  # noqa: E402
from fast_inference import ClassBuilder  # noqa: E402
from price_inference import FORGES, HORIZON, TRANSLATES, load_entsoe_key, parse_day  # noqa: E402

BACKFILL_DAYS = int(os.environ.get("PRICE_BACKFILL_DAYS", 3))


def input_frame(regions, day, backfill_days):
    """Forge input: empty quarter-hour grid per zone (backfill days + target day); y is unknown."""
    ds = pd.date_range(pd.Timestamp(day) - pd.Timedelta(days=backfill_days),
                       pd.Timestamp(day) + pd.Timedelta(days=1), freq="15min", inclusive="left")
    return pd.DataFrame([{"unique_id": r, "ds": t, "y": None} for r in regions for t in ds])


def run_forge(day, regions, args, entsoe_key):
    forge = ClassBuilder(str(APP_DIR), FORGES[0], "forge")
    return forge.process(input_frame(regions, day, args.backfill_days), config_process={
        "workdir": args.workdir, "forecast_date": day.isoformat(),
        "entsoe_api_key": entsoe_key, "history_days": max(args.history_days, args.backfill_days + 7)})


def run_translate(processed, day, args):
    start = pd.Timestamp(day)
    translate = ClassBuilder(str(APP_DIR), TRANSLATES[0], "translate")
    translate.load(model_path=args.workdir)
    forecast = translate.predict(data=processed[processed["ds"] < start], horizon=HORIZON,
                                 future_df=processed[processed["ds"] >= start])
    if forecast["yhat"].isna().all():
        raise RuntimeError(f"Empty forecast for {day}")
    return forecast


def save_real_prices(conn, processed, tags):
    observed = processed.dropna(subset=["y"]).rename(columns={"y": "price", "region": "tag_region"})
    observed = observed.merge(tags[["price_tag_id", "region"]], left_on="tag_region", right_on="region")
    return db.save_prices(conn, observed[["price_tag_id", "ds", "price"]])


def save_forecasts(conn, forecast, tags):
    frame = forecast.rename(columns={"unique_id": "region", "p10": "yhat_lower", "p90": "yhat_upper"})
    for column in ("yhat_lower", "yhat_upper"):
        if column not in frame:
            frame[column] = None
    frame = frame.merge(tags[["price_tag_id", "region"]], on="region")
    return db.save_price_forecasts(conn, frame)


def forecast_day(conn, day, regions, tags, args, entsoe_key):
    processed = run_forge(day, regions, args, entsoe_key)
    print(f"   prices saved: {save_real_prices(conn, processed, tags)}", flush=True)
    forecast = run_translate(processed, day, args)
    print(f"   forecast rows saved: {save_forecasts(conn, forecast, tags)}", flush=True)
    return pd.DataFrame({"ds": pd.to_datetime(forecast["ds"]),
                         "region_name": forecast["unique_id"], "yhat": forecast["yhat"]})


def main():
    parser = argparse.ArgumentParser(description="PriceFM+ price forecasts saved in PostgreSQL")
    parser.add_argument("-r", "--regions", nargs="+", required=True, help="PriceFM regions, e.g. BE ES PL")
    parser.add_argument("-m", "--mode", choices=("production", "backtest"), default="production")
    parser.add_argument("-d", "--dates", nargs=2, metavar=("START", "END"), help="backtest range, YYYYMMDD")
    parser.add_argument("-n", "--days", type=int, default=1, help="production: number of days to forecast")
    parser.add_argument("--workdir", default=str(Path.home() / "PriceFM_Plus_forecast"),
                        help="folder for the PriceFM data and weights (large, reused between runs)")
    parser.add_argument("--history_days", type=int, default=10)
    parser.add_argument("--backfill-days", type=int, default=BACKFILL_DAYS,
                        help="days of real prices saved before the target day")
    parser.add_argument("--db-url", default=None, help="PostgreSQL URL (default: env FORECAST_DB_URL)")
    args = parser.parse_args()

    if args.days < 1:
        raise SystemExit("-n must be at least 1")
    entsoe_key = load_entsoe_key()
    args.workdir = str(Path(args.workdir).expanduser().resolve())
    Path(args.workdir).mkdir(parents=True, exist_ok=True)
    regions = list(dict.fromkeys(args.regions))
    out_dir = Path.cwd()

    if args.mode == "production":
        first = datetime.now(timezone.utc).date() + timedelta(days=1)
        days = [first + timedelta(days=i) for i in range(args.days)]
        out_name = f"forecast_production_{days[0]:%Y%m%d}.csv" if args.days == 1 else \
            f"forecast_production_{days[0]:%Y%m%d}_{days[-1]:%Y%m%d}.csv"
    else:
        if not args.dates:
            raise SystemExit("backtest mode needs -d START END (YYYYMMDD YYYYMMDD)")
        start, end = parse_day(args.dates[0]), parse_day(args.dates[1])
        if end < start:
            raise SystemExit("END date is before START date")
        days = [start + timedelta(days=i) for i in range((end - start).days + 1)]
        out_name = f"forecast_backtest_{start:%Y%m%d}_{end:%Y%m%d}.csv"

    # Checks the database and creates it (tables + price tags) when missing.
    conn = db.connect(args.db_url)
    try:
        tags = db.get_price_tags(conn)
        tags = tags[tags["region"].isin(regions)]
        missing = sorted(set(regions) - set(tags["region"]))
        if missing:
            raise SystemExit(f"No price_tag for regions {missing}; add them to price_tag first")

        frames, failed = [], []
        for day in days:
            print(f"== Forecast for {day} ({', '.join(regions)})", flush=True)
            try:
                frames.append(forecast_day(conn, day, regions, tags, args, entsoe_key))
            except Exception as exc:
                conn.rollback()
                failed.append(day)
                print(f"Forecast failed for {day}: {exc!r}", flush=True)
                if args.mode == "production":
                    raise
    finally:
        conn.close()

    if not frames:
        raise SystemExit("No forecasts were produced")
    result = pd.concat(frames, ignore_index=True).sort_values(["region_name", "ds"])
    path = out_dir / out_name
    result.to_csv(path, index=False)
    print(f"Saved {len(result)} rows to {path}")
    if failed:
        print("Days without forecast:", ", ".join(map(str, failed)))


if __name__ == "__main__":
    main()
