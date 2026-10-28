"""Command-line price forecasts with PriceFM+ through the FAST Forge/Translate pipeline.

  python price_inference.py -r BE ES -m production
  python price_inference.py -r BE ES -m backtest -d 20260201 20260202

Regions are PriceFM bidding zones (e.g. BE, ES, PL, AT, DE_LU). Production forecasts the
next UTC day; backtest forecasts every UTC day from the first to the last date (inclusive).
The result is a CSV (ds, region_name, yhat) saved in the folder where the command is run.
Production accepts -n DAYS to forecast several consecutive days starting tomorrow.
ENTSO-E key: "token" in credentials_entsoe.json (current folder, else next to this script).
"""

import argparse
import os, json
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

import pandas as pd

APP_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(APP_DIR))

from fast_inference import ModelInference  # noqa: E402

FORGES = ["pricefm_data"]
TRANSLATES = ["pricefm_plus"]
HORIZON = 96


def seed_frame(regions, day):
    """Minimal history frame (last hour of the issue day); the Forge fills in the real data."""
    ds = pd.date_range(pd.Timestamp(day) - pd.Timedelta(minutes=45), periods=4, freq="15min")
    return pd.DataFrame([{"unique_id": r, "ds": t, "y": None} for r in regions for t in ds])


def forecast_day(day, regions, args, entsoe_key):
    config_forge = {
        "workdir": args.workdir,
        "forecast_date": day.isoformat(),
        "entsoe_api_key": entsoe_key,
        "history_days": args.history_days,
    }
    pipeline = ModelInference(
        forges=FORGES,
        translate=TRANSLATES,
        config_forges=[config_forge],
        config_translate=[{"model_path": args.workdir}],
        horizon=HORIZON,
    )
    forecast = pipeline.run(
        data=seed_frame(regions, pd.Timestamp(day)),
        cutoff_date=None,
        path_fast=str(APP_DIR),
    )
    return pd.DataFrame({
        "ds": pd.to_datetime(forecast["ds"]),
        "region_name": forecast["unique_id"],
        "yhat": forecast["yhat"],
    })


def load_entsoe_key():
    for folder in (Path.cwd(), APP_DIR):
        path = folder / "credentials_entsoe.json"
        if path.is_file():
            with open(path) as file:
                return json.load(file)["token"]
    raise FileNotFoundError("credentials_entsoe.json not found (current folder or next to the script)")


def parse_day(text):
    try:
        return datetime.strptime(text, "%Y%m%d").date()
    except ValueError:
        raise SystemExit(f"Invalid date '{text}': use YYYYMMDD")


def main():
    parser = argparse.ArgumentParser(description="PriceFM+ price forecasts (FAST Forge/Translate)")
    parser.add_argument("-r", "--regions", nargs="+", required=True, help="PriceFM regions, e.g. BE ES PL")
    parser.add_argument("-m", "--mode", choices=("production", "backtest"), default="production")
    parser.add_argument("-d", "--dates", nargs=2, metavar=("START", "END"), help="backtest range, YYYYMMDD")
    parser.add_argument("-n", "--days", type=int, default=1, help="production: number of days to forecast")
    parser.add_argument("--workdir", default=str(Path.home() / "PriceFM_Plus_forecast"),
                        help="folder for the PriceFM data and weights (large, reused between runs)")
    parser.add_argument("--history_days", type=int, default=10)
    args = parser.parse_args()

    ENTSOE_API_KEY = load_entsoe_key()
    if args.days < 1:
        raise SystemExit("-n must be at least 1")
    args.workdir = str(Path(args.workdir).expanduser().resolve())
    Path(args.workdir).mkdir(parents=True, exist_ok=True)
    regions = list(dict.fromkeys(args.regions))
    # The Forge/Translate change the working directory, so fix the output folder now.
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

    frames, failed = [], []
    for day in days:
        print(f"== Forecast for {day} ({', '.join(regions)})", flush=True)
        try:
            frames.append(forecast_day(day, regions, args, ENTSOE_API_KEY))
        except Exception as exc:
            failed.append(day)
            print(f"Forecast failed for {day}: {exc!r}", flush=True)
            if args.mode == "production":
                raise

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
