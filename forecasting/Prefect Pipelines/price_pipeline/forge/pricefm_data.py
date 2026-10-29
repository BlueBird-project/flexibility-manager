class PriceFMDataForge:
    """FAST Forge: prepares the data PriceFM+ needs for a day-ahead price forecast.

    Downloads the released processed dataset (first run only), extends it up to the
    target UTC day with ENTSO-E (all PriceFM+ inputs for the needed bidding zones),
    and returns the input frame enriched with:
      - ``region``: PriceFM bidding zone of each ``unique_id``
      - ``y``: observed day-ahead price (EUR/MWh) where available
      - ``target_date``: UTC day to forecast (future frame only)

    config_process keys (all optional):
      workdir          folder holding ./Data and ./Weights (default ~/PriceFM_Plus_forecast)
      entsoe_api_key   ENTSO-E key (default: env ENTSOE_API_TOKEN)
      site_region      {unique_id: bidding zone}; ids not listed are used as the zone
      forecast_date    "YYYY-MM-DD" UTC target day (default: day after the history ends)
      history_days     days refreshed before the issue day (default 10, 7 are required)
      refresh_minutes  skip ENTSO-E sync if the same request ran this recently (default 30)
    """

    name = "pricefm_data"

    def _setup(self, config_process):
        import os
        from pathlib import Path

        workdir = Path(config_process.get(
            "workdir", Path.home() / "PriceFM_Plus_forecast")).expanduser()
        workdir.mkdir(parents=True, exist_ok=True)
        # PriceFM_Plus resolves ./Data against the working directory at import time.
        os.chdir(workdir)
        return workdir

    def _target_date(self, data, config_process):
        import pandas as pd

        if config_process.get("forecast_date"):
            return pd.Timestamp(config_process["forecast_date"]).date()
        if "target_date" in data.columns and data["target_date"].notna().any():
            return pd.Timestamp(data["target_date"].dropna().iloc[0]).date()
        last = pd.to_datetime(data.loc[data["y"].notna(), "ds"]).max() \
            if "y" in data.columns and data["y"].notna().any() \
            else pd.to_datetime(data["ds"]).min()
        return (last + pd.Timedelta(minutes=15)).date()

    def _zone_map(self, data, config_process):
        site_region = config_process.get("site_region", {})
        return {uid: site_region.get(uid, uid) for uid in data["unique_id"].unique()}

    def _sync(self, regions, target_date, config_process):
        import os
        import json
        from datetime import datetime, timedelta
        from pathlib import Path
        import pandas as pd

        import PriceFM_Plus
        from PriceFM_Plus.data_pipeline.core import ALL_FEATURES
        from PriceFM_Plus.data_pipeline.sync import sync_dataset

        if not Path("Data/Config/active_regions.json").is_file():
            PriceFM_Plus.download_data("processed")
        active = json.loads(Path("Data/Config/active_regions.json").read_text())
        unknown = [r for r in regions if r not in active]
        if unknown:
            raise ValueError(f"Unknown PriceFM regions {unknown}; active: {active}")

        stamp = Path(f".forge_sync_{target_date}.json")
        refresh = float(config_process.get("refresh_minutes", 30))
        if stamp.is_file():
            previous = json.loads(stamp.read_text())
            age = (datetime.now() - datetime.fromisoformat(previous["at"])).total_seconds() / 60
            if previous["regions"] == sorted(regions) and age < refresh:
                return

        key = config_process.get("entsoe_api_key") or os.environ.get("ENTSOE_API_TOKEN")
        if not key:
            raise ValueError("ENTSO-E API key required (config 'entsoe_api_key' or env ENTSOE_API_TOKEN)")
        os.environ["ENTSOE_API_TOKEN"] = key

        frame = pd.read_csv(
            f"Data/Processed/per_region_feature_interp/{regions[0]}_DayAheadPrice.csv",
            usecols=["timestamp_utc"])
        last_day = pd.Timestamp(frame["timestamp_utc"].iloc[-1]).date()
        issue_day = target_date - timedelta(days=1)
        start = min(last_day, issue_day) - timedelta(days=int(config_process.get("history_days", 10)))
        for fill in ("copy", "interp", "none"):
            result = sync_dataset(regions, start.isoformat(), target_date.isoformat(),
                                  ALL_FEATURES, fill=fill, download=fill == "copy")
            print(f"[PriceFMDataForge] sync {fill}: "
                  f"{ {k: v for k, v in result.items() if k != 'download'} }")
        stamp.write_text(json.dumps({"at": datetime.now().isoformat(),
                                     "regions": sorted(regions)}))

    def _observed_prices(self, regions):
        import pandas as pd

        frames = []
        for region in regions:
            f = pd.read_csv(
                f"Data/Processed/per_region_feature_interp/{region}_DayAheadPrice.csv",
                usecols=["timestamp_utc", "value", "source_observed"])
            f = f[f["source_observed"].astype(bool)]
            frames.append(pd.DataFrame({
                "region": region,
                "ds": pd.to_datetime(f["timestamp_utc"], utc=True).dt.tz_localize(None),
                "price": f["value"].to_numpy()}))
        return pd.concat(frames, ignore_index=True)

    def process(self, data, config_process: dict):
        import pandas as pd

        self._setup(config_process)
        data = data.copy()
        data["ds"] = pd.to_datetime(data["ds"])
        if "y" not in data.columns:
            data["y"] = None

        zones = self._zone_map(data, config_process)
        regions = sorted(set(zones.values()))
        target_date = self._target_date(data, config_process)
        self._sync(regions, target_date, config_process)

        data["region"] = data["unique_id"].map(zones)
        observed = self._observed_prices(regions)
        data = data.merge(observed, on=["region", "ds"], how="left")
        data["y"] = pd.to_numeric(data["y"], errors="coerce").fillna(data["price"])
        data = data.drop(columns="price")
        data["target_date"] = pd.Timestamp(target_date)
        return data


import dill

forge = PriceFMDataForge()

with open("forge_pricefm_data.dill", "wb") as f:
    dill.dump(forge, f)
