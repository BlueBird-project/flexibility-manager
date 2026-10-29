import pandas as pd


class BaseTranslate:

    name = "base_translate"

    def load(self, model_path: str):
        raise NotImplementedError

    def predict(self, data):
        raise NotImplementedError


class PriceFMPlusTranslate(BaseTranslate):
    """FAST Translate: PriceFM+ (Realistic case) day-ahead forecast of one UTC day.

    ``model_path`` is the work directory holding ./Data (prepared by the Forge) and
    ./Weights (downloaded here on first use). Returns 96 quarter-hours per unique_id
    with ``yhat`` (median) plus ``p10`` / ``p90`` in EUR/MWh; ds is naive UTC.
    """

    MODEL, CASE, FOLD = "PriceFM_Plus", "Realistic", 3

    def __init__(self):
        pass

    def load(self, model_path: str = None):
        import os
        import gzip
        import json
        from pathlib import Path
        from importlib.resources import files

        workdir = Path(model_path or Path.home() / "PriceFM_Plus_forecast").expanduser()
        workdir.mkdir(parents=True, exist_ok=True)
        # PriceFM_Plus resolves ./Data and ./Weights against the cwd at import time.
        os.chdir(workdir)

        import PriceFM_Plus
        if not Path("Data/Config/active_regions.json").is_file():
            PriceFM_Plus.download_data("processed")
        if not (Path("Weights") / self.MODEL).is_dir():
            PriceFM_Plus.download_weights(self.MODEL)

        from PriceFM_Plus.PriceFM_Plus import build_model
        from PriceFM_Plus.region_connectivity import load_connectivity

        meta = json.loads(gzip.decompress(
            files("PriceFM_Plus").joinpath("inference_metadata.json.gz").read_bytes()))
        entry = meta[self.MODEL][self.CASE][str(self.FOLD)]
        self.protocol, self.scalers = entry["protocol"], entry["scalers"]
        self.model_regions, self.adjacency = load_connectivity()
        if self.protocol["model_regions"] != list(self.model_regions):
            raise ValueError("Weight topology differs from the downloaded data")
        p = self.protocol
        self.network = build_model(
            self.model_regions, loops=int(p["loops"]), hidden_width=int(p["hidden_width"]),
            extra_depth=bool(p["extra_depth"]), temporal_mix=bool(p["temporal_mix"]),
            recent_price_context=bool(p["recent_price_context"]),
            recent_price_sequence=bool(p.get("recent_price_sequence", False)),
            branch_dense_layers=int(p["branch_dense_layers"]),
            head_dense_layers=int(p["head_dense_layers"]))
        self.weights = Path("Weights") / self.MODEL / self.CASE / "Joint" / "L2" / f"fold{self.FOLD}"

    def _target_date(self, data, future_df):
        import pandas as pd

        if "target_date" in future_df.columns and future_df["target_date"].notna().any():
            return pd.Timestamp(future_df["target_date"].dropna().iloc[0]).normalize()
        last = pd.to_datetime(data["ds"]).max()
        return (last + pd.Timedelta(minutes=15)).normalize()

    def _forecast_zone(self, region, day):
        import numpy as np
        import pandas as pd
        import tensorflow as tf
        from PriceFM_Plus import data as D
        from PriceFM_Plus._inference import INPUT_KEYS

        p = self.protocol
        D.set_variant(p["variant"])
        D.SPLITS = dict(p["split"])
        D.TARGET_DATES = pd.date_range(day, day, freq="D", tz="UTC")
        D.SPLIT_SCHEME = f"infer_{day.date()}"
        D.SCALER_MODE = p["scaler_mode"]
        D.GLOBAL_SCALERS = (dict(self.scalers[p["regions"][0]])
                            if D.SCALER_MODE != "regional_robust" else {})
        D._refresh_cache()

        batch, _, scalers, _ = D.build_case_region(region, p["case"], self.model_regions)
        for name, ref in self.scalers[region].items():
            cur = scalers[name]
            if not (np.isclose(cur["center"], ref["center"], rtol=1e-6, atol=1e-5) and
                    np.isclose(cur["scale"], ref["scale"], rtol=1e-6, atol=1e-5)):
                raise ValueError(f"Saved scaler mismatch for {region} {name}")
        batch["region_code"] = np.broadcast_to(
            self.adjacency[self.model_regions.index(region)], (1, len(self.model_regions))).copy()
        coverage = float(batch["lead_mask"].mean())
        if coverage < 0.5:
            print(f"[PriceFMPlusTranslate] WARNING {region}: only {coverage:.0%} of the "
                  f"day-ahead features for {day.date()} are available")

        self.network.load_weights(self.weights / f"best_{region}.weights.h5")
        out = self.network({k: tf.convert_to_tensor(batch[k]) for k in INPUT_KEYS},
                           training=False).numpy()[0]
        price = self.scalers[region]["DayAheadPrice"]
        return out * price["scale"] + price["center"]  # (96, 3): p10, p50, p90

    def predict(
        self,
        data: pd.DataFrame,
        horizon: int,
        future_df: pd.DataFrame
    ) -> pd.DataFrame:
        import pandas as pd

        day = self._target_date(data, future_df)
        if horizon != 96:
            print(f"[PriceFMPlusTranslate] horizon={horizon} ignored: PriceFM+ forecasts "
                  f"one full UTC day (96 quarter-hours)")

        zones = (data[["unique_id", "region"]].drop_duplicates()
                 if "region" in data.columns
                 else data[["unique_id"]].drop_duplicates().assign(
                     region=lambda d: d["unique_id"]))
        ds = pd.date_range(day, periods=96, freq="15min")

        cache, frames = {}, []
        for uid, region in zones.itertuples(index=False):
            if region not in cache:
                cache[region] = self._forecast_zone(region, day.tz_localize("UTC")
                                                    if day.tzinfo is None else day)
            out = cache[region]
            frames.append(pd.DataFrame({
                "unique_id": uid, "ds": ds,
                "yhat": out[:, 1], "p10": out[:, 0], "p90": out[:, 2]}))
        return pd.concat(frames, ignore_index=True)


import dill

translate = PriceFMPlusTranslate()

with open("translate_pricefm_plus.dill", "wb") as f:
    dill.dump(translate, f)
