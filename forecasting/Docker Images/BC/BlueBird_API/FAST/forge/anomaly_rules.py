class BaseForge:

    name = "base_forge"

    def process(self, data, config_process=None):
        raise NotImplementedError


class AnomalyRules(BaseForge):
    """Use-case specific, configurable anomaly handling (extends positive_pv / anomaly_interpolation).

    config_process["rules"] is an ordered list of {type: ..., params}. Rule types
    (method `_rule_<type>`, add your own by adding a method or subclassing):
      night_zero        PV: value -> 0 when the sun is below `min_elevation_deg`
                        (needs asset coords + tz; `window_minutes` margin around sunrise/sunset)
      negative_to_zero  values < 0 -> 0
      negative_to_nan   values < 0 -> NaN (e.g. meter resets), to be interpolated later
      clip_min          values < value -> value
      clip_max_quantile values > factor * quantile(q) -> NaN
      tod_outlier       robust z-score vs the same time-of-day (median/MAD) > k -> NaN
      rolling_spike     value > factor * rolling median (window_days) + min_abs -> NaN
      interpolate_nan   linear interpolation of NaN (optional `limit`, in samples)
    Remaining NaN after the chain are left for interpolate_15min / later rules.
    """

    name = "anomaly_rules"

    # -------- helpers --------
    def _sun_elevation(self, ds_local, lat, lon, tz):
        """NOAA solar elevation (degrees) for naive LOCAL timestamps."""
        import numpy as np
        import pandas as pd

        ts = pd.DatetimeIndex(ds_local).tz_localize(
            tz, ambiguous=True, nonexistent="shift_forward"
        ).tz_convert("UTC")
        doy = ts.dayofyear.values
        hour = ts.hour.values + ts.minute.values / 60.0 + ts.second.values / 3600.0
        g = 2 * np.pi / 365.0 * (doy - 1 + (hour - 12) / 24.0)
        eqtime = 229.18 * (0.000075 + 0.001868 * np.cos(g) - 0.032077 * np.sin(g)
                           - 0.014615 * np.cos(2 * g) - 0.040849 * np.sin(2 * g))
        decl = (0.006918 - 0.399912 * np.cos(g) + 0.070257 * np.sin(g) - 0.006758 * np.cos(2 * g)
                + 0.000907 * np.sin(2 * g) - 0.002697 * np.cos(3 * g) + 0.00148 * np.sin(3 * g))
        tst = hour * 60.0 + eqtime + 4.0 * lon
        ha = np.radians(tst / 4.0 - 180.0)
        lat_r = np.radians(lat)
        cos_zen = np.sin(lat_r) * np.sin(decl) + np.cos(lat_r) * np.cos(decl) * np.cos(ha)
        return 90.0 - np.degrees(np.arccos(np.clip(cos_zen, -1, 1)))

    # -------- rules (each returns the modified series y and a flagged count) --------
    def _rule_night_zero(self, df, y, p, asset):
        import numpy as np
        import pandas as pd

        coords = asset.get("coords")
        if not coords:
            print("[FAST - forge] anomaly_rules: night_zero skipped (no coordinates)")
            return y, 0
        lat, lon = coords
        tz = asset.get("tz", "UTC")
        thr = p.get("min_elevation_deg", -0.5)
        win = pd.Timedelta(minutes=p.get("window_minutes", 30))
        ds = pd.DatetimeIndex(df["ds"])
        elev = np.maximum.reduce([
            self._sun_elevation(ds - win, lat, lon, tz),
            self._sun_elevation(ds, lat, lon, tz),
            self._sun_elevation(ds + win, lat, lon, tz),
        ])
        night = elev < thr
        flagged = int((night & (y.fillna(0).values != 0)).sum())
        y = y.copy()
        y[night] = 0.0
        return y, flagged

    def _rule_negative_to_zero(self, df, y, p, asset):
        mask = y < 0
        y = y.copy()
        y[mask] = 0.0
        return y, int(mask.sum())

    def _rule_negative_to_nan(self, df, y, p, asset):
        import numpy as np

        mask = y < 0
        y = y.copy()
        y[mask] = np.nan
        return y, int(mask.sum())

    def _rule_clip_min(self, df, y, p, asset):
        v = p.get("value", 0.0)
        mask = y < v
        return y.clip(lower=v), int(mask.sum())

    def _rule_clip_max_quantile(self, df, y, p, asset):
        import numpy as np

        cap = y.quantile(p.get("quantile", 0.999)) * p.get("factor", 1.5)
        mask = y > cap
        y = y.copy()
        y[mask] = np.nan
        return y, int(mask.sum())

    def _rule_tod_outlier(self, df, y, p, asset):
        import numpy as np
        import pandas as pd

        k = p.get("k", 6.0)
        tod = df["ds"].dt.hour * 60 + df["ds"].dt.minute
        med = y.groupby(tod).transform("median")
        mad = (y - med).abs().groupby(tod).transform("median") * 1.4826
        floor = p.get("min_scale_frac", 0.1) * y.quantile(0.95)
        scale = np.maximum(mad, floor if np.isfinite(floor) else 0.0) + 1e-9
        mask = ((y - med).abs() / scale) > k
        y = y.copy()
        y[mask] = np.nan
        return y, int(mask.sum())

    def _rule_rolling_spike(self, df, y, p, asset):
        import numpy as np
        import pandas as pd

        step = df["ds"].diff().median()
        window = max(3, int(pd.Timedelta(days=p.get("window_days", 7)) / step))
        med = y.rolling(window, center=True, min_periods=3).median()
        mask = y > (p.get("factor", 3.0) * med + p.get("min_abs", 0.0))
        y = y.copy()
        y[mask] = np.nan
        return y, int(mask.sum())

    def _rule_interpolate_nan(self, df, y, p, asset):
        n = int(y.isna().sum())
        y = y.interpolate(method=p.get("method", "linear"), limit=p.get("limit"), limit_direction="both")
        return y, n

    # -------- entry point --------
    def process(self, data, config_process=None):
        import pandas as pd

        config_process = config_process or {}
        asset = config_process.get("_asset", {})
        rules = config_process.get("rules", [])
        out = []
        for uid, g in data.sort_values(["unique_id", "ds"]).groupby("unique_id", sort=False):
            g = g.reset_index(drop=True)
            y = g["y"].astype(float)
            for rule in rules:
                params = {k: v for k, v in rule.items() if k != "type"}
                fn = getattr(self, f"_rule_{rule['type']}", None)
                if fn is None:
                    raise ValueError(f"Unknown anomaly rule: {rule['type']}")
                y, n = fn(g, y, params, asset)
                print(f"[FAST - forge] anomaly_rules[{uid}] {rule['type']}: {n} values affected")
            g["y"] = y
            out.append(g)
        return pd.concat(out, ignore_index=True)


import dill

forge = AnomalyRules()

with open("forge_anomaly_rules.dill", "wb") as f:
    dill.dump(forge, f)
