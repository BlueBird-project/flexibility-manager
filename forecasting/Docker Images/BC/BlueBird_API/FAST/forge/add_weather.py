class BaseForge:

    name = "base_forge"

    def process(self, data, config_process=None):
        raise NotImplementedError


class AddWeather(BaseForge):
    """Merge Open-Meteo variables (configurable preset) onto the series.

    The runner injects config_process["_weather"] = {preset, variables, source, resolution, cache_dir}.
    - source `historical_forecast`: archived NWP forecasts (closest to what is available at inference;
      NOTE Open-Meteo stitches the first hours of each model run, so it is still optimistic vs a true
      day-ahead forecast). source `archive`: reanalysis = ACTUALS (flag it in the results).
    - Variables are validated against the API: invalid/renamed names are dropped with a warning.
    - Responses are cached on disk per (lat, lon, source, resolution, period) and only missing
      variables are downloaded.
    """

    name = "add_weather"

    ENDPOINTS = {
        "historical_forecast": "https://historical-forecast-api.open-meteo.com/v1/forecast",
        "archive": "https://archive-api.open-meteo.com/v1/archive",
        "forecast": "https://api.open-meteo.com/v1/forecast",
    }

    def _request(self, url, params, tries=3):
        import re
        import time
        import requests

        last = None
        for i in range(tries):
            try:
                r = requests.get(url, params=params, timeout=60)
                if r.status_code == 200:
                    return r.json(), []
                if r.status_code == 400:
                    reason = r.json().get("reason", "")
                    bad = re.findall(r"value '([^']+)'", reason) or re.findall(r"String value (\S+)", reason)
                    return None, (bad or [reason])
                last = f"HTTP {r.status_code}: {r.text[:200]}"
            except Exception as e:
                last = str(e)
            time.sleep(2 * (i + 1))
        raise RuntimeError(f"Open-Meteo request failed: {last}")

    def _download(self, lat, lon, start, end, variables, source, resolution):
        import pandas as pd

        block = "minutely_15" if resolution == "minutely_15" else "hourly"
        variables = list(variables)
        dropped = []
        while variables:
            params = {"latitude": lat, "longitude": lon, "start_date": start, "end_date": end,
                      block: ",".join(variables), "timezone": "UTC"}
            js, bad = self._request(self.ENDPOINTS[source], params)
            if js is not None:
                break
            bad = [x for b in bad for x in str(b).split(",")]
            bad_names = [b for b in bad if b in variables]
            if len(variables) > 1 and len(bad_names) == len(variables):
                bad_names = []
            if not bad_names and len(variables) > 1:
                # the API did not name the culprit: probe variables one by one
                for v in variables:
                    ok, _ = self._request(self.ENDPOINTS[source], {**params, block: v})
                    if ok is None:
                        bad_names.append(v)
            if not bad_names:
                if block == "minutely_15":
                    print("[FAST - forge] add_weather: minutely_15 unsupported, falling back to hourly")
                    return self._download(lat, lon, start, end, variables, source, "hourly")
                raise RuntimeError(f"Open-Meteo rejected the request: {bad}")
            print(f"[FAST - forge] add_weather WARNING: dropping unknown variables {bad_names}")
            dropped += bad_names
            variables = [v for v in variables if v not in bad_names]
        if not variables:
            return pd.DataFrame(columns=["ds"]), dropped
        df = pd.DataFrame(js[block])
        df["ds"] = pd.to_datetime(df.pop("time"), utc=True)
        # Variables the API accepted but has no data for are all-NaN: drop them too
        empty = [c for c in df.columns if c != "ds" and df[c].isna().all()]
        if empty:
            print(f"[FAST - forge] add_weather WARNING: no data for {empty}")
            df = df.drop(columns=empty)
            dropped += empty
        return df, dropped

    def process(self, data, config_process=None):
        import hashlib
        import os
        import pandas as pd
        from pathlib import Path

        cfg = config_process["_weather"]
        asset = config_process["_asset"]
        variables = list(cfg.get("variables", []))
        if not variables:
            return data
        lat, lon = asset["coords"]
        tz = asset.get("tz", "UTC")
        source = cfg.get("source", "historical_forecast")
        resolution = cfg.get("resolution", "hourly")

        start = (data["ds"].min() - pd.Timedelta(days=1)).strftime("%Y-%m-%d")
        end = (data["ds"].max() + pd.Timedelta(days=1)).strftime("%Y-%m-%d")
        cache_dir = Path(cfg.get("cache_dir", "cache/weather"))
        cache_dir.mkdir(parents=True, exist_ok=True)
        key = hashlib.md5(f"{lat:.4f}|{lon:.4f}|{source}|{resolution}|{start}|{end}".encode()).hexdigest()[:12]
        cache_file = cache_dir / f"weather_{key}.csv"
        bad_file = cache_dir / f"weather_{key}.invalid.txt"

        cached = pd.read_csv(cache_file, parse_dates=["ds"]) if cache_file.exists() else pd.DataFrame(columns=["ds"])
        invalid = set(bad_file.read_text(encoding="utf-8").split()) if bad_file.exists() else set()
        missing = [v for v in variables if v not in cached.columns and v not in invalid]
        if missing:
            new, dropped = self._download(lat, lon, start, end, missing, source, resolution)
            if len(new.columns) > 1:
                if len(cached.columns) > 1:
                    cached["ds"] = pd.to_datetime(cached["ds"], utc=True)
                    cached = cached.merge(new, on="ds", how="outer")
                else:
                    cached = new
                cached.to_csv(cache_file, index=False)
            if dropped:
                invalid |= set(dropped)
                bad_file.write_text("\n".join(sorted(invalid)), encoding="utf-8")
            print(f"[FAST - forge] add_weather: downloaded {[v for v in missing if v not in dropped]}")
        else:
            print(f"[FAST - forge] add_weather: all {len(variables)} variables from cache")

        usable = [v for v in variables if v in cached.columns]
        if not usable:
            print("[FAST - forge] add_weather WARNING: no usable weather variables")
            return data
        w = cached[["ds"] + usable].copy()
        w["ds"] = pd.to_datetime(w["ds"], utc=True).dt.tz_convert(tz).dt.tz_localize(None)
        w = w.drop_duplicates("ds").set_index("ds").sort_index()
        freq = cfg.get("freq") or pd.infer_freq(data["ds"].iloc[:50]) or "15min"
        w = w.resample(freq).interpolate("linear", limit_direction="both").reset_index()

        out = data.merge(w, on="ds", how="left")
        out[usable] = out[usable].interpolate(limit_direction="both")
        print(f"[FAST - forge] add_weather: preset={cfg.get('preset')} source={source} vars={usable}")
        return out


import dill

forge = AddWeather()

with open("forge_add_weather.dill", "wb") as f:
    dill.dump(forge, f)
