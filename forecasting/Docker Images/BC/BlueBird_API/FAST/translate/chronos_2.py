import pandas as pd

class BaseTranslate:

    name = "base_translate"

    def load(self, model_path: str):
        raise NotImplementedError

    def predict(self, data):
        raise NotImplementedError

class ChronosTranslate(BaseTranslate):

    import pandas as pd

    def __init__(self):
        pass

    def load(self, model_path: str, device: str = None, **kwargs):
        from chronos import Chronos2Pipeline
        kw = {"device_map": device} if device else {}
        self.pipeline = Chronos2Pipeline.from_pretrained(
            model_path, **kw
        )

    def rolling_forecast(
        self,
        data: pd.DataFrame,
        origins,
        horizon: int,
        freq: str = "15min",
        context_length: int = 1024,
        covariate_cols=None,
        batch_origins: int = 16,
        clip_min=None
    ) -> pd.DataFrame:
        """Out-of-sample forecasts for many origins (each uses only data BEFORE its origin), so they
        can be used as a leak-free feature for the hybrid arms. `data` holds history and the known
        future covariates (weather) over every horizon. Returns unique_id, ds, origin, chronos."""

        import pandas as pd

        covariate_cols = list(covariate_cols or [])
        step = pd.tseries.frequencies.to_offset(freq)
        frames = []
        for uid, g in data.groupby("unique_id", sort=False):
            g = g.sort_values("ds").reset_index(drop=True)
            for i in range(0, len(origins), batch_origins):
                ctx_rows, fut_rows = [], []
                for origin in origins[i:i + batch_origins]:
                    origin = pd.Timestamp(origin)
                    hist = g[(g["ds"] < origin) & g["y"].notna()].tail(context_length)
                    item = f"{uid}|{origin.isoformat()}"
                    c = hist[["ds", "y"] + covariate_cols].copy()
                    c["item_id"] = item
                    ctx_rows.append(c)
                    if covariate_cols:
                        fut_idx = pd.date_range(origin, periods=horizon, freq=step)
                        f = g.set_index("ds").reindex(fut_idx)[covariate_cols]
                        f.index.name = "ds"
                        f = f.reset_index()
                        f["item_id"] = item
                        fut_rows.append(f)
                ctx = pd.concat(ctx_rows, ignore_index=True).rename(columns={"ds": "timestamp", "y": "target"})
                fut = (pd.concat(fut_rows, ignore_index=True).rename(columns={"ds": "timestamp"})
                       if fut_rows else None)
                pred = self.pipeline.predict_df(
                    ctx, future_df=fut, prediction_length=horizon,
                    id_column="item_id", timestamp_column="timestamp", target="target",
                    quantile_levels=[0.5],
                )
                pred = pred.rename(columns={"timestamp": "ds", "predictions": "chronos"})
                pred["unique_id"] = pred["item_id"].str.split("|").str[0]
                pred["origin"] = pd.to_datetime(pred["item_id"].str.split("|").str[1])
                frames.append(pred[["unique_id", "ds", "origin", "chronos"]])
        out = pd.concat(frames, ignore_index=True)
        if clip_min is not None:
            out["chronos"] = out["chronos"].clip(lower=clip_min)
        return out

    def predict(
        self,
        data: pd.DataFrame,
        horizon: int,
        future_df : pd.DataFrame,
        quantiles=(0.1, 0.9),
        **kwargs
    ) -> pd.DataFrame:
        """Returns unique_id, ds, yhat (median) and, when `quantiles` is given, yhat_lower/yhat_upper
        (= min/max of `quantiles`)."""

        import pandas as pd
        data = data.rename(
            columns={
                "ds": "timestamp",
                "y": "target",
                "unique_id": "item_id"
            }
        )

        future_df = future_df.drop(["y"], axis = 1, errors = "ignore").rename(
            columns = {
                "ds": "timestamp",
                "unique_id": "item_id"
            }
        )[1:horizon+1]


        levels = sorted({0.5, *[float(q) for q in quantiles]}) if quantiles else [0.5]
        forecast = self.pipeline.predict_df(
            data,
            prediction_length=horizon,
            future_df = future_df,
            quantile_levels=levels
        )

        forecast = forecast.rename(
            columns={
                "timestamp": "ds",
                "predictions": "yhat",
                "item_id": "unique_id"
            }
        )

        if quantiles:
            for out_col, level in (("yhat_lower", min(quantiles)), ("yhat_upper", max(quantiles))):
                col = next(c for c in forecast.columns if str(c) == str(float(level)))
                forecast[out_col] = forecast[col]
            forecast = forecast.drop(columns=[c for c in forecast.columns if str(c) in {str(l) for l in levels}])
        return forecast

import dill
translate = ChronosTranslate()

with open("translate_chronos_2.dill", "wb") as f:
    dill.dump(translate, f)