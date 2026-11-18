import pandas as pd


class BaseTranslate:

    name = "base_translate"

    def load(self, model_path: str):
        raise NotImplementedError

    def predict(self, data):
        raise NotImplementedError


def _extract_bounds(quantiles_arr, levels, lo, hi):
    """(lower, upper) arrays from a [..., Q] quantile array whose Q levels are `levels`; None if unavailable."""
    import numpy as np
    arr = np.asarray(quantiles_arr, dtype=float)
    levels = np.asarray(levels, dtype=float)
    if arr.shape[-1] != len(levels):
        return None
    pick = lambda q: arr[..., int(np.abs(levels - q).argmin())]
    return pick(lo), pick(hi)


class TimestralTranslate(BaseTranslate):
    """Foundation forecaster: Mistral Timestral (https://huggingface.co/mistralai/Timestral-209M).

    Requires `pip install git+https://github.com/mistralai/timestral.git` (Python >= 3.10, torch >= 2.4).
    API (from the model card/README, NOT executed here): TimestralForecaster.from_pretrained(repo, device=...);
    forecast(context[B,V,T], prediction_length, past_covariates[B,C,T], known_covariates[B,C,T+h]) -> .median[B,V,h].
    Weather is passed as known-future covariates; NaN marks missing/padded values.
    """

    name = "timestral"

    def __init__(self):
        self.model = None

    def load(self, model_path: str = "mistralai/Timestral-209M", device: str = None, **kwargs):
        try:
            from timestral import TimestralForecaster
        except ImportError as e:
            raise ImportError("timestral is not installed: pip install git+https://github.com/mistralai/timestral.git") from e
        if device == "mps":  # MPS support is not documented for this model
            print("[FAST - translate] timestral: MPS not documented, using cpu")
            device = "cpu"
        self.model = TimestralForecaster.from_pretrained(model_path, device=device or "cpu")

    def rolling_forecast(self, data, origins, horizon, freq="15min", context_length=2048,
                         covariate_cols=None, batch_origins=16, clip_min=None, quantiles=None):
        import numpy as np

        cov = list(covariate_cols or [])
        step = pd.tseries.frequencies.to_offset(freq)
        frames = []
        for uid, g in data.groupby("unique_id", sort=False):
            g = g.sort_values("ds").reset_index(drop=True)
            gi = g.set_index("ds")
            for i in range(0, len(origins), batch_origins):
                batch = [pd.Timestamp(o) for o in origins[i:i + batch_origins]]
                hists = [g[(g["ds"] < o) & g["y"].notna()].tail(context_length) for o in batch]
                T = max(len(h) for h in hists)
                ctx = np.full((len(batch), 1, T), np.nan, dtype=np.float32)
                known = np.full((len(batch), len(cov), T + horizon), np.nan, dtype=np.float32) if cov else None
                for b, (o, h) in enumerate(zip(batch, hists)):
                    ctx[b, 0, T - len(h):] = h["y"].to_numpy(np.float32)
                    if cov:
                        fut_idx = pd.date_range(o, periods=horizon, freq=step)
                        allc = np.concatenate([h[cov].to_numpy(np.float32),
                                               gi.reindex(fut_idx)[cov].to_numpy(np.float32)]).T
                        known[b, :, T - len(h):] = allc
                kw = {"known_covariates": known} if cov else {}
                fc = self.model.forecast(ctx, prediction_length=horizon, **kw)
                med = np.asarray(fc.median)[:, 0, :]
                bounds = None
                if quantiles:
                    # UNVERIFIED: assumes fc.quantiles [B,V,h,Q] and fc.quantile_levels; Timestral may only return the
                    # median, in which case the bounds stay empty (the API can fall back to a residual-based interval)
                    q, lv = getattr(fc, "quantiles", None), getattr(fc, "quantile_levels", None)
                    if q is not None and lv is not None:
                        bounds = _extract_bounds(np.asarray(q)[:, 0], lv, min(quantiles), max(quantiles))
                for b, o in enumerate(batch):
                    frame = pd.DataFrame({"unique_id": uid, "ds": pd.date_range(o, periods=horizon, freq=step),
                                          "origin": o, "timestral": med[b]})
                    if bounds is not None:
                        frame["yhat_lower"], frame["yhat_upper"] = bounds[0][b], bounds[1][b]
                    frames.append(frame)
        out = pd.concat(frames, ignore_index=True)
        if clip_min is not None:
            out["timestral"] = out["timestral"].clip(lower=clip_min)
        return out

    def predict(self, data, horizon, future_df, covariate_cols=None, context_length=2048, quantiles=None, **kwargs):
        """API contract (BaseTranslate): history + future grid -> unique_id, ds, yhat for the `horizon` steps after
        each asset's last observation. Univariate unless covariate_cols is given (then future_df must hold them)."""
        import numpy as np

        data = data.copy()
        data["y"] = pd.to_numeric(data["y"], errors="coerce")
        future_df = future_df.copy()
        freq = pd.infer_freq(data.sort_values("ds")["ds"].drop_duplicates()) or "h"
        outs = []
        for uid, g in data.groupby("unique_id", sort=False):
            last = g.loc[g["y"].notna(), "ds"].max()
            origin = last + pd.tseries.frequencies.to_offset(freq)
            fut = future_df[(future_df["unique_id"] == uid) & (future_df["ds"] > last)].copy()
            fut["y"] = np.nan
            full = pd.concat([g, fut], ignore_index=True)
            outs.append(self.rolling_forecast(full, [origin], horizon, freq=freq, context_length=context_length,
                                              covariate_cols=covariate_cols, quantiles=quantiles, **kwargs))
        out = pd.concat(outs, ignore_index=True)
        out = out.rename(columns={"timestral": "yhat"})
        return out[["unique_id", "ds", "yhat"] + [c for c in ("yhat_lower", "yhat_upper") if c in out.columns]]


import dill
translate = TimestralTranslate()

with open("translate_timestral.dill", "wb") as f:
    dill.dump(translate, f)
