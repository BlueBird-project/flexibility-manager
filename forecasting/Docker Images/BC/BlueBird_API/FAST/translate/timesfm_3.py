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


class TimesFM3Translate(BaseTranslate):
    """Foundation forecaster: Google TimesFM 3.0 (https://huggingface.co/google/timesfm-3.0-pytorch).

    Requires `pip install "timesfm[torch]"` (module `timesfm3`, per the google-research/timesfm README; the exact
    import path was NOT verified here). Weights are under a NON-COMMERCIAL licence.
    API: TimesFM3Evaluator(ModelConfig(checkpoint_path, per_core_batch_size, device)).predict_batch(
    contexts=[(V,T)], horizon, past_future_covariates=[(C,T+h)]) -> items with .forecast (V,h).
    Weather is passed as past-and-future covariates.
    """

    name = "timesfm_3"

    def __init__(self):
        self.model = None

    def load(self, model_path: str = "google/timesfm-3.0-pytorch", device: str = None, batch_size: int = 16, **kwargs):
        try:
            from timesfm3 import ModelConfig, TimesFM3Evaluator
        except ImportError as e:
            raise ImportError('timesfm3 is not installed: pip install "timesfm[torch]"') from e
        if device == "mps":  # only the separate MLX backend targets Apple silicon
            print("[FAST - translate] timesfm_3: MPS not supported by the torch backend, using cpu")
            device = "cpu"
        self.model = TimesFM3Evaluator(ModelConfig(checkpoint_path=model_path, per_core_batch_size=batch_size,
                                                   device=device or "cpu"))

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
                contexts, pf = [], []
                for o in batch:
                    h = g[(g["ds"] < o) & g["y"].notna()].tail(context_length)
                    contexts.append(h["y"].to_numpy(np.float32)[None, :])
                    if cov:
                        fut_idx = pd.date_range(o, periods=horizon, freq=step)
                        pf.append(np.concatenate([h[cov].to_numpy(np.float32),
                                                  gi.reindex(fut_idx)[cov].to_numpy(np.float32)]).T)
                kw = {"past_future_covariates": pf} if cov else {}
                outs = list(self.model.predict_batch(contexts=contexts, horizon=horizon, return_quantiles=bool(quantiles), **kw))
                for o, res in zip(batch, outs):
                    frame = pd.DataFrame({"unique_id": uid, "ds": pd.date_range(o, periods=horizon, freq=step),
                                          "origin": o, "timesfm3": np.asarray(res.forecast).reshape(-1)[:horizon]})
                    if quantiles:
                        # UNVERIFIED output layout: assumes res.quantiles is [(V,) h, Q] with Q = 9 levels 0.1..0.9
                        # (or 10 with the mean first); otherwise the bounds stay empty
                        q = getattr(res, "quantiles", None)
                        bounds = None
                        if q is not None:
                            q = np.asarray(q, dtype=float)
                            q = q.reshape(-1, q.shape[-1])[-horizon:] if q.ndim > 2 else q
                            q = q[:, 1:] if q.shape[-1] == 10 else q
                            bounds = _extract_bounds(q, [i / 10 for i in range(1, 10)], min(quantiles), max(quantiles))
                        if bounds is not None:
                            frame["yhat_lower"], frame["yhat_upper"] = bounds[0][:horizon], bounds[1][:horizon]
                    frames.append(frame)
        out = pd.concat(frames, ignore_index=True)
        if clip_min is not None:
            out["timesfm3"] = out["timesfm3"].clip(lower=clip_min)
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
        out = out.rename(columns={"timesfm3": "yhat"})
        return out[["unique_id", "ds", "yhat"] + [c for c in ("yhat_lower", "yhat_upper") if c in out.columns]]


import dill
translate = TimesFM3Translate()

with open("translate_timesfm_3.dill", "wb") as f:
    dill.dump(translate, f)
