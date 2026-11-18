class BaseForge:

    name = "base_forge"

    def process(self, data, config_process=None):
        raise NotImplementedError


class LoadCsvAsset(BaseForge):
    """Loader forge: reads one asset file (config_process["_asset"]["path"]) into unique_id, ds, y.

    Config: ds_col, y_col, id_col (null -> asset unique_id). Cumulative or any other
    columns are ignored; only the chosen target column is kept.
    """

    name = "load_csv_asset"

    def process(self, data=None, config_process=None):
        import pandas as pd

        config_process = config_process or {}
        asset = config_process["_asset"]
        ds_col = config_process.get("ds_col", "ds")
        y_col = config_process.get("y_col", "y")
        id_col = config_process.get("id_col")

        df = pd.read_csv(asset["path"], sep=None, engine="python", encoding="utf-8")
        out = pd.DataFrame({
            "unique_id": df[id_col].astype(str) if id_col else asset["unique_id"],
            "ds": pd.to_datetime(df[ds_col]),
            "y": pd.to_numeric(df[y_col], errors="coerce"),
        })
        if out["ds"].dt.tz is not None:
            out["ds"] = out["ds"].dt.tz_convert(asset.get("tz", "UTC")).dt.tz_localize(None)

        out = out.groupby(["unique_id", "ds"], as_index=False)["y"].mean()
        out = out.sort_values(["unique_id", "ds"]).reset_index(drop=True)
        print(f"[FAST - forge] {self.name}: {len(out)} rows, {out['ds'].min()} -> {out['ds'].max()}")
        return out


import dill

forge = LoadCsvAsset()

with open("forge_load_csv_asset.dill", "wb") as f:
    dill.dump(forge, f)
