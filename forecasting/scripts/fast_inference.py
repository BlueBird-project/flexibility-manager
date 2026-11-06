from pathlib import Path
from datetime import datetime, timedelta
import pandas as pd


# ==========================================================
# TEMP
# ==========================================================

def ClassBuilder(path_fast, path_class, type_class):

    import subprocess, dill, os

    subprocess.run(
        ["python", os.path.join(path_fast, type_class, f"{path_class}.py")], check = True
    )

    dill_file = os.path.join(
        path_fast,
        f"{type_class}_{path_class}.dill"
    )

    try:
        with open(dill_file, "rb") as f:
            class_fast = dill.load(f)
    except FileNotFoundError:
        dill_file = os.path.join(
            os.getcwd(),f"{type_class}_{path_class}.dill")
        with open(dill_file, "rb") as f:
            class_fast = dill.load(f)

    os.remove(dill_file)

    return class_fast


# ==========================================================
# DATA LOADER
# ==========================================================

def load_data(
    filename: str | None = None,
    db_connection=None,
    sql_query: str | None = None
) -> pd.DataFrame:

    if filename is not None:

        extension = Path(filename).suffix.lower()

        if extension == ".csv":
            try:
                df = pd.read_csv(filename, sep=",")
            except Exception:
                df = pd.read_csv(filename, sep=";")

            if len(df.columns) == 1:
                df = pd.read_csv(filename, sep=";")

        elif extension == ".feather":
            df = pd.read_feather(filename)

        elif extension in [".xlsx", ".xls"]:
            df = pd.read_excel(filename)

        else:
            raise ValueError(f"Unsupported extension: {extension}")

    else:

        df = pd.read_sql(sql_query, db_connection)

    df["ds"] = pd.to_datetime(df["ds"])

    return df.sort_values(["unique_id", "ds"])


# ==========================================================
# FORECAST STORAGE
# ==========================================================

def save_forecasts_file(
    forecasts: pd.DataFrame,
    output_file: str
):

    output_file = Path(output_file)

    if output_file.exists():

        previous = pd.read_feather(output_file)

        forecasts = pd.concat(
            [previous, forecasts],
            ignore_index=True
        )

        forecasts = forecasts.drop_duplicates()
    try:
        forecasts.to_feather(output_file)
    except:
        forecasts.to_csv("forecast_test.csv")


def save_forecasts_db(
    forecasts: pd.DataFrame,
    db_connection,
    table_name: str = "forecasts"
):

    forecasts.to_sql(
        table_name,
        db_connection,
        if_exists="append",
        index=False
    )


# ==========================================================
# INFERENCE RUNNER
# ==========================================================

class ModelInference:

    def __init__(
        self,
        forges,
        translate,
        config_forges = [],
        config_translate = [],
        horizon: int = 24
    ):
        self.forges = forges
        self.translates = translate
        self.horizon = horizon
        self.config_forges = config_forges
        self.config_translate = config_translate

    def run(
        self,
        data: pd.DataFrame,
        cutoff_date: datetime,
        path_fast: str,
    ) -> pd.DataFrame:

        import subprocess, dill
        
        if cutoff_date is not None:

            history = data[data["ds"] <= cutoff_date].copy()

            future_df = data[data["ds"] > cutoff_date].copy()

        else:

            history = data.copy()

            freq = pd.infer_freq(
                history.sort_values("ds")["ds"]
            )

            if freq is None:
                freq = "h"

            future_frames = []

            for unique_id in history["unique_id"].unique():

                asset_history = history[
                    history["unique_id"] == unique_id
                ]

                last_ds = asset_history["ds"].max()

                future_dates = pd.date_range(
                    start=last_ds,
                    periods=self.horizon + 1,
                    freq=freq
                )

                future_asset = pd.DataFrame(
                    {
                        "ds": future_dates,
                        "unique_id": unique_id,
                        "y": None
                    }
                )

                future_frames.append(future_asset)

            future_df = pd.concat(
                future_frames,
                ignore_index=True
            )

        for forge, config in zip(self.forges, self.config_forges):
            try:
                forge = ClassBuilder(path_fast, forge, "forge")
                history = forge.process(history, config_process = config)
            except Exception as e:
                print(f"Error processing forge {forge}: {e}")

        for forge, config in zip(self.forges, self.config_forges):
            try:
                forge = ClassBuilder(path_fast, forge, "forge")
                future_df = forge.process(future_df, config_process=config)
            except Exception as e:
                print(f"Error processing forge {forge}: {e}")

        for translate in self.translates:
            list_forecast = []
            try:
                translate = ClassBuilder(path_fast, translate, "translate")
                translate.load(model_path = self.config_translate[0].get("model_path", None))
                forecast = translate.predict(
                    data=history,
                    horizon=self.horizon,
                    future_df = future_df
                )
                forecast["created_at"] = datetime.now().isoformat()
                forecast["model"] = translate.__class__.__name__
                list_forecast.append(forecast)
            except Exception as e:
                print(f"Error processing translate {translate}: {e}")

        

        return forecast

# ==========================================================
# PRODUCTION MODE
# ==========================================================

def run_production(
    pipeline: ModelInference,
    data: pd.DataFrame,
    path_fast: str
):

    return pipeline.run(
        data=data,
        cutoff_date=None,
        path_fast=path_fast
    )


# ==========================================================
# BACKTEST MODE
# ==========================================================

def run_backtest(
    pipeline: ModelInference,
    data: pd.DataFrame,
    path_fast: str,
    start_date: str,
    end_date: str
):

    from tqdm import tqdm
    from datetime import datetime

    forecasts = []

    

    current_date = datetime.strptime(start_date, "%Y-%m-%d")
    finish_date = datetime.strptime(end_date, "%Y-%m-%d")

    pbar = tqdm(total = (finish_date - current_date).days)

    while current_date <= finish_date:

        forecast = pipeline.run(
            data=data,
            cutoff_date=current_date,
            path_fast=path_fast
        )

        forecast["cutoff_date"] = current_date

        forecasts.append(forecast)

        current_date += timedelta(days=1)
        pbar.update(1)
    return pd.concat(
        forecasts,
        ignore_index=True
    )


# ==========================================================
# MAIN
# ==========================================================

def inference_pipeline(
    forges,
    translate,
    mode="production",
    filename=None,
    db_connection=None,
    sql_query=None,
    output_file="forecasts.feather",
    start_date=None,
    end_date=None,
    horizon=24*4,
    path_fast = None,
    config_files = {"forge": [], "translate": []}
):

    data = load_data(
        filename=filename,
        db_connection=db_connection,
        sql_query=sql_query
    )

    pipeline =ModelInference(
        forges=forges,
        translate=translate,
        horizon=horizon,
        config_forges = config_files["forge"],
        config_translate = config_files["translate"]
    )

    if mode == "production":

        forecasts = run_production(
            pipeline=pipeline,
            data=data,
            path_fast=path_fast
        )

    elif mode == "backtest":

        forecasts = run_backtest(
            pipeline=pipeline,
            data=data,
            start_date=start_date,
            end_date=end_date,
            path_fast=path_fast
        )

    else:

        raise ValueError(
            f"Invalid mode: {mode}"
        )

    if filename is not None:

        save_forecasts_file(
            forecasts,
            output_file
        )

    else:

        save_forecasts_db(
            forecasts,
            db_connection
        )

    return forecasts

import argparse

if __name__ == "__main__":

    parser = argparse.ArgumentParser()

    parser.add_argument("--file", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--path_fast", "-fast", required=True)
    parser.add_argument("--forges", "-f", nargs="+", default = [])
    parser.add_argument("--translate", "-t", nargs="+", default = [])
    parser.add_argument("--config", "-c", nargs="+", default = [])
    parser.add_argument("--mode", default="production")
    parser.add_argument("--start_date", "-s", default=None)
    parser.add_argument("--end_date", "-e", default= None)
    parser.add_argument("--horizon_span", "-hs", default= 24*4)

    args = parser.parse_args()

    config_ = {
        "location": "Namur, Belgium"
    }

    config_interpolate = {}

    config_pv = {}

    config_chronos_2 = {"model_path": "amazon/chronos-2"}

    inference_pipeline(
        forges=args.forges,
        translate=args.translate,
        start_date=args.start_date,
        end_date= args.end_date,
        mode=args.mode,
        filename=args.file,
        output_file=args.output,
        path_fast=args.path_fast,
        config_files = {
            "forge": [config_, config_interpolate, config_pv],
            "translate": [config_chronos_2]
        },
        horizon= int(args.horizon_span)
    )