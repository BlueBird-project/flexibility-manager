import pandas as pd

class BaseTranslate:

    name = "base_translate"

    def load(self, model_path: str):
        raise NotImplementedError

    def predict(self, data):
        raise NotImplementedError

class ChronosTranslate(BaseTranslate):

    def __init__(self):
        pass

    def load(self, model_path: str):
        from chronos import Chronos2Pipeline
        self.pipeline = Chronos2Pipeline.from_pretrained(
            model_path
        )

    def predict(
        self,
        data: pd.DataFrame,
        horizon: int,
        future_df : pd.DataFrame
    ) -> pd.DataFrame:


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


        forecast = self.pipeline.predict_df(
            data,
            prediction_length=horizon,
            future_df = future_df
        )

        forecast = forecast.rename(
            columns={
                "timestamp": "ds",
                "predictions": "yhat",
                "item_id": "unique_id"
            }
        )

        return forecast

import dill
translate = ChronosTranslate()

with open("translate_chronos_2.dill", "wb") as f:
    dill.dump(translate, f)