from fastapi import FastAPI
from fastapi.responses import JSONResponse
from fastapi.encoders import jsonable_encoder
import json
from typing import List
from pydantic import BaseModel
import pandas as pd
from neuralforecast.core import NeuralForecast
from datetime import datetime
import numpy as np
import math
from icecream import ic

def RoundUp(figure):
    try:
        figure =  np.round(figure, 2)
    except:
        return 0
    
    if math.isfinite(figure) == True:
        return figure
    else:
        return -1

def PredictionTaimu(model, ds, unique_id, y, exogenous_variables = {}, mode = "df"):
    data ={
            "ds": ds,
            "unique_id": unique_id,
            "y": y
        }
    
    for var_ in exogenous_variables:
        data[var_] = exogenous_variables[var_]
    
    data = pd.DataFrame(data)

    data["ds"] = data["ds"].apply(lambda x:  datetime.strptime(x, "%Y-%m-%d %H:%M:%S"))

    predicted_values = model.predict(data)

    forecasting_values = {}
    if mode == "df":
        for var_ in predicted_values.columns:
            if var_ not in ["ds", "unique_id"]:
                forecasting_values[var_] = predicted_values[var_].apply(lambda x: RoundUp(x)).tolist()
            else:
                forecasting_values[var_] = predicted_values[var_].tolist()
    elif mode == "separated":
        forecasting_values_values = {}
        for id_ in pd.unique(predicted_values["unique_id"]):
            data_pred = predicted_values[predicted_values["unique_id"] == id_].reset_index().drop(["index"], axis = 1)
            forecasting_values[id_] = {}
            for var_ in predicted_values.columns:
                if var_ not in ["ds", "unique_id"]:
                    forecasting_values[id_][var_] = data_pred[var_].apply(lambda x: RoundUp(x)).tolist()
                else:
                    forecasting_values[var_] = predicted_values[var_].tolist()
    
    return forecasting_values

    

app = FastAPI()

with open("model_config.json") as file:
    config_ = json.load(file)

path_model = config_["path_model"]

model = NeuralForecast.load(path_model)

class PredictionInput(BaseModel):
    ds: List[str]
    unique_id: List[str]
    y: List[float]

@app.get("/")
def root():
    return {"message": "API Forecasting Ready"}

@app.post("/predict/")
def predict(data: PredictionInput):
    ds_list = data.ds
    unique_list = data.unique_id
    value_list = data.y

    forecasting = PredictionTaimu(
        model, ds_list, unique_list, value_list
    )

    return forecasting


