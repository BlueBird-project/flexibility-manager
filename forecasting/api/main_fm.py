import torch
from chronos import Chronos2Pipeline
from codecarbon import OfflineEmissionsTracker
import pandas as pd
from typing import List
from pydantic import BaseModel
from fastapi import FastAPI

def InferenceFM(data_to_forecast, forecast_horizon = 1, freq = "H", get_emissions = True, country_iso_code = "ES", project_name = "BlueBird FM Forecasting"):
    
    if get_emissions:
        tracker = OfflineEmissionsTracker(country_iso_code=country_iso_code, project_name=project_name)
        tracker.start()

    if torch.cuda.is_available():
        device = "cuda"
    elif torch.backends.mps.is_available():
        device = "mps"
    else:
        device = "cpu"


    pipeline = Chronos2Pipeline.from_pretrained(
        "amazon/chronos-2",
        device_map=device
    )

    data_to_forecast["ds"] = pd.to_datetime(data_to_forecast["ds"])
    data_to_forecast = data_to_forecast.sort_values(by=["unique_id", "ds"])

    data_to_forecast = data_to_forecast.rename(columns={"ds": "timestamp", "y": "target", "unique_id": "item_id"})
    
    if freq == "H":
        freq_number = 24
    elif freq == "D":
        freq_number = 1
    elif freq == "Q":
        freq_number = 24*4

    # Forecast
    forecast = pipeline.predict_df(
        data_to_forecast,
        prediction_length = freq_number*forecast_horizon
    )

    forecast_dict = {
        "ds": forecast["timestamp"].tolist(),
        "y": forecast["predictions"].tolist(),
        "unique_id": forecast["item_id"].tolist()
    }

    if get_emissions:
        tracker.stop()

        data_process = {
            "duration": tracker.final_emissions_data.duration,
            "emissions": tracker.final_emissions_data.emissions,
            "energy_consumed": tracker.final_emissions_data.energy_consumed,
            "water_consumed": tracker.final_emissions_data.water_consumed
        }

    if get_emissions:
        return {"forecast": forecast_dict, "process_info": data_process}
    else:
        return {"forecast": forecast_dict}

app = FastAPI()

class PredictionInput(BaseModel):
    ds: List[str]
    unique_id: List[str]
    y: List[float]
    emissions: bool = True
    forecast_horizon: int = 1
    freq: str = "H",
    country_iso_code: str = "ES",

@app.get("/")
def root():
    return {"message": "API Forecasting Ready"}

@app.post("/predict/")
def predict(data: PredictionInput):
    ds_list = data.ds
    unique_list = data.unique_id
    value_list = data.y

    results = InferenceFM(
        data_to_forecast = pd.DataFrame({"ds": ds_list, "unique_id": unique_list, "y": value_list}),
        forecast_horizon = data.forecast_horizon,
        freq = data.freq,
        get_emissions = data.emissions,
        country_iso_code = data.country_iso_code,
    )

    if data.emissions:
        forecasting = {
            "forecast": results["forecast"],
            "process_info": results["process_info"]
        }
    else:
        forecasting = {
            "forecast": results["forecast"]
        }

    return forecasting
