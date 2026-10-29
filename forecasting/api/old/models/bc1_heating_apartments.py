from typing import List
from pydantic import BaseModel
from fastapi import FastAPI
import pandas as pd

#### AUX FUNCTIONS #####
def InferenceFM(data_to_forecast, forecast_horizon = 1, freq = "H", 
                weather_info = pd.DataFrame(),
                get_emissions = True,country_iso_code = "ES", project_name = "BlueBird FM Forecasting"):

    from codecarbon import OfflineEmissionsTracker
    from chronos import Chronos2Pipeline
    import torch
    
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

    try:
        max_date = max(data_to_forecast["ds"])
        weather_future = weather_info[(weather_info["ds"] > max_date) & (weather_info["ds"] <= (max_date + timedelta(days = forecast_horizon)))]
    except:
        weather_future = pd.DataFrame()

    # Forecast
    if weather_future.shape[0] == 0:
        forecast = pipeline.predict_df(
            data_to_forecast,
            prediction_length = freq_number*forecast_horizon
        )
    else:
        data_to_forecast = pd.merge(data_to_forecast, weather_info, on = "ds")
        weather_future.rename(columns = {"ds": "timestamp", "unique_id": "item_id"})
        forecast = forecast = pipeline.predict_df(
                    data_to_forecast,
                    prediction_length = freq_number*forecast_horizon,
                    future_df= weather_future
                )

    forecast_dict = {
        "ds": forecast["timestamp"].tolist(),
        "y": forecast["predictions"].tolist(),
        "unique_id": forecast["item_id"].tolist()
    }

    if get_emissions:
        tracker.stop()
        try:
            data_process = {
                "duration": tracker.final_emissions_data.duration,
                "emissions": tracker.final_emissions_data.emissions,
                "energy_consumed": tracker.final_emissions_data.energy_consumed,
                "water_consumed": tracker.final_emissions_data.water_consumed
            }
        except:
            data_process = {}

    if get_emissions:
        return {"forecast": forecast_dict, "process_info": data_process}
    else:
        return {"forecast": forecast_dict}

def ObtainWeatherValues(lat, lon, min_date, max_date):
	
	import openmeteo_requests
	from datetime import datetime, timedelta

	import pandas as pd
	import requests_cache
	from retry_requests import retry

	# Setup the Open-Meteo API client with cache and retry on error
	cache_session = requests_cache.CachedSession('.cache', expire_after = -1)
	retry_session = retry(cache_session, retries = 5, backoff_factor = 0.2)
	openmeteo = openmeteo_requests.Client(session = retry_session)

	# Make sure all required weather variables are listed here
	# The order of variables in hourly or daily is important to assign them correctly below
	url = "https://archive-api.open-meteo.com/v1/archive"
	params = {
		"latitude": lat,
		"longitude": lon,
		"start_date": min_date,
		"end_date": max_date,
		"hourly": ["temperature_2m", "relative_humidity_2m", "apparent_temperature", "wind_speed_10m", "direct_radiation"],
	}
	responses = openmeteo.weather_api(url, params = params)

	# Process first location. Add a for-loop for multiple locations or weather models
	response = responses[0]

	# Process hourly data. The order of variables needs to be the same as requested.
	hourly = response.Hourly()
	hourly_temperature_2m = hourly.Variables(0).ValuesAsNumpy()
	hourly_relative_humidity_2m = hourly.Variables(1).ValuesAsNumpy()
	hourly_apparent_temperature = hourly.Variables(2).ValuesAsNumpy()
	hourly_wind_speed_10m = hourly.Variables(3).ValuesAsNumpy()
	hourly_direct_radiation = hourly.Variables(4).ValuesAsNumpy()

	hourly_data = {
		"date": pd.date_range(
			start = pd.to_datetime(hourly.Time(), unit = "s", utc = True),
			end =  pd.to_datetime(hourly.TimeEnd(), unit = "s", utc = True),
			freq = pd.Timedelta(seconds = hourly.Interval()),
			inclusive = "left"
		)
	}

	hourly_data["temperature_2m"] = hourly_temperature_2m
	hourly_data["relative_humidity_2m"] = hourly_relative_humidity_2m
	hourly_data["apparent_temperature"] = hourly_apparent_temperature
	hourly_data["wind_speed_10m"] = hourly_wind_speed_10m
	hourly_data["direct_radiation"] = hourly_direct_radiation

	hourly_dataframe = pd.DataFrame(data = hourly_data)

	hourly_dataframe["ds"] = hourly_dataframe["date"].apply(lambda x: datetime.strftime(x, "%Y-%m-%d %H:%M:%S"))

	return hourly_dataframe

def ObtainCoordinates(dict_, use_case, location_id):

    from geopy.geocoders import Nominatim
    geolocator = Nominatim(user_agent="BB")

    if type(dict_[use_case][location_id]["location"]) != str:
        return dict_[use_case][location_id]["location"]
    else:
        direccion = dict_[use_case][location_id]["location"]
        location = geolocator.geocode(direccion)

        if not location:   
            print("No se encontró la dirección")

        return [location.latitude, location.longitude]

def interpolate_timeseries_15min(
    df,
    id_col="Cups",
    time_col="ds",
    freq="15min"
):
    """
    Interpola series temporales separadas por `id_col` a una frecuencia fija
    usando interpolación cúbica.

    Parameters
    ----------
    df : pd.DataFrame
        Dataset original
    id_col : str
        Columna identificadora de series (ej. 'Cups')
    time_col : str
        Columna de timestamps en formato string (ej. 'ds')
    freq : str
        Frecuencia objetivo (por defecto '15min')

    Returns
    -------
    pd.DataFrame
        Dataset interpolado con frecuencia regular
    """

    df = df.copy()

    # Convertir ds a datetime
    df[time_col] = pd.to_datetime(df[time_col])

    interpolated_series = []

    for cup_id, g in df.groupby(id_col):
        g = g.sort_values(time_col).set_index(time_col)

        # Reindexar a una grilla temporal regular
        full_index = pd.date_range(
            start=g.index.min(),
            end=g.index.max(),
            freq=freq
        )
        g = g.reindex(full_index)

        # Aplicar interpolación cúbica solo a columnas numéricas
        num_cols = g.select_dtypes(include="number").columns
        
        if len(g[num_cols].dropna()) >= 4:
            try:
                g[num_cols] = g[num_cols].interpolate(
                    method="spline",
                    order=3,
                    limit_direction="both"
                )
            except:
                 g[num_cols] = g[num_cols].interpolate(
                method="linear",
                limit_direction="both")
        else:
            # Fallback seguro
            g[num_cols] = g[num_cols].interpolate(
                method="linear",
                limit_direction="both")
             


        # Restaurar identificador
        g[id_col] = cup_id
        g = g.reset_index().rename(columns={"index": time_col})

        interpolated_series.append(g)

    return pd.concat(interpolated_series, ignore_index=True)

### API CODE #####

app = FastAPI()

class PredictionInput(BaseModel):
    ds: List[str]
    unique_id: List[str]
    y: List[float]
    pilot: str = "None",
    emissions: bool = True
    forecast_horizon: int = 1
    freq: str = "H"

@app.get("/")
def root():
    return {"message": "API Forecasting Ready"}


@app.post("/forecast/")
def forecast():

    import json
    from datetime import datetime

    with open("locations.json") as file:
        locations = json.load(file)

    location_info = ObtainCoordinates(locations, use_case, location_id)

    weather_info = ObtainWeatherValues(
        location_info[0], location_info[1], 
        min_date = datetime.strftime(min_date, "%Y-%m-%d"),
        max_date = datetime.strftime(max_date, "%Y-%m-%d")
)