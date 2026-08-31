import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import torch
from chronos import Chronos2Pipeline
from codecarbon import OfflineEmissionsTracker
from tqdm import tqdm
import json

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

        if location:
            print("Latitud:", location.latitude)
            print("Longitud:", location.longitude)
            print("Dirección encontrada:", location.address)
        else:
            print("No se encontró la dirección")

        return [location.latitude, location.longitude]

def SMAPE(actual, predicted) -> float: 
        import numpy as np
        # Convert actual and predicted to numpy 
        # array data type if not already 
        if not all([isinstance(actual, np.ndarray),  
                    isinstance(predicted, np.ndarray)]): 
            actual, predicted = np.array(actual), np.array(predicted) 
    
        smape = round( 
            np.mean( 
                np.abs(predicted - actual) / 
                ((np.abs(predicted) + np.abs(actual))/2) 
            )*100, 2
        )

        return (200 - smape) / 2

def Coverage(y_true, y_pred, threshold = 0.75):
    import numpy as np
    total_ = 0
    for true, pred in zip(y_true, y_pred):
        if true >= float(pred)*threshold and true <= float(pred)*(2-threshold):
            total_ = total_ + 1
    
    return np.round((total_/len(y_true))*100,2)

def InferenceFM(data_to_forecast, forecast_horizon = 1, freq = "H", 
                weather_info = pd.DataFrame(),
                get_emissions = True,country_iso_code = "ES", project_name = "BlueBird FM Forecasting"):

    from codecarbon import OfflineEmissionsTracker
    
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

file = "..."
pilot_name = "..."
project_forecast = "..."
use_case = "...",
location_id = 0
weather_min = "..."
weather_max = "..."
ds_col = "Timestamp"
unique_col = "unique_id"
value_col = "value"

#### STEP 1: PROCESS DATA ###

data = pd.read_csv(file, sep = ";")
data = data[[ds_col, unique_col, value_col]].groupby([ds_col, unique_col]).mean().reset_index()

data = interpolate_timeseries_15min(
    data, unique_col, ds_col
)

try:
    with open("results.json") as file:
        list_result = json.load(file)
except:
    list_result = []


min_date = min(data[ds_col])
max_date = max(data[ds_col])

if type(min_date) == str:
    min_date = datetime.strptime(min_date[0:10], "%Y-%m-%d")
    max_date = datetime.strptime(max_date[0:10], "%Y-%m-%d")

days_ = (max_date - min_date).days - 10


#### STEP 2: OBTAIN WEATHER VALUES ###

with open("locations.json") as file:
    locations = json.load(file)

location_info = ObtainCoordinates(locations, use_case, location_id)

weather_info = ObtainWeatherValues(
    location_info[0], location_info[1], 
    min_date = datetime.strftime(min_date, "%Y-%m-%d"),
    max_date = datetime.strftime(max_date, "%Y-%m-%d")
)

weather_info["unique_id"] = use_case

weather_info = interpolate_timeseries_15min(
    weather_info.drop(["date"], axis = 1), "unique_id", "ds"
)



list_data = []
list_data_weather = []

for i in tqdm(range(15, days_)):
    dict_result = {}
    random_date = min_date + timedelta(days = i)
    dict_result["date"] = random_date + timedelta(days = 1)
    dict_result
    data_to_forecast = data[data["Timestamp"] < datetime.strftime(random_date, "%Y-%m-%d")]
    test_data = data[(data["Timestamp"] >=  datetime.strftime(random_date, "%Y-%m-%d")) & (data["Timestamp"] < datetime.strftime(random_date + timedelta(days = 1), "%Y-%m-%d"))]
    
    data_to_forecast.columns = ["ds", "unique_id", "y"]
    test_data.columns = ["ds", "unique_id", "y"]

    result = InferenceFM(
        data_to_forecast, freq= "Q", get_emissions= False
    )

    result_weather = InferenceFM(
        data_to_forecast, freq = "Q", weather_info = weather_info, get_emissions= False
    )

    prediction = pd.DataFrame(result["forecast"]).rename(columns={ "y": "yhat"})
    prediction_weather = pd.DataFrame(result_weather["forecast"]).rename(columns={ "y": "yhat"})
    compare = pd.merge(test_data, prediction, on = ["ds", "unique_id"])
    compare_weather = pd.merge(test_data, prediction_weather, on = ["ds", "unique_id"])

    list_data.append(compare)
    list_data_weather.append(compare_weather)
    
    dict_result["Coverage90"] = Coverage(compare['y'], compare['yhat'], 0.9)
    dict_result["Coverage80"] = Coverage(compare['y'], compare['yhat'], 0.8)
    dict_result["Coverage95"] = Coverage(compare['y'], compare['yhat'], 0.95)
    dict_result["SMAPE"] = SMAPE(compare['y'], compare['yhat'])

    dict_result["Coverage90_weather"] = Coverage(compare_weather['y'], compare_weather['yhat'], 0.9)
    dict_result["Coverage80_weather"] = Coverage(compare_weather['y'], compare_weather['yhat'], 0.8)
    dict_result["Coverage95_weather"] = Coverage(compare_weather['y'], compare_weather['yhat'], 0.95)
    dict_result["SMAPE_weather"] = SMAPE(compare_weather['y'], compare_weather['yhat'])

    list_result.append(dict_result)

data_forecast_fm = pd.concat(list_data)
data_forecast_fm_weather = pd.concat(list_data_weather)

data_forecast_fm.to_csv(f"forecast_test_{pilot_name}_{project_forecast}.csv", index = False)
data_forecast_fm_weather.to_csv(f"forecast_test_{pilot_name}_{project_forecast}_weather.csv", index = False)

with open("results.json") as file:
    json.dump(result, file)

