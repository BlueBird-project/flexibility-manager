import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import torch
from chronos import Chronos2Pipeline
from codecarbon import OfflineEmissionsTracker
from tqdm import tqdm
import json

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
ds_col = "Timestamp"
unique_col = "unique_id"
value_col = "value"

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
    prediction = pd.DataFrame(result["forecast"]).rename(columns={ "y": "yhat"})
    compare = pd.merge(test_data, prediction, on = ["ds", "unique_id"])
    
    dict_result["Coverage90"] = Coverage(compare['y'], compare['yhat'], 0.9)
    dict_result["Coverage80"] = Coverage(compare['y'], compare['yhat'], 0.8)
    dict_result["Coverage95"] = Coverage(compare['y'], compare['yhat'], 0.95)
    dict_result["SMAPE"] = SMAPE(compare['y'], compare['yhat'])

    list_result.append(dict_result)

with open("results.json") as file:
    json.dump(result, file)

