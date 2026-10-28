import pandas as pd
from datetime import datetime, timedelta
from lightgbm import LGBMRegressor
from xgboost import XGBRegressor
from catboost import CatBoostRegressor
from sklearn.linear_model import (
    LinearRegression,
    Ridge,
    Lasso,
    ElasticNet,
)
from sklearn.ensemble import (
    RandomForestRegressor,
    ExtraTreesRegressor,
    GradientBoostingRegressor,
)
from sklearn.neighbors import KNeighborsRegressor
from sklearn.metrics import mean_absolute_error as MAE
from sklearn.metrics import r2_score as R2
from sklearn.metrics import root_mean_squared_error as RMSE

from mlforecast import MLForecast
from mlforecast.lag_transforms import ExpandingMean, RollingMean
from mlforecast.target_transforms import Differences
from datetime import datetime
from tqdm import tqdm
import json

tqdm.pandas()
#### FUNCTIONS #####

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

def LagList(num_days, freq):

    if freq == "15min":
        list_ = []
        for i in range(num_days*24*4):
            list_.append(i+1)
    elif freq == "1H":
        for i in range(num_days*24):
            list_.append(i+1)
    elif freq == "D":
        for i in range(num_days*24):
            list_.append(i+1)

    for i in list_:
        dict_ = {}
        if i % 4 != 0:
            dict_[i] = [ExpandingMean()]
        else:
            dict_[i] = [RollingMean(window_size=28)]
    

    return list_, dict_

def Coverage(y_true, y_pred, threshold = 0.75, delta = 0.1):
    import numpy as np
    total_ = 0
    for true, pred in zip(y_true, y_pred):
        if abs(true-pred) >= delta:
            if true >= float(pred)*threshold and true <= float(pred)*(2-threshold):
                total_ = total_ + 1
        else:
            total_ = total_ + 1
    
    return np.round((total_/len(y_true))*100,2)

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

def rank_forecasting_models(
    coverage_analysis,
    metric_config=None,
):

    import pandas as pd
    import numpy as np

    if isinstance(coverage_analysis, str):
        df = pd.read_csv(coverage_analysis)
    else:
        df = coverage_analysis.copy()

    drop_cols = [c for c in ["date", "Unnamed: 0"] if c in df.columns]

    if drop_cols:
        df = df.drop(columns=drop_cols)

    if metric_config is None:

        metric_config = {
            "Coverage80": {
                "weight": 3,
                "target": 90,
                "target_weight": 0.50,
            },
            "Coverage85": {
                "weight": 5,
                "target": 85,
                "target_weight": 0.50,
            },
            "Coverage90": {
                "weight": 8,
                "target": 75,
                "target_weight": 0.60,
            },
            "SMAPE": {
                "weight": 3,
                "target": 95,
                "target_weight": 0.40,
            },
            "RMSE": {
                "weight": 2,
                "target": None,
            },
            "MAE": {
                "weight": 5,
                "target": None,
            },
            "R2": {
                "weight": 1,
                "target": 0.999,
                "target_weight": 0.50,
            },
        }

    total_weight = sum(
        cfg["weight"]
        for cfg in metric_config.values()
    )

    metric_config = {
        k: {
            **v,
            "weight": v["weight"] / total_weight,
        }
        for k, v in metric_config.items()
    }

    models = sorted(
        {
            col.split("_", 1)[1]
            for col in df.columns
            if "_" in col
        }
    )

    model_stats = []

    for model in models:

        row = {"Model": model}

        for metric in metric_config:

            col = f"{metric}_{model}"

            if col in df.columns:
                row[metric] = df[col].mean()

        model_stats.append(row)

    results = pd.DataFrame(model_stats)

    scores = results.copy()

    metrics_high_is_better = {
        "Coverage80",
        "Coverage85",
        "Coverage90",
        "SMAPE",
        "R2",
    }

    for metric, cfg in metric_config.items():

        if metric not in scores.columns:
            continue

        values = scores[metric]

        vmin = values.min()
        vmax = values.max()

        # -----------------
        # Ranking score
        # -----------------

        if np.isclose(vmax, vmin):

            ranking_score = pd.Series(
                10.0,
                index=scores.index,
            )

        else:

            if metric in metrics_high_is_better:

                ranking_score = (
                    (values - vmin)
                    / (vmax - vmin)
                    * 10
                )

            else:

                ranking_score = (
                    (vmax - values)
                    / (vmax - vmin)
                    * 10
                )

        # -----------------
        # Target score
        # -----------------

        target = cfg.get("target", None)

        if target is None:

            final_metric_score = ranking_score

        else:

            max_distance = np.max(
                np.abs(values - target)
            )

            if np.isclose(max_distance, 0):

                target_score = pd.Series(
                    10.0,
                    index=scores.index,
                )

            else:

                target_score = (
                    10
                    * (
                        1
                        - np.abs(values - target)
                        / max_distance
                    )
                ).clip(
                    lower=0,
                    upper=10,
                )

            target_weight = cfg.get(
                "target_weight",
                0.50,
            )

            final_metric_score = (
                (1 - target_weight)
                * ranking_score
                + target_weight
                * target_score
            )

        scores[f"{metric}_score"] = final_metric_score

    scores["FinalScore"] = 0.0

    for metric, cfg in metric_config.items():

        col = f"{metric}_score"

        if col in scores.columns:

            scores["FinalScore"] += (
                scores[col]
                * cfg["weight"]
            )

    scores["FinalScore"] = scores["FinalScore"].round(2)

    ranking = (
        scores.sort_values(
            "FinalScore",
            ascending=False,
        )
        .reset_index(drop=True)
    )

    ranking.insert(
        0,
        "Rank",
        range(1, len(ranking) + 1),
    )

    return ranking

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


use_case = "...."
location_id = 0

init_train = "1900-01-01"
end_train = "2025-12-01"

with open("locations.json") as file:
    locations = json.load(file)

location_info = ObtainCoordinates(locations, use_case, location_id)

weather_info = ObtainWeatherValues(
    location_info[0], location_info[1], 
    min_date ="2025-01-01",
    max_date = "2026-01-01"
)

weather_info["unique_id"] = use_case

weather_info = interpolate_timeseries_15min(
    weather_info.drop(["date"], axis = 1), "unique_id", "ds"
)

### MANAGE DATA  ####

data = pd.read_csv(r"....")

train_data = data[(data["ds"] <= end_train) & (data["ds"] >= init_train)]
test_data = data[data["ds"] >= end_train]


models = [

    # Baseline lineal
    LinearRegression(),

    # Lineales regularizados
    Ridge(alpha=1.0, random_state=0),
    Lasso(alpha=0.001, random_state=0),
    ElasticNet(alpha=0.001, l1_ratio=0.5, random_state=0),

    # LightGBM
    LGBMRegressor(
        n_estimators=500,
        learning_rate=0.03,
        num_leaves=31,
        subsample=0.8,
        colsample_bytree=0.8,
        random_state=0,
        verbosity=-1,
    ),

    # LightGBM más complejo
    LGBMRegressor(
        n_estimators=1000,
        learning_rate=0.02,
        num_leaves=64,
        max_depth=8,
        min_child_samples=20,
        subsample=0.8,
        colsample_bytree=0.8,
        random_state=0,
        verbosity=-1,
    ),

    # XGBoost
    XGBRegressor(
        n_estimators=500,
        learning_rate=0.03,
        max_depth=6,
        subsample=0.8,
        colsample_bytree=0.8,
        objective="reg:squarederror",
        random_state=0,
    ),

    # CatBoost
    CatBoostRegressor(
        iterations=500,
        learning_rate=0.03,
        depth=6,
        loss_function="RMSE",
        verbose=False,
        random_state=0,
    ),

    # Random Forest
    RandomForestRegressor(
        n_estimators=500,
        max_depth=12,
        min_samples_leaf=5,
        random_state=0,
        n_jobs=-1,
    ),

    # Extra Trees
    ExtraTreesRegressor(
        n_estimators=500,
        max_depth=12,
        min_samples_leaf=5,
        random_state=0,
        n_jobs=-1,
    ),

    # Gradient Boosting clásico
    GradientBoostingRegressor(
        n_estimators=300,
        learning_rate=0.05,
        max_depth=4,
        random_state=0,
    ),

    # KNN
    KNeighborsRegressor(
        n_neighbors=20,
        weights="distance",
    ),
]

models = [

    # Baseline lineal
    LinearRegression(),
    # Lineales regularizados
    Ridge(alpha=1.0, random_state=0),
    Lasso(alpha=0.001, random_state=0),
    ElasticNet(alpha=0.001, l1_ratio=0.5, random_state=0)]


list_lags, dict_lags = LagList(7, "15min")

fcst = MLForecast(
    models=models,
    freq='15min',
    lags= list_lags,
    lag_transforms=dict_lags,
    date_features=['dayofweek', 'year', 'month', "hour"],
    target_transforms=[Differences([1])]
)

train_data["ds"] = train_data["ds"].progress_apply(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"))
test_data["ds"] = test_data["ds"].progress_apply(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"))

train_data = pd.merge(train_data, weather_info.drop(["unique_id"], axis = 1), on = "ds")
test_data = pd.merge(test_data, weather_info.drop(["unique_id"], axis = 1), on = "ds")

fcst.fit(train_data, static_features=[])

train_last_date = datetime.strptime("2025-12-15", "%Y-%m-%d")
list_dicts = []

for i in tqdm(range(10)):
    try:

        dict_ = {
            "date": (train_last_date + timedelta(days = i)).isoformat()
        }

        if i == 0:
            result = fcst.predict(h = 96, X_df= test_data.reset_index().drop(["index", "y"], axis = 1))
        else:
            test_data_filter = test_data[test_data["ds"] <= (train_last_date + timedelta(days = i))].copy()
            new_df = pd.concat([train_data, test_data_filter], ignore_index= True)
            result = fcst.predict(new_df= new_df, h = 96, X_df= test_data.reset_index().drop(["index", "y"], axis = 1))
        result = pd.merge(test_data, result, on = "ds")

        for model_ in result.drop(['ds', 'unique_id_x', 'y', 'unique_id_y'], axis = 1).columns:
            if model_ == "yhat":
                model_name = "Chronos-2"
            else:
                model_name = model_

            dict_[f"Coverage80_{model_name}"] = Coverage(result[result['y'] > 0.1]['y'], result[result['y'] > 0.1][model_], 0.8)
            dict_[f"Coverage85_{model_name}"] = Coverage(result[result['y'] > 0.1]['y'], result[result['y'] > 0.1][model_], 0.85)
            dict_[f"Coverage90_{model_name}"] = Coverage(result[result['y'] > 0.1]['y'], result[result['y'] > 0.1][model_], 0.9)
            dict_[f"SMAPE_{model_name}"] = SMAPE(result[result['y'] > 0.1]['y'], result[result['y'] > 0.1][model_])
            dict_[f"RMSE_{model_name}"] = RMSE(result[result['y'] > 0.1]['y'], result[result['y'] > 0.1][model_])
            dict_[f"MAE_{model_name}"] = MAE(result[result['y'] > 0.1]['y'], result[result['y'] > 0.1][model_])
            dict_[f"R2_{model_name}"] = R2(result[result['y'] > 0.1]['y'], result[result['y'] > 0.1][model_])
        list_dicts.append(dict_)
    except Exception as e:
        print(f"ERROR: {e}")
        pass

pd.DataFrame(list_dicts).to_csv(f"metrics_analysis_{use_case}_weather.csv", index= False)

rank_forecasting_models(pd.DataFrame(list_dicts))[['Rank', 'Model', 'Coverage80_score', 'Coverage85_score',
       'Coverage90_score', 'SMAPE_score', 'RMSE_score', 'MAE_score',
       'R2_score', 'FinalScore']].to_csv(f"metrics_analysis_{use_case}_weather.csv", index = False)

fcst.save("ml_test_weather")