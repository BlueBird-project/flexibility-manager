class BaseForge:

    name = "base_forge"

    def process(self, data):
        raise NotImplementedError

class weatherVals(BaseForge):

    import pandas as pd

    def ObtainCoordinates(self, dict_):

        from geopy.geocoders import Nominatim

        geolocator = Nominatim(user_agent="BB")

        if type(dict_["location"]) != str:
            return dict_["location"]
        else:
            direccion = dict_["location"]
            location = geolocator.geocode(direccion)

            if not location:   
                ic("No se encontró la dirección")

            return [location.latitude, location.longitude]

    name = "obtain_weather_values"

    def __init__(
        self,
        weather_columns=None
    ):

        self.weather_columns = (
            weather_columns
            or [
                "temperature_2m", "relative_humidity_2m", "apparent_temperature", "wind_speed_10m", "direct_radiation"
            ]
        )


    def ObtainWeatherValues(self, lat, lon, min_date, max_date):

        import openmeteo_requests
        import pandas as pd
        import requests_cache

        from datetime import datetime, timedelta
        from retry_requests import retry

        # --------------------------------------------------
        # Dates
        # --------------------------------------------------

        if type(min_date) == "str":
            min_ts = datetime.strptime(min_date, "%Y-%m-%d")
        else:
            min_ts = min_date

        if type(max_date) == "str":
            max_ts = datetime.strptime(max_date, "%Y-%m-%d") + timedelta(days = 2)
        else:
            max_ts = max_date + timedelta(days = 2)

        today = datetime.now()

        # --------------------------------------------------
        # OpenMeteo client
        # --------------------------------------------------

        cache_session = requests_cache.CachedSession(
            ".cache",
            expire_after=3600
        )

        retry_session = retry(
            cache_session,
            retries=5,
            backoff_factor=0.2
        )

        openmeteo = openmeteo_requests.Client(
            session=retry_session
        )

        weather_frames = []

        variables = [
            "temperature_2m",
            "relative_humidity_2m",
            "apparent_temperature",
            "wind_speed_10m",
            "direct_radiation",
        ]

        # --------------------------------------------------
        # HISTORICAL PART
        # --------------------------------------------------

        historical_end = min(max_ts, today - pd.Timedelta(days=1))

        if min_ts <= historical_end:

            url = "https://archive-api.open-meteo.com/v1/archive"

            params = {
                "latitude": lat,
                "longitude": lon,
                "start_date": min_ts.strftime("%Y-%m-%d"),
                "end_date": historical_end.strftime("%Y-%m-%d"),
                "hourly": variables,
            }

            response = openmeteo.weather_api(
                url,
                params=params
            )[0]

            hourly = response.Hourly()

            df_hist = pd.DataFrame(
                {
                    "date": pd.date_range(
                        start=pd.to_datetime(
                            hourly.Time(),
                            unit="s",
                            utc=True
                        ),
                        end=pd.to_datetime(
                            hourly.TimeEnd(),
                            unit="s",
                            utc=True
                        ),
                        freq=pd.Timedelta(
                            seconds=hourly.Interval()
                        ),
                        inclusive="left",
                    ),
                    "temperature_2m": hourly.Variables(0).ValuesAsNumpy(),
                    "relative_humidity_2m": hourly.Variables(1).ValuesAsNumpy(),
                    "apparent_temperature": hourly.Variables(2).ValuesAsNumpy(),
                    "wind_speed_10m": hourly.Variables(3).ValuesAsNumpy(),
                    "direct_radiation": hourly.Variables(4).ValuesAsNumpy(),
                }
            )

            weather_frames.append(df_hist)

        # --------------------------------------------------
        # FORECAST PART
        # --------------------------------------------------

        forecast_start = max(min_ts, today)

        if forecast_start <= max_ts:

            url = "https://api.open-meteo.com/v1/forecast"

            forecast_days = max(
                1,
                int((max_ts - forecast_start).days) + 1
            )

            params = {
                "latitude": lat,
                "longitude": lon,
                "forecast_days": min(forecast_days, 16),
                "hourly": variables,
            }

            response = openmeteo.weather_api(
                url,
                params=params
            )[0]

            hourly = response.Hourly()

            df_forecast = pd.DataFrame(
                {
                    "date": pd.date_range(
                        start=pd.to_datetime(
                            hourly.Time(),
                            unit="s",
                            utc=False
                        ),
                        end=pd.to_datetime(
                            hourly.TimeEnd(),
                            unit="s",
                            utc=False
                        ),
                        freq=pd.Timedelta(
                            seconds=hourly.Interval()
                        ),
                        inclusive="left",
                    ),
                    "temperature_2m": hourly.Variables(0).ValuesAsNumpy(),
                    "relative_humidity_2m": hourly.Variables(1).ValuesAsNumpy(),
                    "apparent_temperature": hourly.Variables(2).ValuesAsNumpy(),
                    "wind_speed_10m": hourly.Variables(3).ValuesAsNumpy(),
                    "direct_radiation": hourly.Variables(4).ValuesAsNumpy(),
                }
            )



            df_forecast = df_forecast[
                df_forecast["date"] >= forecast_start
            ]

            weather_frames.append(df_forecast)

        # --------------------------------------------------
        # MERGE
        # --------------------------------------------------

        if len(weather_frames) == 0:
            return pd.DataFrame()

        weather = pd.concat(
            weather_frames,
            ignore_index=True
        )

        weather = weather.drop_duplicates(
            subset=["date"]
        )


        weather["ds"] = weather["date"].apply(lambda x: datetime.strftime(x, "%Y-%m-%d %H:%M:%S"))
        weather = weather.drop(["date"], axis = 1, errors = "ignore")
        return weather.reset_index(drop=True)


    def process(self, data: pd.DataFrame, config_process: dict):
        from datetime import datetime, timedelta
        import pandas as pd

        # Init and finish dates
        if data.shape[0] != 0:
            start_date = data["ds"].min()
            end_date = data["ds"].max() 
        else:
            start_date = datetime.strftime(datetime.now() - timedelta(days = 14), "%Y-%m-%d")
            end_date = datetime.strftime(datetime.now() + timedelta(days = 7), "%Y-%m-%d")
        location = config_process["location"]
        #####(location)
        lat, lon = self.ObtainCoordinates({"location": location})
        weather = self.ObtainWeatherValues(lat, lon, start_date, end_date)

        weather["ds"] = pd.to_datetime(weather["ds"])

        if data.shape[0] == 0:
            return weather
        else:
            merged = data.merge(
                weather[
                    ["ds"] + self.weather_columns
                ],
                on="ds",
                how="left"
            )
    
            return merged

import dill

forge = weatherVals()

with open("forge_weather_vals.dill", "wb") as f:
    dill.dump(forge, f)


