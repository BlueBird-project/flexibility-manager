class Translate:
    def __init__(self, xai_available = False):
        self.xai = xai_available
        self.model = None

def ForecastNP(self, prev_data,key_arguments):

    if self.model != None:
        
        future = self.model.make_future_dataframe(
            prev_data, periods = key_arguments["periods"]
        )

        forecast = self.model.predict(future)

        if self.xai == True:
            xai_to_share = forecast.drop(["y", "yhat1"], axis = 1)
            forecast_to_share = forecast[["ds", "yhat1"]]
            forecast_to_share.columns = ["ds", "yhat"]

            return forecast_to_share, xai_to_share
        else:
            forecast_to_share = forecast[["ds", "yhat1"]]
            forecast_to_share.columns = ["ds", "yhat"]

            return forecast_to_share

    else:
        return None

def LoadNP(self, path = "", model = None):
    if model != None:
        self.model = model
        self.xai = True
    else:
        pass

Translate.load = classmethod(LoadNP)
Translate.predict = classmethod(ForecastNP)

translate = Translate()

import dill

with open('neuralprophet.pkl', 'wb') as outp:
    dill.dump(translate, outp)