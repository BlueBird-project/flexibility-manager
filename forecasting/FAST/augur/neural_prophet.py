class Augur:
    ''' Dynamic class for timeseries forecast '''

    def __init__(self, model_name = None, type_model = None, training_date = None, metric = {}):
        self.model_name = model_name
        self.type_model = type_model
        self.training_date = training_date
        self.metric = metric
        self.model = None
    
def Train_NP(
    self,
    train_data,
    test_data,
    ds_column,
    value_column,
    freq = "1H"
):
    
    from neuralprophet import NeuralProphet

    data_to_train = train_data[[ds_column, value_column]]

    data_to_train.columns = ["ds", "y"]

    data_to_train = data_to_train.groupby(["ds"]).sum().reset_index().drop(["index"], axis = 1, errors = "ignore")

    m = NeuralProphet(
        yearly_seasonality = 12,
        weekly_seasonality=7,
        daily_seasonality=24,
        trend_reg=1,
        learning_rate=0.01,
    )
    
    df_train, df_test = m.split_df(data_to_train, freq=freq, valid_p= 1.0/ 30)

    metrics = m.fit(df_train, freq=freq, validation_df=df_test, progress="bar")

    self.model = m

    future = m.make_future_dataframe(data_to_train, periods = 30)
    forecast = m.predict(future)

    return_forecast = forecast[["ds", "yhat1"]]
    return_forecast.columns = ["ds", "yhat"]

    metrics_train = {
        "rmse_train": min(metrics["RMSE_val"]),
        "mae_train": min(metrics["MAE_val"]),
        "loss_train": min(metrics["Loss_val"])
    }

    return metrics_train, return_forecast

def Save_NP(self, model_name):

    from neuralprophet import save

    if self.model != None:
        save(self.model, f"{model_name}.np")

Augur.train = classmethod(Train_NP)
Augur.save = classmethod(Save_NP)

augur = Augur()

import dill

with open('prophet.pkl', 'wb') as outp:
    dill.dump(augur, outp)