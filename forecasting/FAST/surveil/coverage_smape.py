class Surveil:
    def __init__(self, surveil_type = None):
        self.surveil_type = surveil_type
    

def MetricsForecasting(self,
                       actuals, 
                       predictions,
                       return_dict = False,
                       return_plot = False):
    
    import plotly.graph_objects as go

    def Coverage(y_true, y_pred, threshold = 0.75):
        import numpy as np
        total_ = 0
        for true, pred in zip(y_true, y_pred):
            if true >= float(pred)*threshold and true <= float(pred)*(2-threshold):
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

    thresholds = {
        "coverage95": 50,
        "coverage75": 80,
        "smape": 90
    }

    type_threhold = {
        "coverage95": 1,
        "coverage75": 1,
        "smape": 1
    }

    metrics = {
        "coverage95": Coverage(actuals, predictions, threshold= 0.95),
        "coverage75": Coverage(actuals, predictions, threshold= 0.75),
        "smape": SMAPE(actuals, predictions)
    }

    approved = True

    for metric in thresholds:
        if thresholds[metric] * type_threhold[metric] > metrics[metric] * type_threhold[metric]:
            approved = False
    
    list_returns = [approved]

    if return_dict:
        list_returns.append(metrics)
    
    if return_plot:
        fig = go.Figure()
        fig.add_trace(
            go.Scatter(x = list(range(len(predictions))), y = predictions, name = "Predictions")
        )
        fig.add_trace(
            go.Scatter(x = list(range(len(actuals))), y = actuals, name = "Real")
        )
        list_returns.append(fig)
    
    return list_returns

Surveil.check = classmethod(MetricsForecasting)
surveil = Surveil(surveil_type= "SMAPE")

import dill
import os

os.chdir("..")
os.chdir("..")
os.chdir(r"workflow/fast_objects/3_surveil")
with open('coverage_smape.pkl', 'wb') as outp:
    dill.dump(surveil, outp)
