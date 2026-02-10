class Forge:
    ''' Dynamic class for timeseries processing before model training'''

    def __init__(self, process_type = None):
        self.process_type = process_type
    
    def process(self):
        pass

def AnomalyInterpolation(self, data, time_col, val_col, process_type):
    
    from adtk.data import validate_series
    from adtk.detector import LevelShiftAD
    import pandas as pd

    def load_data(data,time_col,y_column):
        df = data.copy()
        df.index = pd.to_datetime(df[time_col])
        df.drop(columns=time_col,inplace=True)
        df[y_column] = df[y_column].astype(float)
        return df[[y_column]]

    def level_shift_anomaly(series,config={'c':1,'side':'both','window':3}):
        s = validate_series(series)
        model = LevelShiftAD(c=config['c'], side=config['side'],window = config['window'])            
        anomalies = model.fit_detect(s)
        return anomalies, model
    
    def DropAnomalies(list_):
        real_value = list_[0]
        anomaly_ = list_[1]

        if anomaly_ == True:
            return None
        else:
            return real_value
    
    print(f"[FAST - Processing]: {process_type}")
    
    df = load_data(data, time_col, val_col)
    anomalies, model = level_shift_anomaly(df)
    anomalies = anomalies.reset_index().fillna(False)
    anomalies.columns = [time_col, "anomaly"]
    anomalies_df = pd.merge(df, anomalies, on = time_col)

    anomalies_df["drop_values"] = anomalies_df[[val_col, "anomaly"]].apply(DropAnomalies, axis = 1)
    anomalies_df["final_values"] = anomalies_df["drop_values"].interpolate()

    final_df = anomalies_df[[time_col, "final_values"]]
    final_df.columns = [time_col, val_col]

    return final_df
    
Forge.process = classmethod(AnomalyInterpolation)
forge = Forge("Anomaly Interpolation")
import dill
import os

os.chdir("..")
os.chdir("..")
os.chdir(r"workflow/fast_objects/1_forge")
with open('anomaly_interpolation.pkl', 'wb') as outp:
    dill.dump(forge, outp)