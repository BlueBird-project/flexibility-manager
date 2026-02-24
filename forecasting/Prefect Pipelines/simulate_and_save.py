from prefect import flow, task
from datetime import datetime
from tqdm import tqdm
import pandas as pd

@task
def Simulate(db, days_ahead = 1) -> pd.DataFrame:
    import pandas as pd
    from datetime import datetime
    import sqlite3
    from chronos import Chronos2Pipeline

    conexion = sqlite3.connect(db)
    cursor = conexion.cursor()
    results = cursor.execute(f""" SELECT * FROM energy_values""")
    res = results.fetchall()
    conexion.close()
    values = pd.DataFrame(res)
    values.columns = ["index", "ds", "unique_id", "y"]
    values["ds"] = values["ds"].apply(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"))
    pipeline = Chronos2Pipeline.from_pretrained("amazon/chronos-2", device_map="cuda")
    pred_df = pipeline.predict_df(
        values,
        prediction_length=24*4*days_ahead,  # Number of steps to forecast
        quantile_levels=[0.1, 0.5, 0.9],  # Quantiles for probabilistic forecast
        id_column="unique_id",  # Column identifying different time series
        timestamp_column="ds",  # Column with datetime information
        target="y",  # Column(s) with time series values to predict
    )

    return pred_df

@task
def Process(datos_all) -> pd.DataFrame:
    
    datos_all["ds"] = datos_all["ds"].apply(lambda x: datetime.strftime(x, "%Y-%m-%d %H:%M:%S"))
    datos_all["value"] = datos_all["predictions"].apply(lambda x: 0 if pd.isna(x) else x)
    return datos_all

@task
def StoreInDB(db_file = "", 
              datos_all = pd.DataFrame()):

    import sqlite3
    import numpy as np

    datos_all.tail(5)
    list_adds = []
    
    # CREATE LIST OF ADDITIONS
    for index, row in tqdm(datos_all.iterrows(), total = datos_all.shape[0]):
        list_adds.append((row["ds"], row["unique_id"], np.round(row["value"],4)))

    print(list_adds[0:5])
    # ADD VALUES TO DB
    try:
        conexion = sqlite3.connect(db_file)
        cursor = conexion.cursor()

        cursor.executemany("""
            INSERT INTO energy_values (ds, asset_id, value) 
            VALUES (?, ?, ?)
            ON CONFLICT (ds, asset_id) DO UPDATE
            SET value = excluded.value
        """, list_adds)

        conexion.commit()
        conexion.close()
    except IndexError:
        pass


@flow(
        name = "Simulate and Save",
        version = "1.0",
        description= "Downloads REE data and saves it in the database"
)
def SimulateAndSave(db_file, days_ahead = 1):

    list_data = Simulate(db_file, days_ahead)
    datos_all = Process(list_data)
    StoreInDB(db_file = db_file,datos_all=datos_all)


if __name__ == "__main__":
    SimulateAndSave()



