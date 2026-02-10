from prefect import task
from prefect.artifacts import create_table_artifact

@task
def GetDataFromDB(db, days_before = None):

    """ 
    This task gets information from the NOVA database. The only necessary input is the name of the database.
    
    :param db: path of the DB File to be used.

    """
    import sqlite3
    import pandas as pd
    from datetime import datetime, timedelta

    # STEP 1 - CONNECTION AND EXECUTION OF SELECT COMMAND
    con = sqlite3.connect(db)
    cur = con.cursor()
    results = cur.execute("""SELECT * FROM energy_values""")
    rows = results.fetchall()
    con.close()

    # STEP 2 - TURN INTO A PANDAS DATAFRAME + DATETIME MANAGEMENT
    data = pd.DataFrame(rows)
    data.columns = ["index", "ds", "unique_id", "y"]
    data["ds"] = data["ds"].apply(lambda x: datetime.strptime(x, "%Y-%m-%d %H:%M:%S"))

    if days_before != None:
        max_date = max(data["ds"]) - timedelta(days = days_before)
        data = data[data["ds"] <= max_date]

    return data[["unique_id", "ds", "y"]].copy()

@task
def Process(data, forge_path: str):

    """
    It executes the process described in the forge_path. It is executed for each unique_id present in the dataframe.
    
    :param data: pandas dataframe to use.
    :param forge_path: Path of the forge object to be used in this process.
    
    """

    import dill
    import pandas as pd

    print(data.columns)

    with open(forge_path, 'rb') as inp:
        forge = dill.load(inp)
    list_data = []
    for uid, group in data.groupby("unique_id", as_index=False):
        processed = forge.process(group, "ds", "y", forge.process_type)
        processed["unique_id"] = uid
        list_data.append(processed)
    return pd.concat(list_data, ignore_index=True) if len(list_data) > 0 else data

@task
def Train(data, path_augur, path_surveil, freq="1h", days_threshold=15, project_name = ""):

    from datetime import timedelta
    import pandas as pd
    import numpy as np
    import dill
    from icecream import ic

    # LOAD AUGUR AND SURVEIL
    with open(path_augur, 'rb') as inp:
        augur = dill.load(inp)
    with open(path_surveil, 'rb') as inp:
        surveil = dill.load(inp)
    
    # SET TRAIN/TEST DATA
    max_date = data["ds"].max()
    train_until = max_date - timedelta(days=days_threshold)
    train_data = data[data["ds"] <= train_until].copy()
    test_data  = data[data["ds"] >  train_until].copy()

    ic(test_data.columns)

    metrics_train, forecast = augur.train(
        train_data=train_data,
        test_data=test_data,
        freq=freq
    )

    metrics_train_table = [{"metric": k, "value": float(np.round(v, 4))} for k, v in metrics_train.items()]
    
    create_table_artifact(key="metric-train", table=metrics_train_table, description="# Métricas train")

    ic(forecast.head(2))

    merged = pd.merge(test_data, forecast[["ds", "unique_id","yhat"]], on=["ds", "unique_id"], how="inner")

    check, metrics_test = surveil.check(merged["y"], merged["yhat"], return_dict=True)

    metrics_test["check"] = check
    metrics_table_test = [{"metric": k, "value": float(v)} for k, v in metrics_test.items()]
    create_table_artifact(key="metrics-test", table=metrics_table_test, description="# Métricas test")

    if True:
        try:
            augur.save(project_name)
        except Exception as e:
            print(f"message: {e}")

    return {"train_metrics": metrics_train, "test_metrics": metrics_test, "passed": bool(check)}

@task
def Forecast(data, path_translate, path_model):

    from icecream import ic
    import dill

    # LOAD TRANSLATE
    with open(path_translate, 'rb') as inp:
        translate = dill.load(inp)

    # LOAD MODEL
    model = translate.load(path_model)

    # INFERENCE
    forecast = translate.predict(model, data)
    
    ic(forecast.head(3))

    return forecast

@task
def InsertIntoDB(db, forecast, name_model):

    import sqlite3
    from datetime import datetime

    try:
        list_forecast = []
        for index, row in forecast.iterrows():
            list_forecast.append(
                (row["ds"], row["unique_id"], row["yhat"], datetime.strftime(datetime.now(), "%Y-%m-%d"), name_model)
            )
    except Exception as e:
        print(f"Failure Message - List Forecast Assemble: {e}")
    
    try:
        conexion = sqlite3.connect(db)
        cursor = conexion.cursor()

        cursor.executemany("""
            INSERT INTO forecasts (ds, asset_id, value, ds_forecast, model) 
            VALUES (?, ?, ?, ?, ?)
        """, list_forecast)

        conexion.commit()
        conexion.close()
    except Exception as e:
        print(f"Failure Message - Commmit to DB: {e}")
    
    print("Values added")




