from prefect import flow
import json
from tasks.tasks_nova import GetDataFromDB, Process, Forecast, InsertIntoDB

@flow(name = "nova-forecast-pipeline")
def main():

    db = r"c:\Users\adrian.carrasco\Documents\Nova Projects\DB\WeSmart.db"
    list_forges = [r"fast_objects\1_forge\anomaly_interpolation.pkl"]
    path_translate = r"fast_objects\4_translate\nf.pkl"
    model_test = r"C:\Users\adrian.carrasco\Desktop\Projects\BlueBird\models\WeSmart\NF_2026_02_04"

    # TASK 1 - IMPORT DATA
    data = GetDataFromDB(db, days_before= 0)

    # TASK 2 - PROCESS
    process_data = data.copy()
    for forge_path in list_forges:
        process_data = Process(process_data, forge_path)

    # TASK 3 - FORECAST
    forecast = Forecast(process_data, 
                        path_translate=path_translate, 
                        path_model = model_test)
    
    InsertIntoDB(db, forecast, model_test)

if __name__ == "__main__":
    main()
    
    