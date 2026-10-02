from prefect import flow
from tasks.tasks_nova import GetDataFromDB, Process, Train
import json


@flow(name="Train pipeline")
def main(
    db,
    list_forges,
    list_augur,
    surveil_path,
    list_assets = None
):
    
    db = r""
    list_forges = [r"fast_objects\1_forge\anomaly_interpolation.pkl"]
    list_forges = []
    list_augur = [r"fast_objects\2_augur\nf.pkl"]
    surveil_path = r"fast_objects\3_surveil\coverage_smape.pkl"

    freq = "15T"

    project_name = "WeSmart"

    # TASK 1 - IMPORT
    data = GetDataFromDB(db, list_assets = list_assets)

    # TASK 2 - PROCESS
    process_data = data.copy()
    for forge_path in list_forges:
        process_data = Process(process_data, forge_path)
    
    # TASK 3 - TRAINING
    results = []
    for augur_pkl in list_augur:
        res = Train(process_data, augur_pkl, surveil_path, list_assets,
                    freq, project_name = project_name)
        results.append(res)
    return results

if __name__ == "__main__":
    main()
