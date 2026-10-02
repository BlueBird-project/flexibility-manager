# Step 0: Requirements and Infos
* Python3 needs to be installed
* Explanation of the variables used is given in `DT-Explanation.md`


# Step 1: Install required dependencies
* `pip3 install -r requirememts.txt`

# Step 2: Generating synthetic dataset
* Tune parameters at the top of the python file to you liking, otherwise defaults are fine.
* Run `python3 waterworks-simulated-data-generator.py`
* CSV file containing the synthetic data according to set parameters will be created

# Step 3: Training and exporting dynamics
* Run `python3 prediction_linear_regression.py`
* A csv containing the weights is saved in the folder `models` called `linear_reg_lag24_weights.csv`
* Prediction plots are saved in the `plots` folder
* Structure of `linear_reg_lag24_weights.csv`
    * The csv has three columns: `target`, `feature`, `weight`
    * `target` column: the variable that is being predicted (e.g. `tank_level_m3`)
        * Formula: `tank_level_m3(t) = weight_1 * feature_1(t-1) + weight_2 * feature_2(t-1) + ... + weight_n * feature_n(t-1)`
    * `feature` column: the variable that is used to predict the target variable (e.g. `inflow_m3h(t-1)`, `outflow_m3h(t-1)`, `tank_level_m3(t-1)`, etc.)
        * Postfix `lagx` indicates the lag of the feature variable (e.g. `inflow_m3h(t-1)` is the inflow variable with a lag of 1 time step, `tank_level_m3(t-24)` is the tank level variable with a lag of 24 time steps, etc.)
    * `weight` column: the weight of the feature variable in the linear regression model with which the feature variable is multiplied to predict the target variable (e.g. `weight_1`, `weight_2`, ..., `weight_n` in the formula above)
    * `intercept` features indicate the intercept for the calculated target