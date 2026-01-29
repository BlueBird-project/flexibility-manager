# Digital Twin Blue Bird Multizone building

This code contains is a set of functions to implement the Digital Twin (DT) of BC2 - Montcada Building, consisting on a multizone building.

Author: Pablo A. Moreno

## Digital Twin Processes

- **Digital Twin Training Process:** this process consists in training and generating the DT models, which will be used independently later by the other processes to predict or estimate the dynamics at any moment.
- **Prediction process:** this process consists on generating the energy prediction whith the new data coming from optimisation process and the different APIS.
- **Dynamics Estimation process:** this process consists on generating the transformed inputs and the matrix coefficients of the dynamics of the model needed by the optimisation process.

Prediction and Dynamics Estimation processes are independent from each other, so they don't need to be both executed, only the one that is needed. Previously to the use of one of these two processes, the training process must be executed to build and train the DT models that are later used by the other processes.

# Set up the code

## Prerequisites

Previously to using the DT it's required to install **R programming language**.

Next step is installing the required R packages, following the next steps:

1. Install biggr package and its requirements as indicated in R package repository. In the biggr GitHub repository, it explains which requirements it has and how to install them.
2. Once biggr package is installed, it is required to have installed specifically the versión 1.0.0 of onlineforecast package for its correct functioning. This specific version can be found [here](https://cran.r-project.org/src/contrib/Archive/onlineforecast/)

Once the previous requirements are met, it's posible to proceed with the next step, which consists on downloading and configuring the Digital Twin.

## Download and configure the code

The nexts steps must be followed to be able to work with the DT:
1. Download the documents of this GitHub repository
2. Copy the documents on the folder in which is going to work.
3. Configure the config.json file with the following information:
   - **InputDataDirectory:** path to where the files needed for each one of the models are stored. For example, when executing the 	DT_dynamics_estimator it's needed to previously save a file named "df_dynamics_estimator" in this directory.
   - **OutputDataDirectory:**  path to where the results files of each one of the functions. For example, when executing the 	DT_dynamics_estimator, a file named dynamics_estimator_results will be stored in a folder named results that will be 	automatically created in this directory (if it is not previously created).

This conclude the configuration of the DT files. Now the functions are ready to be used to create and use the DT of the BC2 - Montcada Building. All the downloaded files must be in stored in the same folder for the correct operation of the DT.

# Run the Digital Twin 

There are three different functions to run the three different process described before that compose the DT, which consists on:

- **DT_training:** this functions is used to run the Digital Twin Training Process, which builds and train the DT model. To run this process the CSV file named "df_one_hour" must be stored in the input directory. This file must contain the data described in the DT Montcada documentation, and formated as required.
- **DT_predict:** this functions is used to run the Digital Twin Prediction Process, which uses the DT model to predict the energy consumption of the building. To run this process the JSON file named "df_predict" must be stored in the input directory. This file must contain the data described in the DT Montcada documentation, and formated as required.
- **DT_dynamics_estimator:** this functions is used to run the Digital Twin Dynamics Estimation Process, which generates a file containing the matrixes of each model and the transformations of the inputs (the ones that are not controlled variables) required by the models. To run this process the JSON file named "df_dynamics_estimator" must be stored in the input directory. This file must contain the data described in the DT Montcada documentation, and formated as required.


