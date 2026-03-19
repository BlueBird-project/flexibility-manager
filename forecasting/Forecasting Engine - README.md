# PIPELINES

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#install">Install</a>
    </li>
    <li>
      <a href="#explanation">Explanation</a>
    </li>
    <li>
      <a href="#execution">Execution</a>
    </li>
    <li>
      <a href="#replication">Replication</a>
    </li>
  </ol>
</details>

## Install

This includes the main steps to install the prefect pipelines as well as the environment recommended for its execution. It has been tested with Python 3.10.11 and CUDA 12.8 for PyTorch. As the FAST Objects are intended to be customizable the requirements are for some of the objects shown in examples.

## Explanation

There are three folders in this sub-repository:

- DB: Which contains the schema of the database that will be used to store the forecasting, both of the energy assets and the price values.
- Prefect Pipelines: There are three pipelines to be shown here, simulate, train and forecast. In this folder, there is also the library tasks to incorporate the different tasks of the pipelines for an easier development.
- FAST: The Prefect Pipelines use FAST objects (as explained in the documentation). In this folder a sample of FAST Objects are given as examples. 
- api: Example of the API created using FASTAPI from a pretrained model (not fully implemented yet)

## Execution

Following these steps, you should be able to run your own forecasting algorithm.

### **1. Install requirements.**

Either the included in this repository or a custom one that enables the use of the FAST Objects.

```bash
pip install -r requirements.txt
```
 
### **2. Create DB** 

Using the schema as a template, a database for the forecasting needs to be created. Then data should be added, specially for the *energy_values* table. 

### **3. Compile FAST Objects** 

This will create a compiled version that the pipelines can execute. From the main folder, it is required just to execute the python scripts with no additional arguments. As an example using the given objects:

```bash

# Forge
python "FAST/1 - Forge/anomaly_interpolation.py"

# Augur
python "FAST/2 - Augur/neural_prophet.py"

# Surveil
python "FAST/3 - Surveil/coverage_smape.py"

# Translate
python "FAST/4 - Translate/neural_prophet.py
```

After which the objects should appear in the *Prefect Pipelines/fast_objects* folder.

### **4. Run Prefect** 

In order to run the pipelines, execute the command:

```bash
prefect server start
```

After which the instance of the local Prefect Server is initiated and the runs can be executed.

To train the models, run the code:

```bash
python "Prefect Pipelines/train_pipeline.py"
```

And once the model is created and saved, the forecast can begin by running the pipeline forecasting as follows:

```bash
python "Prefect Pipelines/forecasting_pipeline.py"
```

## Replication

A guide will be updated here as a reference to create custom FAST Objects and Prefect Pipelines. 