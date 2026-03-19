# PIPELINES

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#install">Install</a>
    </li>
    <li>
      <a href="#execution">Execution</a>
    </li>
    <li>
      <a href="#explanation">Explanation</a>
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

## Execution



### 1. Install requre
 


## Replication

How to use the FAST Pipelines System to create your own pipelines. 