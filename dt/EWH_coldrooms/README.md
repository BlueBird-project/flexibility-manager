# BlueBird EWH Coldroom digital twin Simulation

## Overview

This code simulates the five cold rooms in the EWH-coldrooms pilot. The pilot contains 4 fridges (cooled by one modulated compressor and one on/off compressor) and 1 freezer (cooled by one on/off compressor). The simulation is an implementation of the model described in the deliverable.

## Setup Instructions

### Prerequisites

- Python 3.7 or higher
- pip (Python package manager)

### Step 1: Create a Virtual Environment

A good idea, but not strictly requried. It is also possible to run the code anywhere you have an active python envoriment. 

### Step 2: Install Dependencies

With the virtual environment activated, install the required packages:

pip install -r requirements.txt

### Step 3: Run the Simulation

Execute the main simulation script: python main_multiroom_final.py

The simulation will run and display two plots illustrating the simulation:

Figure 1: Room temperatures with setpoints, door events, and overall compressor power usage
Figure 2: Per-room cooling allocation with overlaid door load demands

