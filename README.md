# Raleigh Cool Pavements

This repository contains the R and Python code used to analyze data from the Raleigh cool pavement sensor study.

The Python scripts are translations of the original R analysis scripts and are used to analyze sensor data collected during summer 2026.

## Files

* `05_Create_Analysis_Dataset.py` — Creates the analysis datasets from the sensor and supporting data.
* `06a_Sensor_Models_DID.py` — Runs the main difference-in-differences models.
* `06b_Sensor_Models_DID_wModifiers.py` — Runs the difference-in-differences models with effect modifiers.
* `06c_Sensor_Models_EventStudy.py` — Runs the event study models.
* The original R scripts are included for reference.

## Download and Run

### 1. Download the repository

Download or clone the repository from GitHub:

[GitHub repository](https://github.com/siddhantchoudhary618/2026-Raleigh-Cool-Pavements)

### 2. Download `05_Analysis_Data.csv`

The `05_Analysis_Data.csv` file is too large to include in the GitHub repository.

Download it from the Google Drive folder:

[Google Drive — Analysis Data](https://drive.google.com/drive/u/0/folders/1QQifWeTJp-fgmwNfsDAewCVCrXfda1Qo)

After downloading it, place it here:

```text
raleigh-cool-pavements-main/
└── Data/
    └── Analysis/
        └── 05_Analysis_Data.csv
```

### 3. Install the required Python packages

Create a virtual environment and install the required packages:

```bash
python3 -m venv venv
source venv/bin/activate
pip install pandas numpy geopandas matplotlib seaborn statsmodels
```

### 4. Change `PROJECT_ROOT`

The Python scripts contain a `PROJECT_ROOT` variable near the top of the file. It currently points to the original computer where the code was developed.

Change it to the location of the repository on your computer.

For example:

```python
PROJECT_ROOT = Path("/Users/siddhantchoudhary/Downloads/raleigh-cool-pavements-main")
```

should be changed to:

```python
PROJECT_ROOT = Path("/path/to/raleigh-cool-pavements-main")
```

Update `PROJECT_ROOT` in:

```text
Code/05_Create_Analysis_Dataset.py
Code/06a_Sensor_Models_DID.py
Code/06b_Sensor_Models_DID_wModifiers.py
Code/06c_Sensor_Models_EventStudy.py
```

### 5. Run the scripts

Run the scripts from the repository's main directory in this order:

```bash
python Code/05_Create_Analysis_Dataset.py
python Code/06a_Sensor_Models_DID.py
python Code/06b_Sensor_Models_DID_wModifiers.py
python Code/06c_Sensor_Models_EventStudy.py
```

`05_Create_Analysis_Dataset.py` should be run first because it creates the analysis datasets used by the other scripts.

### 6. Outputs

The Python analysis results are saved in:

```text
Data/Analysis/
├── Python_Analysis_05/
├── Python_Analysis_06a/
├── Python_Analysis_06b/
└── Python_Analysis_06c/
```

Each analysis folder contains the corresponding figures and results.

## Data

The sensor data used in this analysis was collected during summer 2026.
