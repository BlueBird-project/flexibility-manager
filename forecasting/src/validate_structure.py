"""Checks that the folders/files needed by price_inference.py exist in the current folder."""
from pathlib import Path

BASE = Path.cwd()

"""
.
├── docker-compose-flow.yml
├── DockerfileFlow
├── requirements-flow.txt
├── .env                       # you create it (step 3)
├── credentials_entsoe.json    # {"token": "YOUR_ENTSOE_KEY"}
├── prefect_price_flow_psq.py
├── price_inference_psq.py
├── price_inference.py
├── db_forecast_psq.py
├── db_forecast.py
├── db_forecast_pg.sql
├── fast_inference.py
├── forge/
│   └── pricefm_data.py
└── translate/
    └── pricefm_plus.py

"""

REQUIRED_DIRS = ["forge", "translate", "pricefm"]
REQUIRED_FILES = [
    "price_inference.py",
    "fast_inference.py",
    "db_forecast.py",
    "db_forecast_psq.py",
    "fast_inference.py",
    "price_inference_psq.py",
    "prefect_price_flow.py",
    "requirements.txt",
    "credentials_entsoe.json",
    "forge/pricefm_data.py",
    "translate/pricefm_plus.py",
    "pricefm/Scripts/python.exe",
    "docker-compose-flow.yml"
]

missing_dirs = [d for d in REQUIRED_DIRS if not (BASE / d).is_dir()]
missing_files = [f for f in REQUIRED_FILES if not (BASE / f).is_file()]

if missing_dirs or missing_files:
    print(f"Structure NOT valid in {BASE}")
    for d in missing_dirs:
        print(f"  Missing folder: {d}")
    for f in missing_files:
        print(f"  Missing file:   {f}")
else:
    print("Structure validated")
