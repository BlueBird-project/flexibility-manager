# TAIMU Guide

![alt text](6.png)

This folder contains:

- A Dockerized runner for FAST inference using [fast_inference.py](fast_inference.py), [Dockerfile](Dockerfile), and [docker-entrypoint.sh](docker-entrypoint.sh)
- A FastAPI server in [main.py](main.py), configured by the adjacent [api_config.json](api_config.json)

## Folder and structure to download

This shows how the executable folder should look like for it to work. 

```text
Taimu/
├── api/
│   ├── main_inference.py
|   ├── fast_inference.py
|   └── api_config.json
├── docker_images/
│   ├── DockerfileForecasting
│   └── Dockerfile
├── FAST/
│   ├── forge
│   │    ├──positive_pv.py
│   │    └──....     
├── ├── translate
│   │
│   └── ...
└── README.md
```

## FastAPI mode

The API is configuration-driven and does not require changes to forge/translate classes.

The file [api_config.json](api_config.json) is required and is loaded automatically when running:

```bash
uvicorn main:app --host 0.0.0.0 --port 8000
```

You can also choose a different config file without editing [api_config.json](api_config.json) by setting an environment variable:

```bash
FAST_API_CONFIG=./api_config_alternative.json uvicorn main:app --host 0.0.0.0 --port 8000
```

Absolute-path example:

```bash
FAST_API_CONFIG=/absolute/path/to/my_config.json uvicorn main:app --host 0.0.0.0 --port 8000
```

### Run FastAPI in Docker

Build the API image with [DockerfileForecasting](DockerfileForecasting):

```bash
docker build -t fast-forecast-api -f docker_images/DockerfileForecasting .
```

Run with the default in-image config:

```bash
docker run --rm -p 8000:8000 fast-forecast-api
```

Run with a local custom config file mounted into the container:

```bash
docker run --rm -p 8000:8000 \
  -v /absolute/path/to/configs:/app/configs \
  -e FAST_API_CONFIG=/app/configs/api_config_custom.json \
  fast-forecast-api
```

The API will be available at http://127.0.0.1:8000/docs.

### API config file

Example [api_config.json](api_config.json):

```json
{
  "path_fast": ".",
  "forges": ["weather_vals", "interpolate_15min", "positive_pv"],
  "translate": ["chronos_2"],
  "config_files": {
    "forge": [{"location": "Namur, Belgium"}, {}, {}],
    "translate": [{"model_path": "amazon/chronos-2"}]
  },
  "default_horizon_span": 96
}
```

### Endpoints

- `GET /health`
- `POST /forecast/day-ahead`
- `POST /forecast/batch`

### Input options (both endpoints)

Use exactly one input source:

- `data`: dictionary with `ds`, `unique_id`, `y`
- `csv_file`: path to a CSV/feather/xlsx/xls file readable by the API process

### Day-ahead request example

```bash
curl -X POST http://localhost:8000/forecast/day-ahead \
  -H "Content-Type: application/json" \
  -d '{
    "horizon_span": 96,
    "data": {
      "ds": ["2026-10-20 00:00:00", "2026-10-20 00:15:00"],
      "unique_id": ["asset_1", "asset_1"],
      "y": [120.5, 122.1]
    }
  }'
```

### Batch request example

```bash
curl -X POST http://localhost:8000/forecast/batch \
  -H "Content-Type: application/json" \
  -d '{
    "start_date": "2026-10-01",
    "end_date": "2026-10-07",
    "horizon_span": 96,
    "csv_file": "./my_input_name.csv"
  }'
```

### API response format

Both endpoints return:

```json
{
  "ds": ["..."],
  "unique_id": ["..."],
  "yhat": [0.0]
}
```

`batch` mode internally simulates daily runs between `start_date` and `end_date` using the same behavior as the script backtest mode.

## Docker mode

## What this Docker image does

The container runs the FAST inference pipeline and lets you control input/output paths and behavior via environment variables.

This means your files can be named anything, as long as you pass the correct path through environment variables.

## Files in this setup

- [Dockerfile](Dockerfile): Builds the runtime image
- [requirements.txt](requirements.txt): Python dependencies
- [docker-entrypoint.sh](docker-entrypoint.sh): Converts environment variables into CLI arguments for [fast_inference.py](fast_inference.py)

## 1. Build the image

Run from the repository root:

```bash
docker build -t fast-inference ./forecasting/FAST
```

## 2. Prepare your host data folder

Put your source file in a local folder, for example:

- /absolute/path/to/data/my_input_name.csv

The container will mount this folder to /app/data.

## 3. Run inference (production mode)

```bash
docker run --rm \
  -v /absolute/path/to/data:/app/data \
  -e INPUT_FILE=/app/data/my_input_name.csv \
  -e OUTPUT_FILE=/app/data/my_output_name.feather \
  -e FORGES="weather_vals interpolate_15min positive_pv" \
  -e TRANSLATE="chronos_2" \
  -e MODE=production \
  -e HORIZON_SPAN=96 \
  fast-inference
```

After completion, the output file is written to your mounted host folder.

## 4. Run backtest mode

```bash
docker run --rm \
  -v /absolute/path/to/data:/app/data \
  -e INPUT_FILE=/app/data/my_input_name.csv \
  -e OUTPUT_FILE=/app/data/my_backtest_output.feather \
  -e FORGES="weather_vals interpolate_15min positive_pv" \
  -e TRANSLATE="chronos_2" \
  -e MODE=backtest \
  -e START_DATE=2026-01-01 \
  -e END_DATE=2026-01-31 \
  -e HORIZON_SPAN=96 \
  fast-inference
```

## Environment variables

All variables are optional because defaults exist, but in practice you should set at least INPUT_FILE and OUTPUT_FILE.

- INPUT_FILE
  - Default: /app/data/input.csv
  - Path to source data file (.csv, .feather, .xlsx, .xls)
- OUTPUT_FILE
  - Default: /app/data/forecasts.feather
  - Path where forecasts are written
- PATH_FAST
  - Default: /app
  - Base path for forge/translate module loading
- FORGES
  - Default: weather_vals interpolate_15min positive_pv
  - Space-separated forge names from the forge folder, without .py
- TRANSLATE
  - Default: chronos_2
  - Space-separated translate names from the translate folder, without .py
- MODE
  - Default: production
  - Allowed: production, backtest
- HORIZON_SPAN
  - Default: 96
  - Forecast horizon passed to the pipeline
- START_DATE
  - Default: empty
  - Required only for MODE=backtest, format YYYY-MM-DD
- END_DATE
  - Default: empty
  - Required only for MODE=backtest, format YYYY-MM-DD

## Input file format expectations

The input file should include at least these columns:

- ds (datetime)
- unique_id (series/entity id)
- y (target)

Additional columns are allowed.

## Using different file names

You do not need to rename your files. Just map your folder and set INPUT_FILE and OUTPUT_FILE accordingly.

Example:

```bash
-v /Users/you/project_data:/app/data \
-e INPUT_FILE=/app/data/energy_series_october.csv \
-e OUTPUT_FILE=/app/data/forecast_october.feather
```

## Troubleshooting

- If the model download is slow or fails, retry with a stable internet connection (Chronos model is loaded from model_path in the pipeline config).
- If your output falls back to CSV, the script encountered a feather write issue and writes forecast_test.csv.
- If a forge or translate name is wrong, verify the file exists in [forge](forge) or [translate](translate) and pass the filename without .py.
