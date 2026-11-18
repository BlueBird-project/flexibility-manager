# BlueBird forecast API (Chronos-2 / TimesFM 3.0 / Timestral)

Send historical data, get a forecast back. Choose the model per request (`"model": "chronos2" | "timesfm3" | "timestral"`) or via `default_model` in `api/api_config.json`.

## Layout
- `api/main_inference.py` FastAPI app, `api/fast_inference.py` FAST runner, `api/api_config.json` config (`path_fast` is relative to `api/`), `api/api_smoke_test.py`, Postman collection.
- `FAST/forge`, `FAST/translate`: the FAST components used. No data, models, caches or secrets are included.

## Setup
macOS / Linux: `python -m venv .venv && source .venv/bin/activate && pip install -r requirements_api.txt`

Windows (PowerShell): `python -m venv .venv; .venv\Scripts\Activate.ps1; pip install -r requirements_api.txt` (for an NVIDIA GPU install the CUDA build of torch first).

Weights download from Hugging Face on first use. Set `"load_args": {"device": "cuda"}` per model in `api_config.json` to use the GPU (TimesFM/Timestral fall back from MPS to CPU on Mac).
Env var: `FAST_API_CONFIG` = another config JSON (relative to `api/`).

## Run (from this folder)
`uvicorn api.main_inference:app --host 0.0.0.0 --port 8000`

- Endpoints: `GET /health`, `GET /models`, `POST /forecast/day-ahead`, `POST /forecast/batch`
- Example: `curl -X POST http://127.0.0.1:8000/forecast/day-ahead -H "Content-Type: application/json" -d '{"horizon_span":4,"model":"timesfm3","data":{"ds":["2026-10-20 00:00:00","2026-10-20 00:15:00"],"unique_id":["a","a"],"y":[100,101.5]}}'`
- Smoke test: `python api/api_smoke_test.py [model]`

## Notes
TimesFM 3.0 weights are non-commercial. Chronos-2 uses extra columns (e.g. weather) as covariates; TimesFM/Timestral are univariate unless `predict_args.covariate_cols` is set. The weather forge needs internet and a `location` entry in `config_files.forge`. The default config only uses the `interpolate_15min` forge.

## FAST DB API (two-way, SQLite)
Run `cd api && uvicorn fast_db_api:app --port 8001` (db path via env `FAST_DB_PATH`, default `api/fast_db.sqlite`, created from `db_forecast_schema.sql`).
Endpoints: POST /load_model, GET /status, POST /push_data, POST /forecast (returns yhat, yhat_lower, yhat_upper), GET /forecasts.
The old API (`uvicorn main_inference:app`) also has POST /load_model and GET /status. `seed_demo_db.py` needs the project's `data/` folder (optional).

## Docker
See the "Docker images" section of the project description: `docker build -f api/DockerfileForecastingGeneral -t bluebird-forecast-general .` (port 8000) and `docker build -f api/DockerfileForecastingDB -t bluebird-forecast-db .` (port 8001), or `docker compose -f api/docker-compose.yml up --build`. Mount `/config`, `/models_cache` (and `/data` for the DB image). CPU by default; GPU via `--build-arg TORCH_INDEX_URL=https://download.pytorch.org/whl/cu124`. The DB image starts with an empty database: only `energy_values` can be pushed, `/forecast` returns 409 "empty database" until data exists.
