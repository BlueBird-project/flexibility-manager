# PriceFM+ Price Inference

## What is PriceFM+

PriceFM+ is a pretrained deep-learning model that forecasts **day-ahead electricity prices** for the European bidding zones (AT, BE, BG, CH, CZ, DE_LU, DK_1, DK_2, EE, ES, FI, FR, GR, HR, HU, IT_*, LT, LV, NL, NO_1–5, PL, PT, RO, RS, SE_1–4, SI, SK). It is the extended version of [PriceFM](https://github.com/runyao-yu/PriceFM).

- **Output:** one full UTC day, 96 quarter-hours, with a median (`yhat`) and a p10–p90 band (`p10`, `p90`), in EUR/MWh.
- **Inputs:** the last 7 days of day-ahead prices and actuals (load, solar, wind, ...) of the zone and its neighbours, plus the ENTSO-E day-ahead forecasts of load, solar and wind for the target day.
- **Context span:** fixed at 7 days by the trained weights. The actuals are cut at 12:00 UTC of the issue day.
- **Data:** the processed dataset and the weights are downloaded from Hugging Face. The data is refreshed from ENTSO-E when a forecast is requested, so an **ENTSO-E API key is required**.

The service follows the FAST schema:

- **Forge** (`forge/pricefm_data.py`): prepares and refreshes the data for the requested zones and day.
- **Translate** (`translate/pricefm_plus.py`): loads PriceFM+ and produces the forecast.

## Folder and structure to download

The `docker build` command must be run from the root of this folder.

```text
Price Inference/
├── Dockerfile
├── requirements-docker.txt
├── .dockerignore
├── app_price_inference.py
├── api_inference.py
├── price_inference.py
├── db_forecast.py
├── fast_inference.py
├── forge/
│   └── pricefm_data.py
├── translate/
│   └── pricefm_plus.py
├── credentials_entsoe.json      (NOT in the image, mounted when running)
└── README.md
```

`credentials_entsoe.json` holds the ENTSO-E key:

```json
{"token": "YOUR_ENTSOE_API_KEY"}
```

## 1. Build the image

```bash
cd "Price Inference"
docker build -t pricefm-inference .
```

If your network uses a proxy that inspects HTTPS (error `self-signed certificate in certificate chain`), the build and the downloads need your company CA. Build with your certificate bundle available and pass it at run time (see below).

## 2. Run the container

```bash
docker run --rm -p 8000:8000 \
  -v pricefm_data:/data/pricefm \
  -v "$PWD/credentials_entsoe.json":/run/secrets/credentials_entsoe.json:ro \
  pricefm-inference
```

- `pricefm_data` is a Docker volume that keeps the downloaded data and weights (several GB) between runs.
- The key is mounted, never copied into the image.
- With a corporate CA bundle add:
  `-v "$PWD/corp-ca.pem":/certs/ca.pem:ro -e SSL_CERT_FILE=/certs/ca.pem -e REQUESTS_CA_BUNDLE=/certs/ca.pem`

The API docs are at http://localhost:8000/docs.

### Load the model first

On start the container loads the model in the background (`AUTO_LOAD_MODEL=1`, the default). The first load downloads the data and weights and can take several minutes. Poll the status until it is `ready`:

```bash
curl localhost:8000/model/status
# {"status":"loading","error":null,"seconds":12.3}
```

Statuses: `not_loaded`, `loading`, `ready`, `failed`. Forecast requests return `503` until the model is `ready`. To start or restart the load manually: `curl -X POST "localhost:8000/model/load?force=true"`.

## 3. Request forecasts

Production, the next day (or the next N days), for one or several zones:

```bash
curl -X POST localhost:8000/forecast/production \
  -H "Content-Type: application/json" \
  -d '{"regions": ["BE", "ES"], "days": 2}'
```

Backtest, every day from the start to the end date (YYYYMMDD, inclusive), which also returns the real prices as `actual`:

```bash
curl -X POST localhost:8000/forecast/backtest \
  -H "Content-Type: application/json" \
  -d '{"regions": ["BE"], "start_date": "20260201", "end_date": "20260202"}'
```

Response (`ds` in UTC, prices in EUR/MWh):

```json
{
  "ds": ["2026-11-04 00:00:00", "..."],
  "region_name": ["BE", "..."],
  "yhat": [0.0],
  "p10": [0.0],
  "p90": [0.0],
  "failed_days": []
}
```

### Endpoints

- `GET /health`
- `POST /model/load`
- `GET /model/status`
- `POST /forecast/production`
- `POST /forecast/backtest`

## Environment variables

All are optional.

- `PRICEFM_WORKDIR`: data and weights folder. Default `/data/pricefm`.
- `ENTSOE_CREDENTIALS_FILE`: path to the key file. Default `/run/secrets/credentials_entsoe.json`.
- `AUTO_LOAD_MODEL`: load the model on start. Default `1`.
- `INFERENCE_TIMEOUT_SECONDS`: time limit per forecast request, returns `504` when exceeded. Default `1800`. Loading has no HTTP timeout.
- `MAX_PRODUCTION_DAYS`: default `7`.
- `MAX_BACKTEST_DAYS`: default `62`.
- `HISTORY_DAYS`: days of data refreshed before the issue day, at least 7. Default `10`.

## Notes and troubleshooting

- The API runs one request at a time with a single worker, because the Forge and Translate change the working directory.
- A request that exceeds the timeout keeps running in the background, and the next request waits for it.
- `URLError ... CERTIFICATE_VERIFY_FAILED`: a proxy is inspecting HTTPS. Use the CA bundle options above.
- Forecast errors about missing data: check that the ENTSO-E key is valid and the zone is in the list above.
