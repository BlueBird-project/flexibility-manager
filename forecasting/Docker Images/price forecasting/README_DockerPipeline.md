# Price forecast pipeline: Docker setup

Runs a PostgreSQL database and a Prefect flow that, every day, saves the real prices and the new PriceFM+ forecast in the database. For the inference API, see `README_PriceForecasting.md`.

## 1. Folder structure

Run every command from the root of this folder.

```
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
```

## 2. Requirements

- Docker Desktop running (Docker Compose v2: `docker compose version`). On Windows, use the WSL2 backend and keep this folder inside your user directory.
- An ENTSO-E API key.
- About 10 GB of free disk space (data and weights are downloaded on the first run).

## 3. Configure

Create `credentials_entsoe.json` in the root:

```json
{"token": "YOUR_ENTSOE_KEY"}
```

Create `.env` from the example and set a password:

macOS / Linux:

```
cp .env.example .env
```

Windows (PowerShell):

```
Copy-Item .env.example .env
```

Then edit `.env` and set the password:

```
POSTGRES_PASSWORD=your_password
```

On Windows, save `.env` and `credentials_entsoe.json` as plain "UTF-8" (not "UTF-8 with BOM"), otherwise the key or the first variable may not be read.

Optional values in `.env`:

| Variable | Default | Meaning |
|---|---|---|
| `POSTGRES_USER` | `pricefm` | Database user |
| `POSTGRES_DB` | `pricefm` | Database name |
| `FLOW_CRON` | `0 13 * * *` | Daily run time (13:00 UTC; the model needs prices up to 12:00 UTC) |
| `FLOW_TIMEZONE` | `UTC` | Time zone of `FLOW_CRON` |
| `PRICE_BACKFILL_DAYS` | `3` | Days of real prices saved before the forecast day |

Behind a corporate proxy: put the CA bundle in the root as `ca.pem` and uncomment the three `ca.pem` / `SSL_CERT_FILE` / `REQUESTS_CA_BUNDLE` lines in `docker-compose.yml`.

## 4. Start

```
docker compose -f docker-compose-flow.yml build --no-cache flow
docker compose -f docker-compose-flow.yml up -d
```

This builds the flow image and starts two containers:

- `postgres`: the database, on `127.0.0.1:5433` (only reachable from this machine). Data is kept in the `pg_data` volume.
- `flow`: waits for the database, creates the tables and the 26 price tags if missing, and serves the daily schedule.

Check that both are running:

```
docker compose -f docker-compose-flow.yml ps
docker compose -f docker-compose-flow.yml logs -f flow
```

## 5. Run a forecast now (first run)

The schedule only runs at the configured time. To fill the database right away:

```
docker compose -f docker-compose-flow.yml exec flow python prefect_price_flow_psq.py --once
```

Useful options:

```
... --once --regions BE ES            # only these zones
... --once --date 20261106 --days 2   # specific target days (YYYYMMDD, UTC)
```

The first run downloads several GB of data and weights into the `pricefm_data` volume; later runs are much faster. Reruns are safe: rows are updated, not duplicated.

## 6. Read the data

From psql:

```
docker compose exec postgres psql -U pricefm -d pricefm -c "SELECT count(*) FROM price_forecast;"
```

From Python (or pgAdmin / any client: host `localhost`, port `5433`). First set the password in the terminal where Python runs:

| Shell | Command |
|---|---|
| macOS / Linux (zsh, bash) | `export PGPASSWORD='your_password'` |
| Windows PowerShell | `$env:PGPASSWORD='your_password'` |
| Windows cmd | `set PGPASSWORD=your_password` |

```python
import os, psycopg

conn = psycopg.connect(host="localhost", port=5433, dbname="pricefm",
                       user="pricefm", password=os.environ["PGPASSWORD"])
print(conn.execute("""
    SELECT t.country, f.ds, f.yhat, f.yhat_lower, f.yhat_upper
    FROM price_forecast f JOIN price_tag t ON t.id = f.price_tag_id
    ORDER BY t.country, f.ds LIMIT 10""").fetchall())
```

Tables: `price_tag` (one row per country), `price` (real prices), `price_forecast` (forecasts). Timestamps (`ds`) are UTC.

## 7. Operate

| Task | Command |
|---|---|
| Stop (keeps data) | `docker compose down` |
| Start again | `docker compose up -d` |
| Rebuild after code changes | `docker compose up -d --build` |
| Logs | `docker compose logs -f flow` |
| Delete everything, including the database and downloaded data | `docker compose down -v` |

## Troubleshooting

- **`set POSTGRES_PASSWORD in .env`**: `.env` is missing or has no password.
- **Windows: files not found or mount errors**: in Docker Desktop, Settings > Resources > File sharing, make sure the drive holding this folder is shared, or move the folder to `C:\Users\<you>`.
- **Windows: scripts fail with `\r` or "bad interpreter" errors**: git converted line endings to CRLF. Run `git config core.autocrlf false`, then clone again.
- **Linux: `permission denied` on the Docker socket**: add your user to the `docker` group (`sudo usermod -aG docker $USER`, then log in again) or prefix the commands with `sudo`.
- **Port 5433 already in use**: change the left side of `"127.0.0.1:5433:5432"` in `docker-compose.yml`.
- **`CERTIFICATE_VERIFY_FAILED`**: corporate proxy, see the CA bundle note in step 3.
- **Changed `POSTGRES_PASSWORD` after the first start**: the database keeps the old one. Reset it with `docker compose down -v` (deletes the data) or change it inside Postgres with `ALTER USER`.
- **A day failed**: the flow logs `<day> failed` and retries the Forge/Translate; check `docker compose logs flow`.
