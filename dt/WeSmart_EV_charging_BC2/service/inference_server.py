#!/usr/bin/env python3
"""
Long-running inference service for the EV-charging DQN. Runs forever; meant to
be the container's entrypoint (see ../Dockerfile).

HTTP API
--------
GET  /health   -> model configuration summary, for the caller to self-check
                  before wiring up the real feed.
POST /decide   -> one 15-min-step decision. Body:

    {
      "timestamp": "2026-08-31T14:30:00",
      "stations": [
        {"station_id": 1, "present": 1, "remaining_kwh": 12.4, "hours_to_departure": 2.5},
        {"station_id": 2, "present": 0}
      ],
      "prices": [0.142, 0.138, 0.130, 0.125],
      "pv_forecast": [1.8, 2.1, 2.4, 2.6]
    }

  - timestamp: local wall-clock time (same convention as the training data,
    no timezone conversion is applied) of the interval this decision is for.
  - stations: one entry per station the model was trained on (see /health for
    the expected station_ids and order-independent — matched by station_id).
    present=0 stations may omit remaining_kwh/hours_to_departure.
  - prices: EUR/kWh, forward-looking, length == price_horizon from /health.
    prices[0] is the price for `timestamp` itself. Belgian day-ahead prices
    are hourly — repeat each hourly value 4x to fill the quarter-hours it
    covers before sending.
  - pv_forecast: net kWh available for EV charging (production minus building
    consumption, already floored at 0) for each quarter-hour, forward-looking,
    length == pv_horizon from /health. Omit entirely if /health reports
    has_pv=false; required if it reports true.

Response:
    {"timestamp": "2026-08-31T14:30:00", "decisions": [{"station_id": 1, "charge": 1}, ...]}

Errors are HTTP 400 with {"error": "..."} for bad input, 500 for anything
unexpected (and logged with a full traceback) — the process itself stays up
either way; one bad request never takes the service down.
"""
from __future__ import annotations

import json
import logging
import os
import signal
import sys
import threading
from datetime import datetime
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import numpy as np
import torch

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from src.agent.helpers.NN.NN import QNetwork  # noqa: E402
from service.state_builder import StateBuilder, StateBuilderError  # noqa: E402

METADATA_FILENAME = "metadata.json"
Q_FILENAME = "q_state_dict.pth"

MAX_BODY_BYTES = 1_000_000  # a single-step request is a few hundred bytes; this is generous headroom


class ModelBundle:
    """Loads a checkpoint + metadata.json once at startup and answers /decide requests."""

    def __init__(self, model_dir: Path):
        meta_path = model_dir / METADATA_FILENAME
        q_path = model_dir / Q_FILENAME
        for p in (meta_path, q_path):
            if not p.exists():
                raise FileNotFoundError(
                    f"{p} not found. Run `python main.py train` to produce it, or point "
                    f"MODEL_DIR at a folder that has both {METADATA_FILENAME} and {Q_FILENAME}."
                )

        self.meta = json.loads(meta_path.read_text(encoding="utf-8"))
        self.state_builder = StateBuilder.from_metadata(self.meta)

        cfg = self.meta["config"]
        self.device = torch.device("cpu")  # a 2-layer MLP is instant on CPU; keeps the image CUDA-free
        self.net = QNetwork(
            int(self.meta["observation_size"]),
            int(self.meta["action_size"]) * 2,
            hidden_sizes=tuple(cfg["hidden_sizes"]),
        ).to(self.device)
        state_dict = torch.load(q_path, map_location=self.device, weights_only=True)
        self.net.load_state_dict(state_dict)
        self.net.eval()

    def decide(self, payload: dict) -> dict:
        if "timestamp" not in payload:
            raise StateBuilderError("'timestamp' is required")
        ts_raw = payload["timestamp"]
        try:
            timestamp = datetime.fromisoformat(ts_raw)
        except (TypeError, ValueError) as e:
            raise StateBuilderError(f"'timestamp' must be ISO-8601: {e}") from e

        stations = payload.get("stations")
        if not isinstance(stations, list) or not stations:
            raise StateBuilderError("'stations' must be a non-empty list")
        prices = payload.get("prices")
        if not isinstance(prices, list):
            raise StateBuilderError("'prices' must be a list")
        pv_forecast = payload.get("pv_forecast")
        if pv_forecast is not None and not isinstance(pv_forecast, list):
            raise StateBuilderError("'pv_forecast' must be a list")

        state = self.state_builder.build(timestamp, stations, prices, pv_forecast)

        with torch.no_grad():
            s = torch.tensor(state, dtype=torch.float32, device=self.device).unsqueeze(0)
            n_stations = len(self.state_builder.station_ids)
            q = self.net(s).view(n_stations, 2)
            actions = q.argmax(dim=-1).cpu().numpy().astype(int).tolist()

        # Training never applies the action bit when no EV is present
        # (EVComponent.apply_actions: `charging = sess is not None and action == 1`) —
        # the network is never trained to produce a meaningful value there, so don't
        # forward whatever it happens to output for an empty station.
        present_by_id = {int(s["station_id"]): bool(s.get("present", False)) for s in stations}
        decisions = [
            {"station_id": sid, "charge": a if present_by_id.get(sid) else 0}
            for sid, a in zip(self.state_builder.station_ids, actions)
        ]
        return {"timestamp": ts_raw, "decisions": decisions}

    def health(self) -> dict:
        sb = self.state_builder
        return {
            "status": "ok",
            "station_ids": sb.station_ids,
            "power_kw": sb.power_kw,
            "price_horizon": sb.price_horizon,
            "has_pv": sb.has_pv,
            "pv_horizon": sb.pv_horizon,
            "interval_minutes": sb.interval_minutes,
            "observation_size": sb.observation_size,
        }


class Handler(BaseHTTPRequestHandler):
    bundle: ModelBundle  # set on the class by main() before serve_forever()
    server_version = "ev-dqn-inference/1.0"

    def log_message(self, fmt: str, *args) -> None:
        logging.info("%s - %s", self.address_string(), fmt % args)

    def _send_json(self, code: int, payload: dict) -> None:
        body = json.dumps(payload).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self) -> None:
        if self.path == "/health":
            self._send_json(200, self.bundle.health())
        else:
            self._send_json(404, {"error": "not found"})

    def do_POST(self) -> None:
        if self.path != "/decide":
            self._send_json(404, {"error": "not found"})
            return
        try:
            length = int(self.headers.get("Content-Length") or 0)
            if length <= 0:
                raise StateBuilderError("empty request body")
            if length > MAX_BODY_BYTES:
                raise StateBuilderError(f"request body too large ({length} bytes)")
            raw = self.rfile.read(length)
            try:
                payload = json.loads(raw)
            except json.JSONDecodeError as e:
                raise StateBuilderError(f"invalid JSON: {e}") from e
            if not isinstance(payload, dict):
                raise StateBuilderError("request body must be a JSON object")

            response = self.bundle.decide(payload)
            self._send_json(200, response)
            logging.info(json.dumps({"event": "decide", **response}))
        except StateBuilderError as e:
            self._send_json(400, {"error": str(e)})
        except Exception:
            logging.exception("unexpected error handling /decide")
            self._send_json(500, {"error": "internal error"})


def main() -> None:
    logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s", stream=sys.stdout)

    model_dir = Path(os.environ.get("MODEL_DIR", "saved_models/EV"))
    host = os.environ.get("HOST", "0.0.0.0")
    port = int(os.environ.get("PORT", "8080"))

    logging.info(f"Loading model from {model_dir} ...")
    Handler.bundle = ModelBundle(model_dir)
    logging.info(f"Loaded: {json.dumps(Handler.bundle.health())}")

    server = ThreadingHTTPServer((host, port), Handler)

    def _shutdown(signum, _frame):
        logging.info(f"Received signal {signum}, shutting down...")
        threading.Thread(target=server.shutdown, daemon=True).start()

    signal.signal(signal.SIGTERM, _shutdown)
    signal.signal(signal.SIGINT, _shutdown)

    logging.info(f"Serving on {host}:{port}")
    server.serve_forever()
    server.server_close()
    logging.info("Stopped.")


if __name__ == "__main__":
    main()
