import json
import sys
import urllib.error
import urllib.request


BASE_URL = "http://127.0.0.1:8000"


def request_json(method: str, path: str, payload: dict | None = None) -> tuple[int, dict]:
    data = None
    headers = {}

    if payload is not None:
        data = json.dumps(payload).encode("utf-8")
        headers["Content-Type"] = "application/json"

    req = urllib.request.Request(
        url=f"{BASE_URL}{path}",
        data=data,
        headers=headers,
        method=method,
    )

    try:
        with urllib.request.urlopen(req, timeout=30) as response:
            body = response.read().decode("utf-8")
            return response.status, json.loads(body)
    except urllib.error.HTTPError as exc:
        body = exc.read().decode("utf-8")
        try:
            parsed = json.loads(body)
        except json.JSONDecodeError:
            parsed = {"raw": body}
        return exc.code, parsed


def main() -> int:
    status, health = request_json("GET", "/health")
    if status != 200 or health.get("status") != "ok":
        print("Healthcheck failed:", status, health)
        return 1

    payload = {
        "horizon_span": 4,
        "data": {
            "ds": [
                "2026-10-20 00:00:00",
                "2026-10-20 00:15:00",
                "2026-10-20 00:30:00",
                "2026-10-20 00:45:00"
            ],
            "unique_id": ["asset_1", "asset_1", "asset_1", "asset_1"],
            "y": [100.0, 101.5, 99.8, 102.1]
        }
    }

    status, forecast = request_json("POST", "/forecast/day-ahead", payload)
    if status != 200:
        print("Forecast endpoint failed:", status, forecast)
        return 1

    for key in ("ds", "unique_id", "yhat"):
        if key not in forecast:
            print(f"Missing key in response: {key}")
            print("Response:", forecast)
            return 1

    sizes = (len(forecast["ds"]), len(forecast["unique_id"]), len(forecast["yhat"]))
    if len(set(sizes)) != 1:
        print("Response arrays have inconsistent lengths:", sizes)
        return 1

    print("Smoke test passed.")
    print("Returned points:", sizes[0])
    return 0


if __name__ == "__main__":
    sys.exit(main())
