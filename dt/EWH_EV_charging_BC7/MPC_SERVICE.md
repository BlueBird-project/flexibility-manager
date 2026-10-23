# fm_charging: smart EV charging service (MQTT)

This service decides **when and how fast each electric car charges** at the charging station of
Hotel Prinz-Luitpold-Bad (`plb`, 4 connectors `c1`–`c4`), so charging happens when electricity is
cheapest, every car still gets its energy before it leaves, and the site never draws more than its
power limit.

It runs forever in a Docker container. It talks to the station only through the **bluebird MQTT
broker**, exactly as described in *"bluebird – MQTT Interface for fm_charging (IMEC)"*.

---

## 1. The big picture

```
                         bluebird MQTT broker (sndbx.ewhindelang.de:8883)
                                          │
 Station manager (ocpp_manager) ──────────┤  car started / stopped, meter readings,
                                          │  connector status, answers to our plans
 Survey app (ui, Inetum) ─────────────────┤  the driver's answer: flexible or not, battery level
                                          │
 THIS SERVICE (fm_charging) ◄─────────────┤  reads all of the above
                            ─────────────►│  sends: one charging plan per car, a baseline, "online"
                                          │
 Trading manager (prices) ──── HTTP ──────► THIS SERVICE   day-ahead electricity prices
```

Nobody calls this service. It **listens**: every message that arrives can trigger a new plan.

---

## 2. What it receives and what it sends

**Receives** (subscribed topics, all under `bluebird/plb/`):

| Message | From | What it tells us |
|---|---|---|
| `charging_sessions/ocpp_manager/plb_cs_c<n>_session` | station manager | a car **started** or **stopped** on connector n (plus warnings: `issue`) |
| `user_events/ui/plb_cs_c<n>_ui_event` | survey app | the driver's choice: `flex_12h`, `flex_24h`, `noflex`, or skipped; battery `soc` (%) and `capacity` (kWh) |
| `measurements/ocpp_manager/plb_cs_c<n>_energy` | station manager | the connector's energy meter (kWh, a running total), every 15 min |
| `measurements/ocpp_manager/plb_cs_c<n>_power` | station manager | measured power (kW), when the station sends it |
| `states/ocpp_manager/plb_cs_c<n>_status` | station manager | connector status: `charging`, `suspended_evse` (held back by us), `suspended_ev` (car paused itself, e.g. full) |
| `command_acks/+/plb_cs_c<n>_power_op` | station manager | was our plan `accepted` or `rejected`? |

**Sends**:

| Message | To | What it is |
|---|---|---|
| `bluebird/plb/commands/fm_charging/plb_cs_c<n>_power_op` | station manager | the **charging plan** for the car on connector n: the maximum kW for each quarter hour until it leaves |
| `bluebird/plb/forecasts/fm_charging/plb_cs_energy_bl` | bluebird platform | the **baseline**: kWh per quarter hour the cars would use *without* our optimisation (used to measure savings) |
| `bluebird/plb/status/fm_charging` | everyone | `online` / `offline` (if the service dies, the broker publishes `offline` for us) |

Example plan (11 kW until 14:00, then nothing, then 4.2 kW from 21:00):

```json
{"v": 1, "command_id": "5b8d0e12-...", "id": "plb_cs_c1_power_op", "created": "2026-10-12T12:11:02.123Z",
 "valid_until": "2026-10-12T22:45:00Z", "transaction_id": 4711,
 "values": [{"ts": "2026-10-12T12:15:00Z", "value": 11.0},
            {"ts": "2026-10-12T14:00:00Z", "value": 0.0},
            {"ts": "2026-10-12T21:00:00Z", "value": 4.2}]}
```

---

## 3. How it decides, step by step

### Step 1: keep a table of the cars
For every connector it remembers the car that is plugged in: transaction id, arrival time, the
driver's choice, battery level, meter readings, the car's power, and the plans already sent.

### Step 2: put every car in one of three groups

| Group | When | What we do |
|---|---|---|
| **Controllable** | started + driver chose `flex_12h` / `flex_24h` + gave `soc` and `capacity` | we plan its charging |
| **Not controllable** (business as usual) | driver chose `noflex`, skipped the survey, or gave flex without `soc`/`capacity` (logged as an error) | we never send it a plan, but we count its power against the site limit |
| **Waiting** | started, but no survey answer yet | treated like "not controllable" until the answer arrives |

A `noflex` decision is final for that car.

### Step 3: work out what each controllable car needs
- **Leaves at:** arrival + 12 h (`flex_12h`) or + 24 h (`flex_24h`).
- **Energy needed:** `(100 − soc) % × capacity ÷ 0.95`. The battery must reach 100%; 0.95 covers charging losses, because the meter counts energy *before* the losses.
- **Still to deliver:** energy needed − what the meter has counted − what it will still charge in the current quarter hour.
- **Car power:** assumed **22 kW** (the connector maximum) until measured.

Not controllable cars are assumed to stay **10 hours** and charge at their power the whole time.

### Step 4: learn the real power of each car
After every meter reading: `power = energy since the last reading ÷ time it was allowed to charge`.
- If the car took **clearly less than the station allowed**, that is its real maximum power (e.g. 11 kW), and every following plan uses it.
- If it took everything it was allowed, nothing new is learned.
- If it takes **nothing** while allowed and its status is `suspended_ev`, the battery is **full**: we stop planning it.
- "Allowed" already includes the site-limit split (below), so a car slowed down by the split is not mistaken for a slow car.

### Step 5: respect the site limit (30 kW)
In each quarter hour: if the cars that are charging would together draw more than 30 kW, **each gets 30 ÷ N** (N = number of cars charging in that quarter hour, controllable or not).

### Step 6: make the plan (the optimisation)
The solver (`plan_charging()` in `src/mpc/mpc.py`) gets the prices for every quarter hour until the
last car leaves, and chooses **how many kW each controllable car gets in each quarter hour** so that:
- the total cost of electricity is as low as possible,
- every car gets its energy before it leaves (if that is physically impossible, it gets as much as possible),
- the site-limit rule of step 5 holds in every quarter hour,
- it only plans **full quarter hours** inside the car's stay: from the next :00/:15/:30/:45 on, up to the last full quarter hour before it leaves.

### Step 7: send the plans (only when something changed)
For each controllable car the plan becomes a list of "from this quarter hour on: X kW".
It is **sent only if it differs** from the last plan for that car, with a new `command_id`.
`valid_until` is the car's departure rounded down to the quarter hour. After that, and before the
first planned quarter hour, the station charges the car uncontrolled.

### Step 8: send the baseline (when a car arrives, answers the survey, or leaves)
The baseline imagines no optimisation: every car charges at full power from its arrival until full
(or for its whole stay if we don't know its battery). Each version has a new `created` time; old
versions are never changed.

### When does it re-plan?
After every relevant message (car started or stopped, survey answer, meter reading, new power
learned) **and** every quarter hour (:00, :15, :30, :45 + a few seconds), even if nothing arrived.

### Prices
From the trading manager (`/api/market/2/offer`, Germany). Handled correctly:
- the API's `ts` is the **end** of each quarter hour,
- when there are two price series, series `1` is used,
- quarter hours whose day-ahead price isn't published yet (e.g. tomorrow, in the morning) get the
  price of the same quarter hour on the previous day. When the real prices arrive, the next re-plan uses them.

If no prices can be fetched, the current plans stay as they are and an error is logged.

---

## 4. How to run it

### What you need
1. **Docker** (Docker Desktop on Windows).
2. **The trading manager** running (it provides the prices on port 11001); see its own repository:
   `docker compose up -d` in its `compose/local_dev` folder.
3. **The MQTT password** for user `fm_charging` (EWH sends it separately).
4. A network that can reach `sndbx.ewhindelang.de` on **port 8883** (see Troubleshooting).

### Start it (from the folder `dt/EWH_EV_charging_BC7`)
1. Put the password in a file (this folder is ignored by git, it is never uploaded):
   ```powershell
   mkdir secrets
   Set-Content -NoNewline -Path secrets\mqtt_password.txt -Value "THE_PASSWORD"
   ```
2. Build and start:
   ```powershell
   docker compose up -d --build
   ```
3. Watch what it does:
   ```powershell
   docker compose logs -f
   ```
4. Stop it (it publishes `offline` first):
   ```powershell
   docker compose down
   ```

It restarts by itself after a crash or a reboot (`restart: unless-stopped`).

### Settings (in `compose.yaml`, or as environment variables)

| Setting | Default | Meaning |
|---|---|---|
| `MQTT_HOST` | `sndbx.ewhindelang.de` | broker address |
| `MQTT_PORT` | `8883` | broker port (TLS) |
| `MQTT_USERNAME` | `fm_charging` | broker user |
| `MQTT_CLIENT_ID` | `fm_charging-1` | keep it the same every time |
| `MQTT_PASSWORD_FILE` | `/run/secrets/mqtt_password` | file with the password (set by `compose.yaml`) |
| `PRICE_API_BASE` | `http://host.docker.internal:11001` | trading manager address |
| `PRICE_MARKET_ID` | `2` | 2 = Germany |
| `SITE_LIMIT_KW` | `30` | power limit of the 4 connectors together |
| `DEFAULT_POWER_KW` | `22` | car power assumed until measured |
| `BAU_HOURS` | `10` | assumed stay of cars we may not control |
| `EFFICIENCY` | `0.95` | charging efficiency (battery kWh per meter kWh) |
| `MPC_TIME_LIMIT_S` | `10` | maximum solver time per plan |

To use a different broker or user (e.g. on EWH's own server), change `MQTT_HOST` / `MQTT_USERNAME`
and the password file; nothing else.

### Run it without Docker (for development)
```powershell
.venv\Scripts\Activate.ps1
pip install -r requirements.txt
$env:MQTT_PASSWORD_FILE = "secrets\mqtt_password.txt"
python run_mpc_service.py
```
Press Ctrl+C to stop.

---

## 5. What you see in the logs

```
INFO connected to sndbx.ewhindelang.de:8883, subscribed to 18 topics
INFO c1: transaction 4711 started at 2026-10-12T10:55:40Z
INFO c1: transaction 4711 user decision flex_12h (soc=40.0, capacity=60.0) -> controllable
INFO sent bluebird/plb/commands/fm_charging/plb_cs_c1_power_op {...the plan...}
INFO c1: plan 5b8d0e12-... accepted
INFO c1: meter 1534.374 kWh, 1.956 kWh since 2026-10-12T11:55:40Z
INFO c1: car power 22.00 -> 10.88 kW (allowed 15.76)
INFO c1: transaction 4711 stopped (ev_disconnected)
```
Warnings worth reading: `plan ... rejected` (the reason follows), `no ack for plan ... probably
quarantined` (the message format was refused by the broker), `no prices`, `MQTT connection ... failed`.

---

## 6. Troubleshooting

| Problem | Likely cause |
|---|---|
| `MQTT connection to sndbx.ewhindelang.de:8883 failed, retrying` and nothing else | port 8883 is blocked on your network (e.g. the university network), or EWH only allows known IP addresses. Test from another network or ask EWH. |
| `MQTT connection refused` | wrong user name or password |
| `no prices (...) -- plans not updated` | the trading manager is not running, or `PRICE_API_BASE` is wrong |
| plan `rejected`, `no user decision with flex` | the station has no flex answer for that transaction (should not happen: we only plan flex cars) |
| `no ack for plan ... probably quarantined` | EWH refused the message format; they report quarantined messages during the test run |

---

## 7. Known limitations

- **No memory after a restart.** The car table lives in memory. If the container restarts during
  sessions, those cars are unknown until they plug in again; their last plan runs until its
  `valid_until`, then they charge uncontrolled. (To add later: save the table to a file.)
- **Slow car, late plan.** A car is assumed to take 22 kW until its first planned charging quarter
  hour shows otherwise. If that quarter hour is near its departure and the car is much slower, there
  may not be enough time left; it then gets as much as possible.
- **Not controllable cars** are assumed to charge for 10 h at their power (their real power is
  measured after 15 min; when they are full they no longer count).
- **The first part of the current quarter hour and the last minutes before departure** are not
  planned (only full quarter hours are); the station charges uncontrolled there.
- **The site-limit rule (30 ÷ N)** is our assumption about how the station shares power. The
  document only says the station "distributes power by its own rules".
- **Values only between 0 and 22 kW**: the real per-connector limits are in "template v3", which we
  don't have yet.

---

## 8. Files

| File | What it is |
|---|---|
| `run_mpc_service.py` | the service: MQTT connection, car table, rules, plans, baseline, prices |
| `src/mpc/mpc.py` | the solvers: `plan_charging()` (used by the service) and `solve_horizon()` (used by the dataset script) |
| `Dockerfile` | builds the container image |
| `compose.yaml` | runs the container with its settings and the password secret |
| `run_mpc_dataset.py` | separate tool: runs the optimisation over a whole historical sessions dataset (`python run_mpc_dataset.py --site-cap-kw 22`) |
