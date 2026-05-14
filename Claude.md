# BlueBird — CLAUDE.md

Architecture and deployment brief. The optimization code exists and works. This document is about **services, containers, APIs, and data flow** — not algorithms.

A live demonstration of the full system is scheduled for **next week**. Prioritize working deployment over polish.

---

## What BlueBird Is

Demand response for local energy assets across EU partner sites. Each asset type is a **pilot**:

- Water Works (water towers)
- Building HVAC
- Sport centers / swimming pools
- Small-scale district heating

---

## Control Model (Background)

- Control is model-based. Each asset has a **Digital Twin**.
- The controller does **sequential decision-making** — economic MPC by default, but the architecture must accommodate RL, ADP, Stochastic Programming, etc. Do not assume MPC anywhere in the plumbing.
- Dynamics are passed to the controller as linear (preferably with booleans) state-space matrices:

  $$\dot{x} = A\,x(t) + B\,u(t) + E\,d(t)$$

- The controller needs **disturbance forecasts** and **electricity market signals**. Both come from separate services.
- **State estimation** (future Kalman filter) lives **inside the Flexibility Controller**. Not a separate service. Not implemented yet — leave a clean extension point, do not stub.

---

## Services

Six services total. Four are the control pipeline; two are infrastructure.

### Control pipeline (each in its own container)

1. **Digital Twin** — owns and publishes system dynamics.
2. **Flexibility Controller** — runs the control policy (MPC or other) on a configurable cadence.
3. **Disturbances Forecast** — writes forecasts to the database on a recurring schedule.
4. **Trading Manager** — fetches electricity market signals. **External repo**, consumed as a container.

### Infrastructure

5. **Database** — separate service. **Treat the engine as a given dependency.** Do not assume SQLite, Postgres, or anything else in code. Connection details come from `.env`. A working DB must be reachable for the demo next week.
6. **Control Room** — separate service. Infrastructure only for now (entry point + scaffold). Future home for: orchestration, KPIs, flexibility-function fitting, single-step runs, plotting. Do not build features yet.

**Hard rule:** no new top-level services without asking. Cross-service communication goes through the database or the shared volume — never direct in-process calls.

---

## Data Flow Between Services

### Shared volume (slow-changing, human-readable artifacts)

One shared volume mounted by all services. Used for things that:

- Update infrequently (weekly or slower)
- Benefit from being human-readable for debugging
- Need to be inspected after the fact

**Current contents:**

- **Dynamics JSON** written by the Digital Twin, read by the Flexibility Controller at every iteration. Schema follows `@FlexOPTi/data/EWH/inputs/cold_room_model_continuous.json`. The Flexibility Controller reads the **latest file** in the folder each iteration.

Rationale for files over DB rows: weekly update cadence + debuggability + offline replay.

### Database (fast-changing, queryable time-series)

- Measurements (read by the Flexibility Controller, or by state estimation once that lands)
- Disturbance forecasts (written by Forecast service, periodically updated)
- Electricity prices (written by Trading Manager every 15 minutes — see below)
- Anything else time-indexed and queried frequently

---

## Service Details

### Digital Twin

Two actions:

1. On request from the Flexibility Controller, update its dynamics.
2. After updating, write the new dynamics JSON to the shared volume.

Schema: `@FlexOPTi/data/EWH/inputs/cold_room_model_continuous.json`.

### Flexibility Controller

- API entry point: `@FlexOPTi/src/core/fm_optimize.jl`. Accepts many keyword arguments.
- Pilot dispatch: the `pilot` keyword arrives as a **string** at the API boundary. It is parsed once into the corresponding type from `@FlexOPTi/src/core/types.jl`, and Julia multiple dispatch handles the rest. No string-matching inside core logic.
- **Cadence is configurable**, not hardcoded to 15 minutes. Control hyperparameters — including the re-optimization period — live in `settings.json`.
- Reads the latest dynamics JSON from the shared volume at each iteration.

### Disturbances Forecast

- Real service not yet available.
- Use **dummy forecasts** as a placeholder. Reference pattern: room temperature in `@FlexOPTi/scripts/ewh/mpc_base.jl`.
- Dummy must run as its own container, write to the same DB tables, on the same cadence as the real service would. Interface compatibility is shown by the example file above.

### Trading Manager

- Lives in a **separate git repository**. Consumed as a container.
- Query example: `@FlexOPTi/scripts/tm/query_prices.jl`.
- Polls every **15 minutes**. Day-ahead prices usually don't change between polls; the re-poll catches market corrections.
- Per pilot, may use price forecasts or day-ahead clearing prices.
- Writes prices to a dedicated DB table.

### Database

- Separate service. Engine not specified — do not hardcode assumptions.
- Connection via `.env`.
- Must be running and reachable for the demo next week.

### Control Room

- Separate service. Infrastructure only — set up the container, entry point, and CLI scaffold.
- Future features (do not build now): run the full system, compute KPIs, fit flexibility functions, run single steps, plot results.
- Goal: enable research and live operation side by side.

---

## Configuration

- **`settings.json`** — all operator-tunable parameters for the Flexibility Controller (including control cadence, horizon, pilot, etc.). An operator changes behavior by editing this file. No code edits, no rebuilds.
- **`.env`** — secrets and environment-specific values (DB connection, service URLs, container settings).
- `docker compose` wires both into the relevant containers.

---

## Pilots and Dispatch

- Pilot types defined in `@FlexOPTi/src/core/types.jl`.
- Adding a new pilot = add a type there + the corresponding method. Do not branch on pilot strings inside core logic.
- The string-to-type conversion happens once, at the API boundary in `fm_optimize.jl`.
- Each pilots define their own digital twin in ./dt . Docker compose should use only one digital twin and be blind of the rest. This means a docker-compose.pilot.yml file. 
---

## Out of Scope (Deferred)

Do not invent logic for these. If a task seems to require them, ask first.

- State estimation / Kalman filter (extension point only)
- Failure handling, retries, fallbacks for service outages (Trading Manager down, forecast stale, etc.)
- Control Room features beyond scaffolding
- Anything that changes the optimization code itself

---

## Quick Reference

| Concern | Location |
|---|---|
| Controller API entry | `@FlexOPTi/src/core/fm_optimize.jl` |
| Pilot types (multiple dispatch) | `@FlexOPTi/src/core/types.jl` |
| Dynamics JSON schema | `@FlexOPTi/data/EWH/inputs/cold_room_model_continuous.json` |
| Dummy forecast pattern | `@FlexOPTi/scripts/ewh/mpc_base.jl` |
| Trading Manager query example | `@FlexOPTi/scripts/tm/query_prices.jl` |
| Trading Manager Docker Build | `@~/DTU/BlueBird/TM/trading-manager/compose/local_dev/`|
| A full run of the services without dockers | `@FlexOPTi/scripts/ewhdr_controller.jl`|

---

## Working Rules

1. Read referenced files before touching related code.
2. Respect the service boundaries. Cross-service = DB or shared volume.
3. Operator-facing config goes in `settings.json` / `.env`. Nothing hardcoded that an operator would reasonably want to change.
4. Pilot dispatch via types, not strings.
5. Files in the shared volume must match the documented schemas.
6. Dummies must be interface-compatible with the real service they replace.
7. Ask before: new top-level service, schema change to dynamics JSON, hardcoding the DB engine, changing the API surface of `fm_optimize.jl`, or adding failure-handling logic.
8. Demo is next week. Working > clever.
9. All packages should be open source. If there is a paid program (Gurobi) for example, an exception should be taken at the core (not pilot dependend) and return to an open source variant (HiGHS). 
