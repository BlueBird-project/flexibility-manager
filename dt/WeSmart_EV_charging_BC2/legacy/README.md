# Legacy code (not used)

These two files are the pre-refactor implementation, kept only for reference.
Nothing in the project imports them.

| File | Superseded by |
|---|---|
| `env.py` (`EVChargingEnv`) | `src/env/base_env.py` (`EnergyEnv`) + `src/env/ev_component.py` + `src/env/pv_component.py` |
| `dqn_agent.py` | `src/agent/dqn_agent_new.py` |

They were monolithic: `EVChargingEnv` hard-coded EV charging into the
environment itself, so adding PV (or a battery) meant editing the env. The
component design replaced that — see `BaseComponent` in `src/env/base_env.py`.

They also predate every bug fix applied to the active code path (normalisation
constants leaking between splits, the look-ahead state showing stale
`remaining_kwh` on arrival, the skip-empty off-by-one). **Do not copy code out of
here** — the equivalents in `src/` are the corrected versions.

Safe to delete once you are confident you don't need them.
