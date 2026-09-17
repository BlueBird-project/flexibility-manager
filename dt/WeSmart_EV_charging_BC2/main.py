#!/usr/bin/env python3
"""
Command-line entry point to train/test the EV-charging DQN.

Subcommands
-----------
- train: trains on a 70/30 split (per-station, chronological) and evaluates on TEST.
- test : loads q and q_target (defaults match training save paths) and evaluates on TEST;
         also runs the always-charge baseline.

Both commands build the environment with the *training* split's normalisation
constants, so a checkpoint always sees inputs on the scale it was trained with.
"""

import argparse
import os
import random
import sys
from pathlib import Path

import numpy as np
import pandas as pd
import torch

ROOT = os.path.dirname(os.path.abspath(__file__))  # folder with main.py and 'src'
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

from src.agent.dqn_agent_new import DQNAgent, DQNConfig, METADATA_FILENAME, Q_FILENAME, Q_TARGET_FILENAME
from src.env.base_env import EnergyEnv
from src.env.ev_component import EVComponent
from src.env.pv_component import PVComponent


# -----------------------------
# Defaults
# -----------------------------
DEFAULT_SESSIONS = "src/env/dataset/wesmart_ev_sessions.csv"
DEFAULT_PRICE = "src/env/dataset/price_two_years_15min.xlsx"
DEFAULT_OUTPUT_DIR = "outputs"
DEFAULT_SAVED_MODELS_DIR = "saved_models"
EXAMPLE_PV = "src/env/dataset/solar_production.csv"
EXAMPLE_CONSUMPTION = "src/env/dataset/common_areas_consumption(in).csv"

# Charging power used by BOTH the feasibility filter and the simulator. The raw
# charger logs (ev_charger_EVSE0*.csv) give a per-session median of 8.86 kW and
# 9.27 kW for the two stations, mode 9 kW — hence 9.0.
POWER_KW = 9.0
# A session is kept only if its plug-in window is at least this multiple of the
# time strictly needed to deliver req_kwh at POWER_KW.
FEASIBILITY_SLACK = 1.5

PENALTY_PER_KWH = 5.0
FIXED_PENALTY = 10.0
EPISODES = 50
SEED = 1
TEST_SIZE = 0.3
# Carved out of what would otherwise be training data, chronologically between train
# and test, and used only to choose which episode's weights to keep.
VAL_SIZE = 0.15

# Live deployment contract (see service/inference_server.py): both are
# forward-looking, quarter-hour resolution, 1-hour lookahead. Belgian day-ahead
# prices are a known schedule by decision time (no forecast error); PV is a real
# forecast with error the model never sees during training — kept short for
# that reason. Changing either requires retraining and redeploying together.
PRICE_HORIZON = 12        # M: price values in the state
PRICE_STEP_MINUTES = 30   # ...spaced 30 min apart: current quarter-hour, then 11 half-hour means (6 h)
PRICE_ENCODING = "window" # min-max within the window + level + spread; see base_env.encode_prices
PV_HORIZON = 4            # N: net PV kWh available for charging, 15-min cadence


def set_seeds(seed: int = SEED):
    np.random.seed(seed)
    torch.manual_seed(seed)
    random.seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)


# -----------------------------
# Dataset split
# -----------------------------
def split_df(df: pd.DataFrame, test_size: float = TEST_SIZE, eval_size: float = 0.0, min_per_split: int = 1):
    """
    Chronological per-station split, so every station is represented in every
    requested split rather than splitting the whole dataset by date.

    A split whose size is 0 comes back genuinely empty. (The previous version
    forced at least one session into the eval split even at eval_size=0.0, and
    both commands then discarded those sessions.)
    """
    if not (0.0 <= test_size < 1.0 and 0.0 <= eval_size < 1.0 and test_size + eval_size < 1.0):
        raise ValueError(f"Invalid split sizes: test_size={test_size}, eval_size={eval_size}")

    df = df.copy()
    df["arrival"] = pd.to_datetime(df["arrival"])
    df = df.sort_values(["station_id", "arrival"])

    train_parts, eval_parts, test_parts = [], [], []
    for _sid, block in df.groupby("station_id"):
        block = block.sort_values("arrival").reset_index(drop=True)
        n = len(block)

        floor_test = min_per_split if test_size > 0 else 0
        floor_eval = min_per_split if eval_size > 0 else 0
        n_test = min(n, max(floor_test, int(round(n * test_size))))
        n_eval = min(n - n_test, max(floor_eval, int(round(n * eval_size))))
        n_train = n - n_test - n_eval

        # if train got squeezed out, claw sessions back from test then eval
        while n_train < min_per_split and n_test > 0:
            n_test -= 1
            n_train += 1
        while n_train < min_per_split and n_eval > 0:
            n_eval -= 1
            n_train += 1

        assert n_train + n_eval + n_test == n
        train_parts.append(block.iloc[:n_train])
        eval_parts.append(block.iloc[n_train:n_train + n_eval])
        test_parts.append(block.iloc[n_train + n_eval:])

    def _concat(parts):
        parts = [p for p in parts if not p.empty]
        if not parts:
            return pd.DataFrame(columns=df.columns)
        return pd.concat(parts).sort_values(["station_id", "arrival"]).reset_index(drop=True)

    return _concat(train_parts), _concat(eval_parts), _concat(test_parts)


def get_run_folder_name(args: argparse.Namespace) -> str:
    """Configuration name derived purely from the datasets provided on the command line."""
    parts = ["EV"]  # EV is always the baseline
    if getattr(args, "pv", None) is not None:
        parts.append("PV")
    if getattr(args, "consumption", None) is not None:
        parts.append("Cons")
    # Future expansions can be added right here:
    # if getattr(args, "battery", None) is not None:
    #     parts.append("Battery")
    return "_".join(parts)


# -----------------------------
# Loaders
# -----------------------------
def load_sessions_csv(path: str, power_kw: float = POWER_KW, slack: float = FEASIBILITY_SLACK) -> pd.DataFrame:
    """
    Load EV sessions and drop those that cannot realistically be served.

    The filter uses the *same* power the simulator charges at — using a higher
    figure here keeps sessions the environment can then never satisfy.
    """
    df = pd.read_csv(path)
    missing = {"arrival", "departure", "station_id"} - set(df.columns)
    if missing:
        raise KeyError(f"{path} is missing required column(s): {sorted(missing)}")
    if "req_kwh" not in df.columns and "kwh_needed" not in df.columns:
        raise KeyError(f"{path} must have a 'req_kwh' or 'kwh_needed' column.")
    need_col = "req_kwh" if "req_kwh" in df.columns else "kwh_needed"

    # These files are month-first (e.g. '1/13/2025 13:15'); let pandas infer, but
    # fail loudly rather than silently coercing bad rows to NaT.
    df["arrival"] = pd.to_datetime(df["arrival"])
    df["departure"] = pd.to_datetime(df["departure"])
    if df["arrival"].isna().any() or df["departure"].isna().any():
        raise ValueError(f"{path} contains unparseable arrival/departure timestamps.")

    n_before = len(df)
    window_h = (df["departure"] - df["arrival"]).dt.total_seconds() / 3600
    df = df[window_h >= slack * df[need_col] / power_kw].reset_index(drop=True)
    df = df.sort_values("arrival").reset_index(drop=True)
    print(f"Sessions: kept {len(df)}/{n_before} "
          f"(feasible at {power_kw} kW with {slack}x slack)")
    return df


def load_price(path: str) -> pd.DataFrame:
    """Load a price series as columns [timestamp (dd-mm-YYYY), Tijd (HH:MM), price (EUR/kWh)]."""
    ext = os.path.splitext(path)[1].lower()

    if ext in (".xlsx", ".xls"):
        try:
            price_df = pd.read_excel(path)
        except ImportError as e:
            raise ImportError(
                f"Reading {path} needs the 'openpyxl' package: pip install -r requirements.txt"
            ) from e
    elif ext == ".csv":
        price_df = pd.read_csv(path, sep=";")
        if price_df.shape[1] == 1:  # not semicolon-separated after all
            price_df = pd.read_csv(path)
    else:
        raise ValueError(f"Unsupported file type: {ext}")

    if "price" not in price_df.columns:
        raise KeyError(f"{path} must have a 'price' column; got {list(price_df.columns)}")

    if "Tijd" not in price_df.columns:
        col = "timestamp"
        if col not in price_df.columns:
            raise KeyError(f"{path} must have 'Tijd' or 'timestamp'; got {list(price_df.columns)}")
        parsed = pd.to_datetime(price_df[col], errors="coerce")
        if parsed.isna().any():
            raise ValueError(f"{path} has {int(parsed.isna().sum())} unparseable timestamps.")
        # emit day-first strings, which is what EnergyEnv._load_prices expects
        price_df["Tijd"] = parsed.dt.strftime("%H:%M")
        price_df["timestamp"] = parsed.dt.strftime("%d-%m-%Y")

    price_df["price"] = price_df["price"] / 1000.0  # EUR/MWh -> EUR/kWh
    return price_df


def load_pv_dataset(path: str, sep: str = ";") -> pd.DataFrame:
    df = pd.read_csv(path, sep=sep)
    if df.shape[1] == 1:
        df = pd.read_csv(path)
    df.columns = df.columns.str.strip()
    return df


def load_consumption_dataset(path: str, sep: str = ";") -> pd.DataFrame:
    df = pd.read_csv(path, sep=sep)
    if df.shape[1] == 1:
        df = pd.read_csv(path)
    df.columns = df.columns.str.strip()
    return df


# -----------------------------
# Environment
# -----------------------------
def make_env(price_df: pd.DataFrame, sessions_df: pd.DataFrame, pv_df: pd.DataFrame = None,
             consumption_df: pd.DataFrame = None, power_kw: float = POWER_KW,
             penalty_per_kwh: float = PENALTY_PER_KWH, fixed_penalty: float = FIXED_PENALTY,
             progress_penalty: float | None = None, price_horizon: int = PRICE_HORIZON,
             pv_horizon: int = PV_HORIZON, clip_features: bool = True,
             price_step_minutes: int = PRICE_STEP_MINUTES, price_encoding: str = PRICE_ENCODING,
             verbose: bool = True) -> EnergyEnv:
    ev = EVComponent(
        sessions_df,
        power_kw=power_kw,
        penalty_per_kwh=penalty_per_kwh,
        fixed_penalty=fixed_penalty,
        progress_penalty=progress_penalty,
        clip_features=clip_features,
    )
    comps = [ev]
    if pv_df is not None:
        comps.append(PVComponent(pv_df, consumption_df=consumption_df, forecast_horizon=pv_horizon,
                                 clip_features=clip_features))
    # price scale comes from the span of the sessions the env is built on (the train
    # split), then stays frozen across reload_data() like every other constant
    sessions_period = (pd.to_datetime(sessions_df["arrival"]).min(),
                       pd.to_datetime(sessions_df["departure"]).max())
    env = EnergyEnv(price_df, components=comps, price_horizon=price_horizon,
                    price_step_minutes=price_step_minutes, price_encoding=price_encoding,
                    price_scale_period=sessions_period)

    if verbose:
        print("─" * 50)
        print("Environment configuration:")
        print(f"  EV charging   : ✓ ({ev.n_stations} stations, {len(sessions_df)} sessions, {power_kw} kW)")
        if pv_df is not None and consumption_df is not None:
            print(f"  PV production : ✓ (net of building consumption, {pv_horizon}-step forecast)")
        elif pv_df is not None:
            print(f"  PV production : ✓ (no building consumption, {pv_horizon}-step forecast)")
        else:
            print("  PV production : ✗ (not provided)")
        print(f"  Penalties     : {penalty_per_kwh} EUR/kWh unmet + {fixed_penalty} EUR fixed, "
              f"{ev.progress_penalty} EUR/interval behind schedule")
        print(f"  Feature clip  : {'✓ (state features bounded to the training box)' if clip_features else '✗ (legacy, unbounded)'}")
        span_h = (env.prices_required * env.interval_minutes) / 60
        if price_encoding == "window":
            print(f"  Prices        : {price_horizon} values every {price_step_minutes} min ({span_h:g} h ahead), "
                  f"min-max in window + level + spread (scale {env.price_scale:.4f} EUR/kWh)")
        else:
            print(f"  Prices        : {price_horizon} raw values every {price_step_minutes} min ({span_h:g} h ahead)")
        print(f"  Observation size : {env.observation_size}")
        print(f"  Action size      : {env.action_size}")
        print("─" * 50)

    return env


def make_deadline_guard(env: EnergyEnv, margin: float = None):
    """
    Force `charge` on any station that can no longer meet its deadline otherwise.

    Reads only the state vector the network itself is given, so the identical rule
    can be reproduced by the live service from its own state (see
    service/inference_server.py). It fires exactly when the env's own
    behind-schedule test fires, i.e. when

        remaining_kwh > hours_left * power_kw * feasibility_margin

    which in the state block is just `urgency > feasibility_margin`.

    This is a safety layer around the policy, not a change to the DQN: the agent
    still chooses freely everywhere the deadline is not at risk. It exists because
    undercharge failures here are driven by rare extreme sessions that no amount of
    training-side tuning caught reliably, and that the validation split contains no
    example of.
    """
    ev = env.components[0]
    prefix = env.state_prefix
    scale = ev.urgency_clip if ev.clip_features else 1.0
    thr = ev.feasibility_margin if margin is None else margin

    def guard(state_vec, actions):
        for i in range(ev.n_stations):
            present = state_vec[prefix + 4 * i]
            urgency = state_vec[prefix + 4 * i + 3] * scale
            if present > 0.5 and urgency > thr:
                actions[i] = 1
        return actions

    return guard


def load_datasets(args: argparse.Namespace):
    sessions_path = args.sessions or DEFAULT_SESSIONS
    price_path = args.price or DEFAULT_PRICE

    print(f"Price data   : {price_path}")
    print(f"Session data : {sessions_path}")

    sessions_df = load_sessions_csv(sessions_path, power_kw=args.power, slack=args.slack)
    price_df = load_price(price_path)
    pv_df = load_pv_dataset(args.pv) if args.pv else None
    consumption_df = load_consumption_dataset(args.consumption) if args.consumption else None

    if consumption_df is not None and pv_df is None:
        print("[WARN] --consumption has no effect without --pv; building load is only "
              "used to net down PV production.")
    return sessions_df, price_df, pv_df, consumption_df


def build_env_with_training_norms(args, price_df, train_df, test_df, pv_df, consumption_df,
                                  verbose: bool = True) -> EnergyEnv:
    """
    Build the env on the TRAIN split (fixing the normalisation constants) and then
    swap in the TEST sessions. reload_data deliberately leaves the constants
    alone, which is what keeps evaluation on the same input scale as training.
    """
    env = make_env(price_df, train_df, pv_df, consumption_df, power_kw=args.power,
                   penalty_per_kwh=args.penalty_per_kwh, fixed_penalty=args.fixed_penalty,
                   progress_penalty=args.progress_penalty, price_horizon=args.price_horizon,
                   pv_horizon=args.pv_horizon, clip_features=not args.legacy_features,
                   price_step_minutes=args.price_step_minutes, price_encoding=args.price_encoding,
                   verbose=verbose)
    # metadata (loaded by the caller) may still override clip_features for an old checkpoint
    env.reload_data(price_df, test_df)
    if verbose:
        print(f"  (env built on {len(train_df)} train sessions to fix normalisation, "
              f"then reloaded with {len(test_df)} test sessions)")
    return env


# -----------------------------
# Reporting
# -----------------------------
def _row_to_dict(tag: str, row: dict) -> dict:
    ev = (row.get("components") or {}).get("EVComponent", {})
    return {
        "mode":                tag,
        "energy_required_kwh": ev.get("required_kwh", 0.0),
        "delivered_kwh":       ev.get("delivered_kwh", 0.0),
        "unmet_kwh":           ev.get("unmet_kwh", 0.0),
        "energy_cost_eur":     ev.get("cost_eur", 0.0),
        "penalty_eur":         ev.get("penalty_eur", 0.0),
        "reward":              row.get("reward", 0.0),
    }


def _save_step_comparison(res_test: dict, res_rb: dict, path) -> None:
    """
    Merge TEST and ALWAYS step logs side by side into one CSV.
    Columns: timestamp, station_id, price,
             test_action, test_energy_kwh, test_pv_used_kwh, test_grid_kwh, test_cost_eur,
             rb_action,   rb_energy_kwh,   rb_pv_used_kwh,   rb_grid_kwh,   rb_cost_eur
    """
    try:
        df_test = (res_test.get("step_log") or {}).get("EVComponent")
        df_rb = (res_rb.get("step_log") or {}).get("EVComponent")
        if df_test is None or df_rb is None or df_test.empty or df_rb.empty:
            return

        metrics = ["action", "energy_kwh", "pv_used_kwh", "grid_kwh", "cost_eur"]

        def _prefix(df, tag):
            cols = {c: f"{tag}_{c}" for c in metrics}
            return df[["timestamp", "station_id", "price", "ev_present"] + metrics].rename(columns=cols)

        left = _prefix(df_test, "test")
        right = _prefix(df_rb, "rb")[["timestamp", "station_id"] + [f"rb_{c}" for c in metrics]]
        merged = left.merge(right, on=["timestamp", "station_id"], how="outer")
        merged = merged.sort_values(["timestamp", "station_id"]).reset_index(drop=True)
        Path(path).parent.mkdir(parents=True, exist_ok=True)
        merged.to_csv(path, index=False)
    except Exception as e:
        print(f"[WARN] Could not write step comparison CSV: {e}")


def write_reports(res_test: dict, res_rb: dict, out_dir: Path, verbose: bool) -> None:
    out_dir.mkdir(parents=True, exist_ok=True)
    row_test = (res_test.get("per_episode") or [{}])[0]
    row_rb = (res_rb.get("per_episode") or [{}])[0]
    cmp_path = out_dir / "summary_compare.csv"
    pd.DataFrame([_row_to_dict("TEST", row_test), _row_to_dict("ALWAYS", row_rb)]).to_csv(cmp_path, index=False)

    step_path = out_dir / "step_compare.csv"
    _save_step_comparison(res_test, res_rb, step_path)

    if verbose:
        print(f"Saved comparison to: {cmp_path}")
        print(f"Saved step log to:   {step_path}")


# -----------------------------
# Train flow
# -----------------------------
def cmd_train(args: argparse.Namespace) -> int:
    set_seeds(args.seed)

    verbose = (args.verbose.lower() == "yes")
    save_model = (args.save_model.lower() == "yes")

    run_folder = get_run_folder_name(args)
    out_dir = Path(DEFAULT_OUTPUT_DIR) / run_folder
    model_dir = Path(DEFAULT_SAVED_MODELS_DIR) / run_folder
    out_dir.mkdir(parents=True, exist_ok=True)
    model_dir.mkdir(parents=True, exist_ok=True)

    sessions_df, price_df, pv_df, consumption_df = load_datasets(args)
    train_df, val_df, test_df = split_df(sessions_df, test_size=TEST_SIZE, eval_size=args.val_size)
    print(f"Split: train={len(train_df)} val={len(val_df)} test={len(test_df)} (of {len(sessions_df)})")

    clip_features = not args.legacy_features
    env = make_env(price_df, train_df, pv_df, consumption_df, power_kw=args.power,
                   penalty_per_kwh=args.penalty_per_kwh, fixed_penalty=args.fixed_penalty,
                   progress_penalty=args.progress_penalty, price_horizon=args.price_horizon,
                   pv_horizon=args.pv_horizon, clip_features=clip_features,
                   price_step_minutes=args.price_step_minutes, price_encoding=args.price_encoding,
                   verbose=True)

    # Validation env: same normalisation constants (built on train, then reloaded),
    # a disjoint chronological slice, and never touched by the test evaluation.
    val_env = None
    if not val_df.empty:
        val_env = make_env(price_df, train_df, pv_df, consumption_df, power_kw=args.power,
                           penalty_per_kwh=args.penalty_per_kwh, fixed_penalty=args.fixed_penalty,
                           progress_penalty=args.progress_penalty, price_horizon=args.price_horizon,
                           pv_horizon=args.pv_horizon, clip_features=clip_features,
                           price_step_minutes=args.price_step_minutes, price_encoding=args.price_encoding,
                           verbose=False)
        val_env.reload_data(price_df, val_df)

    # DQNConfig re-seeds torch/numpy/random on construction, so --seed has to reach
    # it or every run trains from the same initialisation regardless of the flag.
    agent = DQNAgent(env.observation_size, env.action_size, cfg=DQNConfig(seed=args.seed),
                     save=save_model, save_dir=str(model_dir))
    if not args.no_deadline_guard:
        agent.action_guard = make_deadline_guard(env)
        agent.guard_meta = {"kind": "deadline_feasibility",
                            "margin": float(env.components[0].feasibility_margin)}
        print(f"  Deadline guard: ✓ (force charge when urgency > "
              f"{env.components[0].feasibility_margin})")

    print("Training Start.....")
    agent.train(env, verbose=verbose, episodes=args.num_episodes,
                val_env=val_env, val_every=args.val_every)

    # Evaluate on TEST. reload_data keeps the training normalisation constants.
    env.reload_data(price_df, test_df)
    res_test = agent.evaluate(env, greedy=True, verbose=verbose, save_prefix=str(out_dir / "test"))
    res_rb = agent.evaluate_always_charge(env, verbose=verbose, save_prefix=str(out_dir / "always_charge"))

    write_reports(res_test, res_rb, out_dir, verbose)
    return 0


# -----------------------------
# Test/Eval flow
# -----------------------------
def _load_state_dict(path: str, device, allow_unsafe: bool = False):
    """
    Load a checkpoint. Training always writes plain state_dicts, so the safe
    weights-only path is the norm; full pickled modules require an explicit
    opt-in because unpickling them executes arbitrary code.
    """
    try:
        return torch.load(path, map_location=device, weights_only=True)
    except Exception as e_state:
        if not allow_unsafe:
            raise RuntimeError(
                f"Could not load {path} as a state_dict: {e_state}\n"
                "If this is a legacy checkpoint containing a pickled QNetwork module, "
                "re-run with --allow-unsafe-load (this executes code from the file)."
            ) from e_state
        obj = torch.load(path, map_location=device, weights_only=False)
        return obj.state_dict() if hasattr(obj, "state_dict") else obj


def cmd_test(args: argparse.Namespace) -> int:
    set_seeds(args.seed)

    verbose = (args.verbose.lower() == "yes")
    run_folder = get_run_folder_name(args)
    model_dir = Path(DEFAULT_SAVED_MODELS_DIR) / run_folder

    q_path = Path(args.q) if args.q else model_dir / Q_FILENAME
    q_tgt_path = Path(args.qtarget) if args.qtarget else model_dir / Q_TARGET_FILENAME
    meta_path = Path(args.metadata) if args.metadata else (q_path.parent / METADATA_FILENAME)

    print(f"Q-network paths: q={q_path}, q_target={q_tgt_path}")
    missing = [p for p in (q_path, q_tgt_path) if not p.exists()]
    if missing:
        print(f"[ERROR] Model file(s) not found: {', '.join(str(p) for p in missing)}\n"
              "Pass custom paths with --q and --qtarget, or run `train` first so defaults are created.")
        return 2

    sessions_df, price_df, pv_df, consumption_df = load_datasets(args)
    train_df, val_df, test_df = split_df(sessions_df, test_size=TEST_SIZE, eval_size=args.val_size)
    print(f"Split: train={len(train_df)} val={len(val_df)} test={len(test_df)} (of {len(sessions_df)})")

    # The state layout (price encoding, horizons) belongs to the checkpoint, not to this
    # invocation: take it from metadata so `test` needs no matching flags. Checkpoints
    # predating the price encoding used 4 raw quarter-hour prices.
    meta = DQNAgent.load_metadata(meta_path)
    if meta:
        env_meta = (meta.get("norm_state") or {}).get("EnergyEnv")
        if env_meta:
            args.price_encoding = env_meta["price_encoding"]
            args.price_horizon = int(env_meta["price_horizon"])
            args.price_step_minutes = int(env_meta["price_step_minutes"])
        else:
            args.price_encoding = "raw"
            args.price_horizon = int(meta.get("price_horizon", 4))
            args.price_step_minutes = int(meta.get("interval_minutes", 15))
        pv_meta = (meta.get("norm_state") or {}).get("PVComponent")
        if pv_meta and "forecast_horizon" in pv_meta:
            args.pv_horizon = int(pv_meta["forecast_horizon"])

    env = build_env_with_training_norms(args, price_df, train_df, test_df, pv_df, consumption_df)

    if meta:
        if meta.get("observation_size") != env.observation_size or meta.get("action_size") != env.action_size:
            print(f"[ERROR] Checkpoint expects obs={meta.get('observation_size')} "
                  f"act={meta.get('action_size')} but this env is obs={env.observation_size} "
                  f"act={env.action_size}. Did you forget --pv/--consumption?")
            return 2
        env.set_norm_state(meta.get("norm_state", {}))
        print(f"Loaded normalisation constants from {meta_path}")
        # metadata wins over the CLI flag: a checkpoint must be fed the state
        # convention it was trained under, whatever this run was invoked with.
        ev_clip = getattr(env.components[0], "clip_features", None)
        if ev_clip is not None:
            print(f"  Effective feature clip: {'on' if ev_clip else 'off (pre-fix checkpoint)'}")
    else:
        print(f"[WARN] No {METADATA_FILENAME} next to the checkpoint; using constants derived "
              "from the training split instead. Re-run `train` to regenerate it.")

    agent = DQNAgent(env.observation_size, env.action_size, save=False)
    device = agent.device

    agent.q.load_state_dict(_load_state_dict(str(q_path), device, args.allow_unsafe_load))
    agent.q_target.load_state_dict(_load_state_dict(str(q_tgt_path), device, args.allow_unsafe_load))
    agent.q.eval()
    agent.q_target.eval()

    # The guard is part of the policy that was trained, so it has to come from the
    # checkpoint, not from this invocation's flags.
    guard_meta = (meta or {}).get("action_guard")
    if guard_meta:
        agent.action_guard = make_deadline_guard(env, margin=guard_meta.get("margin"))
        agent.guard_meta = guard_meta
        print(f"  Deadline guard: ✓ from checkpoint (margin {guard_meta.get('margin')})")
    else:
        print("  Deadline guard: ✗ (checkpoint trained without it)")

    if args.save_prefix:
        prefix = args.save_prefix
        out_dir = Path(prefix).parent
    else:
        out_dir = Path(DEFAULT_OUTPUT_DIR) / run_folder
        prefix = str(out_dir / "test")

    print("Testing Start.....")
    res_test = agent.evaluate(env, greedy=True, verbose=verbose, save_prefix=prefix)
    res_rb = agent.evaluate_always_charge(env, verbose=verbose, save_prefix=f"{prefix}_always")

    write_reports(res_test, res_rb, out_dir, verbose)
    return 0


# -----------------------------
# Argparse
# -----------------------------
def _add_common(p: argparse.ArgumentParser) -> None:
    p.add_argument("--sessions", type=str, default=None, help=f"Path to sessions CSV (default: {DEFAULT_SESSIONS})")
    p.add_argument("--price", type=str, default=None, help=f"Path to price XLSX/CSV (default: {DEFAULT_PRICE})")
    p.add_argument("--pv", type=str, default=None, help=f"Path to PV dataset CSV (default: no PV, e.g. {EXAMPLE_PV})")
    p.add_argument("--consumption", type=str, default=None,
                   help=f"Path to building consumption CSV, requires --pv (e.g. {EXAMPLE_CONSUMPTION})")
    p.add_argument("--power", type=float, default=POWER_KW,
                   help=f"Charging power in kW, used by both the filter and the simulator (default: {POWER_KW})")
    p.add_argument("--slack", type=float, default=FEASIBILITY_SLACK,
                   help=f"Feasibility slack multiplier for the session filter (default: {FEASIBILITY_SLACK})")
    p.add_argument("--penalty-per-kwh", type=float, default=PENALTY_PER_KWH,
                   help=f"EUR per kWh unmet at departure (default: {PENALTY_PER_KWH})")
    p.add_argument("--fixed-penalty", type=float, default=FIXED_PENALTY,
                   help=f"Flat EUR penalty for an undercharged departure (default: {FIXED_PENALTY})")
    p.add_argument("--progress-penalty", type=float, default=None,
                   help="EUR charged each interval an EV is behind schedule "
                        "(default: fixed-penalty/2; set 0 to disable this shaping)")
    p.add_argument("--price-step-minutes", type=int, default=PRICE_STEP_MINUTES,
                   help=f"Spacing of the price values in the state, a multiple of 15 (default: "
                        f"{PRICE_STEP_MINUTES}). The first value is the current quarter-hour; each "
                        f"later one averages the quarter-hours in its step. `test` takes it from the checkpoint.")
    p.add_argument("--price-encoding", choices=["window", "raw"], default=PRICE_ENCODING,
                   help="window: prices min-max scaled within the window plus a price level and "
                        "spread, all bounded (default). raw: EUR/kWh as-is, the pre-change state. "
                        "`test` takes it from the checkpoint.")
    p.add_argument("--price-horizon", type=int, default=PRICE_HORIZON,
                   help=f"Number of forward-looking price values in the state, incl. the current "
                        f"one (default: {PRICE_HORIZON}; with a {PRICE_STEP_MINUTES}-min step that is "
                        f"6 h ahead). The live service then needs 1 + (horizon-1) * step/15 "
                        f"quarter-hour prices per request. `test` takes it from the checkpoint.")
    p.add_argument("--pv-horizon", type=int, default=PV_HORIZON,
                   help=f"Forward-looking PV forecast steps in the state, incl. the current one "
                        f"(default: {PV_HORIZON} = 1h at 15-min steps). Only used with --pv. "
                        f"Must match the live service's 'pv_forecast' array length.")
    p.add_argument("--seed", type=int, default=SEED, help=f"Random seed (default: {SEED})")
    p.add_argument("--val-size", type=float, default=VAL_SIZE,
                   help=f"Fraction of sessions held out (chronologically, between train and test) "
                        f"to pick the best checkpoint (default: {VAL_SIZE}; 0 disables and keeps "
                        f"whatever the last episode produced). `test` must use the same value as "
                        f"`train` so the normalisation split lines up.")
    p.add_argument("--no-deadline-guard", action="store_true",
                   help="Disable the safety layer that forces charging on a station that can no "
                        "longer meet its deadline. On by default: undercharge failures here are "
                        "driven by rare extreme sessions that no training-side setting caught "
                        "reliably and that the validation split contains no example of.")
    p.add_argument("--legacy-features", action="store_true",
                   help="Disable state-feature clipping (reproduces pre-fix checkpoints, which "
                        "extrapolate on any session bigger/longer than the training maximum)")
    p.add_argument("--verbose", choices=["yes", "no"], default="yes", help="Verbose output (default: yes)")


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="ev-dqn",
        description="Train or test the EV charging DQN (70/30 per-station chronological split).",
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    pt = sub.add_parser("train", help="Train the agent and evaluate on the test split.")
    _add_common(pt)
    pt.add_argument("--save-model", choices=["yes", "no"], default="yes", help="Save checkpoints (default: yes)")
    pt.add_argument("--num_episodes", type=int, default=EPISODES,
                    help=f"Number of training episodes (default: {EPISODES})")
    pt.add_argument("--val-every", type=int, default=1,
                    help="Run the validation evaluation every N episodes (default: 1)")
    pt.set_defaults(func=cmd_train)

    pe = sub.add_parser("test", help="Load q/q_target and evaluate on the test split (policy & always-charge).")
    _add_common(pe)
    pe.add_argument("--q", type=str, default=None,
                    help=f"Path to saved q state_dict (default: {DEFAULT_SAVED_MODELS_DIR}/<run>/{Q_FILENAME})")
    pe.add_argument("--qtarget", type=str, default=None,
                    help=f"Path to saved q_target state_dict (default: {DEFAULT_SAVED_MODELS_DIR}/<run>/{Q_TARGET_FILENAME})")
    pe.add_argument("--metadata", type=str, default=None,
                    help=f"Path to {METADATA_FILENAME} (default: next to the q checkpoint)")
    pe.add_argument("--allow-unsafe-load", action="store_true",
                    help="Permit loading legacy pickled-module checkpoints (executes code from the file)")
    pe.add_argument("--save-prefix", type=str, default=None,
                    help="Prefix for the evaluation CSVs (default: outputs/<run>/test)")
    pe.set_defaults(func=cmd_test)

    return p


def main(argv=None) -> int:
    argv = argv if argv is not None else sys.argv[1:]
    args = build_parser().parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
