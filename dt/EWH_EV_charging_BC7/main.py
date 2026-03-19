#!/usr/bin/env python3
"""
Command-line entry point to train/test the EV-charging DQN.

Subcommands
-----------
- train: trains on a 70/30 split (per-station, chronological) and evaluates on TEST.
- test : loads q and q_target (defaults match training save paths) and evaluates on TEST; also runs always-charge.

Defaults mirror the current main_def.py behavior.
"""

import os, sys
ROOT = os.path.dirname(os.path.abspath(__file__))  # folder with main.py and 'src'
if ROOT not in sys.path:
    sys.path.insert(0, ROOT)

import argparse
from pathlib import Path
import random
import sys
import torch
import numpy as np
import pandas as pd

from src.agent.dqn_agent import DQNAgent
from src.env.env import EVChargingEnv

# -----------------------------
# Utility: dataset split (same behavior as main_def.py)
# -----------------------------
def split_df(df: pd.DataFrame, test_size: float = 0.3, eval_size: float = 0.0, min_per_split: int = 1):
    df = df.copy()
    df["arrival"] = pd.to_datetime(df["arrival"])
    df = df.sort_values(["station_id", "arrival"])
    train_parts, eval_parts, test_parts = [], [], []
    for sid, block in df.groupby("station_id"):
        block = block.sort_values("arrival").reset_index(drop=True)
        n = len(block)
        if n < (2 * min_per_split + min_per_split):
            if n == 1:
                train_parts.append(block)
            elif n == 2:
                train_parts.append(block.iloc[:1])
                eval_parts.append(block.iloc[1:])
            else:  # n >= 3
                train_parts.append(block.iloc[:1])
                eval_parts.append(block.iloc[1:2])
                test_parts.append(block.iloc[2:])
            continue
        n_test = int(n * test_size)
        n_eval = int((n - n_test) * eval_size)
        n_train = n - n_test - n_eval
        if n_train < min_per_split:
            diff = min_per_split - n_train
            n_train += diff
            n_eval -= min(diff, n_eval)
            n_test -= max(0, diff - n_eval)
        if n_eval < min_per_split:
            diff = min_per_split - n_eval
            n_eval += diff
            n_train -= min(diff, n_train)
            n_test -= max(0, diff - n_train)
        if n_test < min_per_split:
            diff = min_per_split - n_test
            n_test += diff
            n_eval -= min(diff, n_eval)
            n_train -= max(0, diff - n_eval)
        train_parts.append(block.iloc[:n_train])
        eval_parts.append(block.iloc[n_train:n_train + n_eval])
        test_parts.append(block.iloc[n_train + n_eval:])
    train_df = pd.concat(train_parts).reset_index(drop=True)
    eval_df = pd.concat(eval_parts).reset_index(drop=True) if eval_parts else pd.DataFrame(columns=df.columns)
    test_df = pd.concat(test_parts).reset_index(drop=True)
    train_df = train_df.sort_values(["station_id", "arrival"]).reset_index(drop=True)
    eval_df = eval_df.sort_values(["station_id", "arrival"]).reset_index(drop=True) if not eval_df.empty else eval_df
    test_df = test_df.sort_values(["station_id", "arrival"]).reset_index(drop=True)
    return train_df, eval_df, test_df


# -----------------------------
# Defaults from your current scripts
# -----------------------------
DEFAULT_SESSIONS = "src/env/dataset/ewh_cleaned_rtu.csv"
DEFAULT_PRICE = "src/env/dataset/price_two_years_15min.xlsx"
DEFAULT_OUTPUT_DIR = "outputs"
DEFAULT_SAVED_MODELS_DIR = "saved_models"

# NEW: default model paths aligned with your agent's saving behavior
DEFAULT_Q = "saved_models/q_state_dict.pth"
DEFAULT_QTARGET = "saved_models/q_target_state_dict.pth"

POWER = 11
PENALTY_PER_KWH = 2
FIXED_PENALTY = 10
EPISODES = 50
SEED = 1


def set_seeds(seed: int = SEED):
    np.random.seed(seed)
    torch.manual_seed(seed)
    random.seed(seed)
    if torch.cuda.is_available():
        torch.cuda.manual_seed_all(seed)


def load_sessions_csv(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    df["arrival"] = pd.to_datetime(df["arrival"])
    df["departure"] = pd.to_datetime(df["departure"])
    # same cleaning as in main_def.py
    mask = (df["departure"] - df["arrival"]).dt.total_seconds() / 3600 >= 1.5 * df["req_kwh"] / POWER
    df = df[mask].reset_index(drop=True)
    df = df.sort_values(["arrival"])
    return df


def load_price_xlsx(path: str) -> pd.DataFrame:
    price_df = pd.read_excel(path)
    # same normalization as in main_def.py
    price_df["price"] = price_df["price"] / 1000.0
    return price_df


def make_env(price_df: pd.DataFrame, sessions_df: pd.DataFrame) -> EVChargingEnv:
    env = EVChargingEnv(
        price_df=price_df,
        sessions_df=sessions_df,
        power_kw=POWER,
        interval_minutes=15,
        price_window=6,
        penalty_per_kwh=PENALTY_PER_KWH,
        fixed_penalty=FIXED_PENALTY,
        skip_empty=True,
    )
    return env


# -----------------------------
# Train flow
# -----------------------------
def cmd_train(args: argparse.Namespace) -> int:
    set_seeds(SEED)

    sessions_path = args.sessions or DEFAULT_SESSIONS
    price_path = args.price or DEFAULT_PRICE
    verbose = (args.verbose.lower() == "yes")
    save_model = (args.save_model.lower() == "yes")

    out_dir = Path(DEFAULT_OUTPUT_DIR)
    model_dir = Path(DEFAULT_SAVED_MODELS_DIR)
    out_dir.mkdir(parents=True, exist_ok=True)
    model_dir.mkdir(parents=True, exist_ok=True)

    sessions_df = load_sessions_csv(sessions_path)
    price_df = load_price_xlsx(price_path)

    # 70/30 split as requested
    train_df, _, test_df = split_df(sessions_df, test_size=0.3, eval_size=0.0)

    env = make_env(price_df=price_df, sessions_df=train_df)

    # Agent (save flag is honored by agent_cl.DQNAgent)
    agent = DQNAgent(env.observation_size, env.action_size, save=save_model)

    # Train
    agent.train(env, verbose=verbose, episodes=EPISODES)

    # Evaluate on TEST (index 2), and Always Charge
    env.reload_data(price_df, test_df)
    res_test = agent.evaluate(env, greedy=True, verbose=verbose, save_prefix=str(out_dir / "test"))
    res_rb = agent.evaluate_always_charge(env, verbose=verbose, save_prefix=str(out_dir / "always_charge"))

    # Comparison CSV
    def _row_to_dict(tag, row):
        return {
            'mode': tag,
            'energy_required_kwh': row.get('energy_required_kwh', 0.0),
            'delivered_kwh': row.get('delivered_kwh', 0.0),
            'unmet_kwh': row.get('unmet_kwh', 0.0),
            'energy_cost_eur': row.get('energy_cost_eur', 0.0),
            'penalty_eur': row.get('penalty_eur', 0.0),
            'avg_price_eur_per_kwh': row.get('avg_price_paid_eur_per_kwh', 0.0),
            'hours_total': row.get('hours_total', 0.0),
            'hours_charging': row.get('hours_charging', 0.0),
            'charge_utilization': row.get('charge_utilization', 0.0),
        }

    row_test = (res_test.get('per_episode') or [{}])[0]
    row_rb = (res_rb.get('per_episode') or [{}])[0]
    df_cmp = pd.DataFrame([
        _row_to_dict('TEST', row_test),
        _row_to_dict('ALWAYS', row_rb),
    ])
    df_cmp.to_csv(out_dir / 'summary_compare.csv', index=False)

    if verbose:
        print(f"Saved per-EV CSVs to: {res_test.get('csv')}, {res_rb.get('csv')}\n"
              f"Comparison to: {out_dir / 'summary_compare.csv'}")

    return 0


# -----------------------------
# Test/Eval flow
# -----------------------------
def _try_load_q(path: str, device: str):
    """
    Load a checkpoint in a PyTorch-2.6+ friendly way:
    1) Try weights-only (state_dict) – the new, safe way.
    2) Fallback (compat): allowlist QNetwork and load full pickled objects
       with weights_only=False (ONLY if you trust the file).
    Returns either:
       - a dict (state_dict), or
       - a full nn.Module (legacy)
    """
    import torch
    # 1) Preferred path: weights_only=True (state_dict)
    try:
        return torch.load(path, map_location=device, weights_only=True)
    except Exception as e_state:
        # 2) Back-compat path: legacy pickled module (if you trust the source)
        try:
            from src.agent.helpers.NN.NN import QNetwork
            import torch.serialization
            torch.serialization.add_safe_globals([QNetwork])  # allowlist your class

            return torch.load(path, map_location=device, weights_only=False)
        except Exception as e_legacy:
            raise RuntimeError(
                f"Failed to load checkpoint:\n"
                f"  as state_dict (weights_only=True): {e_state}\n"
                f"  as legacy module (weights_only=False): {e_legacy}\n"
                f"Path: {path}"
            )


def cmd_test(args: argparse.Namespace) -> int:
    set_seeds(SEED)

    sessions_path = args.sessions or DEFAULT_SESSIONS
    price_path = args.price or DEFAULT_PRICE
    verbose = (args.verbose.lower() == "yes")
    save_prefix = args.save_prefix  # may be None

    # Resolve model paths (defaults aim at where train saves them)
    q_path = Path(args.q or DEFAULT_Q)
    q_tgt_path = Path(args.qtarget or DEFAULT_QTARGET)

    if not q_path.exists() or not q_tgt_path.exists():
        print("[ERROR] Model file(s) not found. "
              f"q: {q_path} exists={q_path.exists()} | qtarget: {q_tgt_path} exists={q_tgt_path.exists()}\n"
              "Pass custom paths with --q and --qtarget, or run `train` first so defaults are created.")
        return 2

    sessions_df = load_sessions_csv(sessions_path)
    price_df = load_price_xlsx(price_path)

    # Use TEST split (30%)
    _, _, test_df = split_df(sessions_df, test_size=0.3, eval_size=0.0)

    env = make_env(price_df=price_df, sessions_df=test_df)

    # Build agent with matching dims, then load q and q_target
    agent = DQNAgent(env.observation_size, env.action_size, save=False)
    device = agent.cfg.device if hasattr(agent, 'cfg') else ("cuda" if torch.cuda.is_available() else "cpu")

    # Load q and q_target (state_dict preferred; legacy module also supported)
    q_obj = _try_load_q(str(q_path), device)
    q_target_obj = _try_load_q(str(q_tgt_path), device)

    def _apply_loaded(obj, net):
        """
        If obj is a state_dict -> load into 'net'.
        If obj is a full module -> copy weights into 'net' (same arch required).
        """
        import torch.nn as nn
        if isinstance(obj, dict):
            net.load_state_dict(obj)
        else:
            # full module – copy parameters
            net.load_state_dict(obj.state_dict())

    # Apply to agent's networks
    _apply_loaded(q_obj, agent.q)
    _apply_loaded(q_target_obj, agent.q_target)
    agent.q_target.eval()
    # Run evals
    res_test = agent.evaluate(env, greedy=True, verbose=verbose, save_prefix=(save_prefix if save_prefix else None))
    res_rb = agent.evaluate_always_charge(env, verbose=verbose, save_prefix=(save_prefix + "_always" if save_prefix else None))

    # Optional comparison CSV when a save_prefix is provided
    if save_prefix:
        try:
            out_dir = Path(save_prefix).parent if Path(save_prefix).suffix else Path(save_prefix)
            out_dir.mkdir(parents=True, exist_ok=True)

            def _row_to_dict(tag, row):
                return {
                    'mode': tag,
                    'energy_required_kwh': row.get('energy_required_kwh', 0.0),
                    'delivered_kwh': row.get('delivered_kwh', 0.0),
                    'unmet_kwh': row.get('unmet_kwh', 0.0),
                    'energy_cost_eur': row.get('energy_cost_eur', 0.0),
                    'penalty_eur': row.get('penalty_eur', 0.0),
                    'avg_price_eur_per_kwh': row.get('avg_price_paid_eur_per_kwh', 0.0),
                    'hours_total': row.get('hours_total', 0.0),
                    'hours_charging': row.get('hours_charging', 0.0),
                    'charge_utilization': row.get('charge_utilization', 0.0),
                }

            row_test = (res_test.get('per_episode') or [{}])[0]
            row_rb = (res_rb.get('per_episode') or [{}])[0]
            df_cmp = pd.DataFrame([
                _row_to_dict('TEST', row_test),
                _row_to_dict('ALWAYS', row_rb),
            ])
            cmp_path = Path(save_prefix).with_name('summary_compare.csv') if Path(save_prefix).suffix else Path(save_prefix) / 'summary_compare.csv'
            df_cmp.to_csv(cmp_path, index=False)
            if verbose:
                print(f"Comparison written to: {cmp_path}")
        except Exception as e:
            print(f"[WARN] Could not write comparison CSV: {e}")

    return 0


# -----------------------------
# Argparse
# -----------------------------
def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="ev-dqn",
        description="Train or test the EV charging DQN (70/30 split kept).",
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    # Train subcommand
    pt = sub.add_parser("train", help="Train the agent and evaluate on test split.")
    pt.add_argument("--sessions", type=str, default=None, help=f"Path to sessions CSV (default: {DEFAULT_SESSIONS})")
    pt.add_argument("--price", type=str, default=None, help=f"Path to price XLSX (default: {DEFAULT_PRICE})")
    pt.add_argument("--verbose", choices=["yes", "no"], default="yes", help="Verbose output (default: yes)")
    pt.add_argument("--save-model", choices=["yes", "no"], default="yes", help="Save model checkpoints (default: yes)")
    pt.set_defaults(func=cmd_train)

    # Test subcommand
    pe = sub.add_parser("test", help="Load q/q_target and evaluate on test split (policy & always-charge).")
    pe.add_argument("--sessions", type=str, default=None, help=f"Path to sessions CSV (default: {DEFAULT_SESSIONS})")
    pe.add_argument("--price", type=str, default=None, help=f"Path to price XLSX (default: {DEFAULT_PRICE})")
    # NEW: defaults point to where training saves them
    pe.add_argument("--q", type=str, default=DEFAULT_Q, help=f"Path to saved q model or state_dict (default: {DEFAULT_Q})")
    pe.add_argument("--qtarget", type=str, default=DEFAULT_QTARGET, help=f"Path to saved q_target model or state_dict (default: {DEFAULT_QTARGET})")
    pe.add_argument("--verbose", choices=["yes", "no"], default="yes", help="Verbose output (default: yes)")
    pe.add_argument("--save-prefix", type=str, default=None, help="Prefix for saving per-EV CSVs; if omitted, nothing is saved")
    pe.set_defaults(func=cmd_test)

    return p


def main(argv=None) -> int:
    argv = argv if argv is not None else sys.argv[1:]
    parser = build_parser()
    args = parser.parse_args(argv)
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())