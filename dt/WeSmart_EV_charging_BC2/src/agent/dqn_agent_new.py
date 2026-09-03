from dataclasses import dataclass, asdict
from pathlib import Path
import json
import random
import numpy as np
import torch
import torch.nn.functional as F
import torch.optim as optim

# Project-specific imports (kept as in your original project)
from src.agent.helpers.NN.NN import QNetwork
from src.agent.helpers.replay_buffer.replay_buffer import ReplayBuffer

METADATA_FILENAME = "metadata.json"
Q_FILENAME = "q_state_dict.pth"
Q_TARGET_FILENAME = "q_target_state_dict.pth"


@dataclass
class DQNConfig:
    gamma: float = .97
    lr: float = 0.0001
    batch_size: int = 64
    buffer_capacity: int = 10_000
    epsilon_start: float = 1.0
    epsilon_end: float = 0.0
    epsilon_decay_steps: int = 350_000  # linear decay steps
    target_update_freq: int = 500  # hard update every N gradient steps
    hidden_sizes: tuple = (64, 64)
    seed: int | None = 1
    device: str = "cuda" if torch.cuda.is_available() else "cpu"
    max_grad_norm: float | None = None  # set None to disable gradient clipping


class DQNAgent:
    """
    Per-station binary DQN.

    The network emits ``action_dim * 2`` values reshaped to (n_stations, 2), so
    each station gets its own independent charge/don't-charge decision rather
    than the agent enumerating a 2^n joint action space. Q(s, a) is the sum of
    the per-station values and the bootstrap target is the sum of per-station
    maxima — i.e. a value-decomposition (VDN-style) factorisation of a DQN.
    """

    def __init__(
        self,
        state_dim: int,
        action_dim: int,
        cfg: DQNConfig | None = None,
        save: bool = True,
        save_dir: str = "./saved_models",
    ):
        """
        Args:
            state_dim: dimension of the flattened observation vector
            action_dim: number of stations (each station has 2 discrete actions: {0,1})
        """
        # a shared default DQNConfig() instance would be built once at import and
        # leak mutations across every agent, so build a fresh one per agent
        self.cfg = cfg if cfg is not None else DQNConfig()
        cfg = self.cfg
        self.device = torch.device(cfg.device)
        self.state_dim = int(state_dim)

        # RNGs
        if cfg.seed is not None:
            random.seed(cfg.seed)
            np.random.seed(cfg.seed)
            torch.manual_seed(cfg.seed)
            if torch.cuda.is_available():
                torch.cuda.manual_seed_all(cfg.seed)

        # Q networks: output has shape action_dim * 2 (per-station binary)
        self.action_dim = int(action_dim)
        self.q = QNetwork(state_dim, self.action_dim * 2, hidden_sizes=cfg.hidden_sizes).to(self.device)
        self.q_target = QNetwork(state_dim, self.action_dim * 2, hidden_sizes=cfg.hidden_sizes).to(self.device)
        self.q_target.load_state_dict(self.q.state_dict())
        self.q_target.eval()

        # Optimizer and buffer
        self.optim = optim.Adam(self.q.parameters(), lr=cfg.lr)
        self.replay = ReplayBuffer(capacity=cfg.buffer_capacity, seed=cfg.seed)

        # Exploration schedule
        self.steps_done = 0  # counts action selections
        self.epsilon = cfg.epsilon_start

        # Track gradient steps for target updates (fixes mismatch)
        self.grad_steps = 0

        self.save = save
        self.save_dir = save_dir

    # ------------------- Policy -------------------
    def _epsilon_now(self) -> float:
        frac = min(1.0, self.steps_done / max(1, self.cfg.epsilon_decay_steps))
        return max(self.cfg.epsilon_end,
                   self.cfg.epsilon_start * (1.0 - frac) + self.cfg.epsilon_end * frac)

    def select_action(self, state_vec: np.ndarray) -> np.ndarray:
        """ε-greedy per-station binary action. Returns an int array of shape (S,) with values in {0,1}."""
        self.steps_done += 1
        self.epsilon = self._epsilon_now()

        if random.random() < self.epsilon:
            return np.random.randint(0, 2, size=self.action_dim)

        with torch.no_grad():
            s = torch.tensor(state_vec, dtype=torch.float32, device=self.device).unsqueeze(0)
            q_vals = self.q(s).view(self.action_dim, 2)  # (S,2)
            return q_vals.argmax(dim=-1).detach().cpu().numpy().astype(np.int64)  # (S,)

    def select_action_greedy(self, state_vec: np.ndarray) -> np.ndarray:
        """Greedy w.r.t. Q (no exploration)."""
        was_training = self.q.training
        self.q.eval()
        with torch.no_grad():
            s = torch.tensor(state_vec, dtype=torch.float32, device=self.device).unsqueeze(0)
            q_vals = self.q(s).view(self.action_dim, 2)
            action = q_vals.argmax(dim=-1).detach().cpu().numpy().astype(np.int64)
        if was_training:
            self.q.train()
        return action

    # ------------------- Learning -------------------
    def optimize(self) -> None:
        if len(self.replay) < self.cfg.batch_size:
            return

        s_b, a_b, r_b, ns_b, d_b = self.replay.sample(self.cfg.batch_size)
        s = torch.tensor(s_b, dtype=torch.float32, device=self.device)  # (B, D)
        ns = torch.tensor(ns_b, dtype=torch.float32, device=self.device)  # (B, D)
        a = torch.tensor(a_b, dtype=torch.int64, device=self.device)  # (B, S) entries in {0,1}
        r = torch.tensor(r_b, dtype=torch.float32, device=self.device)  # (B,)
        d = torch.tensor(d_b, dtype=torch.float32, device=self.device)  # (B,)

        B = s.shape[0]

        # Q(s, a): sum over stations of Q_s(s, a_s)
        q_s = self.q(s).view(B, self.action_dim, 2)  # (B, S, 2)
        q_sa = q_s.gather(dim=2, index=a.unsqueeze(-1)).squeeze(-1)  # (B, S)
        q_sa_sum = q_sa.sum(dim=1)  # (B,)

        with torch.no_grad():
            q_ns = self.q_target(ns).view(B, self.action_dim, 2)  # (B, S, 2)
            q_ns_sum = q_ns.max(dim=2).values.sum(dim=1)  # (B,)
            target = r + self.cfg.gamma * q_ns_sum * (1.0 - d)

        loss = F.mse_loss(q_sa_sum, target)
        self.optim.zero_grad()
        loss.backward()
        if self.cfg.max_grad_norm is not None:
            torch.nn.utils.clip_grad_norm_(self.q.parameters(), self.cfg.max_grad_norm)
        self.optim.step()

        # Hard target update every N gradient steps
        self.grad_steps += 1
        if self.grad_steps % self.cfg.target_update_freq == 0:
            self.q_target.load_state_dict(self.q.state_dict())

    # ------------------- Checkpointing -------------------
    def save_checkpoint(self, env, save_dir: str | None = None) -> Path:
        """
        Persist weights *and* the normalisation constants they were trained with.

        Without the metadata, evaluating on a different split silently rescales
        the inputs (a test split has different max kWh / max duration), so the
        network sees out-of-distribution states.
        """
        path = Path(save_dir or self.save_dir)
        path.mkdir(parents=True, exist_ok=True)
        torch.save(self.q.state_dict(), path / Q_FILENAME)
        torch.save(self.q_target.state_dict(), path / Q_TARGET_FILENAME)

        cfg = asdict(self.cfg)
        cfg["hidden_sizes"] = list(cfg["hidden_sizes"])
        meta = {
            "state_dim": self.state_dim,
            "action_dim": self.action_dim,
            "observation_size": int(env.observation_size),
            "action_size": int(env.action_size),
            "price_horizon": int(getattr(env, "price_horizon", 0)),
            "interval_minutes": int(getattr(env, "interval_minutes", 15)),
            "components": [type(c).__name__ for c in env.components],
            "norm_state": env.get_norm_state(),
            "config": cfg,
        }
        (path / METADATA_FILENAME).write_text(json.dumps(meta, indent=2), encoding="utf-8")
        print(f"Models saved (state_dict + {METADATA_FILENAME}) to {path}")
        return path

    @staticmethod
    def load_metadata(path: str | Path) -> dict | None:
        p = Path(path)
        if not p.exists():
            return None
        return json.loads(p.read_text(encoding="utf-8"))

    # ------------------- Training loop -------------------
    def train(self, env, episodes: int = 1000, verbose: bool = True, start_date=None):
        for ep in range(episodes):
            obs = env.reset(start_date=start_date)  # np.ndarray state vector
            state = env.normalize_state(obs)
            done = False
            total_reward = 0.0
            info_history = []

            while not done:
                action = self.select_action(state)  # (S,) ints in {0,1}
                next_obs, reward, done, info = env.step(action)
                info_history.append(info)

                if done or next_obs is None:
                    next_state = np.zeros_like(state)  # terminal placeholder
                else:
                    next_state = env.normalize_state(next_obs)

                self.replay.push(state, action, float(reward), next_state, float(done))
                self.optimize()

                state = next_state
                total_reward += float(reward)

            if verbose:
                print(f"Ep {ep + 1:4d} | R={total_reward:.3f} | ε={self.epsilon:.3f} | "
                      + self._format_metrics(env, info_history))

            # sanity check against the greedy policy — note this runs on the
            # *training* data, so it is a fit check, not a generalisation check
            if ep > 0 and ep % 10 == 0:
                self.evaluate(env, greedy=True, verbose=verbose, label="TRAIN-EVAL")

        if self.save:
            self.save_checkpoint(env)
        return self

    @staticmethod
    def _format_metrics(env, info_history: list[dict]) -> str:
        parts = []
        for comp in env.components:
            m = comp.get_episode_metrics(info_history)
            if m:
                name = type(comp).__name__.replace("Component", "")
                parts.append(f"[{name}] " + " | ".join(f"{k}={v:.2f}" for k, v in m.items()))
        return " || ".join(parts)

    # ------------------- Evaluation -------------------
    def _run_episode(self, env, policy, save_prefix: str | None, start_date=None) -> dict:
        """Run exactly one episode under `policy` and collect logs/metrics."""
        obs = env.reset(start_date=start_date)
        state = env.normalize_state(obs)
        done = False
        ep_reward = 0.0
        info_history = []

        while not done:
            action = policy(state)
            obs_next, reward, done, info = env.step(action)
            info_history.append(info)
            ep_reward += float(reward)
            state = np.zeros_like(state) if (done or obs_next is None) else env.normalize_state(obs_next)

        comp_metrics = {type(c).__name__: c.get_episode_metrics(info_history) for c in env.components}
        results = [{"reward": ep_reward, "components": comp_metrics}]

        summary = {"episodes": 1, "avg_episode_reward": ep_reward}
        for name, m in comp_metrics.items():
            for k, v in m.items():
                summary[f"{name}_{k}"] = float(v)

        csv_path = None
        if save_prefix is not None:
            try:
                Path(save_prefix).parent.mkdir(parents=True, exist_ok=True)
                logs = env.get_episode_logs()
                df_sessions = next(iter(logs.values())) if logs else None
                if df_sessions is not None:
                    csv_path = f"{save_prefix}_per_ev.csv"
                    df_sessions.to_csv(csv_path, index=False)
            except Exception as e:
                print(f"[WARN] Could not write per-EV CSV: {e}")

        return {
            "summary": summary,
            "per_episode": results,
            "csv": csv_path,
            "step_log": {
                type(c).__name__: c.get_step_log() for c in env.components if hasattr(c, "get_step_log")
            },
        }

    def evaluate(self, env, greedy: bool = True, verbose: bool = True,
                 save_prefix: str | None = None, start_date=None, label: str = "EVAL") -> dict:
        """Single-episode evaluation of the learned policy."""
        # keep exploration state intact so a mid-training eval doesn't shift the schedule
        eps_backup, steps_backup = self.epsilon, self.steps_done
        policy = self.select_action_greedy if greedy else self.select_action
        try:
            out = self._run_episode(env, policy, save_prefix, start_date)
        finally:
            self.epsilon, self.steps_done = eps_backup, steps_backup

        if verbose:
            self._print_result(label, env, out)
        return out

    def evaluate_always_charge(self, env, verbose: bool = True,
                               save_prefix: str | None = None, start_date=None) -> dict:
        """Rule-based baseline: charge whenever an EV is plugged in."""
        ones = np.ones(env.action_size, dtype=int)
        out = self._run_episode(env, lambda _state: ones, save_prefix, start_date)
        if verbose:
            self._print_result("RB", env, out)
        return out

    @staticmethod
    def _print_result(label: str, env, out: dict) -> None:
        row = out["per_episode"][0]
        parts = [f"R={row['reward']:.3f}"]
        for name, m in row["components"].items():
            if m:
                parts.append(f"[{name.replace('Component', '')}] " + " | ".join(f"{k}={v:.2f}" for k, v in m.items()))
        print(f"{label} | " + " || ".join(parts))
