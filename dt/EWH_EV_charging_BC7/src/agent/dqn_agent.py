from dataclasses import dataclass
import random
import numpy as np
import torch
import torch.nn.functional as F
import torch.optim as optim

# Project-specific imports (kept as in your original project)
from src.agent.helpers.NN.NN import QNetwork
from src.agent.helpers.replay_buffer.replay_buffer import ReplayBuffer


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
    def __init__(self, state_dim: int, action_dim: int, cfg: DQNConfig = DQNConfig(), save = True):
        """
        Args:
            state_dim: dimension of the flattened observation vector
            action_dim: number of stations (each station has 2 discrete actions: {0,1})
        """
        self.cfg = cfg
        self.device = torch.device(cfg.device)

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
            a = q_vals.argmax(dim=-1).detach().cpu().numpy().astype(np.int64)  # (S,)
            return a

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

        B = self.cfg.batch_size

        # Q(s, a): sum over stations of Q_s(s, a_s)
        q_s_flat = self.q(s)  # (B, S*2)
        q_s = q_s_flat.view(B, self.action_dim, 2)  # (B, S, 2)
        q_sa = q_s.gather(dim=2, index=a.unsqueeze(-1)).squeeze(-1)  # (B, S)
        q_sa_sum = q_sa.sum(dim=1)  # (B,)

        with torch.no_grad():
            q_ns_flat = self.q_target(ns)  # (B, S*2)
            q_ns = q_ns_flat.view(B, self.action_dim, 2)  # (B, S, 2)
            q_ns_max = q_ns.max(dim=2).values  # (B, S)
            q_ns_sum = q_ns_max.sum(dim=1)  # (B,)
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

    # ------------------- Training loop -------------------
    def train(self, env, episodes: int = 1000, verbose: bool = True):
        for ep in range(episodes):
            obs = env.reset()  # np.ndarray state vector
            state = env.normalize_state(obs)  # expects np.ndarray back
            done = False
            total_reward = 0.0
            total_cost = 0.0
            total_penalty = 0.0
            total_delivered = 0.0
            steps_in_ep = 0

            init_req_kwh = float(env.sessions_df["req_kwh"].sum())
            n_cars = int(len(env.sessions_df)) if hasattr(env, "sessions_df") else 0

            while not done:
                action = self.select_action(state)  # (S,) ints in {0,1}
                next_obs, reward, done, info = env.step(action)

                # Accumulate environment cost & penalty from info dict
                step_cost = float(np.sum(info.get("charging_cost", 0.0)))
                step_penalty = float(np.sum(info.get("penalties", 0.0)))
                step_delivered = float(np.sum(info.get("energy_delivered", 0.0)))

                total_cost += step_cost
                total_penalty += step_penalty
                total_delivered += step_delivered

                if done or next_obs is None:
                    next_state = np.zeros_like(state)  # terminal placeholder
                else:
                    next_state = env.normalize_state(next_obs)  # normalized
                # state is normalized, next state too
                self.replay.push(state, action, float(reward), next_state, float(done))
                self.optimize()

                state = next_state
                total_reward += float(reward)
                steps_in_ep += 1

            # if verbose:  # and ((ep + 1) % 1 == 0):
            #     print(
            #         f"Ep {ep + 1:5d} "
            #         f"steps={steps_in_ep:4d} "
            #         f"R={total_reward:9.3f} "
            #         f"cost={total_cost:.2f}€ "
            #         f"pen={total_penalty:.2f}€ "
            #         f"deliv={total_delivered:.2f}kWh "
            #         f"ε={self.epsilon:6.3f}"
            #     )

            unmet_kwh = max(0.0, init_req_kwh - total_delivered)
            pct_unmet = (100.0 * unmet_kwh / init_req_kwh) if init_req_kwh > 0 else 0.0

            avg_cost_per_car = (total_cost / n_cars) if n_cars > 0 else 0.0
            avg_unmet_per_car = (unmet_kwh / n_cars) if n_cars > 0 else 0.0

            pct_met = (100.0 * total_delivered / init_req_kwh) if init_req_kwh > 0 else 0.0

            print(
                f"EPR Ep {ep + 1:4d} | "
                f"cost={total_cost:.2f}€ (avg/car={avg_cost_per_car:.2f}€) | "
                f"unmet={unmet_kwh:.2f}kWh (avg/car={avg_unmet_per_car:.2f}kWh, {pct_unmet:.2f}% ) | "
                f"delivered={total_delivered:.2f}kWh ({pct_met:.2f}% met) | "
                f"R={total_reward:.3f} | ε={self.epsilon:.3f}"
            )


            if ep > 0 and ep % 10 == 0:
                self.evaluate(env, greedy=True, verbose=True)

        if self.save:
            from pathlib import Path
            Path("./saved_models").mkdir(parents=True, exist_ok=True)
            torch.save(self.q.state_dict(), "./saved_models/q_state_dict.pth")
            torch.save(self.q_target.state_dict(), "./saved_models/q_target_state_dict.pth")
            print("Models saved (state_dict)")
        return self

    # ------------------- Evaluation helpers -------------------
    def _extract_station_slices(self, env, state_vec: np.ndarray):
        """
        Return per-station (present, remaining_kwh, time_to_dep) slices
        from a *raw* env state vector.

        State layout:
        [ next_price, price_window values, time_sin, time_cos,
          (present, remaining, time_to_departure) × n_stations ]
        """
        base = 1 + getattr(env, "price_window", 0) + 2
        S = getattr(env, "n_stations", self.action_dim)

        vals = []
        for st in range(S):
            i = base + st * 4
            vals.append(tuple(state_vec[i:i + 3]))

        return vals

    def evaluate(
            self,
            env,
            greedy: bool = True,
            verbose: bool = True,
            print_each_episode: bool = False,
            save_prefix: str | None = None,
    ):

        """
        Quick test evaluation. Runs a very small number of episodes (default 1, hard-capped).
        Metrics align with env's info dict: uses 'energy_cost' and 'delivered_kwh_per_station'.
        """
        # Cap episodes to keep eval quick

        # Backup exploration state so eval doesn't affect training
        eps_backup = getattr(self, "epsilon", None)
        steps_backup = getattr(self, "steps_done", None)

        results = []
        total_reward = 0.0
        try:
            obs = env.reset()
            init_req_kwh = float(env.sessions_df["req_kwh"].sum())

            state = env.normalize_state(obs)
            done = False
            ep_cost = 0.0
            ep_delivered = 0.0
            ep_reward = 0.0
            ep_penalty = 0.0

            # time accounting
            dt_h = getattr(env, 'slot_hours', None)
            if dt_h is None:
                dt_h = env.interval_minutes / 60.0
            ep_time = 0.0
            ep_charge_time = 0.0

            while not done:
                action = self.select_action_greedy(state) if greedy else self.select_action(state)
                obs_next, reward, done, info = env.step(action)

                step_cost = float(np.sum(info.get("charging_cost", 0.0)))
                step_delivered = float(np.sum(info.get("energy_delivered", 0.0)))
                step_penalty = float(np.sum(info.get("penalties", 0.0)))

                ep_cost += step_cost
                ep_delivered += step_delivered
                ep_reward += float(reward)
                ep_penalty += step_penalty

                ep_time += dt_h

                if np.any(action == 1):
                    ep_charge_time += dt_h

                state = np.zeros_like(state) if (done or obs_next is None) else env.normalize_state(obs_next)

            util = (ep_charge_time / ep_time) if ep_time > 0 else 0.0
            price_paid = (ep_cost / ep_delivered) if ep_delivered > 0 else 0.0
            row = {
                "energy_required_kwh": init_req_kwh,
                "delivered_kwh": ep_delivered,
                "unmet_kwh": max(0.0, init_req_kwh - ep_delivered),
                "energy_cost_eur": ep_cost,
                "penalty_eur": ep_penalty,
                "avg_price_paid_eur_per_kwh": price_paid,
                "hours_total": ep_time,
                "hours_charging": ep_charge_time,
                "charge_utilization": util,
                "total_reward": ep_reward,
            }
            results.append(row)
            total_reward += ep_reward

            n_cars = int(len(env.sessions_df)) if hasattr(env, "sessions_df") else 0
            unmet_kwh = row["unmet_kwh"]
            pct_met = (100.0 * row["delivered_kwh"] / row["energy_required_kwh"]) \
                if row["energy_required_kwh"] > 0 else 0.0
            pct_unmet = (100.0 * unmet_kwh / row["energy_required_kwh"]) if row["energy_required_kwh"] > 0 else 0.0
            avg_cost_per_car = (row["energy_cost_eur"] / n_cars) if n_cars > 0 else 0.0
            avg_unmet_per_car = (unmet_kwh / n_cars) if n_cars > 0 else 0.0

            if verbose and print_each_episode:
                print(
                    f"EPR-TEST | cost={row['energy_cost_eur']:.2f}€ (avg/car={avg_cost_per_car:.2f}€) | "
                    f"unmet={unmet_kwh:.2f}kWh (avg/car={avg_unmet_per_car:.2f}kWh, {pct_unmet:.2f}%) | "
                    f"delivered={row['delivered_kwh']:.2f}kWh ({pct_met:.2f}% met) | "
                    f"R={ep_reward:.3f}"
                )

            # if verbose and print_each_episode:
            #     print(
            #         f"req={init_req_kwh:.2f}kWh deliv={ep_delivered:.2f}kWh "
            #         f"cost={ep_cost:.2f}€ pen={ep_penalty:.2f}€ avg_price={price_paid:.3f}€/kWh "
            #         f"util={util:.2f} R={ep_reward:.2f}"
            #     )

        finally:
            # restore exploration state
            if eps_backup is not None:
                self.epsilon = eps_backup
            if steps_backup is not None:
                self.steps_done = steps_backup

        if not results:
            return {"summary": {}, "per_episode": []}

        # Aggregate concise summary (single print)
        def _avg(key):
            vals = [r[key] for r in results]
            return float(np.mean(vals)) if vals else 0.0

        delivered_sum = float(np.sum([r["delivered_kwh"] for r in results]))
        cost_sum = float(np.sum([r["energy_cost_eur"] for r in results]))
        penalty_sum = float(np.sum([r["penalty_eur"] for r in results]))
        overall_price = (cost_sum / delivered_sum) if delivered_sum > 0 else 0.0

        summary = {
            "episodes": len(results),
            "avg_unmet_kwh": _avg("unmet_kwh"),
            "avg_delivered_kwh": _avg("delivered_kwh"),
            "avg_energy_required_kwh": _avg("energy_required_kwh"),
            "avg_energy_cost_eur": _avg("energy_cost_eur"),
            "avg_penalty_eur": _avg("penalty_eur"),
            "avg_price_paid_eur_per_kwh": _avg("avg_price_paid_eur_per_kwh"),
            "overall_price_paid_eur_per_kwh": overall_price,
            "avg_charge_utilization": _avg("charge_utilization"),
            "avg_episode_reward": (total_reward / len(results)),
        }

        # if verbose:
        #     print(
        #         f"[TEST] episodes={len(results)} "
        #         f"avg_deliv={summary['avg_delivered_kwh']:.2f}kWh "
        #         f"avg_cost={summary['avg_energy_cost_eur']:.2f}€ "
        #         f"avg_pen={summary['avg_penalty_eur']:.2f}€ "
        #         f"avg_price={summary['avg_price_paid_eur_per_kwh']:.3f}€/kWh "
        #         f"R_avg={summary['avg_episode_reward']:.2f}"
        #     )
        print(
            f"EPR-TEST | cost={row['energy_cost_eur']:.2f}€ (avg/car={avg_cost_per_car:.2f}€) | "
            f"unmet={unmet_kwh:.2f}kWh (avg/car={avg_unmet_per_car:.2f}kWh, {pct_unmet:.2f}%) | "
            f"delivered={row['delivered_kwh']:.2f}kWh ({pct_met:.2f}% met) | "
            f"R={ep_reward:.3f}"
        )

        # Save per-EV session CSV if supported by env
        csv_path = None
        if save_prefix is not None and hasattr(env, "get_session_log_df"):
            try:
                from pathlib import Path
                import pandas as pd
                Path(save_prefix).parent.mkdir(parents=True, exist_ok=True)
                df_sessions = env.get_session_log_df()
                csv_path = f"{save_prefix}_per_ev.csv"
                df_sessions.to_csv(csv_path, index=False)
            except Exception as e:
                print(f"[WARN] Could not write per-EV CSV: {e}")

        return {"summary": summary, "per_episode": results, "csv": csv_path}


    def evaluate_always_charge(
            self,
            env,
            verbose: bool = True,
            print_each_episode: bool = False,
            save_prefix: str | None = None,
    ):

        """
        Rule-based test: always charge while the episode runs.
        Runs a very small number of episodes (default 1, hard-capped).
        """

        results = []
        obs = env.reset()
        init_req_kwh = float(env.sessions_df["req_kwh"].sum())

        done = False
        ep_cost = 0.0
        ep_delivered = 0.0
        ep_reward = 0.0
        ep_penalty = 0.0

        dt_h = getattr(env, 'slot_hours', None)
        if dt_h is None:
            dt_h = env.interval_minutes / 60.0
        ep_time = 0.0
        ep_charge_time = 0.0

        while not done:
            action = np.ones(env.action_size, dtype=int)
            obs_next, reward, done, info = env.step(action)

            step_cost = float(np.sum(info.get("charging_cost", 0.0)))
            step_delivered = float(np.sum(info.get("energy_delivered", 0.0)))
            step_penalty = float(np.sum(info.get("penalties", 0.0)))

            ep_cost += step_cost
            ep_delivered += step_delivered
            ep_reward += float(reward)
            ep_penalty += step_penalty

            ep_time += dt_h

            if np.any(action == 1):
                ep_charge_time += dt_h

            ep_charge_time += dt_h

        price_paid = (ep_cost / ep_delivered) if ep_delivered > 0 else 0.0
        util = (ep_charge_time / ep_time) if ep_time > 0 else 0.0
        row = {
            "energy_required_kwh": init_req_kwh,
            "delivered_kwh": ep_delivered,
            "unmet_kwh": max(0.0, init_req_kwh - ep_delivered),
            "energy_cost_eur": ep_cost,
            "penalty_eur": ep_penalty,
            "avg_price_paid_eur_per_kwh": price_paid,
            "hours_total": ep_time,
            "hours_charging": ep_charge_time,
            "charge_utilization": util,
            "total_reward": ep_reward,
        }
        results.append(row)

        n_cars = int(len(env.sessions_df)) if hasattr(env, "sessions_df") else 0

        unmet_kwh = row["unmet_kwh"]
        pct_unmet = (100.0 * unmet_kwh / row["energy_required_kwh"]) \
            if row["energy_required_kwh"] > 0 else 0.0

        pct_met = (100.0 * row["delivered_kwh"] / row["energy_required_kwh"]) \
            if row["energy_required_kwh"] > 0 else 0.0

        avg_cost_per_car = (row["energy_cost_eur"] / n_cars) if n_cars > 0 else 0.0
        avg_unmet_per_car = (unmet_kwh / n_cars) if n_cars > 0 else 0.0

        if verbose and print_each_episode:
            print(
                f"EPR-RB | "
                f"cost={row['energy_cost_eur']:.2f}€ (avg/car={avg_cost_per_car:.2f}€) | "
                f"unmet={unmet_kwh:.2f}kWh (avg/car={avg_unmet_per_car:.2f}kWh, {pct_unmet:.2f}% ) | "
                f"delivered={row['delivered_kwh']:.2f}kWh ({pct_met:.2f}% met) | "
                f"R={ep_reward:.3f}"
            )

        # if verbose and print_each_episode:
        #     print(
        #         f"req={init_req_kwh:.2f}kWh deliv={ep_delivered:.2f}kWh "
        #         f"cost={ep_cost:.2f}€ pen={ep_penalty:.2f}€ avg_price={price_paid:.3f}€/kWh "
        #         f"util={util:.2f} R={ep_reward:.2f}"
        #     )



        if not results:
            return {"summary": {}, "per_episode": []}

        def _avg(key):
            vals = [r[key] for r in results]
            return float(np.mean(vals)) if vals else 0.0

        delivered_sum = float(np.sum([r["delivered_kwh"] for r in results]))
        cost_sum = float(np.sum([r["energy_cost_eur"] for r in results]))
        penalty_sum = float(np.sum([r["penalty_eur"] for r in results]))
        overall_price = (cost_sum / delivered_sum) if delivered_sum > 0 else 0.0

        summary = {
            "episodes": len(results),
            "avg_unmet_kwh": _avg("unmet_kwh"),
            "avg_delivered_kwh": _avg("delivered_kwh"),
            "avg_energy_required_kwh": _avg("energy_required_kwh"),
            "avg_energy_cost_eur": _avg("energy_cost_eur"),
            "avg_penalty_eur": _avg("penalty_eur"),
            "avg_price_paid_eur_per_kwh": _avg("avg_price_paid_eur_per_kwh"),
            "overall_price_paid_eur_per_kwh": overall_price,
            "avg_charge_utilization": _avg("charge_utilization"),
            "avg_episode_reward": _avg("total_reward"),
        }

        # if verbose:
        #     print(
        #         f"[RB]   episodes={len(results)} "
        #         f"avg_deliv={summary['avg_delivered_kwh']:.2f}kWh "
        #         f"avg_cost={summary['avg_energy_cost_eur']:.2f}€ "
        #         f"avg_pen={summary['avg_penalty_eur']:.2f}€ "
        #         f"avg_price={summary['avg_price_paid_eur_per_kwh']:.3f}€/kWh "
        #         f"R_avg={summary['avg_episode_reward']:.2f}"
        #     )

        print(
            f"EPR-RB | "
            f"cost={row['energy_cost_eur']:.2f}€ (avg/car={avg_cost_per_car:.2f}€) | "
            f"unmet={unmet_kwh:.2f}kWh (avg/car={avg_unmet_per_car:.2f}kWh, {pct_unmet:.2f}% ) | "
            f"delivered={row['delivered_kwh']:.2f}kWh ({pct_met:.2f}% met) | "
            f"R={ep_reward:.3f}"
        )

        # Save per-EV session CSV if supported by env
        csv_path = None
        if save_prefix is not None and hasattr(env, "get_session_log_df"):
            try:
                from pathlib import Path
                import pandas as pd
                Path(save_prefix).parent.mkdir(parents=True, exist_ok=True)
                df_sessions = env.get_session_log_df()
                csv_path = f"{save_prefix}_per_ev.csv"
                df_sessions.to_csv(csv_path, index=False)
            except Exception as e:
                print(f"[WARN] Could not write per-EV CSV: {e}")

        return {"summary": summary, "per_episode": results, "csv": csv_path}

