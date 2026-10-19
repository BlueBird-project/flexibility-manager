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
    # 1/(1-gamma) is the effective planning horizon in steps -- 0.97 gives
    # ~33 steps (~8.3h), but sessions now run 10-24h (40-96 steps), so the
    # value function couldn't "see" consequences (departure penalty, missed
    # cheap price slots) late in a long session from decisions made early
    # in it. 0.995 gives ~200 steps (~50h), covering even the longest
    # (30h) sessions with margin.
    gamma: float = 0.995
    lr: float = 0.0001
    batch_size: int = 64
    # One training episode is ~42,000 steps on the current dataset; the old
    # 10,000-capacity buffer held less than a quarter of a single episode,
    # so training only ever saw a narrow, recent slice of experience.
    buffer_capacity: int = 100_000
    epsilon_start: float = 1.0
    epsilon_end: float = 0.0
    epsilon_decay_steps: int = 350_000  # linear decay steps (counted in env-steps, unaffected by steps_per_update)
    # optimize() used to run every single env-step (~42k gradient steps per
    # episode from a 100k-capacity buffer -- each transition revisited
    # 1000+ times over a full run, heavy oversampling of correlated data,
    # and the dominant CPU cost since there's no GPU here). Now it only
    # runs once every steps_per_update env-steps -- a real ~48x fewer
    # gradient computations, and closer to how DQN is normally run (Atari
    # DQN updates every 4 env-steps, not every 1).
    steps_per_update: int = 48  # ~12h of simulated time between gradient updates
    # target_update_freq/behavior_tau below are both counted in GRADIENT
    # STEPS, which only equalled env-steps while updates ran every step.
    # Both are rescaled here to preserve the same real (env-step) cadence
    # they had before steps_per_update was introduced, not left as raw
    # numbers that would now silently mean something ~48x staler/slower.
    target_update_freq: int = 10  # was 500 grad-steps == 500 env-steps; 500/48 ~= 10 grad-steps now == ~500 env-steps still
    hidden_sizes: tuple = (128, 128)
    seed: int | None = 1
    device: str = "cuda" if torch.cuda.is_available() else "cpu"
    # We've seen strong episode-to-episode oscillation in training (unmet%
    # swinging between ~0.5% and ~13% and even regressing over 10+ episodes
    # of pure-greedy on-policy training) -- consistent with occasional large
    # gradient spikes destabilizing the network, more likely now that gamma
    # is higher (larger-magnitude Q targets -> larger TD-error gradients).
    # Clipping the gradient norm caps how much any single update can move
    # the network, without changing what it's being pushed toward.
    max_grad_norm: float | None = 10.0  # set None to disable gradient clipping
    # Raw per-episode reward reaches the tens of thousands of EUR (summed
    # over ~40k steps); DQN is well known to destabilize with large,
    # unnormalized reward magnitudes (this is why Atari DQN famously clips
    # rewards to [-1,+1]). This scales the reward used for the Q-learning
    # target/replay buffer only -- printed/logged cost, unmet_kwh etc. stay
    # in real EUR, only the learning signal is rescaled.
    reward_scale: float = 0.01

    # select_action()/select_action_greedy() used to read straight from `q`,
    # the network being gradient-updated every single environment step --
    # so the ACTING policy was a moving target even within one episode: two
    # near-identical states a few steps apart could get opposite decisions
    # purely from weight drift, not from any real change in the situation.
    # Confirmed empirically: back-to-back EPR-TEST checkpoints swung from
    # 9.90% unmet to 0.36% unmet a few episodes apart with epsilon already
    # at 0. q_behavior is a Polyak/EMA-smoothed copy of q that acting reads
    # from instead -- nudged a small fraction toward q after every gradient
    # step, so it keeps adapting but never jumps, unlike q_target's abrupt
    # every-500-steps hard copy (which would trade this noise for a
    # different problem: frozen for 500 steps, then one big behavioral
    # jump). Training itself (q vs q_target, the TD target, Double DQN) is
    # untouched -- this only changes which weights choose the action taken.
    # Rescaled for steps_per_update=48: was 0.01 per call when a call
    # happened every env-step; solving (1-0.01)^1 = (1-tau)^(1/48) for tau
    # keeps the same cumulative EMA decay rate in real env-step time now
    # that a call only happens every 48th env-step.
    behavior_tau: float = 0.38

    # Optional Weights & Biases logging (reward + loss curves). Off by
    # default -- opt in via --wandb on the CLI scripts, or cfg.use_wandb.
    # Requires WANDB_API_KEY set in the environment (never hardcode a key
    # here or anywhere in this repo): PowerShell -> $env:WANDB_API_KEY="..."
    use_wandb: bool = False
    wandb_project: str = "ev-charging-dqn"
    wandb_run_name: str | None = None
    wandb_log_every: int = 200  # throttle loss logging (grad steps); ~42k/episode is too much at every step


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

        # EMA-smoothed copy of q that acting reads from (see DQNConfig.behavior_tau)
        self.q_behavior = QNetwork(state_dim, self.action_dim * 2, hidden_sizes=cfg.hidden_sizes).to(self.device)
        self.q_behavior.load_state_dict(self.q.state_dict())
        self.q_behavior.eval()

        # Optimizer and buffer
        self.optim = optim.Adam(self.q.parameters(), lr=cfg.lr)
        self.replay = ReplayBuffer(capacity=cfg.buffer_capacity, seed=cfg.seed)

        # Exploration schedule
        self.steps_done = 0  # counts action selections
        self.epsilon = cfg.epsilon_start

        # Track gradient steps for target updates (fixes mismatch)
        self.grad_steps = 0

        self.save = save

        # Optional W&B logging -- imported lazily so it's not a hard
        # dependency when unused, and never touches disk/network unless
        # explicitly enabled.
        self._wandb = None
        if cfg.use_wandb:
            import wandb
            wandb.init(project=cfg.wandb_project, name=cfg.wandb_run_name, config=vars(cfg))
            self._wandb = wandb

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
            q_vals = self.q_behavior(s).view(self.action_dim, 2)  # (S,2)
            a = q_vals.argmax(dim=-1).detach().cpu().numpy().astype(np.int64)  # (S,)
            return a

    def select_action_greedy(self, state_vec: np.ndarray) -> np.ndarray:
        """Greedy w.r.t. the EMA-smoothed behavior network (no exploration)."""
        with torch.no_grad():
            s = torch.tensor(state_vec, dtype=torch.float32, device=self.device).unsqueeze(0)
            q_vals = self.q_behavior(s).view(self.action_dim, 2)
            action = q_vals.argmax(dim=-1).detach().cpu().numpy().astype(np.int64)
        return action

    # ------------------- Learning -------------------
    def optimize(self) -> None:
        if len(self.replay) < self.cfg.batch_size:
            return

        s_b, a_b, r_b, ns_b, d_b = self.replay.sample(self.cfg.batch_size)
        s = torch.tensor(s_b, dtype=torch.float32, device=self.device)  # (B, D)
        ns = torch.tensor(ns_b, dtype=torch.float32, device=self.device)  # (B, D)
        a = torch.tensor(a_b, dtype=torch.int64, device=self.device)  # (B, S) entries in {0,1}
        r = torch.tensor(r_b, dtype=torch.float32, device=self.device)  # (B, S) per-station reward
        d = torch.tensor(d_b, dtype=torch.float32, device=self.device)  # (B,)

        B = self.cfg.batch_size

        # Q(s, a) per station -- NOT summed. Each station's cost/penalty is
        # independent of every other station's action, so each head must be
        # trained on its OWN TD-error; summing before the loss (as before)
        # made every station's gradient a blend of all stations' errors
        # that step, which is pure noise for whichever station actually
        # predicted correctly that step.
        q_s_flat = self.q(s)  # (B, S*2)
        q_s = q_s_flat.view(B, self.action_dim, 2)  # (B, S, 2)
        q_sa = q_s.gather(dim=2, index=a.unsqueeze(-1)).squeeze(-1)  # (B, S)

        with torch.no_grad():
            # Double DQN: SELECT the next action with the main network, but
            # EVALUATE its value with the target network. Vanilla DQN does
            # both with the target network's own max, which is a biased
            # estimator (max of noisy estimates overestimates the true
            # max); since q_target is a periodic hard-copy of q, that bias
            # compounds every copy instead of self-correcting -- confirmed
            # directly: loss went from 0.0003 to 6.5e22 over one run.
            # Using two different networks for "which action" vs. "how
            # good is it" breaks that feedback loop.
            q_ns_main = self.q(ns).view(B, self.action_dim, 2)  # main network picks the action
            a_ns_selected = q_ns_main.argmax(dim=2)  # (B, S)

            q_ns_target = self.q_target(ns).view(B, self.action_dim, 2)  # target network evaluates it
            q_ns_max = q_ns_target.gather(dim=2, index=a_ns_selected.unsqueeze(-1)).squeeze(-1)  # (B, S)
            target = r + self.cfg.gamma * q_ns_max * (1.0 - d.unsqueeze(-1))  # (B, S), elementwise per station

        loss = F.mse_loss(q_sa, target)
        self.optim.zero_grad()
        loss.backward()
        if self.cfg.max_grad_norm is not None:
            torch.nn.utils.clip_grad_norm_(self.q.parameters(), self.cfg.max_grad_norm)
        self.optim.step()

        # EMA-nudge the behavior network toward q every step (see
        # DQNConfig.behavior_tau) -- smooths the ACTING policy without
        # slowing down what q itself learns.
        with torch.no_grad():
            tau = self.cfg.behavior_tau
            for p_b, p_q in zip(self.q_behavior.parameters(), self.q.parameters()):
                p_b.data.mul_(1.0 - tau).add_(tau * p_q.data)

        # Hard target update every N gradient steps
        self.grad_steps += 1
        if self.grad_steps % self.cfg.target_update_freq == 0:
            self.q_target.load_state_dict(self.q.state_dict())

        if self._wandb is not None and self.grad_steps % self.cfg.wandb_log_every == 0:
            self._wandb.log({
                "train/loss": loss.item(),
                "train/grad_steps": self.grad_steps,
                "train/epsilon": self.epsilon,
            }, step=self.steps_done)

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
            total_curtailed = 0
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
                step_curtailed = int(np.sum(info.get("curtailed", 0)))

                total_cost += step_cost
                total_penalty += step_penalty
                total_delivered += step_delivered
                total_curtailed += step_curtailed

                if done or next_obs is None:
                    next_state = np.zeros_like(state)  # terminal placeholder
                else:
                    next_state = env.normalize_state(next_obs)  # normalized
                # state is normalized, next state too. The replay buffer
                # (and hence the Q-learning target) sees the SCALED reward;
                # total_reward below stays in real EUR for printing/logging.
                #
                # Per-station, not the joint scalar `reward`: each station's
                # cost/penalty is independent of every other station's
                # action (info["charging_cost"]/["penalties"] are already
                # broken out per station -- env.step() only collapses them
                # into one scalar for convenience). Pushing the joint scalar
                # here meant every station's Q-head learned from the SAME
                # blended TD-error each step, even when only one station's
                # prediction was actually wrong -- pure noise for the other
                # stations. optimize() now trains each station's head on
                # its own per-station TD-error instead.
                per_station_reward = -(
                        np.asarray(info["charging_cost"], dtype=np.float32)
                        + np.asarray(info["penalties"], dtype=np.float32)
                )
                self.replay.push(state, action, per_station_reward * self.cfg.reward_scale, next_state, float(done))
                # Every env-step still gets PUSHED to the replay buffer --
                # only the gradient update itself is throttled (see
                # DQNConfig.steps_per_update). self.steps_done increments
                # once per select_action() call above, i.e. once per
                # env-step, so this is a stride over env-steps.
                if self.steps_done % self.cfg.steps_per_update == 0:
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
                f"curtailed={total_curtailed} station-steps | "
                f"R={total_reward:.3f} | ε={self.epsilon:.3f}"
            )

            if self._wandb is not None:
                self._wandb.log({
                    "episode/index": ep + 1,
                    "episode/reward": total_reward,
                    "episode/cost_eur": total_cost,
                    "episode/penalty_eur": total_penalty,
                    "episode/unmet_kwh": unmet_kwh,
                    "episode/pct_met": pct_met,
                    "episode/curtailed": total_curtailed,
                    "episode/epsilon": self.epsilon,
                }, step=self.steps_done)

            if ep > 0 and ep % 10 == 0:
                # NOTE: this reuses the same `env` passed into train(), which
                # still holds the TRAINING split at this point (main.py only
                # swaps env to the held-out test split *after* train()
                # returns) -- tagged TRAIN-CKPT, not TEST, so the print
                # doesn't misleadingly claim held-out performance.
                self.evaluate(env, greedy=True, verbose=True, tag="TRAIN-CKPT")
                self.evaluate_always_charge(env, verbose=True, tag="TRAIN-CKPT-RB")

        # Final check on the train set, right as training wraps up -- same
        # pairing as the periodic mid-training checkpoint above, just once
        # more at the very end, so "how did this training run turn out on
        # its own data" is always visible without needing verbose=True or
        # a separate held-out-test call (main.py/sweep_penalties.py swap in
        # the test split and print their own EPR-TEST/EPR-RB pair AFTER
        # train() returns -- this one is train-set, always printed).
        self.evaluate(env, greedy=True, verbose=True, tag="TRAIN-CKPT")
        self.evaluate_always_charge(env, verbose=True, tag="TRAIN-CKPT-RB")

        if self.save:
            from pathlib import Path
            Path("./saved_models").mkdir(parents=True, exist_ok=True)
            torch.save(self.q.state_dict(), "./saved_models/q_state_dict.pth")
            torch.save(self.q_target.state_dict(), "./saved_models/q_target_state_dict.pth")
            print("Models saved (state_dict)")

        # wandb.finish() is NOT called here anymore -- it used to fire
        # immediately when train() returned, which meant its sync/summary
        # dump printed BEFORE the caller's own results line (e.g.
        # sweep_penalties.py's "[i/N] penalty_per_kwh=... savings=...").
        # Callers now finish it themselves via finish_wandb() after they've
        # printed whatever they need to, so wandb's noisy dump lands last.
        return self

    def finish_wandb(self) -> None:
        """Call once the caller is done printing/using this run's wandb
        handle -- see the note in train() for why this isn't automatic."""
        if self._wandb is not None:
            self._wandb.finish()

    # ------------------- Evaluation helpers -------------------
    def evaluate(
            self,
            env,
            greedy: bool = True,
            verbose: bool = True,
            print_each_episode: bool = False,
            save_prefix: str | None = None,
            tag: str = "TEST",
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
            ep_curtailed = 0

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
                step_curtailed = int(np.sum(info.get("curtailed", 0)))

                ep_cost += step_cost
                ep_delivered += step_delivered
                ep_reward += float(reward)
                ep_penalty += step_penalty
                ep_curtailed += step_curtailed

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
                "curtailed_station_steps": ep_curtailed,
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
        if verbose:
            print(
                f"EPR-{tag} | cost={row['energy_cost_eur']:.2f}€ (avg/car={avg_cost_per_car:.2f}€) | "
                f"unmet={unmet_kwh:.2f}kWh (avg/car={avg_unmet_per_car:.2f}kWh, {pct_unmet:.2f}%) | "
                f"delivered={row['delivered_kwh']:.2f}kWh ({pct_met:.2f}% met) | "
                f"curtailed={ep_curtailed} station-steps | "
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
            tag: str = "RB",
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
        ep_curtailed = 0

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
            step_curtailed = int(np.sum(info.get("curtailed", 0)))

            ep_cost += step_cost
            ep_delivered += step_delivered
            ep_reward += float(reward)
            ep_penalty += step_penalty
            ep_curtailed += step_curtailed

            ep_time += dt_h

            if np.any(action == 1):
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
            "curtailed_station_steps": ep_curtailed,
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

        if verbose:
            print(
                f"EPR-{tag} | "
                f"cost={row['energy_cost_eur']:.2f}€ (avg/car={avg_cost_per_car:.2f}€) | "
                f"unmet={unmet_kwh:.2f}kWh (avg/car={avg_unmet_per_car:.2f}kWh, {pct_unmet:.2f}% ) | "
                f"delivered={row['delivered_kwh']:.2f}kWh ({pct_met:.2f}% met) | "
                f"curtailed={ep_curtailed} station-steps | "
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

