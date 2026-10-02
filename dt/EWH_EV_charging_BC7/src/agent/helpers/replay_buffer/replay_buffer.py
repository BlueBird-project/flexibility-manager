import random
import numpy as np
from collections import deque
from typing import Deque, Tuple

class ReplayBuffer:
    def __init__(self, capacity: int = 100_000, seed: int | None = None):
        self.buffer: Deque[Tuple[np.ndarray, int, float, np.ndarray, float]] = deque(maxlen=capacity)
        self.rng = random.Random(seed)

    def push(self, s: np.ndarray, a: int, r: float, s_next: np.ndarray, done: float) -> None:
        self.buffer.append((s, a, r, s_next, done))

    def sample(self, batch_size: int):
        batch = self.rng.sample(self.buffer, batch_size)
        s, a, r, s_next, d = zip(*batch)
        return (
            np.stack(s).astype(np.float32),
            np.asarray(a, dtype=np.int64),
            np.asarray(r, dtype=np.float32),
            np.stack(s_next).astype(np.float32),
            np.asarray(d, dtype=np.float32),
        )

    def __len__(self) -> int:
        return len(self.buffer)