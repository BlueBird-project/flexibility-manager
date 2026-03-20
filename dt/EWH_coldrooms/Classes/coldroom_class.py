import numpy as np
from dataclasses import dataclass
from scipy.linalg import expm

@dataclass
class MultiRoomParams:
    # Physical parameters (Arrays of length N)
    m: np.ndarray       # kg, thermal mass per room
    cv: np.ndarray      # J/kgK, specific heat per room
    UA: np.ndarray      # W/K, wall heat transfer per room
    COP: float          # Coefficient of performance (shared)

    # Compressor Parameters (Shared)
    P1_max: float       # Watts, Primary modulated max
    P2_max: float       # Watts, Secondary ON/OFF max
    min_run_steps: int  # L, Minimum time steps for secondary
    turn_on_threshold: float   # Start P2 if P1 > this
    turn_off_threshold: float  # Stop P2 if P1 < this
    
    # Control Parameters
    K_gain: float       # W/K, The low-level controller gain
    dt: float           # seconds, simulation timestep

class MultiRoomStateSpace:
    def __init__(self, p: MultiRoomParams):
        self.p = p
        self.n = len(p.m)  # Dynamically determine number of rooms

    def build_state_space(self):
        p = self.p
        n = self.n
        
        # Dimensions
        # states: T_i for each room
        # inputs: u_i (electrical power) for each room
        # disturbances: 2 for each room (T_ambient, Q_internal/door)
        n_states = n
        n_inputs = n
        n_dist = n * 2 

        A = np.zeros((n_states, n_states))
        B = np.zeros((n_states, n_inputs))
        E = np.zeros((n_states, n_dist))

        for i in range(n):
            C_i = p.m[i] * p.cv[i]
            
            # Diagonal A: Rate of temp change due to wall losses
            A[i, i] = -p.UA[i] / C_i
            
            # Diagonal B: Rate of temp change due to cooling power u_i
            # u is electrical power, B handles COP/C translation
            B[i, i] = -p.COP / C_i
            
            # E matrix: Mapping disturbances to each room
            # Even indices: Ambient temp impact
            # Odd indices: Internal heat gain impact
            E[i, 2*i] = p.UA[i] / C_i     # T_ambient term
            E[i, 2*i + 1] = 1 / C_i      # Q_internal term

        # Exact Discretization using Augmented Matrix
        total_dim = n_states + n_inputs + n_dist
        M = np.zeros((total_dim, total_dim))
        M[:n_states, :n_states] = A
        M[:n_states, n_states:n_states+n_inputs] = B
        M[:n_states, n_states+n_inputs:] = E

        Md = expm(M * p.dt)

        # Extract discrete-time matrices
        Ad = Md[:n_states, :n_states]
        Bd = Md[:n_states, n_states:n_states+n_inputs]
        Ed = Md[:n_states, n_states+n_inputs:]

        return Ad, Bd, Ed