import numpy as np
import matplotlib.pyplot as plt
import random
from dt.EWH_coldrooms.Classes.coldroom_class import MultiRoomParams, MultiRoomStateSpace

def run_simulation():
    # --- 1. Setup Parameters ---
    p = MultiRoomParams(
        m = np.array([1200, 1000, 1500, 1100, 800]), 
        cv = np.array([1000, 1000, 1000, 1000, 900]),
        UA = np.array([40, 35, 50, 45, 38]),      
        COP = 2.5,                            
        P1_max = 2000,                        
        P2_max = 500,    
        P3_max = 2000,  
        d      = 0.5,                  
        K_gain = 2500,
        dt = 60                               
    )

    model_builder = MultiRoomStateSpace(p)
    Ad, Bd, Ed = model_builder.build_state_space()

    steps = 480 
    n_rooms = len(p.m)
    T_rooms = np.zeros((n_rooms, steps))
    T_rooms[:, 0] = [3.0, 4.0, 2.0, 5., -15]      
    
    current_sets = np.array([3.0, 4.0, 2.0, 5.0, -15])
    T_set_log = np.zeros((n_rooms, steps))
    U_allocated_log = np.zeros((n_rooms, steps)) # Added to log power per room

    Q_door_peak = 4500     
    door_duration = 8      
    n_hours = (steps // 60) + 1
    door_schedule = [random.sample(range(60 - door_duration), n_hours) for _ in range(n_rooms)]

    P1_log, P2_log, P3_log = np.zeros(steps), np.zeros(steps), np.zeros(steps)
    Door_load_log = np.zeros((n_rooms, steps))
    delta2, delta3 = 0, 0

    # --- 2. Simulation Loop ---
    for k in range(steps - 1):
        # Setpoint Change Logic
        if k == 100: current_sets[0] += 1.0 # Only Room 1
        if k == 250: # Simultaneous change
            current_sets[1] -= 1.0
            current_sets[2] -= 2.0
        if k == 400: current_sets[-1] -= 4.0

        T_set_log[:, k] = current_sets

        # Door Logic
        current_hour, min_in_hour = k // 60, k % 60
        current_q_door = np.zeros(n_rooms)
        for i in range(n_rooms):
            if door_schedule[i][current_hour] <= min_in_hour < (door_schedule[i][current_hour] + door_duration):
                current_q_door[i] = Q_door_peak
        Door_load_log[:, k] = current_q_door

        # Control & Allocation
        u_req = np.maximum(0, p.K_gain * (T_rooms[:-1, k] - current_sets[:-1]))
        total_requested = np.sum(u_req)

        # Compressor Staging
        if total_requested > p.P1_max:
            delta2 = 1
        else:
            delta2 = 0
        
        p2_act = delta2 * p.P2_max
        p1_act = np.min([total_requested, p.P1_max])
        total_cap = p1_act + p2_act
        
        # Proportional Lowering (As this is not an MPC we just assume an ideal proportional allocation based on request)
        u_allocated = u_req * (total_cap / total_requested) if total_requested > total_cap else u_req
        U_allocated_log[:-1, k] = u_allocated

        # Freezer compressor logic 
        if T_rooms[-1, k] <= current_sets[-1] - p.d:
            delta3 = 0
        elif T_rooms[-1, k] >= current_sets[-1] + p.d:
            delta3 = 1
        else:
            delta3 = delta3

        p3_act = delta3 * p.P3_max

        u = np.concatenate([u_allocated, [p3_act]])
        U_allocated_log[-1, k] = p3_act

        # Physics Update
        T_ambient = 15.0
        dist_vec = np.zeros(n_rooms * 2)
        for i in range(n_rooms):
            dist_vec[2*i] = T_ambient
            dist_vec[2*i + 1] = current_q_door[i]

        T_rooms[:, k+1] = Ad @ T_rooms[:, k] + Bd @ u + Ed @ dist_vec
        P1_log[k], P2_log[k], P3_log[k] = p1_act, p2_act, p3_act

    # --- 3. Plotting ---

    # FIGURE 1: Temperatures and Shared Power
    fig1, axes1 = plt.subplots(n_rooms + 2, 1, figsize=(10, 12), sharex=True)
    for i in range(n_rooms):
        axes1[i].plot(T_rooms[i, :], color='tab:blue', label="Actual Temp")
        axes1[i].step(range(steps), T_set_log[i, :], color='tab:green', linestyle='--', label="Setpoint")
        axes1[i].fill_between(range(steps), axes1[i].get_ylim()[0], axes1[i].get_ylim()[1], 
                              where=Door_load_log[i,:] > 0, color='red', alpha=0.1, label="Door Open")
        axes1[i].set_ylabel("Temp (°C)")
        axes1[i].set_title(f"Cold room {i+1}")
        axes1[i].legend(loc='upper right', fontsize='x-small')

    #Fridge compressors
    axes1[-2].stackplot(range(steps), P1_log, P2_log, labels=['P1 (Primary)', 'P2 (Secondary)'], alpha=0.8)
    axes1[-2].set_title("Shared Fridge Compressor Plant Power")
    axes1[-2].set_ylabel("Power (W)")
    axes1[-2].legend(loc='upper right')

    #Freezer compressor
    axes1[-1].stackplot(range(steps), P3_log, labels=['P3 (Freezer)'], alpha=0.8)
    axes1[-1].set_title("Freezer Compressor Power")
    axes1[-1].set_ylabel("Power (W)")
    axes1[-1].legend(loc='upper right')

    fig1.tight_layout()

    # FIGURE 2: Cooling Allocation per Room
    fig2, axes2 = plt.subplots(n_rooms, 1, figsize=(10, 10), sharex=True)
    for i in range(n_rooms):
        axes2[i].plot(U_allocated_log[i, :], color='tab:purple', label=f"Cooling to Cold room {i+1}")
        # Overlay Door Load to see demand vs supply
        ax_twin = axes2[i].twinx()
        ax_twin.plot(Door_load_log[i, :], color='red', alpha=0.2, label="Door Load")
        ax_twin.set_ylabel("Heat Load (W)")
        axes2[i].set_ylabel("Allocated Power (W)")
        axes2[i].legend(loc='upper left', fontsize='x-small')
        axes2[i].grid(True, alpha=0.2)
        axes2[i].set_title(f"Energy Allocation: Cold room {i+1}")

    axes2[-1].set_xlabel("Time (minutes)")
    fig2.tight_layout()

    plt.show()

if __name__ == "__main__":
    run_simulation()