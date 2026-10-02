import numpy as np
import pandas as pd

# =========================
# CONFIGURABLE PARAMETERS
# =========================

# Time configuration
DAYS = 365*20
FREQ = "h"

# Tank properties
TANK_CAPACITY_M3 = 2500
INITIAL_LEVEL_M3 = 1700

# Pumps
NUM_PUMPS = 4
MAX_FLOW_PER_PUMP_M3H = TANK_CAPACITY_M3 / 24/4  # one pump fills tank in 1 day
POWER_KW_PER_PUMP_AT_100 = 15

PUMP_MIN_PERCENT = 0
PUMP_MAX_PERCENT = 100

# Flow deviation (linear with noise)
FLOW_DEVIATION_STD = 0.05  # 5% flow noise

# Outflow behaviour
MEAN_OUTFLOW_M3H = TANK_CAPACITY_M3 / 72  # empties in 3 days if no filling
DAILY_VARIATION_AMPLITUDE = 0.4
OUTFLOW_RANDOM_NOISE_STD = 5

# Seasonal variation (summer > winter)
SEASONAL_VARIATION_AMPLITUDE = 0.3

# Soft feedback control parameters
BASE_PUMP_MEAN_PERCENT = 65
LEVEL_FEEDBACK_STRENGTH = 45
PUMP_STD_PERCENT = 12

# Output
OUTPUT_FILE = "waterworks_synthetic_dataset.csv"

# =========================
# DATA GENERATION
# =========================

date_range = pd.date_range(
    start="2025-01-01 00:00:00",
    periods=DAYS * 24,
    freq=FREQ
)

data = []
tank_level = INITIAL_LEVEL_M3

for t in date_range:
    hour = t.hour
    month = t.month

    # --- Outflow model ---
    daily_factor = 1 + DAILY_VARIATION_AMPLITUDE * np.sin((hour - 6) / 24 * 2 * np.pi)
    seasonal_factor = 1 + SEASONAL_VARIATION_AMPLITUDE * np.sin(
        (month - 1) / 12 * 2 * np.pi - np.pi / 2
    )

    base_outflow = MEAN_OUTFLOW_M3H * daily_factor * seasonal_factor
    outflow = max(0, base_outflow + np.random.normal(0, OUTFLOW_RANDOM_NOISE_STD))

    # --- Soft negative feedback pump control ---
    fill_ratio = tank_level / TANK_CAPACITY_M3

    dynamic_mean = BASE_PUMP_MEAN_PERCENT - LEVEL_FEEDBACK_STRENGTH * fill_ratio

    pump_setpoints = np.clip(
        np.random.normal(dynamic_mean, PUMP_STD_PERCENT, NUM_PUMPS),
        PUMP_MIN_PERCENT,
        PUMP_MAX_PERCENT
    )

    pump_flows = []
    pump_powers = []

    for pct in pump_setpoints:
        ideal_flow = (pct / 100) * MAX_FLOW_PER_PUMP_M3H
        deviation = np.random.normal(0, ideal_flow * FLOW_DEVIATION_STD)
        actual_flow = max(0, ideal_flow + deviation)

        pump_flows.append(actual_flow)
        pump_powers.append((pct / 100) * POWER_KW_PER_PUMP_AT_100)

    total_inflow = np.sum(pump_flows)
    total_power = np.sum(pump_powers)

    # --- Tank dynamics ---
    tank_level += total_inflow - outflow
    tank_level = min(max(tank_level, 0), TANK_CAPACITY_M3)

    # --- Row assembly ---
    # --- Price generation (EUR per kWh) ---
    # Price range: 0.25 .. 0.45 EUR/kWh
    # Higher in late morning (~10-11) and evening (~18-21), otherwise lower,
    # with random variation.
    morning_peak = np.exp(-((hour - 10) ** 2) / (2 * 2.0 ** 2))
    evening_peak = np.exp(-((hour - 20) ** 2) / (2 * 2.0 ** 2))
    peak_weight = 0.75 * morning_peak + 1.0 * evening_peak

    baseline_price = 0.27
    peak_addition = 0.18 * peak_weight
    noise = np.random.normal(0, 0.02)

    price = baseline_price + peak_addition + noise
    price = float(np.clip(price, 0.25, 0.45))

    row = {
        "timestamp": t,
        "tank_level_m3": tank_level,
        "outflow_m3h": outflow,
        "total_inflow_m3h": total_inflow,
        "total_pump_power_kw": total_power,
        "price_eur_per_kwh": round(price, 3)
    }

    for i in range(NUM_PUMPS):
        row[f"pump_{i+1}_setpoint_percent"] = pump_setpoints[i]
        row[f"pump_{i+1}_flow_m3h"] = pump_flows[i]

    data.append(row)

# =========================
# EXPORT
# =========================

df = pd.DataFrame(data)
df.to_csv(OUTPUT_FILE, index=False)

print(f"Dataset generated: {OUTPUT_FILE}")
