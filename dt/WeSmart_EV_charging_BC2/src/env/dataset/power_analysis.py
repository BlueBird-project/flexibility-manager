import pandas as pd
import matplotlib.pyplot as plt

# ======================
# Prepare data
# ======================

# Uncomment if loading from CSV
df = pd.read_csv('ev_charger_EVSE02_Borne_2_with_connected .csv', sep=';')

df['Timestamp'] = pd.to_datetime(df['Timestamp'])
df = df.sort_values('Timestamp').reset_index(drop=True)

# ======================
# Detect charging sessions
# ======================

df['session_id'] = (
    (
        (df['Connected'] == 1) &
        (df['Connected'].shift(fill_value=0) == 0)
    )
).cumsum()

# Keep only connected periods
charging = df[df['Connected'] == 1].copy()

# ======================
# Analyze sessions
# ======================

session_results = []

for session_id, session in charging.groupby('session_id'):

    # Need at least 3 points to remove first and last
    if len(session) <= 2:
        continue

    # Remove first and last timestamp
    session = session.iloc[1:-1].copy()

    # Remove timestamps with no charging energy
    session = session[session['Charging_kWh'] > 0].copy()

    # Skip empty sessions
    if len(session) == 0:
        continue

    # Calculate interval duration (hours)
    session['dt_hours'] = (
        session['Timestamp']
        .diff()
        .dt.total_seconds() / 3600
    )

    # Fill first NaN with median timestep
    median_dt = session['dt_hours'].median()

    if pd.isna(median_dt) or median_dt == 0:
        continue

    session['dt_hours'] = session['dt_hours'].fillna(median_dt)

    # Calculate power (kW)
    session['Power_kW'] = (
        session['Charging_kWh'] /
        session['dt_hours']
    )

    # Representative session power
    avg_power = session['Power_kW'].mean()

    session_results.append({
        'session_id': session_id,
        'intervals_used': len(session),
        'avg_power_kW': avg_power,
        'rounded_power_kW': round(avg_power),
        'total_energy_kWh': session['Charging_kWh'].sum()
    })

# ======================
# Results dataframe
# ======================

results = pd.DataFrame(session_results)

# ======================
# Print summary
# ======================

print("\n===== SESSION SUMMARY =====")

print(f"Total sessions analyzed: {len(results)}")

print(f"\nMean charging power: {results['avg_power_kW'].mean():.2f} kW")
print(f"Median charging power: {results['avg_power_kW'].median():.2f} kW")
print(f"Minimum charging power: {results['avg_power_kW'].min():.2f} kW")
print(f"Maximum charging power: {results['avg_power_kW'].max():.2f} kW")

# Count sessions by representative power
power_counts = (
    results['rounded_power_kW']
    .value_counts()
    .sort_index()
)

print("\n===== SESSION COUNTS BY POWER =====")
print(power_counts)

print("\n===== TOP 10 MOST COMMON POWERS =====")
print(power_counts.sort_values(ascending=False).head(10))

# ======================
# Plot 1: Session counts by power
# ======================

plt.figure(figsize=(10, 6))

power_counts.plot(kind='bar')

plt.xlabel('Representative Session Power (kW)')
plt.ylabel('Number of Sessions')
plt.title('EV Charging Sessions by Representative Power')
plt.grid(axis='y', alpha=0.3)

plt.tight_layout()
plt.show()

# ======================
# Plot 2: Histogram of average powers
# ======================

plt.figure(figsize=(10, 6))

plt.hist(results['avg_power_kW'], bins=20)

plt.xlabel('Average Session Power (kW)')
plt.ylabel('Number of Sessions')
plt.title('Distribution of Average Session Powers')
plt.grid(alpha=0.3)

plt.tight_layout()
plt.show()

# ======================
# Optional: inspect first sessions
# ======================

print("\n===== FIRST 10 SESSIONS =====")
print(results.head(10))