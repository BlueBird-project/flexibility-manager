import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# -----------------------
# SETTINGS
# -----------------------

FILE_PATH = "wesmart_ev_sessions.csv"
DEFAULT_POWER_KW = 11

# -----------------------
# LOAD DATA
# -----------------------

df = pd.read_csv(FILE_PATH)

df['arrival'] = pd.to_datetime(df['arrival'])
df['departure'] = pd.to_datetime(df['departure'])

# -----------------------
# FEATURE ENGINEERING
# -----------------------

# Time features
# Round arrival timestamp to nearest hour
df['arrival_hour'] = df['arrival'].dt.round('h').dt.hour

df['arrival_dayofweek'] = df['arrival'].dt.dayofweek

# Staying time (hours)
df['staying_time_h'] = (
    df['departure'] - df['arrival']
).dt.total_seconds() / 3600

# -----------------------
# POWER HANDLING
# -----------------------

if 'power_kw' not in df.columns:
    print(f"Using default power: {DEFAULT_POWER_KW} kW")
    df['power_kw'] = DEFAULT_POWER_KW
else:
    print("Using per-session power_kw")

# Clean data
df = df[
    (df['power_kw'] > 0) &
    (df['staying_time_h'] > 0)
]

# -----------------------
# FLEXIBILITY (KEY PART)
# -----------------------

df['charging_time_h'] = df['req_kwh'] / df['power_kw']

df['idle_time_h'] = (
    df['staying_time_h'] - df['charging_time_h']
)

df['is_flexible'] = df['idle_time_h'] > 0

# -----------------------
# SUMMARY
# -----------------------

print("\n===== GENERAL SUMMARY =====")

print(
    df[
        ['staying_time_h', 'charging_time_h', 'idle_time_h']
    ].describe()
)

print("\n===== FLEXIBILITY =====")

print(f"Average idle time: {df['idle_time_h'].mean():.2f} h")

print(
    f"Flexible sessions: "
    f"{df['is_flexible'].mean()*100:.2f}%"
)

print("\n===== PER STATION =====")

print(
    df.groupby('station_id')[
        [
            'charging_time_h',
            'staying_time_h',
            'idle_time_h'
        ]
    ].mean()
)

# -----------------------
# PLOTTING
# -----------------------

sns.set(style="whitegrid")

# -----------------------
# ARRIVAL DISTRIBUTION
# -----------------------

plt.figure()

sns.histplot(
    df['arrival_hour'],
    bins=np.arange(-0.5, 24.5, 1)
)

plt.xticks(range(24))

plt.title("Arrival Distribution (All Stations)")
plt.xlabel("Hour of Day")
plt.ylabel("Count")

plt.xlim(-0.5, 23.5)

plt.show()

plt.figure()

sns.histplot(
    data=df,
    x='arrival_hour',
    hue='station_id',
    bins=np.arange(-0.5, 24.5, 1),
    multiple='stack'
)

plt.xticks(range(24))

plt.title("Arrival Distribution per Station")
plt.xlabel("Hour of Day")
plt.ylabel("Count")

plt.xlim(-0.5, 23.5)

plt.show()

# -----------------------
# STAYING TIME
# -----------------------

plt.figure()

sns.histplot(
    df['staying_time_h'],
    bins=50
)

plt.title("Staying Time Distribution")
plt.xlabel("Hours")
plt.ylabel("Count")

plt.show()

plt.figure()

sns.boxplot(
    data=df,
    x='station_id',
    y='staying_time_h'
)

plt.title("Staying Time per Station")

plt.show()

# -----------------------
# FLEXIBILITY VISUALS
# -----------------------

# 1. Scatter (charging vs staying)

plt.figure()

plt.scatter(
    df['charging_time_h'],
    df['staying_time_h'],
    alpha=0.5
)

max_val = max(
    df['charging_time_h'].max(),
    df['staying_time_h'].max()
)

plt.plot(
    [0, max_val],
    [0, max_val],
    linestyle='--'
)

plt.xlabel("Charging Time Needed (h)")
plt.ylabel("Actual Staying Time (h)")
plt.title("Charging vs Staying Time")

plt.show()

# 2. Idle time distribution

plt.figure()

plt.hist(
    df['idle_time_h'],
    bins=50
)

plt.axvline(
    0,
    linestyle='--'
)

plt.xlabel("Idle Time (hours)")
plt.title("Flexibility (Idle Time Distribution)")

plt.show()

# 3. Session-level stacked bars (sample)

df_sample = (
    df.sort_values('staying_time_h')
    .head(50)
)

plt.figure(figsize=(12, 6))

plt.bar(
    range(len(df_sample)),
    df_sample['charging_time_h'],
    label='Charging time'
)

plt.bar(
    range(len(df_sample)),
    df_sample['idle_time_h'],
    bottom=df_sample['charging_time_h'],
    label='Idle time (flexibility)'
)

plt.title(
    "Charging vs Idle Time per Session (Sample)"
)

plt.xlabel("Session")
plt.ylabel("Hours")

plt.legend()

plt.show()


avg_idle = df['idle_time_h'].mean()
print(f"\nAverage idle time: {avg_idle:.2f} h\n")

# 4. Aggregated per station

summary = df.groupby('station_id')[
    ['charging_time_h', 'staying_time_h']
].mean()

summary['idle_time_h'] = (
    summary['staying_time_h']
    - summary['charging_time_h']
)

summary[
    ['charging_time_h', 'idle_time_h']
].plot(
    kind='bar',
    stacked=True
)

plt.title(
    "Average Charging vs Idle Time per Station"
)

plt.ylabel("Hours")

plt.show()

# -----------------------
# STAYING TIME CATEGORIES
# -----------------------

bins = [0, 4, 8, np.inf]
labels = ['<4h', '4-8h', '>8h']

df['staying_category'] = pd.cut(
    df['staying_time_h'],
    bins=bins,
    labels=labels
)

plt.figure()

df['staying_category'] \
    .value_counts() \
    .sort_index() \
    .plot(kind='bar')

plt.title(
    "Staying Time Categories (All Sessions)"
)

plt.xlabel("Staying Time Category")
plt.ylabel("Number of Sessions")

plt.show()

# -----------------------
# SAVE OUTPUT
# -----------------------

# df.to_csv("processed_ev_analysis.csv", index=False)

# average arrival, average arrival per station
# average staying time, average staying time per station
mean_arr = round(df['arrival_hour'].mean(), 2)
mean_arr_station = avg_arrival_station = (df.groupby('station_id')['arrival_hour'].mean().round(2))
mean_stay = df['staying_time_h'].mean()
mean_stay_station = avg_stay_station = (df.groupby('station_id')['staying_time_h'].mean().round(2))

print(f"\n-------------------------------------------\n"
      f"Mean Arrival: {mean_arr}\n"
      f"Mean Arrival Per Station: {mean_arr_station}"
      f"Mean Stay: {mean_stay}\n"
      f"Mean Stay Per Station: {mean_stay_station}")

print("\nSaved: processed_ev_analysis.csv")