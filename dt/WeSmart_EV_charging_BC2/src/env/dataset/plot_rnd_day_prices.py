import pandas as pd
import matplotlib.pyplot as plt
import random

# ==========================
# User settings
# ==========================
csv_file = "epex_be_day_ahead_2025.csv"
timestamp_col = "timestamp"
price_col = "price"

# ==========================
# Load data
# ==========================
df = pd.read_csv(csv_file, sep=';')

df['price'] = df['price'] / 1000

df[timestamp_col] = pd.to_datetime(df[timestamp_col])
df = df.sort_values(timestamp_col)
df["date"] = df[timestamp_col].dt.date

# ==========================
# Find complete days (96 points)
# ==========================
complete_days = [
    day for day, group in df.groupby("date")
    if len(group) == 96
]

if len(complete_days) < 5:
    raise ValueError("Less than 5 complete days available.")

# ==========================
# Select 5 random days
# ==========================
random.seed(42)  # Optional
selected_days = random.sample(complete_days, 5)

print("Selected days:")
for day in selected_days:
    print(day)

# Common x-axis settings
tick_positions = range(0, 96, 16)  # Every 4 hours
tick_labels = [f"{h:02d}:00" for h in range(0, 24, 4)]

# ==================================================
# 1. Plot all 5 days on the SAME figure
# ==================================================
plt.figure(figsize=(12, 6))

for day in selected_days:
    day_data = (
        df[df["date"] == day]
        .sort_values(timestamp_col)
    )

    plt.plot(
        day_data[price_col].values,
        label=str(day),
        linewidth=2
    )

plt.xticks(tick_positions, tick_labels)
plt.xlabel("Hour")
plt.ylabel("Electricity Price")
plt.title("Electricity price profiles - Comparison")
plt.legend()
plt.grid(True)
plt.tight_layout()

plt.savefig("all_5_days_comparison.svg")
plt.show()

# ==================================================
# 2. Plot each day separately
# ==================================================
for i, day in enumerate(selected_days, start=1):

    day_data = (
        df[df["date"] == day]
        .sort_values(timestamp_col)
    )

    plt.figure(figsize=(10, 5))

    plt.plot(
        day_data[price_col].values,
        linewidth=2
    )

    plt.xticks(tick_positions, tick_labels)
    plt.xlabel("Hour")
    plt.ylabel("Electricity Price")
    plt.title(f"Electricity Price Profile - {day}")
    plt.grid(True)
    plt.tight_layout()

    plt.savefig(f"price_curve_{i}_{day}.svg")
    plt.show()
    plt.close()


