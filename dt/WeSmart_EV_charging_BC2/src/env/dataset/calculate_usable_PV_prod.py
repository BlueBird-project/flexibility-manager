import pandas as pd

# 1. Load the datasets (using ';' as the separator)
# Replace 'production.csv' and 'consumption.csv' with your actual file paths
prod_df = pd.read_csv('solar_production.csv', sep=';')
cons_df = pd.read_csv('common_areas_consumption(in).csv', sep=';')

# Clean column names of any leading/trailing spaces
prod_df.columns = prod_df.columns.str.strip()
cons_df.columns = cons_df.columns.str.strip()

# 2. Parse the Timestamps using their respective formats
# Production format: DD/MM/YYYY HH:MM (e.g., 01/01/2025 00:00)
cons_df['Timestamp'] = pd.to_datetime(
    cons_df['Timestamp'].astype(str).str.strip(),
    format='%d/%m/%Y %H:%M',
    errors='coerce'
)

# Consumption format: YYYY-MM-DD HH:MM:SS (e.g., 2025-01-01 00:00:00)
prod_df['Timestamp'] = pd.to_datetime(
    prod_df['Timestamp'].astype(str).str.strip(),
    format='%Y-%m-%d %H:%M:%S',
    errors='coerce'
)

# Drop any rows with unparseable or corrupted timestamps
prod_df = prod_df.dropna(subset=['Timestamp'])
cons_df = cons_df.dropna(subset=['Timestamp'])

# 3. Merge the datasets on the standardized datetime values
merged_df = pd.merge(prod_df, cons_df, on='Timestamp')

if merged_df.empty:
    print("Warning: The merged dataset is empty. Verify that your two CSV files have overlapping dates/times.")
else:
    # 4. Calculate Net Energy (Production - Consumption)
    merged_df['Net_kWh'] = merged_df['Production_kWh'] - merged_df['Consumption_kWh']

    # 5. Calculate Statistics
    positives = (merged_df['Net_kWh'] > 0).sum()
    negatives = (merged_df['Net_kWh'] < 0).sum()
    zeroes = (merged_df['Net_kWh'] == 0).sum()

    print("--- Data Processing Summary ---")
    print(f"Total matching 15-min intervals: {len(merged_df)}")
    print(f"Positive values (Surplus Production):  {positives}")
    print(f"Negative values (Deficit / Grid Pull): {negatives}")
    print(f"Neutral values  (Perfect Balance):     {zeroes}\n")

    # 6. Define Usable Energy
    # If "usable" means how much surplus self-produced energy you have left,
    # then during deficits (negative values) your usable surplus energy is 0 kWh.
    # We use `.clip(lower=0)` to set negative values to 0.
    merged_df['Usable_kWh'] = merged_df['Net_kWh'].clip(lower=0)

    # NOTE: If you want to keep the raw positive & negative values in your file instead,
    # comment out the line above and uncomment the line below:
    # merged_df['Usable_kWh'] = merged_df['Net_kWh']

    # 7. Convert Timestamp back to the standard ISO 8601 format (YYYY-MM-DD HH:MM:SS)
    output_df = merged_df[['Timestamp', 'Usable_kWh']].copy()
    output_df['Timestamp'] = output_df['Timestamp'].dt.strftime('%Y-%m-%d %H:%M:%S')

    # 8. Save output file using the requested ';' delimiter
    output_df.to_csv('usable.csv', sep=';', index=False)
    print("Successfully saved results to 'usable.csv'")