import sqlite3

conn = sqlite3.connect('/data/bb.db')
conn.row_factory = sqlite3.Row

for table in ['setpoints', 'measurements', 'disturbance_forecasts', 'assets']:
    print(f'\n=== {table} ===')
    rows = conn.execute(f'SELECT * FROM {table} ORDER BY rowid DESC LIMIT 10').fetchall()
    if rows:
        keys = list(rows[0].keys())
        print('  '.join(f'{k:>20}' for k in keys))
        print('  '.join('-' * 20 for _ in keys))
        for r in rows:
            print('  '.join(f'{str(v):>20}' for v in r))
    else:
        print('  (empty)')
