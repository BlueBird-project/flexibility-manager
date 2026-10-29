#!/usr/bin/env sh
set -eu

INPUT_FILE=${INPUT_FILE:-/app/data/input.csv}
OUTPUT_FILE=${OUTPUT_FILE:-/app/data/forecasts.feather}
PATH_FAST=${PATH_FAST:-/app}
FORGES=${FORGES:-weather_vals interpolate_15min positive_pv}
TRANSLATE=${TRANSLATE:-chronos_2}
MODE=${MODE:-production}
HORIZON_SPAN=${HORIZON_SPAN:-96}
START_DATE=${START_DATE:-}
END_DATE=${END_DATE:-}

set -- \
  python /app/fast_inference.py \
  --file "$INPUT_FILE" \
  --output "$OUTPUT_FILE" \
  --path_fast "$PATH_FAST" \
  --mode "$MODE" \
  --horizon_span "$HORIZON_SPAN" \
  --forges $FORGES \
  --translate $TRANSLATE

if [ -n "$START_DATE" ]; then
  set -- "$@" --start_date "$START_DATE"
fi

if [ -n "$END_DATE" ]; then
  set -- "$@" --end_date "$END_DATE"
fi

exec "$@"
