#!/bin/sh
# Seeds /config with the neutral default config on first start, then runs the server.
set -e
: "${CONFIG_FILE:?CONFIG_FILE not set}"
if [ ! -f "$CONFIG_FILE" ]; then
    mkdir -p "$(dirname "$CONFIG_FILE")"
    cp "$DEFAULT_CONFIG" "$CONFIG_FILE" && echo "[entrypoint] created default config at $CONFIG_FILE"
fi
exec "$@"
