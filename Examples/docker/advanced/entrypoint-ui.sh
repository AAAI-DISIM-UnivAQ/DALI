#!/bin/bash
# entrypoint-ui.sh — runs inside the dali-ui container
#
# 1. Writes the SICStus license file from environment variables
# 2. Waits for server.txt on the shared volume
# 3. Launches active_user_wi.pl (interactive user agent)
#
# Required env vars:
#   SERVER_TXT               — path to server.txt on shared volume
#   SICSTUS_LICENSE_SITE     — site identifier
#   SICSTUS_LICENSE_EXPIRY   — expiry date (or "permanent")
#   SICSTUS_LICENSE_KEY      — license key string

set -e

# Launch UI dashboard
cd /dali/examples/docker/advanced
echo "[ui] Starting Dashboard on port 5000..."
# dashboard.py will find tmux session via TMUX_TMPDIR env var
exec python3 ui/dashboard.py --folder . --port 5000
