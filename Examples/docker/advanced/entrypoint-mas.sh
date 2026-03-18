#!/bin/bash
# entrypoint-mas.sh — runs inside the dali-mas container
#
# 1. Writes the SICStus license file from environment variables
# 2. Copies startmas.sh from the mounted example directory
# 3. Launches startmas.sh in --no-split mode (one tmux window per agent)
#    and redirects server.txt to the shared volume
#
# Required env vars:
#   EXAMPLE_DIR              — path to mounted example (e.g. /dali/examples/basic)
#   SICSTUS_LICENSE_SITE     — site identifier
#   SICSTUS_LICENSE_EXPIRY   — expiry date (or "permanent")
#   SICSTUS_LICENSE_KEY      — license key string

set -e
 
SICSTUS_HOME=${SICSTUS_HOME:-/opt/sicstus}
SHARED=${SHARED:-/dali/shared}
EXAMPLE_DIR=${EXAMPLE_DIR:-/dali/examples/docker/advanced}
LICENSE_FILE="${SICSTUS_HOME}/lib/sicstus-4.6.0/license/sp-license"

# ── Write SICStus license ─────────────────────────────────────────
echo "[mas] Writing SICStus license to multiple locations..."
# Standard paths for SICStus 4.6
L1="${SICSTUS_HOME}/lib/sicstus-4.6.0/library/license.pl"
L2="${SICSTUS_HOME}/lib/sicstus-4.6.0/license/sp-license"
L3="${SICSTUS_HOME}/lib/license/sp-license"
L4="${SICSTUS_HOME}/sp-license"

for LFILE in "$L1" "$L2" "$L3" "$L4"; do
    mkdir -p "$(dirname "$LFILE")"
    cat > "$LFILE" <<EOF
site('${SICSTUS_LICENSE_SITE}').
product('sicstus4.6_linux', '${SICSTUS_LICENSE_EXPIRY}', '${SICSTUS_LICENSE_KEY}').
EOF
    tr -d '\r' < "$LFILE" > "${LFILE}.tmp" && mv "${LFILE}.tmp" "$LFILE"
done

# ── Verify SICStus Installation & License ─────────────────────────
echo "[mas] Verifying SICStus installation at $SICSTUS_HOME..."
if [ ! -x "$SICSTUS_HOME/bin/sicstus" ]; then
    echo "[mas] ERROR: SICStus binary NOT found at $SICSTUS_HOME/bin/sicstus."
    echo "[mas] Listing /opt/sicstus structure for debug:"
    ls -R /opt/sicstus | head -n 20
    exit 1
fi

echo "[mas] Binary found. Verifying license..."
# DEBUG: Check where sicstus looks for the license
strace -e openat,open "$SICSTUS_HOME/bin/sicstus" --goal "halt." 2>&1 | grep sp-license || true

if "$SICSTUS_HOME/bin/sicstus" --goal "halt." ; then
    echo "[mas] SUCCESS: SICStus is working and license is valid."
else
    echo "[mas] ERROR: SICStus license check failed. See strace output above for the searched path."
    # List files in lib as a last resort
    ls -R "$SICSTUS_HOME/lib" | grep license || true
    exit 1
fi

# ── Prepare shared volume ─────────────────────────────────────────
mkdir -p "$SHARED"

# Rimuovi flag stale da run precedenti
rm -f "$SHARED/restart.flag"

# ── Symlink server.txt into the shared volume ─────────────────────
# startmas.sh writes server.txt in the current directory (the example dir).
# We'll set up a symlink so the UI container can find it at $SHARED/server.txt.

# ── Launch MAS ────────────────────────────────────────────────────
echo "[mas] Starting MAS from $EXAMPLE_DIR ..."
cd "$EXAMPLE_DIR"

# Override SICSTUS_HOME path in startmas.sh via environment
export SICSTUS_HOME

# Run startmas.sh in non-interactive mode (--no-split).
# The script writes server.txt in $EXAMPLE_DIR; we then symlink to shared/.
bash startmas.sh --no-split &
MAS_PID=$!

# ── Wait for server.txt ───────────────────────────────────────────
echo "[mas] Waiting for LINDA server to create server.txt..."

# Start tailing server.log in the background if it exists (or wait for it)
(
    while [ ! -f "server.log" ]; do sleep 0.5; done
    echo "[mas] Found server.log, tailing output..."
    tail -f server.log
) &
TAIL_PID=$!

for i in $(seq 1 60); do
    if [ -s server.txt ]; then
        echo "[mas] SUCCESS: server.txt created by LINDA server."
        # Mirror to shared volume IMMEDIATELY so UI can start
        cp server.txt "$SHARED/server.txt"
        break
    fi
    if [ $((i % 5)) -eq 0 ]; then
        echo "[mas] ... still waiting for server.txt (attempt $i/60)..."
        # Debug: Check if sicstus process is even alive
        if ! pgrep sicstus > /dev/null; then
            echo "[mas] WARNING: No sicstus process found! It might have crashed."
            [ -f "server.log" ] && echo "[mas] Last 20 lines of server.log:" && tail -n 20 server.log
        fi
    fi
    sleep 1
    if [ $i -eq 60 ]; then
        echo "[mas] ERROR: Timeout waiting for server.txt."
        echo "[mas] Contents of current directory:"
        ls -la
        [ -f "server.log" ] && echo "[mas] Full server.log content:" && cat server.log
        kill $TAIL_PID 2>/dev/null
        exit 1
    fi
done

kill $TAIL_PID 2>/dev/null
echo "[mas] MAS setup complete. Agents should be active."

# Keep syncing server.txt in case it gets updated, and stay alive
# Also watch for restart command from UI
while kill -0 $MAS_PID 2>/dev/null; do
    if [ -f "$SHARED/restart.flag" ]; then
        echo "[mas] Restart flag detected! Restarting MAS..."
        rm -f "$SHARED/restart.flag"
        kill -TERM $MAS_PID
        wait $MAS_PID 2>/dev/null || true
        bash startmas.sh --no-split &
        MAS_PID=$!
    fi
    [ -f "server.txt" ] && cp server.txt "$SHARED/server.txt" 2>/dev/null || true
    sleep 0.5
done
