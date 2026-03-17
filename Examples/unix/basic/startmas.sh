#!/bin/bash
#
# startmas.sh — DALI Multi-Agent System launcher (Linux/macOS)
#
# What this script does, in order:
#   1. Acquire an exclusive lock (/tmp/dali_startmas.lock) — prevents concurrent startups
#   2. Kill any running SICStus processes (active_server_wi.pl / active_dali_wi.pl / active_user_wi.pl)
#   3. Kill any process holding ports 3010–3019
#   4. Destroy any leftover 'DALI_session' tmux session
#   5. Clean work/, build/, conf/mas/ directories
#   6. Build agent files from mas/instances/ + mas/types/ → build/
#   7. Copy built agents to work/
#   8. Start the LINDA server in tmux window 'server' (tries ports 3010–3019, writes chosen port to server.txt)
#   9. Wait (up to 30 s) for server.txt to be written by on_open/3
#  10. Confirm the chosen port is in LISTEN state via ss
#  11. Start the user agent and all agents:
#      - SPLIT_PANES=true  → all in split panes inside the 'server' window (tiled layout)
#      - SPLIT_PANES=false → each in a dedicated tmux window (tab)
#  12. Apply 'tiled' layout (only when SPLIT_PANES=true)
#  13. Attach to the tmux session (interactive mode only — skipped when invoked from a UI/daemon)
#
# Enable debugging
# set -x  # Start debugging

# Define paths and variables
SICSTUS_HOME=/usr/local/sicstus4.6.0
MAIN_HOME=../../..
DALI_HOME=../../../src
CONF_DIR=conf
PROLOG="$SICSTUS_HOME/bin/sicstus"
LINDA_PORT=3010
BUILD_HOME=build
T0=$(date +%s%3N)   # script start time in milliseconds
LOG_FILE="./log/restart.log"

# This need to be false to see correctly the output in the UI
SPLIT_PANES=true  # true = all agents in split panes (tiled); false = one tmux window per agent

log() {
    local now; now=$(date +%s%3N)
    local elapsed=$(( now - T0 ))
    local msg; msg=$(printf "[BASH][%6d ms] %s" "$elapsed" "$*")
    echo "$msg"                        # stdout (visible in tmux pane)
    echo "$msg" >> "$LOG_FILE"        # shared log file
}

kill_and_wait() {
    local pattern="$1"
    pkill -9 -f "$pattern" 2>/dev/null || true
    for _i in $(seq 1 20); do
        pgrep -f "$pattern" &>/dev/null || return 0
        sleep 0.2
    done
    log "WARNING: process matching '$pattern' did not die in 4 s"
}

mkdir -p ./log
log "══════════════════════════════════════════"
log "START $(date '+%A %Y-%m-%d %H:%M:%S %Z')"
log "══════════════════════════════════════════"
clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

log "current directory: $current_dir"

# Test if tmux is installed
if command -v tmux &> /dev/null; then
    log "tmux installed: $(tmux -V)"
else
    log "ERROR: tmux is not installed — required on Unix-like OS"
    log "ERROR: check https://github.com/tmux/tmux/wiki/Installing"
    exit -1
fi

# Uses a lock file so concurrent startmas.sh invocations never overlap.
LOCKFILE="/tmp/dali_startmas.lock"
exec 200>"$LOCKFILE"
flock -x 200   # blocks until any other running instance releases the lock

log "START cleanup — port range $LINDA_PORT..$(( LINDA_PORT + 9 ))"

# Helper: kill by pattern and wait until pgrep confirms the process is gone.
log "  kill_and_wait active_server_wi.pl"
kill_and_wait "active_server_wi.pl"
log "  kill_and_wait active_dali_wi.pl"
kill_and_wait "active_dali_wi.pl"
log "  kill_and_wait active_user_wi.pl"
kill_and_wait "active_user_wi.pl"

# Kill by port as belt-and-suspenders and destroy leftover tmux session.
for _p in 3010 3011 3012 3013 3014 3015 3016 3017 3018 3019; do
    ss -tlnp "sport = :$_p" 2>/dev/null \
      | grep -oP 'pid=\K[0-9]+' \
      | xargs -r kill -9 2>/dev/null || true
done
tmux kill-session -t DALI_session 2>/dev/null || true

log "END cleanup"

# Check if SICStus Prolog exists and is executable
if [[ -x "$PROLOG" ]]; then
  log "SICStus Prolog found at $PROLOG"
else
  log "ERROR: SICStus Prolog not found at $PROLOG or is not executable"
  exit -1
fi

cleanup() {
    log "START cleanup dynamic files and tmux session"
    # Kill the tmux session if we exited or the terminal was closed abruptly
    tmux kill-session -t DALI_session 2>/dev/null || true
    
    # Find all files in work, build, conf/mas and delete them, EXCEPT .gitkeep
    find work/ -type f ! -name '.gitkeep' -delete 2>/dev/null || true
    find build/ -type f ! -name '.gitkeep' -delete 2>/dev/null || true
    find conf/mas/ -type f ! -name '.gitkeep' -delete 2>/dev/null || true
    rm -f server.txt
    
    # Also ensure work/log exists for future runs
    mkdir -p work/log
    log "END cleanup dynamic files"
}

# Register the cleanup function to run when the script exits
trap cleanup EXIT

# Clean directories on startup (belt-and-suspenders)
cleanup

# Build agents based on instances
log "START build instances loop"
# For the basic example, agents are fully defined in mas/ directory directly.
cp mas/*.txt $BUILD_HOME/ 2>/dev/null || true
log "END build instances loop"

ls $BUILD_HOME
cp $BUILD_HOME/*.txt work

# Wait until a tmux pane has produced at least one line of output.
# Usage: wait_for_pane <pane_id|session:window> [timeout_seconds]
wait_for_pane() {
    local target="$1"
    local timeout="${2:-10}"
    local elapsed=0
    while [ "$elapsed" -lt "$timeout" ]; do
        local content
        content=$(tmux capture-pane -pt "$target" 2>/dev/null | tr -d '[:space:]')
        [ -n "$content" ] && return 0
        sleep 0.3
        elapsed=$((elapsed + 1))
    done
    log "WARNING: pane '$target' had no output after ${timeout}s — continuing anyway"
}

# Remove any stale server.txt so we can detect when the new server writes its actual port.
rm -f server.txt

# Start the LINDA server in a new tmux session.
# go($LINDA_PORT,'server.txt') will try ports LINDA_PORT..LINDA_PORT+9 until one is free,
# then write the chosen host:port to server.txt via on_open/3.
log "START tmux server (starting from port $LINDA_PORT)"
srvcmd="$PROLOG --noinfo -l $DALI_HOME/active_server_wi.pl --goal go($LINDA_PORT,'server.txt')."
log "  cmd: $srvcmd"
log "  note: $LINDA_PORT is the first candidate; Prolog will try up to $((LINDA_PORT+9)) — actual port written to server.txt"
tmux new-session -d -s DALI_session -n "server" $srvcmd
log "END tmux server launched"

# Wait until server.txt is written by on_open/3 (contains the actual host:port chosen).
log "START server.txt wait"
LINDA_PORT_ACTUAL=""
for i in $(seq 1 150); do
    if [ -s server.txt ]; then
        LINDA_PORT_ACTUAL=$(grep -oP ':\K[0-9]+(?=\.)' server.txt | head -1)
        [ -n "$LINDA_PORT_ACTUAL" ] && break
    fi
    sleep 0.2
    if [ $i -eq 150 ]; then
        log "ERROR: server.txt not written within 30 s — dumping server pane"
        tmux capture-pane -pt DALI_session:server -S -50 2>/dev/null | tail -20 | while IFS= read -r l; do log "  [server] $l"; done
        log "ABORT: LINDA server did not start within 30 s"
        exit 1
    fi
done
log "END server.txt ready — actual port: $LINDA_PORT_ACTUAL"

# Confirm the chosen port is actually in LISTEN state (belt-and-suspenders check).
if ss -tnlp "sport = :$LINDA_PORT_ACTUAL" 2>/dev/null | grep -q "LISTEN"; then
    log "LINDA listening on $LINDA_PORT_ACTUAL confirmed"
else
    log "WARNING: port $LINDA_PORT_ACTUAL not yet in LISTEN — proceeding anyway"
fi
# Update LINDA_PORT so downstream uses (if any) reflect the actual port chosen.
LINDA_PORT=$LINDA_PORT_ACTUAL

# Start user agent — pane or window depending on SPLIT_PANES
log "START user agent (SPLIT_PANES=$SPLIT_PANES)"
if [ "$SPLIT_PANES" = "true" ]; then
    user_pane=$(tmux split-window -d -P -F '#{pane_id}' -t DALI_session:server \
        "$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal utente.")
    wait_for_pane "$user_pane"
    log "END wait_for_pane user ($user_pane)"
else
    tmux new-window -t DALI_session -n "user" \
        "$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal utente."
    wait_for_pane DALI_session:user
    log "END wait_for_pane user"
fi

# Launch agents — pane or window depending on SPLIT_PANES
for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    agent_name="${agent_base%.*}"
    log "  START agent $agent_name"
    $current_dir/conf/makeconf.sh $agent_base $DALI_HOME
    if [ "$SPLIT_PANES" = "true" ]; then
        agent_pane=$(tmux split-window -d -P -F '#{pane_id}' -t DALI_session:server \
            "$current_dir/conf/startagent.sh $agent_base $PROLOG $DALI_HOME")
        wait_for_pane "$agent_pane"
        log "  END wait_for_pane $agent_name ($agent_pane)"
    else
        tmux new-window -t DALI_session -n "$agent_name" \
            "$current_dir/conf/startagent.sh $agent_base $PROLOG $DALI_HOME"
        wait_for_pane "DALI_session:$agent_name"
        log "  END wait_for_pane $agent_name"
    fi
done

# Apply tiled layout when all agents share the same window
if [ "$SPLIT_PANES" = "true" ]; then
    tmux select-layout -t DALI_session:server tiled
fi

log "DONE — MAS started"
echo "" >> "$LOG_FILE"   # blank separator line between runs

# Attach to the session only when running interactively
if [ -t 0 ]; then
    tmux attach -t DALI_session
    echo "Press Enter to shutdown the MAS"
    read
fi
