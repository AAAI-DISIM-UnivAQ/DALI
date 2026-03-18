#!/bin/bash
#
# startmas.sh — DALI Advanced example for Docker
#
# Adapted from examples/unix/advanced/startmas.sh for the Docker environment.

SICSTUS_HOME=${SICSTUS_HOME:-/opt/sicstus}
DALI_HOME=/dali/src
CONF_DIR=conf
PROLOG="$SICSTUS_HOME/bin/sicstus"
LINDA_PORT=${LINDA_PORT:-3010}
BUILD_HOME=build
LOG_FILE="./log/restart.log"

SPLIT_PANES=false

if [ "$1" == "--no-split" ]; then
    SPLIT_PANES=false
fi

T0=$(date +%s%3N)

log() {
    local now; now=$(date +%s%3N)
    local elapsed=$(( now - T0 ))
    local msg; msg=$(printf "[BASH][%6d ms] %s" "$elapsed" "$*")
    echo "$msg"
    echo "$msg" >> "$LOG_FILE"
}

kill_and_wait() {
    local pattern="$1"
    pkill -9 -f "$pattern" 2>/dev/null || true
    for _i in $(seq 1 20); do
        pgrep -f "$pattern" &>/dev/null || return 0
        sleep 0.2
    done
}

mkdir -p ./log
log "══════════════════════════════════════════"
log "START $(date '+%A %Y-%m-%d %H:%M:%S %Z')"
log "══════════════════════════════════════════"

current_dir=$(pwd)

if ! command -v tmux &>/dev/null; then exit 1; fi
if [[ ! -x "$PROLOG" ]]; then exit 1; fi

# Lean on TMUX_TMPDIR from environment
TMUX="tmux"

cleanup() {
    $TMUX kill-session -t DALI_session 2>/dev/null || true
    # DO NOT delete server.txt/log on failure so we can debug
    # find work/ -type f ! -name '.gitkeep' -delete 2>/dev/null || true
}
trap cleanup EXIT INT TERM

kill_and_wait "active_server_wi.pl"
kill_and_wait "active_dali_wi.pl"
kill_and_wait "active_user_wi.pl"
for _p in $(seq $LINDA_PORT $((LINDA_PORT + 9))); do
    ss -tlnp "sport = :$_p" 2>/dev/null | grep -oP 'pid=\K[0-9]+' | xargs -r kill -9 2>/dev/null || true
done
$TMUX kill-session -t DALI_session 2>/dev/null || true

cleanup

# ── Build advanced agents ─────────────────────────────────────────
mkdir -p build work conf/mas
log "START build instances loop"
for instance_filename in mas/instances/*.txt; do
    [ -e "$instance_filename" ] || continue
    instance_base="${instance_filename##*/}"
    while IFS= read -r line || [ -n "$line" ]; do
        line=$(echo "$line" | tr -d '\r')
        [ -z "$line" ] && continue
        type_file="mas/types/${line}.txt"
        if [ -f "$type_file" ]; then
            cat "$type_file" > "build/$instance_base"
            echo "" >> "build/$instance_base"
            log "built instance $instance_base from type $line"
        fi
    done < "$instance_filename"
done
log "END build instances loop"

cp build/*.txt work/ 2>/dev/null || true

# ── Start LINDA server ────────────────────────────────────────────
rm -f server.txt server.log
echo "[startmas] Starting LINDA server logging to server.log..."

# In Docker, we sometimes have issues with tmux stdout. 
# Let's run it directly to ensure proper quoting expansion
"$PROLOG" --noinfo -l "$DALI_HOME/active_server_wi.pl" --goal "go($LINDA_PORT,'server.txt')." > server.log 2>&1 &
LINDA_PID=$!
log "LINDA server started as PID $LINDA_PID"

LINDA_PORT_ACTUAL=""
for i in $(seq 1 150); do
    if [ -s server.txt ]; then
        LINDA_PORT_ACTUAL=$(grep -oP ':\K[0-9]+(?=\.)' server.txt | head -1)
        [ -n "$LINDA_PORT_ACTUAL" ] && break
    fi
    sleep 0.2
    if [ $i -eq 150 ]; then exit 1; fi
done
LINDA_PORT=$LINDA_PORT_ACTUAL

# ── Start agents ──────────────────────────────────────────────────
# Create the tmux session first (now that server is outside tmux)
$TMUX new-session -d -s DALI_session -n "server" "tail -f server.log"
for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    agent_name="${agent_base%.*}"
    bash "$current_dir/conf/makeconf.sh" "$agent_base" "$DALI_HOME"
    echo "[startmas] Launching agent: $agent_name ..."
    $TMUX new-window -t DALI_session -n "$agent_name" \
        "$current_dir/conf/startagent.sh $agent_base $PROLOG $DALI_HOME"
    sleep 2
done

# ── Start User Agent (logic moved to MAS) ────────────────────────
log "START user agent (SPLIT_PANES=$SPLIT_PANES)"

$TMUX new-window -t DALI_session -n "user" \
    "$PROLOG --noinfo --quiet -l $DALI_HOME/active_user_wi.pl --goal \"utente.\""
$TMUX set-window-option -t DALI_session:user allow-rename off
log "User Agent started in new window"

log "MAS Advanced started (non-interactive Docker mode)"

while $TMUX has-session -t DALI_session 2>/dev/null; do
    sleep 5
done
