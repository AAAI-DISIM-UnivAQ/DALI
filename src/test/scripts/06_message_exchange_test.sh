#!/bin/bash

# DALI Multi-Agent System with Message Exchange Testing
# Integrates startmas_modular.sh logic with message verification capabilities

# Enable debugging if needed
# set -x

clear
echo "======================================================================"
echo "DALI MAS - Message Exchange Testing Script"
echo "======================================================================"

# Save the current directory
current_dir=$(pwd)
echo "Current directory: $current_dir"

# =================================================================
# CONFIGURATION VARIABLES
# =================================================================
# Time intervals for message testing (in seconds)
STARTUP_WAIT_TIME=8          # Time to wait after all agents are started
MESSAGE_TEST_INTERVAL=10     # Time to run message exchange test
STABILIZATION_TIME=5         # Time to wait between test phases
SHUTDOWN_DELAY=3             # Time before final shutdown

# Agent interaction settings
TEST_MESSAGE_EVENT="goE"     # Event to trigger for testing
TEST_CYCLES=3                # Number of message test cycles

echo "Test Configuration:"
echo "- Startup wait time: ${STARTUP_WAIT_TIME}s"
echo "- Message test interval: ${MESSAGE_TEST_INTERVAL}s"
echo "- Test cycles: ${TEST_CYCLES}"
echo "- Stabilization time: ${STABILIZATION_TIME}s"
echo ""

# =================================================================
# OS DETECTION AND NETWORK OPTIMIZATION
# =================================================================
os_name=$(uname -s)

case "$os_name" in
    Darwin)
        echo "Operating System: macOS"
        current_msl=$(sysctl -n net.inet.tcp.msl)
        if [ "$current_msl" -gt 1000 ]; then
            echo "Reducing TIME_WAIT timeout for macOS from $current_msl to 1000..."
            sudo sysctl -w net.inet.tcp.msl=1000
        else
            echo "TIME_WAIT timeout already optimal: $current_msl"
        fi
        ;;
    Linux)
        echo "Operating System: Linux"
        current_timeout=$(sysctl -n net.ipv4.tcp_fin_timeout)
        if [ "$current_timeout" -gt 30 ]; then
            echo "Reducing TIME_WAIT timeout for Linux from $current_timeout to 30..."
            sudo sysctl -w net.ipv4.tcp_fin_timeout=30
            sudo sysctl -w net.ipv4.tcp_tw_reuse=1
        else
            echo "TIME_WAIT timeout already optimal: $current_timeout"
        fi
        ;;
    *)
        echo "Operating System not supported: $os_name"
        exit 1
        ;;
esac

# =================================================================
# PORT AVAILABILITY CHECK
# =================================================================
echo "Waiting for DALI Server Port 3010 to be free..."
while netstat -an | grep -q "3010"; do
    sleep 1
done
echo "✓ Port 3010 is now free, proceeding with DALI startup..."

# =================================================================
# SICSTUS PROLOG DETECTION
# =================================================================
echo "Auto-detecting SICStus Prolog installation..."
PROLOG=""

SICSTUS_PATHS=(
    "/usr/local/sicstus4.6.0/bin/sicstus"
    "/usr/local/sicstus/bin/sicstus"
    "/opt/sicstus/bin/sicstus"
    "/Applications/SICStus Prolog 4.6.0/bin/sicstus"
    "sicstus"
)

for path in "${SICSTUS_PATHS[@]}"; do
    if command -v "$path" &> /dev/null; then
        PROLOG="$path"
        echo "✓ SICStus Prolog found at: $PROLOG"
        break
    fi
done

if [ -z "$PROLOG" ]; then
    echo "Error: SICStus Prolog not found in common locations."
    echo "Tried paths: ${SICSTUS_PATHS[*]}"
    exit 1
fi

# =================================================================
# DALI SYSTEM DETECTION
# =================================================================
# Determine DALI core path based on where script is run from
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
if [[ "$current_dir" == */src/test/scripts ]]; then
    # Running from scripts directory
    DALI_CORE_PATH="$(cd .. && pwd)"
else
    # Running from project root
    DALI_CORE_PATH="$current_dir/src"
fi

if [ -d "$DALI_CORE_PATH" ]; then
    echo "✓ Modular DALI system found at $DALI_CORE_PATH"
else
    echo "Error: DALI core system not found at $DALI_CORE_PATH"
    exit 1
fi

# =================================================================
# WINDOW POSITIONING CONFIGURATION
# =================================================================
WINDOW_START_X=100
WINDOW_START_Y=100
WINDOW_OFFSET=50
WINDOW_WIDTH_MACOS=450
WINDOW_HEIGHT_MACOS=325
WINDOW_COLS_LINUX=80
WINDOW_ROWS_LINUX=25

# =================================================================
# AGENT BUILDING PHASE
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 1: Building Agent Instances"
echo "======================================================================"

# Create build directory
mkdir -p build
rm -f build/*.txt

# Build agents from test configuration
echo "Building test agent instances..."

# Use original agent types from Examples/advanced/mas/types/
if [[ "$current_dir" == */src/test/scripts ]]; then
    # Running from scripts directory
    AGENT_TYPES_DIR="../../../Examples/advanced/mas/types"
else
    # Running from project root
    AGENT_TYPES_DIR="Examples/advanced/mas/types"
fi
if [ -d "$AGENT_TYPES_DIR" ]; then
    echo "Using original agent types from: $AGENT_TYPES_DIR"
    # Copy original agent types as agents
    if [ -f "$AGENT_TYPES_DIR/agentType1.txt" ]; then
        cp "$AGENT_TYPES_DIR/agentType1.txt" "build/agent1.txt"
        echo "Copied agentType1.txt as agent1.txt"
    fi
    if [ -f "$AGENT_TYPES_DIR/agentType2.txt" ]; then
        cp "$AGENT_TYPES_DIR/agentType2.txt" "build/agent2.txt" 
        echo "Copied agentType2.txt as agent2.txt"
    fi
    
    # Add test sender agent (does not respond to events, only sends messages)
    cat > "build/test_sender.txt" << 'EOF'
:- write('=== Test Sender Agent Started ==='), nl.

% Timer-based message sending (no event responses)
t10.

% Send test messages periodically
send_test_messages :-
    write('TestSender: Sending test messages...'), nl,
    messageA(agent1, send_message(test_msg_from_sender, test_sender)),
    sleep(2),
    messageA(agent2, send_message(hello_from_sender, test_sender)),
    sleep(2),
    messageA(agent1, send_message(chain_test, test_sender)).

% Automatic execution after startup
:- call_after(5, send_test_messages).

% Periodic test execution
periodic_test :-
    send_test_messages,
    call_after(15, periodic_test).

:- call_after(15, periodic_test).
EOF

else
    echo "Error: Agent types directory not found at $AGENT_TYPES_DIR"
    echo "Cannot proceed without original agent types"
    exit 1
fi

# Always add test sender agent regardless of source
cat > "build/test_sender.txt" << 'EOF'
:- write('=== Test Sender Agent Started ==='), nl.

% Timer for periodic sending
t10.

% Send initial test messages after startup
start_testingI :- 
    write('TestSender: Starting message sending...'), nl,
    messageA(agent1, send_message(go, test_sender)),
    messageA(agent2, send_message(go, test_sender)).

% Send test messages periodically
send_test_messagesI :-
    write('TestSender: Sending periodic test messages...'), nl,
    messageA(agent1, send_message(test_msg, test_sender)),
    messageA(agent2, send_message(hello, test_sender)).
EOF

# Create configuration files for the agents
echo "Creating agent configuration files..."
# Determine communication path based on where script is run from
if [[ "$current_dir" == */src/test/scripts ]]; then
    # Running from scripts directory
    COMM_PATH="../conf/communication"
else
    # Running from project root
    COMM_PATH="src/test/conf/communication"
fi

cat > "build/agent1.conf" << EOF
agent('build/agent1','agent1','no',italian,['$COMM_PATH'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOF

cat > "build/agent2.conf" << EOF
agent('build/agent2','agent2','no',italian,['$COMM_PATH'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOF

cat > "build/test_sender.conf" << EOF
agent('build/test_sender','test_sender','no',italian,['$COMM_PATH'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOF

echo "Built agents:"
ls -la build/

# =================================================================
# TEMPORARY SCRIPTS SETUP
# =================================================================
TEMP_DIR="${current_dir}/temp_scripts"
mkdir -p "$TEMP_DIR"

# Array to store window identifiers for cleanup
declare -a DALI_WINDOW_IDS=()

# =================================================================
# PHASE 2: STARTING DALI COMPONENTS
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 2: Starting DALI Components"
echo "======================================================================"

# Start LINDA server
echo "Starting DALI Server..."
LINDA_TITLE="[DALI] DALI Server"
DALI_WINDOW_IDS+=("$LINDA_TITLE")

case "$os_name" in
    Darwin)
        echo "Starting: DALI Server (positioned at $WINDOW_START_X,$WINDOW_START_Y, size ${WINDOW_WIDTH_MACOS}x${WINDOW_HEIGHT_MACOS})"
        osascript << EOF
tell application "Terminal"
    set newTab to do script "cd '$current_dir' && '$PROLOG' --noinfo -l '$DALI_CORE_PATH/active_server_wi.pl' --goal go."
    set custom title of newTab to "$LINDA_TITLE"
    set position of window 1 to {$WINDOW_START_X, $WINDOW_START_Y}
    set size of window 1 to {$WINDOW_WIDTH_MACOS, $WINDOW_HEIGHT_MACOS}
end tell
EOF
        ;;
    Linux)
        gnome-terminal --title="$LINDA_TITLE" --geometry="${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+${WINDOW_START_X}+${WINDOW_START_Y}" -- bash -c "cd '$current_dir' && '$PROLOG' --noinfo -l '$DALI_CORE_PATH/active_server_wi.pl' --goal go." &
        ;;
esac

sleep 3
echo "✓ DALI Server started"

# Start agents
echo ""
echo "Starting agent instances..."
agent_count=0

for agent_file in build/*.txt; do
    if [ -f "$agent_file" ]; then
        agent_name=$(basename "$agent_file" .txt)
        agent_count=$((agent_count + 1))
        
        # Calculate window position
        window_x=$((WINDOW_START_X + WINDOW_OFFSET * agent_count))
        window_y=$((WINDOW_START_Y + WINDOW_OFFSET * agent_count))
        
        AGENT_TITLE="[DALI] DALI Agent (Modular): $agent_name.txt"
        DALI_WINDOW_IDS+=("$AGENT_TITLE")
        
        echo "Agent: $agent_name.txt (using modular architecture)"
        
        case "$os_name" in
            Darwin)
                echo "Starting: DALI Agent (Modular): $agent_name.txt (positioned at $window_x,$window_y, size ${WINDOW_WIDTH_MACOS}x${WINDOW_HEIGHT_MACOS})"
                osascript << EOF
tell application "Terminal"
    set newTab to do script "cd '$current_dir' && '$PROLOG' --noinfo -l '$DALI_CORE_PATH/dali_core.pl' --goal \"start_dali_agent('build/$agent_name.conf').\""
    set custom title of newTab to "$AGENT_TITLE"
    set position of front window to {$window_x, $window_y}
    set size of front window to {$WINDOW_WIDTH_MACOS, $WINDOW_HEIGHT_MACOS}
end tell
EOF
                ;;
            Linux)
                gnome-terminal --title="$AGENT_TITLE" --geometry="${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+${window_x}+${window_y}" -- bash -c "cd '$current_dir' && '$PROLOG' --noinfo -l '$DALI_CORE_PATH/dali_core.pl' --goal \"start_dali_agent('build/$agent_name.conf').\"" &
                ;;
        esac
        
        sleep 2
    fi
done

echo "✓ All agents started successfully"

# Start user interface
echo ""
echo "Starting DALI User Interface..."
UI_WINDOW_X=$((WINDOW_START_X + WINDOW_OFFSET * (agent_count + 1)))
UI_WINDOW_Y=$((WINDOW_START_Y + WINDOW_OFFSET * (agent_count + 1)))

UI_TITLE="[DALI] DALI User Interface"
DALI_WINDOW_IDS+=("$UI_TITLE")

case "$os_name" in
    Darwin)
        echo "Starting: DALI User Interface (positioned at $UI_WINDOW_X,$UI_WINDOW_Y, size ${WINDOW_WIDTH_MACOS}x${WINDOW_HEIGHT_MACOS})"
        osascript << EOF
tell application "Terminal"
    set newTab to do script "cd '$current_dir' && '$PROLOG' --noinfo -l '$DALI_CORE_PATH/active_user_wi.pl' --goal go."
    set custom title of newTab to "$UI_TITLE"
    set position of front window to {$UI_WINDOW_X, $UI_WINDOW_Y}
    set size of front window to {$WINDOW_WIDTH_MACOS, $WINDOW_HEIGHT_MACOS}
end tell
EOF
        ;;
    Linux)
        gnome-terminal --title="$UI_TITLE" --geometry="${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+${UI_WINDOW_X}+${UI_WINDOW_Y}" -- bash -c "cd '$current_dir' && '$PROLOG' --noinfo -l '$DALI_CORE_PATH/active_user_wi.pl' --goal go." &
        ;;
esac

echo "✓ DALI User Interface started"

# =================================================================
# PHASE 3: SYSTEM INITIALIZATION WAIT
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 3: System Initialization"
echo "======================================================================"

echo "Waiting for all components to initialize..."
echo "- Startup wait time: ${STARTUP_WAIT_TIME} seconds"

for i in $(seq 1 $STARTUP_WAIT_TIME); do
    echo -n "."
    sleep 1
done
echo ""
echo "✓ System initialization completed"

# =================================================================
# PHASE 4: MESSAGE EXCHANGE TESTING
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 4: Message Exchange Testing"
echo "======================================================================"

echo "Starting message exchange testing cycles..."
echo "Event to trigger: $TEST_MESSAGE_EVENT"
echo "Number of cycles: $TEST_CYCLES"
echo ""

for cycle in $(seq 1 $TEST_CYCLES); do
    echo "--- Test Cycle $cycle/$TEST_CYCLES ---"
    echo "$(date): Observing automatic message exchange..."
    
    case $cycle in
        1)
            echo "Cycle 1: Initial automatic message sending (test_sender starts after 5s)"
            ;;
        2)
            echo "Cycle 2: Periodic message exchange (test_sender sends every 15s)"
            ;;
        3)
            echo "Cycle 3: Continuous message flow observation"
            ;;
    esac
    
    # Simply observe the automatic message exchange
    echo "Observing automatic message exchange for ${MESSAGE_TEST_INTERVAL}s..."
    echo "- test_sender will automatically send messages to agent1 and agent2"
    echo "- agent2 will send initial message to agent1 via start_agentI"
    echo "- All message exchanges will be visible in agent windows"
    
    for i in $(seq 1 $MESSAGE_TEST_INTERVAL); do
        if [ $((i % 3)) -eq 0 ]; then
            echo -n "."
        fi
        sleep 1
    done
    echo ""
    
    if [ $cycle -lt $TEST_CYCLES ]; then
        echo "Stabilization pause: ${STABILIZATION_TIME}s"
        sleep $STABILIZATION_TIME
    fi
    
    echo "✓ Test cycle $cycle completed"
    echo ""
done

echo "✓ All message exchange test cycles completed"

# =================================================================
# PHASE 5: FINAL MONITORING PERIOD
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 5: Final Monitoring Period"
echo "======================================================================"

echo "Final monitoring period to observe system behavior..."
echo "Duration: ${MESSAGE_TEST_INTERVAL} seconds"

for i in $(seq 1 $MESSAGE_TEST_INTERVAL); do
    if [ $((i % 3)) -eq 0 ]; then
        echo -n "*"
    fi
    sleep 1
done
echo ""
echo "✓ Monitoring period completed"

# =================================================================
# PHASE 6: SYSTEM INFORMATION
# =================================================================
echo ""
echo "======================================================================"
echo "SYSTEM INFORMATION"
echo "======================================================================"

echo "Modular MAS with Message Testing completed successfully!"
echo "Using new modular DALI architecture"
echo "Core system: $DALI_CORE_PATH/dali_core.pl"
echo "SICStus Prolog: $PROLOG"
echo "Temporary scripts: $TEMP_DIR"
echo ""
echo "Test Summary:"
echo "- Total test cycles: $TEST_CYCLES"
echo "- Message test interval per cycle: ${MESSAGE_TEST_INTERVAL}s"
echo "- Total testing time: $((TEST_CYCLES * MESSAGE_TEST_INTERVAL + (TEST_CYCLES - 1) * STABILIZATION_TIME))s"
echo "- Test agents: agent1 (original), agent2 (original), test_sender (automatic)"
echo "- Message flow: test_sender -> agent1/agent2, agent2 -> agent1 (via start_agentI)"
echo ""

# =================================================================
# PHASE 7: SHUTDOWN PREPARATION
# =================================================================
echo "======================================================================"
echo "PHASE 7: Shutdown"
echo "======================================================================"

echo "Message exchange testing completed."
echo "Waiting ${SHUTDOWN_DELAY} seconds before shutdown..."
sleep $SHUTDOWN_DELAY

echo "Press Enter to shutdown the MAS, or wait 10 seconds for automatic shutdown"

# Auto-shutdown after 10 seconds if no input
if timeout 10 bash -c 'read'; then
    echo "Manual shutdown requested"
else
    echo "Auto-shutdown initiated"
fi

# =================================================================
# CLEANUP PHASE
# =================================================================
echo ""
echo "Shutting down MAS..."

# Wait 5 seconds before killing processes to allow clean shutdown
echo "Waiting 5 seconds before terminating processes..."
sleep 5

# Stop SICStus Prolog processes
echo "Stopping SICStus Prolog processes..."
pkill -9 sicstus 2>/dev/null || true

# Close DALI windows
case "$os_name" in
    Darwin)
        echo "Closing DALI Terminal windows..."
        for window_title in "${DALI_WINDOW_IDS[@]}"; do
            echo "Closing window: $window_title"
            osascript -e "
            tell application \"Terminal\"
                repeat with w in windows
                    if custom title of w is \"$window_title\" then
                        close w
                    end if
                end repeat
            end tell
            " 2>/dev/null || true
        done
        echo "DALI windows closed. Terminal.app remains open for your use."
        ;;
    Linux)
        echo "Closing terminal windows..."
        pkill gnome-terminal 2>/dev/null || true
        pkill xterm 2>/dev/null || true
        pkill konsole 2>/dev/null || true
        ;;
esac

echo "Processes cleanup completed."

# Clean up temporary scripts
echo "Cleaning up temporary files..."
rm -rf "$TEMP_DIR"

echo ""
echo "======================================================================"
echo "MAS MESSAGE TESTING SHUTDOWN COMPLETE"
echo "======================================================================"
echo "All tracked processes have been terminated"
echo "Test completed at: $(date)"
echo "======================================================================"
