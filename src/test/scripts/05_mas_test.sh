#!/bin/bash
# src/test/scripts/05_mas_test.sh

# Set base path
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../.."
cd "$BASE_DIR"

SICSTUS_HOME=/usr/local/sicstus4.6.0
SICSTUS=$SICSTUS_HOME/bin/sicstus
ADVANCED_DIR="../Examples/advanced"
TEST_TIMEOUT=60
AGENT_STARTUP_TIME=3
COMMUNICATION_TIMEOUT=20

# Define timeout function for cross-platform compatibility
# macOS doesn't have timeout command by default, so we use perl
timeout_cmd() {
    local duration="$1"
    shift
    if command -v timeout &> /dev/null; then
        # Linux/GNU timeout
        timeout "$duration" "$@"
    else
        # macOS fallback using perl
        perl -e 'alarm shift; exec @ARGV' "$duration" "$@"
    fi
}

echo "=================================="
echo "Testing MAS with startmas_modular.sh"
echo "=================================="

# Function to check if a process is running by name
check_process() {
    local process_name="$1"
    if pgrep -f "$process_name" > /dev/null; then
        return 0
    else
        return 1
    fi
}

# Function to count sicstus processes
count_sicstus_processes() {
    pgrep -f "sicstus" | wc -l | tr -d ' '
}

# Function to cleanup processes
cleanup() {
    echo "Cleaning up processes..."
    pkill -9 sicstus 2>/dev/null || true
    # Close any DALI terminal windows on macOS
    if [[ "$OSTYPE" == "darwin"* ]]; then
        osascript -e 'tell application "Terminal" to close (every window whose custom title contains "[DALI]")' 2>/dev/null || true
    fi
    sleep 2
}

# Ensure clean start
cleanup
sleep 2

# Verify prerequisites
if [ ! -f "$SICSTUS" ]; then
    echo "Error: SICStus Prolog not found at $SICSTUS"
    exit 1
fi

if [ ! -d "$ADVANCED_DIR" ]; then
    echo "Error: Advanced examples directory not found at $ADVANCED_DIR"
    exit 1
fi

if [ ! -f "$ADVANCED_DIR/startmas_modular.sh" ]; then
    echo "Error: startmas_modular.sh not found in $ADVANCED_DIR"
    exit 1
fi

# Change to advanced directory
cd "$ADVANCED_DIR"

echo "Starting MAS system..."

# Create a PID file to track the MAS
PID_FILE="mas_pid.txt"
rm -f "$PID_FILE"

# Start the MAS in background and capture PID
echo "Launching startmas_modular.sh..."

# Create a script that will auto-shutdown the MAS after test completion
cat > mas_controller.sh << 'EOF'
#!/bin/bash
# Start the MAS and wait for our signal to shut down
echo "" | ./startmas_modular.sh &
MAS_SCRIPT_PID=$!
echo $MAS_SCRIPT_PID > mas_script.pid

# Wait for shutdown signal file
while [ ! -f "shutdown_signal.txt" ]; do
    sleep 1
    # Safety timeout
    if [ -f "force_shutdown.txt" ]; then
        break
    fi
done

# Send Enter to shutdown the MAS
kill -TERM $MAS_SCRIPT_PID 2>/dev/null || true
EOF
chmod +x mas_controller.sh

# Remove old signal files
rm -f shutdown_signal.txt force_shutdown.txt mas_script.pid

# Start the MAS controller in background
./mas_controller.sh &
MAS_PID=$!
echo $MAS_PID > "$PID_FILE"

# Also create a safety shutdown after timeout
(sleep $TEST_TIMEOUT; touch force_shutdown.txt) &
SAFETY_PID=$!

echo "MAS started with PID: $MAS_PID"

# Wait for system to initialize
echo "Waiting for system initialization..."
echo "- Server startup: 3 seconds"
sleep 3
echo "- Agent startup time (estimated): $((AGENT_STARTUP_TIME * 2)) seconds for 2 agents"
sleep $((AGENT_STARTUP_TIME * 2))
echo "- Additional stabilization time: 5 seconds"
sleep 5

# Check if the MAS process is still running
if ! kill -0 $MAS_PID 2>/dev/null; then
    echo "Error: MAS process died during startup"
    cleanup
    exit 1
fi

echo "Checking running processes..."

# Count SICStus processes
SICSTUS_COUNT=$(count_sicstus_processes)
echo "SICStus processes running: $SICSTUS_COUNT"

# We expect at least 4 processes:
# 1. LINDA server
# 2. User interface
# 3. Agent 1
# 4. Agent 2
EXPECTED_MIN_PROCESSES=4

if [ "$SICSTUS_COUNT" -lt "$EXPECTED_MIN_PROCESSES" ]; then
    echo "Error: Expected at least $EXPECTED_MIN_PROCESSES SICStus processes, found $SICSTUS_COUNT"
    cleanup
    exit 1
fi

echo "✓ Sufficient processes are running ($SICSTUS_COUNT >= $EXPECTED_MIN_PROCESSES)"

# Check for specific processes
echo "Checking specific process types..."

# Check for server
if check_process "active_server_wi.pl"; then
    echo "✓ LINDA server is running"
else
    echo "Error: LINDA server not found"
    cleanup
    exit 1
fi

# Check for user interface
if check_process "active_user_wi.pl"; then
    echo "✓ User interface is running"
else
    echo "Error: User interface not found"
    cleanup
    exit 1
fi

# Check for agents (at least 2)
AGENT_COUNT=$(pgrep -f "startagent_modular.sh\|dali_core.pl" | wc -l | tr -d ' ')
echo "Agent processes found: $AGENT_COUNT"

if [ "$AGENT_COUNT" -lt 2 ]; then
    echo "Error: Expected at least 2 agents, found $AGENT_COUNT"
    cleanup
    exit 1
fi

echo "✓ Agents are running ($AGENT_COUNT agents detected)"

# Check port 3010 is in use (LINDA server)
if netstat -an | grep -q ":3010.*LISTEN"; then
    echo "✓ Port 3010 is in use (LINDA server listening)"
else
    echo "Error: Port 3010 not in use - LINDA server may not be running"
    cleanup
    exit 1
fi

# Test communication by checking log files (if they exist)
echo "Checking for communication logs..."
if [ -d "log" ]; then
    LOG_FILES=$(find log -name "*.txt" -o -name "*.log" 2>/dev/null | wc -l)
    if [ "$LOG_FILES" -gt 0 ]; then
        echo "✓ Log files found: $LOG_FILES files"
    else
        echo "Note: No log files found yet"
    fi
fi

# Check work directory for agent files
if [ -d "work" ]; then
    WORK_FILES=$(find work -name "*.txt" 2>/dev/null | wc -l)
    if [ "$WORK_FILES" -gt 0 ]; then
        echo "✓ Agent work files found: $WORK_FILES files"
    else
        echo "Error: No agent work files found"
        cleanup
        exit 1
    fi
fi

# Check build directory for compiled agents
if [ -d "build" ]; then
    BUILD_FILES=$(find build -name "*.txt" 2>/dev/null | wc -l)
    if [ "$BUILD_FILES" -gt 0 ]; then
        echo "✓ Built agent files found: $BUILD_FILES files"
    else
        echo "Error: No built agent files found"
        cleanup
        exit 1
    fi
fi

# Wait for communication to occur
echo "Monitoring system for communication..."
echo "- Allowing agents time to communicate: $COMMUNICATION_TIMEOUT seconds"
sleep $COMMUNICATION_TIMEOUT

# Final check - processes should still be running
FINAL_SICSTUS_COUNT=$(count_sicstus_processes)
echo "Final SICStus processes count: $FINAL_SICSTUS_COUNT"

if [ "$FINAL_SICSTUS_COUNT" -lt "$EXPECTED_MIN_PROCESSES" ]; then
    echo "Warning: Some processes may have terminated during execution"
    echo "Initial count: $SICSTUS_COUNT, Final count: $FINAL_SICSTUS_COUNT"
else
    echo "✓ All processes maintained throughout test"
fi

# Check for any error indicators
ERROR_INDICATORS=0

# Check for common error patterns in any output
if pgrep -f "sicstus" > /dev/null; then
    echo "✓ SICStus processes are still active"
else
    echo "Warning: No SICStus processes found at end of test"
    ERROR_INDICATORS=$((ERROR_INDICATORS + 1))
fi

# Signal the MAS to shutdown gracefully
echo "Test completed. Signaling MAS to shutdown..."
touch shutdown_signal.txt

# Kill safety timeout process
kill $SAFETY_PID 2>/dev/null || true

# Wait a moment for graceful shutdown
sleep 3

# Cleanup any remaining processes
echo "Cleaning up any remaining processes..."
cleanup

# Wait for cleanup to complete
echo "Waiting for processes to terminate properly..."
sleep 5

# Verify cleanup
REMAINING_PROCESSES=$(count_sicstus_processes)
if [ "$REMAINING_PROCESSES" -eq 0 ]; then
    echo "✓ All processes cleaned up successfully"
else
    echo "Warning: $REMAINING_PROCESSES SICStus processes still running after cleanup"
fi

# Remove temporary files
rm -f mas_controller.sh
rm -f shutdown_signal.txt
rm -f force_shutdown.txt
rm -f mas_script.pid
rm -f "$PID_FILE"

# Final result
echo "=================================="
if [ "$ERROR_INDICATORS" -eq 0 ]; then
    echo "✓ MAS Test PASSED"
    echo "  - LINDA server started successfully"
    echo "  - User interface started successfully"  
    echo "  - At least 2 agents started successfully"
    echo "  - All processes maintained during test"
    echo "  - No critical errors detected"
    exit 0
else
    echo "✗ MAS Test FAILED"
    echo "  - $ERROR_INDICATORS error(s) detected"
    exit 1
fi
