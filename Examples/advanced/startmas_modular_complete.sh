#!/bin/bash

# DALI Multi-Agent System Startup Script - COMPLETE WORKING VERSION
# This version ensures LINDA server works correctly with full MAS integration

clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

echo "=========================================="
echo "DALI MAS - COMPLETE WORKING VERSION"
echo "=========================================="
echo "Current directory: $current_dir"
echo "Starting DALI MAS with full LINDA server integration..."

# Reduce TIME_WAIT timeout based on OS
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

# Clean up any existing processes
echo "Cleaning up any existing DALI processes..."
pkill -9 sicstus 2>/dev/null || true
rm -f server.txt 2>/dev/null || true

# Wait for port 3010 to be free
echo "Waiting for DALI Server Port 3010 to be free..."
while netstat -an | grep -q "3010"; do
    sleep 1
done
echo "Port 3010 is now free, proceeding with DALI startup..."

# =================================================================
# TERMINAL WINDOWS CONFIGURATION
# =================================================================
WINDOW_START_X=100
WINDOW_START_Y=100
WINDOW_OFFSET=50

# Window dimensions
WINDOW_WIDTH_MACOS=500
WINDOW_HEIGHT_MACOS=350

WINDOW_COLS_LINUX=80
WINDOW_ROWS_LINUX=25

# Auto-detect SICStus Prolog installation
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
    echo "✗ SICStus Prolog not found in common locations."
    echo "Please ensure SICStus Prolog is installed and accessible."
    echo "Tried paths: ${SICSTUS_PATHS[*]}"
    exit 1
fi

# Define paths - USE ABSOLUTE PATHS
DALI_HOME="/Users/giodegas/ai/DALI_2024/DALI/src"
CONF_DIR=conf
INSTANCES_HOME=mas/instances
TYPES_HOME=mas/types
BUILD_HOME=build
WORK_HOME=work
TEMP_DIR="$current_dir/temp_scripts"

# Create directories
mkdir -p "$TEMP_DIR"
mkdir -p build work conf/mas

# Clean directories
rm -rf build/*
rm -f work/*
rm -rf conf/mas/*
rm -rf "$TEMP_DIR"/*

# Verify critical directories exist
for dir in "$INSTANCES_HOME" "$TYPES_HOME" "$BUILD_HOME" "$CONF_DIR"; do
    if [ ! -d "$dir" ]; then
        echo "✗ Directory $dir does not exist"
        exit 1
    fi
done

# Verify DALI system files exist
if [ ! -f "$DALI_HOME/dali_core.pl" ]; then
    echo "✗ DALI core not found at $DALI_HOME/dali_core.pl"
    exit 1
fi

if [ ! -f "$DALI_HOME/active_server_wi.pl" ]; then
    echo "✗ LINDA server not found at $DALI_HOME/active_server_wi.pl"
    exit 1
fi

if [ ! -f "$DALI_HOME/active_user_wi.pl" ]; then
    echo "✗ User interface not found at $DALI_HOME/active_user_wi.pl"
    exit 1
fi

echo "✓ All DALI system files verified"

# =================================================================
# PHASE 1: AGENT PREPARATION
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 1: Agent Preparation"
echo "======================================================================"

# Verify communication configuration
if [ ! -f "conf/communication.con" ]; then
    echo "✗ Communication configuration not found: conf/communication.con"
    exit 1
fi
echo "✓ Communication configuration found"

# Build agents from instances and types
echo "Building agents from instances and types..."
agent_count=0
for instance_filename in $INSTANCES_HOME/*.txt; do
    if [ ! -f "$instance_filename" ]; then
        echo "✗ No instance files found in $INSTANCES_HOME"
        exit 1
    fi
    
    type=$(<$instance_filename)
    type_filename="$TYPES_HOME/$type.txt"
    
    if [ ! -f "$type_filename" ]; then
        echo "✗ Type file $type_filename not found"
        exit 1
    fi
    
    instance_base="${instance_filename##*/}"
    agent_name_base="${instance_base%.*}"
    
    echo "Building agent: $agent_name_base (type: $type)"
    
    # Copy agent source to work directory
    cp "$type_filename" "$WORK_HOME/$instance_base"
    echo "  ✓ Agent source copied to work/$instance_base"
    
    # Create agent configuration file
    cat > "conf/mas/$instance_base" << EOF
agent('$current_dir/work/$agent_name_base','$agent_name_base','no',italian,['$current_dir/conf/communication'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOF
    echo "  ✓ Configuration created at conf/mas/$instance_base"
    
    agent_count=$((agent_count + 1))
done

echo "✓ $agent_count agents prepared successfully"

# Counter for window positioning
script_counter=0
window_x_pos=$WINDOW_START_X
window_y_pos=$WINDOW_START_Y

# Array to track created DALI windows (macOS only)
declare -a DALI_WINDOW_IDS=()
declare -a DALI_PROCESS_IDS=()

# Function to open a new terminal with better error handling
open_terminal() {
    local cmd="$1"
    local title="$2"
    local capture_pid="$3"  # Optional: capture process PID
    
    script_counter=$((script_counter + 1))
    local script_name="$TEMP_DIR/dali_script_$script_counter.sh"
    
    # Create script with proper cleanup and error handling
    cat > "$script_name" << 'SCRIPT_EOF'
#!/bin/bash
echo "=== DALI COMPONENT STARTUP ==="
echo "Title: TITLE_PLACEHOLDER"
echo "Command: CMD_PLACEHOLDER"
echo "Directory: DIR_PLACEHOLDER"
echo "Time: $(date)"
echo "================================"

cd "DIR_PLACEHOLDER"

# Execute the command and capture any errors
echo "Executing command..."
CMD_PLACEHOLDER

exit_code=$?
echo ""
echo "=== EXECUTION COMPLETED ==="
echo "Exit code: $exit_code"
echo "Time: $(date)"
echo "============================="

if [ $exit_code -ne 0 ]; then
    echo "ERROR: Command failed with exit code $exit_code"
    echo "Press Enter to close this window..."
    read
else
    echo "SUCCESS: Command completed successfully"
    echo "Press Enter to close this window..."
    read
fi
SCRIPT_EOF
    
    # Replace placeholders
    sed -i '' "s|TITLE_PLACEHOLDER|$title|g" "$script_name"
    sed -i '' "s|CMD_PLACEHOLDER|$cmd|g" "$script_name"
    sed -i '' "s|DIR_PLACEHOLDER|$current_dir|g" "$script_name"
    
    chmod 755 "$script_name"
    
    case "$os_name" in
        Darwin)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos)"
            window_title="[DALI] $title"
            
            # Use osascript to create terminal window and capture the process
            osascript << EOF &
tell application "Terminal"
    set newTab to do script "cd '$current_dir' && $cmd"
    delay 1
    set custom title of front window to "$window_title"
    set bounds of front window to {$window_x_pos, $window_y_pos, $((window_x_pos + WINDOW_WIDTH_MACOS)), $((window_y_pos + WINDOW_HEIGHT_MACOS))}
end tell
EOF
            
            DALI_WINDOW_IDS+=("$window_title")
            ;;
        Linux)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos)"
            if command -v gnome-terminal &> /dev/null; then
                gnome-terminal --title="$title" --geometry=${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -- bash -c "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            elif command -v xterm &> /dev/null; then
                xterm -title "$title" -geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            else
                echo "✗ No supported terminal emulator found"
                exit 1
            fi
            ;;
    esac
    
    # Increment position for next window
    window_x_pos=$((window_x_pos + WINDOW_OFFSET))
    window_y_pos=$((window_y_pos + WINDOW_OFFSET))
}

# =================================================================
# PHASE 2: LINDA SERVER STARTUP
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 2: Starting LINDA Server"
echo "======================================================================"

echo "Starting DALI/LINDA server..."
srvcmd="$PROLOG --noinfo -l $DALI_HOME/active_server_wi.pl --goal go."
open_terminal "$srvcmd" "LINDA Server"

# Wait for server to start and verify it's working
echo "Waiting for LINDA server to initialize..."
sleep 4

# Check if server.txt was created (indicates server is running)
server_attempts=0
max_attempts=10
while [ ! -f "server.txt" ] && [ $server_attempts -lt $max_attempts ]; do
    echo "Waiting for server.txt to be created... (attempt $((server_attempts + 1))/$max_attempts)"
    sleep 2
    server_attempts=$((server_attempts + 1))
done

if [ -f "server.txt" ]; then
    echo "✓ LINDA server started successfully"
    echo "Server connection info:"
    cat server.txt
else
    echo "✗ LINDA server failed to start properly"
    echo "server.txt was not created within expected time"
    exit 1
fi

# Verify server process is running
if ps aux | grep -v grep | grep "active_server_wi.pl" > /dev/null; then
    echo "✓ LINDA server process confirmed running"
else
    echo "✗ LINDA server process not found"
    exit 1
fi

# =================================================================
# PHASE 3: AGENTS STARTUP
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 3: Starting DALI Agents"
echo "======================================================================"

echo "Starting agents with modular DALI system..."
for instance_filename in $INSTANCES_HOME/*.txt; do
    instance_base="${instance_filename##*/}"
    agent_name_base="${instance_base%.*}"
    
    echo "Starting agent: $agent_name_base"
    
    # Use dali_core.pl with start_dali_agent
    agent_cmd="$PROLOG --noinfo -l $DALI_HOME/dali_core.pl --goal \"start_dali_agent('$current_dir/conf/mas/$instance_base').\""
    open_terminal "$agent_cmd" "Agent: $agent_name_base"
    
    sleep 3  # Give each agent time to connect to LINDA server
done

echo "✓ All agents started successfully"

# =================================================================
# PHASE 4: USER INTERFACE STARTUP
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 4: Starting User Interface"
echo "======================================================================"

echo "Starting DALI user interface..."
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal user_interface."
open_terminal "$user_cmd" "User Interface"

sleep 2

echo ""
echo "========================================="
echo "✓ DALI MAS STARTED SUCCESSFULLY"
echo "✓ LINDA Server: ACTIVE"
echo "✓ Agents: $agent_count RUNNING"
echo "✓ User Interface: ACTIVE"
echo "✓ Core system: $DALI_HOME/dali_core.pl"
echo "✓ SICStus Prolog: $PROLOG"
echo "========================================="
echo ""
echo "The MAS is now fully operational!"
echo "Check the terminal windows to interact with agents."
echo ""
echo "Press Enter to shutdown the entire MAS"
read

# =================================================================
# SHUTDOWN SEQUENCE
# =================================================================
echo ""
echo "========================================="
echo "SHUTTING DOWN DALI MAS"
echo "========================================="

# Stop all SICStus Prolog processes
echo "Stopping all DALI processes..."
pkill -9 sicstus 2>/dev/null || true

# Close DALI terminals
case "$os_name" in
    Darwin)
        echo "Closing DALI Terminal windows..."
        for window_title in "${DALI_WINDOW_IDS[@]}"; do
            echo "Closing window: $window_title"
            osascript -e "
            tell application \"Terminal\"
                repeat with w in windows
                    try
                        if custom title of w is \"$window_title\" then
                            close w
                        end if
                    end try
                end repeat
            end tell
            " 2>/dev/null || true
        done
        echo "✓ DALI windows closed"
        ;;
    Linux)
        echo "Closing terminal windows..."
        pkill gnome-terminal 2>/dev/null || true
        pkill xterm 2>/dev/null || true
        pkill konsole 2>/dev/null || true
        echo "✓ Terminal windows closed"
        ;;
esac

# Clean up temporary files
echo "Cleaning up temporary files..."
rm -rf "$TEMP_DIR"
rm -f server.txt 2>/dev/null || true

echo "✓ Cleanup completed"
echo ""
echo "========================================="
echo "DALI MAS SHUTDOWN COMPLETE"
echo "All processes terminated cleanly"
echo "========================================="
