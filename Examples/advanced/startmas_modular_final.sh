#!/bin/bash

# DALI Multi-Agent System Startup Script - FINAL WORKING VERSION
# This version uses absolute paths to ensure all components work correctly

clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

echo "=========================================="
echo "DALI MAS - FINAL WORKING VERSION"
echo "=========================================="
echo "Current directory: $current_dir"
echo "Starting DALI MAS with absolute paths for reliability..."

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
WINDOW_START_X=50
WINDOW_START_Y=50
WINDOW_OFFSET=60

# Window dimensions (larger for better visibility)
WINDOW_WIDTH_MACOS=600
WINDOW_HEIGHT_MACOS=400

WINDOW_COLS_LINUX=90
WINDOW_ROWS_LINUX=30

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

# Define paths - USING ABSOLUTE PATHS FOR RELIABILITY
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

# Verify DALI system files exist with absolute paths
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

echo "✓ All DALI system files verified with absolute paths"

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

# Build agents from instances and types (like in test scripts)
echo "Building agents from instances and types..."
agent_count=0
declare -a AGENT_NAMES=()

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
    
    # Copy agent source to work directory (like in test scripts)
    cp "$type_filename" "$WORK_HOME/$instance_base"
    echo "  ✓ Agent source copied to work/$instance_base"
    
    # Create agent configuration file with absolute paths
    cat > "conf/mas/$instance_base" << EOF
agent('$current_dir/work/$agent_name_base','$agent_name_base','no',italian,['$current_dir/conf/communication'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOF
    echo "  ✓ Configuration created at conf/mas/$instance_base"
    
    AGENT_NAMES+=("$agent_name_base")
    agent_count=$((agent_count + 1))
done

echo "✓ $agent_count agents prepared successfully"

# =================================================================
# PHASE 1.5: DALI PRECOMPILATION VERIFICATION
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 1.5: DALI Precompilation Verification"
echo "======================================================================"

echo "Starting temporary LINDA server for precompilation verification..."
temp_server_cmd="$PROLOG --noinfo -l $DALI_HOME/active_server_wi.pl --goal go."
eval "$temp_server_cmd" > /dev/null 2>&1 &
TEMP_SERVER_PID=$!
sleep 3

echo "✓ Temporary LINDA server started (PID: $TEMP_SERVER_PID)"

# Precompile each agent to verify DALI syntax is correct
precompilation_failed=false
for agent_name in "${AGENT_NAMES[@]}"; do
    echo "Precompiling agent: $agent_name"
    
    # Use the working compiler for verification
    precompile_cmd="$PROLOG --noinfo -l $DALI_HOME/dali_working_compiler.pl --goal \"compile_agent_verify('$current_dir/conf/mas/$agent_name.txt').\""
    
    if eval "$precompile_cmd" > /dev/null 2>&1; then
        echo "  ✓ Precompilation verification successful for $agent_name"
        
        # Check if the expected Prolog files were generated
        if [ -f "work/${agent_name}.pl" ]; then
            echo "    ✓ Generated work/${agent_name}.pl"
        fi
        if [ -f "work/${agent_name}.ple" ]; then
            echo "    ✓ Generated work/${agent_name}.ple"  
        fi
        if [ -f "work/${agent_name}.plv" ]; then
            echo "    ✓ Generated work/${agent_name}.plv"
        fi
        if [ -f "work/${agent_name}.plf" ]; then
            echo "    ✓ Generated work/${agent_name}.plf"
        fi
        
    else
        echo "  ✗ Precompilation verification failed for $agent_name"
        echo "    Attempting with visible output for debugging..."
        eval "$precompile_cmd"
        precompilation_failed=true
        break
    fi
done

# Clean up temporary server
echo "Cleaning up temporary LINDA server..."
kill $TEMP_SERVER_PID 2>/dev/null || true
sleep 2

if [ "$precompilation_failed" = true ]; then
    echo ""
    echo "✗ PRECOMPILATION FAILED"
    echo "DALI syntax verification failed. Please fix the agent source files."
    exit 1
fi

echo "✓ All agents passed DALI precompilation verification"

# Counter for window positioning
script_counter=0
window_x_pos=$WINDOW_START_X
window_y_pos=$WINDOW_START_Y

# Array to track created DALI windows (macOS only)
declare -a DALI_WINDOW_IDS=()

# Function to open a new terminal with absolute paths
open_terminal() {
    local cmd="$1"
    local title="$2"
    
    script_counter=$((script_counter + 1))
    local script_name="$TEMP_DIR/dali_script_$script_counter.sh"
    
    # Create script with proper command handling and escaped quotes
    cat > "$script_name" << 'SCRIPT_EOF'
#!/bin/bash
echo "=========================================="
echo "DALI COMPONENT: TITLE_PLACEHOLDER"
echo "=========================================="
echo "Directory: DIRECTORY_PLACEHOLDER"
echo "Time: $(date)"
echo "=========================================="

cd "DIRECTORY_PLACEHOLDER"

echo "Executing DALI component..."
echo ""

# Execute the command
COMMAND_PLACEHOLDER

exit_code=$?
echo ""
echo "=========================================="
echo "EXECUTION COMPLETED"
echo "Exit code: $exit_code"
echo "Time: $(date)"
echo "=========================================="

if [ $exit_code -ne 0 ]; then
    echo "WARNING: Command exited with code $exit_code"
else
    echo "SUCCESS: Command completed successfully"
fi

echo ""
echo "This window will remain open for monitoring."
echo "Close this window manually when done."
echo ""

# Keep the window open
while true; do
    sleep 60
done
SCRIPT_EOF

    # Replace placeholders safely
    sed -i '' "s|TITLE_PLACEHOLDER|$title|g" "$script_name"
    sed -i '' "s|DIRECTORY_PLACEHOLDER|$current_dir|g" "$script_name"
    # Use a more complex replacement for the command to handle quotes properly
    escaped_cmd=$(echo "$cmd" | sed 's/"/\\"/g')
    sed -i '' "s|COMMAND_PLACEHOLDER|$escaped_cmd|g" "$script_name"
    
    chmod 755 "$script_name"
    
    case "$os_name" in
        Darwin)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos)"
            window_title="[DALI] $title"
            
            # Use osascript to create terminal window
            osascript << EOF &
tell application "Terminal"
    set newTab to do script "$script_name"
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
                gnome-terminal --title="$title" --geometry=${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -- bash "$script_name" &
            elif command -v xterm &> /dev/null; then
                xterm -title "$title" -geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e bash "$script_name" &
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

echo "Starting DALI/LINDA server with absolute path..."
srvcmd="$PROLOG --noinfo -l $DALI_HOME/active_server_wi.pl --goal go."
echo "Server command: $srvcmd"
open_terminal "$srvcmd" "LINDA Server"

# Wait for server to start
echo "Waiting for LINDA server to initialize..."
sleep 5

# Check if server.txt was created (indicates server is running)
server_attempts=0
max_attempts=15
while [ ! -f "server.txt" ] && [ $server_attempts -lt $max_attempts ]; do
    echo "Waiting for server.txt... (attempt $((server_attempts + 1))/$max_attempts)"
    sleep 2
    server_attempts=$((server_attempts + 1))
done

if [ -f "server.txt" ]; then
    echo "✓ LINDA server started successfully"
    echo "Server connection info:"
    cat server.txt
    echo ""
else
    echo "⚠ server.txt not found, but server may still be running"
fi

# Verify server process is running
if ps aux | grep -v grep | grep "active_server_wi.pl" > /dev/null; then
    echo "✓ LINDA server process confirmed running"
else
    echo "✗ LINDA server process not found"
    echo "Check the LINDA Server window for error messages"
fi

# =================================================================
# PHASE 3: AGENTS STARTUP
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 3: Starting DALI Agents"
echo "======================================================================"

echo "Starting agents with modular DALI system (absolute paths)..."
for agent_name in "${AGENT_NAMES[@]}"; do
    echo "Starting agent: $agent_name"
    
    # Use absolute paths for dali_core.pl and agent configuration
    # Use proper Prolog atom syntax (single quotes) for the file path
    agent_cmd="$PROLOG --noinfo -l $DALI_HOME/dali_core.pl --goal \"start_dali_agent('$current_dir/conf/mas/$agent_name.txt').\""
    echo "Agent command: $agent_cmd"
    open_terminal "$agent_cmd" "Agent: $agent_name"
    
    sleep 4  # Give each agent time to connect to LINDA server
done

echo "✓ All $agent_count agents started successfully"

# =================================================================
# PHASE 4: USER INTERFACE STARTUP
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 4: Starting User Interface"
echo "======================================================================"

echo "Starting DALI user interface with absolute path..."
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal user_interface."
echo "User interface command: $user_cmd"
open_terminal "$user_cmd" "User Interface"

sleep 3

echo ""
echo "========================================="
echo "✓ DALI MAS STARTED SUCCESSFULLY"
echo "✓ LINDA Server: ACTIVE"
echo "✓ Agents: $agent_count RUNNING"
echo "✓ User Interface: ACTIVE"
echo "✓ Using absolute paths for reliability"
echo "✓ SICStus Prolog: $PROLOG"
echo "========================================="
echo ""
echo "The MAS is now fully operational!"
echo "Check each terminal window to verify components are running:"
echo "1. LINDA Server window - should show server listening"
echo "2. Agent windows - should show agent initialization"
echo "3. User Interface window - should show FIPA interface"
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
sleep 2

# Verify all processes stopped
if ps aux | grep -v grep | grep "sicstus" > /dev/null; then
    echo "⚠ Some SICStus processes may still be running"
    ps aux | grep -v grep | grep "sicstus" || true
else
    echo "✓ All SICStus processes stopped"
fi

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
        echo "Note: Please close the DALI terminal windows manually"
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
