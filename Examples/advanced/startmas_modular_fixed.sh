#!/bin/bash

# DALI Multi-Agent System Startup Script - FIXED MODULAR VERSION
# Based on working test scripts approach

clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

echo "=========================================="
echo "DALI MAS - FIXED MODULAR VERSION"
echo "=========================================="
echo "The current directory is: $current_dir"
echo "Starting DALI MAS with Modular Architecture (FIXED version)..."

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

# Wait for port 3010 to be free
echo "Waiting to have DALI Server Port 3010 free to use..."
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
WINDOW_WIDTH_MACOS=450
WINDOW_HEIGHT_MACOS=325

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
        echo "SICStus Prolog found at: $PROLOG"
        break
    fi
done

if [ -z "$PROLOG" ]; then
    echo "Error: SICStus Prolog not found in common locations."
    echo "Please ensure SICStus Prolog is installed and accessible."
    echo "Tried paths: ${SICSTUS_PATHS[*]}"
    exit 1
fi

# Define paths - SIMPLIFIED APPROACH
DALI_HOME="../../src"
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
        echo "Error: Directory $dir does not exist"
        exit 1
    fi
done

# Verify modular DALI structure exists
if [ ! -f "$DALI_HOME/dali_core.pl" ]; then
    echo "Error: Modular DALI core not found at $DALI_HOME/dali_core.pl"
    exit 1
fi

echo "✓ Modular DALI system found at $DALI_HOME"

# =================================================================
# SIMPLIFIED AGENT PREPARATION (like in test scripts)
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 1: Agent Preparation (Simplified Approach)"
echo "======================================================================"

# Copy communication configuration
if [ -f "conf/communication.con" ]; then
    echo "✓ Communication configuration found"
else
    echo "Error: conf/communication.con not found"
    exit 1
fi

# Build agents based on instances (copy agent types to work directory)
echo "Building agents from instances and types..."
for instance_filename in $INSTANCES_HOME/*.txt; do
    if [ ! -f "$instance_filename" ]; then
        echo "Error: No instance files found in $INSTANCES_HOME"
        exit 1
    fi
    
    type=$(<$instance_filename)
    type_filename="$TYPES_HOME/$type.txt"
    
    if [ ! -f "$type_filename" ]; then
        echo "Error: Type file $type_filename not found"
        exit 1
    fi
    
    instance_base="${instance_filename##*/}"
    agent_name_base="${instance_base%.*}"
    
    echo "Building agent: $instance_base (type: $type)"
    
    # Copy agent source to work directory
    cp "$type_filename" "$WORK_HOME/$instance_base"
    
    # Create agent configuration file (like in test scripts)
    cat > "conf/mas/$instance_base" << EOF
agent('$current_dir/work/$agent_name_base','$agent_name_base','no',italian,['$current_dir/conf/communication'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOF
    
    echo "  ✓ Agent source copied to work/$instance_base"
    echo "  ✓ Configuration created at conf/mas/$instance_base"
done

echo "✓ All agents prepared successfully"

# Counter for window positioning
script_counter=0
window_x_pos=$WINDOW_START_X
window_y_pos=$WINDOW_START_Y

# Array to track created DALI windows (macOS only)
declare -a DALI_WINDOW_IDS=()

# Function to open a new terminal
open_terminal() {
    local cmd="$1"
    local title="$2"
    script_counter=$((script_counter + 1))
    local script_name="$TEMP_DIR/dali_script_$script_counter.sh"
    
    # Create script with proper cleanup
    echo "#!/bin/bash" > "$script_name"
    echo "echo \"$title\"" >> "$script_name"
    echo "cd \"$current_dir\"" >> "$script_name"
    echo "echo \"Starting: $cmd\"" >> "$script_name"
    echo "$cmd" >> "$script_name"
    echo "echo \"Process finished. Press Enter to close this window...\"" >> "$script_name"
    echo "read" >> "$script_name"
    chmod 755 "$script_name"
    
    case "$os_name" in
        Darwin)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos)"
            window_title="[DALI] $title"
            osascript -e "
            tell application \"Terminal\"
                set newTab to do script \"cd '$current_dir' && $cmd\"
                delay 0.5
                set custom title of front window to \"$window_title\"
                set bounds of front window to {$window_x_pos, $window_y_pos, $((window_x_pos + WINDOW_WIDTH_MACOS)), $((window_y_pos + WINDOW_HEIGHT_MACOS))}
            end tell
            " &
            DALI_WINDOW_IDS+=("$window_title")
            ;;
        Linux)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos)"
            if command -v gnome-terminal &> /dev/null; then
                gnome-terminal --title="$title" --geometry=${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -- bash -c "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            elif command -v xterm &> /dev/null; then
                xterm -title "$title" -geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            else
                echo "Error: No supported terminal emulator found"
                exit 1
            fi
            ;;
    esac
    
    # Increment position for next window
    window_x_pos=$((window_x_pos + WINDOW_OFFSET))
    window_y_pos=$((window_y_pos + WINDOW_OFFSET))
}

# =================================================================
# PHASE 2: SYSTEM STARTUP (Simplified)
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 2: Starting DALI Components (Simplified)"
echo "======================================================================"

# Start the LINDA server
echo "Starting DALI/LINDA server..."
srvcmd="$PROLOG --noinfo -l $DALI_HOME/active_server_wi.pl --goal go."
open_terminal "$srvcmd" "DALI Server"

sleep 3  # Wait for server to start

echo "✓ DALI/LINDA server started"

# Start agents using the MODULAR SYSTEM (like test scripts)
echo "Starting agents with modular DALI system..."
for instance_filename in $INSTANCES_HOME/*.txt; do
    instance_base="${instance_filename##*/}"
    agent_name_base="${instance_base%.*}"
    
    echo "Starting agent: $agent_name_base"
    
    # Use dali_core.pl directly with start_dali_agent (like test scripts)
    agent_cmd="$PROLOG --noinfo -l $DALI_HOME/dali_core.pl --goal \"start_dali_agent('$current_dir/conf/mas/$instance_base').\""
    open_terminal "$agent_cmd" "DALI Agent: $agent_name_base"
    
    sleep 2
done

# Start user interface
echo "Starting user interface..."
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal user_interface."
open_terminal "$user_cmd" "DALI User Interface"

echo ""
echo "========================================="
echo "✓ DALI MAS STARTED SUCCESSFULLY"
echo "✓ Using simplified modular approach"
echo "✓ Core system: $DALI_HOME/dali_core.pl"
echo "✓ SICStus Prolog: $PROLOG"
echo "========================================="
echo ""
echo "Press Enter to shutdown the MAS"
read

# Cleanup
echo "Shutting down MAS..."

# Stop SICStus Prolog processes
pkill -9 sicstus 2>/dev/null || true

# Close DALI terminals
case "$os_name" in
    Darwin)
        echo "Closing DALI Terminal windows..."
        for window_title in "${DALI_WINDOW_IDS[@]}"; do
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
        ;;
    Linux)
        pkill gnome-terminal 2>/dev/null || true
        pkill xterm 2>/dev/null || true
        ;;
esac

# Clean up temporary files
rm -rf "$TEMP_DIR"

echo "✓ MAS shutdown complete"
