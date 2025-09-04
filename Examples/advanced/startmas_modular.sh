#!/bin/bash

# DALI Multi-Agent System Startup Script - Modular Architecture Version
# Updated to use only project files and avoid external paths

# Enable debugging
# set -x  # Start debugging

clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

# Print the current directory
echo "The current directory is: $current_dir"
echo "Starting DALI MAS with Modular Architecture (Project-only version)..."

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
# Initial window positioning
WINDOW_START_X=100
WINDOW_START_Y=100
WINDOW_OFFSET=50

# Window dimensions
# macOS: dimensions in pixels (width x height)
WINDOW_WIDTH_MACOS=450
WINDOW_HEIGHT_MACOS=325

# Linux: dimensions in characters (columns x rows)
WINDOW_COLS_LINUX=80
WINDOW_ROWS_LINUX=25
# =================================================================

# Auto-detect SICStus Prolog installation
echo "Auto-detecting SICStus Prolog installation..."
PROLOG=""

# Common SICStus installation paths
SICSTUS_PATHS=(
    "/usr/local/sicstus4.6.0/bin/sicstus"
    "/usr/local/sicstus/bin/sicstus"
    "/opt/sicstus/bin/sicstus"
    "/Applications/SICStus Prolog 4.6.0/bin/sicstus"
    "sicstus"  # Try from PATH
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

# Define paths and variables - PROJECT-ONLY PATHS
DALI_HOME="../../src"
DALI_MODULAR_HOME="$DALI_HOME"  # Points to new modular structure
COMMUNICATION_DIR=$DALI_HOME
CONF_DIR=conf
WAIT="ping -c 1 127.0.0.1"
INSTANCES_HOME=mas/instances
TYPES_HOME=mas/types
BUILD_HOME=build
TEMP_DIR="$current_dir/temp_scripts"  # Use project directory instead of /tmp

# Create temporary directory for scripts (in project folder)
mkdir -p "$TEMP_DIR"

# Cleaup old files
rm -rf build/*
rm -f work/*  # Remove agent history
rm -rf conf/mas/*
rm -rf "$TEMP_DIR"/*  # Clean our temporary scripts

# Verify critical directories exist
for dir in "$INSTANCES_HOME" "$TYPES_HOME" "$BUILD_HOME" "$CONF_DIR"; do
    if [ ! -d "$dir" ]; then
        echo "Error: Directory $dir does not exist"
        exit 1
    fi
done

# Verify modular DALI structure exists
if [ ! -f "$DALI_MODULAR_HOME/dali_core.pl" ]; then
    echo "Error: Modular DALI core not found at $DALI_MODULAR_HOME/dali_core.pl"
    echo "Please ensure the modular DALI system is properly installed"
    exit 1
fi

echo "Modular DALI system found at $DALI_MODULAR_HOME"

# Clean directories
rm -rf tmp/*
rm -rf build/*
rm -f work/*  # Remove agent history
rm -rf conf/mas/*
rm -rf "$TEMP_DIR"/*  # Clean our temporary scripts

mkdir -p build work conf/mas # fox issue #73

# Build agents based on instances
for instance_filename in $INSTANCES_HOME/*.txt; do
    if [ ! -f "$instance_filename" ]; then
        echo "Error: No instance files found in $INSTANCES_HOME"
        exit -1
    fi
    type=$(<$instance_filename)  # Agent type name is the content of the instance file
    type_filename="$TYPES_HOME/$type.txt"
    if [ ! -f "$type_filename" ]; then
        echo "Error: Type file $type_filename not found"
        exit -1
    fi
    echo "Instance: " $instance_filename " of type: " $type_filename
    instance_base="${instance_filename##*/}"  # Extract instance base name
    cat "$type_filename" > "$BUILD_HOME/$instance_base"
    chmod 755 "$BUILD_HOME/$instance_base"
done

echo "Built agents:"
ls -l $BUILD_HOME
cp $BUILD_HOME/*.txt work
chmod 755 work/*.txt

# Counter for unique script names
script_counter=0

# Variables for stacked window positioning (initialized from constants)
window_x_pos=$WINDOW_START_X
window_y_pos=$WINDOW_START_Y
window_offset=$WINDOW_OFFSET

# Array to track created DALI windows (macOS only)
declare -a DALI_WINDOW_IDS=()



# Improved function to open a new terminal based on OS with stacked window positioning
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
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos, size ${WINDOW_WIDTH_MACOS}x${WINDOW_HEIGHT_MACOS})"
            if command -v osascript &> /dev/null; then
                # Create a new Terminal window with DALI marker and position it
                window_title="[DALI] $title"
                osascript -e "
                tell application \"Terminal\"
                    set newTab to do script \"cd '$current_dir' && $cmd\"
                    delay 0.5
                    set custom title of front window to \"$window_title\"
                    set bounds of front window to {$window_x_pos, $window_y_pos, $((window_x_pos + WINDOW_WIDTH_MACOS)), $((window_y_pos + WINDOW_HEIGHT_MACOS))}
                end tell
                " &
                # Save the window title for cleanup
                DALI_WINDOW_IDS+=("$window_title")
            else
                # Fallback: open Terminal with the script
                open -a Terminal "$script_name" &
            fi
            ;;
        Linux)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos, size ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX})"
            if command -v gnome-terminal &> /dev/null; then
                gnome-terminal --title="$title" --geometry=${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -- bash -c "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            elif command -v xterm &> /dev/null; then
                xterm -title "$title" -geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            elif command -v konsole &> /dev/null; then
                konsole --title "$title" --geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e bash -c "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            else
                echo "Error: No supported terminal emulator found"
                echo "Please install gnome-terminal, xterm, or konsole"
                exit 1
            fi
            ;;
    esac
    
    # Increment position for next window (stacking/cascade effect)
    window_x_pos=$((window_x_pos + window_offset))
    window_y_pos=$((window_y_pos + window_offset))
}

# Start the LINDA server
srvcmd="$PROLOG --noinfo -l $COMMUNICATION_DIR/active_server_wi.pl --goal go."
echo "Starting server with command: $srvcmd"
open_terminal "$srvcmd" "DALI Server"

sleep 2  # Increased sleep time

echo "Server ready. Starting the MAS with Modular Architecture..."
$WAIT > /dev/null  # Wait for a while

echo "Launching agent instances using modular DALI system..."
# Launch agents in separate terminals - UPDATED FOR MODULAR ARCHITECTURE
for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    echo "Agent: $agent_base (using modular architecture)"
    # Create the agent configuration
    if ! $current_dir/conf/makeconf.sh $agent_base $DALI_HOME; then
        echo "Error: Failed to create configuration for agent $agent_base"
        exit -1
    fi
    # Start the agent in a new terminal using the NEW MODULAR SYSTEM
    agent_cmd="$current_dir/conf/startagent_modular.sh $agent_base $PROLOG $DALI_MODULAR_HOME"
    open_terminal "$agent_cmd" "DALI Agent (Modular): $agent_base"
    sleep 2  # Increased sleep time
    $WAIT > /dev/null  # Wait a bit before launching the next agent
done

echo "✓ All agents started successfully"

# Start user agent in another terminal
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal user_interface."
open_terminal "$user_cmd" "DALI User Interface"

echo "Modular MAS started successfully!"
echo "Using new modular DALI architecture"
echo "Core system: $DALI_MODULAR_HOME/dali_core.pl"
echo "SICStus Prolog: $PROLOG"
echo "Temporary scripts: $TEMP_DIR"
echo ""
echo "Press Enter to shutdown the MAS"
read

# Clean up processes
echo "Shutting down MAS..."

# Stop SICStus Prolog processes
echo "Stopping SICStus Prolog processes..."
pkill -9 sicstus 2>/dev/null || true

# Close only DALI terminals - improved approach
case "$os_name" in
        Darwin)
            echo "Closing DALI Terminal windows..."
            # Close only DALI windows instead of closing entire Terminal
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

echo "MAS shutdown complete"
echo "All tracked processes have been terminated" 