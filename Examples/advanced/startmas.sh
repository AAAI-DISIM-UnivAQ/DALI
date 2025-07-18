#!/bin/bash

# Enable debugging
# set -x  # Start debugging

clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

# Print the current directory
echo "The current directory is: $current_dir"

# Reduce TIME_WAIT timeout based on OS
os_name=$(uname -s)

case "$os_name" in
    Darwin)
        echo "Operating system: macOS"
        current_msl=$(sysctl -n net.inet.tcp.msl)
        if [ "$current_msl" -gt 1000 ]; then
            echo "Reducing TIME_WAIT timeout for macOS from $current_msl to 1000..."
            sudo sysctl -w net.inet.tcp.msl=1000
        else
            echo "TIME_WAIT timeout already optimal: $current_msl"
        fi
        ;;
    Linux)
        echo "Operating system: Linux"
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
        echo "Unsupported operating system: $os_name"
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

# Define paths and variables
SICSTUS_HOME=/usr/local/sicstus4.6.0
DALI_HOME="../../src"
COMMUNICATION_DIR=$DALI_HOME
CONF_DIR=conf
PROLOG="$SICSTUS_HOME/bin/sicstus"
WAIT="ping -c 1 127.0.0.1"
INSTANCES_HOME=mas/instances
TYPES_HOME=mas/types
BUILD_HOME=build
TEMP_DIR="$current_dir/temp_scripts"

# Check if SICStus Prolog exists and is executable
if [[ -x "$PROLOG" ]]; then
  printf "SICStus Prolog found at %s\n" "$PROLOG"
else
  printf "Error: SICStus Prolog not found at %s or is not executable.\n" "$PROLOG" >&2
  exit -1
fi

# Create temporary directory for scripts
mkdir -p "$TEMP_DIR"

# Verify critical directories exist
for dir in "$INSTANCES_HOME" "$TYPES_HOME" "$BUILD_HOME" "$CONF_DIR"; do
    if [ ! -d "$dir" ]; then
        echo "Error: Directory $dir does not exist"
        exit -1
    fi
done

# Clean directories
rm -rf tmp/*
rm -rf build/*
rm -f work/*  # Remove agent history
rm -rf conf/mas/*
rm -rf "$TEMP_DIR"/*  # Clean temporary scripts

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
                # Save window title for cleanup
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
    
    # Increment position for next window (cascade/stack effect)
    window_x_pos=$((window_x_pos + window_offset))
    window_y_pos=$((window_y_pos + window_offset))
}

# Start the LINDA server
srvcmd="$PROLOG --noinfo -l $COMMUNICATION_DIR/active_server_wi.pl --goal go."
echo "Starting server with command: $srvcmd"
open_terminal "$srvcmd" "DALI Server"

sleep 2  # Increased sleep time

echo "Server ready. Starting the MAS..."
$WAIT > /dev/null  # Wait for a while

echo "Launching agents instances..."
# Launch agents in separate terminals
for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    echo "Agent: $agent_base"
    # Create the agent configuration
    if ! $current_dir/conf/makeconf.sh $agent_base $DALI_HOME; then
        echo "Error: Failed to create configuration for agent $agent_base"
        exit -1
    fi
    # Start the agent in a new terminal
    agent_cmd="$current_dir/conf/startagent.sh $agent_base $PROLOG $DALI_HOME"
    open_terminal "$agent_cmd" "DALI Agent: $agent_base"
    sleep 2  # Increased sleep time
    $WAIT > /dev/null  # Wait a bit before launching the next agent
done

# Start user agent in another terminal
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal user_interface."
open_terminal "$user_cmd" "DALI User Interface"

echo "MAS started."
echo "Press Enter to shutdown the MAS"
read

# Clean up processes
pkill -9 sicstus
pkill -9 xterm

# Terminal windows cleanup
case "$os_name" in
        Darwin)
            echo "Closing DALI Terminal windows..."
            # Close only DALI windows instead of closing all Terminal
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
            if command -v gnome-terminal &> /dev/null; then
                pkill -9 gnome-terminal
            elif command -v xterm &> /dev/null; then
                pkill xterm
            elif command -v konsole &> /dev/null; then
                pkill konsole
            else
                echo "Error: No supported terminal emulator found"
                exit 1
            fi
            ;;
    esac

# Clean up temporary scripts
echo "Cleaning up temporary files..."
rm -rf "$TEMP_DIR"


