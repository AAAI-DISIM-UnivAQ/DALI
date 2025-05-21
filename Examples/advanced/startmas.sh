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
        echo "Sistema operativo: macOS"
        current_msl=$(sysctl -n net.inet.tcp.msl)
        if [ "$current_msl" -gt 1000 ]; then
            echo "Reducing TIME_WAIT timeout for macOS from $current_msl to 1000..."
            sudo sysctl -w net.inet.tcp.msl=1000
        else
            echo "TIME_WAIT timeout already optimal: $current_msl"
        fi
        ;;
    Linux)
        echo "Sistema operativo: Linux"
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
        echo "Sistema operativo non supportato: $os_name"
        exit 1
        ;;
esac

# Wait for port 3010 to be free
echo "Waiting to have DALI Server Port 3010 free to use..."
while netstat -an | grep -q "3010"; do
    sleep 1
done
echo "Port 3010 is now free, proceeding with DALI startup..."

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

# Check if SICStus Prolog exists and is executable
if [[ -x "$PROLOG" ]]; then
  printf "SICStus Prolog found at %s\n" "$PROLOG"
else
  printf "Error: SICStus Prolog not found at %s or is not executable.\n" "$PROLOG" >&2
  exit -1
fi

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

# Funzione per aprire un nuovo terminale in base all'OS
open_terminal() {
    local cmd="$1"
    local title="$2"
    echo "#!/bin/bash" > /tmp/runmas.sh
    echo "echo \"$title\"" >> /tmp/runmas.sh
    echo "cd \"$current_dir\"" >> /tmp/runmas.sh
    echo "$cmd" >> /tmp/runmas.sh
    chmod 755 /tmp/runmas.sh
    case "$os_name" in
        Darwin)
            echo 
            open -a Terminal "/tmp/runmas.sh"
            ;;
        Linux)
            if command -v gnome-terminal &> /dev/null; then
                gnome-terminal --title="$title" -- bash -c "cd '$current_dir' && $cmd; exec bash"
            elif command -v xterm &> /dev/null; then
                xterm -title "$title" -e "cd '$current_dir' && $cmd; exec bash" &
            else
                echo "Error: No supported terminal emulator found"
                exit 1
            fi
            ;;
    esac
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
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal utente."
open_terminal "$user_cmd" "DALI User Interface"

echo "MAS started."
echo "Press Enter to shutdown the MAS"
read

# Clean up processes
pkill -9 sicstus
pkill -9 xterm

case "$os_name" in
        Darwin)
            echo 
            osascript -e 'tell application "Terminal" to quit'
            ;;
        Linux)
            if command -v gnome-terminal &> /dev/null; then
                pkill -9 gnome-terminal
            elif command -v xterm &> /dev/null; then
                pkill xterm
            else
                echo "Error: No supported terminal emulator found"
                exit 1
            fi
            ;;
    esac


