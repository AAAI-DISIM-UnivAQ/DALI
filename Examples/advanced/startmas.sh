#!/bin/bash

# Enable debugging
# set -x  # Start debugging

clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

# Print the current directory
echo "The current directory is: $current_dir"

# Test if tmux is installed
if command -v tmux &> /dev/null; then
    echo "tmux is installed."
    tmux -V  # Display tmux version
else
    echo "TMUX is a requirement in Unix-like OS to run DALI"
    echo "tmux is not installed."
    echo "Check installation instructions at https://github.com/tmux/tmux/wiki/Installing"
    exit -1
fi

# Create or attach to the tmux session
# tmux new-session -d -s DALI_session top

# Define paths and variables
SICSTUS_HOME=/usr/local/sicstus4.6.0

DALI_HOME="../../"
CORE_DIR="$DALI_HOME/src"
COMMUNICATION_DIR="$DALI_HOME/src"
EVENT_DIR="$DALI_HOME/src"
UTILS_DIR="$DALI_HOME/src"

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

# Clean directories
rm -rf tmp/*
rm -rf build/*
rm -f work/*  # Remove agent history
rm -rf conf/mas/*

# Build agents based on instances
for instance_filename in $INSTANCES_HOME/*.txt; do
    type=$(<$instance_filename)  # Agent type name is the content of the instance file
    type_filename="$TYPES_HOME/$type.txt"
    echo "Instance: " $instance_filename " of type: " $type_filename
    instance_base="${instance_filename##*/}"  # Extract instance base name
    cat "$type_filename" >> "$BUILD_HOME/$instance_base"
done

ls $BUILD_HOME
cp $BUILD_HOME/*.txt work

# Start the LINDA server in a new console
srvcmd="$PROLOG --noinfo -l $DALI_HOME/active_server_wi.pl --goal go(3010,'server.txt')."
echo "server: $srvcmd"

tmux new-session -d -s DALI_session "$srvcmd"

sleep 1

echo "Server ready. Starting the MAS..."
$WAIT > /dev/null  # Wait for a while

echo "Launching agents instances..."
# Launch agents in horizontal splits, one after the other
for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    echo "Agent: $agent_base"
    # Create the agent configuration
    $current_dir/conf/makeconf.sh $agent_base $DALI_HOME
    # Start the agent in the new pane
    echo split-window -v -t DALI_session "$current_dir/conf/startagent.sh $agent_base $PROLOG $DALI_HOME"
    tmux split-window -v -t DALI_session "$current_dir/conf/startagent.sh $agent_base $PROLOG $DALI_HOME"
    sleep 1
    $WAIT > /dev/null  # Wait a bit before launching the next agent
done

# Start user agent in another vertical split
% tmux split-window -v -t DALI_session "$PROLOG --noinfo -l $COMMUNICATION_DIR/user_console.pl --goal 'initialize_client,client_loop.'"

echo "MAS started."

# Select an even layout to properly display the panes
tmux select-layout -t DALI_session tiled

# Attach to the session so you can see everything
tmux attach -t DALI_session

echo "Press Enter to shutdown the MAS"
read

# Clean up processes
killall -9 sicstus

