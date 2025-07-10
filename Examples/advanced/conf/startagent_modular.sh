#!/bin/bash
# Modular DALI Agent Startup Script
# Parameters: $1 = agent_name, $2 = sicstus_path, $3 = dali_home

# set -x  # Enable for debugging

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Agent name and paths
AGENT_NAME="$1"
SICSTUS_PATH="$2"
DALI_HOME="$3"
AGENT_CONFIG="$SCRIPT_DIR/mas/$AGENT_NAME"

echo "Starting modular DALI agent: $AGENT_NAME"
echo "  - Config file: $AGENT_CONFIG"
echo "  - SICStus: $SICSTUS_PATH"
echo "  - DALI Home: $DALI_HOME"

# Check if agent config file exists
if [ ! -f "$AGENT_CONFIG" ]; then
    echo "Error: Agent configuration file not found: $AGENT_CONFIG"
    exit 1
fi

# For now, use the standard DALI system (active_dali_wi.pl)
# In the future, this could be replaced with a modular version
eval "$SICSTUS_PATH --noinfo -l $DALI_HOME/active_dali_wi.pl --goal \"start0('$AGENT_CONFIG').\"" 