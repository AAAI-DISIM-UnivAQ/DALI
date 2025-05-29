#!/bin/bash
# src/test/scripts/03_test_basic_agent.sh

# Set base path
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../.."
cd "$BASE_DIR"

SICSTUS_HOME=/usr/local/sicstus4.6.0
SICSTUS=$SICSTUS_HOME/bin/sicstus

echo "Testing basic agent..."

# Create test directory structure
mkdir -p test/work
mkdir -p test/conf/mas
mkdir -p test/conf

# Copy the communication file from Examples/advanced/conf
if [ -f "../Examples/advanced/conf/communication.con" ]; then
    cp "../Examples/advanced/conf/communication.con" "test/conf/"
    echo "✓ Copied communication.con from Examples/advanced/conf"
else
    echo "✗ communication.con not found in ../Examples/advanced/conf"
    exit 1
fi

# Create agent configuration file with correct paths
cat > test/conf/mas/agent1.txt << EOL
agent('test/work/agent1','agent1','no',italian,['test/conf/communication'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOL

# Create agent program file
cat > test/work/agent1.txt << EOL
t60.
testE:>write('Test completed successfully!'),nl.
EOL

# Start the server
$SICSTUS --noinfo -l active_server_wi.pl --goal go. &
SERVER_PID=$!

# Wait for server to be ready
sleep 2

# Start the agent
$SICSTUS --noinfo -l active_dali_wi.pl --goal "start0('test/conf/mas/agent1.txt')." &
AGENT_PID=$!

# Wait for agent to be ready
sleep 2

# Check if agent is running
if ps -p $AGENT_PID > /dev/null; then
    echo "✓ Agent started successfully"

else
    echo "✗ Agent failed to start"
    kill $SERVER_PID
    exit 1
fi

# Terminate processes
pkill sicstus

echo "Test completed!"