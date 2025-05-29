#!/bin/bash
# src/test/scripts/04_test_agent_communication.sh

# Set base path
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../.."
cd "$BASE_DIR"

SICSTUS_HOME=/usr/local/sicstus4.6.0
SICSTUS=$SICSTUS_HOME/bin/sicstus

echo "Testing agent communication..."

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

# Create agent configuration files
for i in 1 2; do
    cat > test/conf/mas/agent$i.txt << EOL
agent('test/work/agent$i','agent$i','no',italian,['test/conf/communication'],['communication_fipa','learning','planasp'],'no','onto/dali_onto.txt',[]).
EOL
done

# Create agent program files
cat > test/work/agent1.txt << EOL
t60.
helloE :> write('Agent1 received: '),write("hello"),nl.
EOL

cat > test/work/agent2.txt << EOL
:- dynamic start_agent/0.
t60.
start_agentI:- messageA(agent1, send_message(hello, agent2)).
EOL

echo "Waiting the 3010 port to be free..."
while netstat -an | grep -q "3010"; do
    sleep 1
done

echo "Port 3010 is free, starting the server..."

# Start the server
$SICSTUS --noinfo -l active_server_wi.pl --goal go. &
SERVER_PID=$!

# Wait for server to be ready
sleep 2

# Start agents
for i in 1 2; do
    $SICSTUS --noinfo -l active_dali_wi.pl --goal "start0('test/conf/mas/agent$i.txt')." &
    AGENT_PIDS[$i]=$!
    sleep 2
done

# Check if agents are running
for i in 1 2; do
    if ps -p ${AGENT_PIDS[$i]} > /dev/null; then
        echo "✓ Agent$i started successfully"
    else
        echo "✗ Agent$i failed to start"
        kill $SERVER_PID
        for j in 1 2; do
            kill ${AGENT_PIDS[$j]} 2>/dev/null
        done
        exit 1
    fi
done

# Wait for communication
sleep 5

# Terminate processes
pkill sicstus


echo "Test completed!"