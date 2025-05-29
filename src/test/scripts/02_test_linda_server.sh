#!/bin/bash
# src/test/scripts/02_test_linda_server.sh

# Set base path
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/../.."
cd "$BASE_DIR"

SICSTUS_HOME=/usr/local/sicstus4.6.0
SICSTUS=$SICSTUS_HOME/bin/sicstus

echo "Testing LINDA server..."

# Start the server
$SICSTUS --noinfo -l active_server_wi.pl --goal go. &
SERVER_PID=$!

# Wait for server to be ready
sleep 2

# Check if server is running
if ps -p $SERVER_PID > /dev/null; then
    echo "✓ LINDA server started successfully"
    
    # Check if server.txt was created
    if [ -f "server.txt" ]; then
        echo "✓ server.txt created successfully"
        cat server.txt
    else
        echo "✗ server.txt not created"
        kill $SERVER_PID
        exit 1
    fi
else
    echo "✗ LINDA server failed to start"
    exit 1
fi

# Terminate server
kill $SERVER_PID
echo "Test completed successfully!"