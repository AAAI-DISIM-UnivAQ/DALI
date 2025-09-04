#!/bin/bash
echo "Starting MAS in background..."

# Start the MAS without sending early termination input
echo "Starting MAS without premature termination..."
echo "The MAS will run independently and we'll monitor it externally"

# Start the MAS in background - let it run without sending input
./startmas_modular.sh > mas_output.log 2>&1 &
MAS_PID=$!
echo "MAS started with PID: $MAS_PID"

# Store the PID so we can terminate it later
echo $MAS_PID > mas_pid.txt

echo "MAS is running independently - monitoring will be done externally"

