#!/bin/bash
# src/test/scripts/01_check_prerequisites.sh

echo "Checking DALI prerequisites..."
SICSTUS_HOME=/usr/local/sicstus4.6.0
SICSTUS=$SICSTUS_HOME/bin/sicstus

# Check SICStus Prolog
if command -v $SICSTUS &> /dev/null; then
    echo "SICStus Prolog installed"
    $SICSTUS --version
else
    echo "SICStus Prolog not found"
    exit 1
fi

# Check required ports
echo "Checking port 3010..."
if netstat -an | grep -q "3010"; then
    echo "Port 3010 already in use"
    exit 1
else
    echo "Port 3010 available"
fi

echo "All prerequisites verified successfully!"