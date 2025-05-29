#!/bin/bash
# src/test/scripts/run_tests.sh

# Set base path
BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPTS_DIR="$BASE_DIR"

echo "Running DALI test suite..."
echo "=========================="

# Make all scripts executable
chmod +x "$SCRIPTS_DIR"/*.sh

# Run tests in sequence
for script in "$SCRIPTS_DIR"/*.sh; do
    if [ "$(basename "$script")" != "run_tests.sh" ]; then
        echo -e "\nRunning $(basename "$script")..."
        echo "------------------------"
        
        if "$script"; then
            echo "✓ $(basename "$script") completed successfully"
        else
            echo "✗ $(basename "$script") failed"
            exit 1
        fi
    fi
done

echo -e "\n=========================="
echo "All tests completed successfully!"