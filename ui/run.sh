#!/bin/bash
# Starts the DALI Agent Dashboard.
# Creates a local venv on first run, then reuses it.
# Works on Debian/Ubuntu even without python3-full / python3-venv installed.

set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV="$SCRIPT_DIR/.venv"
PYTHON="$VENV/bin/python"

#Create venv 
if [[ ! -f "$PYTHON" ]]; then
    echo "Creating virtual environment..."
    python3 -m venv "$VENV" || {
        echo ""
        echo "ERROR: python3-venv is missing. Fix with:"
        echo "  sudo apt install python3-venv python3-full"
        exit 1
    }
fi

# Bootstrap pip if missing (minimal Debian installs omit it) 
if ! "$PYTHON" -m pip --version &>/dev/null; then
    echo "Bootstrapping pip..."
    "$PYTHON" -m ensurepip --upgrade 2>/dev/null || {
        echo "ensurepip not available, downloading pip..."
        curl -sS https://bootstrap.pypa.io/get-pip.py | "$PYTHON"
    }
fi

# Install dependencies (only when requirements.txt changed) 
STAMP="$VENV/.installed_stamp"
if [[ ! -f "$STAMP" || "$SCRIPT_DIR/requirements.txt" -nt "$STAMP" ]]; then
    echo "Installing dependencies..."
    "$PYTHON" -m pip install -q -r "$SCRIPT_DIR/requirements.txt"
    touch "$STAMP"
fi

# Launch 
exec "$PYTHON" "$SCRIPT_DIR/dashboard.py" "$@"
