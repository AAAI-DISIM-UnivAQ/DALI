#!/bin/bash

# Test script generation with quotes
current_dir=$(pwd)
TEMP_DIR="$current_dir/temp_scripts"
mkdir -p "$TEMP_DIR"

# Test command with quotes (simulating the agent command)
test_cmd='/usr/local/sicstus4.6.0/bin/sicstus --noinfo -l /Users/giodegas/ai/DALI_2024/DALI/src/dali_core.pl --goal "start_dali_agent('\''./conf/mas/agent1.txt'\'')."'

echo "Testing script generation with command:"
echo "$test_cmd"
echo ""

# Generate test script
script_name="$TEMP_DIR/test_script.sh"

cat > "$script_name" << 'SCRIPT_EOF'
#!/bin/bash
echo "=========================================="
echo "DALI COMPONENT: TITLE_PLACEHOLDER"
echo "=========================================="
echo "Directory: DIRECTORY_PLACEHOLDER"
echo "Time: $(date)"
echo "=========================================="

cd "DIRECTORY_PLACEHOLDER"

echo "Executing DALI component..."
echo ""

# Execute the command
COMMAND_PLACEHOLDER

exit_code=$?
echo ""
echo "=========================================="
echo "EXECUTION COMPLETED"
echo "Exit code: $exit_code"
echo "Time: $(date)"
echo "=========================================="

if [ $exit_code -ne 0 ]; then
    echo "WARNING: Command exited with code $exit_code"
else
    echo "SUCCESS: Command completed successfully"
fi

echo ""
echo "Script completed."
SCRIPT_EOF

# Replace placeholders
sed -i '' "s|TITLE_PLACEHOLDER|Test Agent|g" "$script_name"
sed -i '' "s|DIRECTORY_PLACEHOLDER|$current_dir|g" "$script_name"
escaped_cmd=$(echo "$test_cmd" | sed 's/"/\\"/g')
sed -i '' "s|COMMAND_PLACEHOLDER|$escaped_cmd|g" "$script_name"

chmod 755 "$script_name"

echo "Generated script content:"
echo "========================="
cat "$script_name"
echo "========================="
echo ""

echo "Testing script syntax:"
if bash -n "$script_name"; then
    echo "✓ Script syntax is correct!"
else
    echo "✗ Script syntax error!"
fi

# Clean up
rm -f "$script_name"
