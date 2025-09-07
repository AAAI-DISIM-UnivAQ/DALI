#!/bin/bash

# DALI Multi-Agent System Startup Script - Modular Architecture Version
# Updated to use only project files and avoid external paths

# Enable debugging
# set -x  # Start debugging

clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

# Print the current directory
echo "The current directory is: $current_dir"
echo "Starting DALI MAS with Modular Architecture (Project-only version)..."

# Reduce TIME_WAIT timeout based on OS
os_name=$(uname -s)

case "$os_name" in
    Darwin)
        echo "Operating System: macOS"
        current_msl=$(sysctl -n net.inet.tcp.msl)
        if [ "$current_msl" -gt 1000 ]; then
            echo "Reducing TIME_WAIT timeout for macOS from $current_msl to 1000..."
            sudo sysctl -w net.inet.tcp.msl=1000
        else
            echo "TIME_WAIT timeout already optimal: $current_msl"
        fi
        ;;
    Linux)
        echo "Operating System: Linux"
        current_timeout=$(sysctl -n net.ipv4.tcp_fin_timeout)
        if [ "$current_timeout" -gt 30 ]; then
            echo "Reducing TIME_WAIT timeout for Linux from $current_timeout to 30..."
            sudo sysctl -w net.ipv4.tcp_fin_timeout=30
            sudo sysctl -w net.ipv4.tcp_tw_reuse=1
        else
            echo "TIME_WAIT timeout already optimal: $current_timeout"
        fi
        ;;
    *)
        echo "Operating System not supported: $os_name"
        exit 1
        ;;
esac

# Wait for port 3010 to be free
echo "Waiting to have DALI Server Port 3010 free to use..."
while netstat -an | grep -q "3010"; do
    sleep 1
done
echo "Port 3010 is now free, proceeding with DALI startup..."

# =================================================================
# TERMINAL WINDOWS CONFIGURATION
# =================================================================
# Initial window positioning
WINDOW_START_X=100
WINDOW_START_Y=100
WINDOW_OFFSET=50

# Window dimensions
# macOS: dimensions in pixels (width x height)
WINDOW_WIDTH_MACOS=450
WINDOW_HEIGHT_MACOS=325

# Linux: dimensions in characters (columns x rows)
WINDOW_COLS_LINUX=80
WINDOW_ROWS_LINUX=25
# =================================================================

# Auto-detect SICStus Prolog installation
echo "Auto-detecting SICStus Prolog installation..."
PROLOG=""

# Common SICStus installation paths
SICSTUS_PATHS=(
    "/usr/local/sicstus4.6.0/bin/sicstus"
    "/usr/local/sicstus/bin/sicstus"
    "/opt/sicstus/bin/sicstus"
    "/Applications/SICStus Prolog 4.6.0/bin/sicstus"
    "sicstus"  # Try from PATH
)

for path in "${SICSTUS_PATHS[@]}"; do
    if command -v "$path" &> /dev/null; then
        PROLOG="$path"
        echo "SICStus Prolog found at: $PROLOG"
        break
    fi
done

if [ -z "$PROLOG" ]; then
    echo "Error: SICStus Prolog not found in common locations."
    echo "Please ensure SICStus Prolog is installed and accessible."
    echo "Tried paths: ${SICSTUS_PATHS[*]}"
    exit 1
fi

# Define paths and variables - PROJECT-ONLY PATHS
DALI_HOME="../../src"
DALI_MODULAR_HOME="$DALI_HOME"  # Points to new modular structure
COMMUNICATION_DIR=$DALI_HOME
CONF_DIR=conf
WAIT="ping -c 1 127.0.0.1"
INSTANCES_HOME=mas/instances
TYPES_HOME=mas/types
BUILD_HOME=build
TEMP_DIR="$current_dir/temp_scripts"  # Use project directory instead of /tmp

# Create temporary directory for scripts (in project folder)
mkdir -p "$TEMP_DIR"

# Cleaup old files
rm -rf build/*
rm -f work/*  # Remove agent history
rm -rf conf/mas/*
rm -rf "$TEMP_DIR"/*  # Clean our temporary scripts

# Verify critical directories exist
for dir in "$INSTANCES_HOME" "$TYPES_HOME" "$BUILD_HOME" "$CONF_DIR"; do
    if [ ! -d "$dir" ]; then
        echo "Error: Directory $dir does not exist"
        exit 1
    fi
done

# Verify modular DALI structure exists
if [ ! -f "$DALI_MODULAR_HOME/dali_core.pl" ]; then
    echo "Error: Modular DALI core not found at $DALI_MODULAR_HOME/dali_core.pl"
    echo "Please ensure the modular DALI system is properly installed"
    exit 1
fi

echo "Modular DALI system found at $DALI_MODULAR_HOME"

# Clean directories
rm -rf tmp/*
rm -rf build/*
rm -f work/*  # Remove agent history
rm -rf conf/mas/*
rm -rf "$TEMP_DIR"/*  # Clean our temporary scripts

mkdir -p build work conf/mas # fox issue #73

# Build agents based on instances
for instance_filename in $INSTANCES_HOME/*.txt; do
    if [ ! -f "$instance_filename" ]; then
        echo "Error: No instance files found in $INSTANCES_HOME"
        exit -1
    fi
    type=$(<$instance_filename)  # Agent type name is the content of the instance file
    type_filename="$TYPES_HOME/$type.txt"
    if [ ! -f "$type_filename" ]; then
        echo "Error: Type file $type_filename not found"
        exit -1
    fi
    echo "Instance: " $instance_filename " of type: " $type_filename
    instance_base="${instance_filename##*/}"  # Extract instance base name
    cat "$type_filename" > "$BUILD_HOME/$instance_base"
    chmod 755 "$BUILD_HOME/$instance_base"
done

echo "Built agents:"
ls -l $BUILD_HOME
cp $BUILD_HOME/*.txt work
chmod 755 work/*.txt

# Counter for unique script names
script_counter=0

# Variables for stacked window positioning (initialized from constants)
window_x_pos=$WINDOW_START_X
window_y_pos=$WINDOW_START_Y
window_offset=$WINDOW_OFFSET

# Array to track created DALI windows (macOS only)
declare -a DALI_WINDOW_IDS=()



# Improved function to open a new terminal based on OS with stacked window positioning
open_terminal() {
    local cmd="$1"
    local title="$2"
    script_counter=$((script_counter + 1))
    local script_name="$TEMP_DIR/dali_script_$script_counter.sh"
    
    # Create script with proper cleanup
    echo "#!/bin/bash" > "$script_name"
    echo "echo \"$title\"" >> "$script_name"
    echo "cd \"$current_dir\"" >> "$script_name"
    echo "echo \"Starting: $cmd\"" >> "$script_name"
    echo "$cmd" >> "$script_name"
    echo "echo \"Process finished. Press Enter to close this window...\"" >> "$script_name"
    echo "read" >> "$script_name"
    chmod 755 "$script_name"
    
    case "$os_name" in
        Darwin)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos, size ${WINDOW_WIDTH_MACOS}x${WINDOW_HEIGHT_MACOS})"
            if command -v osascript &> /dev/null; then
                # Create a new Terminal window with DALI marker and position it
                window_title="[DALI] $title"
                osascript -e "
                tell application \"Terminal\"
                    set newTab to do script \"cd '$current_dir' && $cmd\"
                    delay 0.5
                    set custom title of front window to \"$window_title\"
                    set bounds of front window to {$window_x_pos, $window_y_pos, $((window_x_pos + WINDOW_WIDTH_MACOS)), $((window_y_pos + WINDOW_HEIGHT_MACOS))}
                end tell
                " &
                # Save the window title for cleanup
                DALI_WINDOW_IDS+=("$window_title")
            else
                # Fallback: open Terminal with the script
                open -a Terminal "$script_name" &
            fi
            ;;
        Linux)
            echo "Starting: $title (positioned at $window_x_pos,$window_y_pos, size ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX})"
            if command -v gnome-terminal &> /dev/null; then
                gnome-terminal --title="$title" --geometry=${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -- bash -c "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            elif command -v xterm &> /dev/null; then
                xterm -title "$title" -geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            elif command -v konsole &> /dev/null; then
                konsole --title "$title" --geometry ${WINDOW_COLS_LINUX}x${WINDOW_ROWS_LINUX}+$window_x_pos+$window_y_pos -e bash -c "cd '$current_dir' && $cmd; echo 'Process finished. Press Enter to close...'; read" &
            else
                echo "Error: No supported terminal emulator found"
                echo "Please install gnome-terminal, xterm, or konsole"
                exit 1
            fi
            ;;
    esac
    
    # Increment position for next window (stacking/cascade effect)
    window_x_pos=$((window_x_pos + window_offset))
    window_y_pos=$((window_y_pos + window_offset))
}

# Function to check Prolog syntax
check_prolog_syntax() {
    local file="$1"
    local file_type="$2"
    
    if [ -f "$file" ]; then
        echo "Checking syntax of $file_type file: $file"
        
        # Use SICStus to check syntax without execution
        # Create a temporary output file to capture detailed error messages
        local temp_output="$TEMP_DIR/syntax_check_$$.tmp"
        
        if ! "$PROLOG" --noinfo --goal "catch((consult('$file'), write('SYNTAX_OK')), Error, (write('SYNTAX_ERROR: '), write(Error), nl)), halt." > "$temp_output" 2>&1; then
            echo "✗ Syntax error detected in $file"
            echo "Error details:"
            cat "$temp_output"
            echo ""
            echo "File content:"
            echo "----------------------------------------"
            cat "$file"
            echo "----------------------------------------"
            echo ""
            echo "Please fix the syntax errors before proceeding."
            echo "Common issues:"
            echo "- Missing dots (.) at the end of clauses"
            echo "- Malformed predicates"
            echo "- Incorrect operator usage"
            rm -f "$temp_output"
            return 1
        elif grep -q "SYNTAX_ERROR" "$temp_output"; then
            echo "✗ Syntax error detected in $file"
            echo "Error details:"
            cat "$temp_output"
            echo ""
            echo "File content:"
            echo "----------------------------------------"
            cat "$file" 
            echo "----------------------------------------"
            echo ""
            rm -f "$temp_output"
            return 1
        else
            echo "✓ Syntax check passed for $file"
            rm -f "$temp_output"
            return 0
        fi
    else
        echo "Warning: File $file not found for syntax check"
        return 1
    fi
}

# =================================================================
# PHASE 1: COMPLETE SYSTEM VALIDATION
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 1: System Validation (Pre-startup checks)"
echo "======================================================================"

# First, validate all agent configurations and generate files
echo "Validating agent configurations and generating Prolog files..."
validation_failed=false

for agent_filename in $BUILD_HOME/*; do
    if [ ! -f "$agent_filename" ]; then
        echo "Error: No agent files found in $BUILD_HOME"
        exit 1
    fi
    
    agent_base="${agent_filename##*/}"
    echo "Validating agent: $agent_base"
    
    # Create the agent configuration
    if ! $current_dir/conf/makeconf.sh $agent_base $DALI_HOME; then
        echo "FATAL ERROR: Failed to create configuration for agent $agent_base"
        validation_failed=true
        break
    fi
    
    echo "✓ Configuration file created for $agent_base"
    
    # Use the legacy system for REAL compilation with temporary LINDA server
    echo "Running REAL DALI compilation for $agent_base using legacy system..."
    agent_config="$current_dir/conf/mas/$agent_base"
    
    # Start temporary LINDA server for compilation if not running
    if ! netstat -an | grep -q "3010"; then
        echo "Starting temporary LINDA server for compilation..."
        "$PROLOG" --noinfo -l "$COMMUNICATION_DIR/active_server_wi.pl" --goal go. > /dev/null 2>&1 &
        TEMP_SERVER_PID=$!
        sleep 2
        echo "✓ Temporary LINDA server started (PID: $TEMP_SERVER_PID)"
        NEED_CLEANUP=true
    else
        echo "✓ LINDA server already running"
        NEED_CLEANUP=false
    fi
    
    # Use modified legacy system for REAL compilation (terminates after compilation)
    echo "Compiling with modified legacy DALI system..."
    if ! "$PROLOG" --noinfo -l "$DALI_MODULAR_HOME/dali_legacy_compiler.pl" --goal compile_with_legacy\(\'$agent_config\'\). > /dev/null 2>&1; then
        echo "Warning: Legacy DALI compilation failed for $agent_base"
        echo "This may indicate issues with the agent source file or configuration"
        # Try again with visible output for debugging
        echo "Attempting compilation with visible output for debugging..."
        "$PROLOG" -l "$DALI_MODULAR_HOME/dali_legacy_compiler.pl" --goal compile_with_legacy\(\'$agent_config\'\).
    fi
    
    # Clean up temporary server if we started it
    if [ "$NEED_CLEANUP" = true ] && [ ! -z "$TEMP_SERVER_PID" ]; then
        echo "Cleaning up temporary LINDA server (PID: $TEMP_SERVER_PID)..."
        kill $TEMP_SERVER_PID 2>/dev/null || true
        sleep 1
    fi
    
    # Check if all required files were generated by the full compiler
    agent_name_base="${agent_base%.*}"
    agent_pl_file="work/${agent_name_base}.pl"
    agent_ple_file="work/${agent_name_base}.ple"
    agent_plv_file="work/${agent_name_base}.plv"
    agent_plf_file="work/${agent_name_base}.plf"
    
    echo "Checking generated files for $agent_base:"
    
    if [ ! -f "$agent_pl_file" ]; then
        echo "FATAL ERROR: Agent program file not generated: $agent_pl_file"
        validation_failed=true
        break
    fi
    echo "  ✓ $agent_pl_file"
    
    if [ ! -f "$agent_ple_file" ]; then
        echo "FATAL ERROR: Agent events file not generated: $agent_ple_file"
        validation_failed=true
        break
    fi
    echo "  ✓ $agent_ple_file"
    
    if [ ! -f "$agent_plv_file" ]; then
        echo "FATAL ERROR: Agent variables file not generated: $agent_plv_file"
        validation_failed=true
        break
    fi
    echo "  ✓ $agent_plv_file"
    
    if [ ! -f "$agent_plf_file" ]; then
        echo "FATAL ERROR: Agent directives file not generated: $agent_plf_file"
        validation_failed=true
        break
    fi
    echo "  ✓ $agent_plf_file"
    
    echo "✓ All required Prolog files generated for $agent_base"
done

# Clean up temporary server if still running from compilation phase
if [ "$NEED_CLEANUP" = true ] && [ ! -z "$TEMP_SERVER_PID" ]; then
    echo "Final cleanup of temporary LINDA server (PID: $TEMP_SERVER_PID)..."
    kill $TEMP_SERVER_PID 2>/dev/null || true
    sleep 1
fi

# If configuration generation failed, exit before starting anything
if [ "$validation_failed" = true ]; then
    echo ""
    echo "VALIDATION FAILED: Cannot proceed with MAS startup"
    echo "Fix the configuration issues before retrying."
    exit 1
fi

echo ""
echo "Performing syntax validation for all generated files..."

# Now check syntax of all generated files
for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    echo "Performing syntax checks for agent $agent_base..."
    
    # Check the main agent .pl file
    agent_pl_file="work/${agent_base%.*}.pl"
    if ! check_prolog_syntax "$agent_pl_file" "agent program"; then
        echo "FATAL ERROR: Syntax check failed for $agent_pl_file"
        echo "Cannot proceed with MAS startup due to syntax errors."
        echo ""
        echo "Please fix the syntax errors in the generated Prolog files before retrying."
        echo "This usually indicates a problem with the DALI code generation process."
        exit 1
    fi
    
    # Check other generated files if they exist - also critical for agent functionality
    agent_plv_file="work/${agent_base%.*}.plv"
    if [ -f "$agent_plv_file" ]; then
        if ! check_prolog_syntax "$agent_plv_file" "agent variables"; then
            echo "FATAL ERROR: Syntax check failed for $agent_plv_file"
            echo "Cannot proceed with MAS startup due to syntax errors in agent variables file."
            exit 1
        fi
    fi
    
    agent_ple_file="work/${agent_base%.*}.ple"
    if [ -f "$agent_ple_file" ]; then
        if ! check_prolog_syntax "$agent_ple_file" "agent events"; then
            echo "FATAL ERROR: Syntax check failed for $agent_ple_file"
            echo "Cannot proceed with MAS startup due to syntax errors in agent events file."
            exit 1
        fi
    fi
    
    echo "✓ All syntax checks passed for agent $agent_base"
done

echo ""
echo "========================================="
echo "✓ ALL VALIDATIONS PASSED"
echo "✓ Ready to start DALI MAS components"
echo "========================================="

# =================================================================
# PHASE 2: SYSTEM STARTUP
# =================================================================
echo ""
echo "======================================================================"
echo "PHASE 2: Starting DALI Components"
echo "======================================================================"

# Start the LINDA server
srvcmd="$PROLOG --noinfo -l $COMMUNICATION_DIR/active_server_wi.pl --goal go."
echo "Starting DALI/LINDA server with command: $srvcmd"
open_terminal "$srvcmd" "DALI Server"

sleep 2  # Increased sleep time

echo "✓ DALI/LINDA server started successfully"
echo "Starting MAS agents with Modular Architecture..."
$WAIT > /dev/null  # Wait for a while

# Launch agents in separate terminals - VALIDATION ALREADY COMPLETED
echo "Launching validated agent instances using modular DALI system..."
for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    echo "Starting agent: $agent_base (modular architecture)"
    
    # Start the agent in a new terminal using the NEW MODULAR SYSTEM
    # (Configuration and syntax validation already completed in Phase 1)
    agent_cmd="$current_dir/conf/startagent_modular.sh $agent_base $PROLOG $DALI_MODULAR_HOME"
    open_terminal "$agent_cmd" "DALI Agent (Modular): $agent_base"
    sleep 2  # Increased sleep time
    $WAIT > /dev/null  # Wait a bit before launching the next agent
done

echo ""
echo "========================================="
echo "✓ ALL AGENTS STARTED SUCCESSFULLY"
echo "========================================="

# Start user agent in another terminal
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal user_interface."
open_terminal "$user_cmd" "DALI User Interface"

echo "Modular MAS started successfully!"
echo "Using new modular DALI architecture"
echo "Core system: $DALI_MODULAR_HOME/dali_core.pl"
echo "SICStus Prolog: $PROLOG"
echo "Temporary scripts: $TEMP_DIR"
echo ""
echo "Press Enter to shutdown the MAS"
read

# Clean up processes
echo "Shutting down MAS..."

# Stop SICStus Prolog processes
echo "Stopping SICStus Prolog processes..."
pkill -9 sicstus 2>/dev/null || true

# Close only DALI terminals - improved approach
case "$os_name" in
        Darwin)
            echo "Closing DALI Terminal windows..."
            # Close only DALI windows instead of closing entire Terminal
            for window_title in "${DALI_WINDOW_IDS[@]}"; do
                echo "Closing window: $window_title"
                osascript -e "
                tell application \"Terminal\"
                    repeat with w in windows
                        if custom title of w is \"$window_title\" then
                            close w
                        end if
                    end repeat
                end tell
                " 2>/dev/null || true
            done
            echo "DALI windows closed. Terminal.app remains open for your use."
            ;;
        Linux)
            echo "Closing terminal windows..."
            pkill gnome-terminal 2>/dev/null || true
            pkill xterm 2>/dev/null || true
            pkill konsole 2>/dev/null || true
            ;;
esac

echo "Processes cleanup completed."

# Clean up temporary scripts
echo "Cleaning up temporary files..."
rm -rf "$TEMP_DIR"

echo "MAS shutdown complete"
echo "All tracked processes have been terminated" 