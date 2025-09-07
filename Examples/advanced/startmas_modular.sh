#!/bin/bash

# DALI Multi-Agent System Startup Script - Hybrid Architecture Version
# Uses legacy compiler + modular execution system (temporary solution)

# Enable debugging
# set -x  # Start debugging

# Set PATH to include system directories
export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:$PATH"

/usr/bin/clear  # Clear the terminal

# Save the current directory to a variable
current_dir=$(pwd)

# Print the current directory
echo "The current directory is: $current_dir"
echo "Starting DALI MAS with Hybrid Architecture (Legacy Compilation + Modular Execution)..."

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

# Function to check if port 3010 is in use (macOS compatible)
check_port_3010() {
    if command -v netstat &> /dev/null; then
        netstat -an | grep -q "3010"
    else
        # Fallback: assume port is free if we can't check
        # This is reasonable since we just killed active_server_wi.pl processes
        return 1
    fi
}

# Function to check for active_server_wi.pl processes
check_active_server_processes() {
    /bin/ps aux | /usr/bin/grep "active_server_wi.pl" | /usr/bin/grep -v grep
}

# Clean up any existing DALI server processes and free port 3010
echo "Checking for existing DALI server processes..."

# First, check for active_server_wi.pl processes
SERVER_PROCESSES=$(check_active_server_processes)
if [ ! -z "$SERVER_PROCESSES" ]; then
    echo "Found existing active_server_wi.pl processes:"
    echo "$SERVER_PROCESSES"
    echo "Killing existing DALI server processes..."
    
    # Kill all active_server_wi.pl processes
    /bin/ps aux | /usr/bin/grep "active_server_wi.pl" | /usr/bin/grep -v grep | /usr/bin/awk '{print $2}' | /usr/bin/xargs /bin/kill -9 2>/dev/null || true
    echo "✓ Existing DALI server processes terminated"
    
    # Wait for port to be freed
    echo "Waiting for port 3010 to be freed..."
    count=0
    while check_port_3010 && [ $count -lt 10 ]; do
        sleep 1
        count=$((count + 1))
        echo "Waiting... ($count/10)"
    done
    
    if check_port_3010; then
        echo "Warning: Port 3010 still in use after killing processes"
        echo "This may indicate another service is using the port"
    else
        echo "✓ Port 3010 is now free"
    fi
else
    echo "No existing active_server_wi.pl processes found"
    
    # Check if port is still in use by other processes
    if check_port_3010; then
        echo "Port 3010 is in use by another process (not active_server_wi.pl)"
        echo "This may indicate another service is using the port"
        echo "Proceeding anyway - the new server will handle the conflict"
    else
        echo "✓ Port 3010 is free and ready for use"
    fi
fi

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

if [ ! -f "$DALI_MODULAR_HOME/dali_full_compiler.pl" ]; then
    echo "Error: Modular DALI compiler not found at $DALI_MODULAR_HOME/dali_full_compiler.pl"
    echo "Please ensure the modular DALI system is properly installed"
    exit 1
fi

echo "Hybrid DALI system found at $DALI_MODULAR_HOME (Legacy compilation + Modular execution)"

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
    # Note: .txt agent files should not be made executable per user requirements
done

echo "Built agents:"
ls -l $BUILD_HOME
cp $BUILD_HOME/*.txt work
# Note: .txt agent files should not be made executable per user requirements

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

# Function to execute Prolog commands with timeout and proper cleanup (for one-time commands)
execute_prolog_with_timeout() {
    local timeout_seconds="$1"
    local description="$2"
    shift 2
    local prolog_args=("$@")
    
    echo "Executing Prolog: $description (timeout: ${timeout_seconds}s)"
    
    # Create a temporary output file to capture detailed output
    local temp_output="$TEMP_DIR/prolog_exec_$$.tmp"
    
    # Define timeout function for cross-platform compatibility
    timeout_cmd() {
        local duration="$1"
        shift
        if command -v timeout &> /dev/null; then
            # Linux/GNU timeout
            timeout "$duration" "$@"
        else
            # macOS fallback using perl (as per user memory)
            perl -e 'alarm shift; exec @ARGV' "$duration" "$@"
        fi
    }
    
    # Execute Prolog with timeout
    if timeout_cmd "$timeout_seconds" "$PROLOG" "${prolog_args[@]}" > "$temp_output" 2>&1; then
        echo "✓ Prolog execution completed successfully: $description"
        rm -f "$temp_output"
        return 0
    else
        local exit_code=$?
        echo "✗ Prolog execution failed or timed out: $description"
        echo "Exit code: $exit_code"
        echo "Output:"
        cat "$temp_output"
        echo ""
        
        # Kill any remaining SICStus processes that might be hanging
        echo "Cleaning up any hanging SICStus processes..."
        pkill -f "sicstus.*dali_full_compiler" 2>/dev/null || true
        pkill -f "sicstus.*compile_with_dali" 2>/dev/null || true
        
        rm -f "$temp_output"
        return 1
    fi
}

# Function to check Prolog syntax with timeout and enhanced validation (macOS compatible)
check_prolog_syntax() {
    local file="$1"
    local file_type="$2"
    local timeout_seconds=10  # Timeout for syntax check
    
    if [ -f "$file" ]; then
        echo "Checking syntax of $file_type file: $file (timeout: ${timeout_seconds}s)"
        
        # Create a temporary output file to capture detailed error messages
        local temp_output="$TEMP_DIR/syntax_check_$$.tmp"
        
        # Define timeout function for cross-platform compatibility
        timeout_cmd() {
            local duration="$1"
            shift
            if command -v timeout &> /dev/null; then
                # Linux/GNU timeout
                timeout "$duration" "$@"
            else
                # macOS fallback using perl (as per user memory)
                perl -e 'alarm shift; exec @ARGV' "$duration" "$@"
            fi
        }
        
        # First, perform basic syntax validation with enhanced error detection
        if ! timeout_cmd "$timeout_seconds" "$PROLOG" --noinfo --goal "catch((consult('$file'), write('SYNTAX_OK')), Error, (write('SYNTAX_ERROR: '), write(Error), nl)), halt." > "$temp_output" 2>&1; then
            echo "✗ Syntax check failed or timed out for $file"
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
            echo "- Timeout: Prolog may be hanging due to infinite loops"
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
            # Additional validation: check for common formatting issues
            echo "Performing additional syntax validation..."
            
            # Check for missing dots at end of lines (basic check)
            local missing_dots=$(grep -v '^$' "$file" | grep -v '\.$' | wc -l)
            if [ "$missing_dots" -gt 0 ]; then
                echo "✗ WARNING: Found $missing_dots lines without proper termination dots"
                echo "This may indicate malformed Prolog syntax"
                echo "File content:"
                echo "----------------------------------------"
                cat "$file"
                echo "----------------------------------------"
                echo ""
                rm -f "$temp_output"
                return 1
            fi
            
            # Check for extremely long lines (potential concatenation issues)
            local long_lines=$(awk 'length($0) > 200 {print NR ": " $0}' "$file" | wc -l)
            if [ "$long_lines" -gt 0 ]; then
                echo "✗ WARNING: Found $long_lines extremely long lines (potential concatenation issues)"
                echo "This may indicate malformed Prolog syntax"
                echo "Long lines:"
                awk 'length($0) > 200 {print NR ": " $0}' "$file"
                echo ""
                echo "File content:"
                echo "----------------------------------------"
                cat "$file"
                echo "----------------------------------------"
                echo ""
                rm -f "$temp_output"
                return 1
            fi
            
            # Check for duplicate content (potential compilation issues)
            local line_count=$(wc -l < "$file")
            local unique_lines=$(sort "$file" | uniq | wc -l)
            if [ "$line_count" -gt 2 ] && [ "$line_count" -eq "$unique_lines" ] && [ "$line_count" -gt 1 ]; then
                # Check if all non-empty lines are identical
                local non_empty_lines=$(grep -v '^$' "$file" | wc -l)
                local unique_non_empty=$(grep -v '^$' "$file" | sort | uniq | wc -l)
                if [ "$non_empty_lines" -gt 1 ] && [ "$unique_non_empty" -eq 1 ]; then
                    echo "✗ WARNING: All non-empty lines are identical - potential compilation issue"
                    echo "This may indicate the compiler generated duplicate content"
                    echo "File content:"
                    echo "----------------------------------------"
                    cat "$file"
                    echo "----------------------------------------"
                    echo ""
                    rm -f "$temp_output"
                    return 1
                fi
            fi
            
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

# Check for existing LINDA server processes - RESPECT REQUIREMENT 9
echo "Checking for existing LINDA server on port 3010..."

# Check if there are any active_server_wi.pl processes running
SERVER_PROCESSES=$(check_active_server_processes)
if [ ! -z "$SERVER_PROCESSES" ]; then
    echo "Found existing active_server_wi.pl processes:"
    echo "$SERVER_PROCESSES"
    echo "Respecting requirement: Not starting new server if active_server_wi.pl is already running"
    echo "Using existing LINDA server for compilation phase"
    NEED_CLEANUP=false
    TEMP_SERVER_PID=""
else
    # No existing processes, check if port is free
    if check_port_3010; then
        echo "Port 3010 is in use but no active_server_wi.pl processes found"
        echo "This may indicate another service is using the port"
        echo "Proceeding with server startup - it will handle the conflict"
    else
        echo "Port 3010 is free, starting new LINDA server for compilation phase"
    fi
    
    # Start LINDA server for compilation
    echo "Starting LINDA server for compilation phase..."
    "$PROLOG" --noinfo -l "$COMMUNICATION_DIR/active_server_wi.pl" --goal go. > /dev/null 2>&1 &
    TEMP_SERVER_PID=$!
    sleep 3  # Wait for server to be ready
    
    # Verify server started successfully
    if check_port_3010; then
        echo "✓ LINDA server started successfully (PID: $TEMP_SERVER_PID)"
        NEED_CLEANUP=true
    else
        echo "✗ Failed to start LINDA server - port 3010 not listening"
        echo "This may indicate a problem with the server startup"
        exit 1
    fi
fi

# Use the same logic as legacy system: compile files from work/ directory
# (which were already generated by the legacy build process above)
for agent_filename in work/*.txt; do
    if [ ! -f "$agent_filename" ]; then
        echo "Error: No agent files found in work/"
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
    
    # Use DALI compiler for REAL compilation (handles DALI syntax correctly)
    echo "Running REAL DALI compilation for $agent_base using DALI compiler..."
    agent_config="$current_dir/conf/mas/$agent_base"
    
    # Use DALI compiler for REAL compilation (handles DALI syntax and generates all files)
    echo "Compiling with DALI compiler (handles DALI syntax correctly)..."
    if ! execute_prolog_with_timeout 10 "DALI compilation for $agent_base" --noinfo -l "$DALI_MODULAR_HOME/dali_working_compiler.pl" --goal compile_agent_working\(\'$agent_config\'\).; then
        echo "FATAL ERROR: DALI compilation failed or timed out for $agent_base"
        echo "This indicates serious issues with the agent source file or configuration"
        echo "The compilation process was terminated after 10 seconds timeout."
        echo "Exiting with halt as per requirements."
        exit 1
    fi
    
    # Check if essential files were generated by the compiler
    agent_name_base="${agent_base%.*}"
    agent_pl_file="work/${agent_name_base}.pl"
    agent_ple_file="work/${agent_name_base}.ple"
    agent_plv_file="work/${agent_name_base}.plv"
    agent_plf_file="work/${agent_name_base}.plf"
    
    echo "Checking generated files for $agent_base:"
    
    # The main .pl file is absolutely required
    if [ ! -f "$agent_pl_file" ]; then
        echo "FATAL ERROR: Agent program file not generated: $agent_pl_file"
        validation_failed=true
        break
    fi
    echo "  ✓ $agent_pl_file (required)"
    
    # Other files are optional for modular system
    if [ -f "$agent_ple_file" ]; then
        echo "  ✓ $agent_ple_file (optional)"
    else
        echo "  - $agent_ple_file (not generated - using modular system)"
    fi
    
    if [ -f "$agent_plv_file" ]; then
        echo "  ✓ $agent_plv_file (optional)"
    else
        echo "  - $agent_plv_file (not generated - using modular system)"
    fi
    
    if [ -f "$agent_plf_file" ]; then
        echo "  ✓ $agent_plf_file (optional)"
    else
        echo "  - $agent_plf_file (not generated - using modular system)"
    fi
    
    echo "✓ Essential Prolog files generated for $agent_base (modular system compatible)"
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
echo "Performing syntax validation for all generated files and configuration files..."

# First, check configuration files
echo "Checking configuration files..."
config_files=(
    "$CONF_DIR/communication.txt"
    "$CONF_DIR/communication.pl"
    "$CONF_DIR/communication.con"
)

for config_file in "${config_files[@]}"; do
    if [ -f "$config_file" ]; then
        echo "Checking configuration file: $config_file"
        if ! check_prolog_syntax "$config_file" "configuration"; then
            echo "FATAL ERROR: Syntax check failed for configuration file $config_file"
            echo "Cannot proceed with MAS startup due to syntax errors in configuration."
            echo ""
            echo "Please fix the syntax errors in the configuration files before retrying."
            echo "Configuration files must be valid Prolog syntax."
            exit 1
        fi
    fi
done

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

# Start the LINDA server - with error handling
srvcmd="$PROLOG --noinfo -l $COMMUNICATION_DIR/active_server_wi.pl --goal go."
echo "Starting DALI/LINDA server with command: $srvcmd"

# Check if active_server_wi.pl is already running before starting server
SERVER_PROCESSES=$(check_active_server_processes)
if [ ! -z "$SERVER_PROCESSES" ]; then
    echo "✓ active_server_wi.pl already running - respecting requirement 9"
    echo "Not starting new server as active_server_wi.pl is already running"
    echo "Existing processes:"
    echo "$SERVER_PROCESSES"
else
    # Check if port is in use by other processes
    if check_port_3010; then
        echo "Port 3010 is in use by another process (not active_server_wi.pl)"
        echo "Starting new DALI server - it will handle the port conflict"
    else
        echo "Port 3010 is free, starting new LINDA server..."
    fi
    
    open_terminal "$srvcmd" "DALI Server"
    sleep 3  # Wait for server to start
    
    # Verify server started successfully
    if check_port_3010; then
        echo "✓ DALI/LINDA server started successfully"
    else
        echo "FATAL ERROR: Failed to start LINDA server - port 3010 not listening"
        echo "This indicates a serious problem with the server startup"
        echo "Exiting with halt as per requirements."
        exit 1
    fi
fi
echo "Starting MAS agents with Hybrid Architecture (Modular Execution)..."
$WAIT > /dev/null  # Wait for a while

# Launch agents in separate terminals - VALIDATION ALREADY COMPLETED
echo "Launching validated agent instances using hybrid DALI system..."
agent_startup_failed=false

for agent_filename in $BUILD_HOME/*; do
    agent_base="${agent_filename##*/}"
    echo "Starting agent: $agent_base (modular execution)"
    
    # Verify essential agent files exist before starting
    agent_name_base="${agent_base%.*}"
    agent_pl_file="work/${agent_name_base}.pl"
    
    # Only the main .pl file is required for modular system
    if [ ! -f "$agent_pl_file" ]; then
        echo "FATAL ERROR: Essential agent file missing for $agent_base"
        echo "Missing file: $agent_pl_file"
        echo "Cannot start agent without essential file. Exiting with halt as per requirements."
        agent_startup_failed=true
        break
    fi
    
    echo "✓ Essential agent file found: $agent_pl_file"
    
    # Start the agent in a new terminal using the MODULAR EXECUTION SYSTEM
    # (Configuration and syntax validation already completed in Phase 1)
    agent_cmd="$current_dir/conf/startagent_modular.sh $agent_base $PROLOG $DALI_MODULAR_HOME"
    open_terminal "$agent_cmd" "DALI Agent (Modular): $agent_base"
    sleep 2  # Increased sleep time
    $WAIT > /dev/null  # Wait a bit before launching the next agent
done

# Check if any agent startup failed
if [ "$agent_startup_failed" = true ]; then
    echo "FATAL ERROR: Agent startup failed due to missing files"
    echo "Exiting with halt as per requirements."
    exit 1
fi

echo ""
echo "========================================="
echo "✓ ALL AGENTS STARTED SUCCESSFULLY"
echo "========================================="

# Start user agent in another terminal
user_cmd="$PROLOG --noinfo -l $DALI_HOME/active_user_wi.pl --goal user_interface."
open_terminal "$user_cmd" "DALI User Interface"

echo "Hybrid MAS started successfully!"
echo "Using hybrid DALI architecture (Legacy compilation + Modular execution)"
echo "Core system: $DALI_MODULAR_HOME/dali_core.pl"
echo "Compiler: $DALI_MODULAR_HOME/dali_legacy_compiler.pl (temporary)"
echo "SICStus Prolog: $PROLOG"
echo "Temporary scripts: $TEMP_DIR"
echo ""
echo "Press Enter to shutdown the MAS"
read

# Clean up processes
echo "Shutting down MAS..."

# Stop SICStus Prolog processes with timeout
echo "Stopping SICStus Prolog processes..."
echo "Giving processes 5 seconds to terminate gracefully..."
pkill sicstus 2>/dev/null || true
sleep 2

# Force kill any remaining SICStus processes
echo "Force killing any remaining SICStus processes..."
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