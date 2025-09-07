#!/bin/bash

# Test the syntax check function directly
PROLOG="/usr/local/sicstus4.6.0/bin/sicstus"
TEMP_DIR="temp_scripts"
mkdir -p "$TEMP_DIR"

# Function to check Prolog syntax with timeout and enhanced validation
check_prolog_syntax() {
    local file="$1"
    local file_type="$2"
    local timeout_seconds=10
    
    if [ -f "$file" ]; then
        echo "Checking syntax of $file_type file: $file (timeout: ${timeout_seconds}s)"
        
        local temp_output="$TEMP_DIR/syntax_check_$$.tmp"
        
        timeout_cmd() {
            local duration="$1"
            shift
            perl -e 'alarm shift; exec @ARGV' "$duration" "$@"
        }
        
        # First, perform basic syntax validation
        if ! timeout_cmd "$timeout_seconds" "$PROLOG" --noinfo --goal "catch((consult('$file'), write('SYNTAX_OK')), Error, (write('SYNTAX_ERROR: '), write(Error), nl)), halt." > "$temp_output" 2>&1; then
            echo "✗ Syntax check failed or timed out for $file"
            echo "Error details:"
            cat "$temp_output"
            rm -f "$temp_output"
            return 1
        elif grep -q "SYNTAX_ERROR" "$temp_output"; then
            echo "✗ Syntax error detected in $file"
            echo "Error details:"
            cat "$temp_output"
            rm -f "$temp_output"
            return 1
        else
            # Additional validation: check for common formatting issues
            echo "Performing additional syntax validation..."
            
            # Check for missing dots at end of lines
            local missing_dots=$(grep -v '^$' "$file" | grep -v '\.$' | wc -l)
            if [ "$missing_dots" -gt 0 ]; then
                echo "✗ WARNING: Found $missing_dots lines without proper termination dots"
                echo "This may indicate malformed Prolog syntax"
                echo "File content:"
                echo "----------------------------------------"
                cat "$file"
                echo "----------------------------------------"
                rm -f "$temp_output"
                return 1
            fi
            
            # Check for extremely long lines
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
                rm -f "$temp_output"
                return 1
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

# Test the function
check_prolog_syntax work/agent1.pl "agent program"
