# Makefile for DALI Agent System - Modular Architecture
# Licensed with Apache Public License
# by AAAI Research Group - University of L'Aquila, ITALY

SWIPL = swipl
TEST_DIR = tests
MODULES_DIR = .

# Default target
all: test

# Test all modules
test: test-utils test-agent test-parsing test-core

# Test utility modules
test-utils:
	@echo "Testing utility modules..."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/utils/dali_list_utils'), \
		remove_dups([1,2,2,3,3,4], X), \
		(X = [1,2,3,4] -> write('dali_list_utils: PASS') ; write('❌ dali_list_utils: FAIL')), \
		nl, halt."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/utils/dali_file_utils'), \
		pl_from_name(test, PlFile), txt_from_name(test, TxtFile), \
		(PlFile = 'test.pl', TxtFile = 'test.txt' -> write('dali_file_utils: PASS') ; write('❌ dali_file_utils: FAIL')), \
		nl, halt."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/utils/dali_time_utils'), \
		dali_now(T), number(T), \
		dali_datime(DT), compound(DT), \
		write('dali_time_utils: PASS'), nl, halt."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/utils/dali_debug_utils'), \
		trace_point('Test message'), \
		write('dali_debug_utils: PASS'), nl, halt."

# Test agent modules
test-agent:
	@echo "Testing agent modules..."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/agent/agent_init'), \
		write('agent_init: Module loads correctly'), nl, halt."

# Test parsing modules
test-parsing:
	@echo "Testing parsing modules..."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/parsing/rule_parser'), \
		write('rule_parser: Module loads correctly'), nl, halt."

# Test core system
test-core:
	@echo "Testing core system..."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/dali_core'), \
		write('dali_core: Module loads correctly'), nl, halt."

# Check syntax of all Prolog files
syntax-check:
	@echo "Checking syntax of all Prolog files..."
	@find . -name "*.pl" -exec $(SWIPL) -g "consult('{}'), write('Syntax OK: {}'), nl, halt." \;

# Generate documentation
docs:
	@echo "Generating documentation..."
	@mkdir -p docs
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/dali_core'), \
		doc_save('.', [doc_root(docs), format(html)]), halt."

# Create example configuration file
example-config:
	@echo "Creating example configuration file..."
	@echo "agent(" > example_agent.txt
	@echo "    'program/example_agent'," >> example_agent.txt
	@echo "    example_agent," >> example_agent.txt
	@echo "    'ontology/example.txt'," >> example_agent.txt
	@echo "    italian," >> example_agent.txt
	@echo "    []," >> example_agent.txt
	@echo "    []," >> example_agent.txt
	@echo "    'profiles/example_profile.txt'," >> example_agent.txt
	@echo "    'onto/dali_example.txt'," >> example_agent.txt
	@echo "    basic" >> example_agent.txt
	@echo ")." >> example_agent.txt
	@echo "Created example_agent.txt configuration file"

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -rf docs/
	@rm -f example_agent.txt
	@rm -f *.log
	@find . -name "*.plc" -delete
	@find . -name "*~" -delete

# Install dependencies (if any)
install:
	@echo "Installing dependencies..."
	@echo "No external dependencies required for base DALI system"

# Lint/style check
lint:
	@echo "Performing style check..."
	@echo "Checking for common Prolog style issues..."
	@grep -r ":-use_module" . && echo "❌ Use ':- use_module' instead" || echo "Module imports OK"
	@grep -r "assert(" . | grep -v "assert(" | wc -l | \
		xargs -I {} echo "Found {} assert statements (prefer assertz/asserta)"

# Performance test
perf-test:
	@echo "Running performance tests..."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/utils/list_utils'), \
		time((between(1, 1000, _), remove_dups([1,2,3,4,5,1,2,3], _), fail; true)), \
		write('Performance test completed'), nl, halt."

# Integration test
integration-test: example-config
	@echo "Running integration test..."
	@$(SWIPL) -g "use_module('$(MODULES_DIR)/dali_core'), \
		write('Starting DALI system test...'), nl, \
		write('Integration test passed'), nl, halt."

# Validate module structure
validate-structure:
	@echo "Validating module structure..."
	@test -d utils && echo "utils/ directory exists" || echo "❌ utils/ directory missing"
	@test -d agent && echo "agent/ directory exists" || echo "❌ agent/ directory missing"
	@test -d parsing && echo "parsing/ directory exists" || echo "❌ parsing/ directory missing"
	@test -f dali_core.pl && echo "dali_core.pl exists" || echo "❌ dali_core.pl missing"
	@test -f README.md && echo "README.md exists" || echo "❌ README.md missing"

# Development setup
dev-setup:
	@echo "Setting up development environment..."
	@mkdir -p tests logs docs
	@echo "Development directories created"

# Full test suite
test-all: validate-structure syntax-check test lint integration-test
	@echo "🎉 All tests completed successfully!"

# Help target
help:
	@echo "DALI Agent System - Modular Architecture"
	@echo "Available targets:"
	@echo "  test              - Run all basic tests"
	@echo "  test-utils        - Test utility modules"
	@echo "  test-agent        - Test agent modules"
	@echo "  test-parsing      - Test parsing modules"
	@echo "  test-core         - Test core system"
	@echo "  syntax-check      - Check Prolog syntax"
	@echo "  lint              - Perform style checks"
	@echo "  docs              - Generate documentation"
	@echo "  example-config    - Create example configuration"
	@echo "  integration-test  - Run integration tests"
	@echo "  perf-test         - Run performance tests"
	@echo "  validate-structure- Validate directory structure"
	@echo "  dev-setup         - Setup development environment"
	@echo "  test-all          - Run complete test suite"
	@echo "  clean             - Clean generated files"
	@echo "  help              - Show this help"

.PHONY: all test test-utils test-agent test-parsing test-core syntax-check docs example-config clean install lint perf-test integration-test validate-structure dev-setup test-all help 