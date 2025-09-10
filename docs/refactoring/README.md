# Testing Scripts

This set of test scripts verify the runnability of the DALI-SICStus codebase.

## Available Tests

1. **01_check_prerequisites.sh** - Verifies system prerequisites
2. **02_test_linda_server.sh** - Tests LINDA server functionality  
3. **03_test_basic_agent.sh** - Tests basic agent functionality
4. **04_test_agent_communication.sh** - Tests agent communication
5. **05_mas_test.sh** - Tests multi-agent system startup
6. **06_message_exchange_test.sh** - **NEW** - Tests message exchange with timing

## Running Tests

### Individual Tests

```bash
cd /path/to/DALI/src/test/scripts
./01_check_prerequisites.sh
./02_test_linda_server.sh
# ... etc
```

### All Tests

```bash
./run_tests.sh
```

## Test 06: Message Exchange Test

The new test `06_message_exchange_test.sh` provides structured timing for observing message exchange between agents:

- **Structured phases** with appropriate wait times
- **Multiple test cycles** to verify consistency  
- **Automatic agent creation** with message exchange logic
- **Detailed logging** of all phases
- **Clean shutdown** with statistics

See `06_message_exchange_test_README.md` for detailed documentation.

## Notes

- For modular version testing, use `make test` in the parent directory
- Tests require SICStus Prolog installation
- Port 3010 must be available for DALI server
