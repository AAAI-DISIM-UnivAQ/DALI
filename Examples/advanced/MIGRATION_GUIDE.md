# Migration Guide: DALI Examples to Modular Architecture

## Overview

This guide explains how to migrate from the original DALI system to the new modular architecture for running multi-agent systems.

## Files Modified/Created

### 🆕 New Files Created

1. **`startmas_modular.sh`** - Updated version of `startmas.sh`
2. **`conf/startagent_modular.sh`** - Updated version of `conf/startagent.sh`

### 📝 Original Files (Still Available)

- `startmas.sh` - Original startup script (uses `active_dali_wi.pl`)
- `conf/startagent.sh` - Original agent starter (uses `active_dali_wi.pl`)

## Key Changes

### 1. System Entry Point

| **Original System** | **Modular System** |
|---|---|
| `active_dali_wi.pl` | `dali_core.pl` |
| `start0('config.txt')` | `start_dali_agent('config.txt')` |

### 2. File Structure

```bash
# Original system
sicstus -l ../../src/active_dali_wi.pl --goal "start0('conf/mas/agent1.txt')."

# Modular system  
sicstus -l ../../src/dali_core.pl --goal "start_dali_agent('conf/mas/agent1.txt')."
```

### 3. Script Changes

#### `startmas_modular.sh` Changes:

- **Verification**: Checks for `dali_core.pl` existence
- **Logging**: Enhanced startup messages
- **Error Handling**: Better error detection for modular system
- **Compatibility**: Calls `startagent_modular.sh` instead of `startagent.sh`

#### `startagent_modular.sh` Changes:

- **Core System**: Uses `dali_core.pl` instead of `active_dali_wi.pl`
- **Entry Point**: Uses `start_dali_agent/1` instead of `start0/1`
- **Validation**: Verifies modular system files exist

## Usage Instructions

### Option 1: Use Modular System (Recommended)

```bash
cd Examples/advanced
./startmas_modular.sh
```

### Option 2: Use Original System (Legacy)

```bash
cd Examples/advanced  
./startmas.sh
```

## What Each Script Does

### `startmas_modular.sh`

1. **System Verification**: Checks for modular DALI files
2. **Server Startup**: Starts LINDA server (unchanged)
3. **Agent Deployment**: Launches agents using modular system
4. **User Interface**: Starts user interface (unchanged)
5. **Enhanced Logging**: Provides clear feedback about modular system usage

### `startagent_modular.sh`

1. **Validation**: Ensures `dali_core.pl` exists
2. **Agent Launch**: Uses new modular entry point
3. **Error Handling**: Better error messages for debugging

## Benefits of Modular System

### **Maintainability**
- Cleaner code organization
- Easier debugging and development
- Modular components

### **Performance**
- Selective module loading
- Optimized memory usage
- Better resource management

### **Development**
- Individual module testing
- Parallel development capability
- Simplified integration

## Compatibility

### **Maintained Compatibility**

- **Agent Configuration Files**: No changes needed
- **Instance and Type Files**: Work unchanged  
- **LINDA Server**: Uses same server system
- **User Interface**: Unchanged user experience
- **Communication Protocol**: FIPA messages work as before

### **Migration Path**

1. **Phase 1**: Use `startmas_modular.sh` (current)
2. **Phase 2**: Additional modules will be implemented
3. **Phase 3**: Full modular system deployment

## Troubleshooting

### Common Issues

#### Error: "Modular DALI core not found"

```bash
Error: Modular DALI core not found at ../../src/dali_core.pl
```

**Solution**: Ensure you're running from the correct directory and the modular system is installed:

```bash
cd Examples/advanced
ls -la ../../src/dali_core.pl  # Should exist
```

#### Error: "startagent_modular.sh not found"

**Solution**: Ensure the modular scripts are executable:

```bash
chmod +x startmas_modular.sh
chmod +x conf/startagent_modular.sh
```

### Debug Mode

Enable debugging in scripts by uncommenting:

```bash
# set -x  # Start debugging
```

## Testing the Migration

### Quick Test

```bash
cd Examples/advanced

# Test 1: Check files exist
ls -la startmas_modular.sh
ls -la conf/startagent_modular.sh
ls -la ../../src/dali_core.pl

# Test 2: Run modular system
./startmas_modular.sh
```

### Expected Output

```
Starting DALI MAS with Modular Architecture...
Modular DALI system found at ../../src
Server ready. Starting the MAS with Modular Architecture...
Launching agent instances using modular DALI system...
Agent: agent1.txt (using modular architecture)
Modular MAS started successfully!
Using new modular DALI architecture
Core system: ../../src/dali_core.pl
```

## Next Steps

The modular architecture is designed for future expansion:

1. **Event Management**: Will be modularized
2. **Communication**: FIPA handling will be separated
3. **Action Processing**: Will get dedicated modules
4. **Memory Management**: Past events will be modularized
5. **Learning System**: Will be extracted to separate modules

## Support

If you encounter issues:

1. **Check Prerequisites**: Ensure SICStus Prolog is installed
2. **Verify Paths**: Confirm all file paths are correct
3. **Test Original**: Try `startmas.sh` to ensure base system works
4. **Check Permissions**: Ensure scripts are executable

For questions about the modular architecture, refer to `src/README.md`. 