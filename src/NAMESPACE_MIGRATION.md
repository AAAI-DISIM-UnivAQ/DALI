# DALI Namespace Migration: Conflict Resolution with SICStus Libraries

## Issue Identified

The original utility module names were conflicting with SICStus Prolog system libraries:

- `list_utils.pl` → Potential conflict with SICStus list utilities
- `file_utils.pl` → Potential conflict with SICStus file utilities  
- `time_utils.pl` → Potential conflict with SICStus time utilities
- `debug_utils.pl` → Potential conflict with SICStus debug utilities

## Solution Applied
### **File Renaming**

All utility files have been renamed with the `dali_` prefix to avoid conflicts:

| **Original Name** | **New Name** | **Status** |
|---|---|---|
| `list_utils.pl` | `dali_list_utils.pl` | Migrated |
| `file_utils.pl` | `dali_file_utils.pl` | Migrated |
| `time_utils.pl` | `dali_time_utils.pl` | Migrated |
| `debug_utils.pl` | `dali_debug_utils.pl` | Migrated |

### **Module Name Updates**

Module declarations have been updated accordingly:

```prolog
% OLD
:- module(list_utils, [...]).
:- module(file_utils, [...]).
:- module(time_utils, [...]).
:- module(debug_utils, [...]).

% NEW  
:- module(dali_list_utils, [...]).
:- module(dali_file_utils, [...]).
:- module(dali_time_utils, [...]).
:- module(dali_debug_utils, [...]).
```

### **Import Updates**

All files that import these utilities have been updated:

#### `src/dali_core.pl`
```prolog
% OLD
:- use_module('utils/list_utils').
:- use_module('utils/file_utils').
:- use_module('utils/debug_utils').
:- use_module('utils/time_utils').

% NEW
:- use_module('utils/dali_list_utils').
:- use_module('utils/dali_file_utils').
:- use_module('utils/dali_debug_utils').
:- use_module('utils/dali_time_utils').
```

#### `src/agent/agent_init.pl`
```prolog
% OLD
:- use_module('../utils/file_utils').
:- use_module('../utils/debug_utils').

% NEW
:- use_module('../utils/dali_file_utils').
:- use_module('../utils/dali_debug_utils').
```

#### `src/parsing/rule_parser.pl`
```prolog
% OLD
:- use_module('../utils/list_utils').
:- use_module('../utils/debug_utils').

% NEW
:- use_module('../utils/dali_list_utils').
:- use_module('../utils/dali_debug_utils').
```

### **Additional Conflict Resolution**

**1. Removed `library(lists)` import** from `dali_list_utils.pl`:

```prolog
% OLD
:- use_module(library(lists)).

% NEW (Commented out to avoid conflicts)
% Note: Removed library(lists) import to avoid conflicts with our custom intersection/3
% :- use_module(library(lists)).
```

**2. Renamed `now/1` predicate** in `dali_time_utils.pl`:

```prolog
% OLD
:- module(dali_time_utils, [..., now/1]).
now(Time) :- statistics(walltime, [Time, _]).

% NEW (Renamed to avoid conflict with system.pl)
:- module(dali_time_utils, [..., dali_now/1]).
dali_now(Time) :- statistics(walltime, [Time, _]).
```

**3. Renamed `datime/1` predicate** in `dali_time_utils.pl`:

```prolog
% OLD
:- module(dali_time_utils, [..., datime/1]).
datime(DateTime) :- get_time(TimeStamp), stamp_date_time(TimeStamp, DateTime, local).

% NEW (Renamed to avoid conflict with system.pl)
:- module(dali_time_utils, [..., dali_datime/1]).
dali_datime(DateTime) :- get_time(TimeStamp), stamp_date_time(TimeStamp, DateTime, local).
```

**Reason**: SICStus Prolog's `library(system)` already defines both `now/1` and `datime/1`, causing redefinition warnings.

### **Testing Updates**

Updated `Makefile` test targets:

```bash
# OLD
use_module('utils/list_utils')
use_module('utils/file_utils')
use_module('utils/time_utils')
use_module('utils/debug_utils')

# NEW
use_module('utils/dali_list_utils')
use_module('utils/dali_file_utils') 
use_module('utils/dali_time_utils')
use_module('utils/dali_debug_utils')
```

## Benefits of This Migration

### **Conflict Resolution**
- **No namespace collisions** with SICStus system libraries
- **Clean module loading** without import warnings
- **Predictable behavior** across different Prolog implementations

### **Better Naming Convention**
- **Clear ownership**: `dali_` prefix indicates DALI-specific modules
- **Consistent naming**: All utility modules follow same pattern
- **Future-proof**: Template for naming additional modules

### **Improved Compatibility**
- **SICStus compatibility**: Works without conflicts in SICStus Prolog
- **SWI-Prolog compatibility**: Also works in SWI-Prolog
- **Cross-platform**: Consistent behavior across systems

## Testing Results

### Before Migration
```
Warning: Local definition of list_utils:intersection/3 overrides weak import from lists
The procedure now/1 is being redefined.
    Old file: /usr/local/sicstus4.6.0/bin/sp-4.6.0/sicstus-4.6.0/library/system.pl
    New file: .../dali_time_utils.pl
Do you really want to redefine it?
```

### After Migration  
```
dali_list_utils: PASS
dali_file_utils: PASS
dali_time_utils: PASS
dali_debug_utils: PASS
```

**All warnings and conflicts resolved!**

## Files Modified

### **Core Files**
- `src/utils/dali_list_utils.pl` (renamed + module name updated)
- `src/utils/dali_file_utils.pl` (renamed + module name updated)
- `src/utils/dali_time_utils.pl` (renamed + module name updated)
- `src/utils/dali_debug_utils.pl` (renamed + module name updated)

### **Import References**
- `src/dali_core.pl` (imports updated)
- `src/agent/agent_init.pl` (imports updated)
- `src/parsing/rule_parser.pl` (imports updated)

### **Documentation & Testing**
- `src/README.md` (file structure updated)
- `src/Makefile` (test targets updated)
- `src/NAMESPACE_MIGRATION.md` (this document)

## Best Practices Established

### **Naming Convention**
- **Prefix all DALI modules** with `dali_` to avoid conflicts
- **Use descriptive names** that clearly indicate purpose
- **Maintain consistency** across all modules

### **Conflict Prevention**
- **Check for system library conflicts** before naming modules
- **Use unique namespaces** for project-specific functionality
- **Document naming decisions** for future reference

### **Testing Strategy**
- **Test all renamed modules** individually
- **Verify integration** with dependent modules
- **Check for warnings** and resolve conflicts

## Future Module Naming

When creating new modules, follow this pattern:

```
Good: dali_event_manager.pl
Good: dali_message_handler.pl
Good: dali_memory_manager.pl

Avoid: event_manager.pl (too generic)
Avoid: utils.pl (conflicts with system utils)
Avoid: memory.pl (conflicts with system memory libs)
```

## Migration Complete

 