# DALI Agent System - Modular Architecture

## Overview

This is the DALI (Distributed Agent Logic Infrastructure) system restructured in a modular architecture to improve maintainability, testability and code reusability.

## Module Structure

```
src/
├── dali_core.pl              # Main system coordinator
├── agent/                    # Agent management
│   ├── agent_init.pl         # Agent initialization
│   ├── agent_config.pl       # Configuration and setup (to be implemented)
│   └── agent_lifecycle.pl    # Lifecycle management (to be implemented)
├── parsing/                  # Processing and parsing
│   ├── rule_parser.pl        # Rule and clause parsing
│   ├── file_processor.pl     # .pl/.ple/.plf file processing (to be implemented)
│   └── term_expander.pl      # Custom term expansion (to be implemented)
├── events/                   # Event management
│   ├── external_events.pl    # External events and communication (to be implemented)
│   ├── internal_events.pl    # Internal events and triggers (to be implemented)
│   └── event_timing.pl       # Event timing management (to be implemented)
├── communication/            # Inter-agent communication
│   ├── message_handler.pl    # FIPA message handling (to be implemented)
│   ├── ontology_manager.pl   # Ontology management (to be implemented)
│   └── protocol_utils.pl     # Communication protocol utilities (to be implemented)
├── actions/                  # Action execution
│   ├── action_executor.pl    # Action execution (to be implemented)
│   ├── action_queue.pl       # Action priority queues (to be implemented)
│   └── goal_manager.pl       # Goal management (to be implemented)
├── memory/                   # Memory management
│   ├── past_events.pl        # Past events management (to be implemented)
│   ├── memory_manager.pl     # Persistent memory management (to be implemented)
│   └── remember_events.pl    # Remember system (to be implemented)
├── learning/                 # Learning system
│   ├── learning_core.pl      # Core learning system (to be implemented)
│   ├── clause_learner.pl     # Clause learning (to be implemented)
│   └── knowledge_update.pl   # Knowledge update (to be implemented)
├── planning/                 # Planning
│   ├── asp_interface.pl      # Answer Set Programming interface (to be implemented)
│   ├── plan_generator.pl     # Plan generation (to be implemented)
│   └── plan_executor.pl      # Plan execution (to be implemented)
└── utils/                    # Common utilities
    ├── dali_list_utils.pl    # List utilities ✓
    ├── dali_file_utils.pl    # File I/O utilities ✓
    ├── dali_time_utils.pl    # Time utilities ✓
    └── dali_debug_utils.pl   # Debug and logging ✓
```

## Implementation Status

### ✅ Completed
- **utils/**: All basic utilities have been extracted and modularized
- **dali_core.pl**: Main coordinator with execution cycle
- **agent/agent_init.pl**: Agent initialization and configuration
- **parsing/rule_parser.pl**: Rule parsing and processing

### 🚧 In Development
The following modules are defined as placeholders in `dali_core.pl` and need implementation:
- External and internal event management
- Communication and messaging system
- Action and goal management
- Memory and past events
- Learning system
- ASP interface for planning

## Usage

### Starting an Agent

```prolog
?- use_module('src/dali_core').
?- start_dali_agent('path/to/agent/config.txt').
```

### Debugging

To enable debugging:

```prolog
?- assertz(debug_on).
```

### Configuration File Structure

The agent configuration file must follow this format:

```prolog
agent(
    'path/to/program/file',    % Program file
    agent_name,                % Agent name
    'path/to/ontology.txt',    % Ontology (or 'no')
    italian,                   % Language
    [],                        % Additional file list
    [],                        % Library list
    'path/to/profile.txt',     % User profile (or 'no')
    'path/to/dali_onto.txt',   % DALI ontology (or 'no')
    specialization_type        % Specialization type
).
```

## Benefits of Modular Architecture

### 1. **Maintainability**
- Each module has well-defined responsibilities
- Isolated changes don't affect other components
- More readable and organized code

### 2. **Testability**
- Ability to test individual modules in isolation
- Simpler unit test implementation
- More effective debugging

### 3. **Reusability**
- Modules can be reused in other projects
- Shared functionality centralized in utilities

### 4. **Scalability**
- Easy to add new functionality
- Selective loading of required modules
- Better performance for specific applications

### 5. **Collaboration**
- Different teams can work on different modules
- Simpler integration
- Reduced merge conflicts

## Migration from Original System

### Compatibility
The modular system maintains compatibility with:
- Custom operators (`:>`, `:<`, `~/`, `</`, `?/`)
- Term expansion rules
- Existing dynamic predicates
- Existing configuration files

### Main Differences
- Use of Prolog modules for separate namespaces
- Explicit imports instead of global includes
- Centralized utilities
- Improved error handling

## Next Steps

1. **Implement event modules**: Separate internal/external event management
2. **Modularize communication**: Extract FIPA messaging system
3. **Separate action management**: Create action queues and executors
4. **Implement memory management**: Modularize past events and remember
5. **Extract learning system**: Separate clause learning
6. **Modularize ASP**: Answer Set Programming interface

## Testing and Validation

To test compatibility:

```bash
# Test module loading
swipl -g "use_module('src/dali_core'), halt."

# Test utilities
swipl -g "use_module('src/utils/list_utils'), remove_dups([1,2,2,3], X), write(X), halt."
```

## Contributing

When implementing new modules:
1. Follow the existing structure
2. Document public interfaces
3. Include usage examples
4. Update this README

## License

Apache Public License - AAAI Research Group, University of L'Aquila 