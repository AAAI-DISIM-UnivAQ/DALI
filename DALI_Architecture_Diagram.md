# DALI Architecture - Modules and Agents

## DALI Architecture Overview

This document visualizes the structure of DALI's main modules and the agents in the Examples/advanced folder with their types and instances.

## 1. DALI Main Modules

```mermaid
graph TD
    A["DALI Core System"] --> B["src/"]
    
    B --> C["dali_core.pl<br/>Core Engine"]
    B --> D["active_dali_wi.pl<br/>Main Runtime"]
    B --> E["agent/<br/>Agent Components"]
    B --> F["parsing/<br/>Rule Parser"]
    B --> G["utils/<br/>Utility Modules"]
    B --> H["communication_fipa.pl<br/>FIPA Communication"]
    B --> I["memory.pl<br/>Memory Management"]
    B --> J["meta1.pl<br/>Meta Reasoning"]
    
    E --> E1["agent_init.pl<br/>Agent Initialization"]
    
    F --> F1["rule_parser.pl<br/>Rule Parsing Engine"]
    
    G --> G1["dali_debug_utils.pl<br/>Debug Utilities"]
    G --> G2["dali_file_utils.pl<br/>File Operations"]
    G --> G3["dali_list_utils.pl<br/>List Processing"]
    G --> G4["dali_time_utils.pl<br/>Time Management"]
    
    B --> K["tokefun.pl<br/>Token Functions"]
    B --> L["learning.pl<br/>Learning Module"]
    B --> M["substitute.pl<br/>Variable Substitution"]
    
    style A fill:#e1f5fe
    style C fill:#fff3e0
    style D fill:#fff3e0
    style H fill:#f3e5f5
    style I fill:#e8f5e8
    style J fill:#ffebee
```

## 2. Advanced Agent System

```mermaid
graph TD
    A["Examples/advanced/"] --> B["mas/<br/>Multi-Agent System"]
    
    B --> C["types/<br/>Agent Types"]
    B --> D["instances/<br/>Agent Instances"]
    
    C --> C1["agentType1.txt<br/>Reactive Agent Type"]
    C --> C2["agentType2.txt<br/>Simple Response Type"]
    
    D --> D1["agent1.txt<br/>Instance of agentType1"]
    D --> D2["agent2.txt<br/>Instance of agentType2"]
    
    C1 -.->|"instantiated as"| D1
    C2 -.->|"instantiated as"| D2
    
    A --> E["startmas.sh<br/>System Launcher"]
    A --> F["startmas_modular.sh<br/>Modular Launcher"]
    A --> G["conf/<br/>Configuration"]
    A --> H["work/<br/>Working Directory"]
    A --> I["log/<br/>Log Files"]
    A --> J["build/<br/>Build Files"]
    
    style A fill:#e1f5fe
    style C fill:#fff3e0
    style D fill:#e8f5e8
    style C1 fill:#ffebee
    style C2 fill:#ffebee
    style D1 fill:#f3e5f5
    style D2 fill:#f3e5f5
```

## 3. Agent Type-Instance Relationships

```mermaid
classDiagram
    class AgentType1 {
        +write("Hello world!")
        +eventE() : "which event?"
        +goE() : send_message to agent2
    }
    
    class AgentType2 {
        +goE() : "received"
    }
    
    class Agent1 {
        +type: agentType1
        +behaviors: inherited from AgentType1
    }
    
    class Agent2 {
        +type: agentType2
        +behaviors: inherited from AgentType2
    }
    
    AgentType1 <|-- Agent1 : implements
    AgentType2 <|-- Agent2 : implements
    Agent1 --> Agent2 : messageA(send_message)
```

## 4. Communication Architecture

```mermaid
sequenceDiagram
    participant A1 as Agent1<br/>(agentType1)
    participant A2 as Agent2<br/>(agentType2)
    participant Core as DALI Core
    participant Comm as Communication Module
    
    A1->>Core: Initialize with agentType1
    A2->>Core: Initialize with agentType2
    
    Note over A1: eventE triggered
    A1->>A1: write("which event?")
    
    Note over A1: goE triggered  
    A1->>A1: write("received.")
    A1->>Comm: messageA(agent2, send_message(go, Me))
    Comm->>A2: deliver message
    A2->>A2: goE triggered
    A2->>A2: write("received")
```

## 5. Complete System Structure

```mermaid
flowchart TB
    subgraph "DALI Framework"
        subgraph "Core Modules"
            DC[dali_core.pl]
            AD[active_dali_wi.pl]
            AI[agent_init.pl]
            RP[rule_parser.pl]
            CF[communication_fipa.pl]
        end
        
        subgraph "Utility Modules"
            DU[debug_utils.pl]
            FU[file_utils.pl]
            LU[list_utils.pl]
            TU[time_utils.pl]
        end
        
        subgraph "Advanced Components"
            MEM[memory.pl]
            META[meta1.pl]
            LEARN[learning.pl]
            TOKEN[tokefun.pl]
        end
    end
    
    subgraph "Agent System"
        subgraph "Agent Types"
            AT1[agentType1<br/>Reactive Agent]
            AT2[agentType2<br/>Simple Agent]
        end
        
        subgraph "Agent Instances"
            A1[agent1]
            A2[agent2]
        end
        
        subgraph "Runtime Environment"
            START[startmas.sh]
            CONF[Configuration]
            LOGS[Log System]
        end
    end
    
    DC --> AI
    AI --> AT1
    AI --> AT2
    AT1 --> A1
    AT2 --> A2
    A1 <--> A2
    CF --> A1
    CF --> A2
    START --> A1
    START --> A2
    
    style DC fill:#ffcdd2
    style AT1 fill:#c8e6c9
    style AT2 fill:#c8e6c9
    style A1 fill:#bbdefb
    style A2 fill:#bbdefb
```

## Component Description

### DALI Core Modules
- **dali_core.pl**: Main engine of the DALI framework
- **active_dali_wi.pl**: Runtime system for agent execution
- **agent_init.pl**: Module for agent initialization
- **rule_parser.pl**: Parser for agent rules
- **communication_fipa.pl**: Implementation of FIPA protocol for communication

### Agent Types (Examples/advanced)
- **agentType1**: Reactive agent with communication capabilities
  - Handles `eventE` and `goE` events
  - Can send messages to other agents
- **agentType2**: Simple agent with basic response
  - Handles `goE` events with minimal output

### Agent Instances
- **agent1**: Instance of agentType1
- **agent2**: Instance of agentType2
- They communicate through the DALI messaging system

### Utilities and Advanced Components
- **Utils Modules**: Debug, file, list and time management
- **Memory Management**: Memory system for agents
- **Meta Reasoning**: Meta-level reasoning capabilities
- **Learning**: Learning module for agents 