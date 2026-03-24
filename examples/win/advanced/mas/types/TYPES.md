# Windows Advanced Types

This folder contains the reusable agent type definitions for the Windows advanced example.

## Role Of This Folder

Each `*.txt` file in this directory is a DALI source template. These files are not launched directly by name. Instead, the Windows startup scripts read the type name requested by each instance in `../instances/` and generate the runtime agent files from the matching type source.

## Naming Convention

- `agentType1.txt` defines the source for the type `agentType1`
- `agentType2.txt` defines the source for the type `agentType2`

An instance file containing `agentType1` will generate an agent using `agentType1.txt`.

## See Also

- [../instances/INSTANCES.md](../instances/INSTANCES.md)
