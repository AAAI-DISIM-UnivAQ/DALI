# Unix Advanced Instances

This folder contains the instance declarations for the Unix advanced example.

## Role Of This Folder

Each `*.txt` file represents one agent instance. The filename becomes the runtime agent name, while the file content identifies which type from `../types/` must be used to build that agent.

## Example

If `agent1.txt` contains:

```text
agentType1
```

then the launcher generates a runtime agent named `agent1` using the source in `../types/agentType1.txt`.

## Practical Meaning

- filename: instance name
- file content: type name

This lets multiple agents share one type definition while still running as separate named agents.

## See Also

- [../types/TYPES.md](../types/TYPES.md)
- [../../UNIX_ADVANCED.md](../../UNIX_ADVANCED.md)
