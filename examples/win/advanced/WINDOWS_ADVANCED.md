# DALI Windows Advanced Example

This folder contains the full Windows DALI example with a typed multi-agent layout. It is designed for native Windows environments using batch scripts and separate SICStus Prolog windows.

## Layout

- `startmas.bat`: builds runtime agents from types and instances, then launches the MAS
- `mas/types/`: reusable DALI agent templates
- `mas/instances/`: instance declarations that map agent names to a type
- `conf/`: helper batch scripts and generated configuration files used at startup
- `work/`: runtime copies of the generated agents
- `log/`: runtime log support folders

## How It Works

The launcher reads each file in `mas/instances/`, treats its content as a type name, loads the corresponding file from `mas/types/`, and writes a generated runtime agent into `build/` and `work/`.

This makes the advanced layout useful when multiple agents share the same DALI code but need different instance names.

## Running The Example

Requirements:

- SICStus Prolog installed on Windows
- `spwin.exe` available in `PATH` or discoverable in common SICStus installation folders

From this directory, run:

```bat
startmas.bat
```

The script starts the LINDA server, waits for `server.txt`, launches the user console, builds all declared agents, and opens the MAS in separate SICStus windows.

## Related Documentation

- [mas/types/TYPES.md](mas/types/TYPES.md)
- [mas/instances/INSTANCES.md](mas/instances/INSTANCES.md)
