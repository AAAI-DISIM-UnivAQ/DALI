# DALI Unix Advanced Example

This folder contains the full Unix-like DALI example with a typed multi-agent layout. It is meant for Linux, macOS, and WSL2 environments using `tmux`.

## Layout

- `startmas.sh`: builds runtime agents from types and instances, then launches the MAS
- `mas/types/`: reusable DALI agent templates
- `mas/instances/`: instance declarations that map agent names to a type
- `conf/`: helper scripts and generated configuration files used at startup
- `work/`: runtime copies of the generated agents
- `log/`: restart and launcher logs

## How It Works

The launcher reads each file in `mas/instances/`, treats its content as a type name, loads the corresponding file from `mas/types/`, and writes a generated runtime agent into `build/` and `work/`.

This makes the advanced layout useful when multiple agents share the same DALI code but need different instance names.

## Running The Example

Requirements:

- SICStus Prolog installed and reachable at the path configured in `startmas.sh`
- `tmux` installed on the system

From this directory, run:

```bash
./startmas.sh
```

The script starts the LINDA server, waits for the selected port, launches the user console, builds all declared agents, and opens the MAS inside a `tmux` session named `DALI_session`.

## Related Documentation

- [mas/types/TYPES.md](mas/types/TYPES.md)
- [mas/instances/INSTANCES.md](mas/instances/INSTANCES.md)
- [../ui/DASHBOARD.md](../ui/DASHBOARD.md)
