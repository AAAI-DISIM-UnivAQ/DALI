# DALI Unix Basic Example

This folder contains the simplest Unix-like DALI example. It is designed for Linux, macOS, and WSL2 environments where agents are launched through `tmux`.

## Layout

- `startmas.sh`: starts the LINDA server, the user agent, and every agent in `mas/`
- `mas/`: flat agent definitions; each `*.txt` file is a complete DALI agent source
- `conf/`: helper scripts and generated configuration files used at startup
- `work/`: runtime copies of the generated agents
- `log/`: restart and launcher logs

## How It Works

Unlike the advanced layout, the basic example does not separate agent types from agent instances. Every file in `mas/` is already a runnable agent and is copied directly into the runtime workspace before startup.

## Running The Example

Requirements:

- SICStus Prolog installed and reachable at the path configured in `startmas.sh`
- `tmux` installed on the system

From this directory, run:

```bash
./startmas.sh
```

This opens a `tmux` session named `DALI_session`, starts the LINDA server, launches the user console, and then starts one pane or window for each agent in `mas/`.

## Notes

- The launcher cleans dynamic files in `work/`, `build/`, and `conf/mas/` on startup and shutdown.
- The LINDA server starts from port `3010` and automatically retries higher ports if needed.
- For the web UI workflow, see [../ui/DASHBOARD.md](../ui/DASHBOARD.md).
