# DALI Windows Basic Example

This folder contains the simplest Windows DALI example. It is designed for native Windows environments where agents are launched through batch scripts and separate SICStus Prolog windows.

## Layout

- `startmas.bat`: starts the LINDA server, the user agent, and every agent in `mas/`
- `mas/`: flat agent definitions; each `*.txt` file is a complete DALI agent source
- `conf/`: helper batch scripts and generated configuration files used at startup
- `work/`: runtime copies of the generated agents
- `log/`: runtime log support folders

## How It Works

Unlike the advanced layout, the basic example does not separate agent types from agent instances. Every file in `mas/` is already a runnable agent and is copied directly into the runtime workspace before startup.

## Running The Example

Requirements:

- SICStus Prolog installed on Windows
- `spwin.exe` available in `PATH` or discoverable in common SICStus installation folders

From this directory, run:

```bat
startmas.bat
```

The script locates `spwin.exe`, cleans previous runtime files, starts the LINDA server, launches the user console, and then opens one SICStus window for each agent in `mas/`.

## Notes

- The launcher removes generated files from `work/`, `build/`, and `conf/mas/` before and after a run.
- The LINDA server starts from port `3010`.
- Each agent runs in its own native Windows Prolog window.
