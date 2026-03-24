# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased] - 2026-03-18

### Fixed

- **`startmas.bat` (Examples/win/basic):**
  - **Critical Fix:** Corrected the agent building logic which was incorrectly using the "advanced" folder structure (`mas/instances` and `mas/types`) instead of the basic flat `mas/` structure.
  - Enabled `setlocal enabledelayedexpansion` at the script level to fix the `!server_ready!` check, ensuring the script properly waits for the LINDA server ready signal before starting agents.
  - Cleaned up verbose output by removing redundant `PATH` and localized search logs for a more streamlined user experience.
- **`startmas.bat` (Examples/win/advanced):**
  - Streamlined script output by removing redundant environment logs and simplifying the `spwin.exe` discovery logic.

### Added

- **Repository Configuration:**
  - Added `examples/more/DALI-F1-race` as a Git submodule pointing to `https://github.com/Mik1810/f1_race` to include the F1 race simulator example.
- **Documentation:**
  - Created `README2.md` as a comprehensive renovation of the root `README.md`, featuring:
    - New **Prerequisites** section with SICStus Prolog download link and university license activation instructions.
    - Dedicated **Quick Start** sections for Windows (Native Batch) and Unix (tmux), each with screenshots (`win.png`, `unix.png`).
    - Warning note for Windows users regarding SICStus 4.6.0 availability and contact information for the correct installer.
    - New **Testing your MAS** section with Prolog interaction commands and message exchange screenshot.
    - Dedicated **DALI Web Dashboard** section with launch instructions and link to `DASHBOARD.md`.
    - New **Development Setup** section guiding users to create their own DALI MAS using existing examples as boilerplates.
    - Expanded **Examples of Applications** section including the F1 Race Simulator with screenshots and video demo.
    - Registered trademark symbol `®` replacing `(R)` for SICStus Prolog across all documentation.
  - Added `img/f1_race_1.png` and `img/f1_race_2.png` screenshots for the F1 Race Simulator.

## [Unreleased] - 2026-03-17

### Fixed

- **`startmas.bat` (Examples/win/advanced):**
  - Dynamic discovery of `spwin.exe` through system PATH and common installation folders (e.g., `Program Files/SICStus*`).
  - Implemented background execution using `start /B ""` to keep the console clean.
  - Added smart waiting mechanism for `server.txt` creation before launching agent instances.
  - Translated batch logs from Italian to English and changed `[DEBUG]` prefixes to `[LOG]`.
  - Added a smart `pause` at the end of the script to automatically trigger dynamic files cleanup (`work/`, `conf/mas/`, `build/`) while preserving `.gitkeep` placeholders.
- **Unix scripts `startmas.sh` and `makeconf.sh` (Examples/unix/basic & Examples/unix/advanced):**
  - Copied the advanced Unix structure to `Examples/unix/basic` to provide a baseline layout parity.
  - Decoupled `unix/basic` from the complex `mas/instances` topology, redirecting the builder to parse a **flat project structure** so glob expansions work flawlessly natively.
  - Adjusted internal paths from `../../src` to `../../../src` to correctly target the DALI engine due to the deeper nested file structure.
  - Added a `trap cleanup EXIT` hook to `startmas.sh` to automatically delete dynamically built agent files and proxy `.txt` files upon tmux session terminal shutdown, while meticulously preserving necessary `.gitkeep` structural markers.
  - Integrated `tmux kill-session` into the cleanup handler to prevent orphan `SICStus` and server background processes when the terminal emulator window is abruptly closed.
  - Fixed a `Syntax error in read/2` bug in `makeconf.sh` by stripping the `.txt` extension before passing the agent name to Prolog.
  - Added explicit lock file cleanup (`rm -f /tmp/dali_startmas.lock`) to the `EXIT` trap.
- **`makeconf.bat`:**
  - Fixed the output path of temporary files that now land in `conf/mas/` consistently with the DALI base and startup builder, instead of dirtying the root `conf/` folder.
  - Recursively updated nested paths `../../src` to `../../../src` for `win/basic` and `win/advanced` nested setups.
- **DALI Agent Dashboard UI:**
  - Added per-pane command inputs for direct agent interaction, significantly improving the user workflow.
  - Removed the global command bar to optimize screen space and focus on agent-specific interactions.
  - **Zero-Config Transformation:** Removed `config.json` dependency. The dashboard now dynamically discovers agents by scanning the MAS folder structure (Basic/Advanced).
  - **Auto-Start MAS:** The dashboard now automatically triggers a MAS restart on the first page load of a browser session (using `sessionStorage`), providing a "plug-and-play" experience while avoiding interruptions on page refresh.
  - **Premium Color Palette:** Implemented a pre-defined 10-color round-robin palette for agents, replacing random MD5 generation for a more professional look.
  - **Terminal Cleanup:** Suppressed Flask startup banners and Werkzeug request logging to keep the console focused on DALI logs.
  - Updated `README.md` and `DASHBOARD.md` with a Zero-Config instructions.
  - Implemented dynamic `SPLIT_PANES` control using a new `--no-split` flag; the MAS now defaults to a tiled layout when run manually, while the UI automatically disables it for compatibility.
  - Added `-f` as an alias for `--folder` and `-p` as an alias for `--port` in `dashboard.py`.
  - Initialized panes with scroll position at the top to allow viewing of initial logs.
  - Changed the MAS shutdown signal from `SIGKILL` to `SIGTERM` in the dashboard, enabling graceful cleanup.
- **Repository Configuration:**
  - Added `.gitattributes` to enforce correct line endings: `LF` for shell scripts and DALI agent files (`.txt`, `.pl`, `.con`), and `CRLF` for Windows batch files, resolving cross-platform compatibility issues.
- **Unix Scripts `startmas.sh` (Examples/unix/basic & Examples/unix/advanced):**
  - Updated the `trap` command to include `INT` and `TERM` signals in addition to `EXIT`, ensuring reliable cleanup in all shutdown scenarios.
  - Added a persistence loop to keep the script alive in non-interactive mode (when launched by the UI), preventing premature cleanup.
  - Standardized the `SPLIT_PANES=true` default for manual usage while supporting the `--no-split` override for the dashboard.
- **Documentation Renovation:**
  - Completely modernized the root `examples/ReadME.md` with a clear OS-based structure (`win/` vs `unix/`).
  - Converted `Double_Events.txt` to a properly formatted `DOUBLE_EVENTS.md`.
  - Added specific legacy warnings (using GitHub Alerts) for technical workarounds related to older DALI/SICStus versions.
  - Standardized paths and descriptions across all documentation to match the current project layout.
