# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased] - 2026-03-17

### Fixed
- **`startmas.bat` (Examples/basic) & (Examples/win/advanced) & (Examples/win/basic):** 
  - Replaced hardcoded Italian path `C:\Programmi` with system variables `%PROGRAMFILES%` and `%PROGRAMFILES(x86)%` for OS language-agnostic search.
  - Added support for auto-discovery of SICStus in paths containing version declarations like `SICStus*\bin`.
  - Fixed a bug when searching paths with spaces by correctly handling strings with double quotes (e.g. `"SICStus Prolog VC16 4.6.0"`).
  - Fixed DALI components startup executable by replacing CLI `sicstus.exe` with native windowed UI `spwin.exe`, executing it in background (`start /B ""`) to hide multiple cmd sessions from the user.
  - Replaced fake hardware "sleep" via `ping -n 3 127.0.0.1` with the smart construct adopted in LINUX versions (`startmas.sh`): it now waits for `server.txt` creation by LINDA server using the native `timeout`.
  - Cleaned up pending process removal logic by automatically injecting `taskkill` to shut down previous executions, preventing interfaces from overlapping on unclean ports.
  - Updated agents start directive: it now launches `--goal "start0('conf/mas/%%~nxG')."` going through the real config file instead of trying to load an inexistent prolog goal `-goal !agent!.` in `active_dali_wi.pl`.
  - Added auto support for the split `mas/instances` and `mas/types` structure replicating Linux build features.
  - Added a smart `pause` at the end of the script to automatically trigger dynamic files cleanup (`work/`, `conf/mas/`, `build/`) upon exit while purposely preserving `.gitkeep` placeholders.
  - Translated batch logs from Italian to English and changed `[DEBUG]` prefixes to `[LOG]`.
- **`makeconf.bat`:** 
  - Fixed the output path of temporary files that now land in `conf/mas/` consistently with the DALI base and startup builder, instead of dirtying the root `conf/` folder.
  - Recursively updated nested paths `../../src` to `../../../src` for `win/basic` and `win/advanced` nested setups.
