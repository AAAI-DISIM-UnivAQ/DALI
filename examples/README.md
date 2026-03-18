# DALI MAS Examples

This folder contains various configurations and examples for the DALI Multi-Agent System, organized by Operating System and complexity.

---

## Repository Structure

The examples are now categorized by platform to provide the best native experience:

### [Windows (`win/`)](win/)
Aimed at classic Windows setups using **SICStus Prolog** directly.
*   **[Basic](win/basic)**: Flat structure, no agent types. Each agent runs in its own separate SICStus window.
*   **[Advanced](win/advanced)**: Uses the `mas/types` and `mas/instances` topology.

### [Unix / macOS / WSL2 (`unix/`)](unix/)
Modern, terminal-based environments using **tmux** for process management.
*   **[Basic](unix/basic)**: Simplified structure for Unix, running agents in a flat project layout.
*   **[Advanced](unix/advanced)**: Full-featured MAS with agent types and instances.
*   ** [Web Dashboard](unix/ui)**: The recommended way to monitor and interact with your MAS on Unix. It provides a real-time web interface with "Zero-Config" auto-discovery.

### [More Examples](more/)
A collection of MAS projects and examples derived from student projects and research.

---

## Key Concepts

*   **[Double Events](DOUBLE_EVENTS.md)**: Learn how to handle multiple simultaneous external events (conjunctions) in DALI rules.
*   **[Warnings & Timers](WARNINGS.md)**: Important information about the `t60.` window and default agent behavior.

---

## 🛠️ Troubleshooting & Setup

> [!WARNING]
> **Legacy Versions Note:** The technical issues and workarounds listed below primarily reference **older versions of DALI and SICStus Prolog**. Modern environments and current DALI releases may have already addressed these natively.

### Unix: Fast Restart (TCP Reuse)
If you restart the MAS frequently during debug, you might encounter "Address already in use" errors. To fix this:
```bash
sudo sysctl -w net.ipv4.tcp_tw_reuse=1
# For macOS
sudo sysctl net.inet.tcp.msl=1
```

### Unix: Command History with `rlwrap`
The User Console in a raw terminal might lack command history. You can install `rlwrap`:
```bash
sudo apt-get install rlwrap
```
Then prepend `rlwrap` to the SICStus execution command in `startmas.sh`.

### Unix: 32-bit SICStus on 64-bit Linux
If using an older 32-bit SICStus version on a 64-bit system (e.g., Ubuntu):
```bash
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get install gcc-multilib libc6:i386 libncurses5:i386 libstdc++6:i386
```
