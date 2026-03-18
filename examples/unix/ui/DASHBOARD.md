# DALI Agent Dashboard

A zero-configuration web dashboard for monitoring and interacting with a DALI Multi-Agent System (MAS) running on **tmux**.  
The backend is written in Python (Flask) and serves a single-page UI that automatically discovers agents and reads tmux pane logs in real time.

---

## Structure

```
ui/
├── dashboard.py      ← Flask backend (Zero-Config)
├── requirements.txt  ← Python dependencies (flask only)
├── run.sh            ← launch script (creates venv, installs deps, starts server)
└── static/
    ├── index.html
    ├── style.css
    └── app.js
```

---

## Quick Start

```bash
cd examples/unix
./run.sh --folder ./advanced
```

Open the browser at `http://localhost:5000`.

`run.sh` automatically creates a virtualenv in `ui/.venv/` on the first run and installs `flask`. Subsequent runs reuse the venv and only reinstall dependencies when `requirements.txt` has changed.

---

## Zero-Config Features

### Automatic MAS Auto-Start
The dashboard automatically triggers a MAS restart on the first page load of a browser session. It uses `sessionStorage` to ensure that standard page refreshes do not interrupt a running MAS session, while still providing a seamless "ready-to-go" state for new users.

### Automatic Agent Discovery
The dashboard dynamically scans the provided MAS folder (`--folder`) to identify agents:
- **Advanced Structure**: Looks for `.txt` files in `mas/instances/`.
- **Basic Structure**: Looks for `.txt` files in `mas/`.
- **Filtering**: Ignores `README` files and automatically identifies the LINDA Server and User Console as infrastructure panes.

### Round-Robin Color Palette
Agents are assigned a professional color scheme automatically from a fixed **10-color palette**. The assignment is deterministic and based on the agent's alphabetical order, ensuring a consistent and harmonious visual experience without manual configuration.

### Log Filtering
Common noisy DALI runtime lines (e.g., `External event preconditions not verified...`) are automatically stripped from all panels before display to improve readability.

---

## UI Features

### Log Panels
Each tmux pane is read every **1 second** and displayed in its own coloured panel.  
Three controls are available per panel:

| Button | Function |
|---|---|
| ✕ | Clears the display (local cache only) |
| ↓ | Toggles auto-scroll to the bottom |
| — | Minimises the panel to the bottom tray |

### Per-pane Command Input
Each agent pane (except for the Server) has a dedicated input field at the bottom. This allows sending commands directly to that specific agent:

1. Type the Prolog command (e.g., `go.`) in the agent's input field.
2. Press <kbd>Enter</kbd> to send.

> [!TIP]
> **Interacting with the User Console:**  
> When sending messages from the **User Console**, follow the standard DALI message sequence:
> 1. **Address:** `agent1.`
> 2. **From:** `user.`
> 3. **Message:** `send_message(go, user).`

---

## CLI Options (`dashboard.py`)

| Option | Default | Description |
|---|---|---|
| `--folder`, `-f` | `./advanced` | MAS project folder to monitor |
| `--port`, `-p` | `5000` | HTTP port to listen on |
| `--session`, `-s` | `DALI_session` | tmux session name |

---

## REST API

| Endpoint | Method | Description |
|---|---|---|
| `GET /api/config` | GET | Dynamic configuration (discovered agents, title, session) |
| `GET /api/panes` | GET | Current content of all tmux panes |
| `POST /api/send` | POST | Send a command to a pane (`{"window": "agent1", "cmd": "..."}`) |
| `POST /api/restart` | POST | Kill and restart the MAS |
