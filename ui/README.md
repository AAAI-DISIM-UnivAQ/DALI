# DALI Agent Dashboard

A generic web dashboard for monitoring and interacting with a DALI Multi-Agent System (MAS) running on **tmux**.  
The backend is written in Python (Flask) and serves a single-page UI that reads tmux pane logs in real time.

---

## Structure

```
ui/
├── config.json       ← single source of truth for all configuration
├── dashboard.py      ← Flask backend
├── requirements.txt  ← Python dependencies (flask only)
├── run.sh            ← launch script (creates venv, installs deps, starts server)
└── static/
    ├── index.html
    ├── style.css
    └── app.js
```

---

## Quick start

```bash
bash ui/run.sh
# or with a custom port
bash ui/run.sh --port 8080
```

Open the browser at `http://localhost:5000`.

`run.sh` automatically creates a virtualenv in `ui/.venv/` on the first run and installs `flask`. Subsequent runs reuse the venv and only reinstall dependencies when `requirements.txt` has changed.

### `dashboard.py` CLI options

| Option | Default | Description |
|---|---|---|
| `--port` | `5000` | HTTP port to listen on |
| `--session` | from `config.json` | tmux session name (overrides config) |

---

## Configuration — `config.json`

All aspects of the UI are controlled by this file. No backend restart is needed to add or remove agents: the UI hot-reloads every 5 seconds.

```jsonc
{
  "title":          "DALI Agent Dashboard",          // title shown in the header and browser tab
  "session":        "DALI_session",                  // tmux session name (must match startmas.sh)
  "startmas_path":  "Examples/advanced/startmas.sh", // path to startmas.sh relative to the repo root
  "accent_color":   "#4caf50",                       // colour of the status LED and spinner
  "panes": [                                         // infrastructure panes — always shown, in order
    {
      "id":     "server",                            // must match the tmux window name
      "label":  "Server (LINDA)",
      "color":  "#121212",
      "border": "#555555"
    },
    { 
        "id": "user", 
        "label": "User Console", 
        "color": "#0b180b", 
        "border": "#4caf50" 
    }
  ],

  "agents": [   // agent panes — hot-reloaded every 5 s
    { 
        "id": "agent1", 
        "label": "Agent 1", 
        "color": "#0a0a18", 
        "border": "#aa88ff" 
    },
    { 
        "id": "agent2", 
        "label": "Agent 2", 
        "color": "#05051a", 
        "border": "#2277ff" 
    }
  ],

  "filtered_lines": 
  [  // noisy DALI lines to hide in the UI
    "External event preconditions not verified: no DeltaTime",
    "This is updated list:"
  ]
}
```

### Top-level fields

| Field | Type | Default | Description |
|---|---|---|---|
| `title` | string | `"DALI Agent Dashboard"` | Title in the header and browser tab |
| `session` | string | `"dali_mas"` | tmux session name — **must match** the name used in `startmas.sh` |
| `startmas_path` | string | `"startmas.sh"` | Path to `startmas.sh` relative to the repo root |
| `accent_color` | hex color | `"#4caf50"` | Colour of the status LED and restart spinner |
| `filtered_lines` | array | `[]` | Lines to strip from all panes before display |

### Pane / agent fields

| Field | Type | Required | Description |
|---|---|---|---|
| `id` | string | ✔ | **tmux window name** (`SESSION:id`) |
| `label` | string | ✘ | Label shown in the UI (default: `id`) |
| `color` | hex color | ✘ | Pane background colour (default: `#111111`) |
| `border` | hex color | ✘ | Border and title colour (default: `#888888`) |

### Difference between `panes` and `agents`

- **`panes`** — fixed infrastructure (LINDA server, user console, etc.); always shown in the defined order and excluded from the command bar dropdown.
- **`agents`** — DALI MAS agents; appended after the fixed panes and available in the command bar for sending interactive commands.

---

## UI features

### Log panels

Each tmux pane is read every **1 second** and displayed in its own coloured panel.  
Three controls are available per panel:

| Button | Function |
|---|---|
| ✕ | Clears the display (the tmux buffer is untouched; new logs keep arriving) |
| ↓ | Toggles auto-scroll to the bottom (automatically disabled when scrolling manually) |
| — | Minimises the panel to the bottom tray; click the chip to restore it |

### Line filter

Strings listed in `filtered_lines` are stripped from all panels before display, removing verbose DALI runtime noise.

### Command bar

A bar at the bottom of the page allows sending commands directly to a tmux pane:

1. Select the target agent from the dropdown.
2. Type the Prolog command (e.g. `send_message(event, sender).`).
3. Press **Send** or <kbd>Enter</kbd>.

### Configuration hot-reload

Every 5 seconds the UI fetches `/api/config`. If the agent list has changed since the last read (agents added or removed in `config.json`), the grid is rebuilt automatically.

### Restart MAS

The **⟳ Restart MAS** button in the header:
1. Sends a `POST /api/restart` request to the backend.
2. The backend kills all SICStus processes and the tmux session.
3. Re-launches the script specified in `startmas_path` from its own directory.
4. The UI shows a waiting overlay until the LINDA server pane becomes available again.

---

## `startmas.sh` requirements

The UI reads each agent via `tmux capture-pane -t SESSION:ID`. For panels to populate correctly, **every agent must have its own tmux window** whose name matches the corresponding `id` in `config.json`.

In `startmas.sh` set:

```bash
SPLIT_PANES=false
```

With `SPLIT_PANES=true` all agents share the `server` window as split panes: the UI cannot read them individually and all agent panels remain empty.

---

## REST API

| Endpoint | Method | Description |
|---|---|---|
| `GET /api/config` | GET | Full configuration (title, session, panes, filtered_lines) |
| `GET /api/reload-config` | GET | Force re-read of `config.json` |
| `GET /api/panes` | GET | Current content of all tmux panes |
| `POST /api/send` | POST | Send a command to a pane (`{"window": "agent1", "cmd": "..."}`) |
| `POST /api/restart` | POST | Kill and restart the MAS |

---

## Restart log

Every restart operation is recorded with a timestamp and elapsed time in:

```
log/restart.log
```

(relative to the MAS directory, one level above `ui/`).
