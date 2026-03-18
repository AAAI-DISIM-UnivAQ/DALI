#!/usr/bin/env python3
"""
DALI Agent Dashboard — Web Dashboard (backend)
Polls tmux panes and serves them as a live web UI.

Agents are discovered automatically based on the MAS folder structure.
No manual configuration file is needed.

Usage:
    ./run.sh
    Open: http://localhost:5000
"""

import signal
import subprocess
import argparse
import atexit
import os
import sys
import time
import threading
import logging
from flask import Flask, jsonify, request, send_from_directory
import flask.cli

_UI_DIR   = os.path.dirname(os.path.abspath(__file__))

# Globals to be set via argparse
MAS_FOLDER = ""
LOG_FILE = ""
SESSION = "DALI_session"
log_t0: float = 0.0


def log(msg: str) -> None:
    """Write a timestamped line to stdout and log/restart.log."""
    elapsed_ms = int((time.time() - log_t0) * 1000) if log_t0 else 0
    line = f"[PY  ][{elapsed_ms:6d} ms] {msg}"
    print(line, flush=True)
    try:
        with open(LOG_FILE, "a", encoding="utf-8") as _f:
            _f.write(line + "\n")
    except Exception:
        pass


_AGENT_PALETTE = [
    {"bg": "#0a0a18", "border": "#aa88ff"}, # Deep Purple
    {"bg": "#05051a", "border": "#2277ff"}, # Electric Blue
    {"bg": "#081a1a", "border": "#00d4ff"}, # Cyan
    {"bg": "#0a1a0a", "border": "#4caf50"}, # Emerald Green
    {"bg": "#1a1a05", "border": "#cead06"}, # Gold/Yellow
    {"bg": "#1a0f05", "border": "#ff9800"}, # Orange
    {"bg": "#1a0505", "border": "#f44336"}, # Soft Red
    {"bg": "#150a15", "border": "#e91e63"}, # Pink/Magenta
    {"bg": "#121212", "border": "#9c27b0"}, # Purple
    {"bg": "#101010", "border": "#607d8b"}, # Blue Gray
]


def discover_agents() -> list:
    """Scan MAS_FOLDER for agents based on typical DALI structures."""
    agents = []
    
    # Pattern 1: Advanced (mas/instances/*.txt)
    instances_dir = os.path.join(MAS_FOLDER, "mas", "instances")
    if os.path.isdir(instances_dir):
        patterns = [os.path.join(instances_dir, "*.txt")]
    else:
        # Pattern 2: Basic (mas/*.txt)
        patterns = [os.path.join(MAS_FOLDER, "mas", "*.txt")]

    import glob
    for p in patterns:
        for f in glob.glob(p):
            name = os.path.splitext(os.path.basename(f))[0]
            if name.lower() == "readme": continue
            agents.append({
                "id": name,
                "label": name.replace("_", " ").title()
            })
    
    sorted_agents = sorted(agents, key=lambda x: x["id"])
    
    # Assign colors round-robin based on sorted position
    for i, agent in enumerate(sorted_agents):
        palette = _AGENT_PALETTE[i % len(_AGENT_PALETTE)]
        agent["color"]  = palette["bg"]
        agent["border"] = palette["border"]

    return sorted_agents


def current_panes() -> list:
    """Build the ordered panes list: infrastructure + discovered agents."""
    infra = [
        {"id": "server", "label": "Server (LINDA)", "color": "#121212", "border": "#555555"},
        {"id": "user",   "label": "User Console",   "color": "#0b180b", "border": "#4caf50"}
    ]
    return infra + discover_agents()


def current_meta() -> dict:
    """Return UI metadata with discovered context."""
    return {
        "title":          "DALI Agent Dashboard",
        "session":        SESSION,
        "accent_color":   "#4caf50",
        "filtered_lines": [
            "External event preconditions not verified: no DeltaTime",
            "This is updated list:",
            "This is list without duplicates:",
            "This is list of past event:",
            "This event is first events:",
            "Do not arrive all events"
        ],
        "startmas_path":  "startmas.sh",
    }


# Initialise defaults
SESSION: str = "dali_mas"

STATIC_DIR = os.path.join(_UI_DIR, "static")


# tmux helpers

def capture_pane(window_id: str) -> str:
    """Read the last ~400 lines from a tmux pane (plain text, no ANSI)."""
    try:
        r = subprocess.run(
            ["tmux", "-S", "/dali/shared/tmux.sock", "capture-pane", "-pt", f"{SESSION}:{window_id}", "-S", "-400"],
            capture_output=True, text=True, timeout=3,
        )
        if r.returncode != 0:
            return f"[pane '{window_id}' not available — is the MAS running?]"
        return r.stdout
    except FileNotFoundError:
        return "[tmux not found — run this inside WSL or a Linux terminal]"
    except subprocess.TimeoutExpired:
        return "[timeout reading pane]"
    except Exception as e:
        return f"[error: {e}]"


def send_keys(window_id: str, cmd: str) -> None:
    """Type a command into a tmux pane as if the user pressed Enter."""
    subprocess.run(
        ["tmux", "-S", "/dali/shared/tmux.sock", "send-keys", "-t", f"{SESSION}:{window_id}", cmd, "Enter"],
        timeout=3,
    )


# Silence Flask/Werkzeug noise
flask.cli.show_server_banner = lambda *args: None
logging.getLogger('werkzeug').setLevel(logging.ERROR)

app = Flask(__name__, static_folder=STATIC_DIR, static_url_path="/static")


@app.route("/")
def index():
    return send_from_directory(STATIC_DIR, "index.html")


@app.route("/api/config")
def api_config():
    meta  = current_meta()
    panes = current_panes()
    return jsonify({
        "title":          meta["title"],
        "session":        meta["session"],
        "accent_color":   meta["accent_color"],
        "filtered_lines": meta["filtered_lines"],
        "panes":          panes,
    })


@app.route("/api/reload-config")
def api_reload_config():
    """Trigger a re-discovery of agents and return the updated panes list."""
    return jsonify({"ok": True, "panes": current_panes()})


@app.route("/api/panes")
def api_panes():
    return jsonify({p["id"]: capture_pane(p["id"]) for p in current_panes()})


@app.route("/api/send", methods=["POST"])
def api_send():
    data = request.get_json()
    if not data or "window" not in data or "cmd" not in data:
        return jsonify({"error": "missing fields"}), 400
    send_keys(data["window"], data["cmd"])
    return jsonify({"ok": True})


@app.route("/api/restart", methods=["POST"])
def api_restart():
    """Trigger MAS restart by touching a shared flag on the volume."""
    try:
        open("/dali/shared/restart.flag", "w").close()
    except Exception as e:
        log(f"Failed to trigger restart IPC: {e}")
    return jsonify({"ok": True})


# MAS lifecycle helpers (for strictly 2-container architecture these are mostly no-ops from ui side)
def _kill_all():
    """Best-effort kill of every MAS-related process."""
    log("_kill_all start")
    cmds = [
        ["pkill", "-f", "startmas.sh"],
        ["pkill", "-9", "-f", "active_server_wi.pl"],
        ["pkill", "-9", "-f", "active_dali_wi.pl"],
        ["pkill", "-9", "-f", "active_user_wi.pl"],
    ]

    def _run(cmd):
        try:
            subprocess.run(cmd, timeout=5, capture_output=True)
        except Exception:
            pass

    threads = [threading.Thread(target=_run, args=(c,), daemon=True) for c in cmds]
    for th in threads:
        th.start()
    for th in threads:
        th.join(timeout=5)
    log("  parallel pkill/tmux done")

    _PATTERNS = ["active_server_wi.pl", "active_dali_wi.pl", "active_user_wi.pl"]
    for attempt in range(10):
        still_alive = False
        for pat in _PATTERNS:
            r = subprocess.run(["pgrep", "-f", pat], capture_output=True)
            if r.returncode == 0:
                still_alive = True
                break
        if not still_alive:
            log(f"  pgrep confirmed dead (attempt {attempt+1})")
            break
        time.sleep(0.1)
    else:
        log("  WARNING: some SICStus processes did not die in 1 s")

    time.sleep(0.2)
    log("_kill_all done")


def _launch(startmas_abs: str):
    """Launch startmas.sh as a detached background process."""
    log(f"_launch start — {startmas_abs}")
    if not os.path.isfile(startmas_abs):
        log(f"  ERROR: startmas.sh not found at {startmas_abs}")
        print(f"[restart] ERROR: startmas.sh not found at {startmas_abs}", flush=True)
        return
    subprocess.Popen(
        ["bash", startmas_abs, "--no-split"],
        cwd=os.path.dirname(startmas_abs),
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    log("  startmas.sh launched as detached process")


def _do_restart(startmas_abs: str):
    """Kill the MAS and relaunch via startmas.sh."""
    import datetime
    global log_t0
    log_t0 = time.time()
    th = threading.current_thread()
    header = (
        f"\n=== restart at {datetime.datetime.now().isoformat()} "
        f"thread={th.name} tid={th.ident} ===\n"
    )
    try:
        with open(LOG_FILE, "a", encoding="utf-8") as _f:
            _f.write(header)
    except Exception:
        pass
    log(f"_do_restart START thread={th.name} tid={th.ident}")
    _kill_all()
    _launch(startmas_abs)
    log("_do_restart DONE — startmas.sh running")


# Entry point 
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="DALI Agent Dashboard")
    parser.add_argument("--port", "-p", type=int, default=5000, help="HTTP port (default: 5000)")
    parser.add_argument("--session", default=None,           help="tmux session name (overrides config.json)")
    parser.add_argument("--folder", "-f", default="./advanced", help="MAS folder (e.g. ./basic or ./advanced)")
    args = parser.parse_args()

    MAS_FOLDER = os.path.abspath(args.folder)
    if not os.path.isdir(MAS_FOLDER):
        print(f"ERROR: folder not found: {MAS_FOLDER}")
        sys.exit(1)

    LOG_FILE = os.path.normpath(os.path.join(MAS_FOLDER, "log", "restart.log"))
    os.makedirs(os.path.dirname(LOG_FILE), exist_ok=True)

    SESSION = args.session if args.session else "DALI_session"

    def _shutdown(signum=None, frame=None):
        print("\n[ui] Shutting down — stopping MAS...", flush=True)
        _kill_all()
        print("[ui] MAS stopped.", flush=True)
        sys.exit(0)

    atexit.register(_kill_all)
    signal.signal(signal.SIGTERM, _shutdown)

    meta = current_meta()
    print(f"\n  \u2022  {meta['title']}")
    print(f"  \u25ba  MAS Folder: {MAS_FOLDER}")
    print(f"  \u25ba  Open in browser: http://localhost:{args.port}\n")
    app.run(host="0.0.0.0", port=args.port, debug=False)
