#!/usr/bin/env python3
"""
DALI Agent Dashboard — Web Dashboard (backend)
Polls tmux panes and serves them as a live web UI.

Configuration is read from ui/config.json — no code changes needed to
add/remove agents or change colours.

Usage:
    bash ui/run.sh
    Open: http://localhost:5000
"""

import json
import signal
import subprocess
import argparse
import atexit
import os
import sys
import time
import threading
from flask import Flask, jsonify, request, send_from_directory

_UI_DIR   = os.path.dirname(os.path.abspath(__file__))
_MAS_DIR  = os.path.normpath(os.path.join(_UI_DIR, ".."))
_CFG_FILE = os.path.join(_UI_DIR, "config.json")
LOG_FILE = os.path.normpath(os.path.join(_MAS_DIR, "log", "restart.log"))
os.makedirs(os.path.dirname(LOG_FILE), exist_ok=True)

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



def _load_config() -> dict:
    """Read config.json and return the parsed dict (with safe defaults)."""
    try:
        with open(_CFG_FILE, encoding="utf-8") as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"[dashboard] WARNING: {_CFG_FILE} not found — using defaults.")
        return {}
    except json.JSONDecodeError as exc:
        print(f"[dashboard] WARNING: could not parse config.json: {exc}")
        return {}


def _pane_entry(p: dict) -> dict:
    """Extract only the UI-relevant keys from a pane/agent config entry."""
    return {
        "id":     p["id"],
        "label":  p.get("label", p["id"]),
        "color":  p.get("color", "#111111"),
        "border": p.get("border", "#888888"),
    }


def _build_panes(cfg: dict) -> list:
    """Build the ordered panes list: infrastructure panes + agent panes."""
    panes  = [_pane_entry(p) for p in cfg.get("panes",  []) if "id" in p]
    agents = [_pane_entry(a) for a in cfg.get("agents", []) if "id" in a]
    return panes + agents


def current_panes() -> list:
    """Return an up-to-date panes list, re-reading config.json each call."""
    return _build_panes(_load_config())


def current_meta() -> dict:
    """Return non-pane config metadata (title, session, accent_color, startmas_path)."""
    cfg = _load_config()
    return {
        "title":          cfg.get("title",          "DALI Agent Dashboard"),
        "session":        cfg.get("session",         SESSION),
        "accent_color":   cfg.get("accent_color",    "#4caf50"),
        "filtered_lines": cfg.get("filtered_lines",  []),
        "startmas_path":  cfg.get("startmas_path",   "startmas.sh"),
    }


# Initialise SESSION from config (can be overridden by --session CLI arg)
_initial_cfg = _load_config()
SESSION: str = _initial_cfg.get("session", "dali_mas")

STATIC_DIR = os.path.join(_UI_DIR, "static")


# tmux helpers

def capture_pane(window_id: str) -> str:
    """Read the last ~400 lines from a tmux pane (plain text, no ANSI)."""
    try:
        r = subprocess.run(
            ["tmux", "capture-pane", "-pt", f"{SESSION}:{window_id}", "-S", "-400"],
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
        ["tmux", "send-keys", "-t", f"{SESSION}:{window_id}", cmd, "Enter"],
        timeout=3,
    )


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
    """Re-read config.json and return the updated panes list (no restart needed)."""
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
    """Kill current MAS and relaunch via startmas.sh."""
    meta = current_meta()
    startmas_abs = os.path.normpath(os.path.join(_MAS_DIR, meta["startmas_path"]))
    t = threading.Thread(
        target=_do_restart,
        args=(startmas_abs,),
        daemon=True,
    )
    t.start()
    return jsonify({"ok": True})


# MAS lifecycle helpers 
def _kill_all():
    """Best-effort kill of every MAS-related process."""
    log("_kill_all start")
    cmds = [
        ["pkill", "-9", "-f", "startmas.sh"],
        ["tmux", "kill-session", "-t", SESSION],
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
        ["bash", startmas_abs],
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
    parser.add_argument("--port",    type=int, default=5000, help="HTTP port (default: 5000)")
    parser.add_argument("--session", default=None,           help="tmux session name (overrides config.json)")
    args = parser.parse_args()

    if args.session:
        SESSION = args.session

    def _shutdown(signum=None, frame=None):
        print("\n[ui] Shutting down — stopping MAS...", flush=True)
        _kill_all()
        print("[ui] MAS stopped.", flush=True)
        sys.exit(0)

    atexit.register(_kill_all)
    signal.signal(signal.SIGTERM, _shutdown)

    meta = current_meta()
    print(f"\n  \u2022  {meta['title']}")
    print(f"  \u25ba  Open in browser: http://localhost:{args.port}\n")
    app.run(host="0.0.0.0", port=args.port, debug=False)
