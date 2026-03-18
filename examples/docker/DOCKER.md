# Docker — DALI Multi-Agent System

Run DALI MAS entirely inside Docker containers, with no local installation of SICStus required beyond providing the installer tarball once.

## Prerequisites

| Requirement | Notes |
|---|---|
| Docker Engine ≥ 24 | [Install Docker](https://docs.docker.com/get-docker/) |
| Docker Compose v2 | Included with Docker Desktop |
| SICStus 4.6.0 Linux tarball | [Download from SICStus](https://sicstus.sics.se/download4.html) |

## Setup (one-time)

1. Copy the SICStus Linux x86-64 tar file into the `installer/` directory:

   ```text
   examples/docker/installer/sicstus-4.6.0-x86_64-linux-glibc2.17.tar
   ```

2. Generate your `.env` file by copying the template and populating your SICStus license variables:

   ```bash
   cd examples/docker
   cp .env.example .env
   # Edit .env with your license info
   ```

   The `.env` file must contain:
   ```env
   SICSTUS_LICENSE_SITE=...
   SICSTUS_LICENSE_EXPIRY=...
   SICSTUS_LICENSE_KEY=...
   ```
   > [!WARNING]
   > Remember to use the right UNIX credentials.


3. Build and start the Docker containers:

   ```bash
   docker compose up --build -d
   ```

## Running

Launch all services (LINDA server + agents + web UI) in the background:

```bash
cd examples/docker
docker compose up -d
```

Then open your browser to **http://localhost:5000** to access the interactive web dashboard.

To follow the MAS logs in real time:

```bash
docker compose logs -f dali-mas
```

## Architecture

The system is split into two containers that communicate through a shared Docker volume:

```
┌────────────────────────────────────────────────────────┐
│                  Docker Network (dali-net)             │
│                                                        │
│  ┌─────────────────┐                                   │
│  │ dali-mas        │                                   │
│  │ (startmas.sh)   │                                   │
│  └────────┬────────┘                                   │
│           │ volume: shared                             │
│           │ (tmux.sock, server.txt, restart.flag)      │
│           ↓                                            │
│  ┌─────────────────┐             ┌─────────┐           │
│  │ dali-ui         │ port: 5000  │         │           │
│  │ (Flask backend) │ ←─────────→ │ Browser │           │
│  └─────────────────┘             └─────────┘           │
└────────────────────────────────────────────────────────┘
```

### `dali-mas`
- Writes the SICStus license from environment variables at startup
- Launches `startmas.sh --no-split`, which starts the LINDA server and all agents inside a `tmux` session
- Each agent runs in a dedicated tmux window
- The tmux socket is exposed on the shared volume so `dali-ui` can read agent output
- Writes `server.txt` (LINDA server address) to the shared volume so `dali-ui` knows where to connect
- Signals readiness via Docker healthcheck (`test -s /dali/shared/server.txt`)

### `dali-ui`
- Waits for `dali-mas` to be healthy before starting
- Runs a Flask web server on port 5000
- Reads agent pane output by attaching to the tmux socket on the shared volume
- Sends commands to agents via `tmux send-keys` through the same socket
- Triggers MAS restarts by writing a `restart.flag` file on the shared volume

### Shared Volume
The `shared` volume is the only communication channel between the two containers. It contains:
- `tmux.sock` — tmux socket used by `dali-ui` to read/write agent panes
- `server.txt` — LINDA server address written by `dali-mas`, read by `dali-ui`
- `restart.flag` — written by `dali-ui` to signal `dali-mas` to restart the MAS

## Shutting Down

```bash
docker compose down
```

To also remove the shared volume (full reset):

```bash
docker compose down -v
```

## Notes

- The SICStus tar file is **not included** in this repository and must not be redistributed. See [Licenza_SICStus 4.6.0.txt](./installer/INSTALLER.md).
- The example directory is mounted into both containers so agent definitions in `mas/` can be edited locally without rebuilding the image.