# Docker — DALI Multi-Agent System

Run DALI MAS entirely inside Docker containers, with no local installation of SICStus required beyond providing the installer tarball once.

## Prerequisites

| Requirement | Notes |
|---|---|
| Docker Engine ≥ 24 | [Install Docker](https://docs.docker.com/get-docker/) |
| Docker Compose v2 | Included with Docker Desktop |
| SICStus 4.6.0 Linux tarball | [Download from SICStus](https://sicstus.sics.se/download4.html) |

## Setup (one-time)

1. Copy the SICStus Linux x86-64 tar file into the `installer/` directory of the specific example you want to run. For instance, for the basic example:

   ```text
   examples/docker/basic/installer/sicstus-4.6.0-x86_64-linux-glibc2.17.tar
   ```

2. Generate your `.env` file by copying the template and populating your SICStus license variables. You must do this **for each example you want to run**. For instance, for the basic example:

   ```bash
   cd examples/docker/basic
   cp .env.example .env
   # Edit .env with your license info
   ```

3. Build the Docker image from your chosen example directory:

   ```bash
   docker compose up --build -d
   ```

## Running the Examples

We provide two isolated `docker-compose` setups: one for the **basic** example and one for the **advanced** example.

### 1. Basic Example

Launch all services (server + agents + web UI) in the background:

```bash
cd examples/docker/basic
docker compose up -d
```

Then open your browser to **http://localhost:5000** to access the interactive web dashboard.

### 2. Advanced Example

Launch all services (server + agents + web UI) in the background:

```bash
cd examples/docker/advanced
docker compose up -d
```

Then open your browser to **http://localhost:5000** to access the interactive web dashboard.

## Architecture

```
┌──────────────────────────────────────────────────────┐
│                  Docker Network (dali-net)            │
│                                                       │
│  ┌─────────────────┐   volume: tmux socket & flag     │
│  │ dali-mas        │ ──────────────────────────────┐  │
│  │ (startmas.sh)   │                               │  │
│  └─────────────────┘                               │  │
│                                                    ↓  │
│  ┌─────────────────┐                        ┌─────────┐
│  │ dali-ui         │ ← localhost:5000 ─ Web │ Browser │
│  │ (Flask backend) │                        └─────────┘
│  └─────────────────┘                                  │
└──────────────────────────────────────────────────────┘
```

## Shutting Down

In the `basic` or `advanced` folder, run:

```bash
docker compose down
```

This stops all containers and removes the shared volume (server.txt, logs).

## Notes

- The SICStus tarball is **not included** in this repository and must not be redistributed. See `installer/Licenza_SICStus 4.6.0.txt`.
