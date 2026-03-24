# ──────────────────────────────────────────────────────────────────
# DALI MAS — Docker image (Advanced Example)
# Base: Debian Bookworm (slim)
#
# PREREQUISITES (before building):
#   Place the SICStus 4.6.0 Linux x86-64 tarball inside installer/:
#     installer/sicstus-4.6.0-x86_64-linux-glibc2.17.tar
#
# BUILD (from advanced directory):
#   docker compose up --build
#
# ──────────────────────────────────────────────────────────────────
FROM debian:bookworm-slim

LABEL maintainer="AAAI Research Group — University of L'Aquila"
LABEL description="DALI Multi-Agent System with SICStus Prolog"

# ── System dependencies ───────────────────────────────────────────
RUN apt-get update && apt-get install -y --no-install-recommends \
        bash \
        coreutils \
        findutils \
        tmux \
        procps \
        iproute2 \
        netcat-openbsd \
        libgmp10 \
        libssl3 \
        libncurses5 \
        libtcl8.6 \
        libtk8.6 \
        python3 \
        python3-pip \
        python3-flask \
    && rm -rf /var/lib/apt/lists/*

# ── SICStus installation ──────────────────────────────────────────
ARG SICSTUS_TARBALL=sp-4.6.0-x86_64-linux-glibc2.17.tar
COPY examples/docker/installer/${SICSTUS_TARBALL} /tmp/sicstus_installer.tar

RUN mkdir -p /tmp/sicstus_unpacked && \
    tar -xf /tmp/sicstus_installer.tar -C /tmp/sicstus_unpacked --strip-components=1 && \
    mkdir -p /opt/sicstus && \
    tar -xf /tmp/sicstus_unpacked/sicstus.tar.compress -C /opt/sicstus && \
    rm -rf /tmp/sicstus_unpacked /tmp/sicstus_installer.tar

ENV SICSTUS_HOME=/opt/sicstus
ENV PATH="${SICSTUS_HOME}/bin:${PATH}"

# ── DALI source code ─────────────────────────────────────────────
WORKDIR /dali
COPY src/ /dali/src/

# ── Entrypoints ───────────────────────────────────────────────────
COPY examples/docker/entrypoint-mas.sh /entrypoint-mas.sh
COPY examples/docker/entrypoint-ui.sh  /entrypoint-ui.sh
RUN chmod +x /entrypoint-mas.sh /entrypoint-ui.sh

ENTRYPOINT ["/bin/bash"]
