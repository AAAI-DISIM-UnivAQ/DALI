/* app.js — DALI Agent Dashboard */

let PANES          = [];
let FILTERED_LINES = [];

// clearSnapshot[id] = text displayed at last clear → only show new text after it
const clearSnapshot = {};
// Whether auto-scroll is active for each pane
const pinned = {};
// Whether each pane is minimized
const minimized = {};
// Last full text received from API per pane (for snapshot diff)
const currentText = {};

/* Bootstrap: fetch config, build UI, start polling */
fetch('/api/config')
  .then(r => r.json())
  .then(cfg => {
    PANES          = cfg.panes;
    FILTERED_LINES = cfg.filtered_lines || [];

    // Apply title
    const titleEl = document.getElementById('header-title');
    if (titleEl && cfg.title) titleEl.textContent = cfg.title;
    document.title = cfg.title || 'DALI Dashboard';

    // Apply accent colour as a CSS variable
    if (cfg.accent_color) {
      document.documentElement.style.setProperty('--accent', cfg.accent_color);
    }

    buildGrid();
    buildAgentSelect();
    poll();
    setInterval(poll,       1000);
    setInterval(syncConfig, 5000);  // hot-reload: detect agents added/removed
  })
  .catch(() => {
    document.getElementById('lbl').textContent = 'config error';
  });

/* Build grid panes  */
function buildGrid() {
  const g = document.getElementById('grid');
  g.innerHTML = '';
  const tray = document.getElementById('minimized-tray');
  if (tray) tray.innerHTML = '';

  PANES.forEach(p => {
    pinned[p.id]      = true;
    minimized[p.id]   = false;
    currentText[p.id] = '';

    const d = document.createElement('div');
    d.className = 'pane';
    d.id        = 'pane-' + p.id;
    d.style.cssText = `background:${p.color};border-color:${p.border}44`;

    d.innerHTML =
      `<div class="pane-hdr" style="background:${p.border}1a;color:${p.border}">` +
        `<span>${p.label}</span>` +
        `<span class="pane-hdr-btns">` +
          `<button class="pane-btn" id="clear-${p.id}" onclick="clearPane('${p.id}')" title="Clear">&#10005;</button>` +
          `<button class="pane-btn" id="pin-${p.id}"   onclick="togglePin('${p.id}')" title="Toggle auto-scroll">&#8595;</button>` +
          `<button class="pane-btn" id="min-${p.id}"   onclick="toggleMinimize('${p.id}')" title="Minimize / Expand">&#8212;</button>` +
        `</span>` +
      `</div>` +
      `<div class="pane-body" id="p-${p.id}"></div>`;

    d.querySelector('.pane-body').addEventListener('scroll', function () {
      pinned[p.id] = this.scrollTop + this.clientHeight >= this.scrollHeight - 20;
      updatePinIcon(p.id);
    });
    g.appendChild(d);
  });
}

function buildAgentSelect() {
  const sel = document.getElementById('tgt');
  sel.innerHTML = '';
  PANES.filter(p => p.id !== 'server').forEach(p => {
    const o = document.createElement('option');
    o.value       = p.id;
    o.textContent = p.id;
    sel.appendChild(o);
  });
}

/* Pin / scroll helpers */
function updatePinIcon(id) {
  const el = document.getElementById('pin-' + id);
  if (el) el.classList.toggle('active', !pinned[id]);
}
function togglePin(id) {
  pinned[id] = !pinned[id];
  updatePinIcon(id);
  if (pinned[id]) {
    const el = document.getElementById('p-' + id);
    if (el) el.scrollTop = el.scrollHeight;
  }
}

/* Minimize helpers  */
function toggleMinimize(id) {
  const pane = document.getElementById('pane-' + id);
  if (!pane) return;

  minimized[id] = true;
  pane.style.display = 'none';

  const cfg   = PANES.find(p => p.id === id);
  const color = cfg ? cfg.border : '#888';
  const label = cfg ? cfg.label  : id;

  const tray = document.getElementById('minimized-tray');
  const chip = document.createElement('button');
  chip.className = 'mini-chip';
  chip.id        = 'chip-' + id;
  chip.textContent = label;
  chip.style.cssText = `color:${color};border-color:${color}66`;
  chip.title = 'Restore ' + label;
  chip.addEventListener('click', () => restorePane(id));
  tray.appendChild(chip);
}

function restorePane(id) {
  minimized[id] = false;
  const pane = document.getElementById('pane-' + id);
  if (pane) pane.style.display = '';
  const chip = document.getElementById('chip-' + id);
  if (chip) chip.remove();
  const btn = document.getElementById('min-' + id);
  if (btn) { btn.innerHTML = '&#8212;'; btn.classList.remove('active'); }
}

/* Clear helpers ─ */
function clearPane(id) {
  clearSnapshot[id] = currentText[id] || '';
  const el = document.getElementById('p-' + id);
  if (el) el.textContent = '';
}
function clearAll() {
  PANES.forEach(p => clearPane(p.id));
}

/* Restart overlay  */
let restarting        = false;
let restartTimer      = null;
let _serverReadySince = 0;

function showOverlay(msg, sub) {
  document.getElementById('overlay-msg').textContent = msg;
  document.getElementById('overlay-sub').textContent = sub || '';
  document.getElementById('overlay').classList.add('visible');
}
function hideOverlay() {
  if (restartTimer) { clearTimeout(restartTimer); restartTimer = null; }
  document.getElementById('overlay').classList.remove('visible');
}

async function restartMas() {
  if (restarting) return;
  restarting = true;
  _serverReadySince = 0;

  showOverlay('Restarting MAS\u2026', 'Sending kill signal\u2026');
  try {
    const r    = await fetch('/api/restart', { method: 'POST' });
    const data = await r.json();
    if (!data.ok) {
      restarting = false;
      hideOverlay();
      alert('Restart failed: ' + (data.reason || data.error || 'unknown error'));
      return;
    }
  } catch (e) {
    restarting = false;
    hideOverlay();
    alert('Restart request failed: ' + e);
    return;
  }
  clearAll();
  showOverlay('Waiting for agents\u2026', 'LINDA server starting\u2026');

  // Safety net: hide overlay after 90 s if MAS never comes back
  restartTimer = setTimeout(() => {
    if (restarting) {
      restarting = false;
      hideOverlay();
      document.getElementById('lbl').textContent = 'restart timeout — check MAS';
    }
  }, 90000);
}

/* Noise filter  */
function filterNoise(text) {
  if (!FILTERED_LINES.length) return text;
  return text
    .split('\n')
    .filter(line => !FILTERED_LINES.some(f => line.includes(f)))
    .join('\n');
}

/**
 * Given the full text from tmux, return only the part the user
 * should see (everything after the clear snapshot, if any).
 */
function visibleText(id, full) {
  const snap = clearSnapshot[id];
  if (!snap) return full;
  const idx = full.indexOf(snap);
  if (idx !== -1) return full.slice(idx + snap.length);
  delete clearSnapshot[id];
  return full;
}

/* Config sync: detect agents added/removed  */
function syncConfig() {
  fetch('/api/config')
    .then(r => r.json())
    .then(cfg => {
      const newIds  = cfg.panes.map(p => p.id).join(',');
      const currIds = PANES.map(p => p.id).join(',');
      if (newIds !== currIds) {
        PANES          = cfg.panes;
        FILTERED_LINES = cfg.filtered_lines || [];
        buildGrid();
        buildAgentSelect();
      }
    })
    .catch(() => {});
}

/* Polling  */
let failCount = 0;

function poll() {
  fetch('/api/panes')
    .then(r => r.json())
    .then(data => {
      failCount = 0;
      document.getElementById('led').className = 'on';
      document.getElementById('lbl').textContent = 'live \u2022 1s refresh';

      if (restarting) {
        // Wait until the LINDA server pane is available to declare the MAS up.
        const serverText  = data['server'] || '';
        const userText    = data['user']   || '';
        const serverReady = serverText && !serverText.startsWith('[pane');
        const userReady   = userText   && !userText.startsWith('[pane');

        if (serverReady && !_serverReadySince) {
          _serverReadySince = Date.now();
        }
        const serverReadyMs = _serverReadySince ? Date.now() - _serverReadySince : 0;
        const timedOut      = serverReadyMs > 10000; // 10 s fallback

        if (serverReady && (userReady || timedOut)) {
          restarting        = false;
          _serverReadySince = 0;
          hideOverlay();
        } else if (serverReady) {
          document.getElementById('overlay-sub').textContent =
            `LINDA ready \u2014 waiting for user agent\u2026 (${Math.round(serverReadyMs / 1000)}s)`;
          return;
        } else {
          document.getElementById('overlay-sub').textContent =
            'Waiting for LINDA server\u2026';
          return;
        }
      }

      PANES.forEach(p => {
        const el = document.getElementById('p-' + p.id);
        if (!el) return;
        const full = data[p.id] || '';
        currentText[p.id] = full;
        const text = filterNoise(visibleText(p.id, full));
        const snap = pinned[p.id];
        el.textContent = text;
        if (snap) el.scrollTop = el.scrollHeight;
      });
    })
    .catch(() => {
      if (++failCount > 2) {
        document.getElementById('led').className = 'off';
        document.getElementById('lbl').textContent = 'session offline';
      }
    });
}

/* Send helpers  */
function post(w, c) {
  return fetch('/api/send', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ window: w, cmd: c }),
  });
}

function doSend() {
  const w = document.getElementById('tgt').value;
  const c = document.getElementById('cmd').value.trim();
  if (!c) return;
  post(w, c);
  document.getElementById('cmd').value = '';
}
