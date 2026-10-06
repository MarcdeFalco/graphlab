// Graphlab — onglet « Remplissage » : version web de flood.ml.
// Trois remplissages de la même grille depuis la même case, avec une pile,
// une file et un tirage aléatoire. L'ordre des cases est calculé en OCaml
// (Algos.flood_order) ; ici on anime et on dessine.
"use strict";
(() => {
  const G = window.graphlab;
  const $ = (id) => document.getElementById(id);
  const MODES = ["stack", "queue", "random"];

  let W = 61, H = 41;
  let walls = new Uint8Array(W * H);
  let tool = "fill";
  let started = false;
  const panes = [...document.querySelectorAll("#flood-panes canvas")].map((canvas) => ({
    canvas, ctx: canvas.getContext("2d"), mode: canvas.dataset.mode,
    order: null, done: 0, image: null, start: -1,
  }));

  function themeColors() {
    const cs = getComputedStyle(document.documentElement);
    const dark = matchMedia("(prefers-color-scheme: dark)").matches;
    return { empty: dark ? [32, 34, 38] : [252, 252, 250], wall: dark ? [200, 200, 205] : [40, 40, 46], start: cs.getPropertyValue("--accent").trim() };
  }
  let theme = themeColors();
  matchMedia("(prefers-color-scheme: dark)").addEventListener("change", () => { theme = themeColors(); redrawAll(); });

  function resizeGrid(cols) {
    W = cols; H = Math.round(cols * 2 / 3); if (H % 2 === 0) H++;
    walls = new Uint8Array(W * H);
    for (const p of panes) {
      p.canvas.width = W; p.canvas.height = H;
      p.image = p.ctx.createImageData(W, H);
      p.order = null; p.done = 0; p.start = -1;
    }
    rooms();
  }

  // couleur d'une case selon son rang dans le remplissage
  function hsl(h, s, l) {
    s /= 100; l /= 100;
    const k = (n) => (n + h / 30) % 12, a = s * Math.min(l, 1 - l);
    const f = (n) => l - a * Math.max(-1, Math.min(k(n) - 3, Math.min(9 - k(n), 1)));
    return [Math.round(255 * f(0)), Math.round(255 * f(8)), Math.round(255 * f(4))];
  }
  function setPixel(p, c, rgb) {
    const d = p.image.data, o = 4 * c;
    d[o] = rgb[0]; d[o + 1] = rgb[1]; d[o + 2] = rgb[2]; d[o + 3] = 255;
  }
  function redraw(p) {
    for (let c = 0; c < W * H; c++) setPixel(p, c, walls[c] ? theme.wall : theme.empty);
    if (p.order) for (let k = 0; k < p.done; k++) setPixel(p, p.order[k], colorAt(k, p.order.length));
    p.ctx.putImageData(p.image, 0, 0);
    $("flood-count-" + p.mode).textContent = p.order ? `${p.done} / ${p.order.length}` : "0";
  }
  const colorAt = (k, total) => hsl((k / Math.max(1, total)) * 290, 85, 55);
  function redrawAll() { panes.forEach(redraw); }

  function resetColors() {
    for (const p of panes) { p.order = null; p.done = 0; }
    redrawAll();
  }

  function startFill(cell) {
    if (walls[cell]) return;
    const wallArray = Array.from(walls);
    for (const p of panes) {
      p.order = Array.from(G.floodOrder(W, H, wallArray, cell, p.mode));
      p.done = 0; p.start = cell;
    }
    redrawAll();
    if (!running) { running = true; requestAnimationFrame(animate); }
  }

  let running = false;
  function animate() {
    const speed = +$("flood-speed").value;
    let active = false;
    for (const p of panes) {
      if (!p.order || p.done >= p.order.length) continue;
      const end = Math.min(p.order.length, p.done + speed);
      for (let k = p.done; k < end; k++) setPixel(p, p.order[k], colorAt(k, p.order.length));
      p.done = end;
      p.ctx.putImageData(p.image, 0, 0);
      $("flood-count-" + p.mode).textContent = `${p.done} / ${p.order.length}`;
      if (p.done < p.order.length) active = true;
    }
    if (active) requestAnimationFrame(animate); else running = false;
  }

  // ---------- murs ----------
  function clearWalls() { walls.fill(0); resetColors(); }
  function randomWalls() {
    for (let c = 0; c < W * H; c++) walls[c] = Math.random() < 0.28 ? 1 : 0;
    resetColors();
  }
  function rooms() {
    walls.fill(0);
    const vline = (x, gap) => { for (let y = 0; y < H; y++) if (Math.abs(y - gap) > 1) walls[y * W + x] = 1; };
    const hline = (y, x0, x1, gap) => { for (let x = x0; x <= x1; x++) if (Math.abs(x - gap) > 1) walls[y * W + x] = 1; };
    const a = Math.floor(W / 3), b = Math.floor(2 * W / 3), m = Math.floor(H / 2);
    vline(a, Math.floor(H * 0.75));
    vline(b, Math.floor(H * 0.25));
    hline(m, 0, a - 1, Math.floor(a / 2));
    hline(m, b + 1, W - 1, Math.floor((b + W) / 2));
    resetColors();
  }
  // labyrinthe parfait : parcours en profondeur aléatoire sur les cases impaires
  function maze() {
    walls.fill(1);
    const stack = [[1, 1]];
    walls[W + 1] = 0;
    while (stack.length) {
      const [x, y] = stack[stack.length - 1];
      const nbrs = [[2, 0], [-2, 0], [0, 2], [0, -2]]
        .map(([dx, dy]) => [x + dx, y + dy, x + dx / 2, y + dy / 2])
        .filter(([nx, ny]) => nx > 0 && ny > 0 && nx < W - 1 && ny < H - 1 && walls[ny * W + nx]);
      if (!nbrs.length) { stack.pop(); continue; }
      const [nx, ny, mx, my] = nbrs[Math.floor(Math.random() * nbrs.length)];
      walls[my * W + mx] = 0; walls[ny * W + nx] = 0;
      stack.push([nx, ny]);
    }
    resetColors();
  }

  // ---------- souris ----------
  function cellAt(p, e) {
    const r = p.canvas.getBoundingClientRect();
    const x = Math.floor((e.clientX - r.left) / r.width * W), y = Math.floor((e.clientY - r.top) / r.height * H);
    return x >= 0 && y >= 0 && x < W && y < H ? y * W + x : -1;
  }
  let painting = false;
  function paintAt(p, e) {
    const c = cellAt(p, e);
    if (c < 0) return;
    const v = tool === "wall" ? 1 : 0;
    if (walls[c] === v) return;
    walls[c] = v;
    for (const q of panes) { q.order = null; q.done = 0; }
    redrawAll();
  }
  for (const p of panes) {
    p.canvas.addEventListener("pointerdown", (e) => {
      if (tool === "fill") { const c = cellAt(p, e); if (c >= 0) startFill(c); return; }
      painting = true; p.canvas.setPointerCapture(e.pointerId); paintAt(p, e);
    });
    p.canvas.addEventListener("pointermove", (e) => { if (painting) paintAt(p, e); });
    p.canvas.addEventListener("pointerup", () => { painting = false; });
  }

  document.querySelectorAll("#flood-tools button").forEach((b) => b.addEventListener("click", () => {
    tool = b.dataset.tool;
    document.querySelectorAll("#flood-tools button").forEach((x) => x.classList.toggle("on", x === b));
  }));
  $("flood-maze").addEventListener("click", maze);
  $("flood-random").addEventListener("click", randomWalls);
  $("flood-rooms").addEventListener("click", rooms);
  $("flood-clear-walls").addEventListener("click", clearWalls);
  $("flood-reset").addEventListener("click", resetColors);
  $("flood-size").addEventListener("change", (e) => resizeGrid(+e.target.value));

  // au premier affichage de l'onglet, on lance une démonstration
  window.addEventListener("graphlab-page", (e) => {
    if (e.detail !== "flood" || started) return;
    started = true;
    let c = Math.floor(H / 2) * W + Math.floor(W / 6);
    if (walls[c]) c++;
    startFill(c);
  });

  resizeGrid(61);
})();
