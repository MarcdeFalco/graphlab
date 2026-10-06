// Graphlab — onglet « Graphes » : édition, dessin, algorithmes pas à pas, exports.
// Les calculs (générateurs, disposition, algorithmes) sont faits en OCaml
// (objet global `graphlab`, voir graphlab_web.ml).
"use strict";
(() => {
  const G = window.graphlab;
  const $ = (id) => document.getElementById(id);
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
  const canvas = $("canvas"), ctx = canvas.getContext("2d");

  // ------------------------------------------------------------------
  // Couleurs (lues dans le CSS, suivent le thème clair / sombre)
  const COLOR_NAMES = ["canvas", "edge", "vertex", "vertex-text", "waiting", "done", "current", "tree", "back",
    "forward", "cross", "scan", "reject", "conflict", "candidate", "note-bg", "note-text", "accent", "muted"];
  let colors = {};
  function readColors() {
    const cs = getComputedStyle(document.documentElement);
    for (const n of COLOR_NAMES) colors[n] = cs.getPropertyValue("--" + n).trim();
  }
  readColors();
  const darkQuery = matchMedia("(prefers-color-scheme: dark)");
  darkQuery.addEventListener("change", () => { readColors(); draw(); });
  const LIGHT = { canvas: "#ffffff", edge: "#2a2a2e", vertex: "#8a8a92", "vertex-text": "#ffffff", waiting: "#e0912f",
    done: "#2f8f57", current: "#2f5fd0", tree: "#18a058", back: "#d93636", forward: "#2f6fe0", cross: "#9a9aa3",
    scan: "#f08c00", reject: "#e8a0a0", conflict: "#d00000", candidate: "#5a8fff", "note-bg": "#fff6d6",
    "note-text": "#5c4300", accent: "#2f5fd0", muted: "#66666d" };
  // couleurs des groupes (composantes, colorations)
  const GROUPS = ["#4e79a7", "#f28e2b", "#59a14f", "#e15759", "#b07aa1", "#edc948", "#76b7b2", "#ff9da7", "#9c755f", "#bab0ac"];
  // classes d'arêtes (voir algos.ml)
  const ECLASS = [null,
    { c: "tree", w: 2.6, label: "arbre / retenue" },
    { c: "back", w: 2.6, label: "arrière" },
    { c: "forward", w: 2.6, label: "avant" },
    { c: "cross", w: 2.2, dash: true, label: "transverse" },
    { c: "scan", w: 3, label: "examinée" },
    { c: "reject", w: 2, dash: true, label: "rejetée" },
    { c: "conflict", w: 3.4, label: "conflit" },
    { c: "candidate", w: 2.2, dash: true, label: "candidate" }];

  // ------------------------------------------------------------------
  // Modèle : la page détient le graphe
  // edges : { u, v, w } ; en non orienté chaque arête n'apparaît qu'une fois
  let model = { directed: false, weighted: false, labels: [], pos: [], edges: [] };
  const undoStack = [], redoStack = [];
  const snapshot = () => JSON.stringify(model);

  function commit(mutate) {
    undoStack.push(snapshot());
    if (undoStack.length > 300) undoStack.shift();
    redoStack.length = 0;
    mutate();
    structureChanged();
  }
  function undo() {
    if (!undoStack.length) return;
    redoStack.push(snapshot());
    model = JSON.parse(undoStack.pop());
    structureChanged();
  }
  function redo() {
    if (!redoStack.length) return;
    undoStack.push(snapshot());
    model = JSON.parse(redoStack.pop());
    structureChanged();
  }

  const n = () => model.labels.length;
  const weightOf = (e) => (model.weighted ? e.w : 1);
  function findEdge(u, v) {
    return model.edges.findIndex((e) => (e.u === u && e.v === v) || (!model.directed && e.u === v && e.v === u));
  }
  function hasArc(u, v) { return model.edges.some((e) => e.u === u && e.v === v); }

  function nextLabel() {
    const used = new Set(model.labels);
    const letters = model.labels.length > 0 && model.labels.every((l) => /^[a-z]$/.test(l));
    if (letters) for (let c = 97; c <= 122; c++) if (!used.has(String.fromCharCode(c))) return String.fromCharCode(c);
    for (let k = 0; ; k++) if (!used.has(String(k))) return String(k);
  }
  function cleanLabel(s) { return s.trim().replace(/[\s,#]+/g, "_"); }

  function addVertex(x, y) {
    commit(() => { model.labels.push(nextLabel()); model.pos.push([x, y]); });
  }
  function removeVertex(i) {
    commit(() => {
      model.labels.splice(i, 1);
      model.pos.splice(i, 1);
      model.edges = model.edges.filter((e) => e.u !== i && e.v !== i)
        .map((e) => ({ u: e.u > i ? e.u - 1 : e.u, v: e.v > i ? e.v - 1 : e.v, w: e.w }));
    });
  }
  function addEdge(u, v, w = 1) {
    if (u === v || findEdge(u, v) >= 0) return false;
    commit(() => model.edges.push({ u, v, w }));
    return true;
  }
  function removeEdge(k) { commit(() => model.edges.splice(k, 1)); }

  function setDirected(d) {
    commit(() => {
      model.directed = d;
      if (!d) {
        const seen = new Set();
        model.edges = model.edges.filter((e) => {
          const key = Math.min(e.u, e.v) + "," + Math.max(e.u, e.v);
          if (seen.has(key)) return false;
          seen.add(key); return true;
        });
      }
    });
  }

  // remplace tout le graphe (générateur, import)
  function loadGraph(g, keepPositions) {
    commit(() => { Object.assign(model, g); });
    if (!keepPositions) fit();
  }

  function fromOCaml(g) {
    return {
      directed: !!g.directed,
      labels: Array.from(g.labels),
      pos: Array.from(g.positions).map((p) => [p[0], p[1]]),
      edges: Array.from(g.edges).map((e) => ({ u: e[0], v: e[1], w: e[2] })),
    };
  }

  // transmet le graphe à OCaml
  function syncGraph() {
    const flat = [];
    for (const e of model.edges) flat.push(e.u, e.v, weightOf(e));
    G.setGraph(model.labels, model.directed, flat);
  }

  function structureChanged() {
    syncGraph();
    clearTrace();
    $("directed").checked = model.directed;
    $("weighted").checked = model.weighted;
    updateSourceList();
    updateRepr();
    updateUndo();
    save();
    draw();
  }

  function updateUndo() {
    $("undo").disabled = !undoStack.length;
    $("redo").disabled = !redoStack.length;
  }

  // ------------------------------------------------------------------
  // Sauvegarde locale et lien de partage
  const STORE = "graphlab.graph.v2";
  function compact() {
    return {
      d: model.directed ? 1 : 0, w: model.weighted ? 1 : 0, l: model.labels,
      p: model.pos.map(([x, y]) => [Math.round(x), Math.round(y)]),
      e: model.edges.map((e) => (model.weighted ? [e.u, e.v, e.w] : [e.u, e.v])),
    };
  }
  function uncompact(c) {
    return {
      directed: !!c.d, weighted: !!c.w, labels: c.l.map(String), pos: c.p.map((p) => [+p[0], +p[1]]),
      edges: c.e.map((e) => ({ u: e[0], v: e[1], w: e.length > 2 ? e[2] : 1 })),
    };
  }
  let saveTimer = null;
  function save() {
    clearTimeout(saveTimer);
    saveTimer = setTimeout(() => { try { localStorage.setItem(STORE, JSON.stringify(compact())); } catch (_) {} }, 300);
  }
  const b64encode = (s) => btoa(String.fromCharCode(...new TextEncoder().encode(s))).replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
  const b64decode = (s) => new TextDecoder().decode(Uint8Array.from(atob(s.replace(/-/g, "+").replace(/_/g, "/")), (c) => c.charCodeAt(0)));

  // ------------------------------------------------------------------
  // Caméra : positions en unités du monde, tailles en pixels écran
  let cam = { x: 0, y: 0, zoom: 1 };
  const size = () => { const r = canvas.getBoundingClientRect(); return { w: r.width, h: r.height }; };
  const R = () => +$("radius").value;          // rayon en pixels
  const T = () => +$("thickness").value;       // épaisseur en pixels
  function toWorld(sx, sy) { const { w, h } = size(); return [(sx - w / 2) / cam.zoom + cam.x, (sy - h / 2) / cam.zoom + cam.y]; }
  function toScreen(x, y) { const { w, h } = size(); return [(x - cam.x) * cam.zoom + w / 2, (y - cam.y) * cam.zoom + h / 2]; }

  function fitCamera(c, w, h, pad) {
    if (!model.pos.length) { c.x = 0; c.y = 0; c.zoom = 1; return; }
    let x1 = Infinity, y1 = Infinity, x2 = -Infinity, y2 = -Infinity;
    for (const [x, y] of model.pos) { x1 = Math.min(x1, x); y1 = Math.min(y1, y); x2 = Math.max(x2, x); y2 = Math.max(y2, y); }
    c.x = (x1 + x2) / 2; c.y = (y1 + y2) / 2;
    const zx = (w - pad) / Math.max(x2 - x1, 1e-9), zy = (h - pad) / Math.max(y2 - y1, 1e-9);
    c.zoom = Math.min(zx, zy);
    if (!isFinite(c.zoom) || c.zoom <= 0) c.zoom = 1;
    if (model.pos.length === 1) c.zoom = 1;
    c.zoom = Math.min(c.zoom, 4);
  }
  function fit() { const { w, h } = size(); fitCamera(cam, w, h, 6 * R() + 40); draw(); }

  // ------------------------------------------------------------------
  // État de l'algorithme
  let algos = Array.from(G.algorithms()).map((a) => ({
    id: a.id, name: a.name, code: Array.from(a.code), source: !!a.source, weighted: !!a.weighted, description: a.description,
  }));
  let step = null, stepIndex = 0, stepCount = 0, playing = null, classes = null;

  // ------------------------------------------------------------------
  // Géométrie des arêtes
  function edgeGeom(e, rw) {
    const [x1, y1] = model.pos[e.u], [x2, y2] = model.pos[e.v];
    const dx = x2 - x1, dy = y2 - y1, l = Math.hypot(dx, dy) || 1;
    // en orienté, deux arcs opposés sont courbés pour rester distincts
    const curved = model.directed && hasArc(e.v, e.u);
    const off = curved ? Math.min(l * 0.18, 3.2 * rw) : 0;
    const cx = (x1 + x2) / 2 - (dy / l) * off, cy = (y1 + y2) / 2 + (dx / l) * off;
    return { x1, y1, x2, y2, cx, cy, curved, l };
  }
  function quadPoint(g, t) {
    const a = (1 - t) * (1 - t), b = 2 * (1 - t) * t, c = t * t;
    return [a * g.x1 + b * g.cx + c * g.x2, a * g.y1 + b * g.cy + c * g.y2];
  }
  function distToEdge(e, x, y, rw) {
    const g = edgeGeom(e, rw);
    let best = Infinity, prev = [g.x1, g.y1];
    for (let k = 1; k <= 16; k++) {
      const p = quadPoint(g, k / 16);
      const vx = p[0] - prev[0], vy = p[1] - prev[1], L = vx * vx + vy * vy || 1;
      const t = Math.max(0, Math.min(1, ((x - prev[0]) * vx + (y - prev[1]) * vy) / L));
      best = Math.min(best, Math.hypot(x - prev[0] - t * vx, y - prev[1] - t * vy));
      prev = p;
    }
    return best;
  }

  // ------------------------------------------------------------------
  // Dessin (paramétré pour servir aussi à l'export PNG)
  function drawScene(c, w, h, camera, pal, opts = {}) {
    const dpr = opts.dpr || 1;
    c.setTransform(dpr, 0, 0, dpr, 0, 0);
    c.fillStyle = pal.canvas; c.fillRect(0, 0, w, h);
    if (!model.labels.length) return;
    const z = camera.zoom;
    c.setTransform(dpr * z, 0, 0, dpr * z, dpr * (w / 2 - camera.x * z), dpr * (h / 2 - camera.y * z));
    const rw = R() / z, tw = T() / z;
    c.lineCap = "round"; c.lineJoin = "round";

    const ecls = new Map();
    if (step && !opts.plain) for (const [i, j, k] of step.eclass) ecls.set(i + "," + j, k);
    const classOf = (e) => ecls.get(e.u + "," + e.v) ?? (model.directed ? undefined : ecls.get(e.v + "," + e.u));
    const curEdge = step && step.edge && !opts.plain ? step.edge : null;
    const isCur = (e) => curEdge && ((curEdge[0] === e.u && curEdge[1] === e.v) ||
      (!model.directed && curEdge[0] === e.v && curEdge[1] === e.u));

    function arrow(g, color, lw) {
      // tangente au point d'arrivée de la courbe
      const tx = g.x2 - g.cx, ty = g.y2 - g.cy, l = Math.hypot(tx, ty) || 1;
      const ux = tx / l, uy = ty / l, px = -uy, py = ux;
      const hw = Math.max(lw * 2.2, rw * 0.38), len = hw * 2.4;
      const ex = g.x2 - ux * (rw + 1 / z), ey = g.y2 - uy * (rw + 1 / z);
      c.fillStyle = color;
      c.beginPath();
      c.moveTo(ex, ey);
      c.lineTo(ex - ux * len + px * hw, ey - uy * len + py * hw);
      c.lineTo(ex - ux * len - px * hw, ey - uy * len - py * hw);
      c.closePath(); c.fill();
    }
    function stroke(e, color, lw, dash) {
      const g = edgeGeom(e, rw);
      c.strokeStyle = color; c.lineWidth = lw;
      c.setLineDash(dash ? [lw * 2.5, lw * 2] : []);
      c.beginPath(); c.moveTo(g.x1, g.y1);
      if (g.curved) c.quadraticCurveTo(g.cx, g.cy, g.x2, g.y2); else c.lineTo(g.x2, g.y2);
      c.stroke(); c.setLineDash([]);
      if (model.directed) arrow(g, color, lw);
    }

    // arêtes : d'abord les arêtes neutres, puis celles colorées par l'algorithme
    const coloured = [];
    for (const e of model.edges) {
      const k = classOf(e);
      if (k || isCur(e)) coloured.push(e);
      else stroke(e, pal.edge, tw, false);
    }
    for (const e of coloured) {
      const k = classOf(e);
      if (k) { const s = ECLASS[k]; stroke(e, pal[s.c], tw * s.w, s.dash); }
      if (isCur(e)) stroke(e, pal.scan, tw * 3.2, false);
    }
    // aperçu d'arête en cours de tracé
    if (opts.preview) {
      const [x1, y1] = model.pos[opts.preview.from];
      c.strokeStyle = pal.accent; c.lineWidth = tw * 1.5; c.setLineDash([6 / z, 5 / z]);
      c.beginPath(); c.moveTo(x1, y1); c.lineTo(opts.preview.x, opts.preview.y); c.stroke(); c.setLineDash([]);
    }

    // poids
    if (model.weighted) {
      c.font = `600 ${Math.max(10, R() * 0.72) / z}px system-ui, sans-serif`;
      c.textAlign = "center"; c.textBaseline = "middle";
      for (const e of model.edges) {
        const g = edgeGeom(e, rw);
        const [mx, my] = g.curved ? quadPoint(g, 0.5) : [(g.x1 + g.x2) / 2, (g.y1 + g.y2) / 2];
        const s = String(e.w), tw2 = c.measureText(s).width;
        c.fillStyle = pal.canvas;
        c.fillRect(mx - tw2 / 2 - 3 / z, my - R() * 0.45 / z, tw2 + 6 / z, R() * 0.9 / z);
        c.fillStyle = classOf(e) ? pal[ECLASS[classOf(e)].c] : pal.muted;
        c.fillText(s, mx, my);
      }
    }

    // sommets
    model.pos.forEach(([x, y], i) => {
      let fill = pal.vertex;
      if (step && !opts.plain) {
        if (step.vgroup[i] >= 0) fill = GROUPS[step.vgroup[i] % GROUPS.length];
        else if (step.vclass[i] === 1) fill = pal.waiting;
        else if (step.vclass[i] === 2) fill = pal.done;
      }
      const cur = step && !opts.plain && step.current === i;
      if (cur) {
        c.fillStyle = pal.current;
        c.beginPath(); c.arc(x, y, rw + Math.max(4, R() * 0.3) / z, 0, 2 * Math.PI); c.fill();
      }
      if (opts.selected === i || opts.hover === i) {
        c.strokeStyle = pal.accent; c.lineWidth = 2.5 / z; c.setLineDash([4 / z, 3 / z]);
        c.beginPath(); c.arc(x, y, rw + 5 / z, 0, 2 * Math.PI); c.stroke(); c.setLineDash([]);
      }
      c.fillStyle = pal.edge;
      c.beginPath(); c.arc(x, y, rw + tw * 0.8, 0, 2 * Math.PI); c.fill();
      c.fillStyle = fill;
      c.beginPath(); c.arc(x, y, rw, 0, 2 * Math.PI); c.fill();
    });

    // noms et annotations
    c.textBaseline = "middle";
    model.pos.forEach(([x, y], i) => {
      if ($("show-labels").checked) {
        const label = model.labels[i];
        const fs = Math.min(R() * 0.95, (2 * R() * 0.85) / Math.max(1, label.length * 0.62)) / z;
        c.font = `600 ${fs}px system-ui, sans-serif`;
        c.fillStyle = pal["vertex-text"]; c.textAlign = "center";
        c.fillText(label, x, y + fs * 0.04);
      }
      if (step && !opts.plain && $("show-notes").checked && step.vnote[i]) {
        const s = step.vnote[i];
        const fs = Math.max(10, R() * 0.7) / z;
        c.font = `600 ${fs}px system-ui, sans-serif`;
        const wd = c.measureText(s).width + 8 / z, ht = fs * 1.35;
        const bx = x + rw * 0.7, by = y - rw * 0.7 - ht;
        c.fillStyle = pal["note-bg"];
        roundRect(c, bx, by, wd, ht, 4 / z); c.fill();
        c.fillStyle = pal["note-text"]; c.textAlign = "left";
        c.fillText(s, bx + 4 / z, by + ht / 2);
      }
    });
  }
  function roundRect(c, x, y, w, h, r) {
    c.beginPath();
    c.moveTo(x + r, y); c.lineTo(x + w - r, y); c.quadraticCurveTo(x + w, y, x + w, y + r);
    c.lineTo(x + w, y + h - r); c.quadraticCurveTo(x + w, y + h, x + w - r, y + h);
    c.lineTo(x + r, y + h); c.quadraticCurveTo(x, y + h, x, y + h - r);
    c.lineTo(x, y + r); c.quadraticCurveTo(x, y, x + r, y); c.closePath();
  }

  let interaction = { selected: -1, hover: -1, preview: null };
  let drawPending = false;
  function draw() {
    if (drawPending) return;
    drawPending = true;
    requestAnimationFrame(() => {
      drawPending = false;
      const { w, h } = size();
      drawScene(ctx, w, h, cam, colors, { dpr: window.devicePixelRatio || 1, ...interaction });
      updateHelp();
    });
  }
  function resize() {
    const dpr = window.devicePixelRatio || 1, { w, h } = size();
    canvas.width = Math.round(w * dpr); canvas.height = Math.round(h * dpr);
    draw();
  }

  // ------------------------------------------------------------------
  // Interaction
  let mode = "move";
  const HELP = {
    move: "Glisser un sommet pour le déplacer, le fond pour déplacer la vue · double-clic : renommer / changer un poids · clic droit sur un sommet : lancer l'algorithme depuis ce sommet",
    vertex: "Cliquer dans le vide pour ajouter un sommet",
    edge: "Glisser d'un sommet à un autre (ou cliquer sur l'un puis l'autre) pour ajouter une arête",
    delete: "Cliquer sur un sommet ou une arête pour le supprimer",
  };
  function updateHelp() {
    $("canvas-help").textContent = model.labels.length ? HELP[mode] : "Graphe vide : outil « Sommet » puis cliquez pour ajouter des sommets, ou créez un graphe à gauche.";
  }
  function setMode(m) {
    mode = m;
    document.querySelectorAll("#modes button").forEach((b) => b.classList.toggle("on", b.dataset.mode === m));
    interaction.selected = -1; interaction.preview = null;
    canvas.style.cursor = m === "vertex" ? "copy" : m === "delete" ? "not-allowed" : m === "edge" ? "crosshair" : "grab";
    draw();
  }
  document.querySelectorAll("#modes button").forEach((b) => b.addEventListener("click", () => setMode(b.dataset.mode)));

  const localXY = (e) => { const r = canvas.getBoundingClientRect(); return [e.clientX - r.left, e.clientY - r.top]; };
  function vertexAt(sx, sy) {
    const [x, y] = toWorld(sx, sy), rw = (R() + 4) / cam.zoom;
    for (let i = model.pos.length - 1; i >= 0; i--) if (Math.hypot(model.pos[i][0] - x, model.pos[i][1] - y) <= rw) return i;
    return -1;
  }
  function edgeAt(sx, sy) {
    const [x, y] = toWorld(sx, sy), rw = R() / cam.zoom, tol = Math.max(6, T() * 2) / cam.zoom;
    let best = -1, bd = tol;
    model.edges.forEach((e, k) => { const d = distToEdge(e, x, y, rw); if (d < bd) { bd = d; best = k; } });
    return best;
  }

  const pointers = new Map();
  let gesture = null;   // { kind: "drag" | "pan" | "edge" | "click", ... }
  let pinch = null;

  canvas.addEventListener("pointerdown", (e) => {
    if (e.button === 2) return;
    hideFloating(true);
    canvas.setPointerCapture(e.pointerId);
    const [sx, sy] = localXY(e);
    pointers.set(e.pointerId, [sx, sy]);
    if (pointers.size === 2) {
      gesture = null; interaction.preview = null;
      const [a, b] = [...pointers.values()];
      pinch = { d: Math.hypot(a[0] - b[0], a[1] - b[1]), zoom: cam.zoom };
      return;
    }
    const v = vertexAt(sx, sy);
    const start = { sx, sy, moved: false };
    if (mode === "delete") { gesture = { kind: "click", ...start, v, e: v < 0 ? edgeAt(sx, sy) : -1 }; return; }
    if (mode === "edge" && v >= 0) {
      gesture = { kind: "edge", ...start, from: v };
      return;
    }
    if (v >= 0) {
      gesture = { kind: "drag", ...start, v, before: snapshot() };
      canvas.style.cursor = "grabbing";
    } else {
      gesture = { kind: "pan", ...start, x: cam.x, y: cam.y };
    }
  });

  canvas.addEventListener("pointermove", (e) => {
    const [sx, sy] = localXY(e);
    if (!pointers.has(e.pointerId)) {
      const hv = vertexAt(sx, sy);
      if (hv !== interaction.hover) { interaction.hover = hv; draw(); }
      return;
    }
    pointers.set(e.pointerId, [sx, sy]);
    if (pinch && pointers.size === 2) {
      const [a, b] = [...pointers.values()];
      cam.zoom = pinch.zoom * Math.hypot(a[0] - b[0], a[1] - b[1]) / pinch.d;
      draw(); return;
    }
    if (!gesture) return;
    if (Math.hypot(sx - gesture.sx, sy - gesture.sy) > 4) gesture.moved = true;
    if (gesture.kind === "drag" && gesture.moved) {
      model.pos[gesture.v] = toWorld(sx, sy);
      draw();
    } else if (gesture.kind === "pan") {
      cam.x = gesture.x - (sx - gesture.sx) / cam.zoom;
      cam.y = gesture.y - (sy - gesture.sy) / cam.zoom;
      draw();
    } else if (gesture.kind === "edge" && gesture.moved) {
      const [x, y] = toWorld(sx, sy);
      interaction.preview = { from: gesture.from, x, y };
      interaction.hover = vertexAt(sx, sy);
      draw();
    }
  });

  function endPointer(e) {
    const had = pointers.has(e.pointerId);
    pointers.delete(e.pointerId);
    if (pointers.size < 2) pinch = null;
    if (!had || pointers.size > 0 || !gesture) { if (pointers.size === 0) gesture = null; return; }
    const g = gesture; gesture = null;
    const [sx, sy] = localXY(e);
    if (g.kind === "drag") {
      canvas.style.cursor = mode === "move" ? "grab" : canvas.style.cursor;
      if (g.moved) {
        // un déplacement est annulable mais ne relance pas les calculs
        undoStack.push(g.before); redoStack.length = 0; updateUndo(); save();
      } else if (mode === "edge") {
        edgeClick(g.v);
      }
    } else if (g.kind === "pan") {
      if (!g.moved) {
        if (mode === "vertex") addVertex(...toWorld(sx, sy));
        if (mode === "edge") { interaction.selected = -1; draw(); }
      }
    } else if (g.kind === "edge") {
      interaction.preview = null;
      if (g.moved) {
        const t = vertexAt(sx, sy);
        if (t >= 0 && t !== g.from) { addEdge(g.from, t); interaction.selected = -1; }
        draw();
      } else edgeClick(g.from);
    } else if (g.kind === "click" && !g.moved) {
      if (g.v >= 0) removeVertex(g.v);
      else if (g.e >= 0) removeEdge(g.e);
    }
  }
  function edgeClick(v) {
    if (interaction.selected >= 0 && interaction.selected !== v) {
      addEdge(interaction.selected, v);
      interaction.selected = -1;
    } else interaction.selected = interaction.selected === v ? -1 : v;
    draw();
  }
  canvas.addEventListener("pointerup", endPointer);
  canvas.addEventListener("pointercancel", (e) => { pointers.delete(e.pointerId); gesture = null; pinch = null; interaction.preview = null; draw(); });
  canvas.addEventListener("pointerleave", () => { if (interaction.hover >= 0) { interaction.hover = -1; draw(); } });

  canvas.addEventListener("wheel", (e) => {
    e.preventDefault();
    const [sx, sy] = localXY(e);
    const [wx, wy] = toWorld(sx, sy);
    cam.zoom *= Math.exp(-e.deltaY * 0.0015);
    const [nx, ny] = toWorld(sx, sy);
    cam.x += wx - nx; cam.y += wy - ny;
    draw();
  }, { passive: false });

  canvas.addEventListener("contextmenu", (e) => {
    e.preventDefault();
    const v = vertexAt(...localXY(e));
    if (v >= 0) { $("source").value = String(v); runAlgo(); }
  });

  canvas.addEventListener("dblclick", (e) => {
    const [sx, sy] = localXY(e);
    const v = vertexAt(sx, sy);
    if (v >= 0) return editFloating(sx, sy, model.labels[v], (s) => {
      const l = cleanLabel(s);
      if (!l || l === model.labels[v]) return;
      if (model.labels.includes(l)) return flash(`Le nom « ${l} » est déjà pris.`);
      commit(() => { model.labels[v] = l; });
    });
    const k = edgeAt(sx, sy);
    if (k >= 0) return editFloating(sx, sy, String(model.edges[k].w), (s) => {
      const w = parseInt(s, 10);
      if (!Number.isFinite(w)) return;
      commit(() => { model.edges[k].w = w; model.weighted = true; });
    });
  });

  // petit champ de saisie posé sur le dessin
  const floating = $("floating");
  let floatingDone = null;
  function editFloating(sx, sy, value, done) {
    floatingDone = done;
    floating.value = value;
    floating.style.left = Math.max(4, sx - 60) + "px";
    floating.style.top = Math.max(4, sy - 44) + "px";
    floating.style.display = "block";
    floating.focus(); floating.select();
  }
  function hideFloating(apply) {
    if (floating.style.display !== "block") return;
    floating.style.display = "none";
    const f = floatingDone; floatingDone = null;
    if (apply && f) f(floating.value);
  }
  floating.addEventListener("keydown", (e) => {
    if (e.key === "Enter") { e.preventDefault(); hideFloating(true); }
    if (e.key === "Escape") { e.preventDefault(); hideFloating(false); }
    e.stopPropagation();
  });
  floating.addEventListener("blur", () => hideFloating(true));

  function flash(msg) { $("message").textContent = msg; }

  // ------------------------------------------------------------------
  // Disposition
  function layoutParams() { return ["c1", "c2", "c3", "c4"].map((id) => +$(id).value); }
  let animating = false;
  function tick() {
    if (!animating) return;
    if (model.labels.length > 1) {
      const flat = model.pos.flat();
      const [c1, c2, c3, c4] = layoutParams();
      const fixed = gesture && gesture.kind === "drag" ? gesture.v : -1;
      const out = Array.from(G.iterate(flat, c1, c2, c3, c4, fixed));
      for (let i = 0; i < model.labels.length; i++) model.pos[i] = [out[2 * i], out[2 * i + 1]];
      save();
      draw();
    }
    requestAnimationFrame(tick);
  }
  $("animate").addEventListener("click", (e) => {
    animating = !animating;
    e.target.classList.toggle("on", animating);
    e.target.textContent = animating ? "Arrêter" : "Animer";
    if (animating) requestAnimationFrame(tick);
  });
  function setPositions(pos) {
    undoStack.push(snapshot()); redoStack.length = 0; updateUndo();
    model.pos = pos; save(); fit();
  }
  $("relayout").addEventListener("click", () => {
    if (n()) setPositions(Array.from(G.layout()).map((p) => [p[0], p[1]]));
  });
  $("circle").addEventListener("click", () => {
    const k = n(), r = Math.max(300, k * 70);
    setPositions(model.labels.map((_, i) => [r * Math.cos(2 * Math.PI * i / k - Math.PI / 2), r * Math.sin(2 * Math.PI * i / k - Math.PI / 2)]));
  });
  $("shuffle").addEventListener("click", () => {
    const k = n(), r = Math.max(400, k * 80);
    setPositions(model.labels.map(() => [Math.random() * r, Math.random() * r]));
  });
  $("fit").addEventListener("click", fit);

  document.querySelectorAll(".slider input").forEach((inp) => {
    const out = inp.parentElement.querySelector("output");
    const show = () => { out.textContent = (+inp.value).toFixed(+inp.step < 0.1 ? 3 : (+inp.step < 1 ? 1 : 0)); };
    show();
    inp.addEventListener("input", () => { show(); draw(); });
  });
  for (const id of ["show-labels", "show-notes"]) $(id).addEventListener("change", draw);
  $("directed").addEventListener("change", (e) => setDirected(e.target.checked));
  $("weighted").addEventListener("change", (e) => commit(() => { model.weighted = e.target.checked; }));
  $("undo").addEventListener("click", undo);
  $("redo").addEventListener("click", redo);

  // ------------------------------------------------------------------
  // Générateurs
  const generators = Array.from(G.generators()).map((g) => ({
    id: g.id, name: g.name,
    params: Array.from(g.params).map((p) => ({ name: p.name, label: p.label, def: p.default, min: p.min, max: p.max, step: p.step })),
  }));
  $("gen").innerHTML = generators.map((g) => `<option value="${g.id}">${esc(g.name)}</option>`).join("");
  function showParams() {
    const g = generators.find((x) => x.id === $("gen").value);
    $("gen-params").innerHTML = g.params.map((p) =>
      `<span>${esc(p.label)}</span><input type="number" data-p="${p.name}" value="${p.def}" min="${p.min}" max="${p.max}" step="${p.step}">`).join("");
  }
  $("gen").addEventListener("change", showParams);
  showParams();
  function generate() {
    const g = generators.find((x) => x.id === $("gen").value);
    const values = g.params.map((p) => {
      const v = +document.querySelector(`#gen-params [data-p="${p.name}"]`).value;
      return Math.max(p.min, Math.min(p.max, Number.isFinite(v) ? v : p.def));
    });
    const res = fromOCaml(G.generate(g.id, values));
    const wParam = g.params.findIndex((p) => p.name === "w");
    res.weighted = ["pondere", "negatif"].includes(g.id) || (wParam >= 0 && values[wParam] > 0);
    loadGraph(res);
  }
  $("gen-go").addEventListener("click", generate);
  $("clear").addEventListener("click", () => {
    loadGraph({ directed: model.directed, weighted: model.weighted, labels: [], pos: [], edges: [] });
    setMode("vertex");
  });

  // ------------------------------------------------------------------
  // Algorithmes
  const GROUPS_OF = [
    ["Parcours", ["bfs", "dfs", "dfsrec"]],
    ["Plus courts chemins", ["dijkstra", "bellman", "floyd"]],
    ["Arbres couvrants", ["prim", "kruskal"]],
    ["Structure", ["topo", "cc", "scc", "bipartite"]],
    ["Coloration", ["coloring"]],
  ];
  $("algo").innerHTML = GROUPS_OF.map(([name, ids]) =>
    `<optgroup label="${name}">${ids.map((id) => { const a = algos.find((x) => x.id === id); return a ? `<option value="${id}">${esc(a.name)}</option>` : ""; }).join("")}</optgroup>`).join("");
  const currentAlgo = () => algos.find((a) => a.id === $("algo").value);

  function updateAlgoInfo() {
    const a = currentAlgo();
    $("algo-desc").textContent = a.description;
    $("source-label").style.visibility = a.source ? "visible" : "hidden";
    $("code").innerHTML = a.code.map((l) => `<li>${esc(l)}</li>`).join("");
    const warn = [];
    if (a.weighted && !model.weighted) warn.push("Graphe non pondéré : chaque arête compte pour 1.");
    if (a.id === "topo" && !model.directed) warn.push("Le tri topologique demande un graphe orienté.");
    if (a.id === "dijkstra" && model.weighted && model.edges.some((e) => e.w < 0)) warn.push("Poids négatifs : Dijkstra peut se tromper (essayez Bellman-Ford).");
    if ((a.id === "prim" || a.id === "kruskal") && model.directed) warn.push("Graphe orienté : l'orientation est ignorée.");
    $("algo-warn").textContent = warn.join(" ");
  }
  function updateSourceList() {
    const old = $("source").value;
    $("source").innerHTML = model.labels.map((l, i) => `<option value="${i}">${esc(l)}</option>`).join("");
    if (old !== "" && +old < n()) $("source").value = old;
    updateAlgoInfo();
  }
  $("algo").addEventListener("change", () => { clearTrace(); updateAlgoInfo(); });

  function runAlgo() {
    if (!n()) return flash("Le graphe est vide.");
    stopPlay();
    stepCount = G.run(currentAlgo().id, +$("source").value || 0);
    const tc = G.traceClasses();
    classes = { edges: Array.from(tc.edges), vertices: Array.from(tc.vertices), groups: !!tc.groups };
    setStep(0);
  }
  function clearTrace() {
    stopPlay();
    step = null; stepCount = 0; stepIndex = 0; classes = null;
    $("code").querySelectorAll("li").forEach((li) => li.classList.remove("hl"));
    $("message").textContent = "Choisissez un algorithme puis « Lancer » (ou clic droit sur un sommet de départ).";
    $("struct-name").textContent = ""; $("chips").innerHTML = ""; $("table-wrap").innerHTML = "";
    updateStepBar(); updateLegend(); draw();
  }
  $("run").addEventListener("click", runAlgo);
  $("stop").addEventListener("click", clearTrace);

  function setStep(k) {
    if (!stepCount) return;
    stepIndex = Math.max(0, Math.min(stepCount - 1, k));
    const s = G.step(stepIndex);
    step = {
      current: s.current, edge: s.edge ? Array.from(s.edge) : null,
      vclass: Array.from(s.vclass), vgroup: Array.from(s.vgroup), vnote: Array.from(s.vnote),
      eclass: Array.from(s.eclass).map((a) => Array.from(a)),
      structure: s.structure, contents: Array.from(s.contents),
      table: Array.from(s.table).map((r) => Array.from(r)),
      message: s.message, line: s.line,
    };
    $("message").textContent = step.message;
    $("code").querySelectorAll("li").forEach((li, i) => li.classList.toggle("hl", i === step.line));
    $("struct-name").textContent = step.structure;
    $("chips").innerHTML = step.structure
      ? (step.contents.length ? step.contents.map((c) => `<span class="chip">${esc(c)}</span>`).join("") : '<span class="empty">(vide)</span>')
      : "";
    renderTable(step.table);
    updateStepBar(); updateLegend(); draw();
  }
  function renderTable(t) {
    if (!t.length) { $("table-wrap").innerHTML = ""; return; }
    const curLabel = step && step.current >= 0 ? model.labels[step.current] : null;
    $("table-wrap").innerHTML = `<table class="data"><thead><tr>${t[0].map((h) => `<th>${esc(h)}</th>`).join("")}</tr></thead><tbody>` +
      t.slice(1).map((r) => `<tr class="${r[0] === curLabel ? "cur" : ""}">${r.map((c) => `<td>${esc(c)}</td>`).join("")}</tr>`).join("") +
      "</tbody></table>";
  }
  function updateStepBar() {
    const has = stepCount > 0;
    for (const id of ["first", "prev", "play", "next", "last", "step"]) $(id).disabled = !has;
    $("step").max = Math.max(0, stepCount - 1);
    $("step").value = stepIndex;
    $("step-info").textContent = has ? `${stepIndex + 1} / ${stepCount}` : "—";
    $("play").textContent = playing ? "⏸" : "▶";
  }
  function stopPlay() { if (playing) { clearTimeout(playing); playing = null; } updateStepBar(); }
  function playTick() {
    if (stepIndex >= stepCount - 1) return stopPlay();
    setStep(stepIndex + 1);
    playing = setTimeout(playTick, +$("speed").value);
  }
  function togglePlay() {
    if (playing) return stopPlay();
    if (!stepCount) return;
    if (stepIndex >= stepCount - 1) setStep(0);
    playing = setTimeout(playTick, +$("speed").value);
    updateStepBar();
  }
  $("first").addEventListener("click", () => { stopPlay(); setStep(0); });
  $("prev").addEventListener("click", () => { stopPlay(); setStep(stepIndex - 1); });
  $("next").addEventListener("click", () => { stopPlay(); setStep(stepIndex + 1); });
  $("last").addEventListener("click", () => { stopPlay(); setStep(stepCount - 1); });
  $("play").addEventListener("click", togglePlay);
  $("step").addEventListener("input", (e) => { stopPlay(); setStep(+e.target.value); });

  function updateLegend() {
    const L = $("legend");
    if (!classes) { L.innerHTML = ""; return; }
    const parts = [];
    const vnames = ["non atteint", "en attente", "traité"], vcols = ["vertex", "waiting", "done"];
    classes.vertices.forEach((on, k) => { if (on && !(classes.groups && k === 2)) parts.push(`<span><i class="sw" style="background:var(--${vcols[k]})"></i>${vnames[k]}</span>`); });
    if (classes.groups) parts.push(`<span>${GROUPS.slice(0, 4).map((c) => `<i class="sw" style="background:${c}"></i>`).join("")} groupes (composantes / couleurs)</span>`);
    parts.push(`<span><i class="sw" style="background:transparent;border:3px solid var(--current)"></i>sommet courant</span>`);
    parts.push(`<span><i class="ln" style="border-color:var(--scan)"></i>arête examinée</span>`);
    classes.edges.forEach((on, k) => { if (on && k > 0 && k !== 5) { const s = ECLASS[k]; parts.push(`<span><i class="ln${s.dash ? " dash" : ""}" style="border-color:var(--${s.c})"></i>${s.label}</span>`); } });
    L.innerHTML = parts.join("");
  }

  // ------------------------------------------------------------------
  // Représentations
  $("rt-algo").addEventListener("click", () => switchRight("algo"));
  $("rt-repr").addEventListener("click", () => switchRight("repr"));
  function switchRight(t) {
    $("rt-algo").classList.toggle("on", t === "algo");
    $("rt-repr").classList.toggle("on", t === "repr");
    $("panel-algo").classList.toggle("hidden", t !== "algo");
    $("panel-repr").classList.toggle("hidden", t !== "repr");
  }
  function adjacency() {
    const adj = model.labels.map(() => []);
    for (const e of model.edges) {
      adj[e.u].push([e.v, e.w]);
      if (!model.directed) adj[e.v].push([e.u, e.w]);
    }
    adj.forEach((l) => l.sort((a, b) => a[0] - b[0]));
    return adj;
  }
  function updateRepr() {
    const k = n(), m = model.edges.length;
    const diam = k ? G.diameter() : 0;
    const txt = `<b>${k}</b> sommet${k > 1 ? "s" : ""} · <b>${m}</b> ${model.directed ? "arc" : "arête"}${m > 1 ? "s" : ""}` +
      (k ? ` · diamètre${model.weighted ? " pondéré" : ""} <b>${diam}</b>` : "") + (model.directed ? " · orienté" : "") + (model.weighted ? " · pondéré" : "");
    $("stats").innerHTML = txt; $("repr-stats").innerHTML = txt;
    const adj = adjacency();
    $("ladj").textContent = model.labels.map((l, i) =>
      `${l} → ${adj[i].map(([j, w]) => model.weighted ? `${model.labels[j]}(${w})` : model.labels[j]).join(", ") || "∅"}`).join("\n");
    if (k <= 40) {
      const cell = (i, j) => { const a = adj[i].find(([v]) => v === j); return a ? (model.weighted ? a[1] : 1) : (model.weighted ? (i === j ? 0 : "∞") : 0); };
      $("matrix").innerHTML = `<table class="data"><thead><tr><th></th>${model.labels.map((l) => `<th>${esc(l)}</th>`).join("")}</tr></thead><tbody>` +
        model.labels.map((l, i) => `<tr><td>${esc(l)}</td>${model.labels.map((_, j) => `<td>${cell(i, j)}</td>`).join("")}</tr>`).join("") + "</tbody></table>";
    } else $("matrix").innerHTML = '<p class="hint">Trop grand pour être affiché (40 sommets au plus).</p>';
    const indeg = model.labels.map(() => 0);
    for (const e of model.edges) indeg[e.v]++;
    $("degrees").innerHTML = `<table class="data"><thead><tr><th>sommet</th>${model.directed ? "<th>d⁺ (sortant)</th><th>d⁻ (entrant)</th>" : "<th>degré</th>"}</tr></thead><tbody>` +
      model.labels.map((l, i) => `<tr><td>${esc(l)}</td>${model.directed ? `<td>${adj[i].length}</td><td>${indeg[i]}</td>` : `<td>${adj[i].length}</td>`}</tr>`).join("") + "</tbody></table>";
  }

  // ------------------------------------------------------------------
  // Exports
  function toText() {
    const lines = ["# Graphlab : une arête par ligne"];
    if (model.directed) lines.push("orienté");
    if (model.weighted) lines.push("pondéré");
    const deg = model.labels.map(() => 0);
    for (const e of model.edges) {
      deg[e.u]++; deg[e.v]++;
      lines.push(`${model.labels[e.u]} ${model.labels[e.v]}${model.weighted ? " " + e.w : ""}`);
    }
    model.labels.forEach((l, i) => { if (!deg[i]) lines.push(l); });
    return lines.join("\n");
  }
  function parseText(text) {
    let directed = false, weighted = false;
    const labels = [], index = new Map(), edges = [];
    const id = (name) => {
      if (!index.has(name)) { index.set(name, labels.length); labels.push(name); }
      return index.get(name);
    };
    text.split("\n").forEach((raw, lineNo) => {
      const line = raw.replace(/#.*/, "").trim();
      if (!line) return;
      const low = line.toLowerCase().normalize("NFD").replace(/[̀-ͯ]/g, "");
      if (low === "oriente" || low === "directed") { directed = true; return; }
      if (low === "pondere" || low === "weighted") { weighted = true; return; }
      const words = line.split(/[\s,;]+/).filter((w) => w && !["->", "--", "→", "—"].includes(w));
      if (words.length === 1) { id(words[0]); return; }
      if (words.length > 3) throw new Error(`ligne ${lineNo + 1} : trop de mots`);
      let w = 1;
      if (words.length === 3) {
        w = parseInt(words[2], 10);
        if (!Number.isFinite(w)) throw new Error(`ligne ${lineNo + 1} : poids « ${words[2]} » invalide`);
        weighted = true;
      }
      const u = id(words[0]), v = id(words[1]);
      if (u !== v) edges.push({ u, v, w });
    });
    // doublons
    const seen = new Set();
    const uniq = edges.filter((e) => {
      const key = directed ? e.u + "," + e.v : Math.min(e.u, e.v) + "," + Math.max(e.u, e.v);
      if (seen.has(key)) return false;
      seen.add(key); return true;
    });
    return { directed, weighted, labels, edges: uniq };
  }
  function importText(text) {
    const g = parseText(text);
    // on garde la position des sommets déjà présents
    const old = new Map(model.labels.map((l, i) => [l, model.pos[i]]));
    const flat = []; for (const e of g.edges) flat.push(e.u, e.v, e.w);
    G.setGraph(g.labels, g.directed, flat);
    const fresh = g.labels.length ? Array.from(G.layout()).map((p) => [p[0], p[1]]) : [];
    const allKnown = g.labels.every((l) => old.has(l));
    g.pos = g.labels.map((l, i) => (allKnown ? old.get(l) : fresh[i]));
    loadGraph(g);
  }

  function ocamlCode() {
    const adj = adjacency();
    const labelsLine = `let noms = [| ${model.labels.map((l) => JSON.stringify(l)).join("; ")} |]`;
    const comment = model.labels.length <= 30 ? `(* sommets : ${model.labels.map((l, i) => `${i} = ${l}`).join(", ")} *)\n` : "";
    const list = model.weighted
      ? `(* listes d'adjacence : (voisin, poids) *)\nlet g = [|\n${adj.map((l, i) => `  [${l.map(([j, w]) => `(${j}, ${w})`).join("; ")}];  (* ${model.labels[i]} *)`).join("\n")}\n|]`
      : `(* listes d'adjacence *)\nlet g = [|\n${adj.map((l, i) => `  [${l.map(([j]) => j).join("; ")}];  (* ${model.labels[i]} *)`).join("\n")}\n|]`;
    const cell = (i, j) => { const a = adj[i].find(([v]) => v === j); return a ? (model.weighted ? String(a[1]) : "1") : (model.weighted ? (i === j ? "0" : "inf") : "0"); };
    const mat = `${model.weighted ? "let inf = max_int\n\n(* matrice des poids (inf : pas d'arête) *)" : "(* matrice d'adjacence *)"}\nlet m = [|\n${model.labels.map((_, i) => `  [| ${model.labels.map((_, j) => cell(i, j)).join("; ")} |];`).join("\n")}\n|]`;
    return `${comment}${labelsLine}\n\n${list}\n\n${mat}\n`;
  }

  function tikzCode() {
    if (!n()) return "";
    let x1 = Infinity, y1 = Infinity, x2 = -Infinity, y2 = -Infinity;
    for (const [x, y] of model.pos) { x1 = Math.min(x1, x); y1 = Math.min(y1, y); x2 = Math.max(x2, x); y2 = Math.max(y2, y); }
    const s = 8 / Math.max(x2 - x1, y2 - y1, 1);
    const f = (v) => (Math.round(v * 100) / 100).toString();
    const name = (i) => `v${i}`;
    const lines = [
      `\\begin{tikzpicture}[every node/.style={circle, draw, minimum size=7mm, inner sep=1pt}${model.directed ? ", >=stealth" : ""}]`,
      ...model.labels.map((l, i) => `  \\node (${name(i)}) at (${f((model.pos[i][0] - x1) * s)}, ${f(-(model.pos[i][1] - y1) * s)}) {$${l}$};`),
    ];
    for (const e of model.edges) {
      const bend = model.directed && hasArc(e.v, e.u) ? " [bend left=15]" : "";
      const w = model.weighted ? ` node[midway, fill=white, draw=none, rectangle, inner sep=1pt] {$${e.w}$}` : "";
      lines.push(`  \\draw${model.directed ? "[->]" : ""} (${name(e.u)}) to${bend}${w} (${name(e.v)});`);
    }
    lines.push("\\end{tikzpicture}");
    return lines.join("\n");
  }

  const dlg = $("dlg");
  function openDialog(title, text, { editable = false, hint = "" } = {}) {
    $("dlg-title").textContent = title;
    $("dlg-text").value = text;
    $("dlg-text").readOnly = !editable;
    $("dlg-apply").classList.toggle("hidden", !editable);
    $("dlg-err").textContent = "";
    $("dlg-hint").textContent = hint;
    dlg.showModal();
  }
  $("open-text").addEventListener("click", () => openDialog("Graphe au format texte", toText(), {
    editable: true, hint: "Modifiez le texte puis « Importer ». Une arête par ligne (a b ou a b poids), un nom seul pour un sommet isolé, lignes « orienté » et « pondéré ».",
  }));
  $("open-ocaml").addEventListener("click", () => openDialog("Code OCaml", ocamlCode(), { hint: "Les sommets sont numérotés de 0 à n − 1 dans l'ordre des noms." }));
  $("open-tikz").addEventListener("click", () => openDialog("Figure TikZ", tikzCode(), { hint: "À placer dans un document LaTeX qui charge \\usepackage{tikz}." }));
  $("dlg-close").addEventListener("click", () => dlg.close());
  $("dlg-copy").addEventListener("click", () => {
    navigator.clipboard?.writeText($("dlg-text").value).then(() => { $("dlg-copy").textContent = "Copié ✓"; setTimeout(() => $("dlg-copy").textContent = "Copier", 1200); });
  });
  $("dlg-apply").addEventListener("click", () => {
    try { importText($("dlg-text").value); dlg.close(); }
    catch (err) { $("dlg-err").textContent = "Erreur : " + err.message; }
  });

  $("share").addEventListener("click", () => {
    const url = location.href.split("#")[0] + "#g=" + b64encode(JSON.stringify(compact()));
    history.replaceState(null, "", url);
    navigator.clipboard?.writeText(url).then(
      () => { $("share-msg").textContent = "Lien copié dans le presse-papiers (il contient tout le graphe)."; },
      () => { $("share-msg").textContent = "Lien placé dans la barre d'adresse."; });
  });
  $("png").addEventListener("click", () => {
    const w = 1600, h = 1100, off = document.createElement("canvas");
    off.width = w; off.height = h;
    const c = {}; fitCamera(c, w, h, 6 * R() + 80);
    drawScene(off.getContext("2d"), w, h, c, LIGHT, { plain: !step });
    download(off.toDataURL("image/png"), "graphe.png");
  });
  $("save-json").addEventListener("click", () => {
    const blob = new Blob([JSON.stringify(compact(), null, 1)], { type: "application/json" });
    const url = URL.createObjectURL(blob);
    download(url, "graphe.json");
    setTimeout(() => URL.revokeObjectURL(url), 2000);
  });
  $("open-json-btn").addEventListener("click", () => $("open-json").click());
  $("open-json").addEventListener("change", async (e) => {
    const f = e.target.files[0]; if (!f) return;
    try { loadGraph(uncompact(JSON.parse(await f.text()))); }
    catch (err) { flash("Fichier illisible : " + err.message); }
    e.target.value = "";
  });
  function download(href, name) {
    const a = document.createElement("a"); a.href = href; a.download = name;
    document.body.appendChild(a); a.click(); a.remove();
  }

  // ------------------------------------------------------------------
  // Clavier
  document.addEventListener("keydown", (e) => {
    if ($("page-graph").classList.contains("hidden") || dlg.open) return;
    const t = document.activeElement;
    const typing = t && (t.tagName === "TEXTAREA" || (t.tagName === "INPUT" && t.type !== "range" && t.type !== "checkbox"));
    if (typing) return;
    const k = e.key.toLowerCase();
    if ((e.ctrlKey || e.metaKey) && k === "z") { e.preventDefault(); e.shiftKey ? redo() : undo(); return; }
    if ((e.ctrlKey || e.metaKey) && k === "y") { e.preventDefault(); redo(); return; }
    if (e.ctrlKey || e.metaKey || e.altKey) return;
    if (t && t.tagName === "SELECT") return;
    if (k === "d") setMode("move");
    else if (k === "s") setMode("vertex");
    else if (k === "a") setMode("edge");
    else if (k === "x") setMode("delete");
    else if (k === "f") fit();
    else if (k === "escape") { interaction.selected = -1; draw(); }
    else if (e.key === "ArrowRight" && stepCount) { e.preventDefault(); stopPlay(); setStep(stepIndex + 1); }
    else if (e.key === "ArrowLeft" && stepCount) { e.preventDefault(); stopPlay(); setStep(stepIndex - 1); }
    else if (e.key === " " && stepCount) { e.preventDefault(); togglePlay(); }
  });

  // ------------------------------------------------------------------
  // Onglets de la page
  function showPage(p) {
    $("tab-graph").classList.toggle("on", p === "graph");
    $("tab-flood").classList.toggle("on", p === "flood");
    $("page-graph").classList.toggle("hidden", p !== "graph");
    $("page-flood").classList.toggle("hidden", p !== "flood");
    if (p === "graph") resize();
    window.dispatchEvent(new CustomEvent("graphlab-page", { detail: p }));
  }
  $("tab-graph").addEventListener("click", () => showPage("graph"));
  $("tab-flood").addEventListener("click", () => showPage("flood"));

  // ------------------------------------------------------------------
  // Démarrage : lien de partage, sinon sauvegarde locale, sinon exemple
  function initialGraph() {
    const m = location.hash.match(/#g=([\w-]+)/);
    if (m) { try { return uncompact(JSON.parse(b64decode(m[1]))); } catch (_) {} }
    try { const s = localStorage.getItem(STORE); if (s) return uncompact(JSON.parse(s)); } catch (_) {}
    const g = fromOCaml(G.generate("pondere", []));
    g.weighted = true;
    return g;
  }
  model = Object.assign(model, initialGraph());
  new ResizeObserver(resize).observe($("canvas-wrap"));
  $("algo").value = model.weighted ? "dijkstra" : "bfs";
  structureChanged();
  resize();
  fit();
  setMode("move");
  window.graphlabApp = { get model() { return model; }, toScreen, runAlgo, setStep, importText, toText, ocamlCode, tikzCode };
})();
