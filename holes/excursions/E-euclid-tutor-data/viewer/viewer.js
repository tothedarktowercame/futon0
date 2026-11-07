/* E-euclid-tutor viewer: step through an euclid_engine replay with each
 * drawn object aligned, both ways, to the Lean euclid_apply line that
 * introduced it. Diagram objects are JSXGraph's own construction
 * primitives (circle through point, intersection, compass, glider), so
 * dragging a free point keeps the construction valid. */

"use strict";

const params = new URLSearchParams(location.search);
const prop = params.get("prop") || "1";
const traceUrl = "traces/prop0" + prop + ".json";
document.getElementById("prop-select").value = prop;

let trace = null;
let cur = 0;              // current step (0 = hypotheses only)
let board = null;
let built = {};           // name -> JSXGraph element
let els = {};             // lean line number -> <tr>

const HI = "#ff9500";

document.getElementById("prop-select").onchange = () => {
  location.search = "?prop=" + document.getElementById("prop-select").value;
};
document.getElementById("btn-fwd").onclick = () => setStep(Math.min(cur + 1, trace.steps.length));
document.getElementById("btn-back").onclick = () => setStep(Math.max(cur - 1, 0));
document.getElementById("btn-reset").onclick = () => setStep(0);
document.getElementById("toggle-aux").onchange = () => draw();

fetch(traceUrl).then(r => r.json()).then(t => {
  trace = t;
  window.__TRACE = t;  // exposed for the headless self-check only
  renderLean();
  setStep(0);
}).catch(function (e) {
  document.getElementById("step-info").textContent =
    "failed to load " + traceUrl + " — serve this directory over HTTP "
    + "(e.g. python3 -m http.server) rather than file://: " + e;
});

// ---------------------------------------------------------------- lean pane

function renderLean() {
  const wrap = document.getElementById("lean-wrap");
  const stepAtLine = {};
  trace.steps.forEach(s => { stepAtLine[s.lean.line] = s; });
  const tbl = document.createElement("table");
  tbl.className = "lean";
  let trs = [];
  trace.lean_text.forEach((text, i) => {
    const ln = i + 1;
    const tr = document.createElement("tr");
    const step = stepAtLine[ln];
    if (step) {
      tr.className = "step-line future";
      tr.dataset.step = step.n;
      tr.onmouseenter = () => hoverStep(step.n, true);
      tr.onmouseleave = () => hoverStep(step.n, false);
      tr.onclick = () => setStep(step.n);
    }
    const td1 = document.createElement("td");
    td1.className = "ln";
    td1.textContent = ln;
    const td2 = document.createElement("td");
    td2.className = "code";
    td2.textContent = text;
    tr.appendChild(td1); tr.appendChild(td2);
    tbl.appendChild(tr);
    els[ln] = tr;
  });
  wrap.innerHTML = "";
  wrap.appendChild(tbl);
}

// ---------------------------------------------------------------- diagram

function draw() {
  if (board) JXG.JSXGraph.freeBoard(board);
  document.getElementById("jxgbox").innerHTML = "";
  board = JXG.JSXGraph.initBoard("jxgbox", {
    boundingbox: [-2.5, 3.5, 7, -3], axis: false,
    showCopyright: false, showNavigation: true,
  });
  // flex layouts can give JSXGraph a zero-height first measurement;
  // re-measure explicitly before anything is drawn
  const box = document.getElementById("jxgbox");
  board.resizeContainer(box.clientWidth, box.clientHeight);
  board.setBoundingBox([-2.5, 3.5, 7, -3], true);
  built = {};
  const showAux = document.getElementById("toggle-aux").checked;
  for (const o of trace.objects) {
    if (o.def_step > cur) continue;
    if (o.aux && !showAux) {
      // build invisible: later intersections and gliders depend on it
      built[o.name] = mk(o, true);
      if (built[o.name]) built[o.name].setAttribute({visible: false});
      continue;
    }
    built[o.name] = mk(o, false);
    const el = built[o.name];
    if (el) {
      // native DOM listeners on the rendered SVG node: reliable in every
      // browser, unlike JSXGraph's internal "over" event plumbing
      const over = () => hoverObj(o, true);
      const out = () => hoverObj(o, false);
      if (el.rendNode) {
        el.rendNode.addEventListener("mouseenter", over);
        el.rendNode.addEventListener("mouseleave", out);
        el.rendNode.addEventListener("pointerenter", over);
        el.rendNode.addEventListener("pointerleave", out);
      }
      el.on("over", over);
      el.on("out", out);
    }
  }
}

function mk(o, aux) {
  const j = o.jsx;
  const base = { name: o.name, withLabel: true, strokeColor: "#3366aa",
                 highlightStrokeColor: HI, fixed: false };
  switch (j.type) {
    case "point":
      if (o.def_step === 0) {
        return board.create("point",
          [trace.placement[o.name][0], trace.placement[o.name][1]],
          Object.assign(base, {fillColor: "#3366aa",
                                highlightFillColor: HI}));
      }
      return board.create("point", [0, 0], Object.assign(base, {fillColor: "#3366aa"}));
    case "line":
      return board.create("line", [built[j.p[0]], built[j.p[1]]],
                          Object.assign(base, {strokeColor: o.hyp ? "#999" : "#3366aa"}));
    case "circle":
      if (j.radius_from) {
        const p1 = built[j.radius_from[0]], p2 = built[j.radius_from[1]];
        const rad = function () { return p1.Dist(p2); };
        return board.create("circle",
          [built[j.center], rad],
          Object.assign(base, {}));
      }
      return board.create("circle", [built[j.center], built[j.through]],
                          Object.assign(base, {}));
    case "compass":
      return board.create("compass",
        [built[j.from[0]], built[j.from[1]], built[j.center]],
        Object.assign(base, {}));
    case "glider": {
      // place at the recorded sample position on its circle
      const g = board.create("glider",
        [trace.placement[o.name][0], trace.placement[o.name][1], built[j.on]],
        Object.assign(base, {fillColor: "#3366aa"}));
      return g;
    }
    case "intersection": {
      const parents = j.parents.map(p => built[p]);
      const el = board.create("intersection", parents.concat([j.idx]),
                              Object.assign(base, {fillColor: "#3366aa"}));
      return el;
    }
    default:
      throw new Error("unknown jsx recipe " + j.type);
  }
}

// aux objects are built with visible:false only when the toggle is off;
// when it is on they are ordinary visible objects
// (handled in draw()).

// ---------------------------------------------------------------- hover

function stepObjects(n) {
  return trace.objects.filter(o => o.def_step === n);
}

function hoverStep(n, on) {
  const step = trace.steps.find(s => s.n === n);
  if (step && els[step.lean.line])
    els[step.lean.line].classList.toggle("hover", on);
  for (const o of stepObjects(n)) {
    const el = built[o.name];
    if (!el) continue;
    el.setAttribute(on
      ? {strokeColor: HI, fillColor: HI, highlightFillColor: HI}
      : {strokeColor: "#3366aa", fillColor: "#3366aa"});
  }
  if (on) showInfo(step);
  else showInfo(currentStep());
}

function hoverObj(o, on) {
  hoverStep(o.def_step, on);
}

// ---------------------------------------------------------------- stepping

function currentStep() { return trace.steps.find(s => s.n === cur) || null; }

function showInfo(step) {
  const info = document.getElementById("step-info");
  if (!step) {
    const v = trace.verification.map(c => c.check + (c.ok ? " ✓" : " ✗"));
    info.innerHTML = "<b>hypotheses</b> — step ▶ to begin. "
      + "Verification of the final placement: " + v.join("; ");
    return;
  }
  const facts = step.facts_added.map(f =>
    "<span class='fact'>" + f.join(" ") + "</span>").join("");
  info.innerHTML =
    "<b>step " + step.n + "</b> <code>" + step.rule + "("
    + step.args.join(", ") + ")</code> → "
    + "<code>" + step.lean.file + ":" + step.lean.line + "</code> — "
    + facts + "<br>placement: " + step.placement_decision;
}

function setStep(n) {
  cur = n;
  window.__CUR = n;  // for the headless self-check
  document.getElementById("step-count").textContent =
    "step " + n + " / " + trace.steps.length;
  for (const s of trace.steps) {
    const tr = els[s.lean.line];
    if (!tr) continue;
    tr.classList.toggle("done", s.n <= cur);
    tr.classList.toggle("future", s.n > cur);
  }
  draw();
  showInfo(currentStep());
}
