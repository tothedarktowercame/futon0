# M-capability-star-map: the mission landscape as a navigable capability graph

**Type:** Mission
**Lifecycle:** HEAD (drafted 2026-06-07 from operator-shape intake)
**Owner:** claude-1 (pending operator-direction)
**Home-repo:** futon0 (workspace-hygiene + cross-repo coordination home, adjacent to the other
capability missions; per `single-locus/mission-home`)
**Pairing:** TBD — natural co-owner is whoever holds M-stack-stereolithography (the posterior render)

**Cross-references (read-only intake):**
- `futon7/holes/M-futon-forward-model.md` — §13 (the forward-looking build brief) + the
  `.semilattice.edn` proto-graph (missions → regions → basins); the forward model's flat
  valuable-path is the **prior this mission graphs properly**.
- `futon5a/holes/missions/M-stack-stereolithography.md` — INSTANTIATE; owns the render of the
  **posterior** (the "exists / coming-out-of-the-bath" semilattice over the substrate-2 commit-DAG).
- `futon5/holes/missions/M-differentiable-code.md` — the **code-level** homolog (code as a graph,
  priors-vs-posteriors); its §8 registers **node-granularity (gap #1) as `:held` on the keystone
  `M-substrate-metric`** — *do not solve granularity here.*
- `futon3c/holes/campaigns/C-substrate-completion.md` + `M-substrate-metric` — the keystone that
  **owns node-granularity** (resolved once, shared prerequisite). This mission **consumes** that
  grain; it must not fork its own.
- `futon0/holes/missions/M-the-futon-stack.md` — the homeostatic-agent mission ("code while I
  sleep"); the star-map is the **navigation prior** the overnight WM steers by.

---

## HEAD

### The question

Today the WM's autonomous selection ranks missions by expected free energy and surfaces **mega-missions**
(cycle 1, 2026-06-07: the EFE-top pick was the 986-line `M-futonzero-mvp`, with no hole closeable in
one cycle; the pilot had to fork-resolve to a tractable target — see M-futon-forward-model §13.2). The
landscape has structure the flat ranking can't see: some nodes are **big undeveloped capabilities**
(legitimately large holes), others are **small, meaningful, single-cycle targets**, and they sit in a
**dependency order** (foundational → composite).

**The question:** can we represent the entire mission landscape as a **graph** — capabilities as
nodes, prerequisites as edges, with a granularity axis (big-hole vs single-cycle-leaf) — such that an
operator (or the WM) can *see where the small meaningful targets are* without losing the big holes?

### The two analogies the operator named

- **Rob's proof-graph.** A large proof is made tractable by decomposing it into a graph of lemmas /
  subgoals at a workable grain. The half-million-LOC capability is the same: a big "proof" whose
  navigability comes from graph decomposition. (Rob gets node-uniformity *by construction* — the
  ≤800-line refactor IS the chunking; M-differentiable-code §"node granularity" records this.)
- **The Khan-Academy star-map.** Khan's knowledge map is a **prior** over what gets built/learned:
  addition → multiplication → … → differential calculus, a prerequisite graph from small foundational
  skills to large composite ones. This mission's PRIOR is the futon analogue: the graph of *intended*
  capabilities and their build-order.

### The prior / posterior structure (the load-bearing frame)

| | What | Where it lives (status) |
|---|---|---|
| **PRIOR** | the star-map — *intended* capabilities, prerequisite edges, build-order | **this mission (new)** |
| **POSTERIOR** | the stereolithographic print — *real, built* capabilities | `M-stack-stereolithography` (INSTANTIATE — exists) |
| **the grain** | node-granularity that makes both commensurable | `M-substrate-metric` (keystone, `:held` — **consume, don't fork**) |

This is the same priors-vs-posteriors shape as `M-webarxana-as-monitor` (wiring = prior, running graph
= posterior), lifted from the wiring/code level to the **capability** level. The prior says *what we
mean to build*; the stereolithographic posterior says *what is actually in the bath*; their difference
is the operator's real forward question.

### What is real now vs what this mission adds (evidence-first, no chained-claims)

- **Real now:** the `.semilattice.edn` proto-graph (missions → regions → basins); the per-mission
  `:open-hole-count` (futon2 mission-registry, live — but a raw count, *not* a grain); the
  stereolithographic posterior render (M-stack-stereolithography, INSTANTIATE); the futon1a hypergraph
  as a candidate graph store; M-substrate-metric as the *held* granularity keystone.
- **The gap this mission adds:** the prior as a proper **dependency graph with a granularity axis** —
  capabilities as nodes carrying prerequisite edges and a big-hole-vs-single-cycle-leaf tag. The flat
  forward-model valuable-path becomes a navigable star-map.

### The operational payoff (why now)

Cycle 1 produced a concrete datum: **the WM ranks large missions whose holes are not single-cycle
units, so pilot fork-resolution is currently load-bearing.** A star-map with a granularity axis lets
the WM (and the operator) pick **single-cycle leaves** directly — turning fork-resolution from a manual
necessity into a graph query. It is the proper form of the "advanceability signal" M-futon-forward-model
§13.2 flagged.

### Design space (sketch — NOT commitments; IDENTIFY decides which are real)

- **Node model** — what is a capability node? Candidates: a mission, a mission-phase, a hole, a
  capability that spans missions. The grain question routes to the keystone (M-substrate-metric); this
  mission picks the node *type*, not the granularity *resolution*.
- **Edge model** — prerequisite / enables / specialises / blocks. Some already exist as prose
  (`:depends-on`, `:enables` annotations); the graph extracts + types them.
- **Store** — futon1a hypergraph (M-differentiable-code's candidate) vs a flat EDN sibling of
  `.semilattice.edn`. (futon1a caveat: POST /entity REPLACES props — fetch-merge-upsert.)
- **The granularity tag** — big-hole vs single-cycle-leaf — **derived from the keystone's grain**, not
  invented here. Until the keystone lands, a *crude proxy* (mission-size + hole-count + "is one hole a
  single-cycle unit?") may stand in, **explicitly flagged as a proxy**.
- **Render** — coordinate with M-stack-stereolithography (it owns the posterior render); the prior
  star-map may share or sibling that viewer (one model, two layers: intended vs built).

### Exclusion discipline (inherited, non-negotiable)

Any landscape extractor MUST exclude **worktrees / futon3 origin material / `<repo>/.state` sandbox
checkouts** — the same hazard as the substrate-2 ingest rule and M-differentiable-code §"which trees
are canonical" (and the same over-exclusion bug fixed in the mission-registry this session, `2cd7445`).
A contaminated landscape is a contaminated prior.

### The Baudrillard caveat (from the sibling discipline)

A high-resolution star-map is itself an instrument that can **substitute** for the judgement it should
serve (M-the-perfect-crime's worry). The map is a prior to *steer by*, not a forecast to *trust*
(M-futon-forward-model's descriptive-not-predictive wall holds here too). Build it to make small
targets *visible*, not to make the future *look settled*.

### HEAD exit criteria (operator-verified)

- Operator confirms the prior/posterior framing + the Khan-star-map / Rob-proof-graph analogies capture
  the intent.
- Operator confirms node-granularity stays **keystone work** (M-substrate-metric), consumed here, not
  forked — and decides whether the **crude proxy** is in-scope (interim) or excluded (wait for the
  keystone).
- Operator names initial owner(s) / pairing (esp. with M-stack-stereolithography for the render).
- Operator confirms the relationship to M-futon-forward-model: is the star-map FFM's **phase-2 graph
  form**, or a sibling FFM **feeds** (the semilattice as input)?

### Carried-forward tensions (for IDENTIFY)

- **Grain dependency.** This mission is gated on the keystone for the *granularity resolution*; it can
  proceed on node/edge *modelling* now, but the single-cycle-leaf tag is only as good as the grain.
  Risk: building the graph encourages a unilateral grain shortcut — the constitution forbids it.
- **Prior vs posterior drift.** The star-map (intended) and stereolithograph (built) will disagree;
  that disagreement is the *signal*, not an error to reconcile away (cf. combining-methods-as-diagnostic).
- **Proxy debt.** A crude advanceability proxy unblocks the WM now but risks becoming the de-facto grain
  (Goodhart). If used, it must be `:prototyping-forward` with an explicit retire-on-keystone clause.
- **Scope boundary.** "The entire mission landscape" is ~125 backlog missions across the stack; the
  graph is large. IDENTIFY must bound the first slice (likely: one region, end-to-end, before the whole).

---

## IDENTIFY (pending)

Pending operator HEAD-verify.

---

## Provenance

- Operator seed: emacs-repl, 2026-06-07, Joe — "convert the entire mission landscape into a big graph …
  big holes (mega-missions) for big undeveloped capabilities, but also know where the small and
  meaningful targets are … a Khan-Academy star-map prior + the stereolithographic print as the map of
  real capabilities. Rob has done the same with a big proof."
- Spawned from: M-futon-forward-model WM watched-cycle 1 (2026-06-07), whose re-route datum (§13.2)
  surfaced node-granularity / advanceability as the operative gap.
