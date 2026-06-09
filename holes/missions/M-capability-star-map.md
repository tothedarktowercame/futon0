# M-capability-star-map: the mission landscape as a navigable capability graph

**Type:** Mission
**Lifecycle:** INSTANTIATE — mechanism proven 2026-06-07 (Unit B EFE-over-graph + INV-G verified + accepted; Unit A pipeline accepted, extraction hollow = `:prototyping-forward`). Integration DEFERRED until REAL data assembled (operator A). **Region sweep COMPLETE + C3 PASS (production-faithful) 2026-06-07** — capability inventory grounded across all 7 thesis-regions (C-pudding-prover §8.2.1–8.2.7); EFE top-ranks an applicable single-cycle leaf with no cherry-pick (keystone `efe-trustworthy-over-starmap` **demonstrated**). **Mission OPEN pending operator close** — caveats: branch `wm-outing/2026-06-07` unmerged · curated candidate set (not full registry scan) · EFE body-term + decompose hardening · VSATARCS regen (doc-gap).
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

## IDENTIFY

Operator direction (2026-06-07): **build this end-to-end *before* the next WM cycle.** Success
criterion (verbatim sense): *the next WM cycle should not need the pilot to cherry-pick a discrete
task — we should trust EFE much more, because EFE will be doing real Active Inference over the
star-map.* (This settled two HEAD questions: no crude proxy; the node model is the lambda/scope
metaphor — the design now lives in the DERIVE-DRAFT below, held for re-anchoring after MAP.)

### The gap (multi-faceted — not "just structure")

The stack has ~125 backlog missions and a real shipped capability set, but **no navigable graph of
capabilities and the missions that mint them** — and that absence is *several* gaps, not one. The
operative reframe (Joe 2026-06-07): a capability star-map **is a build system** — capabilities are
build-targets with dependencies, built in topological order, only the *stale* ones rebuilt, and a
**scheduler** (the WM, with EFE as its cost function) picks the next target. We have no "Makefile for
capabilities." It decomposes:

**Clarity gap (load-bearing):** **missions ≠ capabilities.** A mission is a unit of *work*; a
capability is an *ability the stack has*; they are many-to-many. We have been conflating them. The
bipartite incidence (missions ⊔ capabilities) separates them — and *which is the node* changes what
"done" and "stale" mean (the build-system duality between missions and features).

**Representation gaps (missing as DATA):**
- **A1.** Missions carry substrate-2 *attributes* (`:mission/{phase,criteria,gates,obligations,
  blocked-by,code-paths,…}`) but no structural *form* — no DEFN/MAP-REDUCE shape; the lifecycle itself
  has no `.edn` analogue (only `mission-lifecycle.md` + a `.flexiarg`). So missions can't be uplifted as
  composable nodes. *(Joe's original (1).)*
- **A2.** No **capability inventory** — capability-nodes don't exist beyond ~10 pudding-prover theses +
  a hand-authored handful.
- **A3.** No **typed edges** — `:blocked-by` is partial + untyped; no *requires* / *uses* / *enables* /
  *built-before* distinction (logical vs temporal order conflated).
- **A4.** The missions≠capabilities split is not in the data.
- **A5.** **Granularity** — node sizes span four orders of magnitude (keystone, M-substrate-metric;
  consumed here, not forked).

**Function gaps (what we can't DO):**
- **B1.** The scheduler (WM) is blind — ranks a flat set, surfaces mega-missions (the cycle-1 trigger).
- **B2.** **No navigation** — the operator can't see "where am I / what's reachable / path to X" (the
  star-map's whole point).
- **B3.** Can't read **done-vs-todo / shipped-vs-holes** at a glance.
- **B4.** Can't **compose** to a target capability (Rob's lemmas compose as data; our missions don't).
- **B5.** Can't **check** a capability claim (is X really shipped — the perfect-crime; needs the code
  grounding = the deferred mission→code bonus).

**NOT a gap (corrected, Joe 2026-06-07):** a *north-star*. **C-pudding-prover already supplies ascent**
— the Daumal altitude/thesis hierarchy tells us whether we are advancing, "to the top" or "in between."
The map is not unoriented; the real gap is **connecting the star-map to the ascent** so the scheduler's
EFE knows "toward the top," not inventing an orientation.

**Also real (landscape honesty):** the map must represent **dead-ends** — NONSTARTER / superseded /
re-routed missions (M-stack-stereolithography's "what's real" *includes what died*); a happy-path-only
map lies.

### Theoretical anchoring
- **Build system / tech-tree** (targets, typed deps, topological order, incrementality, staleness; WM =
  scheduler, EFE = cost function) — the operative frame.
- **Khan star-map** (curriculum + navigation) and **Rob's proof-DAG** (composition + local
  checkability) — the seed analogies.
- **C-pudding-prover** (capabilities are *minted*; the ascent = the orientation).
- **AIF+ / Bayesian-structure-learning** (EFE over the structure) + **lambda calculus / wiring
  diagrams** (the DERIVE-draft below).

### Scope (Joe 2026-06-07: ONE complex mission — not a family, and not a cheap win)
- **In:** the structured mission/capability *form* (the node); the bipartite incidence + both
  projections; typed edges; the capability inventory (seeded from the prover + extracted); the
  build-graph + the scheduler-EFE wiring; a navigation surface.
- **Out / consumed:** node-granularity *resolution* (keystone, M-substrate-metric); the render
  (M-stack-stereolithography owns it); the mission→code grounding (deferred end-bonus); the predictive
  arm; **living hole-DISCOVERY** (auto-surfacing *new/undiscovered* holes from turns — M-a-sorry-
  enterprise, the likely *next* mission; the map is a **fixed curriculum** until then — see MAP §Limitation).
- **One mission, complex.** The facets are interdependent (can't type edges without the form; can't
  schedule without the edges) so they don't cleanly fork — *and that interdependence is exactly why this
  is not a cheap win*, and why the IDENTIFY must guard against the inverse over-collapse (cram five gaps,
  declare done when one lands).

### Completion criteria (testable)
1. A structured mission/capability *form* exists (substrate-2-uplift-able) — the DEFN/MAP-REDUCE node.
2. The capability inventory + typed-edge graph exists for at least one region (the bounded first slice).
3. The WM's EFE, run over the *extracted* graph, returns an **applicable single-cycle leaf** as its top
   pick — *no pilot cherry-pick* (the cycle-1 gate).
4. The "WM overnight unsupervised" keystone path (ensemble 1) is reproduced by the extracted graph, not
   only hand-authored.

### Relationship to other missions
**Consumes:** M-substrate-metric (granularity keystone), C-pudding-prover (capability registry +
ascent), M-stack-stereolithography (posterior + render), M-differentiable-code (code-graph homolog +
scope-grain), M-futon-forward-model (the prior it graphs). **Enables:** the WM overnight cadence
(T4.2), M-the-futon-stack's "code while I sleep."

### Source material
`mission-lifecycle.md` + `.flexiarg`; the substrate-2 `:mission/*` props (the mission watcher);
`futon5/data/missions/*-exotype.edn` (existing structured mission forms); `pudding-prover-registry.edn`;
`.semilattice.edn`; the scattered `:depends-on` / `:enables` annotations.

### Owner + deps
Owner claude-1 (pending operator-direction confirmation). Repos: futon0 (home), futon3c (substrate-2 /
WM), futon2 (mission-registry), futon5 (exotypes), futon7 (prover, FFM).

**Exit criterion (IDENTIFY):** Joe agrees this multi-faceted gap is the real one and the scope (one
complex mission) is right. THEN a real **MAP** (the survey), THEN the DERIVE-draft below gets
re-anchored.

---

## MAP

MAP is research — facts, not decisions (survey what exists toward each IDENTIFY gap *before* the
DERIVE-draft is re-anchored). Q1–Q7 each tie to a gap; answered with concrete findings + counts; the
ready-vs-missing table closes the phase.

- **Q1 (A1 — structural form):** What does the substrate-2 mission **watcher** already extract toward a
  structural mission form? Full `:mission/*` prop set + how each is derived + the `/api/alpha/missions`
  shape + the watcher code.
- **Q2 (A1 — lifecycle / DEFN form):** Any *structured* form of the lifecycle or a mission-as-DEFN /
  map-reduce? `mission-lifecycle.flexiarg`; `futon5/data/missions/*-exotype.edn` (what they structure;
  bespoke or schema?); `futon3c/docs/futonic-missions.md` §GF.
- **Q3 (A2/A4 — capability inventory + split):** Capability-node sources + counts (pudding-prover
  theses; `:mission/criteria` as proxy; semilattice regions; VSATARCS evidence-kinds). Does any data
  distinguish a *capability* (ability) from a *mission* (work)?
- **Q4 (A3 — typed edges):** How complete + typed is the dependency-edge data? `:mission/blocked-by`
  coverage; scattered `:depends-on`/`:enables` (count/format/where); any *typed* edge or all untyped?
- **Q5 (build-system — order + staleness):** Temporal + staleness data for a topological order +
  incrementality (`:mission/{days-stale,mtime,date,cycles,…}`; piano_roll; any existing DAG/topsort).
- **Q6 (B1 — scheduler):** What does the WM `forward-model` compute EFE *over*, and how (flat vs
  structured)? (`futon2.aif.forward-model`.)
- **Q7 (B2 — navigation):** Existing mission graph / navigation surfaces (Mission Control; the
  arxana-browser missions view; the semilattice; VSATARCS; the operator portal).

### Findings (2026-06-07; three parallel read-only research passes)

- **Q1 (watcher):** `futon3c/.../watcher/file_ingest.clj:683` + `mission_control_backend.clj:571` extract
  ~24 `:mission/*` props — by **parsing the markdown** (status/title/date/blocked-by/owner/summary/
  cross-refs/code-paths/phase/gates/psrs/purs) + filesystem (mtime/days-stale) + evidence (turn-counts)
  + devmap overlay. Served flat via `GET /api/alpha/missions`; source-of-truth = substrate-2 hyperedges
  (`code/v05/mission-doc`). It is a **markdown→attributes *projector*, not a mission-as-computation
  mapper** — it captures what a mission *says*, not what it *does*. `mission-shapes.clj` has Malli
  `Obligation/Cycle/MissionState` shapes **designed but not cross-repo-populated**.
- **Q2 (lifecycle / DEFN form):** `mission-lifecycle.flexiarg` is a **state-machine** (governance prose,
  not computable EDN). BUT the **exotype `.edn`s** (`futon5/data/missions/*-exotype.edn`) ARE a real
  **mission-as-wiring schema** — `{:ports{:input :output} :components :edges :invariants}` with typed
  I/O + timescales, validated by `futon5.ct.mission/validate`. **Sparse (3 missions), hand-authored,
  NOT wired to the watcher.** So a structural form *exists as a schema* — just not generic or live.
- **Q3 (capabilities + split):** pudding-prover = **44 proof-states + 7 kit** (the capability seed).
  Mission criteria are **artifact-shaped, not capability-shaped** (58/211 have criteria; ~22% name an
  ability). **Capabilities ≠ missions are conceptually distinct (proof-state vs kit) but operationally
  conflated** — no `:capability` entity, no mission→capability edge.
- **Q4 (typed edges):** pudding-prover has systematic typed edges (`:parent`×29, `:specialises`×1,
  `:couples`×2); closure annotations carry **typed** `:enables [{:action :capability}]` (15+, v0.2.2+).
  Missions: `:blocked-by` 35/211, **untyped** ("Blocked by: None" is modal). No generic
  `:requires/:uses/:produces/:built-before`.
- **Q5 (order/staleness):** mtime→`:days-stale` (>7d ⇒ stale) + turn-counts READY; **no topological
  sort / DAG exists anywhere** (grep topo/toposort/dag = 0). piano_roll reads Status-line dates, not git.
- **Q6 (scheduler):** `forward_model.clj:25` = 5 flat action-classes; `:open-mission` targets =
  `mission-registry/open-missions` (**flat vec, one action/mission**); `efe.clj` scores **independent
  (state,action) pairs**, sorts by G-total. **The EFE is FLAT — zero structure/dependency-awareness.**
  (B1, confirmed at the source.)
- **Q7 (navigation):** five surfaces (Mission-Control list · arxana-browser repo/status groups ·
  semilattice centrality ranking · VSATARCS narrative · Programme card by salience) — **all
  lists/rankings; none a navigable dependency graph.**

### Ready vs missing
| Gap | READY (reusable) | MISSING (the work) |
|---|---|---|
| **A1 form** | watcher props (parsed); **exotype schema** (ports/components/edges/invariants); Malli mission-shapes | exotype sparse(3)+hand-authored+not-wired; no *generic* DEFN/map-reduce form; lifecycle has no `.edn` |
| **A2 capabilities** | pudding-prover 44+7; closure `:enables` | missions don't *declare* capabilities; no `:capability` entity; no mission→capability edge |
| **A3 typed edges** | prover `:parent/:specialises/:couples`; closure typed `:enables` | mission `:blocked-by` untyped (35/211); no `:requires/:uses/:produces/:built-before` |
| **A5 granularity** | `:altitude` (prover ordering); `:open-hole-count` (live) | keystone (M-substrate-metric); single-cycle-leaf tag |
| **order/staleness** | mtime/`:days-stale`; turn-counts; piano_roll dates | **no toposort/DAG anywhere** |
| **B1 scheduler** | `efe.clj` compute-efe/rank-actions; 5 action-classes | **EFE flat — no structure/dependency awareness** |
| **B2 navigation** | 5 list/ranking surfaces | **none is a dependency graph** |

### Surprises (these change the DERIVE)
1. **The structural form is half-built, not greenfield.** The **exotype schema** (`:ports/:components/
   :edges/:invariants`, validated by `futon5.ct.mission/validate`) is *already* a mission-as-wiring-
   diagram form — the DERIVE should **generalize the exotype**, not invent a form. (Plus the Malli
   `mission-shapes.clj`.)
2. **Typed capability-edges already exist in a corner.** Closure annotations' `:enables
   [{:action :capability}]` is the typed mission→capability edge, in 15+ closures — a precedent +
   extraction source, not clean-slate.
3. **The pudding-prover is the only systematic capability + typed-edge structure** — the natural seed
   for both the inventory (44+7) and the edge vocabulary, confirming IDENTIFY's "capabilities are minted
   = read from the prover."
4. **The EFE is genuinely flat and there is NO DAG anywhere** — so "EFE over the structure" has **no
   existing scaffolding**; it is the substantive new build (B1), and it can't borrow a toposort because
   none exists.

### Limitation — fixed curriculum vs living map (the M-a-sorry-enterprise boundary, Joe 2026-06-07)

A 5th finding, and a real scope-boundary. **Turn→pattern tagging is LIVE** — the stack produces a
per-turn context-retrieval certificate over 853+ patterns, and `loop_learning.clj`'s `:patterns-applied`
codes each frame's structural patterns (the M-pattern-application-diagnostic capability). **Turn→sorry
tagging is NOT** — discovering a *sorry* (a hole) from a turn is **M-a-sorry-enterprise**
(`futon5a/holes/missions/`, IDENTIFY/Design, **blocked on ≥5 sessions of retrieval data**); its own
insight is that the per-turn retrieved-patterns *overlap-signal* an emergent sorry.

**Consequence:** the star-map we can build *now* draws its holes from **known** sorries (the
`:open-hole-count` hole-counter over mission docs) + hand-curation — so it is a **fixed curriculum**
(known capabilities, known holes). It becomes a **living map** — auto-surfacing *new, previously
undiscovered* holes/capabilities from the work itself — only once M-a-sorry-enterprise lands the
turn→sorry mining. Identifying holes is what we are doing **live and hands-on right now** (this very
session); automating the discovery of *new* holes is next-level.

**This is not a defect:** Khan's star-map *is* a fixed curriculum and a build-system's Makefile *is*
static — the fixed version is the faithful v1. The discipline is to **build so the living layer plugs
in** (the hole-counter is the fixed hole-source; the turn→sorry miner becomes a *second* source later),
not to pretend the v1 is living. M-a-sorry-enterprise is the likely **next mission after this one**, and
is anyway downstream (data-blocked).

**MAP exit:** every Q1–Q7 has a concrete answer; the ready-vs-missing table is complete. Headline — the
*form* + *capability-seed* + *typed-edge precedent* are **more ready** than IDENTIFY assumed (generalize
the exotype + the prover); the *scheduler-over-structure*, the *navigation graph*, and a *toposort* are
**genuinely greenfield**. The DERIVE-draft below re-anchors here.

---

## DERIVE (re-anchored on MAP, 2026-06-07)

The MAP found the *form*, *capability-seed*, and *typed-edge precedent* half-built (generalise the
exotype + the prover); the *scheduler-over-structure*, *toposort*, and *navigation* greenfield. This
section is the design proper; the conceptual subsections that follow (the lambda/scope rationale, the
duality, the ensemble VERIFY-spike) are its elaboration. Exit target: implementable from here alone.

### Entity types
- **Capability** (the minted node): `{:cap/id :cap/title :cap/status(:held|:satisfied — prover
  lifecycle) :cap/altitude(ascent ordering, prover) :cap/minted-by[mission-ids]
  :cap/grounding(substrate-2 commit-vertices — DEFERRED, the mission→code bonus)}`. **Seeded from
  `pudding-prover-registry.edn` (44 proof-states + 7 kit)**, extended as missions mint.
- **Mission** (the lambda / dual node): the **generalised exotype** — `{:mission/id
  :mission/scope(input capabilities = exotype `:ports/:input`) :mission/body(open holes)
  :mission/produces(output capabilities = `:ports/:output`) :mission/phase :mission/status}`. Fed by the
  watcher's parsed `:mission/*` props + the exotype `.edn` when one exists.
- **Hole** (body-element, PLUGGABLE): `{:hole/id :hole/source(:hole-counter|:sorry-miner)
  :hole/single-cycle?}`. **Fixed source = the live `:open-hole-count`; the turn→sorry miner is a *second*
  source (M-a-sorry-enterprise, future) behind the same interface.**
- Real object = the **bipartite incidence** Mission ⊔ Capability; missions-as-nodes /
  capabilities-as-nodes are its two projections.

### Relation types (typed edges — seeded, not invented)
| Edge | From → To | Meaning | Seed (MAP) |
|---|---|---|---|
| `:requires` | mission/cap → cap | hard prerequisite (scope binding); **logical** | mission `:blocked-by`, typed up |
| `:produces` | mission → cap | the mint (output wire) | closure `:enables [{:action :capability}]`; criteria |
| `:enables` | cap → mission/cap | positive lookahead | closure `:enables` (live) |
| `:specialises` | cap → cap | refinement | prover `:specialises` |
| `:couples` | cap ↔ cap | mutual | prover `:couples` |
| `:built-before` | mission → mission | **temporal** order (≠ `:requires`) | piano_roll / `:mission/date` |

### Invariant rules (the VERIFY targets)
- **INV-1 (acyclic):** the `:requires`/`:produces` graph is a DAG ⇒ a **toposort exists** (the
  build-order). *(None exists today — MAP Q5; the new build.)*
- **INV-2 (mint-provenance / no perfect-crime):** every `:satisfied` capability has a `:minted-by`
  mission that is `:complete` — no capability "satisfied" without a shipped producing mission (the
  M-the-perfect-crime check at the capability level; the mission→code grounding strengthens it).
- **INV-3 (applicability):** a mission is *applicable* iff every `:requires` capability is `:satisfied`.
- **INV-4 (single-cycle leaf):** a mission is a *leaf* iff applicable ∧ its body is one-cycle
  (`:hole/single-cycle?`). The granularity tag (fixed hole-counter; keystone refinement later).
- **INV-G — THE BUCK STOPS HERE (load-bearing safety invariant):** the map is *generative* — it can
  inductively surface a **missing** capability ("have X,Y; Z is reachable and serves the ascent; Z is
  unminted"). Generativity is gated **at the PURSUIT boundary, not the DISCOVERY boundary:**
  - **Discovery** (propose a missing capability) is autonomous — graph inference + a NAG.
  - **Pursuit/minting** of a capability **not in the operator's pre-registered brief** is
    operator-gated (consent) — the system never *decides* to pursue a new capability overnight.
  - **The EFE goal is the operator's pre-registered ascent** (prover altitude toward the pre-registered
    targets), **NOT capability-maximisation.** ⇒ the scheduler cannot *want* an un-registered capability.
  - *(WM-I4 / the consent-gate at the capability level. The 3AM-pentagon case: surfacing "we lack Z" is a
    harmless inference; pursuing Z is impossible without pre-registration + consent, and the EFE never
    seeks it.)*

### Data flow
watcher `:mission/*` + exotype `.edn` → **Mission nodes**; `pudding-prover-registry.edn` → **Capability
nodes**; `:blocked-by` + closures + criteria → **typed edges**; hole-counter → **body/leaf tag** → the
**bipartite graph** (futon1a hypergraph or an EDN sibling of `.semilattice.edn`) → **toposort** →
**EFE-over-graph** (extend `futon2.aif.forward-model`) → the **WM scheduler's pick**. The operator's
**pre-registered brief** (M-futon-forward-model §13) feeds the **EFE goal + the INV-G consent-gate**.

### The wiring diagram = the mission's OWN exotype (dogfood the generalised form)
Author `futon5/data/missions/M-capability-star-map-exotype.edn` — ports (in: mission-docs/prover/
closures/hole-counts/pre-registered-brief; out: the graph + the scheduler-pick + the NAG proposals),
components (extractor / graph-store / toposort / EFE-scheduler / **consent-gate**), edges (the data flow),
invariants (INV-1..G). Building the mission *as* an exotype validates the generalised node-form.

### EFE-over-the-graph (the substantive new build)
Extend `efe.clj`/`forward-model` so a mission-action's G-total is a functional of the graph:
**pragmatic** = ascent-progress toward the pre-registered goal − body-size; **epistemic** = structure
info-gain; **applicability gate** = unbound `:requires` ⇒ high G. The toposort bounds candidates to the
*applicable frontier*; EFE picks the leaf within it ⇒ the EFE-min pick **is** an applicable single-cycle
leaf on the ascent (the cycle-1 gate, bounded by INV-G).

### Key decisions (IF / HOWEVER / THEN / BECAUSE)
- **IF** capabilities are the nodes (Khan-faithful), **HOWEVER** missions-as-nodes extracts more easily
  (the watcher already emits mission attributes), **THEN** store the **bipartite incidence** and project
  both, starting mission-side, **BECAUSE** node/edge duality makes it one structure, not a choice.
- **IF** the scheduler is EFE, **HOWEVER** a free EFE could "seek capability," **THEN** bind the goal to
  the pre-registered ascent + gate pursuit (INV-G), **BECAUSE** the buck stops here — autonomy advances
  *pre-registered* targets, never *chooses new ones*.
- **IF** holes come from the counter now, **HOWEVER** new-hole discovery is M-a-sorry-enterprise (future,
  data-blocked), **THEN** make `hole-source` pluggable, **BECAUSE** the map must upgrade
  curriculum→living without a rebuild (MAP §Limitation) — *and* the map gives the future miner both a
  socket to plug into and a structure to do inductive capability-discovery on.

---

### Conceptual foundations (the lambda/scope rationale — elaborates the design above)

> These subsections were authored during the 2026-06-07 concept session (ahead of phase) and are the
> *rationale* for the formal DERIVE above; the "Ensemble 1" section is the VERIFY-spike that validated it.

### The missions-as-lambdas / scopes model (firmed up)

A mission is a **lambda**: `M = λ(scope).body ⇒ capability`.

- **scope** (the free variables) = the capabilities M depends on — its prerequisite missions /
  capabilities (the INPUT edges). Source: the existing `:depends-on` / `:enables` prose annotations,
  extracted + typed.
- **body** = the work M does — its open holes. Body *size* = `:open-hole-count` (live, futon2
  mission-registry).
- **capability** (the result) = what M produces once fully applied — what other missions then bind to
  (the OUTPUT edges).
- **application (β-reduction)** = advancing M = discharging one hole = *partial application*. A
  **fully-applied** lambda (all holes discharged) is no longer a lambda but a **value** — a built
  capability, a node in the stereolithographic posterior.

This makes "small vs big target" **structural**, not a heuristic:
- a **single-cycle leaf** = an **applicable** lambda (scope variables all bound — prerequisites built)
  with a **small body** (one/few holes);
- a **mega-mission** = a lambda with a **large body** and/or **unbound scope** (prerequisites missing).
  Both legitimate; both *visible* in the graph.

### Granularity layering (the keystone boundary — explicit, no fork)

- Lambda **body-size is measured at the MISSION level** by `:open-hole-count` (live). This is **NOT**
  the code-level `:scope` grain held on `M-substrate-metric`; the star-map *consumes* the live
  hole-count and does **not** fork the keystone.
- The richer body measure — the code-`:scope`s a mission would produce (M-differentiable-code's
  `:scope` grain, probe-supported 0.974) — is the **keystone refinement** that plugs in when
  M-substrate-metric lands. **mission-scopes ⟷ code-scopes** is the eventual unification; build the
  star-map to *slot it in*, not to pre-empt it.

### EFE as Active Inference over the star-map (the success criterion, made precise)

Today's EFE (`futon2.aif.forward-model`) ranks **action-classes** and treats `:open-mission` targets as
a **flat set** — which is why cycle 1 surfaced a mega-mission. The build makes EFE a **functional of the
lambda structure**:
- **pragmatic value** ← progress toward the goal (valuable-path / the capability the operator wants),
  discounted by **body-size** (a large body = many cycles = high expected free energy *now*);
- **epistemic value** ← information gained about the structure (advancing an applicable leaf reduces
  uncertainty cleanly; poking an unbound-scope mega-mission does not);
- **applicability gate** ← an un-applicable lambda (unbound scope) carries high EFE — the **dependency
  structure does the gating**.

When EFE is computed over the lambda graph this way, the EFE-minimising pick **is** an applicable
single-cycle leaf on the valuable-path — *cherry-picking becomes a graph query.* (This is
M-differentiable-code's "free-energy / EIG over the structure" objective + M-bayesian-structure-learning,
at the mission level.)

### The recursion is the granularity (sharpening, 2026-06-07)

A mission's body = its holes; **each hole is itself a (sub-)mission-as-lambda.** So the lambda is
recursive — `M = λ(scope).{hole₁ … holeₙ}` where each `holeᵢ` is a λ. Granularity is then **recursion
structure**, not a bolted-on axis: a **leaf** = a lambda whose body is one irreducible reduction step
(no sub-lambdas); a **mega-mission** = a lambda whose body is a deep tree of sub-lambdas. This unifies
the Khan star-map (small skills compose up), Rob's proof-decomposition (lemmas within lemmas), and the
raw hole-count (the body's breadth) — and it sets the extraction strategy: **recursively decompose
mega-mission bodies into sub-lambdas until you hit leaves** (uniformity *by extraction*, per
M-differentiable-code). The star-map is thus a **nested / operadic wiring diagram** — boxes within
boxes (a mission as a wiring-diagram box: input wires = scope, output wire = capability, interior =
the un-reduced body).

### Deferred bonus — mission→code (substrate-2) grounding (Joe, 2026-06-07)

Added at the **end**, not now (it must not block the conceptual build). A fully-applied mission's
**output wire** (the capability it produces) gets **grounded in the substrate-2 posterior** — the real
commits/code that realize it (the M-stack-stereolithography commit-DAG; `:authored`/`:edits` edges).
Two payoffs:
1. **Attribution** — *which real mission shipped which feature* (Joe's "very useful going forward").
2. **Falsification** — a mission whose output wire claims capability X but has **no code grounding** is
   a chained-claim crime (M-the-perfect-crime, lifted to the capability level). The grounding turns the
   prior wiring diagram into a **checkable** prior — the same prior/posterior discipline, now at the wire.

Feasibility note (honest, not "easy" on faith): the association is mission-commits/files → substrate-2
vertices, and it holds **iff** the substrate-2 commit-vertex layer (M-live-geometric-stack phase 3,
what M-stack-stereolithography consumes) is live.

### Node/edge duality — the bipartite incidence is the real object (Joe, 2026-06-07)

Don't choose. The real object is the **bipartite incidence** `missions ⊔ capabilities`, edge = "mission
M produces / consumes capability C". The two readings are its one-mode **projections** (graph-theoretic
node/edge duality, almost-for-free):
- **missions-as-nodes** (M1~M2 share a capability) — *start here*; it matches the lambda model exactly
  (a mission's scope = the capabilities it consumes, its output = the capability it produces — the
  **lambda IS a mission's bipartite-local structure**).
- **capabilities-as-nodes** (C1~C2 share a mission) — the **dual view**, a re-projection away.

Keep the capability incidence even while working the mission-view, so the dual is a reformatting, not a
rebuild.

### Capabilities are MINTED — the pudding-prover IS the capability registry

Answers the open "named vs discovered" question: **neither — capabilities are *minted*** (the
C-pudding-prover lifecycle held → contract-released → satisfied = hole → proof-with-holes → built). So
the star-map's **capability-nodes ARE the pudding-prover's theses / kit-capabilities**
(`futon7/holes/pudding-prover-registry.edn`). M-capability-star-map (capability *structure* — which
missions mint which, the deps) and C-pudding-prover (capability *minting-status* — held / satisfied /
cadence) are the **structure-view and status-view of one capability set.** We *read* the vocabulary
from the prover and extend it as missions mint new capabilities — we do not invent it.

### The duality is functional for EFE (not just representational)

The capability-view is where **goals** live (a target capability to mint); the mission-view is where
**actions** live (which mission/lambda to apply). EFE bridges them: **select the mission-action that
minimises expected free energy toward the target capability.** Generative model = the bipartite graph;
goal = a capability node; policy = a mission edge.

### Worked anchor — "WM overnight unsupervised" as a capability node (Joe's example)

A first-class capability (≈ pudding-prover **T4.2** / M-the-futon-stack's *"I trust the stack to code
while I sleep"*). Its **producing subgraph** (in-edges, capability-view) is *this session's work*: the
guardrails core + hole-counter + gate-runner (steps-forward-not-wedges + safe), and — the last edge —
**this mission** (EFE-over-star-map = the pick the pilot can trust). The star-map is again
self-referential: **building it mints the very capability that motivated it.**

### Ensemble 1 — the keystone, demonstrated (2026-06-07)

First hand-authored slice (no extractor yet): `M-capability-star-map.ensemble.edn` — **8 capability
nodes, 6 attested/satisfied** (the T1-to-T2 tech layer, backfilled from the piano_roll earliest
missions + VSATARCS + Agency) and **2 held**. The chain:

`agency` + `evidence-persistence` → `self-representing-stack` → `live-geometric-stack` → `war-machine`
→ `wm-steps-forward-guardrailed` → **[`efe-trustworthy-over-starmap` — HELD, this mission]** →
`wm-overnight-unsupervised` (frontier ≈ T4.2).

**Keystone finding (compositional, not asserted):** every node in the frontier's transitive scope is
`:satisfied`/attested *except* `:efe-trustworthy-over-starmap`, which **this mission mints**. So
M-capability-star-map is the single *substantive* held node gating "WM overnight unsupervised" — the
keystone. (Honesty: minor mechanical prereqs remain outside the slice — worktree-staging, the
full-frame gate on the WM's own pick — operational, not the substantive gate.)

**Model stress-test: PASSED.** The lambda / bipartite-incidence / minted-capability frame expressed a
real attested ensemble + the keystone cleanly — the concept is ripe to *extract*, not only grapple.
(Agency is the attestation Joe flagged: earliest cohort 2026-02-07, born *with* the mission format —
not before it — daily-driver, `futon3c/src/futon3c/agency/`.)

### End-to-end build plan (the cars; bounds the first slice per the scope tension)

1. **Model** — the lambda/scope node+edge schema (above), + the edge extractor (`:depends-on` /
   `:enables` → typed edges). *(claude-1 design; codex impl.)*
2. **Graph** — extract the mission landscape into the star-map (futon1a hypergraph or an EDN sibling of
   `.semilattice.edn`); exclude worktrees/origin/`.state`. Bound the **first slice to ONE region**
   end-to-end before the whole ~125.
3. **EFE-AIF** — extend `forward-model` so EFE is the structural functional above; the WM ranks lambdas,
   not a flat open-mission set.
4. **End-to-end test (the gate):** a dry WM selection over the star-map returns an **applicable
   single-cycle leaf** as EFE-top — *no pilot fork-resolution needed.* Only then do we run the next WM
   cycle.

---

## ARGUE (2026-06-07)

### Pattern cross-reference (`futon3/library/` — all citations verified to exist)

The design is **not a novel mechanism** — it is a structural consequence of patterns already in the
library. Strongest support, by design element:

| Design element | Pattern | How it supports |
|---|---|---|
| **Gradient-intrinsic safety** (the buck stops here) | **`futon-theory/structural-tension-as-observation`** | Two-loop AIF (fast task / glacial library) with **I4 EXOGENEITY**: fast tensions CANNOT force a library rewrite without the deliberate NAMING→SELECTION→CANALISATION threshold. *This is "water doesn't flow uphill / ants don't rewrite NATO" made structural* — the Markov-blanket closure is the gate pipeline, not a rule. |
| **Off-goal = not pursuable** | **`futon-theory/task-as-arrow`** (BHK) | Tasks are constructive arrows A→B that **compose**; an off-goal capability is an arrow that **doesn't compose into the ascent** — *not forbidden, not wireable*. EFE slopes toward composable proof-chains. |
| **Node = exotype; toposort is structural** | **`futon-theory/theory-as-exotype`** + `four-types` | Theory(exotype)→patterns(genotype)→code(phenotype); a capability ships only if it type-checks against the exotype ⇒ build-order is a **structural type constraint**, not metadata. Confirms node = generalised exotype. |
| **INV-2 mint-provenance** | **`futon-theory/retroactive-canonicalization`** + `agent/evidence-over-assertion` + `coordination/mandatory-{psr,pur}` | Every capability claim has an auditable **genealogy** (incident→pattern→canonisation→spec); no `:satisfied` without a shipped producing mission — the perfect-crime check, already a library discipline. |
| **EFE-over-graph (scheduler)** | `aif/expected-free-energy-scorecard` + `aif/candidate-pattern-action-space` | G = named weighted terms (risk/ambiguity/info/cost), selected from a **bounded candidate set** via retrieval + gating — the EFE-over-applicable-frontier. |
| **INV-G discovery vs pursuit** | `aif/niche-construction` + `musn/expensive-move-consent` + `agent/scope-before-action` + `budgeted-action-selection/mana-gated-work` | False-floor ⇒ missing-edge *discovery* (autonomous); niche-construction + expensive/human-contact moves require **consent** before *pursuit*. |
| **Bipartite / node-edge duality** | `f6/negative-space-duality` + `gauntlet/world-is-hypergraph` | Node + edge are complementary masks of one structure; the typed hypergraph IS the world. |
| **Navigation** | `system-coherence/present-graph-topology-not-adjacency-lists` | When topology is load-bearing, exhibit the **edges**, not per-node adjacency — the star-map's point. |
| **DAG / INV-1** | `futon-theory/mission-dependency` | Missions form a DAG (blocks/enables, no cycles, blocked cites blocker) — the toposort precedent. |
| **The decompose move** | `math-informal/split-into-cases` (Rob's proof-decomposition) + `eight-gates/split-isolate` | Break a mega-mission into sub-steps as a *leaf-sized* action; the first outing already counts decompose as **progress, not stuck** (M-war-machine-first-outing §8.8). |
| **VERIFY discipline** | `mission-coherence/logic-model-before-code` | Invariants as `core.logic`+pldb over an abstract trace **before** code — how INV-1..G should be checked next. |
| **Pluggable hole-source** | `hdm/deep-storage-to-active-graph` + `aif/candidate-pattern-action-space` | Ingestion is a pluggable pipeline; action-space candidates come from a swappable retrieval fn. |

### Theoretical coherence — inevitable, not merely workable

IDENTIFY framed safety as a *gate* (INV-G). The cross-reference shows the **stronger** truth Joe named:
it is the **I4 exogeneity** of the two-loop AIF (`structural-tension-as-observation`). The fast loop (EFE
selecting *which capability to advance*) and the glacial loop (re-authoring the *inventory*) share a
Markov blanket — the gate pipeline — and I4 forbids the fast loop from forcing the glacial. So **the EFE
cannot flow "uphill" to an un-registered capability because the structure has no edge there**; off-goal
capabilities are BHK arrows that don't compose into the ascent (`task-as-arrow`). The consent-gate is the
belt; **gradient-intrinsic exogeneity is the trousers** — and it was already a structural law. The design
does not *add* safety; it *inherits* it.

### The decompose move — how big things progress, safely (Joe 2026-06-07)

A mega-mission has no single-cycle leaf — so the legal move is neither "do the big thing" nor "wedge"
(cycle 1's failure). It is to take the **small step of breaking it down**: reveal one layer of sub-steps.
**Decompose is itself a leaf-sized action** — the recursion-is-granularity made operational (author one
layer of sub-lambdas) — and the first outing already counts it as *progress*, not stuck
(M-war-machine-first-outing §8.8: "deep-but-decomposable work never counts as stuck"). Patterns:
`math-informal/split-into-cases` (Rob's proof-decomposition — the analogy that seeded this mission) and
`eight-gates/split-isolate`.

**The safety line — "assuming the breakdown is itself a small incremental step" (Joe):** a decompose
**refines the PATH to a pre-registered goal** (changes *how*, not *what*) — it does **not** extend the
goal-set, so it stays downhill and within INV-G. This *sharpens* the first outing's blunt escalation of
∇-deform (§8.13): a **small** decompose (the next layer of sub-steps of an already-registered goal) is a
legal autonomous leaf; a **big** ∇-deform (a new typed-edge *family* / niche-construction / wholesale
re-architecting) *changes what is pursued* → glacial / consent (INV-G). The line is exactly Joe's caveat —
**is the breakdown itself a small incremental step?** Decompose is how water finds its way down a cliff:
by many small steps, never flowing up.

### "Each mission step is small" — and the lifecycle's operator-verify exits ARE the gates (Joe 2026-06-07)

A further claim, *to be borne out as we go*: **each mission-lifecycle step is small** — a phase advance
or a hole-discharge is bounded, a leaf. This is *why* the single-cycle-leaf model fits — the futonic
lifecycle already chunks a mission into small steps (and the big ones decompose, above).

But not every identified step should be *pushed*. Joe's sharp example: an **IDENTIFY-only mission whose
gap hasn't been agreed** — advancing it is still "incremental work of some kind," yet we may not want
the WM to push it, because the work is misdirected until the gap is real. The resolution is **already in
the lifecycle**: its exit criteria *include operator-AGREEMENT steps* — HEAD-verify, IDENTIFY's "*a
human agrees the gap is real and the scope is right*", the consent calls. **Those exits ARE the INV-G
gates** — INV-G is not bolted on, it is the lifecycle's own operator-verify exits, respected. The WM
advances the **automatable within-phase work** and **NAGs the operator-agreement exits** (surfaces "this
needs your IDENTIFY-verify"; never pushes past an un-agreed gap).

And the gradient already wants this: an un-agreed-gap mission carries **high ambiguity** (is the work
even right?) → high `G-ambiguity` → the EFE *de-prioritises* it (not downhill), and the
operator-agreement NAG is what resolves the ambiguity. So "don't push un-agreed gaps" is **not a special
rule** — it falls out of EFE + the lifecycle's existing exits.

**The optimism (Joe, to be borne out):** *almost everything other than the operator-agreement exits is
not only incremental but automatable.* If so, the WM advances the large majority of the landscape
autonomously, with the operator-agreement exits as the narrow earned NAGs — exactly the "trust EFE, no
cherry-pick" criterion, now with a clean account of *what stays the operator's* (and a direct line to
M-the-futon-stack's "code while I sleep").

### Trade-offs
- **Fixed curriculum first** (MAP §Limitation): give up living hole-discovery now for a tractable, faithful
  v1 — the hole-source plugs in later.
- **Consume the granularity keystone** (M-substrate-metric): a hole-counter proxy for the single-cycle-leaf
  tag until the keystone lands.
- **Generalise the exotype** rather than design a fresh node-form: less novelty, but it dogfoods + reuses
  `futon5.ct.mission/validate`.

### Generalisation
The construction (bipartite incidence + EFE-over-graph + I4-exogenous goal-binding) is the **mission-level
instance** of M-differentiable-code's code-level graph — the same shape at any granularity, which is why
the node is a *generalised* exotype (`theory-as-exotype` is recursive).

### Plain-language argument (no jargon)
We are drawing a map of what the system can do and what it would take to do more — like a tech-tree or a
Khan-Academy skill map. It shows both the big unbuilt things and the small next steps, so the overnight
builder can pick a *small, ready* next step on its own. It is safe for a simple reason: the builder only
ever rolls **downhill** toward goals you wrote down in advance. It can *notice* it lacks some ability, but
it can't *decide on its own* to go chase one — especially a dangerous one — because there is no downhill
path to anything you didn't pre-register. And when a pre-registered goal is too big for one step, it's
allowed to take the small step of *breaking it into pieces* — that's still downhill. Water doesn't flow
uphill; it just finds its way down a step at a time.

**ARGUE exit:** the design feels inevitable given the constraints (it inherits the stack's I4 exogeneity +
BHK composition); the plain-language argument stands alone. Next: VERIFY — INV-1..G as a `core.logic`
logic-model before code (`mission-coherence/logic-model-before-code`), with INV-G machine-checked.

---

## VERIFY (2026-06-07)

### Structural verification — invariants as a logic-model BEFORE code (`mission-coherence/logic-model-before-code`)

`futon3c/src/futon3c/logic/capability_star_map_invariants.clj` — a `core.logic` + pldb model over an
abstract WM-action trace. `(run-verify)` against the live JVM returns **`:verified? true`**:

- **Witness clean** (0 violations across all 6 categories): every move in its legal form — acyclic
  `:requires` edges; an applicable advance; an exit-crossing advance *with the gap agreed*; a
  pre-registered pursuit; an un-registered pursuit *with consent*; a small (path-refining) decompose; a
  big decompose *with consent*; a satisfied cap with a complete producer.
- **Each adversarial trace caught by its own category** (one per invariant):

| Invariant | Adversarial planted | Caught? |
|---|---|---|
| `:acyclic` (INV-1) | a 2-cycle `a→b→a` | ✓ (no toposort) |
| `:provenance` (INV-2) | `:satisfied` cap, producer not `:complete` | ✓ |
| `:applicability` (INV-3) | advance an inapplicable mission | ✓ |
| `:buck` (INV-G) | **pursue `:cap/pentagon` — un-registered, no consent** | ✓ |
| `:decompose` | a goal-extending decompose without consent | ✓ |
| `:gate` | advance past an un-agreed operator-verify exit | ✓ |

The safety guarantee Joe named is now **machine-refuted, not merely argued**: a `:pursue` of a
non-pre-registered capability with no consent is *caught* — the 3AM-pentagon case cannot pass.

### Completion-criteria pre-check (against IDENTIFY)
- **C1** (structured form) — DERIVE node = generalised exotype; modelled. Impl = INSTANTIATE.
- **C2** (capability inventory + typed-edge graph, one region) — design covers it; ensemble-1 spike
  pre-witnessed it. Impl = INSTANTIATE.
- **C3** (EFE returns an applicable single-cycle leaf, no cherry-pick) — INV-3 + INV-1 + the
  EFE-over-graph design; the *gate* is structurally checked. The live EFE-over-graph is the INSTANTIATE
  build; **INV-4 (single-cycle-leaf) is the spike that confirms it empirically** (deferred — granularity
  predicate over the live hole-counter, not a pure trace relation; consistent with the keystone boundary).
- **C4** (keystone path reproduced by the *extracted* graph) — INSTANTIATE (the extractor).

### Decision log
- Model is OFFLINE-ONLY for VERIFY; may later register as a live probe family (like
  `:sorry-closures-stick`) during INSTANTIATE.
- clj-kondo reports ~17 "unresolved symbol" errors — all `pldb/db-rel` relations + `l/run*`/`l/fresh`
  binding symbols (core.logic macros kondo can't expand); the proven template `aif2_invariants.clj` has
  the identical profile (19). The model loads + runs + verifies — dispositive. Not a real defect.
- DERIVE revisions required by VERIFY: **none**.

### PUR — `mission-coherence/logic-model-before-code`
- **Pattern:** logic-model-before-code (verify a design as `core.logic`+pldb over an abstract trace).
- **Actions:** encoded INV-1/2/3/G + decompose-line + operator-verify-gate as 6 violation-queries;
  conforming witness + 6 adversarial traces; `run-verify`.
- **Outcome:** success — `:verified? true` (witness clean; all 6 adversarial caught by category).
- **Prediction error:** low — the template transferred cleanly; one adaptation (acyclicity as a plain
  fixpoint transitive-closure rather than a recursive core.logic `reachableo`, to guarantee termination
  on a cyclic adversarial) was a clean call.
- **Notes:** INV-G's adversarial (the pentagon pursuit) is the headline — the safety claim is now
  checkable, not only argued-inevitable.

**VERIFY exit:** the design is checked against its structural constraints; all six invariants verified;
the one risk that can't be settled statically (does EFE-over-graph actually surface single-cycle leaves
in practice — INV-4 / C3) is named for the INSTANTIATE spike. Next: INSTANTIATE (the extractor + the
graph + the EFE-over-graph), bounded to the first-slice region.

---

## INSTANTIATE (2026-06-07 — first slice: the WM-region; coding via codex handoffs)

Bounded to ONE first-slice region — the **WM-region** (the ensemble-1 chain `agency → self-rep →
live-geometric-stack → war-machine → wm-steps-forward-guardrailed → efe-trustworthy → wm-overnight`), so
the *extracted* graph must reproduce the hand-authored ensemble (completion-criterion C4). Two codex
handoffs (Unit A / Unit B); claude-1 reviews each (read diff · re-run gates · `run-verify` on the real graph).

### The bipartite-graph schema (the seam between the two units)
A single EDN sibling of `.semilattice.edn` — `M-capability-star-map.graph.edn`:
```clojure
{:star-map/region :wm
 :capabilities {<cap-id> {:title _ :status (:held|:satisfied) :scope [<cap-id>…]   ; :scope = :requires
                          :minted-by [<mission-id>…] :pre-registered? bool
                          :pre-witness [{:id _ :doing _ :attests <cap-id>
                                         :witness-kind _ :status :candidate
                                         ;; optional, witness-kind dependent:
                                         :success-criteria [_] :requires [_] :hard-gates [_]}]}}
 :missions     {<mission-id> {:scope [<cap-id>…] :produces [<cap-id>…]
                              :open-hole-count n :phase _ :status _ :next-exit-operator-verify? bool}}
 :edges        [{:from _ :to _ :type (:requires|:produces|:enables|:specialises|:couples|:built-before)}…]}
```
Extends `M-capability-star-map.ensemble.edn` (the hand-authored target + test fixture).

### Unit A (codex-1) — extractor + toposort + real-graph invariant check
- Extract the WM-region missions (watcher `:mission/*` via `/api/alpha/missions` or `futon2.aif.mission-registry`)
  + capabilities (`pudding-prover-registry.edn`) + edges (`:blocked-by` typed-up to `:requires`; closure
  `:enables`; prover `:parent/:specialises/:couples`). **Exclude worktrees / origin / `.state`** (the
  contaminated-prior hazard — see Exclusion discipline + `2cd7445`).
- A `toposort` over `:requires` (INV-1 — error on cycle).
- Run the `futon3c.logic.capability-star-map-invariants` queries on the REAL extracted graph (write a
  graph→trace adapter): **acceptance = 0 violations** on the conforming real graph + the keystone path
  reproduces ensemble-1 (C2, C4).
- Out: the extractor ns + `M-capability-star-map.graph.edn` + the graph→invariant adapter.

### Unit B (codex-2) — EFE-over-graph + INV-G goal-binding
- Extend `futon2.aif.forward-model` / `efe.clj` so a mission-action's G-total is a **functional of the
  graph**: applicability-gate (unbound `:requires` ⇒ high G), body-size penalty (`:open-hole-count`),
  ascent-progress toward the **pre-registered goal** (the goal is an INPUT, never chosen).
- **INV-G:** the EFE goal is bound to the pre-registered ascent — the selector **cannot** return a
  `:pursue` of an un-registered capability; a goal-extending decompose needs consent.
- Acceptance: over the WM-region graph the EFE-top pick is an **applicable single-cycle leaf** (C3 — no
  cherry-pick); `q-buck`/`q-gate` on the real selection trace = 0 violations. Test against
  `ensemble-1.edn` as the fixture until Unit A's real graph lands.
- Out: the EFE-over-graph extension + tests.

### Gates (both)
clj-kondo clean; check-parens clean; tests pass; **`run-verify` on the real extracted graph ⇒ 0
violations** (the design invariants hold on the data, not just the abstract trace). Reload via Drawbridge;
NEVER restart the JVM; no blocking `tick!`. Bell `claude-1` back with summary + commit sha(s) + gate results.

### Deferred (named, not this slice)
mission→code grounding bonus · the navigation render (M-stack-stereolithography owns it) · the **INV-4
single-cycle-leaf empirical spike** (does EFE pick a leaf in *practice* — the C3 confirmation, run after
Unit B) · living hole-source (M-a-sorry-enterprise) · futon1a graph-store (EDN suffices for slice 1).

### Unit A review (claude-1, 2026-06-07) — pipeline ✓, extraction HOLLOW (we-do-discipline catch)

codex-1 `b2cf27e`/`c300403`. Gates re-confirmed (kondo 0/0; 3 tests; `run-verify-equivalent :verified?
true`; keystone = single held node `:efe-trustworthy-over-starmap`). **The PIPELINE is real + valuable**
— the schema, `requires-toposort` (8 caps, correct order), `graph->trace` adapter, the on-real-graph
invariant run, and `keystone-path-report` all work; this is the substrate Unit B runs EFE over.

**But the EXTRACTION is hollow, and the C4 claim is circular** (honest re-characterisation):
- `capability-nodes` reads the capabilities + their `:scope` (= the `:requires` edges) + `:minted-by`
  **straight from `ensemble.edn`** — the hand-authored target. So "the extracted graph reproduces
  ensemble-1" is true **because it copied it** (C4 is vacuous, not an independent validation).
- The `:minted-by` mission ids are **invented aliases** (`M-wm-guardrails-core`, `M-war-machine-hole-
  counter`, … — no such mission docs); **12 of 14 missions have `:open-hole-count 0`** (not found in the
  registry). The "enriches from the live mission registry" claim is mostly empty — only ~2 real
  hole-counts + 2 pudding `:specialises/:couples` edges are genuinely extracted.

**Disposition:** the capability-inventory genuinely *can't* be independently extracted yet (the A2 gap —
no inventory exists beyond the prover theses + this hand-authored ensemble), so seeding the capabilities
from the ensemble is **forced**, not a shortcut — accept it as a `:prototyping-forward` PIPELINE-first
slice. BUT two things are honest defects, not forced: the **fictional mission aliases** (should map to
real mission docs or be labelled placeholders) and the **over-stated "extraction/real-graph" framing**.
C2 is met only weakly (a graph exists for the region, hand-authored); **C4 is not genuinely met**.
Genuine landscape extraction = future (capability inventory A2-blocked; mission-mapping a targeted
follow-on).

**Operator disposition (Joe, 2026-06-07): accept A.** The *mechanism* evidence is good (Unit B); **real
content is required to close**, and we **integrate only when the real data is assembled** — NOT on the
hand-authored ensemble. The A+B integration (the C3 live acceptance) is **deferred** until a real
capability landscape exists.

### INSTANTIATE checkpoint (2026-06-07) — mechanism proven, real content pending
- **Unit B (EFE-over-graph + INV-G), `futon2 35ce7f6`:** ✅ verified + accepted. The mechanism + the
  safety gate are real — applicability-gate / body-size / ascent-credit; un-registered `:pursue` +
  goal-extending decompose + unagreed-exit advance all **refused before ranking** (`action-admissible?`
  → `rank-star-map-actions`); 23 tests; `selection-trace-step` wires a real selection back to the
  logic-model. "Water doesn't flow uphill" is enforced in code, not just argued.
- **Unit A (pipeline), `futon3c b2cf27e` / `futon0 c300403`:** ✅ accepted as the substrate; ⚠️
  extraction hollow (hand-authored ensemble + thin enrichment; C4 circular). Kept `:prototyping-forward`.
- **Integration: DEFERRED** (operator) — only on real data.
- **Mission state: OPEN** at *mechanism-proven, real-content-pending.* **NOT WM-overnight-ready** — that
  gate needs EFE picking leaves on the REAL landscape with no cherry-pick.
- **To close:** assemble the real capability landscape —
  - (1) real **mission-mapping** — ✅ **DONE + reviewed** (codex-1 `9bf7f02`/`e6d4eda`, claude-1
    verified 2026-06-07): the mission half is now real — **10 registry-verified mission nodes + 4 honest
    `builder/wm-*` placeholders** (`:real-mission? false :built-under WM-GUARDRAILS-SPEC`), **0 fictional
    aliases**; capabilities + `:requires` untouched; `run-verify-equivalent :verified? true`,
    keystone single-held. The mission half of the hollow-extraction finding is closed.
  - (2) the **capability inventory** (A2 — the harder, gating piece). **Disposition (Joe, 2026-06-07):
    NOT a new mission — it is a CROSS-MISSION concern, kept inside THIS mission, grounded in
    `C-pudding-prover §8.2`**: the prover registry = the capability set's STATUS view, this mission = the
    STRUCTURE view, and the campaign's Daumal ascent (§10) = the pre-registered EFE goal. New capabilities
    are *minted* via the prover lifecycle and coordinated as registry entries there — not invented
    per-mission. Cross-mission notes go in C-pudding-prover as needed.
    - **Approach — region-by-region grounding plan (Joe + claude-1, 2026-06-07; recorded in
      `C-pudding-prover §8.2`):** the inventory is **authored, not extracted** (criteria are
      artifact-shaped; "what M mints / requires" is judgment) — so the danger is a high-res capability
      map that *substitutes* for reality (Baudrillard / the perfect-crime at the capability level). The
      discipline IS the task: **ground, don't invent** (INV-2 — every capability node → a real shipped
      producing mission, + the mission→code commits where pulled; *no capability without a shipped
      producer*); **valuable-path first, region-by-region** (the ascent §10 + the semilattice ~25, not all
      ~125 — tail stays lazy); **WM-region first** (its missions are now real) → ground + register here →
      integrate + run C3 on that region → expand outward, growing the registry. Whole-landscape inventory
      + living discovery (M-a-sorry-enterprise) is the longer horizon.
  - Then integrate (A+B) and run the C3 acceptance **on real data** (deferred until then, per operator).

### Checkpoint — 2026-06-07 (Task 2: WM-region capability axis GROUNDED; A+B/C3 dispatched)
**What was done:**
- **Definitive Task-2 checklist** stood up in `C-pudding-prover §8.2.1` (single source of truth; this doc
  points there). WM-region capability grounding ledger: capability → minting mission(s) → grounding
  artifact → shipping commit(s) → status, two backing axes (capability = claude-1, VSATARCS doc-ref =
  claude-3, paired).
- **Capability axis: 6/6 ☑ grounded** — row 1 `:agency` (claude-1: `cc7ac36`/`0b5f262`); rows 2–4
  `:evidence-persistence`/`:self-representing-stack`/`:live-geometric-stack` (codex-1 audit, all real
  missions + artifacts + shipping commits); row 5 `:war-machine` (claude-1 adj — ≥1 shipped producer:
  first-outing CLOSED `24df6fc`; pilot open = refinement); row 6 `:wm-steps-forward-guardrailed`
  (verified-via-spec). codex-1's audit was honest — it flagged rows 5/6 rather than forcing green checks.
- **Producer rule (claude-1, Joe-delegated):** a producer is a real `:complete` mission OR a spec-governed
  builder with verified + live-verified commits; **≥1 suffices** (a second open producer is refinement,
  not a gap). Resolves rows 5 (war-machine) + 6 (WM-GUARDRAILS-SPEC builders).
- **Pairing with claude-3** (VSATARCS doc-axis): one ledger, two columns; doc-integrity findings in
  sibling lane `futon7/holes/vsatarcs-doc-integrity.md` (DI-1 BROKEN-CITATION, DI-2 DRIFTED — both from
  the cross-check loop working). Doc-ref cells rows 2–6 filling in parallel.
- **A+B integration + C3 acceptance dispatched to codex-1** — run Unit B EFE over the now-grounded
  WM-region graph; assert C3 (EFE-top = applicable single-cycle leaf, no cherry-pick) + INV-G gate
  refusals + keystone single-held. Honest-fail required (don't tune the graph to pass).
**Next:** review codex-1's C3 result (re-run gates, confirm C3 actually holds) + converge claude-3's
doc-axis → WM-region is one coherent claimed-with-backing set → mission's first region closes.

### Checkpoint — 2026-06-07 (C3 acceptance: honest FAIL — no-cherry-pick PROVEN, decompose gap localized)
**A+B integration ran** (codex-1, futon3c branch `wm-outing/2026-06-07` `ad2da10`, new test ns
`capability_star_map_integration_test.clj`). **Reviewed by claude-1:** read the test (honest — even named
`...currently-fails-on-mega-mission-test`), re-ran the targeted ns (3 tests/11 assertions, 0 failures),
verified hole-counts from `graph.edn`. Gates: kondo 0/0, check-parens OK, logic-model `:verified? true`.
- **C3 verdict: FAIL — and the FAIL is the finding.** Live candidate universe over the real WM-region graph
  = the 2 non-complete real missions: `M-war-machine-pilot` (`:open`, **9 holes**) + `M-capability-star-map`
  (`:unknown`, **10 holes**). Both multi-hole → **no single-cycle leaf exists in the region.** EFE top pick
  = `M-war-machine-pilot`: applicable? **true** (G-applicability 0.0) but single-cycle-leaf? **false** (9
  holes). G-total 15.18. No graph tuning, no candidate cherry-pick.
- **What's PROVEN — the disease is cured:** EFE does **not** cherry-pick / fake-pick the mega-mission. It
  scores it applicable-but-not-a-leaf and does not pretend otherwise. INV-G holds: pentagon `:pursue` +
  goal-extending decompose both refused, ranked empty. Keystone path: single held substantive node;
  `run-verify-equivalent :verified? true`. The watched-cycle-1 disease (silent cherry-pick of a mega-mission)
  is gone.
- **The localized gap — why C3 can't pass as-is:** when no single-cycle leaf exists, EFE must propose a
  **path-refining decompose** (the legal `:extends-goal? false` kind from ARGUE) of the top applicable
  multi-hole mission INTO a single-cycle leaf, and rank THAT top. The integration only supplied
  `:open-mission` actions + a goal-EXTENDING decompose (correctly refused); a path-REFINING decompose was
  never generated/ranked. This is the **"decompose-should-move-the-field"** refinement — now shown
  **REQUIRED, not optional**, for the WM-region to yield a leaf.
- **Data soundness:** the FAIL is on SOUND status data — the live futon2 mission-registry `classify-status`
  reads each doc's `Status:` line (leading-token), insulated from the VSATARCS render drift (DI-2
  cross-check, claude-1). So this is a real STRUCTURAL finding, not a stale-status artifact.
- **Disposition: OPEN — the first region does NOT close on C3.** Next decision (Joe): (i) wire the
  path-refining decompose to close C3; (ii) accept-with-known-gap; or (iii) spin a decompose sub-mission.
  codex-1's test is a characterization test on a branch (locks in no-cherry-pick); merge is part of the call.

### Checkpoint — 2026-06-07 (region sweep COMPLETE — all 7 regions grounded; terminal C3 dispatched)
**Joe /loop'd "do the other regions the same way."** All registry thesis-clusters grounded in
`C-pudding-prover §8.2.1–8.2.7`, region by region (capability→producer→commit; codex-1 commit-audits;
claude-3 doc-axis + the business region; claude-1 authors/reviews/converges):
- **T4/WM** (§8.2.1): two-axis complete 6/6. · **T5/semantic** (§8.2.3): interest-network clean + arxana/stereo
  ☑-substrate + CT-vision frontier. · **KIT** (§8.2.4): 4 verified + 3 frontier (= the T2 business frontier).
- **T0/T-inf** (§8.2.5): light (witnessed history / xeno-eval frontier). · **T3/math** (§8.2.6): substrate
  grounded + Rob ⊗ external-witness + theses held. · **T2/T1.5 business** (§8.2.7, claude-3 authored): Eric
  ⊗ external-witness (invoices verified on disk) + scan ☑-substrate + cold-cycle ◇ frontier.
- **Grounding taxonomy** (held across every region): `☑ verified` / `☑ substrate` / `⊗ external-witness` /
  `◇ frontier`.
- **Uniform healthy asymmetry: ZERO Baudrillard** across all documented regions — real-but-undocumented or
  honestly-held, never documented-but-unshipped. The stack under-documents, never over-claims.
- **Concrete actionable: regenerate VSATARCS** — 4 undocumented-but-real caps + the status-drift (DI-1/2/3)
  all close with one regen.
**Terminal C3 dispatched** (codex-1): does EFE pick an applicable single-cycle leaf over the EXPANDED
candidate set? — the test of whether landscape-expansion resolves the cherry-pick (vs wiring decompose).
claude-3's standing prediction: may still FAIL if the relevant single-cycle candidates (cold-conversion) are
frontier/unwired → then the decompose / ACT-DELIVER build is the real next step. **Next:** the C3 verdict.

### Checkpoint — 2026-06-07 (TERMINAL C3: PASS on the production-faithful pipeline — core acceptance MET)
**Verdict: C3 PASS** (codex-1 `4bff38c`, branch `wm-outing/2026-06-07`; **independently re-run + reviewed by
claude-1** — read the deftest, re-ran 5 tests/22 assertions, 0 failures).
- The first terminal run FAILed — but as a **raw-candidate artifact**: it ranked ALL live missions incl. 0-hole
  ones, top-picking M-arxana-roundtrip (0 holes, `:identify`) — which production NEVER admits.
- Corrected run applies the PRODUCTION guardrails filter (`futon3c.wm.guardrails/autonomous-admissible?` →
  `open-mission-with-holes?`, requires ≥1 open hole). Filtered set = the 7 ≥1-hole missions. **EFE top-ranks
  the applicable single-cycle leaf `M-essay-corpus-substrate`** (1 hole, `single-cycle-leaf? true`, G-total
  −2.158) above M-webarxana (3.842) / M-stack-stereolithography (6.842). No cherry-pick; INV-G holds.
- **Meaning: the expand-the-landscape choice (Joe, over wiring the decompose) RESOLVED the watched-cycle-1
  cherry-pick disease.** A real single-cycle leaf now exists, the production guardrails surface it, EFE picks
  it. The keystone `efe-trustworthy-over-starmap` is **demonstrated.**
**Honest caveats (not yet closed-clean):**
- Curated expanded candidate set, NOT a full production mission-registry scan (test note flags this).
- On branch `wm-outing/2026-06-07` — not merged.
- **Latent EFE scoring inversion:** the `body = 3×holes` term would prefer a 0-hole mission over a 1-hole leaf
  *if one reached EFE* — masked by the guardrails ≥1-hole filter (defense-in-depth), but a body term that
  prefers *exactly* 1 hole would be a robustness hardening.
- The path-refining **decompose** move is still the fix for any region that is ALL multi-hole (no leaf) —
  not blocking now, but the named follow-on.
**Next (Joe's call):** merge the branch · advance/close the mission (keystone demonstrated) · the hardening
(EFE body-term + decompose move) · regenerate VSATARCS (the doc-gap actionable).

### Design — star-map ⟷ live-WM EFE BLEND (Joe-confirmed 2026-06-07; the next INSTANTIATE slice)
**Finding:** the merge is ALREADY the architecture — `futon2.aif.efe/compute-efe` (the live scorer) computes
`graph-efe-terms` and adds `:G-graph-pragmatic` into `g-total` **additively** (efe.clj:348), alongside
risk/ambiguity/info/survival/structural-pressure. It does NOT replace anything. It's **inert in the live WM**
because the judgement never passes `:capability-graph` + `:pre-registered-goal` (so the graph term is 0; only
the unused `rank-star-map-actions` feeds a graph). Joe: *"the EFE is the nervous system — treat with delicacy;
⭐ is a marker, not necessarily preferred."*
**The slice (wire it conservatively):**
1. Thread `:capability-graph` (= `M-capability-star-map.graph.edn`) + `:pre-registered-goal`
   (= `:wm-overnight-unsupervised`, the Daumal ascent) into the **live judgement's** EFE opts (the server-side
   assembly that builds `judgement.ranked-actions` for `/api/alpha/war-machine`). Missions not in the graph
   get `G-graph-pragmatic` 0 — existing behaviour preserved; only graph-known missions get the blend (bounded rollout).
2. **Conservative weights (the delicacy = where "⭐ ≠ preferred" lives):** applicability-penalty **1000 → a
   nudge (~5)** (Joe: nudge, not veto — guardrails hard-gate admissibility separately); ascent-weight **20 → ~6**;
   body-weight **3 kept** (drives the single-cycle-leaf shape). Tunable up later.
3. **⭐ provenance → UI:** tag actions with a non-zero star-map contribution (`:star-map? true`) + pass a ⭐
   through the snapshot's `ranked-actions` so the UI marks star-map-originating recommendations (marker, not preference).
4. **Calibration readout:** live top ranked-actions BEFORE vs AFTER — confirm star-map NUDGES (doesn't flip the
   ranking / dominate) + ⭐ appears on graph-known missions.
**Deferred (Joe, later round):** a **peradam / mana reward system** keyed off ascent-credit; extending
`graph.edn` to all grounded regions. **Discipline:** additive-only · staging branch (Joe merges) · NO
live-reload of the EFE into the running JVM (nervous system + the day's crash) · never restart the JVM.
Implementation → codex-1; claude-1 reviews.

### Live finding — WM priorities vs operator expectation (5-cycle run, Joe 2026-06-08)
The first live guardrails-mode run (NAG namespaces reloaded; **blend NOT yet active in the JVM**)
selected from the OLD free-energy/tension scorer: the 7 NAGs + the engaged action were dominated by
**futon6 / math domain-work** (canon-fingerprint-store, bayesian-structure-learning, coupling-as-constraint,
fulab-wiring-survey, tpg/xor-coupling-probe) — high-tension / strain-edge missions.

**Operator signal (Joe — a signal, not a decree; [[feedback_operator_not_sovereign]]):** he'd have expected
**local-capability-expanding** missions at the top — the **self-representing-stack follow-on backlog** (the
stack improving its OWN capability = the wm-overnight ascent) — not domain-work. *"I love futon6 and
working-in with Rob … but from a WM perspective I'd have expected questions/missions that expand our local
capability."* Mild, explicitly not a complaint — a priority-alignment datum.

**Diagnosis = exactly the blend's purpose.** The old selector ranks by tension/free-energy with **no
ascent-credit**, so it favours the strongest *local strain* (active math research) over the **pre-registered
ascent** (local self-improvement). The star-map **blend** adds `G-ascent-progress` toward the goal — it is
the mechanism that would re-weight local-capability (ascent) missions *above* off-ascent domain-work. So this
observation is **live evidence FOR the blend** and a **calibration target**: with the blend active, the top
picks should shift from futon6/math toward self-representing-stack follow-ons. **Testable prediction:** reload
the blend + re-run → the engaged action / top NAGs should move toward the local-capability ascent.

**Open — the pre-registered goal's CONTENT.** The blend wired `:pre-registered-goal :wm-overnight-unsupervised`
(a single frontier node). But "expand our *local* capability" suggests the goal may need to encode
**local-capability-first** more richly (the self-representing-stack ascent path), or the ascent-credit won't
capture what Joe means by "local." A goal-definition refinement to weigh when the blend goes live — and the
operator's expectation is the ground-truth to calibrate the ascent-weight against.

### Realignment verdict — blend live + extended graph (claude-1, 2026-06-08)
Brought the blend online (reloaded efe + report wiring) + extended the graph to the full grounded landscape
(`dcec611`; **reviewed: ascent spine clean — every spine cap keeps its original scope, no domain cap injected
into the `wm-overnight` transitive scope, so domain (t3/t2) caps get ZERO ascent-credit — correct**).
**Verdict: the blend ALONE does NOT realign.** A fresh cycle engages `M-canon-fingerprint-store` (futon6) —
because resolving its NAG made it advanceable + it's low-body, while the local-capability missions are
HIGH-hole (war-machine-pilot 9, capability-star-map 11) → big `G-body-size` penalty the conservative
`graph-ascent-weight 6.0` can't overcome. **Ascent-credit fights the body-term and loses.**
**The realignment mechanism is the GAP-READER** (`E-efe-education` R1; branch `wm-outing/2026-06-08-gap-reader`
`393a92e`, reviewed/accepted, awaiting operator live-activation): claude-3's `:gap-score` directly rewards the
LOW-hole local-capability stubs (war-machine-tuning 0.49 …), where gap-credit AND body-credit ALIGN — so it
lifts exactly the missions ascent-credit-alone couldn't. **R1 (the chord) IS the realignment** — same lever.
**Open design tension:** `G-body-size` (prefer-low-hole) vs ascent (prefer local-capability even if high-hole)
— the body-term hardening (prefer-exactly-1-hole) + the gap/ascent terms need joint tuning. Test on R1
live-activation: does the engaged action shift to the high-gap local-capability stubs?

**CORRECTION (claude-3 STANDARD-VERIFY, 2026-06-08) — "gap IS the realignment" is REFUTED.** Scaled to the
full 194-mission corpus, gap-score is **size-dominated + domain-blind**: M-canon-fingerprint-store (math,
0.458) ≈ M-war-machine-tuning (local, 0.491) — effectively TIED, so gap does NOT discriminate math-from-local.
The realignment is the **body-term's** doing; gap merely rode along (the 6-mission story was an artifact).
**Live risk caught BEFORE activation:** top-gap missions are tiny abandoned stubs (M-coupling-as-constraint
0.94 / M-tpg-coupling-evolution 0.88 — BOTH open) → gap-as-is would actively mis-steer the WM toward them.
**Gap-reader activation GATED** until: (a) claude-3 de-biases gap → growth-surface (count of stub-frames above
a size floor, not mean stub-fraction); (b) claude-1 gates gap on ASCENT-credit>0 (gap refines WITHIN the local
domain the ascent selects, not globally); (c) live experiment runs WITH A CONTROL (body-only vs body+gap) to
attribute the realignment honestly. **Architecture:** ascent = local-vs-domain selector; gap = within-local
expansion-refiner; body = boundedness. The chord-as-STRUCTURE (reduction↔expansion coupling) stands; the
gap-as-realignment-SIGNAL did not survive the cross-check.

### ACTIVATION verdict — gap-reader live on the WM, controlled experiment (claude-1, 2026-06-08)
Operator consent given ("let's activate and test it out on the WM"). Merged the gap-reader + domain-gate to
`futon2` main (`ad46dd8`), reloaded efe + report via Drawbridge, reset the three caches so the now-ratified
domain file (`mission-domain-ratified.edn`, **176 local / 16 math / 2 other**, Joe's Reading-B rulings) is read.
**Controlled experiment (body-only vs body+gap, same everything else):**
- **Baseline (blend-only):** engages `M-canon-fingerprint-store` (**math**), which sits at **#1**. The mis-steer.
- **Blend + domain-gated gap:** engages **`M-futonzero-mvp`** (**local-capability**); the whole top-8 is local;
  canon-math falls to **#47** (`G-gap 0.0`), distributed-frontiermath to **#68**. Gate is clean — every
  gap-bearing mission is `:local-capability`; every `:math` mission gets `G-gap 0` and sinks.
**Verdict: the realignment IS the gap-reader — but only once domain-GATED.** This resolves the STANDARD-VERIFY
correction above: raw gap "rode along" (size-dominated, domain-blind), but gap *gated to the ratified local
partition* is exactly the lever that lifts local-capability over math — the body-term alone left canon at #1.
Architecture confirmed: **ascent = local-vs-domain selector · gap (gated) = within-local expansion-refiner ·
body = boundedness** — with the ratified domain-gate as the discriminator the raw gap-score lacked.
**Honest caveat — the delicacy is too hot.** `gap-weight 6.0` **saturates**: the top **5** missions tie at the
ceiling (`G-gap 6.0`, gap-score maxed at 1.0), so the engaged `M-futonzero-mvp` is merely *first of a 5-way
tie* (a designed false-floor tie, [[reference_wm_g_ties_false_floor]], but here the gap has washed out the
ascent/body ordering at the very top). The gap currently **over-discriminates at the domain boundary** (good —
math sinks) and **under-discriminates within local** (ascent/body swamped). **Next precision update
(`E-efe-education`):** drop gap-weight to ~2–3 so gap is a *domain tie-breaker* (canon stays sunk) not a
*within-local flattener* (ascent/body should order the top, gap nudges). This is the first concrete
precision-error the live system handed us — the chaos-tuning loop's first datum.

---

## Checkpoint: Basecamp — rounded-up directions for future work (Joe + claude-1, 2026-06-08/09)

A long session resolved the WM into an **AlphaZero-shaped active-inference loop** (the destination named in
[[M-futonzero-capability]] §22). This basecamp rounds up the directions it opened, organised by the loop's layers.
**★ ready to do · ⛓ gated on a named dependency · ✅ landed this session.**

### 1. PREFERENCE — the marking-rule (exploit pole)
- **★ INSTANTIATE the goal-directed marking-rule into the live EFE** — make the live WM de-select off-map work the
  way the Petri-net POC does (`futon7 M-pudding-peradams.petri-poc.bb`, ✅ validated *in-model*). Earns witness (c)
  for `:efe-trustworthy-over-starmap` **live** → flips the goal's last `:held` precondition → unblocks
  `:wm-overnight-unsupervised`. Honest precondition: the *harder* POC (real off-ascent-but-mints discrimination + the
  gap-vs-marking contest). Home: `futon3c E-efe-trustworthy-over-starmap`. (Live EFE = Joe's consent locus.)
- **★ Pre-witness graph wiring** — add a `:pre-witness` field to capability nodes; author the on-ascent `:held`
  capabilities' pre-witnesses (operator's high-leverage authoring; the play-selector kit ranks them). Home: this
  mission + C-pudding-prover §11.

### 2. EXPLORE — E-warranted-play (the policy / play-out pole)
- **✅ The strategic play-selector kit** (`futon3c E-warranted-play.selector.bb`) — ranks `:held` caps by
  on-ascent × tractable × generativity.
- **EFE-over-play-outs (the AlphaGo move)** — extend the WM from one-priority to policy-level `G(π)`; **patterns
  ARE the policy layer** (✅ thesis validated: the pattern-backed futon5 wiring, all 6 validate). Shallow (1–2 ply)
  now, ⛓ deepen as the field-simulator earns trust (§3).
- **Run the warranted-try loop live** (the overnight run) → the morning **Dokusan** over the minted 🟡s. ⛓ on the
  reward (§4) for full certification.

### 3. SIMULATOR / FIELD — substrate-2 + full-C (the value's ground)
- **⛓ M-mission-scopes-into-substrate-2 §D2 (co-owned, claude-3)** — full-C `T·(10−H)` as the macro *where-prior*
  that **RETIRES** the additive gap-term (today's F1 gap-reader is the *stepping-stone*, not the destination).
  End-state = retire; transition = both-live-gated + operator consent. This is the field the play-out rolls over.
  Home: `futon3c M-mission-scopes-into-substrate-2 §D2`.
- **The field-transition model** ("apply pattern → next state") — the regulator's *no-fabricated-dynamics* gap; the
  honest hard part of deepening the play-out.

### 4. REWARD — the peradam economy (G1 is the last mile)
- **✅ The three-witness certifier** — built, de-laundered (the review caught it laundering its own routes), merged
  (`futon7`).
- **⛓ G1 — bind `:arrow-witness` to a live affect-event.** codex-2 hardened the affect detector (✅ the arrow
  *feed*); remaining: the reliable binding **into** `certify-peradam` + **G2** (performed-affect resistance). **G1 is
  THE last mile for the multiball** (the build's own peradam: labor ✓ + fruit ✓, arrow pending). Home:
  M-pudding-peradams §7-G1, §14–15.
- **Keep the kit↔peradam distinction**: a *kit* certifies by working (a build); a *peradam* witnesses a
  **transformation** (the hybrid shifts — the affect-arrow). Don't cross-wire them (the experiment did, once).

### 5. THE LEARNER — FutonZero (the destination to charter)
- **Charter the generative FutonZero** = **done 2026-06-09** as [[M-futonzero-generative]]: the self-bootstrapping
  WM-learner (AlphaZero, no training data, *any* domain). The Sen distinction *is* the objective: turn
  **functionings** into **capabilities**. Realization remains gated on field-simulator adequacy + reward
  anti-laundering.

### 6. THE INSTRUMENT — E-possible-world-regulator (follow-ups)
- **F3** (mission→feature bridge → the sustainability axis); the **ascent-term residual** (graph-only → registry);
  and — now meaningful, not saturation-blocked — the **gap-weight delicacy-tune** via `E-efe-education`.

### 7. LOOSE THREADS
- **M-hypergraph-operator** — **partial-fold ruling landed** (codex-4, 2026-06-09): fold the generic
  operational-loop half as superseded by WM/capability-star-map/pudding-prover (not built as the futon5a
  pocketwatch); keep the JSDQ 8-sector/39-item person-facing capability operator as a distinct unbuilt mission
  target.
- **futon5 `warranted-play/2026-06-08-wiring`** — the pattern-backed wiring (committed, **unmerged**).
- **Campaign-BSL math/superpod instance** (Rob) — awaiting the local proving-ground.
- **VSATARCS regen** (standing DI-3 doc actionable).

---

## Checkpoint: Basecamp fan-out — 7 pattern-warranted mini-pilots (Joe + claude-1, 2026-06-09)

Joe's move: run E-warranted-play's explore pole at **fan-out scale** — one pattern-warranted mini-pilot per
Basecamp thread, in parallel (propose-only envelope: read-only, no live-EFE/JVM, no outward acts, no commits;
operator Dokusans). **6/7 returned warranted next-moves in ~100s** (workflow `wf_6ee2f2a9-dc2`). Triaged:

**★ Ready to dispatch now**
- **§1.2 pre-witness graph wiring** — *warrant: logic-model-before-code.* Add a `:pre-witness` field to the
  capability-node schema + draft candidate `:pre-witness` specs ("doing X attests C") for the two on-ascent
  `:held` caps (`:efe-trustworthy-over-starmap`, `:wm-overnight-unsupervised`). Propose-only (graph.edn untouched).
- **§5 charter the generative FutonZero** — *warrant: futon-theory/baldwin-cycle.* Write the HEAD/IDENTIFY charter
  for a new `M-futonzero-generative` (self-bootstrapping WM-learner; AlphaZero→WM mapping from
  [[M-futonzero-capability]] §22; two hard gates = field-simulator adequacy + reward anti-laundering). Charter
  **dispatched 2026-06-09 (codex-4)**; realization gated.
- **§7 M-hypergraph-operator fold-or-keep** — *warrant: subsumption-claim discipline.* Both-live verification → a
  **partial-fold** ruling: mark the operational-loop half as superseded by the WM/Pudding surfaces · **KEEP** the
  JSDQ person-facing capability model as a distinct unbuilt mission. **Dispatched 2026-06-09 (codex-4):** ruling
  added to `futon5a/holes/missions/M-hypergraph-operator.md`; star-map node narrowed to "JSDQ hypergraph
  operator".

**⛓ Gated (blocker named)**
- **§1.1 marking-rule into the live EFE — ⚠ HYPOTHESIS CORRECTED 2026-06-09 (live check refuted it).** Two parts
  of the original finding stand: **both** `:G-ascent-progress` and `:G-gap` are already live and summed into
  `:G-total`, and **no new EFE term is needed.** But pilot #1's *hypothesis* — that the live caller **OMITS**
  `:capability-graph`/`:pre-registered-goal` from opts (so only `:G-gap` ranks) — was **REFUTED on the live
  judgement**: `:opts-have-graph? true`, and the marking-rule *is* firing (`ascent-progress 0.5`). The real bug
  is **not a missing graph** and **not a one-line opts fix**. It is two coupled defects in `graph-efe-terms`:
  **(a) off-map escape hatch** (off-graph actions score a flat `0`, so off-map work is free while on-map work is
  taxed — top-12 ranked actions all off-map, first on-map mission at rank 83/88), and **(b) body-penalty
  granularity** (`body = weight × total-open-hole-count` penalizes a whole mission as if done in one step, burying
  the two `ascent>0` missions at +20/+21 — the single-step-vs-policy confusion in miniature). **→ Full diagnosis,
  the demonstrated fix (off-map penalty ~4 + leaf-aware body → on-ascent mission #1, cursor → rank 9), and the
  policy-layer charter now live in [[M-wm-policies]] (`futon2/holes/M-wm-policies.md`) §2–3.** Lesson:
  verify-before-claim earned its keep — the unverified call-site hypothesis was the wrong tree.
- **§3 D2 / full-C** — gated (full-C not yet computed over substrate-2; claude-3's lane). Move: post claude-3 the
  both-live-gate readiness checklist. *warrant: the transition-guard / subsumption-claim.*
- **§4 G1 arrow-binding** — updated by **E-affect-live** (codex-4, 2026-06-09). The stale blocker was too strong:
  the affect producer exists and `storage/futon0/vitality/affect.jsonl` has now been materialised from the live
  Evidence Landscape. `futon7/holes/pudding-prover.bb` also has the read-only `arrow-witness-for-event` resolver
  at the documented `FAST-FOLLOW G1` site. Remaining work is no longer "find the feed"; it is calibrating/applying
  the resolver to a specific work-event while preserving G2 (performed-affect) and the out-of-blanket fruit gate.
  *warrant: guardrails-vs-tooling (the affect feed is fallible tooling, not an invariant).*

**✗ Did not return:** §6 regulator follow-up (the agent didn't emit its structured result; re-run if wanted).

**Meta:** the fan-out demonstrated E-warranted-play's explore pole **at scale** — 6 honest warranted moves (3 ready,
3 gated-with-real-blockers, incl. a genuine diagnostic on §1) in one ~100s parallel sweep, propose-only, zero live
contact. The overnight-run shape, run now. Highest-leverage next (UPDATED 2026-06-09): §1's call-site hypothesis
was *checked and refuted* live; the real keystone-wiring bug (off-map escape + body granularity) and its fix now
live in [[M-wm-policies]] — Track 1 is the ready cleanup, Track 2 the policy layer.

### Closure proposal — what would close M-capability-star-map itself

M-capability-star-map should close as the **capability build-map + scheduler-wiring mission**, not as the whole
overnight learner. Successful closure means:

1. **Machine-readable graph exists and is consumed.** The graph has the mission/capability split, typed edges,
   ascent goal, held/satisfied status, and `:pre-witness` candidate specs for the on-ascent held caps; the live WM
   ranking path consumes the graph + pre-registered goal rather than a flat mission list.
2. **The star-map is visible in the WM/WebArxana surface.** Joe can inspect a Khan-Academy-style capability map
   showing satisfied/held/frontier progress, prerequisite edges, on-ascent held nodes, and witness affordances.
   A pure data artifact is not enough; the map must be operator-visible.
3. **Production-faithful C3 remains green.** With production guardrails filtering before EFE, the top ranked action
   is an applicable single-cycle leaf, and unsafe pursuit / unagreed operator-exit actions do not reach ranking.
4. **Keystone honesty is explicit.** `:efe-trustworthy-over-starmap` has its witness shape, remains distinct from
   `:wm-overnight-unsupervised`, and is not marked satisfied until the named witness run actually attests it.
5. **Known residuals are routed, not smuggled into closure.** Full-C / field simulator, G1 reward rigor,
   path-refining decompose, body/gap precision tuning, VSATARCS regen, FutonZero-generative, and the
   now-narrowed JSDQ hypergraph operator are named follow-ons unless Joe explicitly makes one a close gate.

Non-closure: the overnight unsupervised run itself, living hole-discovery from turns, and the generative FutonZero
learner are downstream consumers of the map; requiring them here would turn this mission into the whole stack.

### Salted follow-ons — not closure gates unless promoted

- **M-futonzero-generative:** **done 2026-06-09.** New HEAD/IDENTIFY charter exists; realization remains gated on
  field-simulator adequacy and reward anti-laundering.
- **M-hypergraph-operator fold-or-keep:** **done 2026-06-09.** Existing mission now records the partial-fold ruling:
  operational-loop half superseded by WM/Pudding surfaces, not built as the original futon5a pocketwatch; JSDQ
  person-facing capability model remains distinct.
- **D2 / full-C:** stays in M-mission-scopes-into-substrate-2 and claude-3's lane; eventual transition retires the
  additive gap-term only after both-live gating.
- **G1 arrow-binding:** stays in M-pudding-peradams / peradam work; specify the read-only resolver over the affect
  feed, but do not make reward-rigor a star-map close condition.
- **EFE precision hardening:** body-term exact-one-hole preference, gap/ascent tuning, and path-refining decompose
  stay as E-efe-education / E-efe-trustworthy-over-starmap follow-ons unless a regression breaks C3.
- **VSATARCS regen:** documentation debt; important for discoverability, not the build-map mechanism itself.

---

## Provenance

- Operator seed: emacs-repl, 2026-06-07, Joe — "convert the entire mission landscape into a big graph …
  big holes (mega-missions) for big undeveloped capabilities, but also know where the small and
  meaningful targets are … a Khan-Academy star-map prior + the stereolithographic print as the map of
  real capabilities. Rob has done the same with a big proof."
- Spawned from: M-futon-forward-model WM watched-cycle 1 (2026-06-07), whose re-route datum (§13.2)
  surfaced node-granularity / advanceability as the operative gap.
