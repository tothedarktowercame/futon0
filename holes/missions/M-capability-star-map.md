# M-capability-star-map: the mission landscape as a navigable capability graph

**Type:** Mission
**Lifecycle:** MAP complete (Q1–Q7 answered + ready-vs-missing table, 2026-06-07). IDENTIFY approved (operator, 2026-06-07). HEAD done. Next: re-anchor the DERIVE-draft (the lambda/scope model + ensemble-1 VERIFY-spike) onto the MAP findings.
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

## DERIVE — DRAFT (authored ahead of MAP; HELD for re-anchoring)

> Discipline note (2026-06-07 step-back): the model below was authored *before* a verified IDENTIFY and
> a real MAP — design leaked ahead of phase. It is kept (not discarded) but marked DRAFT: it must be
> re-anchored once MAP surveys what actually exists (the `:mission/*` props, the exotype `.edn`s). The
> "Ensemble 1" section within is in truth a **VERIFY-spike run early** (it validated this draft).

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

## Provenance

- Operator seed: emacs-repl, 2026-06-07, Joe — "convert the entire mission landscape into a big graph …
  big holes (mega-missions) for big undeveloped capabilities, but also know where the small and
  meaningful targets are … a Khan-Academy star-map prior + the stereolithographic print as the map of
  real capabilities. Rob has done the same with a big proof."
- Spawned from: M-futon-forward-model WM watched-cycle 1 (2026-06-07), whose re-route datum (§13.2)
  surfaced node-granularity / advanceability as the operative gap.
