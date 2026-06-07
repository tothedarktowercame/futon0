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

## IDENTIFY (in progress — operator directed the build 2026-06-07; residual HEAD questions noted)

Operator direction (2026-06-07): **build this end-to-end *before* the next WM cycle.** Success
criterion (verbatim sense): *the next WM cycle should not need the pilot to cherry-pick a discrete
task — we should trust EFE much more, because EFE will be doing real Active Inference over the
star-map.* This answers two HEAD questions: **no crude proxy** (build the real EFE-AIF), and the **node
model is the missions-as-lambdas / scopes metaphor**. (Residual HEAD questions — the M-futon-forward-
model relationship and owner/pairing — still open; see HEAD exit criteria.)

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
