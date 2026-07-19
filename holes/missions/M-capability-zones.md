# M-capability-zones: a Voronoi partition of Futon City, so learning has ground to stand on

**Type:** Mission
**Status:** INSTANTIATE (S1) — chartered 2026-07-19 (claude-9 draft from the morning-after
design conversation with Joe). Operator scope-pin + dimensional amendment 2026-07-19; S1
belled to codex-10 the same day (spec: `M-capability-zones-S1-handoff.md`). Paper pattern
landed in `p4ng/main-2026.tex` (sibling of R17′′).
**Owner:** claude-9 (ground control); claude-3 holds the owner seat for S1 dispatch/review
(session of 2026-07-19) — authors per slice via the coding-handoff protocol
**Home-repo:** futon0 (cross-repo coordination home, per `single-locus/mission-home` and the
M-capability-star-map precedent; the successor mission to that CLOSED mission)
**Discharges:** `repair-attempt-036-policy-nondiscrimination` and
`repair-attempt-037-artifact-binding-mismatch` (both discharge against slice 2's concrete spec —
attempt-037 proved a vague repair spec cannot be authored; this document is the concrete one)

**Cross-references (read-only intake):**
- `futon0/holes/missions/M-capability-star-map.md` — CLOSED predecessor; proved EFE-over-the-
  capability-graph is trustworthy. This mission gives the graph a *partition* and the partition
  a seat in G.
- `futon5a/holes/missions/M-learning-loop.md` — owns operator-side capability mining (hinge-log →
  capability graph in futon1b, WebArxana render). This mission is its machine-side sibling; it
  consumes the same graph substrate and does not fork the vocabulary.
- `futon3c/src/futon3c/live_efe_map.clj` (M-live-efe-map, ALL LIVE) — the coordinate field
  (BGE mission embeddings `futon3a/resources/notions/bge_mission_embeddings.json` + carpet
  positions `futon6/data/mission-carpet-pos-embed.json`) and the live traffic layer (agent
  saucers, operator rocket with the honest-absence rule).
- `futon2/holes/E-feature-constellation` + `powerbi-tui/skills-expanded-v2.md` §"The research
  programme, computed" — Figure 5: 172 missions retracted to the above-median evidential core;
  node size = enacted magnitude, edge width = summed turn-attestation, zero hand-typed rows.
  **Load-bearingness is already computed there.**
- `futon2/src/futon2/aif/action_proposer.clj` (`gap-actions`), `intrinsic_values.clj`
  (per-class Beta posteriors + `wm-hyperparameter-update` hyperedge persistence),
  `efe.clj` (`:G-efe = risk + ambiguity`), `full_loop_runner.clj` (`selection-discrimination`).
- `M-substrate-metric` (via C-substrate-completion) — **owns the ground metric and node
  granularity. Consume, don't fork**: slice 1 uses plain BGE cosine as the interim distance,
  marked `:interim-metric`; the Wasserstein/Fisher–Rao upgrade lands there, not here.
- `p4ng/main-2026.tex` — catalog pattern **"Capability Zones over the Embedded Landscape
  (staged)"** (sibling of R17′′), landed 2026-07-19. This mission is that pattern's build-out;
  the pattern's **dimensional note is normative here**: operative partition in a 3-D reduction,
  2-D carpet as its projection, high-D nearest-seed as diagnostic only, R9-spirit acceptance
  invariant (accepted object = operative object).
- `futon2/docs/futonzero-alphazero.md` — the honest R2 caveat ("the prior isn't trained from
  outcomes"). The retro-mined corpus this mission produces is the training data R2 was waiting
  for; R2 itself stays in its own lane (futon6 M-differentiable-substrate).

---

## Corpus scope (operator-pinned, Joe 2026-07-19, recorded by claude-3)

- **Construction corpus (S1 harvest):** the FUTON stack — the `~/code/futon*` family
  (17 repos; worktree clones like `futon3c-index-check` excluded). Same landscape whose
  missions feed `futon3c.live-efe-map` and that was fair game for M-capability-star-map.
- **Demonstration stratum (distinct, NOT in the S1 harvest):** `~/code/apm-lean`,
  `~/code/ukrn-services-simulation`, `~/code/mathlib4/DarkTower`, and future work under the
  tithe plan (20% outward bucket, `futon7/holes/M-futon-forward-model.sequencing-v1.md`
  §tithe). Joe: these are "demonstrations of capability moreso than loci for building
  capability." Kept distinct because:
  1. honest coverage — S1's per-repo coverage note stays within the construction corpus;
  2. demonstrations are prime evidence for the DERIVE attachment-mode question — they are
     the edge-attachment cases (the charter's own example, mathematical reasoning, is
     exactly what apm-lean/DarkTower demonstrate);
  3. the Beta-update semantics for demonstration evidence (capability exercised in foreign
     territory) may legitimately differ from construction evidence — that is a design
     point for review when the stratum is harvested, not a hand-tuned constant.
- **Follow-on:** harvest the demonstration stratum after S1 review lands, same machinery,
  items tagged with `:demonstration` provenance in the artifact.

## HEAD

### The question

When the War Machine's leading candidates are all "learn a capability I don't have," how should
it tell them apart — without inventing discrimination it hasn't earned?

### The finding that forced the question (2026-07-19 night)

After four consecutive fully-gated groundings (attempts 032–035), attempt-036's top-5 were all
`:learn-action-class` candidates at bit-identical `:G-efe 17.470403…`, and the
`selection-discrimination` tripwire refused to pick arbitrarily. Diagnosis (verified live): the
14 gap-classes' intrinsic-value posteriors have **zero recorded updates** — all at the
Beta(1,1) prior. The identical G was *epistemically honest ignorance*: the machine has never
observed anything about any capability gap. Both the estimator and the tripwire behaved
correctly. What's missing is the loop that lets evidence accumulate — and the ground it would
accumulate over. Attempt-037 then proved that a repair without a concrete spec cannot be
authored (`:artifact-binding-mismatch`; same signature as 014/021 — spec-vagueness, not
pinning, is the variable).

### The three principles (from the design conversation, 2026-07-19)

1. **Stigmergic, not biographical (Joe: "I've simply been the ant building this anthill").**
   Capabilities live in the built structure, not in agent biographies. No agent-resolved
   attribution; a capability exists where the structure bears load through it. The months of
   git history are described as *which load paths got built*, never as "who learned what."
2. **Earned, not invented.** Discrimination between gap classes must come from evidence
   (retro-mined history, live traffic, recorded outcomes) flowing through the *existing*
   posterior machinery. The tripwire is untouched; it starts passing when the posteriors
   genuinely separate. No hand-tuned per-class constants, ever.
3. **Morphogenetic attachment; hyper-prior recovered empirically.** Sometimes new capability
   attaches preferentially to existing mass (futon1→1b XTDB, ants→R-numbers→War Machine; the
   Self-Improving Loops and Live Invariants clusters); sometimes at the edge (mathematical
   reasoning; "the JVM into a ClojureVM", built the night before this charter). Neither mode is
   preferred a priori. The Gypsy-Jazz/novelty dial is NOT a slice-1 or slice-2 parameter — it
   is a DERIVE-phase question answered from history (see §Derive), defaulting to neutral until
   then.

### The mechanism in one paragraph

Embed each action-class into the same BGE space that Futon City already lives in (seed = the
class's text now; the centroid of its exercised commits once mined — seeds drift with the
anthill). The Voronoi partition over those seeds assigns every mission, feature, commit, and
live agent position a **zone** — the **operative membership computed in a 3-D reduction of the
embedding space** (amended 2026-07-19, per the paper pattern's dimensional note — see
cross-refs: print-flat 2-D silently misfiles boundary features through adjacency distortion;
raw high-D suffers distance concentration, leaving every nearest-seed margin uniformly thin and
the partition uninspectable; three dimensions keep the Voronoi cells compact and the
tessellation walkable), rendered on the 2D carpet as a projection *from the 3-D partition*,
with near-boundary margins reported honestly as mixed. The high-dimensional nearest-seed
reading is retained **only as a boundary-distortion diagnostic**. The load-bearing reason is an
acceptance invariant in the spirit of R9: **the partition the operator visually accepts (S1.5)
is the same object the machine's preferences run on (S2)** — the machine must operate in a
space a human can walk. Then three signals per zone, all read from surfaces that
already exist: **load** (enacted magnitude + attestation summed over the zone — the Figure-5
quantities), **demand** (live-map traffic: saucer/rocket time-in-zone, with clock-lineage for
history), and **posterior variance** (the per-class Beta posteriors, finally fed). G's prior
preference reads load, its ambiguity leg reads variance, demand weights both. A gap class whose
zone holds heavy mass or traffic but no machine capability is the learning target — and the
036 tripwire passes because the zones genuinely differ.

## Slices

### S1 — seeds, membership, and the retro harvest
- Embed the full action-class vocabulary (all classes, not only the current 14 gaps) into BGE
  space; `zone-of` membership fn (high-dim cosine, nearest seed, margin reported).
- Retro-mine the git corpus into capability terms: map commits → action-classes exercised
  (three grains, best-first: attempt items + feature cards where they exist; mission docs +
  pattern references; raw commit paths/messages for the long tail). Write
  `wm-hyperparameter-update` records through the existing `intrinsic_values` machinery — the
  posteriors leave Beta(1,1) on real observation counts.
- Zone seeds recomputed as evidence centroids after the harvest; drift is expected and logged.
- **Acceptance:** posteriors show non-uniform counts across ≥ half the vocabulary; `credit-for`
  returns non-default values; a written honest-coverage note stating what fraction of history
  resisted description (no silent caps).
- *Amendment (2026-07-19, dimensional note):* S1's high-D `zone-of` is **diagnostic-grade**;
  harvest assignments made in raw BGE space are **provisional** and are re-computed in the
  accepted 3-D reduction before any deposit to the live posterior store (the S1 handoff's
  artifact-only deposit gate already enforces this ordering — nothing live is touched until
  after S1.5). Belled to codex-10 2026-07-19 (`invoke-1784453722209-864-0b5d57a9`) under the
  pre-amendment reading; the deliverables survive unchanged, only their finality labels shift.

### S1.5 — the partition made visible before it is used
- Build the **3-D reduction** of the BGE space (deterministic, versioned; method recorded —
  this is a *projection for operability*, distinct from the ground metric `:held` on
  M-substrate-metric). Operative `zone-of` runs in this space.
- Re-assign the S1 harvest in the 3-D partition; the high-D vs 3-D **disagreement set is part
  of the acceptance material** (it is the boundary-distortion diagnostic made concrete).
- Render the zones on the live map (WebArxana layer beside the existing saucers/rocket) — the
  2-D carpet layer is a projection *of the 3-D partition*, not an independent 2-D tessellation.
- *Surface pin (Joe, 2026-07-19):* S1.5 ships the **2-D projection** — that is the acceptance
  surface for today. Navigable 3-D exploration of Futon City (using the `~/vsat/` tooling) is
  a **reach goal, held** for a later excursion; reasonable, but not what is needed to get the
  War Machine running again. Priority order: S1.5 acceptance → S2 discharge of 036/037.
- **Operator visual acceptance is the gate** (Field Desk principle: feature-acceptance before
  machine use). Joe walks the zones; boundary complaints are fixed or recorded as accepted
  distortion BEFORE slice 2 wires anything into G.
- On acceptance, the partition object (seeds + reduction parameters + version) is **frozen**;
  the posterior deposit (re-assigned records) happens against this accepted object.

### S2 — a seat in G (discharges 036/037)
- **Reads only the S1.5-accepted partition object, version-pinned** (R9-spirit invariant: the
  accepted object IS the operative object; no recomputation drift between what Joe walked and
  what G reads).
- Prior preference: zone load enters C for `:learn-action-class` candidates.
- Ambiguity: per-class posterior variance replaces the class-flat term.
- Demand: zone traffic weight (live + lineage-derived).
- Invariants: `selection-discrimination` untouched; `:G-core = risk + ambiguity` untouched
  (new terms enter through the existing augmentation/preference channels with named keys, per
  the B-2a score-provenance discipline); retries=0-style regression — with an empty posterior
  store, behavior is byte-identical to today.
- **Acceptance:** a replayed attempt-036 selection discriminates (distinct-G ≥ 2 among top-5)
  *purely from harvested evidence*; both repair obligations discharge through a normal
  full-loop click with card + executed review + grounding.

### DERIVE (later, empirical — not blocking)
- The attachment-mode hyper-prior: for each historical capability emergence, measure its birth
  adjacency (attached-to-mass vs edge) and correlate with subsequent load-bearingness. If a
  circumstance-conditional pattern exists, it becomes the learned dial; until then the residual
  ant-preference stays neutral. (This is FutonZero R2's corpus doing double duty.)

## Sorries / held elsewhere
- Ground metric + granularity: `:held` on M-substrate-metric (interim = BGE cosine, named).
- R2 (train the prior from outcomes): stays in futon6; this mission only produces the corpus.
- Operator-side capability mining: stays in M-learning-loop; shared vocabulary, no fork.
