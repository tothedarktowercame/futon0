# Stack-fitness completeness contract

*The properties the futon stack must exhibit to honestly carry the
"homeostatic software agent" claim, and which of them it currently
satisfies.*

The stack's M-the-futon-stack mission stakes a load-bearing claim:
that the apparatus we have built is *an agent maintaining itself*,
not a workspace running batch jobs. The claim is evocative; the
vocabulary — AIF loop, free-energy minimisation, invariants,
substrate — can be borrowed by any sufficiently elaborate piece of
software. The criteria below specify, property by property, what the
stack must do to earn the description honestly. Each criterion is
**operationally checkable**: verifiable from substrate-2 queries,
running processes, and observable evidence rather than from the
prose of any single mission doc.

This document serves two purposes:

1. **Specification** — a clear contract any stack-as-agent
   implementation can be measured against. Applies to the futon
   stack today, and would apply to any successor / sibling stack
   making the same claim.
2. **Assessment** — a current-state read on which criteria the
   futon stack satisfies (as of 2026-05-17), with pointers into
   missions, library patterns, and substrate-2 queries.

The criteria parallel the AIF completeness contract in
`~/npt/working-paper/docs/aif-completeness.md` (R1-R12). Where that
contract measures an AIF *implementation* against the Friston / Da
Costa / Parr formulation, this contract measures a *stack-as-agent*
against the homeostat claim. The numbering is independent: criteria
here are F1-F10 (F for *fitness*).

## Homeostasis, not progression

One structural difference from the AIF contract is worth flagging
up front. AIF completeness criteria are *acquired*: once an
implementation has an explicit belief state, it has one — the
criterion is satisfied permanently for that implementation. Fitness
criteria are *maintained*: a stack with current explicit fitness
state can lose it under load (watcher daemon dies, satisficing
signatures stop firing, ΔT goes unbounded). This means **every F-
criterion must be paired with a freshness / trend reading**, not
just a one-shot pass / fail. The contract here defines what fitness
*is*; an honest reading of stack fitness shows current level + trend.

This mirrors the §2.6 trajectory levels in
`~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md`
(stack-fitness arm), where L1-L5 are *fitness levels* not *progress
milestones*.

## The criteria

The first six (F1-F6) are **required** for an honest homeostat
claim. F7 is the validation harness. F8-F10 are extensions for
multi-corpus / multi-domain / hyperparameter operation; they
reserve room for the M-INC step (b) event-vocabulary integration,
the descriptive-essay corpus, and self-tuning of fitness bounds.

### F1 — Explicit fitness state

The stack exposes its geometric state — T (tangent / connectivity),
∇T (gradient), ΔT (laplacian / concentration), drift (Jaccard over
vocabulary fingerprints) — as queryable values per (vertex,
commit). Mean *and* variance / dispersion are explicit; a point
estimate alone is not enough.

**Operational check.** Hit `localhost:7071` `GET /api/alpha/hyperedges?...`
and verify queries return T(v,c), ∇T(e,c), ΔT(v,c), drift(C₁,C₂,c)
deterministically given the substrate at commit c.

**This implementation.** M-LGS COMPLETE 2026-04-28. Substrate-2 live
with ~360,000 hyperedges across 16 labels covering 12 codebases.
Geometric layer derived from typed edges, not stored. Query layer
backed by XTDB at futon1a. **F1 satisfied.**

### F2 — Liveness invariant

The substrate emits change-signals as the world changes. Drift is
detectable; satisficing signatures fire when the typed apparatus
encounters states it can name; the watcher daemon runs continuously
and writes new hyperedges as repos commit.

**Operational check.** Verify the watcher daemon PID is up.
Verify satisficing-zapper has emitted at least one signature in the
last week. Verify drift between two recent commits is non-zero on
at least one cross-prototype pair.

**This implementation.** Multi-watcher running; ~5s latency on
updates. Satisficing-zapper emits 6 of 8 specified signatures (2
closed as not-pain-driven 2026-04-28; flagged in inventory G.3 for
operator review). **F2 satisfied with caveat** — G.3 review pending.

### F3 — Coherent structure

Clusters in T correspond to recognisable structure (active missions
co-locate; related repos sit nearby; cross-prototype geometry
maps to declared parents). Pathological hotspots (mono-culture,
runaway concentration) are distinguishable from healthy
concentration (active work zones).

**Operational check.** Query top-K T-vertices, verify they cluster
in the active mission set. Query the distribution of ΔT
magnitudes, verify it is not uniform. Compare current clusters to
the cluster-leaf set of the VSATARCS anthology (35 stories
clustered from substrate state) — overlap should be substantial.

**This implementation.** VSATARCS anthology IS evidence of F3
(stories were generated from the IDENTIFY-condition mission cluster
tree). No automated coherence check yet; **G.1 (`scan-stack-fitness`)
is the planned implementation.** **F3 partially satisfied;
evidentially present but not yet algorithmically observed.**

### F4 — Bounded self-balance

ΔT is bounded across windows; drift is bounded; the stack shows
allostatic homeostasis under load. The metabolic-balance shape
(10th-shape, M-bounded-in-flight-state) engages when work
accumulates faster than closure: graduated drive states (silent /
advisory / high / stop-the-line) replace silent overload.

**Operational check.** Per-window max |ΔT| stays under a documented
threshold. Run the metabolic-balance check-fn against current
session state; verify it returns either `:ok` or one of the
graduated drive states (not crashing, not silently `:ok` when load
is high).

**This implementation.** Metabolic-balance namespace + check-fn
shipped via M-bounded-in-flight-state INSTANTIATE blocks D-01..D-06
(2026-05-03/04). V-1 calibration tests pass. **|ΔT| bound is not
yet documented** — flagged as F4-gap. **F4 partially satisfied.**

### F5 — Adaptive response

The stack responds to perturbation. New mission opens → geometry
reshapes around the new vertex; new repo wires in → substrate-2
indexes it within one watcher cycle; external surface fails → the
relevant arm's fitness reading degrades visibly. Response time
from perturbation to stabilisation is measurable and bounded.

**Operational check.** Synthesise a small perturbation (e.g. add a
new file to a watched repo, open a fresh mission doc); measure
time-to-substrate-reflection. Expected: < 30 seconds on the watcher
cycle; ΔT response visible at next satisficing-zapper firing.

**This implementation.** Watcher latency ~5s confirmed; perturbation
response not yet measured as a first-class invariant. **F5 partially
satisfied;** measurement infrastructure (scan-stack-fitness) needs
to surface response-time histograms.

### F6 — Operator inhabitation

The operator reads, queries, and acts on the substrate at a
documented cadence. Without F6, F1-F5 run unmonitored and the stack
risks the **inverted hermit trap** — an apparatus humming
autonomously while its operator drifts away from the substrate.
This is the cross-cutting "personal-breakthrough" coordinate of the
inventory's §2.6.

**Operational check.** Stack-HUD session-count over the last week
is non-zero. Operator-authored content (mission-doc edits, library
flexiargs, story-corpus authoring, war bulletins) is non-zero over
the last week. The operator can describe, from memory, the current
top-K |ΔT| / drift hotspots without consulting an LLM.

**This implementation.** Ad-hoc; no canonical metric. The 13 days
between 2026-05-04 and 2026-05-17 (zero futon commits while npt
work consumed attention) is a documented F6-violation. **F6 not
yet operationalised.** Implementing the personal-breakthrough
trajectory reading in `scan-stack-fitness` is the path.

### F7 — Validation harness

The fitness criteria are tested. Tests exist for: the watcher
daemon's liveness, satisficing-zapper firing under known input,
the metabolic-balance check-fn's calibration, the boundedness of
|ΔT| on synthetic perturbations, the deterministic-query property
of substrate-2 (same commit → same answer).

**Operational check.** Run the test suite. Verify F1-F5 each have
at least one test that would fail if the criterion regressed.

**This implementation.** Partial. M-LGS Phase-5 satisficing-zapper
has tests; metabolic-balance has V-1 calibration tests; substrate-2
determinism is tested. Cross-criterion harness (F1-F6 together) not
yet written. **F7 partially satisfied.**

### F8 — Multi-corpus composition (extension)

The substrate composes with multiple corpora — futon stack code,
npt working paper, EoI corpus, descriptive essay drafts — without
privileging any single one. New corpus types register their vertex
+ edge schemas; cross-corpus drift becomes queryable.

**Operational check.** Substrate-2 accepts at least two
distinct-domain corpora (code-substrate + prose-substrate, say).
Cross-corpus queries return meaningful values (not 0 or undefined).

**This implementation.** Substrate-2 covers 12 distinct codebases
via 16 labels; npt working paper + EoI corpus + descriptive essay
not yet integrated. **F8 open;** Phase 2 salt-down sprint (corpus-
registry refactor; generic render scripts) lands the
multi-corpus-shaped infrastructure. Full F8 satisfaction is a
Phase 3 / 4 deliverable.

### F9 — Feed-readable annotation graph (extension)

The substrate's reads compose into a typed annotation graph that
external consumers (War Machine, M-INC posterior projection,
Director EoIs) can pull from. State transitions on annotation
entities are first-class events under the M-INC vocabulary v1
(state/spawned, state/refined, state/strengthened, state/addressed,
state/falsified, state/foreclosed, state/reopened, link/asserted).

**Operational check.** A War Machine query returns at least one
fireable-move recommendation grounded in the annotation graph,
with provenance + EFE score + dispatch metadata.

**This implementation.** Open. **Hard dependency: M-INC step (b)
event vocabulary committed in code (codex-7-owned).** Until step
(b) lands, F9 cannot be checked. Phase 4 of the descriptive-essay
roadmap is the F9-satisfying work.

### F10 — Dual-loop fitness (extension)

The fitness substrate observes its own parameters — the bounds on
|ΔT|, the latency targets, the drain-channel thresholds — and
adjusts them when warranted. The substrate's substrate. Analogue
of AIF's R12 (hyperparameter inference) at the homeostat level.

**Operational check.** Locate the parameter store (where are the
F4 |ΔT| bounds, the F5 perturbation-response timeouts, the F2
satisficing-signature liveness windows declared?). Verify the
stack can revise these parameters under evidence, not by edict.

**This implementation.** Open. This corresponds to
M-the-futon-stack Q6 (debug / unplug if off the rails) and is the
deepest reflexive-discipline target. Not on near-term roadmap.

## Cross-references

- `~/code/futon0/holes/missions/M-the-futon-stack.md` — Q1-Q6 are
  the questions these criteria formalise. F1-F6 ≈ Q1 + Q4 + Q5;
  F7 ≈ Q2 (cognitive function vs bookkeeping); F8-F10 reserve
  future verification surface; Q3 (efficiency / thermodynamic
  constraint) cross-cuts F1-F5 and is not yet a separate
  criterion.
- `~/code/futon3/holes/missions/M-live-geometric-stack.md` — the
  substrate-2 mission that ships F1, F2, F4 (partial), F8 (12
  codebases), F10 readiness.
- `~/code/futon3/library/invariant-coherence/metabolic-balance.flexiarg`
  + siblings — pattern artefacts exemplifying F4.
- `~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md`
  §2.6 — L1-L5 trajectory levels with operator-facing failure-mode
  rubric; F1-F5 + F6 = the same content under audit-criterion
  framing rather than trajectory-level framing.
- `~/npt/working-paper/docs/aif-completeness.md` — R1-R12 sibling
  contract. Same template, AIF-implementation scope rather than
  stack-fitness scope.

## Implementation receipts (summary)

| Criterion | Status | Pointer |
|---|---|---|
| F1 — Explicit fitness state | ✓ satisfied | M-LGS, substrate-2 query layer |
| F2 — Liveness invariant | ✓ with caveat | multi-watcher live; 6/8 signatures (G.3 review) |
| F3 — Coherent structure | ⚠ partial | VSATARCS evidence; G.1 to operationalise |
| F4 — Bounded self-balance | ⚠ partial | metabolic-balance shipped; |ΔT| bound undocumented |
| F5 — Adaptive response | ⚠ partial | watcher latency ok; response-time not first-class |
| F6 — Operator inhabitation | ✗ open | no canonical metric; 2026-05-04..17 gap is documented violation |
| F7 — Validation harness | ⚠ partial | per-component tests exist; cross-criterion harness missing |
| F8 — Multi-corpus composition | ✗ open | Phase 2 salt-down sprint lands the infrastructure |
| F9 — Feed-readable annotation graph | ✗ open | hard-dep on M-INC step (b) |
| F10 — Dual-loop fitness | ✗ open | M-the-futon-stack Q6; not on near-term roadmap |

## Status of this document

Drafted 2026-05-17 as **session task S3** of
`~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md`
§4.5. Companion to the descriptive-essay roadmap's Phase 2 salt-
down work: where the npt R1-R12 contract drove AIF-completeness
review for the working paper, this F1-F10 contract drives stack-
fitness review for the futon-stack-as-Hyperreal-offer essay.

Versioning: this is v0.1. Expected refinements:
- v0.2 after `scan-stack-fitness` (G.1) runs and surfaces actual
  values against F3-F5
- v0.3 after M-INC step (b) lands and F9 becomes checkable
- v1.0 once F1-F6 are all ✓ or have documented exceptions
