# Mission: APM Capability Ratchet — From Solved Proofs to Transferable Mathematical Capability

**Date:** 2026-07-22
**Status:** MAP — initial capability-topology map added 2026-07-22
**Owner:** Joe + agents
**Mission home:** `futon0`
**Initial problem corpus:** `apm-lean/problems/`
**Cross-ref:**
  - `M-futonzero-capability.md` (completed — observes persistent capability)
  - `M-futonzero-prelim-practice.md` (proposed — Joe practises mathematics)
  - `M-futonzero-generative.md` (chartered — stack-wide policy learning)
  - `M-capability-star-map.md` (completed — capability/mission distinction and graph)
  - `M-capability-zones.md` (14 action-class zones; `apm-lean` is a named demonstration stratum)
  - `futon3c/holes/missions/M-apm-solutions.md` (APM expository proof pipeline)
  - `futon5a/holes/missions/M-learning-loop.md` (mined capability graph and WebArxana projection)
  - `futon5a/holes/excursions/E-capability-graph-ui-polish.md` (operator-gated graph UI follow-up)
  - `futon6/README-mentor.md` (mathematical strategy and failure patterns)
  - `futon6/holes/missions/M-hyperreal-dictionary-planning.md` (formalisation and teaching-unit packets)
  - `apm-lean/docs/apm-problem-bundle-spec.md` (canonical APM problem bundles)

## 0. HEAD

The APM pipeline can produce informal solutions, Lean formalizations, proof
outlines, failed routes, and status evidence. Closing another proof is a useful
functioning, but it is not by itself evidence that the system has acquired a
reusable mathematical capability.

This mission builds the missing ratchet:

```text
solve a problem
  → reconstruct the mathematical decisions
  → distil a candidate capability packet
  → render it as both solver guidance and teaching guidance
  → test it on sealed transfer problems
  → promote, revise, split, or reject it from evidence
```

The motivating claim is deliberately stronger than “retrieval helps” and
weaker than “the model trained itself”:

> Mathematical work expands system capability when a reusable, inspectable
> artifact measurably improves recognition, strategy selection, explanation,
> or proof completion on problems other than the one that produced it.

The model weights do not change. The capability accumulates in external,
replayable substrate: mathematical schemas, formal lemmas, recognition cues,
failure checks, teaching tactics, and evidence-backed selection policy.

## 1. IDENTIFY

### 1.1 The current gap

The present APM bundle records *a problem* well:

- canonical statement and provenance;
- informal solution;
- formalization-oriented proof outline;
- Lean source and remaining `sorry` boundaries;
- validation and classification metadata.

What it does not yet have to record is *what became reusable because this
problem was solved*. Consequently:

- the next solver can retrieve an old solution without learning when its move
  applies;
- a successful shortcut can remain trapped in one `Main.lean`;
- a failed route can be documented without becoming a reusable diagnostic;
- a clear explanation can be mistaken for evidence of transfer;
- repeated completion can look like capability growth even when every new
  problem still requires fresh search.

This is the mathematical instance of the distinction in
`M-futonzero-capability`: a one-off functioning is not yet a persistent freedom
to achieve that class of result.

### 1.2 Why tutoring is the right dual

A solver must recognize a proof state and choose a productive next move. A
tutor must recognize a learner state and choose the smallest intervention that
helps the learner make that move. Both require the same underlying structure:

- prerequisites;
- recognition cues;
- applicability conditions;
- a move and its expected effect;
- common false starts;
- counterexamples and scope boundaries;
- progressively stronger hints;
- nearby problems that distinguish imitation from transfer.

The tutor is therefore not a separate chatbot or prose generator. It is a
consumer and an assay of the same capability packets used by the solver. If a
packet cannot support misconception diagnosis and graded hints, that is
evidence that its strategy model may be too shallow. If it can teach the source
solution but cannot improve a held-out attempt, it has not demonstrated
capability.

### 1.3 Sibling boundary

| Mission | Primary subject | This mission does not replace it |
|---|---|---|
| `M-futonzero-capability` | Observe persistent capabilities in stack evidence | This mission supplies a math-specific capability object and witness |
| `M-futonzero-prelim-practice` | Develop and measure Joe's mathematical learning | This mission evaluates the packet before a human curriculum consumes it |
| `M-futonzero-generative` | Learn stack-wide policy/value from warranted play-outs | This mission is a bounded, domain-specific ratchet with no claim of weight or reward learning |
| `M-apm-solutions` | Produce expository solutions and formalizations | This mission starts from those proof episodes and tests what transfers |
| `M-hyperreal-dictionary-planning` | Build a corpus substrate and packetization path | This mission defines and tests one concrete packet class over APM |

## 2. MAP — placement in the existing capability topology

The stack now has two concrete capability maps relevant to this mission. They
answer different questions and must not be collapsed merely because both use
the word “capability.”

### 2.1 Capability zones: where capability-building work happens

`M-capability-zones` partitions Futon City using the 14 action types recognized
by the War Machine forward model:

```text
no-op                 address-sorry       open-mission
advance-mission       close               close-mission
close-hole            survey              survey-mission
apply-cascade         fire-pattern        learn-action-class
pursue                decompose
```

The accepted and frozen partition artifact is the version-pinned `pca3-v1`
Voronoi partition; its live deposit and S2 use remain separately Joe-gated
under `M-capability-zones`. It is a partition of *actions and evidence about
work*, not a taxonomy of mathematical subjects or proof techniques. In
particular:

- `address-sorry` describes work on a concrete proof or warrant gap; it does
  not name the mathematical capability acquired by closing that gap.
- `fire-pattern` describes applying an already warranted reusable method; it
  must not be used to claim that a newly proposed packet is already a pattern.
- `learn-action-class` describes building missing support for a War Machine
  action class; it is not a generic label for every episode in which an agent
  learns some mathematics.
- `decompose`, `survey`, and `close-hole` can describe ratchet-cycle work while
  remaining silent about whether transfer has been demonstrated.

This distinction gives the ratchet two coordinates:

| Ratchet event | Candidate action-zone evidence | Capability claim permitted? |
|---|---|---|
| Inspect a proof episode and its failed routes | `survey`, sometimes `survey-mission` | No |
| Isolate a reusable mathematical move and its obligations | `decompose` | Candidate only |
| Close a source Lean hole | `address-sorry`, `close-hole` | Functioning only |
| Apply an already promoted packet to a new problem | `fire-pattern` if the trace warrants it | Evidence toward persistence |
| Build missing harness support | `advance-mission`; `learn-action-class` only when the WM action vocabulary itself is extended | No mathematical promotion by itself |
| Complete a sealed transfer gate | `close-hole` or `close` for that exact gate | Promotion only through the packet evaluator |

These labels are hypotheses until assigned by the accepted zone machinery.
The mission must not hand-label `a94J05` history to manufacture a favourable
zone trajectory.

The strongest direct connection is already present in the capability-zones
charter: `apm-lean`, DarkTower, and other outward work are a named
**demonstration stratum**, deliberately distinct from the FUTON construction
corpus. APM proofs show capability exercised in foreign mathematical territory;
they are not primarily loci where the stack's infrastructure is built. The
ratchet can therefore supply unusually precise demonstration evidence:

- source problem and checked proof;
- packet version allegedly exercised;
- accessible hints and conversion factors;
- transfer result and failure mode;
- formal validation artifacts.

Harvesting or depositing that stratum remains owned by `M-capability-zones`.
This mission supplies typed evidence and consumes the accepted partition; it
does not recompute the 14 seeds, fork `pca3-v1`, or write directly to the live
posterior store.

### 2.2 Capability graph: what reusable capability is evidenced

`M-learning-loop` has separately materialized a capability graph in Futon1b and
renders it through WebArxana at `/live/capability-graph.html`. Its current
vocabulary is mined from operator hinge-log evidence, and its graph distinguishes:

- capability entities;
- operator-turn evidence;
- canonical mission entities;
- `belongs-to-mission` relations;
- `evidences-capability` relations;
- `develops-capability` relations;
- witness-bearing `shares-capability-with` relations.

That graph is the natural family of surface for mathematical capability
packets. It can show that several apparently different problems exercise a
shared move and let a reader inspect the actual proof/evaluation witnesses.
However, the current ingestor is intentionally narrower: it resolves canonical
mission endpoints for its existing mining corpus and should not be tricked into
accepting `apm-lean` problem bundles as operator turns or FUTON mission nodes.

MAP therefore proposes an explicit extension seam rather than an immediate
write:

```text
APM problem bundle ──checked-proof──▶ proof episode
       │                                  │
       │                                  └──evidences──▶ capability packet@version
       │                                                       │
       └──transfer-probe/result─────────────────────────────────┘

ratchet mission ──develops──▶ candidate packet
held-out problem A ◀──shared demonstrated capability──▶ held-out problem B
```

The diagram names semantic requirements, not approved Futon1b relation types.
Before implementation, the graph owner must decide whether existing
`evidences-capability`, `develops-capability`, and
`shares-capability-with` relations can be generalized without semantic drift,
or whether a separate typed adapter is required. Candidate and promoted packet
states must remain visibly distinct, and every displayed transfer connection
must retain its run and proof witnesses.

### 2.3 Connection to `E-capability-graph-ui-polish`

`E-capability-graph-ui-polish` is an operator-gated interactive walkthrough of
the accepted WebArxana prototype. It is not a dispatchable implementation
parcel, and this mission must not silently add work to it. Its four checks are,
however, direct acceptance requirements for any later mathematical packet
projection:

1. **Interaction sweep:** packet, problem, proof, and evidence nodes have
   legible drag/zoom/pan/hover behaviour and complete tooltips.
2. **Voice legibility:** operator-voiced versus agent-only evidence remains
   visible. Packet promotion status must use a separate encoding; it must not
   overload the existing ink-ring voice signal.
3. **Edge readability:** weak evidence, capability development, and witnessed
   transfer connections remain distinguishable without relying only on colour
   or the legend.
4. **Table↔graph mirror:** packet version, status, provenance, evaluation
   verdict, and witness references are available outside hover and colour.

The useful connection is a future bounded walkthrough item or parcel produced
*after* the capability graph has an approved mathematical adapter. UI polish
cannot stand in for the adapter, graph semantics, or transfer evidence.

### 2.4 MAP deliverable and open questions

The MAP output for the first slice should be a small, machine-readable
crosswalk, not another embedding model:

```clojure
{:schema :apm-capability-topology-map.v1
 :packet-id :complex-analysis/nonvanishing-transform
 :packet-version 1
 :source-problems ["a94J05"]
 :zone-evidence [{:event-id "..." :partition "pca3-v1" :class :survey ...}]
 :graph-attachment {:state :proposed
                    :entity-kind :mathematical-capability-packet
                    :evidence-refs [...]}
 :demonstration-stratum true}
```

MAP must answer:

1. What is the canonical entity identity for a packet version, proof episode,
   transfer probe, and evaluation run?
2. Which existing graph relations retain their meaning for mathematics, and
   which proposed relationships need new typed contracts?
3. How does candidate→tested→promoted status appear without laundering a
   negative or partial result?
4. How are `pca3-v1` zone assignments attached as evidence without treating
   the 14 action classes as mathematical capabilities?
5. Which graph/UI projection is useful before a second independent problem has
   exercised the packet?
6. What operator judgment is genuinely needed, and what can be validated
   mechanically before invoking the UI walkthrough?

## 3. Mission result

Deliver a local, evidence-backed harness that turns selected APM proof episodes
into candidate mathematical capability packets and evaluates whether those
packets improve performance on unseen problems.

The first complete vertical slice must provide:

1. a machine-readable capability-packet schema and validator;
2. one packet distilled from a completed APM problem;
3. solver and tutor projections generated from the same packet;
4. a sealed transfer-probe set with positive, contrastive, and boundary cases;
5. baseline, raw-solution-retrieval, and capability-packet evaluation runs;
6. a promotion report that either promotes the packet or honestly refuses it.

The first slice is intentionally one capability, not an ontology of all
mathematics.

## 4. Capability packet contract

The initial shape should be expressible in EDN and versioned. Exact field names
belong to DERIVE, but the semantic contract is:

```clojure
{:capability/id :complex-analysis/nonvanishing-transform
 :capability/version 1
 :capability/status :candidate

 :claim
 {:can-do "Recognize and execute a nonvanishing-transform constancy proof"
  :not-claiming ["model-weight learning" "closure of all related problems"]}

 :recognition
 {:cues [...]
  :anti-cues [...]
  :preconditions [...]
  :target-shape [...]}

 :strategy
 {:move "..."
  :subgoals [...]
  :alternatives [...]
  :failure-modes [...]
  :kill-conditions [...]}

 :teaching
 {:diagnostic-questions [...]
  :misconceptions [...]
  :hint-ladder [...]
  :contrastive-examples [...]}

 :formal
 {:lean-theorems [...]
  :mathlib-interfaces [...]
  :checked-artifacts [...]}

 :provenance
 {:source-problems [...]
  :proof-cycles [...]
  :failed-routes [...]}

 :evaluation
 {:probe-set-id "..."
  :conditions [:baseline :raw-solution :capability-packet]
  :runs [...]
  :promotion-verdict :pending}}
```

### 4.1 Packet invariants

1. **Problem ≠ capability.** A packet names an ability and its applicability
   contract, not merely the source theorem or proof text.
2. **Functioning ≠ capability.** Source-problem success may create a candidate;
   only transfer evidence may promote it.
3. **Explanation ≠ evidence.** Fluency, length, or pedagogical polish cannot
   substitute for held-out performance.
4. **No solution leakage.** Transfer runs may see the promoted packet but not
   source-specific solution text, hidden answers, or prior traces for the probe.
5. **Failure is retained.** Dead routes and boundary cases are append-only
   evidence and must influence recognition or diagnostic checks.
6. **Formal/informal alignment.** A formal recipe must name the mathematical
   move it implements; informal exposition must not claim more than the checked
   theorem establishes.
7. **One substrate, two projections.** Solver guidance and tutor guidance must
   be traceable to the same packet version.
8. **No premature abstraction.** A general Lean lemma is promoted only when its
   statement is stable and at least one transfer case exercises it; otherwise
   retain a checked recipe rather than inventing an unused library API.
9. **No hidden capability claim.** Every `:tested` or `:promoted` status points
   to replayable evaluation evidence.

## 5. The ratchet cycle

### 5.1 Observe the proof episode

Read the complete bundle and proof evidence, including:

- the canonical statement;
- the final informal and formal proofs;
- the original `sorry` boundary or blocker;
- approaches considered or abandoned;
- theorem/API searches;
- validation artifacts;
- any difference between the planned and successful proof.

The useful datum is often the strategy change. A proof that closed by a route
different from its outline has generated prediction error worth preserving.

### 5.2 Distil a candidate

Reconstruct:

- what surface features should trigger the strategy;
- what invariant or theorem actually makes it work;
- which hypotheses are load-bearing;
- what nearby approach is tempting but inferior or false;
- what portion is mathematical and what portion is Lean/Mathlib plumbing;
- what more general schema, if any, survives removal of source-specific names.

Distillation may initially be human/agent-authored. An automated extractor is
not a prerequisite for the first slice; deterministic validation and explicit
provenance are.

### 5.3 Adversarialize the packet

Before evaluation, construct:

- positive near-transfer probes;
- at least one cross-surface probe using different notation or theorem order;
- a boundary case where a precondition fails;
- a plausible distractor strategy;
- a seeded learner/solver error for diagnosis.

Probe statements and rubrics must be frozen before the evaluated runs. Where a
probe is formal, Lean validates its statement and any reference proof, but the
reference proof remains sealed from the evaluated agent.

### 5.4 Evaluate three conditions

For the same model/tool envelope and fixed budget, compare:

1. **Baseline:** no source solution and no capability packet.
2. **Raw retrieval:** source solution or ordinary semantic retrieval is
   available.
3. **Capability packet:** the packet is available, while source-specific
   solution material remains unavailable unless independently retrieved under
   the same declared policy.

The harness records more than closure:

- strategy selected before substantial proof search;
- correctness of applicability and hypothesis checks;
- proof completion or partial progress;
- hints, retries, tokens, wall time, and tool calls;
- repeated failure patterns;
- misconception diagnosis quality;
- whether the resulting proof uses the capability or closes accidentally by an
  unrelated route.

### 5.5 Promote, revise, split, or reject

- **Promote** when evidence shows transfer beyond the source problem and the
  packet's scope prediction is calibrated.
- **Revise** when the move transfers but recognition, teaching, or formal
  interfaces are weak.
- **Split** when one packet hides two independently useful capabilities.
- **Reject** when raw retrieval performs as well, probes reveal leakage, or the
  abstraction does not survive the source problem.

Rejection is a useful result. It prevents a polished one-off solution from
being laundered into the capability graph.

## 6. First vertical slice: `a94J05`

The initial candidate comes from `apm-lean/problems/a94J05`, whose completed
part (b) proves that an entire function with real part bounded below is
constant.

The source proof episode contains a useful strategy revision:

- original route: periodicity of complex exponential plus continuity/IVT over
  the discrete set `2πiℤ`;
- successful route: bounded `exp(-f)` is constant by Liouville; differentiate
  that identity; use nonvanishing of exponential to force `f' = 0`.

Candidate capability:

> **Nonvanishing-transform constancy.** When a transform turns the available
> bound into a Liouville-bounded entire function, and the transform derivative
> is nonzero on the relevant range, constancy of the transform can be pulled
> back by differentiation.

The slice should test whether the packet helps with variants such as:

- reversing the one-sided real-part bound;
- replacing real-part information with imaginary-part information via a
  justified rotation;
- recognizing when the transform derivative may vanish and cancellation is
  invalid;
- recovering the same strategy from unfamiliar notation;
- explaining to a learner why exponential periodicity is true but needlessly
  expensive for this proof.

These are candidate probe families, not yet a frozen test set. DERIVE must
produce precise statements and verify that they are neither duplicates nor
false generalizations.

## 7. Ownership and wiring

This mission must compose with the current stack rather than create a parallel
mathematics database.

### `futon0` owns

- the capability-packet schema and validator;
- evaluation condition definitions;
- sealed-run orchestration at the capability-measurement layer;
- promotion reports and capability trajectories;
- solver/tutor projection comparison.

### `apm-lean` owns

- canonical APM problem bundles;
- Lean statements, proofs, and validation artifacts;
- bundle-local links declaring which packet versions a problem produced, used,
  challenged, or falsified;
- APM-derived packet content during the first bounded slice, unless a later
  promotion explicitly moves a generic mathematical pattern to its canonical
  library home.

### `futon3c` supplies, without duplication

- proof cycle and failed-route evidence;
- visible agent routing and bounded execution;
- Mentor interventions and structural proof checks;
- evidence identities needed to replay an evaluation.

### `futon6` supplies, without duplication

- mathematical strategy patterns and Mentor vocabulary;
- corpus/hypergraph context where available;
- the future generic formalization/teaching-unit packet seam.

Cross-repo artifacts must carry stable IDs and provenance. Mission-home means
that this charter and harness are governed from `futon0`; it does not transfer
ownership of APM proofs away from `apm-lean`.

## 8. Scope

### In scope

- One versioned capability-packet schema.
- One `a94J05`-derived vertical slice.
- Deterministic packet validation.
- A small, sealed transfer suite.
- Solver and tutor projections from one packet.
- Baseline/raw-retrieval/packet comparison.
- Promotion evidence consumable by FutonZero and the capability star map.
- A documented procedure for distilling later APM proof episodes.

### Out of scope

- Ingesting or reproducing the Berkeley Problems in Mathematics corpus.
- Claiming broad mathematical competence from one packet.
- Training or fine-tuning model weights.
- Autonomous self-play or reward learning.
- A general-purpose tutoring UI.
- Replacing Mathlib search, LeanDojo, the proof peripheral, Arxana, or the
  Hyperreal Dictionary.
- Automatically promoting every closed proof.
- Building a universal mathematical ontology before the first transfer result.

The harness is source-agnostic: BPM, another textbook, or newly authored
problems can become later probe sources when provenance and use rights are
settled, but no published corpus is needed to prove the ratchet works.

## 9. Gate contract

- [ ] **G5 — Capability specification:** packet semantics, status transitions,
  and the distinction between functioning and capability are explicit.
- [ ] **G4 — Evaluation authorization:** model/tool envelopes, accessible
  artifacts, budgets, and sealed materials are declared per run.
- [ ] **GF — Fidelity:** problem statements, formal theorem meanings, proof
  provenance, and existing mission ownership are preserved.
- [ ] **G3 — Pattern reference:** distillation names the proof/Mentor patterns
  it uses and records alternatives considered.
- [ ] **G2 — Execution:** schema validator, first packet, projections, and
  evaluation runner exist.
- [ ] **GD — Documentation:** a human-readable packet and tutor view explain
  the capability without requiring the source proof transcript.
- [ ] **G1 — Validation:** sealed probes run in all three conditions and produce
  a promotion verdict from predeclared criteria.
- [ ] **G0 — Evidence durability:** packet version, run manifests, outputs,
  scores, and verdict are append-only or content-addressed and replayable.

## 10. Success and falsification criteria

### Success for the first slice

1. The packet validator rejects malformed provenance, unsupported promotion,
   and missing applicability boundaries.
2. The `a94J05` packet is understandable without opening its source solution,
   while retaining links back to the checked proof.
3. The three-condition comparison produces discriminating evidence; promotion
   is forbidden unless at least one held-out positive probe shows better
   strategy recognition or proof progress under the packet condition than
   baseline.
4. At least one boundary probe is correctly refused or qualified.
5. The tutor projection diagnoses a seeded error and emits a hint that does not
   reveal the full answer immediately.
6. Re-running the evaluation from its manifest reproduces the scored artifacts.
7. The promotion report can honestly return any of `promote`, `revise`, `split`,
   or `reject`.

### Falsification conditions

The initial design is refuted or must be revised if:

- packet gains disappear when source-solution leakage is removed;
- raw solution retrieval matches or beats the packet on every transfer metric;
- evaluators cannot agree whether a run used the claimed strategy;
- the supposed capability cannot be stated without naming `a94J05` particulars;
- tutor quality and solver utility require divergent, contradictory packet
  contents rather than projections over one substrate;
- evaluation cost exceeds ordinary proof search without yielding diagnostic or
  transfer information;
- promotion status cannot be reconstructed from durable evidence.

## 11. Expected follow-on if the slice works

Only after the first packet survives transfer:

1. Distil a small portfolio across analysis, algebra, topology, and functional
   analysis.
2. Add packet relations: `requires`, `generalizes`, `contrasts-with`,
   `implemented-by`, `diagnoses`, and `evidenced-by`.
3. Test whether selecting APM work by expected capability gain produces more
   transfer than selecting only by low `sorry` count.
4. Feed promoted packets into `M-futonzero-prelim-practice` as teaching units
   and compare predicted misconceptions with Joe's actual stuck points.
5. Expose promoted, witnessed capability nodes to the capability star map;
   candidates remain visibly unpromoted.
6. Decide whether generic mathematical packets belong in the existing pattern
   library or in the future Hyperreal Dictionary substrate. Do not fork either
   ontology speculatively.

## 12. Remaining work

These checkboxes are the charter's machine-visible open state.

- [x] Locate the ratchet relative to the 14 accepted action-class zones and
  preserve the construction-corpus/demonstration-stratum distinction.
- [x] Identify the existing capability-graph and
  `E-capability-graph-ui-polish` as the projection family and operator-gated UI
  acceptance surface, without claiming an APM adapter already exists.
- [ ] DERIVE `apm-capability-topology-map.v1`, including stable identities and
  the zone/graph non-conflation invariants.
- [ ] Agree the mathematical adapter boundary with the capability-graph owner;
  do not extend the live ingestor opportunistically.
- [ ] Produce the first `a94J05` topology crosswalk using accepted `pca3-v1`
  assignments rather than hand labels.
- [ ] Audit existing task/run/evidence schemas before defining packet and run
  manifests.
- [ ] DERIVE `mathematical-capability-packet.v1` and its status-transition
  rules.
- [ ] Decide the initial APM-side packet path and cross-repo stable-ID shape.
- [ ] Implement a deterministic schema validator with negative fixtures.
- [ ] Distil the `a94J05` candidate packet from the checked bundle and proof
  revision.
- [ ] Freeze and validate the first transfer probes and sealed reference
  artifacts.
- [ ] Define scoring rubrics and promotion thresholds before evaluated runs.
- [ ] Implement baseline, raw-retrieval, and packet run manifests.
- [ ] Implement solver and tutor projections over the same packet version.
- [ ] Run the first comparison and publish the evidence-backed verdict.
- [ ] Register a capability-star-map node only if the verdict warrants it.

## 13. Current disposition

This mission has entered MAP but is not realized. The immediate next move is
the bounded `a94J05` topology crosswalk and packet-and-probe slice, beginning
with identity/schema audit and probe design.
It is not a mandate to grind the remaining APM `sorry` backlog, build a general
tutor, or declare the stack mathematically self-improving.

The mission succeeds when one completed proof leaves behind a tested freedom
to solve or teach a class of later problems—not when it merely leaves behind a
better description of the proof already completed.
