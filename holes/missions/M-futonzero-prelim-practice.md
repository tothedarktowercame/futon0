# Mission: FutonZero Prelim Practice — Joe Learns Graduate Mathematics

**Date:** 2026-03-29
**Status:** IDENTIFY (mission proposal)
**Blocked by:** M-apm-solutions pipeline run (in progress)
**Cross-ref:**
  - `futon0/holes/missions/M-futonzero-capability.md` (completed — the observer)
  - `futon0/holes/missions/M-futonzero-mvp.md` (tabled — the tutor/coach design)
  - `futon3c/holes/missions/M-apm-solutions.md` (in progress — the 490-problem pipeline)
  - `apm-lean/` (canary proofs, pipeline scripts, tutoring report)

## 1. Motivation

Joe was a PhD student at UT Austin's mathematics department for 2.5 years.
He spent most of that time building AI for mathematics (PlanetMath,
peeragogy, what became the FUTON stack) rather than attending classes.
The preliminary exam problems in `~/code/storage/apm/` are from his own
department. He knows roughly strong-undergrad-level mathematics.

The M-apm-solutions pipeline is producing, for each of 490 problems:
- An informal solution with the futonic pedagogical rotation (Setup /
  Key Insight / Proof / Where You'd Get Stuck)
- A Lean 4 formalisation (closed, partial, or timed out)
- Evidence (timing, sorry boundaries, difficulty profile)
- Retrospectives every 10 problems (pattern recurrence, QP distribution,
  Mathlib boundary map)

This corpus is a *teaching resource*, not a *reference manual*. The
pipeline produces the material. This mission uses the material to
actually teach Joe graduate mathematics — closing the loop that was
left open in 2003 when he left Austin without passing prelims.

### 1.1 Why not just read the solutions?

Reading 490 solutions in sequence teaches you nothing. You remember
what you did, not what you read. The M-futonzero-mvp mission (tabled)
designed a spaced-repetition practice loop for exactly this reason:
capability growth requires active retrieval, not passive consumption.

The difference now: the MVP was designed before the three-column stack
existed and before the 490-problem corpus was available. With both in
place, the practice loop has something real to observe (the self-
representing stack) and something real to practice on (the corpus).

### 1.2 What this adds to M-futonzero-capability

The completed FutonZero capability monitor observes agent capability
trajectories across the stack. This mission extends it to observe
*Joe's* mathematical capability trajectory — a human learner using AI-
generated teaching materials under the same futonic discipline that
produced them. The monitor doesn't change; the domain changes.

## 2. Design

### 2.1 The practice session

A practice session is a timed, structured encounter with one problem:

1. **Read the problem** (2 min). No hints, no solution visible.
2. **Attempt a proof sketch** (10 min). Write an informal argument.
   Record dead ends and stuck points. This is the *generate* phase —
   active retrieval, not passive reading.
3. **Compare with the tutored solution** (5 min). Read the pipeline's
   Observe/Propose/Execute/Validate output. Note: where was your
   approach the same? Where did you diverge? Where did the "Where
   You'd Get Stuck" section predict your actual stuck point?
4. **Review the Lean proof** (3 min). Read the formal version. Note
   which Mathlib lemmas the proof uses — these are the tools you
   didn't know about (or did).
5. **Record evidence** (2 min). Rate: (a) did you get the key insight?
   (b) did you find the right tool? (c) was the proof complete?
   (d) what did you learn that you didn't know before?

Total: ~22 minutes per problem. Three problems per hour. A 2-hour
evening session covers 5-6 problems.

### 2.2 The scheduling policy

Adapted from the tabled MVP's "pachinko" loop, simplified:

- **Due problems** (previously seen, interval elapsed): retry from
  scratch. The interval grows with success (spaced repetition).
- **New problems**: selected by subject to maintain balance across
  topology, analysis, algebra, and functional analysis.
- **Edge problems**: problems whose pipeline evidence shows high
  difficulty (C2-profile, many sorry) — presented when the learner
  has demonstrated readiness in that subject.
- **Probe problems**: problems from subjects the learner hasn't
  touched recently — tests transfer and retention.

The scheduler reads:
- Joe's practice evidence (from step 5)
- The pipeline evidence (difficulty profile, sorry count, subject)
- The retrospective observations (which patterns recur, which
  techniques transfer across subjects)

### 2.3 The capability trajectory

FutonZero tracks, per subject:

- **Tool recognition rate**: did Joe identify the right tool (Fatou,
  Closed Graph, Cauchy-Schwarz, etc.) before seeing the solution?
- **Key-insight rate**: did Joe find the key insight independently?
- **Dead-end quality**: are Joe's dead ends the same ones the pipeline
  predicted, or novel ones? Novel dead ends are interesting data.
- **Lean reading fluency**: can Joe read the Lean proof and identify
  which step corresponds to which part of the informal argument?
- **Transfer**: does solving a Cauchy-Schwarz problem in analysis
  improve performance on a Hölder problem in functional analysis?

The trajectory is the primary output — not a score, but a narrative
of capability expansion over time, in the Sen sense: what can Joe do
now that he couldn't do before, and what conditions enabled it?

### 2.4 The feedback loop to M-apm-solutions

Practice sessions generate evidence that feeds back to the pipeline:

- If Joe consistently gets stuck on a specific tool (e.g., uniform
  integrability), the retrospective can flag this and the next batch
  of solutions can spend more time explaining it.
- If a "Where You'd Get Stuck" section fails to predict Joe's actual
  stuck point, that's a QP-pattern gap — a new dead end worth naming.
- If a Lean proof's sorry boundary corresponds to a concept Joe also
  doesn't understand, that's a Mathlib gap that's also a pedagogy gap.

The pipeline teaches; the practice measures; the measurement improves
the teaching. This is the return loop that M-futonzero-capability was
designed to observe.

## Checkpoint 0 — Distilling the Excursions (2026-04-09)

**Status:** Open for Joe to inhabit.

The excursions in `futon0/analysis/excursions/` are the empirical ground
for this mission. Before IDENTIFY can be honest, the excursion experience
needs to be distilled — not by an agent reading files, but by Joe
reflecting on what he actually learned.

### Questions for Joe (the Hīnayāna peripheral)

These can't be answered by grepping. They require sitting with the
experience and noticing what changed.

1. **What did you learn from the excursions that you didn't know before
   starting them?** Not what the files say — what shifted in your
   understanding of the stack, of mathematics, of your own learning?

2. **Where did you get stuck during excursions?** The sorry-initialization
   excursion identifies "spaces that would tell us what the topology is,
   if we inhabited them." Where were YOUR sorrys — the places where you
   committed to a shape you couldn't fill?

3. **What teaching tactics would have helped?** Looking back at stuck
   points: was it `what-theorem?` (you didn't know the right tool) or
   `where-stuck?` (you couldn't articulate the gap) or `why-this-lemma?`
   (you knew the tool but not why it applied)?

4. **What transferred?** The sorry ontology says excursions make sorrys
   legible. Did solving one problem make another problem more readable?
   Did understanding a futon module help with a math concept, or vice
   versa?

5. **What's the sorry in your mathematical knowledge that this mission
   should close?** Not "pass the prelims" — what specific gap, if
   closed, would have the highest cross-problem impact on your
   capability trajectory? (This is the Bayesian model question from
   M-diagramprover, applied to Joe instead of Lean.)

### What the checkpoint will contain (once Joe answers)

- A distilled list of learnings from excursions E-sorry-initialization
  through E-Vasilopita
- Joe's self-assessed capability map (which techniques are solid, which
  are sorry, which are dark)
- The first data points for the Bayesian learner model
- A revised scope for the practice sessions based on actual experience,
  not projected experience

### Why this must be interactive

The excursion insight (E-sorry-initialization §"The Claim"): property 5
(inhabitation rate) "can only be discovered by inhabiting the space."
A sorry in an uninhabited peripheral is epistemically opaque. The same
applies here: Joe's mathematical sorrys are opaque until he inhabits
them through practice. An agent reading the excursion files sees the
structure but not the experience. The checkpoint requires the experience.

This is the Hīnayāna entry: small vehicle, one person, direct practice.
The Mahāyāna generalisation (MathDojo for all students) comes after.

---

## 3. Prerequisites

### 3.1 From M-apm-solutions (in progress)

- At least 50 problems with complete solutions (pipeline batch 1-5)
- At least 5 retrospectives
- Evidence files for all attempted problems

### 3.2 From M-futonzero-capability (complete)

- The capability monitor can observe a new domain (math practice)
  without domain-specific instrumentation — it reads the three-column
  stack, which the practice evidence feeds into

### 3.3 From M-futonzero-mvp (tabled, partially extracted)

- The scheduling policy (§9 of the MVP spec, the "pachinko" loop)
- The learner-state model (due/review/edge/probe categories)
- The reporting format (7-day capability trajectory)

### 3.4 New

- A practice CLI: `futonzero practice` that runs a session
- A practice evidence format (EDN, appended to the evidence store)
- A subject-balanced problem selector
- Integration with the existing pipeline output

## 4. Scope

### In scope

- Evening practice sessions (Joe, solo, 2 hours, 5-6 problems)
- Evidence capture from each session
- Capability trajectory reporting (weekly)
- Feedback to M-apm-solutions retrospectives

### Out of scope (for now)

- Multi-learner support (just Joe)
- Real-time AI tutoring during the attempt phase (the attempt must
  be unassisted to measure genuine capability)
- Formal assessment or certification
- Integration with university systems

## 5. Success criteria

After 4 weeks of practice (20 sessions, ~100 problems):

1. Joe can identify the key insight in ≥50% of new problems before
   seeing the solution.
2. Joe's tool recognition rate improves measurably across the 4 weeks.
3. At least one transfer event is documented (technique from one
   subject applied in another).
4. The capability trajectory shows learning, not just repetition.
5. At least one feedback loop to M-apm-solutions produces a
   measurable improvement in solution quality for subsequent batches.

## 6. Teaching Tactics — Informal Proof Assistance (2026-04-09)

In formal theorem proving, a tactic closes a goal. In MathDojo (the
interactive study layer), a teaching tactic *teaches you how to close
a goal yourself*. The output isn't a proof step — it's understanding.

| Formal tactic | Teaching tactic | What it does for the learner |
|---|---|---|
| `exact?` | `what-theorem?` | "What known result closes this? Here are 3 candidates — which fits?" |
| `apply` | `why-this-lemma?` | "This lemma applies because... Can you see which hypothesis matches?" |
| `rw` | `rewrite-hint` | "These are equal. Can you see why? Hint: expand the definition of X." |
| `cases` | `what-cases?` | "This splits into cases. What are they? What differs?" |
| `contradiction` | `what-breaks?` | "Assume the opposite. Follow the logic until something contradicts." |
| `calc` | `chain-hint` | "You need a chain of inequalities. What's the first bound?" |
| `sorry` | `where-stuck?` | "What's the goal? What do you have? What's the gap?" |

The `where-stuck?` tactic uses the same QP diagnostic questions from
M-diagramprover's ArSE kick (QP-3 structural probe, QP-6 tension
dissolution, QP-8 confidence inversion) — but directed at the human
learner instead of the AI agent.

**Token efficiency:** Each teaching tactic is one focused LLM call
with the specific proof context. Not "explain this whole problem"
but "I'm at step 3, I have ∫|f|^p, I need to show it's finite."
~200 tokens in, ~500 out. One penny per hint.

**Bayesian learner model signal:** Each interaction records *which*
teaching tactic was needed at *which* step: "Joe needed `what-theorem?`
at Cauchy-Schwarz, `chain-hint` at the integral bound." This profiles
exactly which mathematical moves Joe has and hasn't internalised.
The geometric-mean gate model from M-diagramprover applies: the
weakest prerequisite technique determines whether Joe can solve the
problem independently.

## 7. MathDojo — Pre-Computed Navigation Layer (2026-04-09)

The proof corpus (`futon3c/data/apm-informal-proofs/`) is the knowledge
base. MathDojo adds a zero-token navigation layer:

- **Topic index**: 107 problems across 16 technique clusters
- **Technique graph**: 85 co-occurrence edges (e.g., measure-theory ↔
  real-analysis: 25 shared problems)
- **Difficulty ladder**: within each cluster, problems ordered by
  technique complexity

Pre-computed from existing files. Browsing costs zero tokens.
Questions cost ~500 tokens each. See `futon3c/data/mathdojo/`.

**Arxana integration**: questions asked during study get indexed into
ArSE. A batch "answer my questions" run alongside apm-daily-batch
processes them cheaply. Answers feed back into the learner model.

**Gap identification**: when the Bayesian model says "Joe doesn't know
Borel-Cantelli," the system points to undergraduate prerequisites.
Open textbooks (Axler's *Measure, Integration & Real Analysis* CC-BY-NC,
Tao's *Analysis* I/II) provide the prerequisite material. This extends
the system to learners who aren't at Joe's level.

## 8. Generalisation Beyond Mathematics (2026-04-09)

Mathematics is the pilot domain because it has the tightest feedback
loop: Lean verifies, Mathlib provides the API surface, the prelim
corpus provides graded problems. But the architecture is domain-agnostic.

### 8.1 Computer programming as mathematics

Per the Library of Congress classification, computer programming IS
part of mathematics (QA 76). The futon stack's self-representing
capability (M-self-representing-stack) makes it a concrete instance:
the stack describes itself as typed EDN hypergraphs. Learning to
program the futon stack is learning to navigate a formal system —
the same way learning to prove theorems is navigating Mathlib.

If the self-representing stack's EDN structures can be modelled in
Lean (or a comparable type system), then:
- The "proofs" are programs that correctly transform the stack's state
- The "theorems" are invariants the stack must uphold
- The "sorry" are features not yet implemented or verified
- The teaching tactics work identically: `what-theorem?` becomes
  "what API call closes this?" and `where-stuck?` becomes "what's
  the type error telling you?"

### 8.2 The transferable architecture

| Component | Math domain | Programming domain | Any domain |
|---|---|---|---|
| Corpus | 489 prelim proofs | Self-representing stack EDN | Any structured knowledge base |
| Navigation | Technique graph (MathDojo) | Module dependency graph | Topic co-occurrence graph |
| Teaching tactics | Proof hints | Code hints | Step-by-step guidance |
| Verification | Lean type-checker | Compiler + tests | Domain-specific validator |
| Learner model | P(insight | technique, exposure) | P(correct-impl | API-knowledge, exposure) | P(competence | prerequisite, exposure) |
| Gap identification | "Review Borel-Cantelli" | "Review core.async" | "Review prerequisite X" |

The capability growth IS the objective function. The prelims are the
first gym equipment. The muscles are transferable.

## 9. Relation to the serendipity thesis

This mission is NOT about optimising Joe's prelim-passing ability.
He's not going back to UT Austin. The prelim problems are a *substrate*
for capability growth — a structured, graded, well-scoped domain
where learning is measurable.

The real value is in what the FUTON stack's ability to support human
learning reveals about learning itself. If the pipeline's tutoring
output actually teaches — not just explains — then the same discipline
transfers to any domain where structured exposition + verification +
spaced practice + teaching tactics is valuable.

The serendipity is that Joe's "failure" (leaving Austin without passing
prelims) produced the system (FUTON) that now produces the tutor
(MathDojo) that teaches the material (489 prelim solutions) that he
didn't learn the first time. The 20-year loop closes — not by going
back to Austin, but by bringing Austin's problems into the system
that grew from leaving.
