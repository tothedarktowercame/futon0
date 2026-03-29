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

## 6. Relation to the serendipity thesis

This mission is NOT about optimising Joe's prelim-passing ability.
He's not going back to UT Austin. The prelim problems are a *substrate*
for capability growth — a structured, graded, well-scoped domain
where learning is measurable.

The real value is in what the practice reveals about the FUTON stack's
ability to support human learning. If the pipeline's tutoring output
actually teaches — not just explains — then the same discipline
transfers to any domain where structured exposition + formal
verification + spaced practice is valuable.

The capability growth IS the objective function. The prelims are the
gym equipment. The muscles are transferable.
