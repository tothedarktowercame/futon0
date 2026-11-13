# Algorithm: APM NL-Proof Harvest — Stage-1 across the whole dataset

Date: 2026-06-16
Status: proposed
Owner mission: M-apm-solutions (futon3c) · feeds E-superpod-mark3 H13 (APM inline eval)
Supersedes (for this purpose): the 5/day cadence of `apm-daily-batch.md`

## Why this exists

We want a **complete** natural-language proof corpus for all 492 APM problems
(117 done → ~375 to go) **before** APM is used as the mark3 inline test set, so
the eval runs over the whole set, not a partial.

**Key observation:** the daily batch's 5/day cadence is gated by the **Lean
stage** (Codex closing the `sorry` + `lake build` + review), NOT by the NL proof.
The informal proof is **Claude-only, one-shot, with no Lean/Codex/lake
dependency** (`apm-daily-batch.md` Step: "Claude produces informal proofs + Lean
theorem statements"). So Stage-1 can be **decoupled and fanned out across the
whole manifest in parallel** — the Lean formalization stays on its own slower
track and is not a prerequisite for the test set.

## Procedure (the altered batch)

1. **Enumerate**: `storage/apm/manifest.edn` (489) minus the
   `data/apm-informal-proofs/` already-done (117) → the ~375 todo. Resumable:
   skip any with an existing `<id>.md`.
2. **Stage-1 only**: per problem, dispatch the **informal-proof prompt** (with
   `data/apm-formalization-patterns.edn` injected for consistency) to a Claude
   agent. Do NOT generate the Lean statement, do NOT hand to Codex, do NOT
   `lake build`. Output: `futon3c/data/apm-informal-proofs/<id>.md`.
3. **Fan out across the Claude pool**: shard the todo list over the registered
   Claude agents (claude-1, claude-3, lon-claude-1, chi-claude-1 …) via
   `agency_send --kind bell` — pool-parallel, not 5/day serial. (No JVM restart;
   if a conductor change is wanted, reload the `:nl-only` mode via Drawbridge.)
4. **Provenance**: each proof carries `source: agent-generated` + the agent id +
   date. These are markup-test substrate, NOT gold proofs.

## Decoupling rule

NL-proof harvest (this) and Lean formalization (`apm-daily-batch` / v3 conductor)
are now **independent stages writing to the same canonical surface**
(`apm-lean/problems/<id>/`). The harvest fills the `:informal-proof` slot for all
problems; the Lean stage closes `sorry`s on whatever cadence it can sustain. APM
becomes a usable test set as soon as the harvest completes — Lean coverage
(currently sorry-free 4 / sorry-carrying 11) deepens the *ground truth* over time
but is not required to start.

## Discipline (we-do)

Agent-generated NL proofs are valid substrate for testing **markup / scope /
grounding** (we test the method on the text, not the proof's correctness). For the
**import-resolution eval** (H13), the proof's *self-declared* imports are NOT
independent ground truth — the independent anchors are (a) the **Lean cross-ref**
(formally-verified `sorry-free` proofs) and (b) **standard-result expectations**
(we know what a standard prelim proof of a given problem depends on). Score H13
against those, not against the generator's own citations.

## Output / done criterion

492/492 problems have an `<id>.md` informal proof under
`apm-informal-proofs/` (provenance-tagged). At that point APM is a ready,
whole-dataset, multi-MSC held test set for H13.
