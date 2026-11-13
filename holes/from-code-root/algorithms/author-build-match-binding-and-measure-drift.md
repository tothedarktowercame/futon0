# Algorithm: Author a Build-Match Binding (Spec-Derived) and Measure the Drift

Date: 2026-07-08
Status: active
Owner mission: M-war-machine-wiring / the actuator (futon2 aif)

## Purpose

Make a mission's CLean testable through **build-match** by authoring the per-box substrate
bindings it is missing — **spec-derived** from each box's meaning, never reverse-engineered from
what happens to be in the store — then run build-match to measure the **drift** between the
authored structural spec and what was actually built.

Two roles, separation of powers (author ≠ reviewer): an **author** spec-derives the bindings; a
**reviewer** runs build-match and hands back a gap report. Distilled from the live claude-4 ↔
zai-10 protocol (autoclock-in → 2/3, discriminating on typed hyperedges; state-snapshot-witness →
3/3 but coarse on generic `:evidence`; single-entry-point → …).

This is an **LLM-facing step** in the pipeline. Its nodes (what each stage produces) are described
by the `aif/*` patterns; this algorithm describes the *procedure* an agent runs over them.

## Prerequisites

1. The mission's CLean exists and passes `clean_argcheck` (`futon6/holes/clean/<mission>.clean.edn`).
2. Authed Drawbridge reachable for READ-ONLY schema exploration (`x-admin-token` ← `futon3c/.admintoken`).
3. The build-match machinery present (`futon2.aif.actuator-a3`: `reviewed-box-bindings`,
   `box-match-snapshot`, `build-match`).

## Inputs

- The CLean — boxes with `:method / :text / :produces / :consumes`.
- The substrate-2 schema — the entity/hyperedge type vocabulary, learned by read-only queries.
- The binding format — `reviewed-box-bindings` + `reviewed-endpoint-bindings` (M-learning-loop is
  the exemplar).

## Procedure

### AUTHOR role — spec-derive the bindings

**Step 1.** Read every box's `:produces` MEANING (+ `:text`). Ask: *what type of thing SHOULD this
box produce?* — from the box's intent, not from the store.

**Step 2.** For each box, decide a binding:
- If `:produces` has a substrate manifestation → `{:kind :entity|:hyperedge :type <kw> :endpoint-types [...]?}`.
- Learn the type NAME from the schema (read-only Drawbridge), but DERIVE the choice from the box meaning.
- Prefer the **most specific honest type**. If several boxes would bind to the same GENERIC type
  (e.g. `:evidence`), the check passes trivially — say so, and use the most specific type each box supports.
- If `:produces` is ABSTRACT/process (an invariant, a spoiled guard, a theorem property, a payload
  living *inside* another box's entity) → mark **unbound: abstract**. Not every box grounds; honesty > coverage.

**Step 3.** The terminal box (`:discharges`) → also an endpoint-binding.

**Step 4.** PREDICT the drift. For each bound box, state whether you expect PASS or FAIL, and why. A
binding you expect to **fail** — bound to the spec-correct type that the build happens not to satisfy
— is the STRONGEST evidence you spec-derived rather than gamed. (autoclock-in `s2` bound `:person`
and predicted its own failure — agents are raw strings, not typed entities.)

**Step 5.** Write the bindings (`reviewed-box-bindings` + `reviewed-endpoint-bindings` entry) and a
grounding note (per box: `:produces`, chosen type or "unbound: abstract", WHY from the meaning,
expected drift). Commit on the actuator branch.

**Step 6.** Gates: `clj-kondo` clean, `check-parens` clean, **no substrate writes, no `:7071` restart**.

### REVIEWER role — measure the drift

**Step 7.** Verify spec-derivation: each binding justifiable from the box meaning ALONE. The tell of
gaming is a binding chosen to pass; the tell of honesty is a binding that predicts its own drift.

**Step 8.** Run build-match (`box-match-snapshot` / `build-match`) against the live substrate.

**Step 9.** Hand back a GAP REPORT: the dial (inhabited/bound), per-box pass/drift, and the MEANING
of each drift — what the authored spec claims vs what the build delivers. A drift is a **finding**, not
a failure.

**Step 10.** Note the granularity: if the dial is high but the bound types are generic, flag the
coarseness (type-inhabitation ≠ instance-inhabitation) — a candidate for the instance-predicate
refinement (build-match v2).

## Outputs

- A `reviewed-box-bindings` + `reviewed-endpoint-bindings` entry for the mission.
- A grounding note (per-box rationale + drift prediction).
- A gap report (the build-match dial + the drift meanings).

## The one invariant (do not violate)

**Spec-derived, not reverse-engineered.** Each binding must be justifiable from the box's `:produces`
MEANING, independent of whether that type is inhabited. The drift between the authored spec and the
actual substrate is the SIGNAL — build-match measures it. A binding chosen to make the check pass
destroys the test.

## Gotchas

- Generic types (`:evidence`) pass trivially — prefer specific types; flag coarseness.
- Not every box grounds — abstract/process boxes are honestly unbound.
- build-match v1 checks TYPE inhabitation, not specific instances.
