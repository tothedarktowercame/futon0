# Algorithm: How To Evaluate Constraints

Date: 2026-03-15
Status: active

## Purpose

Use this when a mission, algorithm, flexiarg, or other durable artifact has an
explicit constraint, acceptance criterion, or success condition and you need to
determine:
- whether it is currently satisfied
- whether the implementation is missing something
- whether the constraint wording itself is too strong, ambiguous, or otherwise
  not aligned with the actual mission goals

This algorithm exists so constraint evaluation does not collapse into either:
- wishful “looks good enough” closure
- silent weakening of the constraint to match current implementation

## Companion Flexiarg Resolution (2026-03-15)

Governing companion flexiargs for this algorithm:
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - governs the operator permission boundary when a constraint change would
    alter the durable contract
- `library/foundational/how-to-abstract.flexiarg`
  - governs honest classification of what the evidence really shows rather
    than overfitting one vivid implementation detail

Adjacent but non-owning algorithm:
- `algorithms/close-verified-mission-and-delegate-deferred-work.md`
  - uses constraint/acceptance-criteria evaluation as part of mission closure
  - should compose this algorithm when deciding whether closure-time criteria
    are actually satisfied or whether the lane must return to implementation
  - does not own the broader question of whether a constraint itself is
    well-formed, overstated, or misaligned with mission goals
- `algorithms/formalize-type-from-validator-source-through-example-roundtrip-and-derived-surface-parity.md`
  - should compose this algorithm before a bounded type slice is treated as
    complete against its owning mission/algorithm constraints
  - does not own the general constraint-evaluation logic itself

## Inputs

- the owning artifact containing the constraint
- the exact constraint text to evaluate
- the strongest local implementation/evidence surfaces
- the owner’s purpose/scope context:
  - goal
  - scope in/out
  - acceptance criteria
  - derivation path

## Preconditions

1. The exact constraint text has been re-read directly from the owning
   artifact.
2. The evaluation is grounded in concrete evidence, not memory.
3. The owning artifact’s goal and scope have been re-read before judging the
   constraint itself.

## Procedure

1. Read the exact constraint first.
   - Quote or restate the exact current wording.
   - Do not start from remembered paraphrase.
2. Bind the owner and its goals.
   - Re-read the owner’s:
     - goal/purpose
     - scope in/out
     - neighboring acceptance criteria or sibling constraints
     - derivation path when needed
   - Determine what the mission/artifact is actually trying to accomplish.
3. Gather concrete evidence.
   - Inspect the real implementation, tests, schemas, proofs, docs, and
     topology that bear on the constraint.
   - Prefer the strongest executable or machine-checkable evidence first.
4. Evaluate literal satisfaction.
   - Ask whether the current implementation satisfies the constraint exactly as
     written.
   - Do not weaken the reading just because the implementation is close.
5. If the answer is “not fully satisfied,” classify the mismatch.
   - Use one of:
     - implementation missing required behavior
     - evidence exists but is not yet wired/checked clearly enough
     - already discovered or already implemented structure is not yet closed
       across the owner's required derived/check surfaces
     - wording is ambiguous or underspecified
     - wording is stronger than the current implementation
     - constraint is not aligned with the mission’s actual goals/scope
6. Distinguish “implementation gap” from “constraint problem.”
   - A constraint is not wrong merely because the implementation does not yet
     satisfy it.
   - Treat the constraint as still binding unless you can show it is not in
     line with the owner’s goal, scope, or honest abstraction boundary.
7. Test alignment against mission purpose.
   - Ask:
     - if implemented literally, would this constraint advance the mission’s
       stated goal?
     - is it consistent with scope in/out?
     - does it reflect the same abstraction boundary as the rest of the owner?
     - does it collapse distinct concerns into one criterion?
     - does it claim a stronger formalization than the lane actually intends to
       own?
8. Decide the next action.
   - If the constraint is aligned and unsatisfied:
     - implement or add the missing evidence
   - If the constraint is aligned but ambiguous:
     - clarify the wording only with operator approval if the durable contract
       changes
   - If the constraint is not aligned with the mission goals:
     - offer the operator the option to change it
9. Enforce the permission rule.
   - Constraints cannot be changed without explicit operator permission.
   - Only offer a constraint change after you have already determined that the
     current constraint is not aligned with the mission’s actual goals/scope.
   - Do not change a constraint unilaterally, even if the current
     implementation falls short of it.
10. Report criterion-by-criterion.
   - State:
     - satisfied / partially satisfied / unsatisfied
     - the concrete evidence
     - whether the gap is in implementation or in the constraint
     - the recommended next action
   - If the owner is recursive or graph-discovery-oriented, also state whether
     the already discovered layer is closed at the owner's required fidelity
     boundary before any deeper discovery proceeds.

## Decision Rule

Default rule:
1. read the exact constraint
2. test literal satisfaction against concrete evidence
3. if unsatisfied, assume implementation gap first
4. only consider changing the constraint after proving misalignment with the
   owner’s actual goals/scope
5. never change the constraint without explicit operator permission

## Outputs

- a concrete satisfaction judgment:
  - satisfied
  - partially satisfied
  - unsatisfied
- evidence references supporting that judgment
- mismatch classification:
  - implementation gap
  - evidence/wiring gap
  - already-discovered-layer still open across required surfaces
  - ambiguous wording
  - misaligned constraint
- a bounded recommendation:
  - implement
  - clarify
  - offer constraint change to operator

## Composition Note

When this algorithm is composed by a mission-closure, type-formalization, or
other “completed chunk” algorithm:
- satisfied criteria may support closure/checkpoint continuation
- partially satisfied or unsatisfied but still-aligned criteria should send the
  run back to implementation/evidence work rather than to closure
- only misaligned constraints should trigger an operator-facing option to
  change the constraint
- if the owner is recursive or graph-discovery-oriented, and the already
  discovered layer is not yet closed across the owner's required
  Python/derived/formal surfaces, the run should loop back there before any
  deeper discovery or new logical-set work begins

## Stop Conditions

Stop and ask for clarification if:
- the exact constraint text cannot be identified
- the owner’s goal/scope is too unclear to judge alignment
- there is not enough evidence to distinguish implementation gap from wording
  problem
- the proposed wording change would materially alter the durable contract and
  operator permission has not been given

## Case Anchors

- `futon-object` mission criterion 6 audit
  - literal evaluation against code showed the mission slice was close but not
    fully satisfied
  - `Mission` had explicit fully-futon-specified trace metadata
  - `Flexiarg` and `Devmap` did not yet carry the same stronger explicit split
  - that forced the correct question:
    - implement the stronger trace metadata
    - or reword the criterion
  - the correct recommendation was not to weaken the criterion automatically,
    but to first judge whether the stronger criterion actually matched the
    mission’s purpose
