# Algorithm: Create Document

Date: 2026-03-14
Status: active

## Purpose

Use this when the operator explicitly wants durable document writeback and the
truthful next move may still be:
- create a new docs owner
- update an existing docs owner instead
- route out of docs entirely because another artifact class is stronger
- stop at discussion only for now

This is the new-document gate sibling of:
- `algorithms/update-document.md`

It owns only the new-document gate for document authoring.

It does not own the shared inner document-authoring/writeback work after the
new-versus-existing gate is crossed. That shared core now belongs to:
- `algorithms/author-document-core-and-route-writeback.md`

It does not own mission, idea, algorithm, flexiarg, skill, or issue-record
creation semantics. Those stay with the routed artifact-specific owners this
algorithm dispatches to.

Family note:
- when create/update pressure is real on both sides, expect the full family
  split by default:
  - create gate
  - update gate
  - shared inner authoring core
  unless a later pass shows that split would be fake

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/create-versus-update-document.flexiarg`
  - governs the lifecycle judgment this algorithm exists to operationalize on
    the new-owner side
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first owner selection before inventing a parallel docs owner
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - governs deference when the requested document is really describing futon's
    own semantics rather than a local docs delta

Inspected but non-owning candidates:
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - governs naming/taxonomy for factoring passes, not the lifecycle gate for
    document ownership
- `library/social/kolmogorov-factoring-for-composable-knowledge.flexiarg`
  - governs the shared inner writeback/factoring core after lifecycle has
    already been decided

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the operator's requested document or document change
- the current explicit owner neighborhood in:
  - `gh/mfuton`
  - `darktower/futon3c` when upstream ownership may matter
- nearby missions, ideas, docs, algorithms, and flexiargs

## Preconditions

1. The operator is actually asking for durable writeback, not just discussion.
2. Existing owner artifacts will be inspected before creating a new document.
3. The strongest current owner may be outside `docs/`.
4. Before creating a brand-new docs owner, check whether the stronger move is
   to update an existing docs owner first.
5. The create-versus-update family split remains explicit in this lane.

## Procedure

1. Start from the requested document and classify the likely artifact class.
   - Ask:
     - is this honestly a docs-owned writeback?
     - or is it actually a mission, idea, algorithm, flexiarg, or issue
       asking to be written?
   - Do not assume "document" means `docs/`.
2. Inspect strongest current owners first.
   - Check `gh/mfuton` for an existing owner.
   - Check `darktower/futon3c` when upstream ownership may already exist.
   - Prefer refining an existing owner when that is cleaner than creating a
     near-duplicate.
3. Decide document lifecycle shape first.
   - Reuse the decision gate:
     `library/social/create-versus-update-document.flexiarg`
   - Decide whether this wave should honestly:
     - create a new docs owner
     - update an existing docs owner instead
     - route to another artifact class instead of docs
     - stop at discussion only for now
4. If the stronger move is to update an existing docs owner, do not keep
   forcing the create path.
   - Call:
     `algorithms/update-document.md`
   - Stop this algorithm after handing off.
5. Route to the strongest artifact-specific owner when the requested document
   is really another artifact class.
   - If it is a mission:
     - use:
       `algorithms/create-mission.md`
   - If it is an idea:
     - discuss first with:
       `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
     - then merge through:
       `algorithms/merge-artifact.md`
   - If it is a new algorithm:
     - use:
       `algorithms/create-algorithm.md`
   - If it is an update to an existing algorithm:
     - use:
       `algorithms/update-algorithm.md`
   - If it is a flexiarg:
     - use:
       `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
   - If it is a local `mfuton` issue record:
     - use:
       `algorithms/create-mfuton-issue-before-triage.md`
   - If it is a futon-origin proxy issue record:
     - use:
       `algorithms/create-futon-issue-before-triage.md`
   - If it is a new agent skill surface or grouped wrapper/backend lane:
     - use:
       `algorithms/create-new-agent-skill-with-typed-surface-grouping-python-wrapper-and-tests.md`
6. If the lane is not yet ready for durable docs writeback, stop honestly.
   - Keep the work at discussion/mission/idea level instead of creating docs
     churn.
7. If a new docs owner is the truthful result, call the shared document
   authoring core.
   - Use:
     `algorithms/author-document-core-and-route-writeback.md`
   - Pass the lifecycle mode as:
     - `new`
8. If a follow-on artifact-boundary cleanup is still wanted after the shared
   core runs, use:
   - `algorithms/conduct-knowledge-orthogonalization-pass.md`

## Current Creation Routing Set

This algorithm should point to every other current algorithm in
`algorithms/` whose basename starts with `create`, excluding
`create-document.md` itself.

The current complete routed set is:

- `algorithms/create-algorithm.md`
- `algorithms/create-new-agent-skill-with-typed-surface-grouping-python-wrapper-and-tests.md`
- `algorithms/create-mission.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/create-futon-issue-before-triage.md`
- `algorithms/create-mfuton-issue-before-triage.md`

## Outputs

- one of:
  - a new docs owner via the shared document-authoring core
  - a routed handoff into `algorithms/update-document.md`
  - a routed handoff into a stronger mission/idea/algorithm/flexiarg/issue
    owner
  - an explicit decision that no durable document should be written yet

## Stop Conditions

Stop and ask the operator if:

- the strongest owner remains ambiguous after explicit-neighborhood inspection
- the requested document is really two different artifact classes at once
- docs-owned explanation and mission-owned scope are still too entangled to
  write honestly
- the truthful next move is still discussion only

## References

- `algorithms/update-document.md`
- `algorithms/author-document-core-and-route-writeback.md`
- `algorithms/merge-artifact.md`
- `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- `algorithms/create-mission.md`
- `algorithms/create-algorithm.md`
- `algorithms/update-algorithm.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/create-futon-issue-before-triage.md`
- `algorithms/create-mfuton-issue-before-triage.md`
- `algorithms/create-new-agent-skill-with-typed-surface-grouping-python-wrapper-and-tests.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `library/social/create-versus-update-document.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
