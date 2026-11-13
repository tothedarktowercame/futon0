# Algorithm: Author Flexiarg Core and Wire Discoverability

Date: 2026-03-14
Status: active

## Purpose

Use this after the flexiarg lifecycle gate has already been decided and the
remaining work is the shared inner authoring/refactoring core for a
flexiarg-owned judgment surface.

This algorithm owns the common inner handling shared by:
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/update-flexiarg-from-recurring-judgment-surface.md`

It is intentionally not the owner of the new-versus-existing decision. Its
callers decide that first, then delegate here.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - governs the carrier boundary that keeps flexiargs about reusable judgment
    surfaces rather than replayable procedure
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first factoring of existing flexiarg owners, helpers, and
    local indexes before inventing parallel structure
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - governs lane and naming discipline for the resulting flexiarg artifact
- `library/system-architecture/edge-types.flexiarg`
  - governs typed edge encoding for flexiarg metadata, including when to use
    `@isa`, `@hasa`, `@childof`, and when to keep `@references` weak
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
  - governs repo-local `data/tmp/...` placement when the judgment surface is
    about temp-producing behavior

Inspected but non-owning candidates:
- `library/social/create-versus-update-flexiarg.flexiarg`
  - lifecycle gating belongs to the caller before this shared core is invoked
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - may govern narrower futon-self-description cases, but not the generic
    flexiarg-authoring core

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- lifecycle mode:
  - `new`
  - `existing`
- the recurring judgment surface already judged to deserve flexiarg work
- the target flexiarg owner or intended owner location
- the current explicit mission owner, or a bounded decision to create one first
- nearby flexiargs, algorithms, docs, and missions in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. The caller has already decided the lifecycle mode honestly.
2. The caller has already decided the lane is still flexiarg-owned rather than
   procedural.
3. The target owner or intended owner location is known.
4. The work is stable enough that a reusable conclusion/boundary can be named.
5. The caller keeps the family split explicit:
   - `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
   - `algorithms/update-flexiarg-from-recurring-judgment-surface.md`

## Procedure

1. Bind the lifecycle mode and target owner.
   - If the mode is `new`:
     - confirm the strongest honest lane, location, and filename
   - If the mode is `existing`:
     - confirm the current flexiarg file is still the strongest owner
   - If a split into stronger parent/child or sibling flexiargs is now the
     clearer shape, make that split explicit rather than leaving it implicit
2. Identify the judgment surface to preserve.
   - Write the smallest stable:
     - conclusion
     - boundary
     - failure modes
     - adoption checks or guardrails
   - If the surface is about temp-producing behavior, name the intended
     repo-local `data/tmp/...` owner path or the explicit exception contract.
3. Recheck the strongest flexiarg neighborhood before writing.
   - Prefer refining an existing owner when that is cleaner than creating a
     near-duplicate.
   - Keep nearby docs, algorithms, and missions as routing/supporting
     artifacts rather than overloading the flexiarg.
4. Author or refactor the flexiarg artifact.
   - Required baseline headers:
     - `@flexiarg`
     - `@title`
     - `@keywords`
     - `@audience`
     - `@tone`
     - `@style`
     - `@references`
   - Stronger typed edge headers:
     - use `@isa` when the flexiarg is a specialization or concrete
       instantiation of a broader abstraction
     - use `@hasa` when the flexiarg honestly owns named child/specialization
       members
     - use `@childof` when the flexiarg is explicitly owned by a stronger
       owner
     - keep `@references` and `@related` for weak adjacency, precedent, and
       discoverability only
   - Include:
     - `! conclusion`
     - context / `IF` / `THEN` / `BECAUSE`
     - case anchors
     - failure modes
     - adoption checks or guardrails
5. Preserve the carrier boundary.
   - Keep replayable ordered control flow in algorithms.
   - Keep reusable non-linear policy and decision gates in the flexiarg.
6. Wire discoverability.
   - Update the relevant `library/*/README.md` index.
   - Add mission/algorithm references where the flexiarg is a governing
     authority.
   - When this flexiarg is a companion to an algorithm, ensure the paired
     algorithm artifact records:
     - governing companion flexiargs
     - inspected but rejected nearby candidates
     - the execution-time rule that the governing companions are read/applied
7. Record reused versus new.
   - Name reused authorities inspected.
   - Name created/updated artifacts and new cross-references.
   - If the flexiarg wave created a new connected neighborhood or sharpened a
     new conceptual relation, first add the immediate local links that make
     that topology legible before reducing overlap.
8. Run the follow-on orthogonality pass.
   - Treat this as the second step after the link-enrichment step above.
   - The pass is not optional.
   - Use:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - If the pass concludes that no broader cleanup is needed, record that
     trivial result explicitly rather than silently skipping the pass.

## Decision Rule

Default rule:
1. let the create/update flexiarg algorithms decide lifecycle mode first
2. once lifecycle mode is known, put the shared inner flexiarg handling here
3. keep replayable ordered procedure out of the flexiarg and in algorithms

## Outputs

- one of:
  - a new flexiarg
  - an updated existing flexiarg
  - a refactored flexiarg family with clearer owner routing
- updated indexes and cross-references for discoverability
- explicit reused-versus-new reporting for the flexiarg wave

## Stop Conditions

Stop and return to the caller if:
- the lifecycle mode is still not actually settled
- the lane is no longer honestly flexiarg-owned
- the target owner is still ambiguous after explicit-neighborhood inspection
- the same artifact would need to own too many unrelated concerns

## References

- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/update-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `library/social/create-versus-update-flexiarg.flexiarg`
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
- `library/system-architecture/edge-types.flexiarg`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
