# Algorithm: Author Document Core and Route Writeback

Date: 2026-03-14
Status: active

## Purpose

Use this after the document lifecycle gate has already been decided and the
remaining work is the shared inner authoring/refactoring core for a
docs-owned writeback.

This algorithm owns the common inner handling shared by:
- `algorithms/create-document.md`
- `algorithms/update-document.md`

It is intentionally not the owner of the new-versus-existing decision. Its
callers decide that first, then delegate here.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first factoring of existing docs owners, local indexes, and
    supporting writeback surfaces before inventing parallel structure
- `library/social/kolmogorov-factoring-for-composable-knowledge.flexiarg`
  - governs bounded docs deltas, canonical-owner preservation, and
    composability during writeback

Inspected but non-owning candidates:
- `library/social/create-versus-update-document.flexiarg`
  - lifecycle gating belongs to the caller before this shared core is invoked
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - may govern narrower upstream-deference cases, but not the generic inner
    docs-authoring core
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - governs naming taxonomy for later factoring passes, not the shared core
    itself

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- lifecycle mode:
  - `new`
  - `existing`
- the requested docs-owned writeback already judged to stay in docs
- the target document owner or intended owner location
- nearby missions, ideas, docs, algorithms, and flexiargs in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. The caller has already decided the lifecycle mode honestly.
2. The caller has already decided that docs is still the truthful artifact
   class.
3. The target owner or intended owner location is known.
4. The explicit local owner neighborhood has been inspected before durable
   writeback.
5. The caller keeps the family split explicit:
   - `algorithms/create-document.md`
   - `algorithms/update-document.md`

## Procedure

1. Bind the lifecycle mode and target owner.
   - If the mode is `new`:
     - confirm the strongest honest location and filename
   - If the mode is `existing`:
     - confirm the current document file is still the strongest owner
2. Identify the smallest honest docs delta.
   - Name the audience and purpose:
     - operator-facing explanation
     - routing note
     - runbook
     - mission-local docs note
     - narrower local docs neighborhood
   - Keep the delta bounded instead of free-floating and broad.
3. Recheck the strongest docs neighborhood before writing.
   - Prefer refining an existing owner when that is cleaner than creating a
     near-duplicate.
   - If a narrower local owner is stronger than broad `docs/`, use it.
4. Author or refactor the document artifact.
   - Keep docs-owned material bounded.
   - Docs own:
     - operator-facing explanation
     - routing
     - runbooks
     - mission-local docs neighborhoods
   - Docs do not silently absorb:
     - mission scope/acceptance
     - exploratory ideas disguised as settled docs
     - algorithmic procedural spines that deserve algorithm ownership
5. Apply docs-first writeback through the merge owner.
   - Use:
     `algorithms/merge-artifact.md`
   - Classify the result as:
     - `docs-neighborhood shaping or docs-neighborhood refinement`
     - or `docs-first plus mission/idea wiring`
6. Wire discoverability.
   - Update the immediate local index or owner README needed so the document is
     discoverable.
   - Do not expand into unrelated broad wiring unless that is truly required by
     the chosen owner.
7. Record reused versus new.
   - Name reused authorities and supporting artifacts.
   - Name new/refactored artifacts and cross-references created.
   - If the writeback created a new connected neighborhood or reframed local
     topology, first enrich the immediate links around that owner before
     reducing overlap.
8. Run the follow-on orthogonality pass.
   - Treat this as the second step after the link-enrichment step above.
   - The pass is not optional.
   - Use:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - If the pass concludes that no broader cleanup is needed, record that
     trivial result explicitly rather than silently skipping the pass.

## Outputs

- one of:
  - a new docs owner
  - an updated existing docs owner
  - a refactored docs neighborhood with clearer owner routing
- updated local discoverability links
- explicit reused-versus-new reporting for the docs wave

## Stop Conditions

Stop and return to the caller if:
- the lifecycle mode is still not actually settled
- docs is no longer the truthful artifact class
- the target owner is still ambiguous after explicit-neighborhood inspection
- the same artifact would need to own too many unrelated concerns

## References

- `algorithms/create-document.md`
- `algorithms/update-document.md`
- `algorithms/merge-artifact.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/kolmogorov-factoring-for-composable-knowledge.flexiarg`
