# Algorithm: Decide Follow-On Orthogonality and Devmap Wiring After Mission Reconstruction

Date: 2026-03-09
Status: active

## Purpose

Use this after a mission has just been created or reconstructed post hoc and
you need to decide whether to:

1. wire it into a devmap now
2. leave it floating intentionally
3. leave it floating temporarily and schedule a later cleanup/rewire pass

This algorithm exists so retroactive mission reconstruction does not stop at
"the mission now exists" when the larger topology question is still open.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/system-architecture/devmap-wiring-decision-for-floating-and-retroactive-missions.flexiarg`
  - governs the actual `wire-now` vs `floating-by-design` vs
    `floating-pending-cleanup` decision
- `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
  - governs the retroactive-mission context that frequently leads into this
    follow-on decision
- `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
  - governs explicit-neighborhood-first topology inspection before widening the
    search for candidate devmap owners

Inspected but non-owning candidates:
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - relevant when the follow-on expands into a broader factoring pass, but not
    the direct devmap-wiring decision owner

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the newly created or reconstructed mission
- its explicit mission neighborhood:
  - parent/child structure
  - direct dependencies
  - lane references
  - mission-owned docs/prototypes
  - governing flexiargs
- candidate devmaps that currently mention, feed, or structurally depend on
  the mission
- any current overlap or duplication pressure that may justify a bounded
  orthogonality pass

## Preconditions

1. The mission already exists as a durable artifact.
2. If the mission was retroactively reconstructed, the mission already names
   `Delta`, `M`, and `Sigma`.
3. The question is now topology/wiring, not whether the mission deserved to
   exist at all.

## Procedure

1. Start from the explicit mission neighborhood.
   - Re-read the mission itself.
   - Re-read its direct parent, direct children, direct `depends on` edges,
     lane catalog, and named governing flexiargs.
   - Do not start from semantic search results.
2. List the candidate devmaps.
   - Include devmaps that already mention or structurally feed this mission.
   - Include the parent mission's devmap when one clearly exists.
   - Do not widen to every semantically adjacent devmap unless there is real
     evidence of participation.
3. Check whether one devmap is the strongest current owner.
   - Ask:
     - does one devmap clearly generate, organize, or absorb this mission's
       continuation?
     - would wiring the mission there improve discoverability or owner clarity?
     - would that wiring distort the mission into a narrower lane than it
       actually has?
4. Classify the current state.
   - `wire-now` when one devmap is clearly strongest.
   - `floating-by-design` when the mission is intentionally cross-devmap,
     substrate-like, or better left unparented for now.
   - `floating-pending-cleanup` when a parent likely exists but cannot yet be
     chosen honestly.
5. If `wire-now`, perform the bounded wiring update.
   - Update the relevant devmap or devmap-local docs.
   - Update lane catalogs, aliases, or supporting docs as needed so the new
     wiring is discoverable.
   - Physically move the mission only if the filesystem tree should now encode
     that owner directly.
6. If `floating-by-design`, record that explicitly.
   - State why the mission floats.
   - If helpful, note the devmaps it participates in without assigning one as
     parent.
7. If `floating-pending-cleanup`, preserve the continuation.
   - Record the candidate devmap parents if they are visible.
   - Record the trigger for revisiting the decision.
   - If several such missions are accumulating, route the wave into:
     - `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon3-onboarding-and-familiarization/M-futon3-devmap-onboarding-and-filesystem-backed-wiring-alignment/M-devmap-owned-mission-rewire-and-cleanup.md`
8. Run the orthogonality gate.
   - Do not ask whether a bounded follow-on pass should happen.
   - Determine and record the required orthogonality continuation now.
   - The follow-on orthogonality pass is not optional after mission creation or
     reconstruction.
   - Use the pass to:
     - deduplicate overlap
     - tighten mission scopes
     - improve mission/devmap boundaries
   - If the pass concludes that no broader cleanup is needed, record that
     trivial result explicitly rather than silently skipping the pass.
9. Publish the result in the artifact neighborhood.
   - The mission or its local docs should make the current wiring state legible:
     - `wire-now`
     - `floating-by-design`
     - `floating-pending-cleanup`

## Stop Conditions

Stop and ask the operator if any of these happen:

- multiple devmaps remain equally plausible owners after explicit-neighborhood
  inspection
- wiring now would require a broad physical/tree refactor rather than a bounded
  update
- the mission appears to be hiding two different scopes that should first be
  split
- the desired devmap itself is still being actively redefined

## Decision Rule: When Floating Is Acceptable

Floating is acceptable when at least one is true:

- the mission is a cross-devmap bridge or substrate lane
- the topology is still honestly unclear
- immediate parenting would distort the mission's scope
- the continuation is already captured and a cleanup trigger is recorded

Floating is not acceptable when:

- the mission has a clearly strongest devmap owner
- the absence of wiring is only inertia
- several similarly floating missions already justify a bounded cleanup wave

## Outputs

- an explicit devmap-wiring state for the mission:
  - `wire-now`
  - `floating-by-design`
  - `floating-pending-cleanup`
- any needed devmap/catalog/docs updates if wiring happened
- a completed orthogonality-pass result, including the explicit case where the
  pass ended trivially
- an explicit cleanup continuation when wiring is deferred

## References

- retroactive mission reconstruction owner:
  - `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
- devmap-wiring decision owner:
  - `library/system-architecture/devmap-wiring-decision-for-floating-and-retroactive-missions.flexiarg`
- mission topology owner:
  - `library/system-architecture/mission-neighborhoods-and-semantic-topology.flexiarg`
- recurring cleanup mission:
  - `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon3-onboarding-and-familiarization/M-futon3-devmap-onboarding-and-filesystem-backed-wiring-alignment/M-devmap-owned-mission-rewire-and-cleanup.md`
