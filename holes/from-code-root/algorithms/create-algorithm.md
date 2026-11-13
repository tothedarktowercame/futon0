# Algorithm: Create Algorithm

Date: 2026-03-14
Status: active

## Purpose

Use this when repeated live work has become stable enough that `mfuton` should
promote a replayable procedural spine into a new algorithm owner, and there is
still an open question about whether the truthful move is:
- create a new algorithm
- update an existing algorithm instead
- stop at flexiarg/doc/mission level for now

This algorithm owns only the new-algorithm gate for algorithm authoring.

Opening-header gate:
- the resulting algorithm must begin with a documentation-style header that
  summarizes the algorithm's intent, "like the documention of a method is done
  in standard programming languages"
- when prerequisites are important to understanding the run, they may and
  usually should appear in that same opening header neighborhood
- if the drafted algorithm still needs the reader to enter the procedure
  section before understanding what the algorithm is trying to accomplish, it
  is not ready

Specialization gate:
- if the strongest truthful new owner is an operator-executed manual procedure,
  do not keep forcing the generic create path just because final placement may
  still need operator-lane discussion
- route instead to:
  `algorithms/operator/create-operator-algorithm.md`

It does not own the shared inner algorithm-authoring/refactoring work after the
new-versus-existing gate is crossed. That shared core now belongs to:
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`

It does not own generic document classification or routing. If the operator is
asking for a durable document but the strongest artifact class is still
unclear, use:
- `algorithms/create-document.md`

Companion flexiarg creation is delegated to:
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`

Default location note:
- the default location for new `mfuton` algorithms is:
  - `algorithms/`
- but an algorithm may honestly live elsewhere when the strongest owner is a
  narrower neighborhood such as:
  - a mission-local docs neighborhood
  - an operator algorithm neighborhood
  - a world/tracker-watch neighborhood
- treat algorithms like other program artifacts in this respect:
  default locations matter for discoverability, but they do not override the
  stronger local owner

Discussion and bound-input note:
- an explicit operator-bound target neighborhood or placement decision is an
  input to classify, not noise to override with a default
- treat the default algorithm locations like function default parameters:
  helpful when nothing stronger is bound, but not authority that silently wins
- if a question, challenge, or alternative placement reveals that algorithm
  class, owner, or location is still disputed, stay in discussion/alignment
  mode rather than crossing into writeback
- a question is not implicit execution handoff in this create lane

## Inputs

- repeated live work, case study, or recurring operator/evolver stretch
- the current explicit mission owner, or a decision to create one first
- nearby algorithms, flexiargs, docs, and missions in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. The work has recurred often enough, or become stable enough, that durable
   codification is being seriously considered.
2. The explicit local mission neighborhood has been inspected before creating
   new artifacts.
3. If no mission owns the lane yet and the work is non-trivial, create or
   reconstruct the mission first.
4. Before creating a brand-new algorithm file, check whether the stronger move
   is to update an existing algorithm owner first.
5. The candidate algorithm can honestly satisfy the opening-header gate
   described in this algorithm and in the shared authoring core.
6. The current shared model of artifact class, owner, and placement is
   sufficient for writeback; if it is still being discussed, stop there rather
   than mutating artifacts.

## Procedure

1. Start from a real repeated lane, not a hypothetical one.
   - Identify the concrete work that keeps recurring.
   - Name the support evidence: transcripts, receipts, diffs, notes, or prior
     algorithms.
2. Inspect explicit reuse first.
   - Check `gh/mfuton` for nearby algorithms, flexiargs, missions, and docs.
   - Check `darktower/futon3c` for upstream or adjacent patterns that already
     cover part of the lane.
   - Reuse or refine an existing owner when that is cleaner than creating a
     near-duplicate.
3. Decide algorithm lifecycle shape first.
   - Reuse the decision gate:
     `library/social/create-versus-update-algorithm.flexiarg`
   - Decide whether this wave should honestly:
     - create a new algorithm
     - update an existing algorithm instead
     - stop at flexiarg-only or discussion/mission/doc level
4. Bind explicit owner and placement inputs before routing through defaults.
   - If the operator has already named the target neighborhood or placement,
     treat that as a bound input to classify.
   - Use `algorithms/` and `algorithms/operator/` as discoverability defaults
     only when no stronger local owner or explicit operator-bound location has
     already been established.
   - If artifact class, owner, or placement is still under active discussion,
     stop at discussion/alignment level rather than silently writing toward the
     default lane.
5. Gate operator-executed procedures into the narrower operator-algorithm lane.
   - If the strongest truthful owner is an operator-executed manual procedure,
     do not keep the generic create path here just because final placement may
     still need operator-lane discussion.
   - Call:
     `algorithms/operator/create-operator-algorithm.md`
   - Stop this algorithm after handing off.
6. If the stronger move is to update an existing algorithm owner, do not keep
   forcing the create path.
   - Call:
     `algorithms/update-algorithm.md`
   - Stop this algorithm after handing off.
7. Handle flexiarg-only or not-yet-algorithm-ready outcomes explicitly.
   - If there is no stable ordered spine and only recurring judgment remains:
     - do not create an algorithm
     - call:
       `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
     - stop this algorithm after the flexiarg path is complete
   - If the lane is still too novel for durable procedure:
     - stop cleanly at mission/doc/discussion level instead of creating
       algorithm churn
8. Before writeback, rerun the discussion-to-writeback gate.
   - If a question, challenge, or location disagreement is still live, do not
     cross into authoring or shared-core handoff yet.
   - Only continue once the shared model of artifact class, owner, and
     placement is sufficient for writeback.
9. If a new algorithm owner is the truthful result, call the shared
   authoring core.
   - Use:
     `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
   - Pass the lifecycle mode as:
     - `new`
   - Let that shared core own:
     - opening-header shaping and review
     - procedural-spine shaping
     - companion-flexiarg handling
     - discoverability wiring
     - reused-versus-new reporting
     - temp-placement policy routing when the algorithm governs temp-producing
       behavior
10. If a follow-on artifact-boundary cleanup is still wanted after the shared
   core runs, use:
   - `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - or, for narrower mission/devmap topology pressure:
     `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`

## Decision Rule

Default factoring rule:
1. use this algorithm only to decide whether the truthful move is still
   creation of a new algorithm owner
2. treat explicit operator-bound placement as an input; default locations do
   not silently override it
3. if a question, challenge, or location dispute is still live, stay in
   discussion/alignment rather than crossing into writeback
4. if the truthful new owner is an operator-executed manual procedure, route
   to:
   `algorithms/operator/create-operator-algorithm.md`
5. do not create a new algorithm that lacks a documentation-style opening
   header explaining the intent and any needed prerequisites
6. once the answer is yes for a general algorithm owner and the shared model is
   sufficient for writeback, compose the shared
   authoring core:
   `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
7. keep reusable non-linear judgment and invariant policy in flexiargs by
   composing:
   `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

## Outputs

- one of:
  - a new algorithm via the shared authoring core
  - a routed handoff into `algorithms/operator/create-operator-algorithm.md`
  - a routed handoff into `algorithms/update-algorithm.md`
  - an explicit flexiarg-only routing into
    `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
  - an explicit decision that the lane is not yet algorithm-ready
- explicit classification of whether default placement was used or overridden by
  a stronger local owner or explicit operator-bound neighborhood
- downstream shared-core output that carries:
  - owning mission and local discoverability links
  - governing companion-flexiarg mapping when required
  - explicit companion-flexiarg resolution text embedded in the resulting
    algorithm artifact, not only in chat

## Stop Conditions

Stop and keep the lane at mission/doc/flexiarg level if:
- the work has not actually stabilized
- the ordered steps are still mostly ad hoc
- a stronger existing owner should simply be updated instead
- artifact class, owner, or placement is still under active discussion
- the same artifact would need to own too many unrelated concerns

## Case Anchors

- the create/update split plus shared inner authoring core in this same
  algorithm family
- companion flexiarg creation owner:
  `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

## References

- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `docs/mission-creation-checklist.md`
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
- `algorithms/operator/create-operator-algorithm.md`
- `algorithms/update-algorithm.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring/M-create-algorithm-discussion-gates-and-default-location-contract.md`
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
- `library/social/create-versus-update-algorithm.flexiarg`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
