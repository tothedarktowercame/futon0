# Algorithm: Author Algorithm Core and Map Companion Flexiargs

Date: 2026-03-14
Status: active

## Purpose

Use this after the algorithm lifecycle gate has already been decided and the
remaining work is the shared inner authoring/refactoring core for an
algorithm.

This algorithm owns the common inner handling shared by:
- `algorithms/create-algorithm.md`
- `algorithms/update-algorithm.md`

It is intentionally not the owner of the new-versus-existing decision. Its
callers decide that first, then delegate here.

Companion flexiarg definition:
- a companion flexiarg is a reusable policy, invariant, or tradeoff encoded as
  a flexiarg that governs how the algorithm should be executed or interpreted
  without flattening that judgment into fake linear procedure

Companion flexiarg creation/update is delegated to:
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

Non-negotiable algorithm-header rule:
- every algorithm authored or refactored through this core must open with a
  documentation-style header that states what the algorithm is trying to do,
  "like the documention of a method is done in standard programming languages"
- that opening header may include prerequisites/preconditions in the same
  header neighborhood when that makes the algorithm easier to understand
- an algorithm is not ready if a reader must enter the procedure section just
  to learn the intended effect of the algorithm

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - governs the core split between replayable procedure and reusable judgment
    surfaces that this algorithm is responsible for preserving
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first factoring of existing owners, helpers, and companion
    flexiargs before inventing new parallel structure
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
  - governs repo-local `data/tmp/...` placement when the algorithm being
    authored owns temp-producing behavior

Inspected but non-owning candidates:
- `library/social/create-versus-update-algorithm.flexiarg`
  - lifecycle gating belongs to the caller before this shared core is invoked
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - may govern narrower upstream-deference cases, but not the generic inner
    authoring/refactoring core

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- lifecycle mode:
  - `new`
  - `existing`
- the repeated lane or change pressure already judged to deserve algorithm work
- the target algorithm owner or intended owner location
- the current explicit mission owner, or a bounded decision to create one first
- nearby algorithms, flexiargs, docs, and missions in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. The caller has already decided the lifecycle mode honestly.
2. The explicit local mission neighborhood has been inspected before durable
   algorithm writeback.
3. The target owner or intended owner location is known.
4. The work is stable enough that a replayable procedural spine can be named.
5. When refining an algorithm through live use, do not assume the operator must
   be the main designer of the algorithm's procedural usability.
   - The agent may often be the stronger local authority on what would make
     the algorithm easier to execute faithfully in later runs.
   - The operator may therefore mainly confirm, correct, or add a few
     high-value distinctions.
   - Preserve operator authority over mission intent, scope, and acceptance,
     while allowing the agent to contribute strongly to procedural refinement.

## Opening Header Contract

Before an algorithm authored through this core is allowed to land, its opening
header must make the algorithm understandable before the reader enters the
procedure steps.

Required opening-header content:
- `Purpose` that summarizes the algorithm's intent and target effect, not only
  when to use it
- `Preconditions` or `Prerequisites` when the run depends on prior state
- scope / non-ownership boundaries when the algorithm delegates or excludes
  nearby concerns

Review rule:
- if the opening reads only like a trigger note ("use this when...") but does
  not also say what the algorithm is attempting to accomplish, the algorithm
  is not ready
- if the prerequisites are important to understanding the run, they belong in
  the opening header neighborhood rather than being left implicit

## Procedure

1. Bind the lifecycle mode and target owner.
   - If the mode is `new`:
     - confirm the strongest honest location and filename
   - If the mode is `existing`:
     - confirm the current algorithm file is still the strongest owner
   - If a helper extraction is now the stronger shape:
     - create that helper algorithm explicitly and refactor the caller(s) to
       dispatch to it rather than leaving the shared inner run implicit
2. Author or correct the opening header before refining the method body.
   - Make the opening header explain what the algorithm is trying to do, not
     only when it should be used.
   - Write that summary "like the documention of a method is done in standard
     programming languages".
   - Include `Preconditions` or `Prerequisites` in that header neighborhood
     when they are important to understanding the run.
   - If scope boundaries or delegated neighbors matter, make them explicit in
     the opening section rather than leaving them to inference from the step
     list.
3. Identify the procedural spine to preserve.
   - Write the smallest ordered list of steps that actually recurs.
   - Keep only replayable control flow:
     inputs, sequence, checkpoints, outputs, and stop conditions.
4. Mark recurring judgment surfaces that remain after procedural extraction.
   - For each surviving reusable policy/invariant/tradeoff, name the candidate
     companion flexiarg surface explicitly.
5. Author or refactor the algorithm artifact.
   - Keep ordered procedure explicit.
   - Keep stop conditions explicit.
   - Cite governing flexiargs instead of embedding long policy prose.
   - If the algorithm governs temp-producing behavior, point to the intended
     repo-local `data/tmp/...` owner path or to the flexiarg that governs that
     placement rather than leaving temp behavior implicit.
   - Make the lifecycle result legible in the artifact itself:
     - new owner
     - updated existing owner
     - extracted helper/child plus routed callers
6. If companion judgment surfaces are needed, delegate their creation/update.
   - Call:
     `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
   - Reuse/update existing flexiargs when they already own the boundary.
7. Map governing companion flexiargs into the resulting algorithm artifact.
   - Record:
     - governing companion flexiargs
     - inspected but non-owning candidates
     - the rule that governing companions are read/applied when the algorithm
       runs
8. Wire discoverability.
   - Update the owning mission.
   - If the algorithm lives under `algorithms/`, update:
     - `algorithms/README.md`
   - If the algorithm lives outside `algorithms/`, update the strongest local
     neighborhood index/README instead of forcing second-class placement.
   - Update relevant `library/*/README.md` indexes for any new/updated
     flexiargs.
9. Record reused versus new.
   - Name reused authorities and supporting artifacts.
   - Name new/refactored artifacts and cross-references created.
   - If the algorithm wave added a new connected topology or aha relation,
     first add the immediate local links that make that topology visible before
     reducing overlap.
10. Run the opening-header review before calling the algorithm done.
   - Check whether the opening header tells the reader what the algorithm is
     trying to accomplish before the procedure begins.
   - Check whether the header includes prerequisites/preconditions when they
     are important to understanding the run.
   - Check whether the opening reads like method documentation rather than a
     vague trigger sentence.
   - If any of those checks fail, revise the header before treating the
     algorithm as complete.
11. Run the follow-on orthogonality pass.
   - Treat this as the second step after the link-enrichment step above.
   - The pass is not optional.
   - Use:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`
   - If the pass concludes that no broader cleanup is needed, record that
     trivial result explicitly rather than silently skipping the pass.
   - If the remaining problem is specifically mission/devmap topology rather
     than general artifact-boundary cleanup, route the narrower question into:
     `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`

## Decision Rule

Default rule:
1. let create/update algorithms decide lifecycle mode first
2. once lifecycle mode is known, put the shared inner handling here
3. do not call an algorithm complete until its opening header explains the
   intent and any needed prerequisites in documentation-style form
4. keep reusable non-linear judgment and invariant policy in flexiargs by
   composing:
   `algorithms/create-flexiarg-from-recurring-judgment-surface.md`

## Outputs

- one of:
  - a new algorithm
  - an updated existing algorithm
  - a refactored algorithm family with an extracted helper/child owner
  - an algorithm plus companion flexiarg(s)
- updated owning mission and local discoverability links
- explicit governing companion-flexiarg mapping for the resulting algorithm,
  when companion flexiargs are required
- explicit companion-flexiarg resolution text embedded in the resulting
  algorithm artifact, not only in the caller algorithm or in chat

## Stop Conditions

Stop and return to the caller if:
- the lifecycle mode is still not actually settled
- the work has not stabilized enough to name a procedural spine honestly
- the same artifact would need to own too many unrelated concerns
- the target owner is still ambiguous after explicit-neighborhood inspection

## References

- `algorithms/create-algorithm.md`
- `algorithms/update-algorithm.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/decide-follow-on-orthogonality-and-devmap-wiring-after-mission-reconstruction.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `library/social/create-versus-update-algorithm.flexiarg`
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
