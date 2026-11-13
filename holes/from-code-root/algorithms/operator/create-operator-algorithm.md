# Algorithm: Create Operator Algorithm

Date: 2026-03-19
Status: active

## Purpose

Use this when repeated live work has become stable enough that `mfuton` should
promote a new operator-executed manual procedure into an operator algorithm
owner, with `algorithms/operator/` as the default discoverability neighborhood
when nothing stronger is already bound, and there is still an open question
about whether the truthful move is:
- create a new operator algorithm
- update an existing operator algorithm instead
- stop at mission/doc/flexiarg level for now

This algorithm is the operator-neighborhood specialization of:
- `algorithms/create-algorithm.md`

Non-negotiable opening rule:
- the resulting operator algorithm must be complete enough that the operator
  can follow it without needing to ask for missing execution details in chat
- if the agent still needs chat to supply omitted values, omitted branches, or
  omitted file contents, the artifact is not yet ready to land as an operator
  algorithm
- the resulting operator algorithm must also begin with a documentation-style
  header that states what the algorithm is trying to do, "like the
  documention of a method is done in standard programming languages"
- when prerequisites are important to understanding the run, they may and
  usually should be included in that same opening header neighborhood

It owns the additional gates that make an artifact honestly ready to be called
an operator algorithm:
- the operator-facing procedure must be self-contained
- the operator-facing steps must contain the currently bound execution details
- the artifact must not depend on hidden chat state or remembered packet values
- the opening header must summarize the intended effect before the step list
- the algorithm may assume the operator already has a text editor and does not
  need generic editor-usage instruction

It does not own the shared inner algorithm-authoring/refactoring work after the
new-versus-existing gate is crossed. That shared core still belongs to:
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`

## Companion Flexiarg Resolution (2026-03-19)

Governing companion flexiargs for this algorithm:
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
  - governs the carrier boundary that keeps this lane about replayable
    operator-run procedure rather than reusable non-linear judgment
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first inspection of the existing operator-algorithm
    neighborhood before creating a new file
- `library/foundational/respond-to-questions-with-sources.flexiarg`
  - governs rereading the governing operator artifact when a question or
    challenge reveals that remembered context may be incomplete or wrong

Inspected but non-owning candidates:
- `library/social/create-versus-update-algorithm.flexiarg`
  - lifecycle pressure is real here, but this specialized operator lane keeps
    the create-side gate in algorithm form rather than making the flexiarg the
    direct owner
- `library/social/knowledge-factoring-pass-naming-and-subtypes.flexiarg`
  - governs naming and subtype cleanup after creation pressure is already
    honest, not the full operator-algorithm creation gate

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Authority routing

- generic create-algorithm gate:
  `algorithms/create-algorithm.md`
- shared inner algorithm-authoring core:
  `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
- operator-algorithm local owner/index:
  `algorithms/operator/README.md`
- algorithm-creation mission owner:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`

## Default Location and Discussion Gate

Default discoverability rule:
- the default location for new operator algorithms is:
  - `algorithms/operator/`
- but like other program artifacts, this default does not override:
  - a stronger local owner
  - an explicit operator-bound neighborhood or placement decision
- treat this default like a function default parameter:
  useful when no stronger input is bound, but not a mandatory override

Discussion/handoff rule:
- while artifact class, owner, or placement is still being aligned, stay in
  discussion mode rather than crossing into writeback
- a question, challenge, or alternative placement from the operator is not
  automatic execution handoff
- if the lane still needs alignment on class, owner, or placement, stop there
  honestly instead of silently authoring toward `algorithms/operator/`

## Inputs

- repeated live operator-facing procedural work
- the current explicit mission owner, or a bounded decision to create one first
- any explicit operator-bound neighborhood or placement that is already part of
  the live discussion
- nearby operator algorithms, general algorithms, docs, and missions in:
  - `gh/mfuton`
  - `darktower/futon3c`

## Preconditions

1. The work has recurred often enough, or become stable enough, that durable
   operator-facing codification is being seriously considered.
2. The explicit local mission neighborhood has been inspected before creating a
   new artifact.
3. The operator-executed procedure is truly replayable and bounded, not merely
   a one-off manual note.
4. Before creating a brand-new operator algorithm, check whether the stronger
   move is to update an existing operator algorithm owner first.
5. The candidate operator algorithm can honestly satisfy the self-contained
   detail gate described below.
6. The current shared model of artifact class, owner, and placement is
   sufficient for writeback; if not, remain in discussion/alignment.

## Self-Contained Detail Gate

Before a new artifact is allowed to land as an operator algorithm, all of these
must be true:

- all execution details needed by the operator are present in the algorithm
  itself
- the artifact does not require remembered chat context to fill in missing
  values
- the artifact does not rely on unresolved placeholders once the current run
  has already bound concrete values
- if the operator must edit a file, the algorithm specifies:
  - which file to edit
  - what exact lines or content must be present
- the artifact may assume the operator already has a text editor and does not
  need generic text-editor instruction

If this gate fails, the artifact is not yet ready to be called an operator
algorithm.

## Opening Header Gate

Before a new artifact is allowed to land as an operator algorithm, its opening
header must let the operator understand the algorithm before entering the
numbered steps.

Required opening-header content:
- `Purpose` that summarizes the algorithm's intent and target effect, not only
  when to use it
- `Preconditions` or `Prerequisites` when the run depends on prior state
- scope / non-ownership boundaries when those distinctions affect execution

Review rule:
- if the opening reads only like a trigger sentence and does not say what the
  algorithm is trying to accomplish, the artifact is not ready
- if important prerequisites exist, they belong in the opening header
  neighborhood rather than being left implicit in later steps

## Procedure

1. Start from a real repeated operator-run lane, not a hypothetical one.
   - Identify the concrete manual procedure that keeps recurring.
   - Name the support evidence: transcripts, receipts, diffs, or current
     operator algorithms that expose the same pressure.
2. Inspect explicit reuse first.
   - Check `gh/mfuton/algorithms/operator/` for an existing stronger owner.
   - Check `gh/mfuton/algorithms/` for a broader generic owner that should be
     refined rather than duplicated.
   - Check `darktower/futon3c` for upstream-adjacent patterns.
   - Prefer updating an existing owner when that is cleaner than creating a
     near-duplicate.
3. Bind explicit operator placement before defaulting.
   - If the operator has already named the target neighborhood or placement,
     treat that as a bound input to classify.
   - Use `algorithms/operator/` only as the default discoverability lane when
     no stronger local owner or explicit operator-bound location has already
     been established.
   - If class, owner, or placement is still under active discussion, stop at
     discussion/alignment level rather than silently writing toward the default
     lane.
4. Decide create-new versus update-existing.
   - If an existing operator algorithm already owns the lane, do not force a
     new file.
   - Route to:
     `algorithms/update-algorithm.md`
     when update pressure is stronger than new-owner pressure.
   - Stop this algorithm after handing off.
5. Confirm the work is really operator-algorithm shaped.
   - Keep it here only if:
     - the operator is the intended primary executor
     - the steps are replayable and manual rather than ad hoc narrative
   - If the lane is still mainly reusable judgment, route to:
     `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
   - If the lane is still too novel for durable procedure, stop honestly at
     mission/doc/discussion level.
6. Apply the self-contained detail gate before writing.
   - If the procedure still depends on hidden packet values, remembered chat
     context, or unresolved placeholders, do not create the operator algorithm
     yet.
   - First bind and write the concrete details into the operator-facing
     artifact.
7. Recheck the discussion-to-writeback gate before authoring.
   - Do not cross into artifact mutation until the shared model of artifact
     class, owner, and placement is sufficient.
   - If a question, challenge, or location dispute is still live, stop and
     align rather than proceeding on default momentum.
8. Author the opening header before finalizing the operator method body.
   - Make the opening header explain what the operator algorithm is trying to
     accomplish, not only when to use it.
   - Write that summary "like the documention of a method is done in standard
     programming languages".
   - Include `Preconditions` or `Prerequisites` in that header neighborhood
     when they are important to understanding the run.
9. Shape the operator-facing procedural spine.
   - The operator algorithm should normally contain:
     - one global failure rule
     - explicit operator actions
     - explicit continue/stop conditions for command results
     - bounded branch points only
     - a clear handoff back to the agent when privileged or manual work ends
   - Do not let vague stage titles stand in for the real operator obligation.
10. Keep the text-editor assumption explicit.
   - Assume the operator already has a text editor.
   - Name the file to edit and the exact content to place there.
   - Do not include generic editor-usage instructions unless a narrower mission
     explicitly makes editor operation part of the task.
11. If a new operator algorithm owner is the truthful result, call the shared
   authoring core.
   - Use:
     `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
   - Pass the lifecycle mode as:
     - `new`
   - Keep `algorithms/operator/` as the default discoverability owner only when
     no stronger local owner or explicit operator-bound placement has already
     been established.
12. Wire discoverability honestly.
   - If the resulting owner actually lives under `algorithms/operator/`,
     update:
     - `algorithms/operator/README.md`
     - `algorithms/README.md`
   - If the resulting owner honestly lives in another neighborhood, update that
     local discoverability surface instead of forcing the operator indexes to
     pretend ownership they do not have.
13. Record reused versus new.
    - Name reused authorities and supporting artifacts.
    - Name the new operator algorithm and the cross-references created.
    - If the operator algorithm introduced a new connected neighborhood or aha
      relation, first add the obvious local links before reducing overlap.
    - Record explicitly whether the final placement used the default
      discoverability lane or an explicit/stronger-owner override.
14. Run the operator-doability review loop before calling the artifact done.
    - Iterate through the drafted operator algorithm step by step.
    - For each step, check all of these:
      - the opening header tells the operator what the algorithm is trying to
        accomplish before the procedure begins
      - the opening header includes prerequisites when they are important to
        understanding the run
      - the step's purpose is explicit
      - the step is logically reachable from the prior steps
      - the step is actually doable with the information and state produced by
        the prior steps
      - the step does not depend on hidden chat context or unstated packet
        values
      - any branch condition tells the operator exactly what to do in each
        branch
    - If any step fails one of those checks, revise the operator algorithm and
      restart the review loop from the beginning rather than only patching the
      local line mentally.
    - Do not call the operator algorithm complete until a full pass succeeds.
15. Run the follow-on orthogonality pass.
    - Treat this as the second step after the link-enrichment step above.
    - The pass is not optional.
    - Use:
      `algorithms/conduct-knowledge-orthogonalization-pass.md`
    - If the pass concludes that no broader cleanup is needed, record that
      trivial result explicitly rather than silently skipping the pass.

## Decision Rule

Default rule:
1. use this algorithm only when the truthful outcome is specifically a new
   operator-run procedure owner
2. treat `algorithms/operator/` as the default discoverability lane, not as a
   mandatory override of explicit operator placement or a stronger local owner
3. if class, owner, or placement is still under live discussion, stay in
   discussion/alignment rather than crossing into writeback
4. do not create the operator algorithm until the self-contained detail gate
   is satisfied
5. do not create the operator algorithm until the opening header explains the
   intent and any needed prerequisites in documentation-style form
6. do not call the operator algorithm complete until the full operator-
   doability review loop succeeds across every step
7. once the create decision, placement decision, and shared model are honest,
   compose the shared authoring core:
   `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`

## Outputs

- one of:
  - a new operator algorithm via the shared authoring core
  - a routed handoff into `algorithms/update-algorithm.md`
  - an explicit flexiarg-only routing into
    `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
  - an explicit decision that the lane is not yet operator-algorithm-ready
- explicit classification of whether default placement was used or overridden by
  a stronger local owner or explicit operator-bound neighborhood
- updated operator/local discoverability links
- an explicit completed doability-review pass over the resulting operator
  algorithm, or a refusal to land it until that pass succeeds
- explicit reused-versus-new reporting for the operator-algorithm wave

## Stop Conditions

Stop and keep the lane at mission/doc/flexiarg level if:
- the work has not actually stabilized
- the operator is not really the intended executor
- the artifact still depends on hidden context or unresolved placeholders
- a stronger existing operator algorithm owner should simply be updated instead
- artifact class, owner, or placement is still under active discussion
- the same artifact would need to own too many unrelated concerns

## References

- `algorithms/create-algorithm.md`
- `algorithms/update-algorithm.md`
- `algorithms/author-algorithm-core-and-map-companion-flexiargs.md`
- `algorithms/create-flexiarg-from-recurring-judgment-surface.md`
- `algorithms/conduct-discussion-on-idea-before-writeup-or-promotion.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
- `algorithms/operator/README.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-algorithm-creation-and-flexiarg-factoring/M-create-algorithm-discussion-gates-and-default-location-contract.md`
- `library/social/algorithms-own-procedural-spines-flexiargs-own-judgment-surfaces.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/foundational/respond-to-questions-with-sources.flexiarg`
