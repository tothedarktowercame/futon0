# Algorithm: Conduct Post-Mortem Learning-Capture Discussion

Date: 2026-03-09
Status: active

## Authority routing

- post-mortem gate:
  `library/social/post-mortem-for-novel-learning-capture.flexiarg`
- automation-extraction gate:
  `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
- adjacent continuity / retroactive-ownership policy:
  `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
- pragmatic operational codification owner:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification.md`
- software-development operating posture:
  `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-my-familiar-software-development-operating-posture.md`

## Purpose

Use this when a mission, repair, or exploratory run has already passed the
novelty gate and you want to run the post-mortem as an actual discussion rather
than silently writing a retrospective note from one side.

This algorithm owns the missing `how` boundary between:

1. deciding that post-mortem mode is warranted
2. having the operator and evolver iteratively discuss the run across multiple
   turns
3. converging on an updated mutually communicated read of what happened, what
   changed immediately, what should be promoted, and what should remain
   ordinary evidence
4. pausing honestly when the discussion is not finished or the operator has not
   yet told `codex` to `run the commit algorithm`

This algorithm is discussion-first.
It should be expected to take multiple turns when the operator is actively
refining the read.
It should not treat finishing the discussion as implicit permission to check in
the result.
The operator tells `codex` when to `run the commit algorithm`.

This algorithm is broader than:
- `algorithms/process-codex-learning-experience.md`

Use that narrower owner when the real task is a focused shared learning
experience that should grow or revise current self/edge/collaboration
world-model carriers directly rather than running a broader run-wide
post-mortem across multiple learning deltas, promotion decisions, or continuity
questions.

Important distinction:

- a post-mortem is materially different from a learning experience
- a post-mortem is primarily about technical issues found, operational issues,
  interpretation of what happened, immediate fixes, promotion decisions, and
  unresolved follow-on work
- a post-mortem may reveal or spawn an associated learning experience, but it
  does not necessarily do so
- therefore an evolver entering post-mortem mode is not, by that fact alone,
  obligated to reread its world-model neighborhood first
- only when the post-mortem exposes a genuine self/operator/edge/collaboration
  learning delta that should durably revise world-model carriers should that
  narrower slice be routed to
  `algorithms/process-codex-learning-experience.md`

## Inputs

- mission, issue, or run being analyzed
- current evidence set:
  - logs
  - commands
  - receipts
  - PRs
  - notes
  - chat summary
- any already-started post-mortem note or draft
- the operator's current read of what mattered

## Preconditions

1. The novelty gate has already classified the run as `novel-learning`, not
   merely `routine-replay`.
2. Do not treat a one-shot evolver summary as a completed post-mortem.
3. A post-mortem note may begin as a draft scaffold, but it is not complete
   until the operator/evolver discussion has iterated over its main claims.
4. Treat the post-mortem as a multi-turn discussion by default.
5. The post-mortem algorithm owns the discussion flow, not implicit check-in
   authority.
6. The governing companion flexiargs still own the reusable policy:
   - when learning capture should happen
   - how stable subprocedures should be promoted once identified
   - when non-linear judgment should stop, defer, or promote rather than being
     flattened into fake procedural certainty
7. Entering post-mortem mode does not, by itself, require rereading the
   evolver's world-model neighborhood first.
8. If the post-mortem exposes a narrower world-model learning delta, route
   that slice explicitly to:
   `algorithms/process-codex-learning-experience.md`
   rather than silently treating the broader post-mortem as if it had already
   completed that narrower owner.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/post-mortem-for-novel-learning-capture.flexiarg`
  - governs when the run belongs in post-mortem mode at all
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs how stable replayable subprocedures discovered during the
    discussion should be promoted

Inspected but non-owning candidates:
- `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
  - governs retroactive owner reconstruction and continuity, but not the
    direct learning-capture discussion flow

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Procedure

1. Confirm that post-mortem mode is actually warranted.
   - Reuse the novelty gate.
   - If the run is only routine replay, record that ordinary evidence is
     enough and stop.
2. Create or open the post-mortem carrier.
   - If no artifact exists, create a short draft note.
   - If one already exists, mark it as a draft or discussion-started artifact
     until the discussion has actually happened.
3. Start the discussion explicitly.
   - Tell the operator that the next step is an iterative post-mortem
     discussion, not a finished write-up.
   - Say that the discussion may take multiple turns.
   - Treat the note as a mutable scaffold, not an authority artifact yet.
4. Iterate through the canonical sections with the operator.
   - `why this qualified`
   - `what worked`
   - `what did not work`
   - `why it happened`
   - `what we changed immediately to improve`
   - `what was promoted`
   - `what was not promoted`
   - `what classical subprocedure was extracted`
   - `what continuity or unresolved follow-on remains`
5. For each section, use the same pattern.
   - propose a short draft from the evidence
   - let the operator correct, narrow, or replace it
   - update the note immediately so the artifact tracks the discussion
   - preserve uncertainty explicitly instead of smoothing it away
   - if the next move is not classically determined, reuse the governing
     flexiarg(s) as the local choice policy instead of pretending the flow is
     linear
6. Separate four kinds of outcomes.
   - interpretation:
     - what we now think happened
   - immediate improvement:
     - what was changed right away because the post-mortem surfaced an
       actionable correction or cleanup
     - this section may honestly be `nothing`
     - do not blur it with broader promotion if the change was only a local
       improvement step
   - promotion:
     - what artifact should be created or updated
   - non-promotion:
     - what should remain ordinary evidence or a one-off note
7. When a stable deterministic subflow is identified, run the extraction gate.
   - reuse
     `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
   - decide whether the classical part should become:
     - an algorithm
     - a script or wrapper
     - a typed command
     - a checklist or runbook
   - if only part of the flow is classical, keep the judgment boundary
     explicit rather than over-automating
8. Check authority routing before promoting semantics.
   - if the lesson changes futon's own meaning or existing upstream theory,
     inspect futon installation authorities first
   - keep local artifacts as overlays, deltas, or explicit proposals when the
     upstream authority is stronger
   - if the discussion has now exposed a genuine self/operator/edge/
     collaboration world-model delta, route that narrower slice to:
     `algorithms/process-codex-learning-experience.md`
     rather than forcing the broader post-mortem to impersonate that owner
9. Finalize or pause honestly.
   - If the operator says the discussion is complete:
     - mark the post-mortem note as captured/final
     - record the promoted artifacts or the explicit decision not to promote
   - If the discussion stops partway:
     - keep the note in a draft/discussion-started state
     - leave a short continuity note naming the next missing sections or
       promotion decisions
   - If the discussion terminates because no more high-signal deltas remain:
     - treat that as a valid stopping condition, not as an incomplete failure
     - record that the current note reflects the operator/evolver stopping read
       at this point in the loop
10. Pause before check-in.
    - Do not assume the writeback should be committed immediately.
    - Wait for the operator to tell you to `run the commit algorithm`.
    - If the operator has not said that yet, leave the updated artifacts as
      local working state and report the current stopping point honestly.

## Outputs

- a post-mortem note that is either:
  - `discussion-started-draft`, or
  - captured/final after the operator discussion is complete
- an explicit immediate-improvement list, or an explicit decision that nothing
  was changed immediately
- explicit promotion or non-promotion decisions
- extracted classical subprocedure artifacts when justified
- continuity state when the post-mortem itself is unfinished
- an explicit statement of whether the operator has or has not yet told
  `codex` to `run the commit algorithm`

## Notes

- This algorithm is intentionally discussion-first.
- It is expected to take multiple turns when the operator is actively refining
  the read.
- `algorithms/process-codex-learning-experience.md` is the narrower owner when
  the real task is a focused learning-experience discussion that should grow or
  revise the current world-model neighborhood directly rather than a broader
  run-wide post-mortem.
- entering post-mortem mode does not, by itself, impose the learning-
  experience world-model reread gate
- if a post-mortem later reveals a bounded world-model learning delta, that is
  a reason to spawn or route to the narrower learning-experience owner, not a
  reason to pretend the two algorithms are the same
- The flexiargs remain the governing policy surfaces; this algorithm is the
  operational program for carrying that policy out.
- A good post-mortem is iterative and corrigible. It should not pretend to have
  consensus or certainty that the discussion did not actually produce.
- In practice, flexiargs often function here as non-linear decision policies:
  they help choose whether to continue, stop, defer, or promote when the next
  move is not yet classically determined by a fixed procedure.
- The `what we changed immediately to improve` section is intentionally allowed
  to be empty. The point is to make immediate repair/codification triggered by
  the post-mortem visible when it happens, not to force change every time.
- Stop when the current shared view is sufficient to complete the task, nothing
  more, nothing less.
