# Algorithm: Close FrontierMath Room Cycle with Summary and Handoff

Date: 2026-03-26
Status: active

## Purpose

Use this when a bounded FrontierMath room task has reached a meaningful local
finish and the human supervisor wants to turn that room work into a durable,
operator-usable outcome.

This algorithm owns the human side of asking for:
- a short result summary
- the git/artifact consequence
- the next handoff or next-room step

## Family Companion Flexiarg Resolution

This algorithm inherits the governing companion posture from:
- `algorithms/frontier-math/README.md`

Local delta:
- this algorithm is summary-and-handoff oriented rather than task-seeding

## Inputs

- one finished or meaningfully blocked room task
- optional canonical job id
- optional visible room result already posted

## Preconditions

1. The bounded task has actually reached a local stopping point.
2. The supervisor wants a reusable outcome rather than just seeing the room
   message scroll by.
3. The result is narrow enough to summarize honestly.

## Procedure

1. Confirm the current task is truly at a local stopping point.
   - complete
   - blocked with named obstruction
   - handed off to a next room step
2. Ask for a short summary rather than a full retrospective.
   - preferred shape:
     - what changed
     - what is blocked
     - what the next step should be
3. Ask explicitly for the git/artifact consequence when relevant.
   - what file or artifact changed
   - whether the result is committed or still local
4. Ask for the next handoff or next-room step.
   - Bell now
   - ask mentor now
   - narrow codex again
   - stop and record blocker
5. Distinguish recommendation text from executable room work.
   - If the room returns something shaped like:
     - `Recipient: @codex ...`
     - `Recipient: @math-curator ...`
     treat it as a recommendation for the human supervisor to reseed through
     the seeding algorithm, not as proof that the next worker has already been
     invoked.
6. If the result is good enough to advance the room, route to the Bell
   algorithm.
7. If Bell later closes into:
   - `No new assignable obligations right now.`
   that is an honest local finish for the bounded slice. Record the closure
   and stop instead of forcing a fake follow-on task.
8. If the result is mainly a blocker or local finding, route into mission or
   artifact writeback rather than pretending the room itself finished the whole
   process.

## Outputs

- one bounded summary
- one git/artifact state read
- one explicit next handoff recommendation

## Stop Conditions

Stop and route elsewhere if:
- the task is not actually at a local stopping point
- the room result is still too ambiguous to summarize honestly
- the next step is implementation or mission writeback rather than another room
  prompt

## References

- `algorithms/frontier-math/README.md`
- `algorithms/frontier-math/run-frontiermath-bell-and-next-work-advance.md`
- `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon6-onboarding-and-familiarization/M-frontiermath-proof-stack-and-classic-code-familiarization/M-solo-frontiermath-forum-codex-compatible-enactment.md`
