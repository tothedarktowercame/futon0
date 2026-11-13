# Algorithm: Seed or Redirect FrontierMath Room Work

Date: 2026-03-26
Status: active

## Purpose

Use this when the human supervisor needs to introduce one bounded work item
into the FrontierMath room or redirect an existing worker to a narrower task.

This algorithm owns the human side of:
- assigning work to `codex` or `math-mentor`
- redirecting an in-flight effort onto a narrower or more useful slice
- keeping the task prompt short and operational

## Family Companion Flexiarg Resolution

This algorithm inherits the governing companion posture from:
- `algorithms/frontier-math/README.md`

Local delta:
- bias toward one bounded ask at a time rather than issuing a burst of room
  prompts

## Inputs

- one target room agent:
  - usually `codex` or `math-mentor`
- one bounded task statement
- optional room context already established by the briefing algorithm

## Preconditions

1. The current room state is understood well enough to issue one honest task.
2. The task is narrow enough to fit inside one room prompt.
3. The task is not duplicating work already clearly in progress.

## Procedure

1. Decide whether the prompt is:
   - a fresh seed
   - a redirect
   - a request for a bounded review/narrowing
2. Address one room actor explicitly.
   - Prefer:
     - `@codex ...`
     - `@math-mentor ...`
     - `@math-curator ...`
   - Do not rely on bare `codex:` / `math-mentor:` wording when the room needs
     explicit mention syntax to wake the worker reliably.
   - Do not assume relay text such as:
     - `Recipient: @codex ...`
     - `Recipient: @math-curator ...`
     is itself an executed task. If a mentor or critic proposes the next
     prompt in that advisory shape, reseed it as a fresh direct `@mention`
     from the human supervisor.
3. Phrase the work as one bounded obligation.
   - include:
     - the concrete thing to do
     - the desired output shape
     - the current blocking focus if relevant
     - the intended local artifact root when file work is involved:
       - prefer `mfuton/...`
       - avoid ambiguous bare `data/...` references when the room should act
         on the local `mfuton` copy
   - avoid:
     - multiple unrelated asks in one prompt
     - vague motivational language
4. If the task is a redirect, name what should stop.
   - Example shape:
     - stop broad search
     - give one blocker summary
     - then test exactly one narrower route
5. If a canonical follow is needed, use the room-command surface with delivery
   follow rather than relying only on the short IRC capture window.
6. After issuing the task, wait for either:
   - a room-visible reply
   - a canonical job record
   - a bounded timeout that triggers the long-task supervision algorithm

## Outputs

- one bounded room work prompt
- a clear target actor
- either:
  - an accepted room task
  - an explicit visible reply
  - a canonical invoke job id for later supervision

## Stop Conditions

Stop and route elsewhere if:
- the room state is too unclear to issue work honestly
- the worker is already busy on a higher-priority task
- the prompt keeps growing into a multi-part plan and should instead be split
  into briefing plus later assignment

## References

- `algorithms/frontier-math/README.md`
- `algorithms/frontier-math/ask-frontiermath-room-for-briefing-and-current-state.md`
- `agent_skills/futon/irc-api/futon-irc-command.bat`
- `futon6/holes/missions/M-distributed-frontiermath.md`

## Current Example

Current bounded control prompt for the FM-001 `T3-search` lane:

`@codex next bounded T3-search control step: keep using mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/. Use the mfuton helper at mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/2026-03-26-generated-witness/scripts/fm001/generate_witness.py. Run exactly one n=3 check, prefer method=wesley, update mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/2026-03-26-n3-search.md with the result, and report exact file plus one-line result.`

Current artifact-cleanup example after mentor recommendation:

`@math-curator Use the mfuton-rooted paths. Lift the n=3 iteration table out of mfuton/data/tmp/FM-001-search-iterations.md into the T3-search artifact bundle alongside mfuton/data/frontiermath-local/FM-001/artifacts/T3-search/2026-03-26-n3-search.md, mark the tmp rows as migrated, and confirm the T3-search artifact bundle now holds the canonical iteration log per the README.`
