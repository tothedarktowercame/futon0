# Algorithm: Ask FrontierMath Room for Briefing and Current State

Date: 2026-03-26
Status: active

## Purpose

Use this when the human supervisor wants to re-enter the FrontierMath room
honestly before pushing new work.

This algorithm owns the bounded "what is happening right now?" prompt cycle
for the local `#math` lane:

- verify the visible room roster
- ask for a short current-state briefing
- avoid issuing new work based on stale assumptions

## Family Companion Flexiarg Resolution

This algorithm inherits the governing companion posture from:
- `algorithms/frontier-math/README.md`

Local delta:
- none

## Inputs

- one current FrontierMath room:
  - normally `#math`
- one visible room roster check
- one short supervisor ask

## Preconditions

1. The FrontierMath lane is already started.
2. The supervisor needs current room state, not a durable post-mortem.
3. The goal is to align on present state before seeding more work.

## Procedure

1. Verify room visibility first.
   - Use:
     - `agent_skills/irc-lane/irc-lane.bat snapshot ...`
   - Confirm the expected visible nick set for the current lane, normally:
     - `math-curator`
     - `math-mentor`
     - `codex`
     - `tickle`
2. Ask for a bounded briefing rather than an open-ended conversation.
   - Prefer a short mentor or codex prompt such as:
     - `@math-mentor brief the current state of #math in 3 short bullets`
     - `codex: state what you are currently doing and what is blocked`
3. If the room was recently restarted or a worker lost continuity, ask for the
   restart-brief form explicitly.
   - Preferred briefing shape:
     - what was completed
     - what is pending
     - what is blocked
     - what should happen next
4. Keep the briefing bounded.
   - If the room starts drifting into long narrative, ask for a shorter
     restatement instead of widening the thread.
5. Do not seed or redirect new work until the current state is aligned enough
   that a next step would be honest.

## Outputs

- one current visible-roster check
- one bounded room briefing
- one aligned read of:
  - what is running
  - what is blocked
  - what should happen next

## Stop Conditions

Stop and route to stronger supervision if:
- the expected room roster is missing key actors
- the room cannot produce a coherent briefing
- the result shows a currently running task that should be handled before any
  new prompt is added

## References

- `algorithms/frontier-math/README.md`
- `agent_skills/irc-lane/irc-lane.bat`
- `agent_skills/futon/irc-api/futon-irc-command.bat`
- `futon3/fulab/meetings/2026-01-27-irc-coordination-review.md`
