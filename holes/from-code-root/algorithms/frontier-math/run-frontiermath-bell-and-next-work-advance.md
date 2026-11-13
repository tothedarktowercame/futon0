# Algorithm: Run FrontierMath Bell and Next-Work Advance

Date: 2026-03-26
Status: active

## Purpose

Use this when one bounded room task is ready to advance and the human
supervisor wants to drive and verify the Bell → repost → next-work transition.

This algorithm owns the human-side procedure for:
- sending the FM Bell phrase through the room
- verifying `tickle` acknowledged it
- verifying the next `@codex` work item was reposted
- verifying the repost turned into a real codex job
- recognizing when Bell closes the slice into an honest idle state instead
  of reposting more work

## Family Companion Flexiarg Resolution

This algorithm inherits the governing companion posture from:
- `algorithms/frontier-math/README.md`

Local delta:
- the Bell phrase and mapped sender identity are critical local constraints

## Inputs

- one mapped codex-style speaker nick
- the Bell event text:
  - usually `BELL SPEC_VERIFIED`
- one FrontierMath room:
  - normally `#math`

## Preconditions

1. The room lane is alive and visible.
2. The Bell is being sent from a nick that the FM conductor accepts as mapped
   to the right agent family.
3. The supervisor is trying to advance work, not merely test room reachability.

## Procedure

1. Use the canonical Bell phrase.
   - current local phrase:
     - `tickle: BELL SPEC_VERIFIED`
2. Send it from a mapped codex-style speaker nick rather than an arbitrary
   probe nick.
3. Watch for the bounded Bell reply cluster.
   - accepted / queued
   - reposted `@codex ...` next work
   - Bell acknowledgement
4. If the Bell was acknowledged but no next work was posted, classify the
   result honestly.
   - If `tickle` says:
     - `No new assignable obligations right now.`
     treat that as a valid idle-state closure rather than generic failure.
   - If Bell was acknowledged with no repost and no explicit idle-state
     wording, record that as a distinct non-advancing result.
5. If next work was posted, verify it turned into a real codex job.
   - use:
     - room log / room reply
     - `!jobs`
     - canonical job lookup when the reposted job id is visible
6. If Bell closed into an idle state, route to one of:
   - `close-frontiermath-room-cycle-with-summary-and-handoff.md`
   - `ask-frontiermath-room-for-briefing-and-current-state.md`
   - `seed-or-redirect-frontiermath-room-work.md`
   depending on whether the supervisor wants to stop, summarize, or seed the
   next bounded obligation manually.
7. Hand the resulting codex task off to the long-task supervision algorithm if
   it becomes genuinely long-running.

## Outputs

- Bell accepted / rejected / acknowledged state
- whether next work was reposted
- whether reposted work turned into a real codex job
- or whether the Bell cleanly closed into an idle no-new-obligations state

## Stop Conditions

Stop and route elsewhere if:
- the Bell sender nick is not mapped correctly
- `tickle` is not visible or not running
- the room itself is unhealthy

## References

- `algorithms/frontier-math/README.md`
- `algorithms/frontier-math/supervise-long-running-frontiermath-room-task.md`
- `agent_skills/futon/irc-api/futon-irc-command.bat`
- `futon6/holes/missions/M-distributed-frontiermath.md`
