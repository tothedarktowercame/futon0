# Algorithm: Supervise Long-Running FrontierMath Room Task

Date: 2026-03-26
Status: active

## Purpose

Use this when a FrontierMath room task has been running long enough that the
human supervisor needs to decide whether it is:

- healthy but slow
- ready for a bounded check-in
- ready for narrowing or interruption

This algorithm exists because real FrontierMath room work can honestly take
minutes, and short follow windows can otherwise create false "broken"
impressions.

## Family Companion Flexiarg Resolution

This algorithm inherits the governing companion posture from:
- `algorithms/frontier-math/README.md`

Local delta:
- this algorithm treats long runtime as normal until evidence says otherwise

## Inputs

- one current room job:
  - job id if known
  - otherwise one recent room prompt and actor
- one bounded observation window
- optional current room log / canonical job snapshot

## Preconditions

1. The room task is already in progress.
2. The supervisor wants to classify the current state honestly rather than
   reflexively restart the lane.
3. The room-command surface or HTTP ledger is available for checking progress.

## Procedure

1. Start from the assumption that real FrontierMath work may legitimately run
   longer than the short default IRC capture window.
2. Query canonical job state first when available.
   - Use:
     - `futon-irc-command.bat job <id> ...`
     - or the HTTP job surface
3. Classify the current state.
   - `running` + live worker process + no crash signal:
     - treat as healthy-but-running
   - `done` + `delivery=delivered`:
     - treat as complete
   - `done` + room reply visible but delivery still unclear:
     - re-check after a short bounded delay before concluding mismatch
   - no progress signal + no live child + no room reply:
     - escalate toward intervention
4. If the task is healthy-but-running, prefer a bounded later re-check rather
   than piling on a second task immediately.
5. If a human check-in is warranted, ask for a narrow blocker/status report.
   - preferred prompt shape:
     - what is blocked
     - what was tried
     - what the next narrow step should be
6. If the task is truly wedged, route to the stronger intervention owner.
   - do not silently fold interruption logic into ordinary supervision
7. Preserve the difference between:
   - room-side visible reply
   - canonical job ledger state
   - operator inference

## Outputs

- one honest classification:
  - healthy-but-running
  - complete
  - needs narrowing prompt
  - likely wedged and needs intervention
- optional supervisor prompt for blocker summary or narrowing

## Stop Conditions

Stop and route to a stronger owner if:
- interruption or restart is now actually required
- the room process itself is no longer alive
- the task state cannot be classified honestly from available evidence

## References

- `algorithms/frontier-math/README.md`
- `agent_skills/futon/irc-api/futon-irc-command.bat`
- `src/mfuton/futon/json_over_http_api/invoke_client.py`
- `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon6-onboarding-and-familiarization/M-frontiermath-proof-stack-and-classic-code-familiarization/M-solo-frontiermath-forum-codex-compatible-enactment.md`
