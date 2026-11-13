# Algorithm: Create mfuton Issue Before Triage

Date: 2026-03-08
Status: active

## Authority routing

- issue/case owner:
  `holes/missions/M-futon-full-system-operation-and-onboarding/M-world-backed-local-issue-case-system.md`
- issue-system README:
  `docs/M-world-backed-local-issue-case-system/README.md`
- record-shape note:
  `docs/M-world-backed-local-issue-case-system/canonical-record-shapes-and-first-layout.md`
- backing/projection note:
  `docs/M-world-backed-local-issue-case-system/world-backing-and-projection-boundary.md`
- episode-to-owner factorization:
  `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`

## Purpose

Use this when an issue first arrives for `mfuton` through conversation or
another external surface and you want a repeatable pre-triage flow for:

1. talking with the reporter just enough to make the issue legible
2. writing the issue up under `world/issues/` as the local canonical record
   for `mfuton` local execution
3. preserving what is reported versus what is actually known
4. optionally linking a projected external issue surface such as GitHub Issues
5. stopping cleanly at the triage boundary instead of silently drifting into
   diagnosis
6. preserving the discovery/trigger context that explains why the issue
   surfaced in this specific work episode
7. keeping this lane distinct from futon-origin issue proxies under
   `world/futon-issues/<futon>/...`

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
  - governs capture-now, owner-later factorization for weakly structured issue
    intake before triage
- `library/system-architecture/deterministic-evidence-authority.flexiarg`
  - governs the separation between reported claims, supplied evidence, and
    later interpretation so narrative intake does not masquerade as verified
    fact

Inspected but non-owning candidates:
- `library/social/time-anchored-debugging-hypothesis-priors.flexiarg`
  - relevant once diagnosis starts, but this algorithm stops before triage
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - relevant to closure/verification after repair, not intake before triage

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- raw operator report, conversation, or other issue ingress
- source surface, source entity, and source artifact when available
- current work context when available:
  - active mission
  - active lane
  - current verification, refactor, or debug pass
  - current script, test, probe, or review step
- any directly supplied logs, screenshots, URLs, repro snippets, or transcripts
- optional external issue/posting surface:
  - GitHub issue
  - chat post
  - email/thread
  - local note

## Preconditions

1. The first task here is faithful capture, not diagnosis.
2. The local canonical record for `mfuton` execution lives under
   `world/issues/...` even when the issue may also be rendered or projected
   elsewhere.
3. Keep these layers separate:
   - reported behavior
   - directly supplied evidence
   - discovery/trigger context
   - unresolved unknowns
   - later triage interpretation
4. Do not infer root cause, repair scope, or repair ownership unless triage has
   actually started.
5. Do not collapse the trigger into only "test failed" or "operator noticed a
   problem" when the real discovery path is knowable.
   - capture what work was being done
   - why that work was happening
   - what exact step surfaced the issue
6. If the source issue is still primarily a futon-owned upstream issue rather
   than an `mfuton` local execution case, use:
   - `algorithms/create-futon-issue-before-triage.md`

## Procedure

1. Establish the reporting boundary.
   - Identify where the issue is arriving from:
     - live conversation
     - external issue post
     - pasted log
     - another artifact or transcript
   - Record the source surface and the source artifact if one exists.
2. Establish the discovery/trigger boundary.
   - Ask what concrete work caused the issue to be noticed now.
   - Record both:
     - the immediate trigger
       - what exact step surfaced the issue
     - the surrounding work context
       - what larger mission, lane, or procedure that step belonged to
   - Prefer the most causally informative trigger available, for example:
     - active mission or repair wave
     - verification pass
     - test harness run
     - refactor or cleanup pass
     - live operator workflow
     - review of a newly introduced invariant or policy check
   - If the issue was discovered while working another mission, preserve that
     mission or adjacent work owner explicitly instead of leaving it implicit.
3. Ask clarifying questions only to make the write-up legible.
   - Good intake questions reduce ambiguity about:
     - what went wrong
     - where it was observed
     - whether the problem is current or historical
     - what evidence already exists
     - what work or verification step exposed it
   - Do not use intake questions to smuggle in full triage.
4. Separate the first write-up into five parts.
   - `reported_behavior`
     - what the reporter says is wrong
   - `supplied_evidence`
     - logs, output, screenshots, URLs, snippets, transcripts
   - `trigger_context`
     - what concrete work exposed the issue
     - which active mission, lane, or procedure that work belonged to
   - `unknowns_before_triage`
     - what is still unclear
   - `projection_note`
     - whether the same issue is or may be rendered in GitHub or another
       external surface
5. Create or open the local `mfuton` world-backed issue/case.
   - If no local case exists, create:
     - `issue.md`
     - `events.ndjson`
     - `links.edn`
     - `notes.md`
   - Assign a local `issue_id`.
6. Write the issue summary without causal overreach.
   - Use a symptom-oriented title, not a guessed root-cause title.
   - Record:
     - `source_surface`
     - `source_entity`
     - `source_artifact`
     - `trigger_summary`
     - `trigger_context`
       - active mission, lane, or work context
       - immediate step that surfaced the issue
   - Set:
     - `current_status = reported-open`
     - `verification_state = reported-unverified`
     - `next_continuation = perform triage`
7. Append the intake/write-up event.
   - Add an `events.ndjson` entry describing:
     - the report trigger
     - the discovery/trigger context
     - clarifying questions asked
     - supplied evidence captured
     - result:
       - issue written up and ready for triage
     - `continuation_state = pending triage`
8. Link any external projection or source artifact.
   - If there is a GitHub issue, link it as a projection or source artifact.
   - If there is only a conversation, transcript, or pasted log, link that
     source instead.
   - If the issue was surfaced while another mission was active, link that
     mission or adjacent work owner as trigger context rather than leaving it
     only in freeform prose.
   - Keep the projection rule explicit:
     - local `world/issues/...` remains canonical for the `mfuton` execution
       case
     - external issue postings are renderings, mirrors, or linked sources
9. Stop at the triage boundary.
   - Do not reproduce, classify, or diagnose unless the flow has explicitly
     moved into triage.
   - Hand the case forward with:
     - a faithful write-up
     - an explicit discovery/trigger context
     - supplied evidence
     - open unknowns
     - an explicit next continuation:
       - `perform triage`

## Decision Rule: When The Write-Up Is Complete Enough

The issue is written up enough to stop and start triage later when all are
true:

- the undesired behavior is legible in plain language
- the source surface/entity/artifact are recorded as well as currently possible
- the discovery/trigger context explains why the issue surfaced now
- the immediate step that exposed the issue is explicit rather than implied
- the supplied evidence is attached or referenced
- unresolved unknowns are explicit instead of hidden
- the issue has a local canonical `world/issues/...` record

Do not force more intake just to simulate certainty.

## Projection Note

The same write-up may later be rendered elsewhere:

- as a GitHub issue body
- as a local note
- as a handoff snippet
- as another tracker entry

But the projection should not replace the local world-backed case.

## Outputs

- a local `mfuton` world-backed issue/case under `world/issues/...`
- a faithful pre-triage write-up
- explicit preservation of the discovery/trigger path that caused the issue to
  be noticed
- explicit separation between reported claims and triage conclusions
- links to any external issue or source artifacts
- a clean handoff point into triage

## References

- downstream triage/repair algorithm:
  `algorithms/process-world-backed-issue-through-triage-and-scheduled-repair.md`
- first exercised local issue/case:
  `world/issues/issue-20260308-futon3c-local-dev-stack-regression/`
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
- `library/system-architecture/deterministic-evidence-authority.flexiarg`

## Notes

- This algorithm is intentionally usable for live conversation with the user.
- It is also intentionally usable when the same intake happens through a GitHub
  issue or another external tracker.
- The boundary here is write-up before triage, not triage before repair.
- In practice, `trigger_summary` should be treated as important causal context,
  not filler. When known, it should answer both:
  - what immediately surfaced the issue
  - what active mission or work stream made that step happen
- This algorithm is for `mfuton` local execution cases.
- Upstream futon issues that need a local proxy before import should use:
  - `algorithms/create-futon-issue-before-triage.md`
