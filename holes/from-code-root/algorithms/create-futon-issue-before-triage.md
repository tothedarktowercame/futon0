# Algorithm: Create Futon Issue Before Triage

Date: 2026-03-11
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

Use this when an issue first arrives from a specific futon installation or repo
and you want a repeatable pre-triage flow for:

1. talking with the reporter just enough to make the issue legible locally
2. writing a local proxy under `world/futon-issues/<futon>/...`
3. preserving the upstream futon issue as canonical for upstream issue identity
   and problem statement
4. separating upstream issue authority from any later `mfuton` local execution
   case
5. stopping cleanly at the futon-origin triage boundary instead of silently
   drifting into local repair work
6. preserving the discovery/trigger context that explains why the issue
   surfaced in this specific work episode

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - governs deference to the upstream futon issue as canonical authority for
    issue identity and problem statement
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`
  - governs local proxy capture before later import, triage, or canonical local
    execution ownership

Inspected but non-owning candidates:
- `library/system-architecture/deterministic-evidence-authority.flexiarg`
  - relevant to later verification authority, but this algorithm stops at
    faithful intake/proxy creation
- `library/social/operator-verification-gate-after-agent-triage-or-fix.flexiarg`
  - closure-side policy, not futon-origin intake policy

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- raw operator report, GitHub issue, conversation, or other futon issue ingress
- source futon installation/repo when available
- source surface, source entity, and source artifact when available
- current work context when available:
  - active mission
  - active lane
  - current verification, refactor, or debug pass
  - current script, test, probe, or review step
- any directly supplied logs, screenshots, URLs, repro snippets, or transcripts

## Preconditions

1. The first task here is faithful capture, not diagnosis.
2. The local futon proxy record lives under `world/futon-issues/<futon>/...`
   even when the same issue is still canonical in an external tracker.
3. Keep these layers separate:
   - upstream issue identity and wording
   - directly supplied evidence
   - discovery/trigger context
   - unresolved unknowns
   - later local import or triage interpretation
4. Do not infer root cause, repair scope, or local `mfuton` repair ownership
   unless a later import/triage step has actually started.
5. Do not silently convert the futon-origin issue into an `mfuton`
   world-backed issue/case.
6. When local `mfuton` execution actually begins, hand off through:
   - `algorithms/create-mfuton-issue-before-triage.md`
7. When futon-origin triage actually begins, hand off through:
   - `algorithms/triage-futon-issue-and-decide-local-work-handoff.md`

## Procedure

1. Establish the futon source boundary.
   - Identify which futon installation/repo the issue belongs to.
   - Record the source surface and the source artifact if one exists.
   - Preserve the upstream issue id or URL when available.
2. Establish the discovery/trigger boundary.
   - Ask what concrete work caused the issue to be noticed or imported now.
   - Record both:
     - the immediate trigger
     - the surrounding work context
   - If the issue was discovered while another mission was active, preserve
     that mission or adjacent work owner explicitly instead of leaving it
     implicit.
3. Ask clarifying questions only to make the proxy write-up legible.
   - Good intake questions reduce ambiguity about:
     - what went wrong
     - where it was observed
     - whether the problem is current or historical
     - what evidence already exists
     - what work or verification step exposed it
   - Do not use intake questions to smuggle in full local triage.
4. Separate the first write-up into six parts.
   - `reported_behavior`
     - what the upstream issue/report says is wrong
   - `supplied_evidence`
     - logs, output, screenshots, URLs, snippets, transcripts
   - `trigger_context`
     - what concrete work exposed the issue or caused the import now
   - `unknowns_before_triage`
     - what is still unclear
   - `upstream_authority_note`
     - which futon issue surface remains canonical
   - `projection_note`
     - whether a later `mfuton` local execution case may be created
5. Create or open the local futon-backed issue/case proxy.
   - If no local proxy exists, create:
     - `issue.md`
     - `events.ndjson`
     - `links.edn`
     - `notes.md`
   - Place it under:
     - `world/futon-issues/<futon>/<issue-id>/`
   - Assign a local `issue_id`.
   - Preserve the upstream futon issue reference in the record.
6. Write the local futon-issue summary without execution overreach.
   - Use a symptom-oriented title, not a guessed root-cause title.
   - Record:
     - `source_surface`
     - `source_entity`
     - `source_artifact`
     - `trigger_summary`
     - `trigger_context`
   - Set:
     - `current_status = reported-open`
     - `verification_state = reported-unverified`
     - `next_continuation = decide futon-origin triage outcome`
7. Append the intake/write-up event.
   - Add an `events.ndjson` entry describing:
     - the report trigger
     - the discovery/trigger context
     - clarifying questions asked
     - supplied evidence captured
     - result:
       - futon-origin issue written up locally and ready for futon-origin
         triage
     - `continuation_state = pending futon-origin triage`
8. Link the upstream futon issue and any local trigger context.
   - Link the upstream GitHub issue or other futon tracker surface as the
     source authority.
   - If the issue surfaced while another local mission was active, link that
     mission or adjacent work owner as trigger context.
   - Keep the projection rule explicit:
     - upstream futon issue remains canonical for upstream issue identity and
       problem statement
     - local `world/futon-issues/<futon>/...` owns the local proxy, trigger
       context, and continuation state for this installation
9. Stop at the write-up boundary.
   - Do not reproduce, classify, or diagnose further inside this algorithm.
   - Hand forward through:
     - `algorithms/triage-futon-issue-and-decide-local-work-handoff.md`

## Decision Rule: When The Write-Up Is Complete Enough

The futon-origin issue is written up enough to stop and triage later when all
are true:

- the undesired behavior is legible in plain language
- the futon source surface/entity/artifact are recorded as well as currently
  possible
- the discovery/trigger context explains why the issue surfaced now
- the supplied evidence is attached or referenced
- unresolved unknowns are explicit instead of hidden
- the issue has a local proxy under `world/futon-issues/<futon>/...`

Do not force more intake just to simulate certainty.

## Outputs

- a local futon-backed issue proxy under `world/futon-issues/<futon>/...`
- a faithful pre-triage write-up
- explicit preservation of the upstream futon issue authority
- explicit separation between source issue authority and later `mfuton` local
  execution
- links to any upstream issue or source artifacts
- a clean handoff point into futon-origin triage when needed

## References

- downstream futon-origin triage/handoff algorithm:
  `algorithms/triage-futon-issue-and-decide-local-work-handoff.md`
- sibling local-execution algorithm:
  `algorithms/create-mfuton-issue-before-triage.md`
- downstream local triage/repair algorithm after import:
  `algorithms/process-world-backed-issue-through-triage-and-scheduled-repair.md`
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
- `library/system-architecture/open-world-ingest-mission-reconstruction-and-canonicalization-factorization.flexiarg`

## Notes

- This algorithm is intentionally usable when Joe or another futon operator
  gives `mfuton` a GitHub issue or equivalent upstream work item.
- The boundary here is write-up before futon-origin triage, not triage itself.
- In practice, `trigger_summary` should be treated as important causal context,
  not filler.
