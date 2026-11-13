# Algorithm: Untrack Local-Only Artifacts Under New Ignore Policy

Date: 2026-03-11
Status: active

## Purpose

Use this when artifacts that were previously tracked should now remain local
only under a newer ignore policy.

The job of this algorithm is not to rewrite history or delete local files. Its
job is narrower:
- stop carrying the artifacts in the current tracked tree
- preserve the local copies on disk
- checkpoint the policy migration honestly

This is a sibling of the normal commit-phase algorithm, not a subtype of plain
staging. The mutation class is different:
- normal commit phase stages tracked content changes for continued tracking
- this algorithm removes paths from the tracked set while leaving local files
  in place

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/json-artifacts-default-ignore-with-operator-exceptions.flexiarg`
  - governs the recurring local-only/commit-default judgment that often forces
    this untracking mutation class
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the governed cached-untrack helper path instead of ad hoc
    direct git mutation

Inspected but non-owning candidates:
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - explains why the governed helper lane should exist, but not the direct
    local-only untracking policy surface

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- repo path being cleaned up
- pathspec set that is now intended to be local-only
- current ignore policy that now covers those paths
- operator confirmation that the intent is local-only going forward, not
  history rewriting

## Preconditions

1. The target repo is `gh` (repo root `i:/gh`).
2. The operator has confirmed that the affected paths should remain local but
   should no longer be tracked.
3. The affected paths are currently tracked in the repo.
4. The current ignore policy already covers those paths or should be updated as
   part of the same checkpoint.
5. There is no unresolved merge/rebase already in progress.

## Procedure

1. Confirm the target repo scope before mutating anything.
   - If the target repo is not `gh`, halt and explain that this untracking
     algorithm is `gh`-only by design.
   - For non-`gh` repos, stop and route through repo-appropriate policy rather
     than forcing this helper flow.
2. Inspect the candidate pathspecs before mutating anything.
   - Confirm they are currently tracked.
   - Confirm they are covered by the intended ignore policy.
   - Confirm the local files should be preserved.
3. Stop if the operator actually wants the files removed from all prior
   history.
   - That is a history-rewrite operation, not this algorithm.
4. Prepare the governed untracking temp file by editing:
   - `mfuton/home/<evolver-name>/tmp/permissions/git_untrack_cached_permitted.tmp`
   - one pathspec per non-empty line
5. If the repo is behind a protected mutation boundary and the exact helper
   command is not already approved for this session, obtain approval on the
   exact helper invocation before the first live attempt.
6. Run the governed cached-untrack helper from the repo root.
   - This algorithm is `gh`-only; invoke from `i:/gh`.
   - Use:
     - `agent_skills/permissions/git_untrack_cached_permitted.bat`
   - Or when the checkpoint should flow directly into commit:
     - `agent_skills/permissions/git_untrack_cached_and_checkin_permitted.bat`
7. If committing separately, prepare:
   - `mfuton/home/<evolver-name>/tmp/permissions/git_checkin_permitted.tmp`
   - Use a message that names the policy migration or local-only cleanup
     intent.
8. Verify the result.
   - Re-run `git status`.
   - Confirm the local files still exist on disk.
   - Confirm the tracked tree now reflects the untracking.
9. If the ignore policy itself still needs updating, checkpoint that in the
   same logical wave or explain the deliberate remainder explicitly.

## Decision Rule

Use this algorithm when all of the following are true:
1. the paths are tracked now
2. the paths should remain local
3. the paths should not remain tracked going forward

Do not force this case through normal `git add` staging just because the
checkpoint was originally discussed as a commit phase.

## Outputs

- the selected paths removed from the tracked tree
- local files preserved on disk
- one or more commits that honestly describe the policy migration

## Stop Conditions

Stop and ask the operator if:
- the paths are ignored but not actually tracked
- the operator intent is unclear between untracking and ordinary staging
- the operator really wants history rewriting rather than local-only
  untracking

## References

- `algorithms/conduct-commit-phase-with-diff-and-total-dirty-closure.md`
- `agent_skills/permissions/git_untrack_cached_permitted.bat`
- `agent_skills/permissions/git_untrack_cached_and_checkin_permitted.bat`
- `agent_skills/permissions/git_checkin_permitted.bat`
