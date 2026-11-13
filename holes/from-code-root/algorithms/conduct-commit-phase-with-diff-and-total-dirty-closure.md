# Algorithm: Conduct Commit Phase with Diff and Total Dirty Closure

Date: 2026-03-09
Status: active

## Commit Target

This algorithm is attempting to check in all files that the current evolver has
worked on, grouped into conceptual units.

Default target:
- commit the current evolver's owned work only
- group that owned work into one or more conceptual units
- do not pull in stray, unrelated, or non-owned dirty files merely to make the
  repo clean

Exceptional target:
- if the operator explicitly asks to commit everything, the algorithm may
  switch into a deliberate full-sweep mode
- that full-sweep mode exists for stray-file cleanup and similar cases
- the evolver must not ask the operator to switch into full-sweep mode merely to
  avoid classification work or to make the tree clean

Operator shorthand:
- preferred repo-scoped invocation:
  - `run the commit algorithm for <repo(s)>`
- `run the commit algorithm` is still a direct invocation of this algorithm and must be treated as such
- do not treat `run the commit algorithm` as optional, approximate, or ignorable just because the repo target packet was omitted
- bare `commit` or other non-algorithm wording does not license ad hoc direct commit mutation outside this algorithm
- the repo list may name one or more explicit repo targets such as (non-exclusively):
  - `development`
  - `gh`
  - `xi`
  - `redistrict`
  - `futon1a`
  - `futon3`
  - `futon3c`
  - `futon4`
  - `futon5`
  - `futon6`
  - `futon7`
- by default, it commits the current evolver's owned work for the named repo
  targets, not everything dirty
- keep going until that owned work is committed
- stop early only if the operator approves leaving some of that owned work for later

## Purpose

Use this when the operator has decided it is time to checkpoint the current
work and the system must turn a dirty worktree into one or more explicit
commits without accidentally carrying unrelated side effects forward.

This algorithm is not exclusive of other policy. It complements:
- general checkpoint discipline
- mission-specific publication or closure algorithms
- repo-specific push/PR algorithms

Its narrower job is the committing phase itself:
- inspect what is actually dirty
- determine whether this is the default owned-work path or an explicit
  operator-declared full-sweep path
- identify which dirty files are owned by the current evolver/lane and which are
  not
- exclude obvious non-core side effects
- commit only the owned conceptual units by default
- commit everything only when the operator has already explicitly requested
  that mode
- decide whether the current owned work for this commit phase is one logical
  group or several
- run as a bounded loop until all dirty work covered by the selected mode is
  committed
- never end early with dirty owned work covered by the selected mode still present
  unless the operator has been consulted and explicitly approved the remainder
- never perform commit mutation outside this algorithm; commit requests must
  route through this algorithm rather than around it

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs promotion of stable stage/check-in helper flows instead of ad hoc
    commit mutation
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the existing permission-helper lane and existing commit
    owner instead of improvising direct git paths

Inspected but non-owning candidates:
- `library/social/json-artifacts-default-ignore-with-operator-exceptions.flexiarg`
  - governs a narrower class of stage/commit decisions for JSON artifacts, not
    the whole commit-phase procedure

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- repo path being checkpointed
- repo-address resolution config that governs named commit targets
  (currently `world/config/repo-address-resolution.json`)
- operator invocation surface for this algorithm, which may arrive as either:
  - `run the commit algorithm for <repo(s)>`
  - `run the commit algorithm`
- repo class and current branch (`futon` vs `gh` vs other local repo)
- current evolver name for repo-scoped naming rules, if already
  known
- active mission or cleanup lane that owns the current work
- current dirty-file set
- current evolver-owned dirty-file set, if already known. This is non-exclusive of owned dirty files discovered while running the algorithm
- commit mode:
  - default `owned conceptual units only`
  - explicit operator-declared `full sweep`
- operator policy about whether multiple commits are acceptable for the current
  checkpoint

## Preconditions

1. The operator has requested or approved entering a committing phase, either
   by directly invoking this named algorithm or by otherwise clearly
   authorizing commit mutation.
2. The dirty worktree belongs to the current repo scope being discussed.
3. There is no unresolved merge/rebase already in progress.
4. For futon repos only, commit mutation must not proceed on the canonical
   integration branch (`main` or `master`).
5. Every repo named for commit mutation must resolve through the current
   repo-address configuration.
6. If a repo named for commit mutation is missing from that configuration,
   this is a failure state of the installation config, not a reason to bypass
   resolution with an ad hoc raw local path.

## Loop Invariant and Termination

This algorithm is a bounded closure loop, not a one-shot "make a commit"
procedure.

Loop invariant:
- the evolver maintains an explicit view of the current dirty work they are
  responsible for closing in this commit phase
- in the default mode, that responsibility contains only the current owned-work
  paths for this phase
- in the explicit full-sweep mode, that responsibility is the remaining
  non-noise dirty set
- known non-owned / unrelated dirty files are outside that responsibility in
  the default mode unless the operator explicitly switched this run into full
  sweep
- each loop iteration either:
  - reduces that remaining responsible dirty work by committing at least one
    logical group, or
  - stops to consult the operator because ambiguity or policy prevents safe
    continuation

Termination rule:
- the commit phase terminates only when one of these is true:
  - all dirty files in the current responsibility for the selected mode are now
    committed
  - the operator has explicitly approved leaving a named remainder uncommitted
    for now
- known non-owned / unrelated dirty files that were outside the current
  responsibility do not count as deferred owned work
- file on the order of megabytes that are not already tracked are not currently ignored and could be check in.
  If the file is the evolvers responsibility than stop and ask the operator if the file should be added or added
  to a .gitignore file.

Corollary:
- do not intentionally stop after "a useful checkpoint" if responsible dirty
  files still remain
- do not treat "one commit happened" as success if responsible dirty files
  still remain
- in the default mode, do not reinterpret "the repo is globally clean" as the
  success condition if that cleanliness required sweeping in non-owned work
- this loop is bounded because the dirty-file set is finite and each successful
  iteration removes at least one logical group from the remaining responsible
  dirty work

## Procedure

1. Bind the target repo set before any mutation.
   - If the operator says `run the commit algorithm for <repo(s)>`, bind the
     named repo targets directly.
   - If the operator says `run the commit algorithm`, treat that as a direct
     invocation of this algorithm rather than as optional shorthand.
   - When the repo list is omitted, bind the target repo set from the current
     live repo scope if that scope is singular and unambiguous.
   - If the target repo set remains ambiguous after that binding attempt, stop
     and ask the operator before mutation.
   - Do not bypass this algorithm merely because the operator omitted the repo
     packet.
   - Resolve each named repo target through the current repo-address
     configuration before any mutation helper is invoked.
   - If a repo named for this commit phase is missing from that configuration,
     treat that as a failure state.
   - Add the repo to the appropriate repo-address configuration file, verify
     that the resolver now accepts it, and only then continue this algorithm.
   - Do not work around that failure by substituting a raw local path merely to
     get the commit through.
2. Determine commit mode before any mutation.
   - Default mode:
     - `owned conceptual units only`
   - Exceptional mode:
     - explicit operator-declared `full sweep`
   - Enter `full sweep` only when the operator has already explicitly asked to
     commit everything or all remaining files.
   - Do not ask the operator to switch into `full sweep` merely because:
     - classification is inconvenient
     - the repo would otherwise remain dirty
     - stray files are present
   - If no explicit operator-declared full-sweep instruction exists, stay in
     the default mode.
3. Enforce the futon-only branch gate before any commit mutation.
   - Classify the target repo first.
   - If the target is not a futon repo (for example `gh` or `i:/development`),
     skip this gate and continue.
   - If the target is a futon repo and the current branch is `main` or
     `master`, create and switch to a new local non-integration working branch
     before any staging/check-in mutation.
   - Use a branch name that reflects the active mission or checkpoint intent so
     the local history stays attributable.
   - After switching to that local branch, continue the commit phase.
   - If branch creation or branch switching fails, stop and ask the operator
     before continuing.
4. Start the bounded closure loop.
5. Inspect the actual dirty set for this iteration.
   - Use `git status`, `git diff --name-only`, `git diff --stat`, or
     equivalent.
   - Do not guess from memory or from the last few edited files alone.
6. Classify each dirty path in the current iteration.
   - owned by the current evolver
   - likely side effect / temp artifact / cache / generated noise
   - known non-owned / unrelated
   - ambiguous ownership
   - tracked path now covered by ignore policy
   - A path may carry both an ownership classification and a `tracked +
     ignored` mutation edge case.
7. Exclude obvious side effects from the current iteration.
   - remove, ignore, or leave out temp/cache/noise files that are not part of
     the current checkpoint work
   - if a file looks accidental but might carry durable evidence, do not drop
     it blindly; clarify first
8. Determine the current dirty work the evolver is responsible for in this
   iteration from the cleaned classification.
   - In the default mode:
     - include only paths classified as `owned by the current evolver`
     - exclude `known non-owned / unrelated` paths from the current
       responsibility
     - those excluded non-owned paths may still remain dirty in the worktree,
       but they are not current work for this run
     - do not pull a path into the current responsibility merely because:
       - it looks conceptually related
       - it would make the tree clean
       - it is convenient to commit it now
     - if unsured of responsibility - read the diff of the file for additional information
   - In the explicit full-sweep mode:
     - include all remaining non-noise paths
   - If any path remains `ambiguous ownership` in the default mode, stop and
     ask the operator before continuing.
9. Before grouping, classify any `tracked + ignored` paths explicitly.
   - If the path should remain tracked going forward, this is a staging edge
     case inside the normal commit phase.
   - If the path should become local-only going forward, stop the normal commit
     phase and call the sibling algorithm:
     - `algorithms/untrack-local-only-artifacts-under-new-ignore-policy.md`
- `world/config/repo-address-resolution.json`
   - Do not silently choose between those mutation classes.
10. Re-read the remaining dirty set after exclusions, ownership selection, and
   any `tracked + ignored` handoff decision.
   - The grouping decision must be made on the cleaned dirty set, not the
     initial noisy one.
11. Check for loop completion.
   - If no dirty files remain in the current responsibility for this run,
     terminate the commit phase successfully.
   - In the default mode, it is acceptable for the repo still to contain:
     - excluded side-effect noise
     - known non-owned / unrelated dirty files that are not part of the
       evolvers responsibility
   - Do not continue merely because the worktree still contains excluded noise
     or known non-owned / unrelated dirty files outside the current
     responsibility.
12. Decide grouping for the next commit iteration.
   - Group only within the current dirty work for the selected mode.
   - In the default mode, each selected group must be both:
     - a conceptual unit
     - a subset of the current owned dirty work for this phase
   - If the remaining responsible files are one major logical group, use one
     commit.
   - If the remaining responsible files clearly belong to more than one major
     logical group, select one group for the current iteration and leave the
     rest in the loop remainder.
   - "Clearly" means the groups are substantially different in mission,
     purpose, or user-facing effect, not just different file extensions.
   - Topic similarity alone is not enough to pull a non-owned path into a
     group.
   - Do not commit an arbitrary partial subset just because it is convenient.
13. Construct the commit message under repo-scoped naming rules before any
    helper mutation.
    - Start from the conceptual checkpoint intent for the selected group.
    - If the target repo is `gh`, the commit subject line must begin with:
      - `<evolver-name>: `
    - For `gh`, the current evolver name is therefore a hard prerequisite to
      commit-message construction rather than an optional stylistic flourish.
    - If the current evolver name is forgotten, unknown, or the evolver is unsure
      whether the remembered name is still current for this session, stop and
      ask the operator what the evolver's name is before continuing.
    - Do not guess the `gh` commit prefix from memory when uncertainty remains.
    - Do not write a `gh` commit message to the helper temp file until that
      prefix gate is satisfied.
    - For repos other than `gh`, do not apply this prefix rule unless another
      repo-specific policy explicitly says to do so.
14. Commit the selected logical group explicitly.
    - Stage only the files for that group.
    - If the repo is behind a protected mutation boundary and the exact helper
      command is not already approved for this session, obtain approval on the
      exact helper invocation before the first live stage/check-in attempt.
    - The governed helper flow is intentionally structured so the operator can
      approve one stable helper command per session and then reuse it.
    - That stability requirement is part of the design, not a convenience
      detail.
    - Therefore do not move the temp-file contents onto the live command line.
      Do not put pathspecs, commit messages, or other checkpoint-specific
      content into the helper invocation itself.
    - Keep the live helper command shape stable and let the varying checkpoint
      content live only in the stable evolver-local temp files.
    - Prepare those temp files by editing their file contents directly as
      ordinary workspace files before the helper run.
    - The temp paths should live under:
      - `mfuton/home/<evolver-name>/tmp/...`
    - If a governed text-mutation surface is used for that editing, target the
      evolver-local helper temp file path directly, for example:
      - `mfuton/home/alice/tmp/permissions/git_stage_permitted.tmp`
      - `mfuton/home/alice/tmp/permissions/git_checkin_permitted.tmp`
    - Do not introduce auxiliary carrier files under a second staging lane or
      any other intermediate file that later gets copied or transferred into
      the active evolver-local helper temp files.
    - Do not prepare those temp files with a separate elevated shell command
      such as `Set-Content`, `echo >`, inline Python, or other ad hoc command
      generation, because that creates a second mutating approval surface and
      defeats the stable-helper design.
    - The governed mutation surface is the helper invocation itself. The
      evolver-local helper temp files are the correct varying-content carrier
      for this algorithm, and temp-file preparation should remain direct to
      those exact paths unless a separate dedicated helper is later introduced
      for that exact purpose.
    - Do not probe with direct `git add`, direct `git commit`, or a speculative
      unapproved helper run "just to see if it works."
    - When a governed local mutation surface is preferred, write the exact
      group pathspecs to:
      - `mfuton/home/<evolver-name>/tmp/permissions/git_stage_permitted.tmp`
      and run:
      - `agent_skills/permissions/git_stage_permitted.bat <repo> <that-temp-path>`
    - When the grouping is already settled and the same checkpoint should flow
      directly into commit, it is acceptable to use:
      - `agent_skills/permissions/git_stage_and_checkin_permitted.bat`
    - Use a commit message that names the checkpoint intent for that group.
    - For `gh`, the message written to
      `mfuton/home/<evolver-name>/tmp/permissions/git_checkin_permitted.tmp`
      must already
      satisfy the `<evolver-name>: ` prefix gate above.
    - When the stage step was kept separate, write the message to:
      - `mfuton/home/<evolver-name>/tmp/permissions/git_checkin_permitted.tmp`
      and run:
      - `agent_skills/permissions/git_checkin_permitted.bat <repo> <that-temp-path>`
    - Important helper consequence:
      - these helpers delete their temp files after an attempt
      - if a helper attempt fails, do not assume the temp files still exist
      - recreate or rewrite the temp files deliberately before retrying by
        editing the files again, not by improvising a new shell mutation path
    - Windows helper invocation note:
      - when invoking a `.bat` helper through `cmd.exe`, use a path shape that
        `cmd.exe` will actually resolve on Windows
      - do not assume a forward-slash path that was acceptable in PowerShell
        will also be accepted by `cmd.exe`
      - if the failure happened before the helper itself began running, the temp
        files are usually still intact and the corrected invocation can be
        retried without regenerating them
15. Re-enter the loop.
    - After a successful commit, go back to step 3.
    - Do not stop merely because one group has been committed.
16. Only allow an early stop if the operator has been consulted about a named
    remainder.
    - If ambiguity remains, perform a call with continuation to the operator
      and preserve the pending remainder explicitly.
    - If the operator approves leaving a remainder for later, record that
      remainder explicitly when reporting the phase result.
    - Without that operator decision, continue looping.

## Decision Rule

Default preference:
1. exclude obvious side-effect noise
2. classify dirty paths by ownership before grouping
3. in the default mode, commit all owned conceptual units and nothing more
4. switch to full-sweep mode only when the operator explicitly asks for it
5. split into multiple commits only when the logical grouping is strong enough
   to justify it
6. keep looping until the dirty work covered by the selected mode reaches zero

High-level rule:
- avoid checking in accidental side effects
- avoid checking in non-owned files merely to achieve a clean repo
- avoid ending the committing phase with responsible dirty files still
  uncommitted
- any intentional remainder requires explicit operator consultation first
- if the operator explicitly requests `commit everything`, honor that full
  sweep as a separate mode rather than pretending the default owned-work rule
  changed on its own

## Outputs

- one or more commits covering the dirty work the evolver was responsible for
  under the selected mode
- explicit statement of committed owned conceptual units in the default mode
- explicit statement of excluded side-effect/noise files left out of the core
  checkpoint
- explicit statement of excluded known non-owned / unrelated dirty files that
  remain in the worktree and were outside the current responsibility in the
  default mode
- explicit statement when the operator-declared `full sweep` mode was used
- explicit statement if any remainder was deliberately deferred after operator
  approval
- explicit statement when a repo-scoped commit-message naming gate affected
  construction of the subject line
- otherwise, a closure judgment that the dirty work covered by the selected
  mode is fully committed

## Failure-State Repair Rule

If a repo named for this commit phase does not resolve because it is missing
from the current repo-address configuration:
- treat that as a failure state of the installation config
- add the repo to the appropriate repo-address configuration file before
  continuing
- verify the resolver accepts the repo target after that repair
- only then continue commit mutation for that repo
- do not treat a direct raw local path as an acceptable substitute for the
  missing config entry when the operator named the repo semantically

## Stop Conditions

Stop and ask the operator if:
- a file is very large (e.g. a file on the order of megabytes that is not already tracked)
- the dirty set contains ambiguous files that might be either noise or real
  evidence
- the target repo set is still ambiguous after direct invocation of this
  algorithm and current-context repo binding
- ownership of a dirty path is ambiguous in the default mode
- a path is both tracked and ignored and the operator intent between staging
  versus local-only untracking is not already clear
- the logical grouping is not clear enough to justify splitting
- the target repo is `gh` and the current evolver name is forgotten, unknown, or
  uncertain while constructing the commit message
- unrelated user changes appear and their checkpoint ownership is not known
- the evolver is about to widen the current responsibility from `owned work` to
  `everything still dirty` without an explicit operator-declared full sweep
- the evolver is about to leave responsible dirty files uncommitted for any
  reason other than an explicit operator-approved remainder

## References

- `holes/missions/M-checkpoint-commit-discipline.md`
- `holes/missions/M-futon-full-system-operation-and-onboarding/M-futon3-onboarding-and-familiarization/M-futon3-devmap-onboarding-and-filesystem-backed-wiring-alignment/M-devmap-owned-mission-rewire-and-cleanup.md`
- `library/social/reverse-morphogenesis-for-retroactive-mission-reconstruction.flexiarg`
- `agent_skills/permissions/git_stage_permitted.bat`
- `agent_skills/permissions/git_untrack_cached_permitted.bat`
- `agent_skills/permissions/git_checkin_permitted.bat`
- `agent_skills/permissions/git_stage_and_checkin_permitted.bat`
- `algorithms/untrack-local-only-artifacts-under-new-ignore-policy.md`
- `world/config/repo-address-resolution.json`
