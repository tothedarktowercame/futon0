# Algorithm: Create New Agent Skill with Typed-Surface Grouping, Python Wrapper, and Tests

Date: 2026-03-12
Status: active

## Purpose

Use this when adding a new reusable agent skill so skill creation stays
orthogonal, test-backed, and consistent with local `mfuton` conventions.

This algorithm enforces four core outcomes:
- group skills into a shared subdirectory when two or more belong to one typed
  surface
- implement the skill core in Python by default, with a `.bat` wrapper for
  invocation
- create and run tests while building the skill, not only after coding
- when the skill can delete a large amount of data or a large remote artifact,
  author an explicit operator-approval gate before the deletion step can be
  invoked

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
  - governs typed-surface authority and keeps the skill lane aligned with the
    wider command/control plane
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
  - governs deference to installation typed-surface authorities instead of
    inventing local parallel semantics
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first library/code-path selection before writing new skill
    code
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
  - governs repo-local `data/tmp/...` placement for skill-generated temp
    files, runtime scratch state, helper bodies, and bounded caches

Inspected but non-owning candidates:
- `library/social/typed-command-surface-over-eval.flexiarg`
  - operationally adjacent, but narrower than the architecture-level typed
    command authority that governs the skill lane
- `library/foundational/kill-two-birds-with-one-stone.flexiarg`
  - adjacent placement heuristic, but not the primary owner of skill-creation
    policy

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- desired skill behavior and operator intent
- candidate typed surface (for example: GitHub PR, session tooling, permissions)
- current `agent_skills` inventory
- installation typed-surface authority artifacts to cross-reference
- governing mission/flexiarg references for the new skill

## Preconditions

1. The operator asked for a new skill or approved creating one.
2. The target lane is reusable enough to justify a skill instead of one-off
   ad hoc commands.
3. Existing skills were inspected before creating new files.
4. The authoritative skill root is confirmed:
   - `gh/mfuton/agent_skills/`
5. A typed-surface installation authority sweep was performed in both:
   - `gh/mfuton`
   - `darktower/futon3c` (when upstream surface contracts are relevant)

## Procedure

1. Start in the authoritative skills directory.
   - Treat `gh/mfuton/agent_skills/` as the default implementation lane.
   - Inspect existing skills and subdirectories before choosing a new path.
2. Run installation typed-surface authority cross-reference sweep.
   - Confirm local installation authority anchors:
     - `docs/futon-installation-surface-catalog.md`
     - `docs/futon-installation-surface-catalog.json`
     - `docs/full-system-operation-lane-references.md`
     - `docs/typed-command-lane-references.md`
   - Confirm typed-surface mission/flexiarg owners the skill should defer to:
     - `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory.md`
     - `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-mfuton-typed-surface-binding-plane.md`
     - `holes/missions/M-futon-full-system-operation-and-onboarding/M-callable-tool-registry-typed-surface.md`
     - `holes/missions/M-typed-command-surface.md`
     - `library/system-architecture/typed-command-control-plane-authority.flexiarg`
     - `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
   - For upstream/runtime surface contract checks, scan `futon3c` artifacts when
     relevant to the skill lane:
     - `futon3c/scripts/windows/README.md`
     - `futon3c/scripts/windows/dev-stack-windows.bat`
3. Classify typed-surface ownership and choose layout.
   - Determine the typed surface the new skill belongs to.
   - If the new skill will make at least two skills that share that typed
     surface, place them under one subdirectory:
     - `agent_skills/<typed-surface-group>/...`
   - If a same-surface sibling currently lives at root and this new skill
     creates the second member, refactor both into one shared subdirectory in
     the same wave unless that migration is explicitly deferred.
4. Implement skill logic in Python by default.
   - Place Python sources alongside the skill in its skill directory by
     default.
   - If the skill instead owns a reusable library seam that should be imported
     directly by Python callers or expected future sibling skills, place that
     reusable core under `src/mfuton/<lane>/` and keep only the invocation
     wrapper and skill glue under `agent_skills/`.
   - Do not move code into `src/mfuton` merely for style; do it only when the
     shared-library seam is intentional and reusable enough to justify a real
     package owner.
   - If the skill may create temp files, runtime scratch state, helper bodies,
     or bounded caches, choose a repo-local owner path under
     `data/tmp/<...>/` instead of OS temp or repo-root `tmp/`.
   - If the skill needs a new Python package capability, do not improvise the
     install lane here; call:
     `algorithms/install-python-capability-package-in-codex-env-and-record-requirement.md`
     and then continue skill creation once the dependency is verified and
     recorded.
  - If the skill can delete a large amount of data or a large remote artifact,
    author an explicit operator-approval gate before the deletion step can be
    invoked.
  - Treat examples such as database deletion or GitLab project deletion as
    belonging to this gate by default.
5. Add a `.bat` wrapper for invocation.
   - Provide a stable batch entrypoint that invokes the Python skill.
   - Prefer governed local environment wrappers already used in this workspace
     (for example, `agent_skills/development/codex-python.bat ...`) unless the skill lane has a
     stronger existing convention.
   - If the wrapper can influence temp placement, make the repo-local
     `data/tmp/...` root explicit rather than relying on ambient OS temp.
   - Return non-zero exit codes on failure and preserve argument pass-through.
6. Wire governance/index artifacts.
   - Add or update the skill row in `agent_skills/README.md` using:
     - `agent_skills/SKILL-ENTRY-TEMPLATE.md`
   - Include:
     - stable skill id
     - script path
     - status
     - governing mission
     - governing flexiargs
      - scope and non-goals
   - When temp-producing behavior is part of the skill, include the intended
     repo-local `data/tmp/...` lane in the nearest README/operator surface.
7. Author tests before or alongside implementation and run them during build.
   - Create targeted tests for the skill lane (typically under
     `tests/mfuton/runtime/` when runtime/CLI behavior is under test; use the
     real repo layout rather than drifting to singular `test/` examples).
   - Cover:
     - expected success path
     - expected failure path
     - wrapper/argument behavior boundaries
   - Run tests repeatedly as the implementation evolves, not only once at the
     end.
   - Use the governed local pytest surface for test runs so repo-local cache
     and basetemp policy are inherited:
     - `scripts/codex-pytest.bat ...`
   - If a non-wrapper invocation is genuinely required for a specialized lane,
     keep the same policy explicitly:
     - `-c pytest.ini`
     - `--basetemp data/tmp/pytest/<surface>`
8. Verify completion criteria.
   - Confirm skill files are in the correct grouped path.
   - Confirm Python core + `.bat` wrapper both exist and execute.
   - Confirm tests exist and pass for the intended scope.
9. Close with explicit output summary.
   - Report:
     - grouped location chosen and why
     - installation typed-surface artifacts reused/cross-referenced
     - Python files created
     - wrapper path
     - tests added and test command results

## Decision Rule

Default creation rule:
1. locate in `agent_skills`
2. group by shared typed surface once there are at least two sibling skills
3. implement core in Python, invoke via `.bat`
4. require test creation plus test-guided verification during implementation

## Outputs

- new or updated skill files under `agent_skills/` (grouped when needed)
- Python implementation plus `.bat` wrapper entrypoint
- tests that validate the skill behavior
- updated `agent_skills/README.md` governance/index entry
- explicit installation typed-surface cross-reference list for the skill lane
- explicit operator-approval deletion gate when the skill includes large-scale
  destructive deletion behavior

## Stop Conditions

Stop and ask the operator if:
- typed-surface ownership is ambiguous and grouping could be wrong
- adding/moving a sibling skill into a grouped subdirectory would break a known
  external contract
- required Python/test tooling is unavailable in the current environment
- the requested behavior is still too fluid to justify skill codification
- the skill includes large-scale deletion behavior but no explicit
  operator-approval gate has been authored yet
- installation typed-surface ownership remains ambiguous after authority sweep

## References

- `agent_skills/README.md`
- `agent_skills/SKILL-ENTRY-TEMPLATE.md`
- `docs/futon-installation-surface-catalog.md`
- `docs/futon-installation-surface-catalog.json`
- `docs/full-system-operation-lane-references.md`
- `docs/typed-command-lane-references.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory.md`
- `holes/missions/M-mfuton-pragmatic-operational-problem-solving-codification/M-my-familiar-software-development-operating-posture.md`
- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-mfuton-typed-surface-binding-plane.md`
- `holes/missions/M-futon-full-system-operation-and-onboarding/M-callable-tool-registry-typed-surface.md`
- `holes/missions/M-typed-command-surface.md`
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
- `library/social/futon-self-description-defers-to-installation-authority.flexiarg`
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
- `futon3c/scripts/windows/README.md`
- `futon3c/scripts/windows/dev-stack-windows.bat`
- `tests/mfuton/runtime/git_github_helper_test.py`
- `tests/mfuton/runtime/irc_lane_skill_test.py`
- `algorithms/create-algorithm.md`
- `algorithms/install-python-capability-package-in-codex-env-and-record-requirement.md`
