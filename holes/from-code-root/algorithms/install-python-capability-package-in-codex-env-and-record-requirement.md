# Algorithm: Install Python Capability Package in Codex Env and Record Requirement

Date: 2026-03-12
Status: active

## Purpose

Use this when live work in this workspace needs a new Python package capability
for agent/runtime/tooling work and the package is not already available in the
governed `codex` environment.

This algorithm owns the replayable procedural spine for adding that capability
without drifting into global installs or unrecorded dependency state.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse-first checking of existing code or already-available package
    capabilities before installation
- `library/social/agent-python-capability-packages.flexiarg`
  - governs package choice and environment posture for agent/runtime capability
    adds in this workspace

Inspected but non-owning candidates:
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
  - relevant for longer-term typed-surface promotion of capabilities, but not
    the direct package-installation procedure owned here

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- the required capability and concrete task that needs it
- the candidate package name(s) and import surface
- the target repo and authoritative requirements manifest
- one smoke-test command that proves the capability is usable

## Preconditions

1. A bounded reuse search has already happened.
   - stable existing library/code-path reuse was checked first
   - if a new package is still needed, the reason is explicit
2. The package is genuinely needed for current workspace work.
3. The governed Python environment for this workspace is `codex`.
4. The authoritative requirements manifest for the target repo is known.
   - for `mfuton`, default to `requirements.txt`
5. A smoke-test/import check is known before installation starts.

## Procedure

1. Confirm the package need and the reuse decision.
   - Name the concrete missing capability.
   - Name the candidate package and the expected import/module name.
   - Record why existing local code or already-adopted packages do not already
     satisfy the need.
2. Identify the authoritative dependency manifest.
   - For `mfuton`, use `requirements.txt`.
   - If the target repo is not `mfuton` and the manifest owner is unclear, stop
     and explain rather than guessing.
3. Check whether the package is already available in `codex`.
   - Run a bounded smoke check via:
     `agent_skills/software-installation/install-python-capability-package.bat check --module <module>`
   - If the smoke check fails because of sandbox, network, or permission
     restrictions rather than a real import/runtime failure, immediately raise
     an elevated prompt and rerun the same command instead of stopping in chat
     first.
   - If already available and usable:
     - skip installation
     - still verify whether the repo manifest already records the dependency
4. Try the governed install path first: `conda`.
   - Preferred lane:
     `agent_skills/software-installation/install-python-capability-package.bat conda-install <package>`
   - Prefer a package version/channel choice that is stable and broadly
     supported in the current environment.
   - If the `conda` install attempt fails because of sandbox, network, or
     permission restrictions, immediately raise an elevated prompt and rerun
     the same `conda` command instead of switching tools or stopping in chat
     first.
5. Verify immediately after the `conda` install attempt.
   - Re-run the smoke check.
   - If this verification rerun fails because of sandbox, network, or
     permission restrictions, immediately raise an elevated prompt and rerun
     the same smoke check.
   - If the capability is now usable, continue to manifest recording.
6. If `conda` cannot provide the package cleanly, use bounded `pip` fallback.
   - Only fall back when the package is unavailable, incompatible, or broken in
     `conda` for the current environment.
   - Use:
     `agent_skills/software-installation/install-python-capability-package.bat pip-install <package>`
   - Do not use global `pip install`.
   - Record why `conda` was insufficient.
   - If the fallback command fails because of sandbox, network, or permission
     restrictions, immediately raise an elevated prompt and rerun the same
     `pip` fallback command.
7. Verify immediately after the `pip` fallback.
   - Re-run the smoke check.
   - If this verification rerun fails because of sandbox, network, or
     permission restrictions, immediately raise an elevated prompt and rerun
     the same smoke check.
   - If the capability is still unusable, stop and report the blocker instead
     of claiming the package was added successfully.
8. Record the dependency in the repo manifest in the same work wave.
   - For `mfuton`, add the package to `requirements.txt`.
   - Prefer explicit version recording based on the installed usable version.
   - Keep the manifest minimal; do not add unrelated packages in the same wave.
9. Record the adoption in the owning artifact neighborhood.
   - Update the owning mission/doc/receipt if the capability matters to the
     current lane.
   - State:
     - what was reused vs newly installed
     - whether `conda` or `pip` was used
     - the smoke-test evidence command
10. Stop cleanly on unresolved ambiguity.
    - Stop if package naming/import mapping is unclear.
    - Stop if the install would require leaving the governed `codex`
      environment.
    - Stop if the target repo manifest owner is unclear.

## Decision Rule

Default install order:
1. check whether the capability already exists in `codex`
2. if installation is needed, try `conda` first
3. if `conda` is insufficient, use `pip` inside `codex`
4. if the install succeeds, record the package in the repo requirements
   manifest in the same work wave
5. if verification fails, stop and report rather than normalizing a broken
   dependency state

## Outputs

- one of:
  - verified existing capability already present in `codex`
  - verified new capability installed through `conda`
  - verified new capability installed through `pip` inside `codex`
  - explicit blocked outcome with reason
- updated repo requirements manifest when the dependency is adopted
- smoke-test evidence for the capability

## Stop Conditions

Stop and report if:
- the capability can already be satisfied by an existing local owner or stable
  library and no new package is needed
- the package cannot be verified after install
- the install path would require an unmanaged/global Python mutation
- the manifest owner cannot be identified honestly

## Case Anchors

- `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-irc-command-typed-surface-and-skill-promotion.md`
  - used stable `irc` package reuse, recorded in `requirements.txt`

## References

- `library/foundational/do-not-recreate-the-wheel.flexiarg`
- `library/social/agent-python-capability-packages.flexiarg`
- `algorithms/create-algorithm.md`
- `agent_skills/software-installation/install-python-capability-package.bat`
- `agent_skills/development/codex-conda.bat`
- `agent_skills/development/codex-python.bat`
- `requirements.txt`
