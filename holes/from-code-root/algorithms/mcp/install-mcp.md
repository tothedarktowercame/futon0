# Algorithm: Install MCP

Date: 2026-03-27
Status: active

## Purpose

Use this when a mission-owned MCP lane is ready to be installed into the local
system and the work needs a reusable procedure rather than another one-off
install narrative.

This algorithm exists because the current local MCP pattern is already real,
but it has been spread across mission notes and local companion examples:

- installation-local third-party payloads under `mfuton/tools/mcp/`
- bounded local companion lifecycle wrappers only when a local server truly
  needs repo-owned `start/status/stop`
- mounted Codex MCP registration in the live Codex config
- tracked local inventory/snapshot updates in:
  - `config/codex-mcp-installed-inventory.json`
  - `config/codex-config.snapshot.toml`

The goal is to make that install/integration path explicit without confusing
it with:

- MCP protocol doctrine
- mission-specific scope/acceptance
- local companion creation as an automatic requirement

## Companion Flexiarg Resolution

Governing companion flexiargs for this algorithm:
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the strongest already-available upstream MCP server or
    payload rather than inventing unnecessary local substitutes
- `library/social/prefer-repo-local-data-tmp-before-os-temp.flexiarg`
  - governs repo-local runtime scratch/log/state placement when the install
    adds a local long-lived companion process
- `library/social/extract-classical-automation-from-novel-evolver-work.flexiarg`
  - governs factoring the stable installation spine out of live one-off work

Inspected but non-owning candidate:
- `library/system-architecture/typed-command-control-plane-authority.flexiarg`
  - relevant when the install leads to typed command/control surfaces, but not
    the direct owner of the MCP installation procedure itself

## Inputs

- the owning mission and its hard constraints
- the chosen MCP server/project
- the intended transport posture
- the intended auth posture
- whether the chosen local system shape is:
  - installation-local payload under `tools/mcp/`
  - hosted remote endpoint only
  - installation-local payload plus bounded local companion lifecycle
- the current Codex MCP config and tracked local snapshot/inventory state

## Preconditions

1. A mission already owns the MCP lane and states what transport, posture,
   capability family, and completion gates are allowed.
2. The chosen server/project has already been inspected enough that the
   install target is not just a vague candidate set.
3. The local install shape is honest:
   - installation-local payload if we are actually bringing third-party server
     code into this installation
   - no fake local payload if the truthful shape is hosted remote-only
4. If a local companion is being considered, there is a concrete reason for
   repo-owned `start/status/stop` rather than simply liking lifecycle symmetry.

## Procedure

1. Start from the mission-owned lane, not from a package manager command.
   - Re-read the owning mission.
   - Extract the hard constraints that matter to install shape:
     - transport
     - read/write posture
     - capability family
     - payload-location or local-companion requirements
2. Inspect explicit reuse first.
   - Check `mfuton/tools/mcp/` for an existing payload.
   - Check `agent_skills/mcp/` for an existing bounded local companion.
   - Check:
     - `config/codex-mcp-installed-inventory.json`
     - `config/codex-config.snapshot.toml`
     - `C:/Users/rob/.codex/config.toml`
   - Reuse or update an existing owner when that is cleaner than creating a
     duplicate payload or duplicate Codex mount.
3. Choose the honest local system shape before installing anything.
   - If the mission requires an installation-local third-party payload:
     - place it under:
       `mfuton/tools/mcp/<server-name>/`
   - If the truthful shape is hosted remote-only:
     - do not invent a local payload checkout just for symmetry
   - If the server must run locally as a long-lived process:
     - decide whether repo-owned bounded lifecycle control is actually needed
4. Gate local companion creation honestly.
   - Create or update a local companion under `agent_skills/mcp/` only when
     the system really needs bounded local `start/status/stop` for a
     long-lived local MCP server.
   - Treat the local companion as a narrow lifecycle surface first:
     - start
     - status
     - stop
   - Do not create a companion just because another installed MCP has one.
   - If a new local companion is warranted, route into:
     `algorithms/create-new-agent-skill-with-typed-surface-grouping-python-wrapper-and-tests.md`
5. Install the upstream payload honestly.
   - Use the upstream project’s real install method.
   - Keep the installation-local payload under `mfuton/tools/mcp/` when that
     is the chosen local shape.
   - If local compatibility patches are needed, keep the patch set explicit
     and reason-readable rather than silently treating the payload as pristine.
6. Bind auth posture without leaking secrets into tracked config.
   - Keep secrets out of tracked snapshot artifacts unless the mission
     explicitly says otherwise.
   - Prefer env-backed or local runtime auth handling when that matches the
     current installation posture.
   - Make the auth caveat explicit in the inventory.
7. Mount the MCP into the live Codex config.
   - Update:
     `C:/Users/rob/.codex/config.toml`
   - Keep the mount aligned with the mission-owned constraints:
     - correct transport
     - correct read/write posture
     - correct URL or command shape
   - If the truthful local shape is direct mounted HTTP, do not add an extra
     wrapper layer merely for symmetry.
8. Prove the mounted surface directly.
   - Verify the mounted MCP surface reaches:
     - initialize
     - tool/resource discovery as applicable
     - one bounded direct proof call in the mission-owned capability family
   - Record honest caveats if the server is installed but not yet usable in
     the intended live posture.
9. Refresh the tracked local install records in the same run.
   - Update:
     - `config/codex-mcp-installed-inventory.json`
     - `config/codex-config.snapshot.toml`
   - Record:
     - source project
     - transport
     - auth posture
     - runtime manager, if any
     - direct proof
     - restore caveats
10. Update the owning mission/docs neighborhood.
   - Record the actual installed subset, current local shape, and any local
     caveats in the strongest local mission/docs owners.
   - Do not leave the install state knowable only from `config.toml`.
11. Offer orthogonality if the install changed broader MCP topology.
   - If the install introduces a new payload family, new companion family, or
     a changed ownership split, offer:
     `algorithms/conduct-knowledge-orthogonalization-pass.md`

## Decision Rule

1. Mission-owned constraints decide the allowed MCP install shape.
2. Installation-local third-party payloads belong under `mfuton/tools/mcp/`
   when the truthful local system shape includes a local payload.
3. A local companion is justified only for bounded local lifecycle control of
   a long-lived local server, not as an automatic symmetry rule.
4. The install is not complete until the mounted surface is directly proven
   and the tracked inventory/snapshot are updated in the same run.

## Outputs

- one installed or updated MCP lane in the local system
- honest classification of:
  - local payload versus hosted remote-only
  - direct mount versus local companion-managed server
- updated live Codex MCP config
- updated tracked MCP inventory and config snapshot
- updated mission/docs neighborhood that records the actual local install
  posture

## Stop Conditions

Stop and ask the operator if any of these happen:

- the owning mission does not yet make the allowed install shape clear
- the chosen server is still only one vague option among several
- the truthful local shape is disputed:
  - local payload versus hosted remote-only
  - direct mount versus bounded local companion
- the install would require storing secrets in tracked repo artifacts without
  explicit approval
- the mounted surface cannot be directly proven after install

## References

- `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-mcp-as-protocol-shaped-typed-surface-projection.md`
- `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-mcp-as-protocol-shaped-typed-surface-projection/M-first-http-transport-gitlab-mcp-typed-surface.md`
- `holes/missions/M-eal-language-spec/M-eal-session-stack-runtime-model/M-mcp-as-protocol-shaped-typed-surface-projection/M-first-github-mcp-query-and-enrichment-surface.md`
- `agent_skills/mcp/README.md`
- `config/codex-mcp-installed-inventory.json`
- `config/codex-config.snapshot.toml`
- `docs/mcp-protocol-coarse-read/community-discovery-gateway-and-gitlab-servers.md`
- `algorithms/create-new-agent-skill-with-typed-surface-grouping-python-wrapper-and-tests.md`
- `algorithms/conduct-knowledge-orthogonalization-pass.md`
