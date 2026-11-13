# MCP Algorithms

This subdirectory holds bounded algorithms for installing and integrating MCP
servers into this local `mfuton` installation.

Intent:
- keep MCP install/integration procedure reusable instead of rediscovering it
  from GitLab/Matrix/GitHub mission notes every time
- separate installation procedure from:
  - MCP protocol doctrine
  - mission-specific scope and acceptance
  - bounded local companion lifecycle control

Boundary:
- the owning mission still decides what MCP lane should exist, what transport
  or posture it is allowed to use, and what should count as success
- this directory owns the replayable installation/integration procedure once
  that mission-level decision is made
- local companion lifecycle wrappers under `agent_skills/mcp/` are only
  created when the chosen posture truly needs bounded local start/status/stop
  control
- this directory does not own Codex host ontology, general MCP doctrine, or
  arbitrary external server management outside the mission-owned local install
  set

Current contents:
- `install-mcp.md`
  - install a mission-owned MCP server into this local system by choosing the
    honest payload/mount/companion shape, landing any installation-local
    payload under `mfuton/tools/mcp/`, proving the mounted surface works, and
    updating the tracked Codex MCP inventory/snapshot surfaces
