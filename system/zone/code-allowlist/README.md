# ~/code is locked; Joe signs off on additions (2026-11-13)

Joe: "~/code is continuously backed up (my GitHub repos, and nothing else)"; "prevent regrowth
… while allowing agents to create or delete new repos with my permission … I'd like to
personally sign off on any additions."

- `~/code` is `chmod a-w`: no top-level entry can be created, renamed or deleted. Everything
  inside the repos stays writable, so ordinary work is unaffected.
- `allowlist.tsv` lists every admitted entry (name, kind, remote or target), committed here.
- `code-admit` is how Joe changes it: `clone <url> [name]`, `add <name>`, `remove <name>
  [dest]`, `lock`/`unlock`, `check`. Each change unlocks, acts, records, commits and relocks.
- Agents ask Joe (in their reply or a bell) for an addition or removal and say why; they do not
  run `code-admit` or `chmod` themselves.
- `code-admit check` exits non-zero if an entry is missing from the allowlist, an allowlisted
  entry is missing, or the directory is unlocked. It is the probe for an M-ways-of-working way
  ("~/code holds only what Joe admitted"); not yet on a timer or in zone-health.

State at lock time: 43 repos, 5 symlinks into ~/storage (storage, data, apm-frames, LeanEuclid,
planetmath), 4 agent-configuration entries (CLAUDE.md, AGENTS.md,
reply-proforma.md, .claude). Cleanup record: ~/notes/claude-9/cleanup/CODE-DESTINATIONS.md.
