# AGENTS.md

## Repo Hygiene Tool

Use `scripts/futon-sync.clj` for cross-repo git maintenance in the FUTON stack.

- Status dashboard:
  - `bb scripts/futon-sync.clj status`
- Review dirty/untracked files repo-by-repo before cleanup:
  - `bb scripts/futon-sync.clj review`
- Pull updates (rebase + autostash):
  - `bb scripts/futon-sync.clj pull`
- Push repos that are ahead (with confirmation):
  - `bb scripts/futon-sync.clj push`
- Stash tracked + untracked work across dirty repos:
  - `bb scripts/futon-sync.clj park`
  - `bb scripts/futon-sync.clj park --dry-run`
- Detect noisy untracked artifacts and suggest `.gitignore` entries:
  - `bb scripts/futon-sync.clj hygiene`
- Apply suggested `.gitignore` fixes automatically:
  - `bb scripts/futon-sync.clj hygiene --fix`

The repo list is read from `data/git_sources.json`.
