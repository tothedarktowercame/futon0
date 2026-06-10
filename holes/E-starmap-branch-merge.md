# Excursion: E-starmap-branch-merge

**Type:** E-prefix excursion (bounded scope-out, single-agent-owned end-to-end).
**Spawned:** 2026-06-10, on the **close of `M-capability-star-map`** (operator decision by Joe via
WM pilot cycle #1; pilot claude-3) — one of the four close-caveats spun out so the mission could
close without dropping them.
**Owner:** TBD (natural: whoever holds the star-map branch / claude-1, the prior mission owner).

## IDENTIFY (the caveat)

The star-map's INSTANTIATE work lives on branch **`wm-outing/2026-06-07`**, which is **unmerged**.
Closing the mission with the branch unmerged would strand the work off `main`.

## Scope

- **IN:** review `wm-outing/2026-06-07` against `main`; merge (or cherry-pick the load-bearing
  commits) with the usual gates (clj-kondo, check-parens, conflict-marker grep, verify the committed
  + remote copy after merge); record what landed.
- **OUT:** new star-map features (those are the sibling excursions).

## Exit condition

`wm-outing/2026-06-07`'s star-map work is on `main` (or explicitly abandoned with a recorded reason),
and `git branch --merged` confirms it.
