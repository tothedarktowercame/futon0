# M-capability-zones S1 honest coverage — 2026-07-19

## Scope and semantics

The pinned 17-name FUTON corpus was traversed without a commit cap. For each Git repository, every commit reachable from its detected default branch was described. Merge-reachable history is included; non-default and unreachable orphan branches are not. `futonY` was present as a directory but was not a Git repository. Worktree clones were excluded. Classification used BGE `BAAI/bge-large-en-v1.5` and text-seed nearest cosine. A global margin below 0.01 resisted description; resisted items are retained.

Grains are best-first: attempt/feature text when its markers and current reachable files exist; mission/pattern text when those paths exist at HEAD; otherwise raw commit message plus paths. Historical file bodies that no longer exist at HEAD were unreachable as rich grains and therefore used the raw fallback rather than being dropped.

## Per-grain fractions

| Grain | Described / corpus | Resisted / described |
|---|---:|---:|
| `attempt-feature` | 1/6810 (0.0001) | 0/1 (0.0000) |
| `commit` | 5323/6810 (0.7816) | 1537/5323 (0.2887) |
| `mission-pattern` | 1486/6810 (0.2182) | 381/1486 (0.2564) |

## Per-repository fractions

| Repository | Default ref | Described / reachable history | Resisted / described | Reachability |
|---|---|---:|---:|---|
| `futon0` | `main` | 241/241 (1.0000) | 82/241 (0.3402) | reachable |
| `futon1` | `main` | 205/205 (1.0000) | 77/205 (0.3756) | reachable |
| `futon1a` | `origin/main` | 118/118 (1.0000) | 45/118 (0.3814) | reachable |
| `futon1b` | `master` | 33/33 (1.0000) | 5/33 (0.1515) | reachable |
| `futon1bi` | `master` | 1/1 (1.0000) | 1/1 (1.0000) | reachable |
| `futon2` | `main` | 792/792 (1.0000) | 217/792 (0.2740) | reachable |
| `futon3` | `main` | 941/941 (1.0000) | 283/941 (0.3007) | reachable |
| `futon3a` | `main` | 97/97 (1.0000) | 23/97 (0.2371) | reachable |
| `futon3b` | `origin/main` | 34/34 (1.0000) | 5/34 (0.1471) | reachable |
| `futon3c` | `origin/master` | 1559/1559 (1.0000) | 489/1559 (0.3137) | reachable |
| `futon4` | `main` | 475/475 (1.0000) | 139/475 (0.2926) | reachable |
| `futon5` | `main` | 353/353 (1.0000) | 88/353 (0.2493) | reachable |
| `futon5a` | `origin/master` | 153/153 (1.0000) | 50/153 (0.3268) | reachable |
| `futon6` | `origin/master` | 1242/1242 (1.0000) | 262/1242 (0.2110) | reachable |
| `futon7` | `master` | 180/180 (1.0000) | 50/180 (0.2778) | reachable |
| `futon7a` | `origin/master` | 386/386 (1.0000) | 102/386 (0.2642) | reachable |
| `futonY` | — | 0/0 (n/a) | 0/0 (n/a) | unreachable: not a git repository |

## Resistance and unreachable material

Resistance means the top two seed cosines were separated by less than 0.01; it is an explicit ambiguity result, not a discarded item. Full SHAs and margins are retained in `futon2/data/capability_zones/harvest-2026-07-19.edn`. No live store was read for classification or written by the harvest. Attempt/feature and mission/pattern bodies deleted from the default-branch checkout were unreachable as rich text; their commits remain covered at raw grain.
