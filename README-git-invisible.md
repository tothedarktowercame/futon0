# Git-invisible material on Dionysus — audit, 2026-08-19

Dionysus goes back this week. Everything under `~` is on the LenovoBackup disk,
so nothing here is *lost*; the question this audit answers is narrower and more
useful: **what exists only on that laptop, is invisible to git, and is needed by
work that is currently live?** Backup preserves bytes. It does not make them
reachable from Zone, and it does not tell you which of them matter.

"Git-invisible" covers four distinct cases, and only the first is the one people
usually think of:

| case | why git cannot see it |
|---|---|
| untracked files | never `git add`ed |
| **gitignored files** | **a rule excludes them — they do not even appear as untracked** |
| non-repo directories | no `.git` at all |
| repos with no remote | committed, versioned, and still on one disk |

The gitignored case is the dangerous one. `futon6` reports `0 dirty / 0
untracked` — a clean bill of health — while holding embedding artifacts that
existed nowhere else, because `.gitignore:9` is a blanket `data/*`. A tidy
status is not evidence of preservation.

## Method, and the trap in it

Size plus **recency**, on the theory that a directory untouched since March is
archive and one touched this month is live.

That theory is half wrong, and the correction matters. Three directories
reported *every* file modified within 30 days:

    corpora              56,227 files    all stamped 2026-07-28
    diagramprover-refs    1,748 files    all stamped 2026-08-02
    python-practice      11,917 files   11,776 stamped 2026-08-09

A copy rewrites mtime. Those are **bulk imports wearing a recent timestamp**,
not active work, and a naive "modified in the last 30 days" filter promotes all
three to WIP. What separates them is the *spread*: authored directories show
scattered dates (`data/`: 01-07, 01-10, 07-13, 08-19), copies show one spike.
`python-practice` is the honest mixed case — a bulk import on 08-09 with genuine
edits on 08-10, 08-11 and 08-16 sitting on top.

Recency is a proxy for authorship. Check what the proxy measures before ranking
by it.

## Findings

**Unique, live, and now preserved.** All three were shipped to Zone and verified
by checksum. Together they are under 1 MB.

| what | size | why it matters |
|---|---|---|
| `futon1bi/` | 432K | **repo with no remote.** Standalone XTDB2 text-index companion extracted from the live futon1b FTS5 sidecar; the reference implementation for the F1' removal spec (`futon7/holes/M-demonstration-foundry.F1-removal-spec.md`). Committed, clean, and on one disk. |
| `futon6/data/showcases/clean-demo/` | 140K | gitignored. Structure+text embeddings of 7 APM problems, plus `ingest/` (`clean-graph.json`, `load.cypher`, `pgvector.sql`) — the neo4j/pgvector handoff artifacts `E-clean.md` specifies for Rob's pipeline. |
| `sequel-probe/` | 452K | not a repo. Edge-of-chaos sweep generator handoff and curves for the MetaCA EoC work. |

**Reproducible — do not preserve.** `corpora/` (1.2G) is a `leanprover-zulip-archive`
and `xtdb-issues` mirror, re-downloadable. The `storage/*-processed-gpu` trees
(81G) are derived embeddings: expensive to recompute, but derivable, and their
inputs are elsewhere.

**Unique but dormant — archive, do not sync.** `_linode_reclaimed/` (16G, a
reclaimed server dump from April), `planetmath/` (2.3G), `excitement-to-evidence/`
(576M), `tts/` (878M). No activity in 30 days. These are exactly what the
LenovoBackup disk is for.

**The `storage/` problem, unchanged.** A git repo with **no remote at all**, HEAD
at 2026-03-30, **45,685 uncommitted files**, and over 300G on disk (`mark2` 126G,
`math-processed-gpu` 70G, `futon6` 45G, `zoomr4` 29G, `futon1a` 27G). It is not in
the `futon-sync` manifest, so nothing watches it. Most of that volume should
never sync anywhere; the point is that no one can currently tell which part of
it is the exception.

## The shape of the gap

Of roughly 82G of candidate material, the amount that was both unique and needed
by live work was **under 1 MB** — about 0.001%. That ratio is the argument
against syncing data wholesale and *for* an explicit opt-in list: the useful
fraction is small enough to carry anywhere, and only findable by asking what
current work depends on.

`futon-sync` cannot help here yet. It reports tracked-file drift, and every item
in the preserved table above was invisible to it — two by being gitignored or
non-repo, one by being a committed repo with nowhere to push. See
[README-inbox-zero](README-inbox-zero.md) mechanisms #4 and #5.
