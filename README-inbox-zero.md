# README-inbox-zero — keeping working trees clean

**Goal.** No repo, on any box, carries uncommitted work that is older than a
day. Not because tidiness is a virtue, but because of what a dirty tree costs —
measured below, on 2026-08-14.

**Status.** Dionysus `futon3c` reached zero on 2026-08-14. Holding it there for
a week is the open experiment. Jujutsu was tried as a mechanism the same day
and parked; that is recorded at the end as a concluded sub-experiment, not as
the plan.

## Why — three costs, all measured on one day

**1. Finished work sits unstaged and is forgotten.** Dionysus `futon3c` held 29
uncommitted files aged up to 42 days. Fourteen were *finished work nobody had
staged*: three test namespaces that pass (28 tests, 73 assertions, 0 failures),
five authored documents of 5–14 KB, four systemd units that were new siblings
of already-tracked infrastructure. None was contentious once looked at. Nothing
in the workflow made anyone look.

**2. The same knowledge gets paid for twice.** One of those unstaged tests,
`test/futon3c/agency/invoke_activity_test.clj`, documents a 2026-08-03 finding:
an invoking lane's activity string must carry its own age, established after
three codex lanes past the soft cap were wrongly reported wedged. On 2026-08-14
that exact insight was re-derived from scratch, over a morning, while
diagnosing `reconcile-stale-invoking!` — because the evidence of it had never
been committed and so, functionally, did not exist.

**3. A dirty tree is an unreliable narrator, and agents believe it.** This is
the expensive one. Zone's working tree had tracked files deleted but not
committed, and local commits that duplicated commits already on origin. Three
agents — `oxf-codex-1`, `ams-claude-2` and `claude-3` — each drew a confident,
wrong conclusion from it:

- a deleted-but-committed file was reported as a missing artefact, producing an
  inventory of "eight artefacts absent from Zone" of which four were in zone's
  git the whole time;
- duplicate shas were read as stranded work, producing the claim that "nobody
  outside zone has *Repair whole-index Tier-0 retrieval*" when it was the tip of
  `origin/master`.

Those conclusions were then relayed between agents and compounded. A filesystem
check (`[ -e path ]`) against a dirty tree is not evidence about a repository,
and everyone involved treated it as though it were.

## Definition

> **Clean** = at a daily check, `git status --porcelain -uall` contains nothing
> older than 24 hours that is not ignored by design, **and the repo is not
> behind its origin**.

This permits a turn's work in progress and forbids accumulation. It is the bar
the week is measured against.

The second clause was added 2026-08-14 after the first one alone declared a
broken box clean — see *Zero dirt is not inbox zero* below.

## Baseline and the run to zero (2026-08-14)

Dionysus `futon3c`, non-evidence dirt: **29 files, oldest 42 days, median 5–9
days.** Taken to **0** in six commits:

| what | how |
|---|---|
| 3 test namespaces, unstaged 8–10 days | committed; verified passing first |
| 5 documents, unstaged 5–12 days | committed as authored |
| 4 systemd units, unstaged 9 days | committed — siblings of tracked infra |
| 13-file apm-driver slice, 1559 insertions | committed; 102 tests pass |
| LaTeX artifacts, `logs/data/` run output | ignored |
| `data/proof-state/` runtime state | ignored (negation reversed) |

The remaining 154 files are a catalogued evidence corpus, deliberately out of
git, being relocated under root `data/` where it is already ignored.

Zone `futon3c` still carries 98 untracked plus 5 modified; that is the mirror
of this exercise and is owned separately.

## Rules that came out of it

**Only the ROOT `data/` is ignored**, via `.gitignore:15 data/*`. Nested
`data/` directories are **not** — the bare `data/` rule was deliberately
removed on 2026-08-11 because it "excluded the DIRECTORY, deadening every
`!data/…` negation AND silently keeping the 44-record experiment-frame corpus
out of history." Any scheme that puts data in `holes/labs/*/data/` will not be
ignored and will not help. Verify with `git check-ignore -v` on a probe file,
never by assumption. *(This was got wrong once already.)*

**Raw data goes under root `data/` and is ignored.** Syncing it between boxes
is then a separate, explicit mechanism —
`scripts/backup_evidence.sh` is the existing manifest-driven, sha-verified
basis. Ignored data does not travel through git, so the sync has to be real.

**Zero dirt is not inbox zero — being behind counts.** (Joe, 2026-08-14: "we
thought we had fixed Lucy but were missing commits, so Lucy was *not* at inbox
zero.")

lucy was declared fixed on the strength of a clean working tree. It then wedged.
Surveying every repo on it afterwards:

| repo | dirty | behind |
|---|---|---|
| futon0 | 0 | **37** |
| futon1b | 0 | **54** |
| futon3a | 0 | **2** |
| futon3b | 0 | **3** |
| futon3c | 0 | **16** |
| futon4 | 0 | **15** |
| futon5 | 0 | **326** |

**Every one of them reported `dirty=0`.** A dirt-only check calls that box
clean. It could not serve: futon1b was a month behind futon3c, so every
`/health` probe ran a full corpus count over 365,131 hyperedges — old code doing
what current code forbids outright ("Liveness must never materialize or count
the corpus") — and the box served for minutes, then wedged, and ingest never
passed cycle 1.

Note what makes this the *same* failure as an uncommitted file rather than a
different one: in both cases a host holds a state nobody else has, invisibly,
until something forces the question. Dirt is local work missing from the
remote; being behind is remote work missing from the local. Both are one host
disagreeing with the record, and only the first was being measured.

**Coherence is a property of the set, not of each repo.** This is the sharper
half. I pulled lucy's futon3c 445 commits forward and restarted, and it broke
*worse* — because moving one repo to a new generation while its dependencies
stay at an old one is not an improvement, it is a mismatch. The box only came
up after all ten futon repos were brought to one generation **before** the
restart. Check the whole dependency set; a per-repo green is not a green box.

**We run on `main` or `master`, not on branches.** (Joe, 2026-08-14.) A runtime
dependency that exists only on a feature branch makes every host's ability to
start depend on which branch it happens to be sitting on — and that is
invisible until something stops.

Measured that day: `futon3c/src` requires `futon2.aif.memory-contract` from
four namespaces (`wm_memory`, `memory_lifecycle`, `dispatch_with_recall`,
`memory_recall`). That namespace was added by futon2 `b48e463` and exists
**only on branch `M-propagators-ant-gate`** — never on `main`. Dionysus
happened to be on that branch, so futon3c started here. lucy was on `main`, so
lucy's futon3c could not start **at all** — and nobody knew, because its JVM
had been up for thirteen days holding code loaded back when its checkout still
provided the namespace. The box was unrestartable for an unknown period and the
only way to discover it was to stop it.

Note the shape: this is the *same* failure as an uncommitted file, one level
up. Work that exists only in one place, invisibly load-bearing, discovered when
the place goes away. A branch is a working tree that happens to have commits.

Current violations on Dionysus (2026-08-14):

| repo | branch | note |
|---|---|---|
| `futon2` | `M-propagators-ant-gate` | **load-bearing** — carries `memory-contract`; merge to `main` is the real fix, not putting every host on the branch |
| `futon5` | `M-propagators-2026-07-15` | four commits of today's sweep landed here |
| `mathlib4` | `darktower` | **exception**: an upstream fork where `master` is mathlib's, so `darktower` *is* our main |

**Uncommitted deletions of tracked files are accidents until proven otherwise.**
Restoring them loses nothing, since a deliberate deletion can be redone and
committed. Applied to 81 files on zone `futon3c` and 34 on zone `futon6`,
including four technical notes.

**Back up before any sync surgery.** `git diff > backup.patch` before
`checkout -- .` / `reset --hard`, and check separately whether incoming commits
touch the files you are about to re-apply. Used twice; both syncs were
non-destructive as a result.

## Mechanisms still on the table

The goal is routine commits onto master, not merely a one-off sweep — a
cleanup that is not held by a mechanism will silt up again within days. The
mtime distribution on zone showed continuous daily accretion across ten days,
not a single abandoned batch, which is what makes a mechanism necessary.

1. **Turn-end promotion.** At `agent-chat--on-turn-end` (`agent-chat.el:190`,
   invoked at `:572`), scope to what the turn touched, gate on
   compile + clj-kondo, commit and push. Held promotions must be **loud** —
   surfaced in `*agents*` the way parks are — because a silent hold is how a
   four-day slice happens.
2. **A dirty-base gate on dispatch.** Refuse to bell a packet whose target
   files are already dirty. This would have prevented the day's worst
   self-inflicted error: a packet that required committing a caller while
   leaving its callee uncommitted, which put a non-compiling commit on master.
3. **A daily `check-clean` reading.** Count and max-age per repo. Without it,
   "clean for a week" is an impression rather than a measurement.

A compile check before push would have caught **all three** non-compiling
commits seen on 2026-08-14 — two of them ours, one upstream
(`No such var: turn-queue/queue-view`, where a caller was pushed and its
definition left uncommitted on another box for three days).

## Appendix — the Jujutsu sub-experiment (2026-08-14, parked)

Tried because auto-snapshot, per-agent workspaces and cheap undo are jj's core
model, and building them ourselves would have been reinventing it.

**What it got right.** Within an hour of colocating, it surfaced all 29 dirty
files including the 14 of finished-but-unstaged work — the single most useful
thing anything did that day. It also demonstrated its own best argument:
rebasing six commits onto a 108-commit-newer master changed every git sha
(`b5785805`→`f3a8389c`, …) while change-ids (`krqsqpnn`, `lnuussnn`,
`osyzrkvt`) survived untouched. A `turn→commit` dataset keyed on sha rots
whenever history is rewritten; keyed on change-id it does not. And
`jj commit <path>` carved out a single file with no staging ritual — the exact
primitive a promotion gate needs.

**Why it was parked.** In colocated mode jj marks every untracked file
intent-to-add in git's index. That is not passive:

- `git commit -a` went from committing 10 files to 184 on Dionysus, 261 on
  zone — it stages tracked *and* intent-to-add paths;
- `.gitignore` stops working on any already-exposed path until
  `jj file untrack` runs;
- **`git rebase` and `git pull --rebase` fail outright** with
  `Entry '…' not uptodate. Cannot merge. / fatal: Cannot autostash`.

The last one ended it. The trial had been rescoped to "jj read-only, git for
all writes" precisely to avoid interference, and this proved that scope
impossible: reading nothing but `jj log` still leaves an index that breaks a
routine git operation for every git user on the repo. With eight agents rooted
at `/home/joe/code` all using git, that is disqualifying regardless of jj's
merits.

Three earlier incidents — detached git HEAD, a conflicted `master` bookmark, a
rebase that silently reverted a file on disk — were **self-inflicted by
interleaving git and jj writes**, and are not charged against jj. The rule they
produced is worth keeping for any future attempt: *one tool per repo; write
with git or with jj, never both.*

**Never exercised**, so nothing is claimed for them: `jj workspace add`,
conflict-tolerant rebase, `jj undo` on a real recovery.

**If revisited**, the honest test is a repo where jj is the *only* interface
and no agent uses git — not colocation alongside a git-using fleet. Removal is
`rm -rf .jj`; git history, worktrees and the working tree are untouched by it.
Turned off on Dionysus `futon3c` (1.1M), Dionysus `futon0`, and zone `futon3c`
(632K) on 2026-08-14.

## Log

- **2026-08-14** — Goal set: no uncommitted work older than a day. Dionysus
  `futon3c` 29 → 0. jj tried, rescoped once, parked the same day. Zone
  `futon3c` and `futon6` synced and their deletions restored. Mesh-wide compile
  break closed upstream (`ed00baf0`). Evidence corpus relocation to root
  `data/` in flight.
