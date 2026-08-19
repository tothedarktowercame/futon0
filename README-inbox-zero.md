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

**A dispatch inherits the base of whatever checkout it lands in — 355 commits,
2026-08-19.** This is the sharpest version of "being behind counts" so far,
because being behind did not stop anything. It silently redefined what was
being tested.

A packet was belled to `codex-10` to add bounded retry to futon3c's evidence
append. It came back clean: implementation correct, clj-kondo 0/0, paren check
OK, focused namespace 14 tests / 48 assertions / 0 failures. The review re-ran
every gate independently rather than trusting the report, found a real defect
in the retry schedule, fixed it, and re-verified with a live kill-and-restart
against a running store. All of that was honest work.

**All of it was performed against a base 355 commits and three days stale.**
Dionysus `futon3c` sat at `42196c67` (08-14) while `origin/master` was at
`33e6493c` (08-17). Nobody noticed until the branch state was checked before
pushing — *after* the review had concluded.

Note what this is not. The tree was clean: `dirty=0`. It was not a case of
unpushed work. The failure is the third variant of the same shape:

| | what is missing where |
|---|---|
| dirty tree | local work missing from the remote |
| behind | remote work missing from the local |
| **stale-base authoring** | **new work built on top of the second one** |

The result is a commit that is simultaneously 2 ahead and 355 behind, and a
green test run describing a tree that exists on exactly one machine. The
evidence that the base mattered is direct: the same suite ran 2,499 tests at
base and 2,504 after, and once rebased the changed namespace went from 17
tests / 53 assertions to 18 / 55 — upstream had added a test to the very file
under review, which the reviewer had never seen.

It was salvageable, which is luck rather than method: the rebase produced zero
conflict markers, only one upstream commit had touched each file, and that
commit (`c1050842`, "Unify problem subject vocabulary") changed the *read*
path while the packet changed the *append* path. Every gate was re-run on the
new base afterwards. Had the two overlapped, the review verdict would simply
have been void.

**The rule: state the base sha in the packet, and check the recipient is not
behind before belling.** `git rev-list --count HEAD..@{u}` is the whole check.
A packet that says "implement X in file Y" contains no statement about *which*
Y, so base freshness is part of the handoff contract and was not part of this
one. This extends the dirty-base gate proposed below (*Mechanisms still on the
table*, #2) from dirty files to behind branches — the proposal was written for
uncommitted work and this shows the same gate is needed for stale ones.

**A monitor that degrades toward "fine" is worse than no monitor — 2026-08-19.**
The mechanism this document asks for in #3 below already existed, as
`scripts/futon-sync.clj`: 393 lines, manifest-driven across 19 repos, with
`status`, `review`, `pull`, `push`, `park` and `hygiene` subcommands. It was
wired to nothing — no timer, no service, no caller anywhere in the stack. The
capability had been written and the automation never added, which is the same
shape as a `FOLLOW-UP:` comment left in code for a year while the problem it
names becomes the blocker.

Worse, running it would not have told the truth. It computed ahead/behind from
`HEAD...@{u}` and **never fetched**, so `=` meant "equal to this machine's ref
as of its last fetch" rather than "equal to the remote". The failure points
toward false confidence, and it is largest exactly where it matters most: on a
box that has stopped fetching, everything reads in sync.

Measured on one laptop, same machine, minutes apart:

| | repos | behind | dirty |
|---|---|---|---|
| before the fix | 17 | **1** | 2 |
| after the fix | 17 | **5** | 3 |

`futon3` was **27 commits behind and reported `=`**; `futon4` ↓7, `futon3a` ↓6,
`futon7` ↓1 were all invisible. The tool was under-reporting drift by four
repos while presenting a green dashboard.

This is the same error as everything else in this document, applied to the
instrument: *a conclusion about a population — the remote — drawn from a proxy
that never looked at it.* `git log --all` shares the trap, since "all" means all
**local** refs; on an unfetched tree it returns a confident empty answer.
`git ls-remote` is the one that asks. Fetch is now the default and `--no-fetch`
is the opt-out.

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

## The fleet, measured for the first time (2026-08-19)

The stated goal is that every repo on every box stays in lock step and clean.
Here is what that goal was actually facing. Five boxes carry the repos
(`hyperreal` carries none), 73 repo-instances, 18 distinct repos:

    17 repos present on more than one box:  2 in lock step, 15 DIVERGED

`futon1` is identical everywhere. `futon1a` agrees on three boxes and differs on
the two Linodes. `futon3c` exists in **four different states at once**. The two
Linodes are byte-identical to each other on every repo and systematically older
than everything else — a mirrored pair, one generation behind.

**Lock step is the wrong target, and that is why it keeps feeling tricky.** The
boxes are not five copies of one workspace; they are doing different work. Zone
runs the APM series, this laptop runs whatever I am on, the Linodes are an older
deployment. Demanding they be identical means either pushing and pulling every
change everywhere the moment it happens, or blocking work until they agree.
Neither survives contact with an actual day.

The invariant worth holding is narrower and achievable:

> **Nothing exists in only one place.**
> *Behind* is benign as long as it is visible. *Ahead* and *dirty* are the
> dangerous states, because those are the ones that vanish with the disk.

Measured against that invariant instead, the same fleet reads:

| box | unpushed commits | dirty files |
|---|---|---|
| dionysus | 0 | 20 |
| zone-joe | **35** | 30 |
| lucy-joe | 0 | 18 |
| linode-chicago | 0 | 1271 |
| linode-joe | 0 | 1271 |
| **fleet** | **35** | **2610** |

## Two kinds of dirt, with opposite remedies

Counting them together is what makes the problem look both enormous and
hopeless. They are not the same thing:

**Real uncommitted work** — 35 commits, all on Zone, four repos. This is the
whole risk, and it is the number that matters. It is also small enough to fix in
an afternoon.

**Generated noise** — of the 2,610 dirty files, **2,012 were one missing
`.gitignore` pattern**. The Linodes name their XTDB store `chicago-store/`; the
rules enumerated `migration-store*`, `switchover-store`, `ams-store.retired-*`
and every other name the repo had happened to see. A list of instances rather
than a description of the kind, failing silently on the first box that picked a
new name — 1,006 untracked `.binpb`/`.arrow` files per box, from one unlisted
directory. Fixed in futon1b as `*-store*/` (`49cc714`), after verifying that
every `*-store*` directory on every box is a store and none is tracked anywhere.

Those boxes looked neglected for months. They were missing a pattern.

The general form, which this document has now hit in four places: **a rule that
enumerates cases looks complete and fails silently on the case in front of you.**
Ignore rules, checklists, and the `futon-sync` dashboard before it fetched are
all the same shape.

## The Street Sweeper already solved this, in May

`futon3c/src/futon3c/peripheral/street_sweeper.clj` and siblings — 2,284 lines,
29 deftests, 94 assertions passing — read the dirty working trees and package
them into semantically chunked, ready-to-apply patches. Exactly the job. It
works. Inspected 2026-08-19, the patches are sensible: a `load-prefer-newer t`
in a bootstrap file, a mission-doc checkpoint, load-path additions.

It ran on **2026-05-25**. It has not run since. There is no timer, no unit, no
Makefile target, no hook — the same unwired state as `futon-sync`, as
`library-check`, as the `FOLLOW-UP:` comment that became a blocker a year later.

But the wiring is the smaller half. Look at where it terminates:

> **Status:** ACTIVE — defer-queue awaits **operator review**

    115 packet directories
    2,551 patches
    10 repos
    all dated 2026-05-25

That is the inbox. It has been non-zero for three months. **The mechanism built
to deliver inbox zero produced an operator queue of 2,551 items and stopped.**

### And the queue decayed to nothing

Sampled 60 of those patches on 2026-08-19 and tested each with
`git apply --check`:

    still apply cleanly :  0
    no longer apply     : 56
    repo not found      :  4

Zero. The trees moved on and every packet died on the vine.

Meanwhile the fleet now carries ~68 real dirty files, not the 929 that prompted
the excursion. So the work itself was not lost — **it was redone by hand**, in
one of the sweeps this document exists because of. The Street Sweeper did the
job correctly in May and a person did it again anyway, because the output was a
proposal and proposals need someone to say yes.

### The rule this establishes

**A mechanism whose output is a human queue is not a mechanism. It is a
deferral, and deferred work decays.**

This is the strongest evidence in the document, and it retires an idea that
appeared in this very file earlier the same day: an "escalate the outlier"
threshold on auto-push, justified as keeping a human informed. Joe rejected it
immediately — *"nothing should be routed to me unless absolutely necessary"* —
and the Street Sweeper is what that rejection looks like when it goes the other
way. 2,551 patches, three months, zero applied.

The test for any proposed mechanism here: **does its output act, or does it
wait?** If it waits, it will silt up at exactly the rate the work arrives, and
the silt will be worthless by the time anyone looks.

Joe's own framing at the excursion's start already said this, and was not
followed:

> "I do broadly trust either claude or codex to deal appropriately with a mere
> 929 dirty paths (piffle!) — but actually we'd need some invariants in place to
> make sure we're doing that sensibly."

The invariants were built — the INV catalog grew 9 → 22, there is a per-repo
`.sweeper-policy.edn`. The trust was not extended. What shipped was the
guardrail without the action it was meant to make safe.

## Why fifty sweeps did not hold

Pushing the commits and packing up the dirty files has been done many times. It
has never once changed the outcome, and the reason is visible in the numbers
above: **every sweep treated one undifferentiated pile.**

A pile that contains judgement cannot be automated. So it stays manual. So it
recurs. Each sweep moves items out of the pile without changing what the pile is
made of, which is why the next one arrives on schedule.

The 2,610 dirty files are not one problem. They are three, with different
remedies and — critically — **different amounts of human judgement required**:

| class | fleet count | judgement needed | remedy |
|---|---|---|---|
| committed but unpushed | 35 commits | **none** | mechanical, automatable today |
| generated noise | ~2,012 files | **none, once per kind** | describe the kind in `.gitignore` |
| real uncommitted work | ~68 files | **all of it** | a person, per file |

Read that way the problem is not large and it is not hopeless. It is one timer,
a handful of one-time pattern fixes, and a queue of about sixty files.

### Committed-but-unpushed is not a hygiene problem, it is mechanics

**The author already declared the work done by committing it.** Pushing carries
no judgement whatsoever — it is the difference between a decision that exists on
one disk and a decision that exists somewhere else too. That is the entire
"exists in only one place" risk class, and it needs no human at all.

The objection is that automation might push a broken commit. Note the
asymmetry: a broken commit that is pushed is a **code-quality** problem, visible
and revertible. A broken commit that is *not* pushed is a **data-loss** problem
sitting on one disk. Not pushing does not make the commit less broken; it makes
it less recoverable. Where a compile check is cheap, gate on it — but do not let
the gate become the reason nothing is pushed.

The mechanism is not new. `futon-sync push` has existed all along and calls
`read-line`, so it can only ever run when a human is already sitting there
deciding to do the thing. **A capability that requires the discipline it is
meant to replace is not a mechanism.** The same was true of `futon-sync status`
until it was put on a timer, and of `library-check.py` until it was wired.

### The anomaly is the thing worth a human

Auto-pushing normal work is safe precisely because normal work is small. What
deserves a person is the *unusual* accumulation: on 2026-08-19 Zone held **110
commits diverged for two days** — the whole APM series since 08-17, on one disk.
No sweep would have caught that sooner, because nothing was watching the number.

So the rule is: **push the ordinary case automatically, escalate the outlier
loudly.** A repo more than a handful of commits ahead is not routine drift, it
is a signal that something happened — a long offline stretch, a divergent
branch, a box nobody has looked at — and that is exactly when a human should be
told rather than have the evidence quietly tidied away.

### What stays manual, honestly

Real uncommitted work needs a person, and no mechanism here changes that. What
changes is that ~68 files are visible instead of buried under 2,012 files of
store data. The deeper fix is **turn-end promotion** (#1 below): commit and push
what a turn touched, at the moment the author still knows what "done" meant.
That is the only mechanism that attacks class 3 at its source rather than
sweeping it afterwards, and it is not built. It is also harness-specific — a
hook in `agent-chat` does not cover Codex or Claude Code sessions — so it is
several mechanisms, not one.

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
3. ~~**A daily `check-clean` reading.**~~ **LANDED 2026-08-19.**
   `futon-sync.timer` on the laptop: every 30 minutes, `futon-sync status`,
   which now fetches first. The fetch is the load-bearing half — it removes the
   stale-ref class outright, and it is read-only, so it is safe to run beside
   working agents (fetch never touches a working tree, HEAD, or an index).
   Verified: unit exits 0, next trigger scheduled, drift logged to the journal.
4. **Cross-machine visibility — the remaining gap.** Each box now measures
   *itself*. Nothing aggregates, so "is Zone in sync?" is still answered by
   ssh-ing to Zone. On 2026-08-19 that cost an exchange between two agents and
   a wrong claim in both directions, while 110 commits of series work sat
   unpushed on one disk for two days. The machines already talk over Agency;
   the missing piece is each host publishing its own reading somewhere shared,
   so the question is a query rather than an expedition. Not built.
5. **Gitignored and non-repo material is invisible to all of this.** `futon-sync`
   reports drift in *tracked* files, so a repo can read `0 dirty / 0 untracked`
   while holding artifacts that exist on one disk. Audited 2026-08-19 in
   [README-git-invisible.md](README-git-invisible.md): three unique items needed
   by live work, totalling under 1 MB, none of them visible to any check in this
   document. A `storage/` repo with no remote and 45,685 uncommitted files is
   not in the manifest at all.
6. **The timer exists on the laptop only.** Zone runs the same repos and has
   the same failure mode; it does not yet have the unit.

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

- **2026-08-19** — `futon-sync.clj` found already written and wired to nothing,
  and computing ahead/behind without fetching: it reported 1 repo behind where
  5 were, hiding `futon3` at 27 commits behind. Fixed to fetch by default and
  put on a 30-minute timer, which lands mechanism #3.
- **2026-08-19** — A belled packet was authored, gated and reviewed on a base
  355 commits behind `origin/master`; discovered only when checking branch
  state before pushing. Rebase was clean and all gates were re-run on the new
  base. Rule added: state the base sha in the packet and check
  `HEAD..@{u}` before dispatching.

- **2026-08-14** — Goal set: no uncommitted work older than a day. Dionysus
  `futon3c` 29 → 0. jj tried, rescoped once, parked the same day. Zone
  `futon3c` and `futon6` synced and their deletions restored. Mesh-wide compile
  break closed upstream (`ed00baf0`). Evidence corpus relocation to root
  `data/` in flight.
