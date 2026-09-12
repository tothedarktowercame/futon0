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
> behind its upstream, and the oldest unpushed commit is less than 24 hours
> old, an upstream is configured, and it has no linked worktree whose HEAD is
> already contained in the default branch or whose checkout is outside the
> repository's sibling tree**.

This permits a turn's work in progress and forbids accumulation. It is the bar
the week is measured against.

The second clause was added 2026-08-14 after the first one alone declared a
broken box clean — see *Zero dirt is not inbox zero* below.

The third and fourth clauses were added 2026-09-01 after the first two could
still declare finished work clean while hundreds of commits existed on only
one machine, and could silently treat a missing upstream as though comparison
had succeeded. Ahead work is permitted during a turn; it becomes inbox debt
when its oldest unpushed commit reaches 24 hours. A missing upstream is a
failed check, not a satisfied one: the behind/ahead clauses are unevaluable
without it. Clauses two through four are evaluated on the repository's default
branch, not whichever feature branch an agent currently has checked out;
feature-branch work is ordinary work in progress and is reported as
information rather than failure. The same review found that `voxterm` was
absent from the repository manifest despite being worked in daily, making its
15-day dirt and missing upstream invisible to the check. It was added to the
manifest rather than treated as outside the definition.
The manifest audit also found `p4ng` absent even though its default branch held
hundreds of unpushed commits; it was added for the same reason.

The fifth clause was added 2026-09-01 after the same audit found 20 extra
`futon3c` worktrees and 182 extra `apm-lean` worktrees consuming 8.1 GB. It
classifies by commit content, not branch attachment: a worktree is dead when
its HEAD is an ancestor of the default branch, including a detached HEAD. A
worktree with commits not in the default branch is reported as information,
with its newest-commit age and dirt count, because it may be active work. A
checkout outside the repository's sibling tree fails separately: sibling
relative dependencies cannot resolve there. This is operationally consequential,
not merely disk hygiene. On 2026-08-22, loading a namespace from a worktree 56
commits behind `master` replaced the shared JVM's current namespaces and made
every master-only route answer `Unknown endpoint` until the canonical checkout
was reloaded. The check reports these worktrees but never removes them or their
branches.

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

**Refined (Joe, 2026-08-24): loud does not mean "to Joe."** Threshold
escalations routed to the operator by default would make him field a queue of
routine requests — exactly the outcome this document exists to prevent. The
escalation router is a tier ladder ordered by *who can act*, and each tier is
tried before the next:

| tier | goes to | when | resolved how |
|---|---|---|---|
| 0 | nobody | ordinary accumulation | auto-push |
| 1 | the **responsible seats** | outlier / held plan / stale base | session-commit-links + claims say exactly whose commits these are; deliver via the followup queue, exact-seat |
| 2 | the **street-sweeper peripheral** | tier-1 seat unfindable (exact-seat validation fails: session rotated, agent gone) | orphaned hygiene becomes an ordinary sweep item |
| 3 | **Joe** | judgement only a human can make | a *hold with a question*, not a notification |

Tier 3's defining examples: *"this seems like a potentially sensitive file —
do you want it in a public repo? Holding on your word"*; an agent explicitly
declaring it cannot decide. Volume is never by itself a tier-3 signal. This
implies a **sensitivity screen before push** (the publish moment): a
describe-the-kind rule set (key material, credentials, personal data, large
binaries), configurable, deliberately not an enumeration of filenames — the
2,012-file lesson above. A sensitive hit is `:held :sensitive-content` and is
the canonical tier-3 item.

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
   four-day slice happens. The data contract and staged watcher/followup
   implementation are tracked in
   [`E-inbox-zero-implementation`](holes/excursions/E-inbox-zero-implementation.md).
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
7. **An operator seat — `*joe-repl*` (Joe, 2026-08-24: "maybe we need a
   joe-repl where agents can bell *me* with things that need attention").**
   Push policy is auto-push-ordinary / escalate-outliers, and the outlier
   escalation needs a destination. The delivery half already exists: the
   slice-4 followup queue delivers typed, deduped, busy-gated messages to an
   exact seat, and the Emacs poller injects them as turns. What is missing is
   only a registered `joe` seat with a buffer behind it — no model, just an
   inbox rendering followups/bells and letting the operator's replies go back
   as bells. Side effect worth as much as the feature: a witnessed operator
   seat makes operator attention measurable through the same fail-closed
   machinery (`M-futon-problems` D8), instead of being inferred from prose.

A compile check before push would have caught **all three** non-compiling
commits seen on 2026-08-14 — two of them ours, one upstream
(`No such var: turn-queue/queue-view`, where a caller was pushed and its
definition left uncommitted on another box for three days).

## Attribution — what the witness chain is for (2026-08-24)

The machinery built for hygiene turns out to be an attribution instrument. The
seat→file→commit chain (`E-inbox-zero-implementation`, slices 1–6) is the only
place in the stack where *who did the work* is recorded from an explicit
witness rather than inferred — and `M-futon-problems` D8 measures the cost of
inference elsewhere: autoclock mission-linkage "present on 65% of turns and
substantially wrong." Full derivation in `M-futon-problems` D15/D16; the
design, as it bears on this document:

**Turn-end promotion is where attribution is nearly free.** A promotion commit
is the one moment when seat, files, and the session's mission-clock are
simultaneously known. So the mechanism that removes "think about git commits"
is the same mechanism that closes the session→mission hop as data instead of
prose — one wire, both goals.

**Primary + crosslist, on different evidence budgets.** A commit's *primary*
mission comes from the seat's clock-in — witness-strength, because a wrong
primary corrupts the attention ledger. *Crosslists* may be structural or
inferred (path containment, substrate-2 commit co-occurrence, retrieval
exhaust — in that precision order), because a wrong crosslist only adds noise
to a deliberately generous index. Every edge names its basis, so downstream
measures can filter by evidence strength.

**Inference proposes; confirmation mints.** For unclocked sessions, inferred
attribution is delivered as a typed followup through the slice-4 queue ("this
work looks like M-x; clock in?") — the confirmation keystroke is the explicit
act that converts inference-strength into witness-strength. Infer-and-write is
what produced the 65% figure; the confirm step is the whole difference.

**"No mission" is a valid value, and an instrument.** Work witnessed to a seat
but claimed by no mission is the emergent-work detector: the mission corpus
only catches problems someone *chose*. Orphans are claimable retroactively —
a later-minted mission adds a claim record (claimant, date, basis) over the
immutable commit observation, with competing claims failing closed to
`:ambiguous` as usual. Two timestamps stay distinct: *unclaimed at commit
time* is the emergent-work signal and must not be erased by later claiming;
*unclaimed now* is the mission-minting menu. A coherent orphan cluster is
evidence a mission wants to exist — measured 2026-08-24: ~14 cost-related
commits since July across three workstreams, all mission-less, forming a
ready-made claim set for an M-cost-tracking that has not been minted. WR-11..22
set the precedent: they "were not missing — they were in bulletins 9 and 10,
unpromoted." Claiming orphans is promotion, applied to commits.

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

- **2026-08-24** — **Live case: one verified fix, traced through the whole
  pipeline (zone).** A one-function fix to `futon3c/scripts/session-cost.py`
  (SDK-driven sessions record prompts as text-block lists; the string-only
  `is_typed_prompt` test made pouch-driven sessions price as turnless) was
  finished and verified at 08:21 and deliberately left uncommitted to see what
  the machinery would do with it. What *should* happen, per
  [`E-inbox-zero-implementation`](holes/excursions/E-inbox-zero-implementation.md)
  slices 1–6: the slice-5 producer sees the successful `Edit` in claude-3's
  invoke stream → writes a seat witness and session-file claim for the exact
  seat → the watcher observes `scripts/session-cost.py :modified` → a dirty set
  of one for that seat → the 24-hour age trigger queues a followup → delivered
  into that seat's REPL buffer when idle. What *did* happen: nothing. The
  serving JVM has no `FUTON3C_INBOX_ZERO_*` environment, no witness dir, and no
  state files; only slice-4's HTTP routes are live (`/api/alpha/followups/ready`
  answers — and correctly withheld delivery during the seat's turn). All six
  slices merged and tested 2026-08-21 — and "needs a restart" turned out to be
  false: **zone's JVM was restarted 2026-08-23 and the feature correctly stayed
  off**, because the activation exports were added to `scripts/dev-laptop-env`
  only, and even there `FUTON3C_INBOX_ZERO_ENABLED` defaults to `false`; zone's
  launcher (`dev-zone-env`) got nothing. Per-box launcher scripts are an
  enumeration of cases, and zone was the case not in the list — the enumerated-
  cases failure form, fifth instance. There is also a half-activation trap:
  bootstrap falls back to `/home/joe/code/storage/inbox-zero/witnesses` when
  `FUTON3_INBOX_ZERO_WITNESS_DIR` is unset, but the slice-5 producer
  (`dev.clj` `record-inbox-zero-tool-results!`) reads that variable raw with no
  fallback and "returns nil when disabled" — so `ENABLED=true` alone yields a
  watcher that observes dirt and a producer that never writes claims: every
  path `:unattributed`, zero followups, indistinguishable from working. The
  code even documents its own precedent one line above the gate
  (`bootstrap.clj:554`): commit-ingest's prior default-false "silently froze
  substrate-2's commit/code-history layer" for a month and was fixed by
  defaulting ON. *A capability that requires the discipline it is meant to
  replace is not a mechanism*, third instance — and a default-off capability
  requires the most discipline of all.
  Meanwhile the fix is **live-serving from dirt**: every cost refresh on zone
  spawns the working-tree script, so production behavior already diverges from
  HEAD — the unreliable-narrator cost, running in the same hour it was measured.
  Measured while tracing, same repo: 12 dirty paths (4 modified, 8 untracked),
  oldest 2026-08-20 — past both the 5-count and 24-hour thresholds, but with no
  witnesses every one is `:unattributed`, and the fail-closed rule correctly
  routes no followup; and **387 commits ahead of `origin/master`, 0 behind,
  oldest 08-20** (fetch-verified; `ls-remote` agrees) — the 08-19 outlier (110
  commits, one disk) recurring at 3.5× on the same box, still class 1: zero
  judgement needed, still in exactly one place.

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

## Checkpoint — 2026-08-26

The inbox-zero machinery is active, but it was deliberately made quiet after
the initial feedback loop.

Initial activation generated 3,263 `:nothing-promotable` plans over about 30
hours, including 84 followups for one seat. Repeated turn-end checks, unstable
dedupe keys, and routing diagnostic/no-op plans as actionable work caused the
flood. Repairs landed on 2026-08-26: one promotion pass per completed turn;
exact agent/session identity required; no messages for plans with zero
promotable files; stable per-seat/repo deduplication; stale followups rejected
at delivery; and propose mode writing only to the ledger.

Live state on Dionysus at this checkpoint:

- `FUTON3C_INBOX_ZERO_ENABLED=true`, promotion mode `execute`, attribution
  sweeper enabled every 30 minutes;
- durable `state.edn` was actively updating (last write 18:46 UTC);
- 64 immutable edit witnesses, 59 file claims, 43 session→commit links, and
  305 commit observations;
- three automatic promotion commits on 2026-08-26: five paths for `claude-12`,
  then one path and one path for `claude-19`.

Successful ordinary work is therefore intentionally silent. A seat is
contacted only when it has actionable work that could not be promoted
automatically. The absence of messages is not evidence that the mechanism is
off.

The remaining blind spots explain why it can still appear inert. Codex edits
do not reach the exact-success witness boundary; neither do direct Emacs edits.
Unattributed files correctly produce no guessed-author notification. At this
checkpoint `futon3c` had nine dirty/untracked paths which the watcher could
observe but generally could not assign to a proven seat.

The separate sync monitor is genuinely absent on this host: neither
`futon-sync.timer` nor `futon-sync.service` is installed, despite mechanism #3
above saying the timer landed. Consequently the fetch/ahead/behind check is not
running here; `futon3c` was three commits ahead of `origin/master` at the
checkpoint. The automatic exact-seat Claude path is operating, while Codex,
direct Emacs, unattributed dirt, and the missing sync timer remain the concrete
gaps.

## Checkpoint — 2026-08-28

`futon3c` had nine dirty/untracked paths at the 2026-08-26 checkpoint. It has
**seventeen** now. The count nearly doubled in two days, and the reason is not
that the machinery stopped: it is that three of the paths are not dirt, and no
sweeper will ever clear them.

Counted across the three dirty repos, twenty-two items in **three** categories —
one more than the taxonomy above admits.

**Live run-products in tracked paths — three items, and no remedy above applies.**

    holes/labs/M-diagramprover/apm-driver/axiom-audit.jsonl         last written 73 minutes ago
    holes/labs/M-diagramprover/apm-driver/campaign-ledger.jsonl     git diff: +1 −0, a pure append
    holes/labs/M-diagramprover/apm-driver/statements-manifest.jsonl

The running machine appends to these. Committing them clears the tree until the
next frame writes a line. This is *generated noise* in the sense of the section
above, with one difference that changes the remedy completely: **those 2,012
files were untracked and a `.gitignore` pattern fixed them; these are tracked, and
an ignore rule does nothing to a tracked file.** They need `git rm --cached` and a
rule, or relocation out of the tree — and until then the goal *"no repo carries
uncommitted work older than a day"* is not merely unmet, it is unreachable, and
the watcher is correct to have nothing to promote.

This is the document's own general form once more, from the other side: the
remedy list enumerated two kinds of dirt and fails silently on the third kind in
front of it.

**Finished work sitting unstaged — thirteen items.** Eleven pattern-library
scribe outputs for frames f37–f49 (codex and zai seats), a hole-review
resolution-witness `.edn`, and two `.flexiarg` files in `futon3`. Cost #1 of this
document, verbatim, two weeks after it was written. These are frame outputs, and
frame outputs are already known to strand when a frame closes badly. The
attribution blind spot named at the 08-26 checkpoint is exactly why they sit
here: they are Codex and Zai seat products, and the exact-success witness
boundary does not reach either.

**Source edits and one stray — six items.** `apm-coordinator-enabled.py`,
`substrate/client.clj`, `live-map-pca3-v1.json`, `README-termux.md`,
`patterns-index.tsv`, and a file in `futon3` named `li` — almost certainly a
mistyped `ls`. A genuine inbox, clearable by a person in an afternoon.

### Why this matters beyond tidiness now

A dirty tree has no sha. On 2026-08-28 the serving `futon3c` JVM was measured to
be built from **nine repositories**, of which three had uncommitted trees and one
(`futon5`) was on a feature branch rather than a mainline. **The state that JVM is
running cannot be named**, so it cannot be pinned, reproduced, or compared with
another JVM's state — which is the prerequisite for running more than one.

That gives the third cost of this document — *a dirty tree is an unreliable
narrator* — a harder consequence than misleading an agent. It blocks the
heredity pin outright. `futon2` is clean in that measurement precisely because
its run-products live under `data/`, which is gitignored; `futon3c` is not,
because its equivalent products are tracked.

### Order of work, and it is not the obvious one

Move the run-products out of tracked paths **first**. Clearing the thirteen
unstaged items first would look like progress and be undone by the next frame,
which is the shape of every sweep in the *"why fifty sweeps did not hold"*
section. With the products gone, the remaining nineteen are a real inbox, the
count stops regenerating, and the pin becomes takeable.

Recorded by claude-13 at Joe's direction; the thirteen unstaged items are other
seats' outputs and are flagged rather than staged.

### Swept to zero the same day

All nine repositories on the serving JVM's classpath are clean as of 2026-08-28.
Nineteen items were cleared after the run-products were untracked; none of them
was contentious once looked at, which is this document's oldest observation.

| what | count | disposition |
|---|---|---|
| pattern-library scribe outputs, frames f37–f49 | 10 | committed — 576 lines of mined pattern content, declaring themselves inputs to `apm-ingest-coined-pattern-files.sh` |
| review-dispatch resolution-witness hole | 1 | committed — see below |
| finished source fixes | 2 | committed — brace-matched registry parsing; page size re-derived after the 115→971 edge repair |
| regenerated tracked resources | 2 | committed — the PCA live map (311→372 missions) and `patterns-index.tsv` |
| second-phone documentation | 1 | committed |
| unstaged library flexiargs | 2 | committed — 22 of 23 siblings were already tracked |
| a 1-byte file named `li` containing a tab | 1 | deleted |

**Cost #2 was paid again, in this sweep, by the agent doing the sweeping.**
`hole-review-dispatch-resolution-witness-v1.edn` was written by `claude-12` on
2026-08-25. It records that `ValidReviewDispatch`'s resolution witnesses —
`candidatePersisted`, `candidateFetchable` and four others — are self-reported
booleans, so the model proves only *"if the controller says everything resolves,
then the enumerated failures do not occur"*. On 2026-08-28 I re-derived exactly
that finding from scratch while mining the APM git log, wrote it up as the L0 gap
in `M-formal-war-machine` §3.1i, and found the earlier record only when sweeping
the tree that had been holding it for three days. It also named a closure I did
not have — content-addressed witnesses, as `TraceReviewSnapshot` already does
with `snapshotDigest`/`contentDigest`.

Three days, one morning's work, and the evidence was on disk the whole time. The
document's second cost is not hypothetical and does not spare the person reading
the document.

### One thing that is clean and still not coherent

`futon5` is checked out on `M-propagators-2026-07-15`, not a mainline branch,
while the serving JVM loads it. Nothing is dirty; the vector is nameable. But
*"one JVM per repo, running master"* is true of `futon3c` and was never checked
for the other eight, and this is the component where it is false. Left as it is —
switching a branch under a running JVM is not a sweep — and recorded so the pin,
when taken, pins a state someone chose rather than one nobody noticed.

## Checkpoint — 2026-08-31 — the facility is broken

**Joe's call, and the evidence supports it.** On 2026-08-31, taking over the
War Machine build after `claude-15`'s session was poisoned, `futon2` held **five
modified tracked files and ten untracked paths** — roughly 1,500 lines plus a
75-file transcript archive, written 08-29 and 08-30. It was cleared this morning
in eight packets (`d16056e`…`f39ef13`).

### What was actually broken, stated precisely

Not the 08-28 sweep. I nearly wrote that it was: the archive directory is named
`claude-13-repl-turns-2026-08-27`, and I began drafting a finding that a 74-file
directory predating the sweep had been missed. **Its files are dated 08-29.** The
directory name records when the *turns* happened, not when the archive was
written. The 08-28 claim — "all nine repositories clean" — stands unrefuted.

What is broken is that **the sweep is an event, not a facility.** It happens when
someone chooses to do it. Nothing notices when it stops. Three days after a sweep
that cleared nineteen items and paid cost #2 out loud, the same repository had
accumulated fifteen dirty paths and nobody was alerted — because the person who
swept was a session, and the session died.

### Cost #2, paid a third time, and now at repository scale

The 08-28 checkpoint records cost #2 as three days of a finding sitting on disk
while someone re-derived it. This morning's version is worse in kind:

**Eight committed records cited untracked files by exact line number.**
`R2-D1-findings.md:108` cites `R2-glossary-formalisation.md:69-84`;
`R8-D1-findings.md:21` cites `R8-glossary-formalisation.md:52`. Both citation and
target were real — on one machine. On any clone, eight committed records pointed
at nothing. The build's citation discipline had been rigorous for weeks *within
the checkout*, and broke at the one boundary nobody checked: the repository edge.

The build had even noticed the pattern and not generalised it. Uncommitted text
in `P-control-map-lint.md` reads: *"`P-R16.md` — cited by exact line in a
committed note — was untracked until this gate, the fourth unanchored artefact
found today."* Four instances, named, in a file that was itself untracked.

### Why this is the same disease as the rest of the day

The War Machine build spent 08-30 finding representations that cannot distinguish
"nothing happened" from "something happened whose value was nought", and finding
gates that pass without examining the thing they grade. Inbox-zero-as-an-event is
both: a clean tree and an unswept tree look identical from inside, and a sweep
that never runs reports nothing rather than reporting failure.

The same shape appeared twice more this morning: 80 contract declarations naming
one dead session as `holder`, and a roster whose `status` field says whether an
agent is mid-invoke but not whether anyone owns it.

### What would make it a facility

A check that fails, on a schedule, without anyone choosing to run it — and one
that reads the *citations*, not just `git status`: every `path:line` reference in
a committed record must resolve to a committed file. That is the check that would
have caught this on 08-29 instead of 08-31, and it is cheap. Not built here;
recorded as the shape of the fix, and as this document's own next item.

## Checkpoint 2026-09-12 — audit and repair (IMPROVEMENTS STILL REQUIRED)

Audited on 2026-09-12 while both Claude and Codex usage were exhausted. The
mechanism was in better shape than feared (wired hourly since 2026-09-01,
fetches before comparing, five clauses implemented), but it had become its own
warning realized in inverted form: **248 consecutive hourly failures into
journald with `OnFailure=` empty** — a permanently red lamp nobody watched.
Repairs made same day:

- **Wired a consumer.** New `scripts/inbox-zero-delta.bb` + systemd
  `futon-sync-delta.service`, fired by `OnSuccess=`/`OnFailure=` on the gate.
  It reports only status *changes* (new failure, recovery, signature change),
  logs them to `data/inbox-zero-delta.log`, and notifies via `notify-send`
  when present. Steady-state red is silent, which is what makes a change
  visible. Bring-up caught two real bugs worth keeping: JSON keys must stay
  strings when comparing against the gate's repo labels, and a `cond` whose
  first branch closed early makes the later "clauses" top-level forms that run
  unconditionally — paren balance alone does not catch that.
- **futon0**: pushed 15 unpushed commits (oldest 295 h); removed three stale
  `.bak` files.
- **futon5**: local `main` fast-forwarded 326 commits to `origin/main`.
- **futon3c**: committed 48 finished scribe pattern-library documents (29–95 h
  unstaged) and the durable-coordinator watchdog/compact-session fixes
  (clj-kondo 0/0, 33 tests / 237 assertions green); pushed.
- **Dead review worktrees**: 74 removed cleanly across futon2/futon3/futon3c/
  futon5a. 17 kept because they hold uncommitted modifications — they stay
  visible to the gate on purpose until reviewed.

### Still open

- **apm-lean: 272 dead frame worktrees.** Deliberately not bulk-removed:
  APM workspaces retire through the lease+audit path
  (`futon3c.apm.workspace-lifecycle/retire!`); raw `git worktree remove` would
  bypass the retirement-audit invariant. Needs a campaign retirement sweep.
- **voxterm**: no remote exists at all and no upstream (clause 4). Creating
  the GitHub repo is Joe's call; the three stale stray files (up to 631 h)
  are untouched.
- **p4ng**: 12 unpushed commits and `wm-status.pdf` — unexamined, left as is.
- **17 dirty review worktrees** kept above.
- The "facility" item from the previous section — citation resolution on a
  schedule — is still not built.
