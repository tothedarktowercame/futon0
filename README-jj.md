# README-jj — Jujutsu trial (rescoped 2026-08-14; runs 2026-08-15 → 2026-08-22)

**Status:** running, on a **second** scope. The first scope was wrong and is
recorded below as a false start rather than quietly rewritten.

**What was wrong with it.** It colocated jj on `futon3c` and otherwise carried
on as before. That tests whether jj *interferes*, not whether jj *helps* — and
`futon3c` is the worst venue available for either question: eight local agents
are rooted at `/home/joe/code` and every one of them uses git, and `origin`
moved twice in the twenty minutes it took to push six commits. Three of the four
friction incidents on day zero came from **two writers on one repo**, not from
jj. The features that are the actual reason to want jj — workspaces, undo on a
real recovery, conflict-tolerant rebase — went unexercised.

## THE RULE THAT WAS MISSING

**One tool per repo. Write with git, or write with jj, never both.**

This is the whole lesson of day zero, promoted from footnote to rule. Colocated
mode means git *can* still read and write — it does not mean you should
interleave them. Mixing produced a conflicted `master` bookmark and a rebase
that silently reverted a file on disk by moving the working copy off the commit
that held the edit.

## Two tracks

### Track A — `futon3c`: jj as a passive safety net (read-only)

Permitted: `jj log`, `jj status`, `jj diff`, `jj op log`.
**Forbidden: every jj write** — no `rebase`, no `bookmark set`, no `git push`,
no `describe`. Agents keep using git; so do I.

This still works, because **jj auto-snapshots on any command, including reads**.
The safety net — uncommitted work acquires a change-id and stops being
invisible — is retained at near-zero interference risk. That is the single
property most worth testing, and it is testable without writing anything.

### Track B — `futon0`: jj as the sole VCS

Single writer, low cadence (one doc at a time, all Joe), no agent traffic. It
already contains a specimen of the problem this trial exists for: three dirty
files, two of them untracked READMEs that nobody has committed.

Here jj is the **only** VCS interface, and the untested features get used
deliberately: `jj undo` on a real recovery, `jj workspace add`, and describing
work in progress as it happens rather than at the end.

## Why

Work sits uncommitted for days, and everything downstream pays for it. Measured
on 2026-08-14 in `futon3c`:

- A four-day-old uncommitted slice in `registry.clj` / `federation.clj` blocked
  two dispatched packets. One agent correctly refused its packet because the
  function it was told to amend existed only in the working tree, so there was
  no committed base to commit against.
- A commit landed on `master` that did not compile — the caller was committed
  while the callee was left uncommitted (`No such var:
  reg/publish-agents-status-async!`). Recovering it took a saved patch, a
  `reset --mixed`, a reverse-apply, two re-commits and a hand-rolled backup.
- Colocating jj immediately surfaced **15 files of real uncommitted source** in
  `futon3c` that had been invisible: three test namespaces (which pass —
  28 tests, 73 assertions, 0 failures), two excursion docs, a tech note, and
  four `scripts/systemd/` files. One of them,
  `test/futon3c/agency/invoke_activity_test.clj`, documents the 2026-08-03
  finding that *activity age is the liveness signal* — the exact insight
  re-derived from scratch eleven days later because the evidence of it was
  never committed.

The design we were about to build — per-turn snapshots, per-agent worktrees,
cheap undo — is Jujutsu's core model. Better to try the real thing than to
reinvent a worse one.

## What jj changes

- **The working copy is a commit.** Auto-snapshotted on every `jj` command.
  There is no uncommitted state, so the failure above cannot take that shape:
  a four-day-old slice would have had a change-id and a place in `jj log` from
  the first minute.
- **Operation log.** `jj op log` records every operation with its exact
  arguments; `jj undo` / `jj op restore` reverse them. Whole-repo undo.
- **Change-ids are stable across rewrites.** Git shas are not — a `turn→commit`
  dataset keyed on sha rots every time history is rebased, and we rebased twice
  in one morning. This matters for the §8.1 `turn→code` corpus in
  `C-substrate-completion`.
- **Colocated:** `.git` stays valid, so every existing git tool, script and
  agent keeps working.

## Success criteria (decide on 2026-08-22)

Baselines measured 2026-08-14. Split by track, because the tracks test different
claims and a single table conflated them last time.

### Track A — does the safety net work?

| # | Criterion | Baseline | Target |
|---|---|---|---|
| A1 | New orphaned files in `futon3c` — untracked, unignored, uncommitted >24h | 15 pre-existing | **0 new** |
| A2 | The net actually catches something — work made visible that git would have hidden | 15 found on day 0, incl. `invoke_activity_test.clj` | ≥1 more, or state that day 0 was the whole yield |
| A3 | jj-caused incidents | should be **0 by construction** — no writes are permitted | any at all is a finding worth reporting |
| A4 | Git tooling regressions (`check-reachable-*`, evidence-manifest verify, `backfill_turn_commit_mission_bestguess.py`, Agency git use) | 0 | **0** |
| A5 | Store overhead | `.jj` 660K vs `.git` 68M | stays proportionate |

### Track B — is jj usable as the primary VCS?

| # | Criterion | Target |
|---|---|---|
| B1 | A week of `futon0` work done jj-only, end to end, without falling back to git | no fallbacks; log any |
| B2 | `jj undo` / `jj op restore` on a **real** recovery | ≥1, timed against the ~30-min git equivalent on 2026-08-14 |
| B3 | `jj workspace add` exercised for per-agent isolation | at least a dry run, with a verdict on whether it beats git worktrees |
| B4 | A conflict handled without halting mid-operation | ≥1, or state that none arose |
| B5 | Promotion gate simpler to write against jj than against git status-baseline diffing | qualitative, but **state a verdict** |

B5 is still the criterion that decides whether this generalises: the point of the
whole exercise is routine commits onto master, and jj gives "what did this turn
touch" for free as the working-copy commit's diff.

### Friction counter — reset, with one carried forward

Day-zero incidents #2–#4 (detached git HEAD, conflicted `master` bookmark,
rebase reverting a file on disk) were **design errors of the trial**, not jj
defects: all three came from interleaving git and jj writes, which the rule
above now forbids. They are struck from the count.

**Carried forward: incident #1**, the `git commit -a` intent-add hazard. That is
a genuine consequence of colocation, it is live on `futon3c` for every agent,
and it stands (Gotcha 1).

Its blast radius shrinks by 154 files once the catalogued evidence corpus moves
under `data/` — a packet already authorised for other reasons, which now also
serves this trial.

### Abort early if

- Any agent commits the evidence corpus, or other untracked bulk, into git.
- Git tooling breaks in a way that is not a one-line fix.
- Two or more agents are blocked by jj on the same day.
- `.jj` grows disproportionately or operations become slow.

Aborting is `rm -rf .jj` and costs nothing else.

## Reproduce on another repo

### 0. Pre-flight — know your exposure first

**Do this before colocating.** jj makes every untracked-and-unignored file
visible and marks it intent-to-add in git's index. Count the files, not the
status entries (git collapses untracked directories):

```bash
cd <repo>
git status --porcelain -uall | grep -c '^??'
git status --porcelain -uall | grep '^??' | sed 's/^?? //' | head -30
```

Measured 2026-08-14: laptop `futon3c` 174, zone `futon3c` **251** (of which 154
are the catalogued evidence corpus in
`holes/labs/evidence-manifest-20260801.tsv`, deliberately kept out of git).

If most of that is deliberate raw data, add ignore rules for it **first** — it
is far less annoying than untangling it afterwards (Gotcha 2).

### 1. Install

Static musl binary, user-level, no sudo:

```bash
V=0.44.0
curl -sL -o /tmp/jj.tar.gz \
  https://github.com/jj-vcs/jj/releases/download/v$V/jj-v$V-x86_64-unknown-linux-musl.tar.gz
mkdir -p ~/.local/bin && tar xzf /tmp/jj.tar.gz -C /tmp ./jj
install -m 755 /tmp/jj ~/.local/bin/jj
jj --version    # ~/.local/bin must be on PATH
```

### 2. Colocate

```bash
cd <repo>
jj git init --colocate
jj config set --repo user.name  "Joseph Corneli"
jj config set --repo user.email "jcorneli@brookes.ac.uk"
jj describe -m "wip: untriaged working-copy files at jj colocation ($(date +%F))"
```

Set the user config immediately. jj does **not** inherit `git config
user.name/user.email`, and the working-copy commit it creates at init is
authored ` <>`. Setting the config only affects future commits — fix the
existing working copy with `jj metaedit --update-author`.

`jj git init` prints a long `jj bookmark track` hint listing every remote
branch. Ignore it unless you actually want local bookmarks tracking those.

### 3. Verify nothing git-side moved

```bash
git rev-parse HEAD            # unchanged
git symbolic-ref -q HEAD      # still refs/heads/<branch>, NOT detached
git status -sb | head -1      # same ahead/behind
git worktree list | wc -l     # unchanged
git ls-files --cached | grep -c '^\.jj/'   # must be 0
jj log --limit 5
```

All five held on laptop `futon3c`: HEAD stayed `refs/heads/master`, ahead-count
unchanged, all four worktrees intact, `.jj` invisible to git.

### 4. Roll back

```bash
rm -rf .jj
```

The working tree and `.git` are untouched. Files that were `jj file untrack`ed
stay on disk; the `.gitignore` edits are ordinary commits and stay unless you
revert them.

## Gotchas — both cost real time on day 0

### 1. `git commit -a` becomes dangerous

jj marks untracked files intent-to-add in git's index, and `git commit -a`
stages tracked **and** intent-to-add paths. Before colocation `-a` would have
committed 10 modified files; after, it would have committed 184 — including the
whole 5.6 MB evidence corpus.

```bash
git commit -a --dry-run --short | wc -l    # check before you ever run it
```

Plain `git commit` with no arguments is still safe (`git diff --cached` is
empty — the intent records carry no content).

**Rule for this repo, and for every packet dispatched into it: never
`git commit -a`. Stage explicit paths and check `git diff --cached` first.**

### 2. `.gitignore` alone will not suppress an already-exposed file

Ignore rules do not apply to anything already in the index. Once jj has
intent-added a path, adding it to `.gitignore` does nothing visible. The
sequence is:

```bash
# 1. add the rule to .gitignore
# 2. then untrack, or the rule has no effect
jj file untrack <paths>...
git check-ignore -q <path> && echo IGNORED
```

Verified on the LaTeX build artifacts: index went 2047 → 2042, files stayed on
disk, and they became properly ignored only after the untrack.

## Rolling out to other repos

There are 13 candidates (`futon0` … `futon7`, `futon1a/1b`, `futon3a/3b`,
`futon5a`). **Do not migrate them on a hunch** — that was the reasoning for a
one-repo trial. Order of adoption if the week succeeds:

1. Repos with the largest untracked-file exposure benefit most, but hurt most on
   day one. Run the pre-flight and clear the deliberate raw data into `data/`
   (ignored as standard, at any depth — `.gitignore:34`) before colocating.
2. Repos that agents write to concurrently need the packet rule from Gotcha 1
   in their `AGENTS.md` before any agent touches them.
3. Anything with git worktrees: verify step 3 above, since jj colocates the main
   working copy only.

## Not yet exercised

Stated plainly so nobody quotes them as tested. These are the reasons to want
jj, and the first scope did not touch any of them — which is precisely why it
was not a fair try:

- `jj workspace add` (the per-agent-worktree replacement) — now **B3**
- First-class conflicts during an automated rebase onto a moving master — **B4**
- `jj undo` / `jj op restore` on a real recovery — now **B2**

Track B exists to convert these from reasons-to-want into evidence. If the week
ends with all three still unexercised, the honest verdict is *not proven*, not
*passed*.

## Log

- **2026-08-14 (day 0, first scope)** — jj 0.44.0 installed to `~/.local/bin` on
  Dionysus. `futon3c` colocated. Verified HEAD/branch/worktrees/`.jj`-exclusion
  unaffected. LaTeX build artifacts ignored + untracked (5 files). Surfaced 15
  files of real uncommitted source, and confirmed the 154 catalogued evidence
  files were untracked-but-unignored — i.e. the out-of-git rule was enforced by
  discipline rather than by `.gitignore`. Both gotchas recorded the same day.

  One genuine win, worth keeping in view when weighing the friction: the
  surfaced `test/futon3c/agency/invoke_activity_test.clj` documents the
  2026-08-03 finding that *activity age is the liveness signal* — the exact
  insight re-derived from scratch eleven days later, at cost, because the
  evidence of it had never been committed. That is the failure this trial
  exists to prevent, caught within an hour of colocating.

  One genuine demonstration: rebasing six commits onto a 108-commit-newer
  `origin/master` changed every git sha (`b5785805`→`f3a8389c`, …) while the
  change-ids (`krqsqpnn`, `lnuussnn`, `osyzrkvt`) survived untouched. A
  sha-keyed `turn→commit` join would have rotted; a change-id-keyed one did not.

  Four friction incidents, of which three were self-inflicted by mixing git and
  jj writes. Scope judged unfair to jj and rewritten the same day.

- **2026-08-14 (rescope)** — split into Track A (`futon3c`, read-only safety
  net) and Track B (`futon0`, jj-only). "One tool per repo" promoted to a rule.
  Clock restarted 2026-08-15 → 2026-08-22. Friction counter reset except
  incident #1.
