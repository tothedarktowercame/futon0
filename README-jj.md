# README-jj — Jujutsu trial (2026-08-14 → 2026-08-21)

**Status:** running. Colocated on `futon3c` (laptop/Dionysus) since 2026-08-14.
One repo, one week, reversible. Nothing else is migrated.

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

## Success criteria (decide on 2026-08-21)

Baselines measured 2026-08-14 in `futon3c`.

| # | Criterion | Baseline | Target |
|---|---|---|---|
| 1 | New orphaned files — untracked, unignored, uncommitted >24h | 15 pre-existing | **0 new** |
| 2 | Agent friction incidents (blocked / confused / made a mess because of jj) | 1 on day 0 (see Gotcha 1) | ≤2 for the week, none unresolved |
| 3 | `jj undo` payoff — recoveries that would otherwise be manual patch surgery | benchmark: ~30 min on 2026-08-14 | ≥1 |
| 4 | Git tooling regressions (`scripts/check-reachable-*`, evidence-manifest verify, `backfill_turn_commit_mission_bestguess.py`, Agency git use) | 0 | **0** |
| 5 | Store overhead | `.jj` 660K vs `.git` 68M | stays proportionate |
| 6 | Promotion gate is simpler to write against jj than against git status-baseline diffing | n/a | qualitative, but state a verdict |

Criterion 6 is the one that decides whether this generalises: the whole point of
the exercise is routine commits onto master, and jj gives "what did this turn
touch" for free as the working-copy commit's diff.

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
jj, but this trial has not yet used them:

- `jj workspace add` (the per-agent-worktree replacement)
- First-class conflicts during an automated rebase onto a moving master
- `jj undo` / `jj op restore` on a real recovery — criterion 3 exists precisely
  to find out

## Log

- **2026-08-14** — jj 0.44.0 installed to `~/.local/bin` on Dionysus. `futon3c`
  colocated. Verified HEAD/branch/worktrees/`.jj`-exclusion unaffected. LaTeX
  build artifacts ignored + untracked (5 files). Surfaced 15 files of real
  uncommitted source and confirmed 154 catalogued evidence files were
  untracked-but-unignored, i.e. the out-of-git rule was enforced by discipline
  rather than by `.gitignore`. Two gotchas above recorded the same day.
