# Installing FUTON

This is the one place to start if you want to run FUTON on your own machine.
Every other futon repository's README points here.

## What you get

FUTON gives AI coding agents a memory that has been evaluated. Agents record
what they did and why as searchable evidence; the next session reads it and
carries on. Working rules live as design patterns, each with records of when it
was chosen and how it turned out, so the rules get revised from evidence rather
than from whoever last edited them.

The part this guide installs is the **shared core**: a durable, searchable
evidence store (futon1b) and the agent coordination server, Agency (futon3c).

## Status — read this first

**Verified 2026-09-26** on a fresh Ubuntu 24.04 machine (x86-64, 16 GiB RAM,
OpenJDK 21) from the public `main`/`master` branches, as recorded in
[`holes/missions/M-joe-told-me-about-futon.md`](holes/missions/M-joe-told-me-about-futon.md)
(Checkpoint 2). What passed:

- dependencies resolve; the store and Agency boot;
- evidence written through Agency is stored, read back, searched, and followed
  along reply chains;
- everything survives stopping and restarting both services.

**Not yet verified:** an agent carrying out a task through Agency. That needs
your own agent CLI (Claude Code or Codex), logged in as a normal user. If you
try it, please report what happened.

**Known rough edges** (harmless, but you will see them):

- A background file watcher watches `/home/joe/code/*` whatever your setup, and
  logs a stream of `git error` / `ConnectException` lines. Boot continues.
- One boot step reports `inventory-snapshot emit FAILED` for a file under
  `/home/joe/code`. Boot continues.
- There is no licence file in the futon repositories yet. Ask before
  redistributing.

## 1. Prerequisites

| Need | Version used | Notes |
|---|---|---|
| Linux | Ubuntu 24.04 | Other distributions untested |
| Git, make, curl | any recent | |
| Java | **OpenJDK 21** | `sudo apt install openjdk-21-jdk-headless` |
| Clojure CLI | 1.12.x | [Official Linux install](https://clojure.org/guides/install_clojure) |
| RAM | ~6.5 GiB free | Two JVMs; see [README-install-profiles.md](README-install-profiles.md) |
| Disk | ~4 GiB | Source ~3.4 GiB, dependency cache ~0.2 GiB |
| Agent CLI | optional | Claude Code or Codex, for agent tasks |

Emacs is **not** needed to run the core. It is the intended day-to-day
interface (Arxana, the REPL buffers), so you will want it later.

Optional check before you start:

```bash
python3 scripts/install-plan.py doctor --profile compact-8g
```

It prints a JSON report on memory, ports, paths and tools. It exits 2 while
the project's own release gates are open; that is expected. Read the `checks`
list for anything marked `"ok": false` on your machine.

## 2. Get the code

The repositories expect to sit side by side in `~/code`:

```bash
mkdir -p ~/code && cd ~/code
for r in futon0 futon1 futon1b futon2 futon3 futon3a futon3b futon3c futon4 futon5; do
  git clone --depth 1 --recurse-submodules https://github.com/tothedarktowercame/$r.git
done
```

These ten are what Agency needs on its classpath. The others (futon1a, futon6)
are not required for the core.

## 3. Start the evidence store (futon1b)

In one terminal:

```bash
cd ~/code/futon1b
MALLOC_ARENA_MAX=2 clojure -M:node -m futon1b-server \
  --store-dir ~/futon-data/store --port 7074
```

The first run downloads dependencies (a few minutes). It is ready when it
prints `futon1b-server up on *:7074`. Check with
`curl http://127.0.0.1:7074/health`.

Use port **7074**: that is where Agency looks by default. (futon1b's own README
shows 7073; either works if you also set `FUTON1B_URL`.)

## 4. Start Agency (futon3c)

In a second terminal:

```bash
cd ~/code/futon3c
FUTON3C_EVIDENCE_BACKEND=futon1b \
FUTON3C_ROLE=laptop \
CLAUDE_PERMISSION=default \
make dev
```

What the three settings do:

- `FUTON3C_EVIDENCE_BACKEND=futon1b` — store evidence durably in futon1b.
  Without it, Agency still starts but keeps evidence in memory only, and says
  so loudly: `I-evidence-per-turn BOOT CHECK FAILED`.
- `FUTON3C_ROLE=laptop` — local-only defaults: no IRC server, bind to
  localhost.
- `CLAUDE_PERMISSION=default` — **important.** Without it, agents launched by
  Agency run Claude Code with `bypassPermissions`: no confirmation before they
  edit files or run commands. Set it unless you have decided otherwise.

Also note: `make dev` sets Codex to `sandbox=danger-full-access
approval=never` by default. Override `CODEX_SANDBOX` and `CODEX_APPROVAL` if
you use Codex. `make dev` looks for Claude Code at `~/.local/bin/claude`; set
`CLAUDE_BIN` if yours is elsewhere.

It is ready when `curl http://localhost:7070/health` returns `"status":"ok"`
and the log shows `I-evidence-per-turn boot check: OK (futon1b)`.

## 5. First run: write and read evidence

```bash
curl -X POST http://localhost:7070/api/alpha/evidence \
  -H 'Content-Type: application/json' \
  -d '{"type":"reflection","claim-type":"observation","author":"me",
       "subject":{"ref/type":"session","ref/id":"first-run"},
       "body":{"text":"hello from a fresh install"},"tags":["first-run"]}'

curl "http://localhost:7070/api/alpha/evidence?author=me"
```

The first returns `{"ok":true,"evidence/id":...}`; the second returns your
entry. Stop both services (Ctrl-C), start them again, and run the second
command: the entry is still there.

If a write is rejected with `invalid-entry`, check that `subject` is present
and that `type` is one of `coordination`, `gate-traversal`,
`pattern-selection`, `pattern-outcome`, `reflection`, `forum-post`,
`mode-transition`, `presence-event`, `correction`, `conjecture`, `arse-qa`,
`memory` (defined in `futon3c/src/futon3c/social/shapes.clj`). The error
message shows the rejected entry but not which field failed.

## 6. First agent task (not yet verified — please report back)

The `laptop` role does not register the local Claude agent by default. Start
Agency with it registered by adding one setting to step 4:

```bash
FUTON3C_EVIDENCE_BACKEND=futon1b FUTON3C_ROLE=laptop \
CLAUDE_PERMISSION=default FUTON3C_REGISTER_CLAUDE=true make dev
```

The log should show `Claude agent registered: claude-1`. Then, with Claude
Code installed and logged in as your normal user (not root):

```bash
curl --max-time 300 -X POST http://localhost:7070/api/alpha/invoke \
  -H 'Content-Type: application/json' \
  -d '{"agent-id":"claude-1","prompt":"How many .md files are in ~/code/futon0/holes/missions? Reply with just the number."}'
```

Then look for the agent's turn in the evidence store:
`curl "http://localhost:7070/api/alpha/evidence?limit=5"`.

## Where to go next

- **Why it is built this way:** [`futon3c/README.md`](https://github.com/tothedarktowercame/futon3c)
  (evidence landscape, reflection API) and
  [`futon4/holes/mission-lifecycle.md`](https://github.com/tothedarktowercame/futon4/blob/main/holes/mission-lifecycle.md)
  (how work is organised as missions).
- **The stack, repo by repo:** [README.md](README.md).
- **Sizing for smaller or larger machines:**
  [README-install-profiles.md](README-install-profiles.md).
- **The fuller installer work in progress:**
  [README-public-install.md](README-public-install.md),
  [README-apollo-trial.md](README-apollo-trial.md). Note that
  `config/public-install-candidate.json` pins commits from 2026-09-09 that no
  longer reproduce; use the branch heads as above until it is re-cut.
