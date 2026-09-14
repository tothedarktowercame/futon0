# Emacs on the phone: the "crash" on every rotation was never Emacs

Worked out on 2026-08-30, after Emacs appeared to die twice in one morning on
orientation changes. Items marked **[verified]** were actually observed on Zone or
on the phone; everything else is inference from the evidence and is flagged as such.

Context: Zone is the machine, a Samsung phone running DeX and Termux is the client
(`README-termux.md`). Emacs runs *on Zone* as `emacs -nw`, viewed through mosh. There
is no Emacs on the phone at all — **[verified]**, `command -v emacs` in Termux returns
nothing.

---

## 1. The rule that explains the whole failure

**Nothing may run as a bare child of `mosh-server`. Ever.**

`README-termux.md` §1 already said this ("mosh survives the network, tmux survives
mosh") and gave the command. It was not being followed in practice: shell history on
the phone shows plain `mosh joe@104.243.39.24`, and every one of the 23 live
`mosh-server` processes on Zone had a bare `-bash` as its only child — no tmux
anywhere. **[verified]**

This document is the *why it actually bites*, which turned out to be sharper than
"the network might drop".

---

## 2. Emacs was not crashing

Everything that a crashing Emacs would leave behind was absent — **[verified]** all
four:

- no files in `/var/lib/systemd/coredump/`, nothing from `coredumpctl`
- no `emacs` segfault in `dmesg` (the only segfaults were `latexml_oxide`, unrelated)
- no `emacs_backtrace*` anywhere under `$HOME`
- no unclean-exit markers in any profile's `auto-save-list/` — Emacs deletes its
  `.saves-PID-host` file on a clean exit and leaves it on a crash. There were none.

The decisive one: a `emacs -nw --with-profile graph` started **Aug 25 was still
running** on 2026-08-30, parked under a `mosh-server` with no client attached.
**[verified]** A process that crashed five days ago is not still in `ps`.

> **Correction, 2026-09-07.** Two of those four bullets do not survive re-checking,
> and Emacs *does* die on its own. The stranding in this section was real; the
> conclusion "Emacs was not crashing" was over-drawn from it. See §7.2 and §7.3.

So the buffers were never lost to a crash. They were *stranded* — alive, running,
and unreachable.

---

## 3. What actually crashes: mosh-client

Reproduced from the phone. **[verified]**

```
/home/builder/.termux-build/mosh/src/src/terminal/terminalframebuffer.cc:83:
  Terminal::Framebuffer::Framebuffer(int, int): assertion "s_height > 0" failed
Aborted
```

mosh-client aborts outright when the terminal reports a height of zero. mosh 1.4.0
as packaged for Termux; the assertion is upstream and unpatched.

**Inferred, not observed:** that an orientation change or a DeX display switch is
what momentarily reports zero rows. It fits — the symptom is specific to rotation,
the timing matches, and a display reconfiguration is exactly when Android hands an
app a transient degenerate size — but the reproduction above came from a
deliberately zero-height pty in a test harness, *not* from physically rotating the
phone. Worth confirming by rotating with `mosh-client` under `strace` if it ever
matters enough.

---

## 4. Why the abort was unrecoverable

**mosh cannot reattach a session.** This is the part that turns a client crash into
apparent data loss.

When mosh-client aborts, `mosh-server` keeps running on Zone, and so does everything
under it. There is no `mosh -r`, no reattach verb, nothing. The session is gone from
your side permanently, while the processes stay alive and invisible on the server.

The accumulated cost of this, on the day it was looked at: **23 `mosh-server`
processes against exactly 1 live `mosh-client`** — so 22 abandoned sessions, the
oldest from Aug 12 — one of them holding the stranded Emacs from §2. **[verified]**

Two fresh `mosh-server` processes had started that morning, 09:18 and 09:58, which
lines up with "it crashed twice today". Circumstantial, not proof.

---

## 5. The fix

tmux absorbs the abort. mosh-client dying then costs a reconnect and nothing else.

### `~/bin/tm` on Zone (new)

The grouped-view logic moved out of `.bashrc` into a real script, because **a shell
function cannot be the remote command of a mosh invocation** and that is precisely
where it needs to run. The `.bashrc` function now delegates:

```bash
tm() { "$HOME/bin/tm" "$@"; }
```

so there is one copy of the logic, not two that drift. The script:

```bash
view="${1:-main}"
tmux has-session -t=main 2>/dev/null || tmux new-session -d -s main
if tmux has-session -t="$view" 2>/dev/null; then
    exec tmux attach-session -t "$view"
else
    exec tmux new-session -s "$view" -t main
fi
```

`main` owns the windows; every other name is a grouped session sharing those same
windows with its own current-window and its own size — so the phone screen and the
DeX monitor can each sit on a different window at full resolution. The deliberate
"NOT auto-attach" decision in `.bashrc` was left intact: `tm` is still called by hand.

### `~/.bashrc` on the phone (new)

```bash
ZONE_HOST=zone   # ssh alias from ~/.ssh/config
zone()      { mosh "$ZONE_HOST" -- /home/joe/bin/tm "${1:-dex}"; }
zone-bare() { mosh "$ZONE_HOST"; }
```

`zone` for the DeX view, `zone phone` for the phone-screen view, `zone-bare` as the
escape hatch for when tmux itself is the problem. Using the `zone` ssh alias rather
than the hardcoded IP, so a change of address is a one-line edit in `~/.ssh/config`.

Both `.bashrc` files were backed up first: `.bashrc.bak-tm-20260830` on Zone,
`.bashrc.bak-zone-20260830` on the phone.

### Verified end to end

- `zone` from the phone → mosh → `/home/joe/bin/tm dex` → tmux `main` plus grouped
  `dex`, client at 120x40. **[verified]**
- SIGKILL the attached client (what an abort looks like): both sessions survive with
  zero clients, and pane scrollback is intact. **[verified]**
- Reattach afterwards reaches the *same* session, marker text still on screen, no
  duplicate session created. **[verified]**
- `destroy-unattached off`, and a detached session with no client persists on its
  own. **[verified]**

One trap found while testing, worth knowing: mosh-client also aborts with
`Terminfo database could not be found` if `TERM` is unset, which is easy to hit from
a non-interactive harness and looks like a different bug than it is. **[verified]**

---

## 6. Left alone deliberately

Both are destructive and neither was done:

1. **The stranded Emacs** (pid 3694405 on 2026-08-30, `graph` profile, running since
   Aug 25) may hold unsaved buffers. Its `server` socket at
   `/run/user/1000/emacs/server` was clobbered by a later Emacs, so `emacsclient`
   cannot reach it — **[verified]**, the socket's mtime is that of the newer process.
   `reptyr` could pull it onto a live terminal, but it is not installed and
   `kernel.yama.ptrace_scope` is `1`, so it would need `sudo` or a sysctl change.
2. **The 23 abandoned `mosh-server` processes.** Harmless but untidy, and they make
   `ps` hard to read. Reaping them kills whatever is parked under them, so it wants a
   look at each one's children first.

The general lesson for §1: because `(server-start)` runs in the `graph` profile's
`init.el`, a *second* Emacs silently takes over the socket name and orphans the
first one's door. If long-lived Emacs sessions matter, an Emacs daemon under a
systemd user unit — reached with `emacsclient -t` inside tmux — removes this whole
class of problem, in the same way `README-termux.md` §1 concluded that a standing
tmux session wants a unit rather than trust.

---

## 7. The recurrence, 2026-09-07 — this one really was Emacs

Emacs died again at ~15:17 today. It does not fit §3 at all, and re-running §2's
checks shows two of them never proved what they were read as proving.

### 7.1 The §5 fix is not actually in use

**[verified]**, all three:

- `tmux ls` → `no server running on /tmp/tmux-1000/default`. There is no tmux on
  Zone at all.
- Both live `mosh-server` processes (425567 from Sep 3, 1873476 from Sep 6) have a
  bare `-bash` as their only child, exactly as on Aug 30.
- The Emacs running now is `782310 → 1873477 (-bash) → 1873476 (mosh-server)`. A
  bare grandchild of `mosh-server`, which is precisely what §1 forbids.

`~/bin/tm` exists and is correct; `.bashrc` still delegates to it. It is simply not
being called. So §1 is still an open finding, not a closed one — the count of
abandoned sessions came down from 23 to 2, but the habit did not change.

### 7.2 This death was not a mosh-client abort

The dead Emacs was **426252**, a child of the `-bash` under `mosh-server` 425567
(Sep 3 20:54). That `-bash`, 425568, **is still alive** — **[verified]**, and so is
its `mosh-server`.

That is the whole distinction. A mosh-client abort (§3/§4) leaves the entire
server-side tree running and merely unreachable; nothing under it dies. Here the
parent shell and the mosh-server both survived and *only Emacs* went. Whatever
happened, it happened to the Emacs process specifically.

### 7.3 Two of §2's four bullets were not evidence

- **Coredumps.** `ulimit -c` is `0` and `kernel.core_pattern` is the literal `core`
  — **[verified]**. Nothing was ever going to appear in
  `/var/lib/systemd/coredump/`, crashing or not. That bullet had no information in
  it either way.
- **Unclean-exit markers.** There are seven in `~/.emacs.d/auto-save-list/`, and
  **every one of their pids is dead** — **[verified]**:

  ```
  2026-08-26 07:01  .saves-2330092-reliablesite~
  2026-08-26 08:57  .saves-2683496-reliablesite~
  2026-08-28 15:11  .saves-3694405-zone~          <- the "stranded" Emacs of §6
  2026-08-30 09:38  .saves-1591180-zone~          <- the morning §2 says had none
  2026-08-30 09:56  .saves-1060028-zone~          <- likewise
  2026-09-03 17:41  .saves-1089042-zone~
  2026-09-07 15:17  .saves-426252-zone~           <- today
  ```

  Two traps in reading these, both of which §2 fell into. The mtime is the *last
  auto-save*, not the moment of death — 3694405's marker says Aug 28 while the
  process was demonstrably alive on Aug 30. And a marker for a *running* pid is
  normal, so the file must be joined against `ps` before it means anything. Done
  properly, it says seven unclean exits in thirteen days.

- **What does still hold.** The kernel logs segfaults independently of `ulimit`,
  and there is no `emacs` segfault in `dmesg` — only the unrelated `latexml_oxide`
  ones. No OOM kill, `systemd-oomd` is `inactive`, and the box had 233 Gi
  available. **[verified]** So: not a segfault, not memory pressure.

**Inferred, not observed:** it was a `SIGKILL`, or something else that bypasses
Emacs's own fatal-signal handler. The reasoning: there is no `emacs_backtrace*`
anywhere under `$HOME` or `/tmp` and the process's cwd was the writable
`/home/joe`, so the handler never ran; and `SIGTERM`/`SIGHUP` are poor candidates
because Emacs handles those through `kill-emacs`, which would have deleted the
marker file. Nothing in `~/bin` kills Emacs. The sender is genuinely unidentified.

### 7.4 The blast radius is bigger than it was in August

Emacs is now the agent bus. `~/bin/cr`, `~/bin/cz` and `~/bin/agent-recall` all
reach it through `/run/user/1000/emacs/server`, and so does the `dev-zone-env`
invoke path. Sixty seconds after the death, in the journal — **[verified]**:

```
15:18:24 dev-zone-env: [invoke-delivery] failed for f188-solver
  output=emacsclient: can't connect to /run/user/1000/emacs/server: Connection refused
```

An agent invocation was lost, not just a buffer. And the replacement Emacs started
at 15:21 took the socket name over again (socket mtime is 15:21, **[verified]**) —
§6's orphaning trap firing once more, three weeks after it was written down.

### 7.5 What was done, 2026-09-07

All three items below were built and verified the same afternoon. The one thing
deliberately **not** done is the cutover itself — see the end of this section.

**a. `~/bin/emacs-graph`, a launcher that leaves evidence.** Replaces
`alias emacs-graph='emacs -nw --with-profile graph'`; the alias is now a comment
in `.bashrc`, because an alias would shadow the script on `PATH`. It sets
`ulimit -c unlimited`, keeps cwd at `$HOME` (where `core` and
`emacs_backtrace.txt` both land, given `kernel.core_pattern` is the bare string
`core`), and appends stderr plus a start/exit/status line to
`~/.emacs-graph/logs/emacs-<timestamp>.log`. Display is unaffected: `-nw` writes
to the tty through stdout, only stderr is redirected. **[verified]** end to end
on a pty — a deliberate `(kill-emacs 7)` was recorded as `status=7`.

The first test run demonstrated the point by accident. It failed, and the log
said why: `Please set the environment variable TERM; see 'tset'.` — §5's last
trap, caught in writing instead of guessed at. **[verified]**

The Emacs pid is not knowable before exec, so join a log to an auto-save marker
by timestamp rather than expecting the pid in both.

**b. A warning when a shell is bare under `mosh-server`.** In `.bashrc`, printed
when `$TMUX` is empty and `$PPID`'s comm is `mosh-server`. Deliberately a
warning and not an auto-attach — §5's "call it by hand" decision stands; this
only makes forgetting visible rather than silent, which is what §7.1 showed was
needed. The phone-side half of §5 could not be touched from Zone.

**c. `~/.config/systemd/user/emacs-graph.service`**, `enabled`, currently
stopped. `Type=notify` (the 32.0.50 build links `libsystemd`, **[verified]**
with `ldd`), `ExecStart=emacs --fg-daemon --with-profile graph`,
`LimitCORE=infinity`, `LANG`/`LC_ALL` pinned to `en_GB.UTF-8` to match the
interactive shell. Two decisions worth keeping:

- **No daemon *name*.** `~/.emacs-graph/init.el:3-5` runs `(server-start)` during
  init, which creates the socket called `server`. `--fg-daemon=NAME` would add a
  *second* socket under a different name and leave two doors into one process.
  Unnamed, init.el and the daemon agree on `server`, which is the name every
  client already uses.
- **`Restart=on-abnormal`, not `on-failure`.** It restarts on a signal death, a
  timeout or a watchdog trip, and not on a plain non-zero exit. That is the
  split this box wants: the 2026-09-07 death looks like SIGKILL (§7.3) and gets
  restarted, while a broken `init.el` or the guard below are *decisions* and
  must not be retried. `on-failure` was tried first and retried the guard five
  times; `RestartPreventExitStatus=` does not help, as it is not consulted for
  an `ExecStartPre` exit. **[verified]** both ways.

An `ExecStartPre` single-owner guard, in the spirit of futon1b's `store-guard`,
refuses to start (exit 78, `EX_CONFIG`) when another Emacs already answers on
`/run/user/1000/emacs/server`. Without it, starting the unit next to a
hand-started Emacs would clobber that session's door — the §6 failure this unit
exists to end. **[verified]**: attempted against the live Emacs, the unit
refused once, `ExecStart` never ran, and the socket's mtime never moved.

**d. `~/bin/eg`, one command for the whole stack.** Knowing the right nesting
is not the same as getting it every time by hand, and §7.1 is the evidence that
by hand loses. `eg` builds `mosh -> tmux -> emacsclient -t -> daemon` in order:

```
eg           the DeX view          eg phone    the phone-screen view
eg main      the ungrouped session
```

It asks the *socket* whether an Emacs is answering rather than asking systemd,
because a hand-started Emacs is a perfectly good server to attach to and the
service would refuse to start beside it anyway. Only when nothing answers does
it start the daemon — which also covers a stale socket file, since Emacs's own
`server-start` clears a dead one on the way up. Then it ensures `main` exists,
ensures a single window named `emacs` running `emacsclient -t`, points the
requested view at that window, and attaches. Being a script rather than an
alias, it is usable as a remote command, so from the phone the whole stack is:

```
mosh zone -- /home/joe/bin/eg dex
```

**[verified]** on a scratch tmux socket (`-L probe`) with stubbed binaries, so
neither the live Emacs nor the real tmux server was involved: builds everything
from nothing and lands on the Emacs window; run again after moving the view
away, it re-selects Emacs without creating a second window or a duplicate
session; run from inside tmux it selects rather than nesting a client in a
client; and `eg phone` gets its own grouped session onto the same window.

### The daemon, actually exercised

`Type=notify` and the restart path were tested through a throwaway
`emacs-graph-selftest.service` — mechanically identical, its own socket name,
no guard — so the live socket was never at risk. **[verified]**: systemd got
`READY=1` and the unit went active in 0.77s, and the journal captured Emacs's
init stderr, which is the diagnosability the whole exercise was for.

Then the actual failure mode, reproduced deliberately: `SIGKILL` to the main
pid. **[verified]** —

```
15:42:43 emacs-graph-selftest.service: Main process exited, code=killed, status=9/KILL
15:42:44 emacs-graph-selftest.service: Failed with result 'signal'.
15:42:49 emacs-graph-selftest.service: Scheduled restart job, restart counter is at 1.
15:42:49 Started emacs-graph-selftest.service
```

That is the September 7 death, this time named in the journal and recovered in
six seconds. Compare §7.3, where the same event left only absences to argue
from. The selftest unit was removed afterwards.

**The cutover was left to a human, on purpose.** The daemon cannot take the
socket while another Emacs holds it — that is the guard working as designed —
so switching over means stopping the current Emacs first, and the current Emacs
is the one carrying the `emacs-repl` agent bus. Once it is gone:

```
eg
```

That is the whole cutover: nothing answers on the socket, so `eg` starts the
service and lands you in `emacsclient -t` inside tmux. The one link never
exercised end to end is `systemctl --user start emacs-graph.service` against
the *real* socket name, because doing so requires killing the Emacs this was
written from. Every part of it was verified separately.

Nothing here prevents another SIGKILL. What it changes is that the next one
leaves a journal entry, an exit code, a core and a backtrace instead of a set
of absences to argue from — and that the bus comes back on its own.

## 8. The real recurring crash, 2026-09-07 to 2026-09-08 — `cm.c:122`

§7 ended with "nothing here prevents another SIGKILL". What actually came next
was not a SIGKILL. The daemon has now aborted **three times** in its own display
code, and because §7.5's logging was in place it is the best-documented failure
on this box:

| # | when | pid | frame split? | outcome |
|---|------|-----|--------------|---------|
| 1 | 2026-09-07 22:10:01 | 881949 | yes | `SIGABRT`, restarted |
| 2 | 2026-09-08 03:01:32 | 2434199 | yes | `SIGABRT`, restarted |
| 3 | 2026-09-08 13:59:14 | 3503878 | yes | `SIGABRT`, restarted in 6 s |

All three: `emacs-graph.service: Main process exited, code=dumped, status=6/ABRT`
— **[verified]** in the journal, which is precisely the diagnosability §7.5 was
built for. Compare §7.3, where the same class of event left only absences to
argue from. Full write-ups with the gdb probes are in
`~/.emacs-graph/crashes/`; this section is the summary and the conclusion.

### 8.1 The defect

A shrink the window tree is allowed to refuse, which nobody checks.

When the tty collapses, `adjust_frame_size` calls `resize_frame_windows`, which
returns `void`. For a **leaf** root window (`window.c:5062`) it assigns the new
size and always succeeds. For a **split** root (`window.c:5082`) it tries a
normal resize, retries with the safe minimums, and if both fail it falls off the
end having changed nothing — two stacked windows need 2 lines each and a 4-line
frame leaves the root 3. `adjust_frame_size` cannot tell, and commits the new
height to `FRAME_LINES` and to `FrameRows` anyway. `adjust_frame_glyphs` then
derives its dimensions from the *window tree*, sees they disagree with
`FRAME_TOTAL_LINES`, and returns early without touching the matrices. So the
frame and the terminal say 4 rows while the tree still occupies 21 and the
matrices are still 21 rows tall. Redisplay walks the stale matrix, writes a
full-width row past the terminal's last line, and `cmcheckmagic` aborts.

`cm.c:122` is the symptom. The invariant break is upstream of it.

### 8.2 What made #3 definitive

The instrumentation from the 03:01 write-up (`frame-size-logging.el` on
`pre-redisplay-functions`, `~/bin/tmux-size-log` on tmux's resize hooks) was
running. Two questions that were open on 09-08 03:19 are now closed —
**[verified]** from the logs and from the core, which agree number for number.

**a. What collapses the terminal to 4 rows: the mosh client itself reports it.**

```
13:59:13.802  client-resized  /dev/pts/2  112x21     (from 50x29)
13:59:13.876  client-resized  /dev/pts/2  112x4      <- 74 ms later
```

Not a pane split, not `resize-window`. A 50x29 → 112x21 → 112x4 walk in 74 ms is
a DeX display switch handing the app a transient degenerate viewport — the same
class of event as the height-0 that aborts mosh-client in §3, one notch less
degenerate. §3's rotation hypothesis was right about the mechanism and wrong
only about which program it kills.

**b. The split precondition, previously derived from the source, is now observed.**

The 03:01 write-up called "this needs a SPLIT root at the moment of collapse" a
testable claim. It got tested by accident six hours before crash #3, by the same
client doing the same thing:

```
07:44:37.486  frame=112x21 root=20 mini=1 sum=21 OK        split=NO   wins=1
07:44:37.644  client-resized 112x4
07:44:37.737  frame=112x4  root=3  mini=1 sum=4  OK        split=NO   wins=1   -> survived 6h15m

13:59:13.799  frame=112x21 root=20 mini=1 sum=21 OK        split=YES  wins=2
13:59:13.876  client-resized 112x4
13:59:14.050  frame=112x4  root=20 mini=1 sum=21 MISMATCH  split=YES  wins=2   -> SIGABRT
```

Same client, same pty, same collapse, same day; the split is the only variable
that differs, and it decides whether the root shrinks cleanly or silently
declines to. That `MISMATCH` is the **only one in the whole log**, and the core
confirms it independently: frame `total_lines = 4`, tree `20 + 1`, matrices 21.
A controlled comparison instead of an inference. **[verified]**

> **Settled the same afternoon, 14:39.** This stopped being a two-instance
> comparison — see §8.3, where the split was toggled deliberately in both
> directions against a throwaway daemon.

### 8.3 The reproduction, fired — the split is causal

Run against a **throwaway** `emacs -Q --fg-daemon=repro` on its own socket, with
the probe's log redirected, so the bus was never at risk: afterwards
`emacs-graph.service` was still `active`, `NRestarts` still 3, and
`/home/joe/core` still the 13:59 crash. **[verified]** The whole thing took
about 90 seconds. Artifacts in `~/.emacs-graph/repro/`, write-up in
`~/.emacs-graph/crashes/20260908T144002-reproduction.txt`.

Same frame, same terminal, same collapse, one variable toggled:

```
CONTROL  delete-other-windows,   then resize-window -y 5
  14:39:49  frame=112x4 root=3  mini=1 sum=4  OK        split=no  wins=1   -> ALIVE

TEST     split-window-vertically, then resize-window -y 5
  14:40:02  frame=112x4 root=19 mini=1 sum=20 MISMATCH  split=yes wins=2   -> SIGABRT
```

The backtrace matches the 03:01 crash frame for frame, down to the two call
sites that distinguish it from the other two (`write_matrix` at dispnew.c:5783,
`redisplay_preserve_echo_area` at xdisp.c:18216). The core's window tree carries
the *same numbers* as the 03:01 core: root 19, children 10 and 9, minibuffer 1.
**[verified]**

Two practical notes for anyone re-running it, both about the harness rather than
the bug. A fresh `tmux -L repro` server has its status bar **on**, so a 21-row
window is a 20-row pane and `resize-window -y 5` is what gives a 4-row frame —
`~/.tmux.conf` sets `status off` (line 49), which is why the live numbers match
1:1 and the scratch server's do not. Set `status off` on the scratch server too
and the arithmetic stops being a trap. And `-Q` leaves the tty menu bar on, so
the core reads `total_lines = 5` (menu bar plus four) where the graph profile
read 4.

**One caveat this turned up:** a `MISMATCH` is not always fatal. The first line
of the repro log is a transient on the daemon's initial non-tty frame during
startup, resolved two seconds later with no crash. So `MISMATCH` looks
necessary but is not sufficient, and a guard hung on it must tolerate the benign
ones. On a tty frame mid-collapse it has been fatal every time observed.

### 8.4 A mitigation that was proposed and does not work

The 03:01 write-up suggested `set -g window-size largest`, so a short client
could not shrink the shared window. Today's client roster refutes it:
there is exactly **one** client attached — `/dev/pts/2`, with `dex` and `main`
grouped and both fed by it — **[verified]**. `largest` over one client is that
client, i.e. still 112x4. It would have changed nothing. It only helps when a
tall client is attached *alongside* the collapsing one, which is not this setup.

### 8.5 The guard — it survives now

`~/.emacs-graph/frame-size-guard.el`, loaded from `init.el` beside the probe and
hot-loaded into the running daemon at 14:48 so it protects the bus without
waiting for a restart. Record:
`~/.emacs-graph/crashes/20260908T1447-guard.txt`.

It hangs on `pre-redisplay-functions` — *appended*, so the probe logs the
provoking state before it is repaired — and acts only on the fatal shape: a tty
frame, shorter than the tree filling it, with a split root. It re-measures
afterwards and logs `REPAIRED` or `INCOMPLETE`, so it cannot claim a success it
did not achieve.

**The first version did not work, and the reason is worth keeping.** Collapsing
the split is necessary and not sufficient:

```
14:44:58.688  GUARD repair #1 ... -> root is now a leaf
14:44:58.688  frame=112x4 root=19 mini=1 sum=20 MISMATCH split=no
              -> SIGABRT anyway
```

`delete-other-windows` gives the survivor the size of the **root**, and the root
is still at its pre-collapse 19. Nothing re-runs `resize_frame_windows` with the
frame's new height: `frame.c:1077` only calls it when the height *changes*, and
by the time this hook runs the height has already been committed — so even an
explicit `set-frame-height` to the value it already holds is a no-op. The leaf
branch never got its chance. The fix is to nudge: drop a line and put it back.
Both values are inside the real terminal, so neither can overflow it, and with
the root now a leaf both assignments succeed.

Verified against the §8.3 harness — **[verified]**, every row:

| | |
|---|---|
| two-window split collapsed to 4 rows | `REPAIRED`, survived |
| grown back to 21 | layout restored |
| three more collapse/grow cycles | `REPAIRED` ×3 |
| three-window split collapsed | `REPAIRED`, survived |
| buffers across collapse+restore | `TOP`/`MID`/`BOT` all back |
| benign shrink 21 → 12, 3-way split | guard did **not** fire |
| `fsg--errors`, cores produced | 0, none |

The last two rows matter as much as the first: a guard that fires on a shrink the
tree can absorb would be its own bug.

**What is not proven:** it has not fired on the real socket. Same binary, same
code path, same terminal type, but not the graph profile's init and its frames.
The next DeX display switch settles it — a `GUARD repair … REPAIRED` line in
`frame-sizes.log` with `NRestarts` still at **3** is the proof.

### 8.6 Where this stands

Crash #3 cost 6 seconds (13:59:15 death → 13:59:21 active) and **no lost agent
invocation** — **[verified]**, nothing in the journal window matches §7.4's
`invoke-delivery failed`. With §8.5 installed, the next one should cost a
redrawn frame and a log line.

Not done, in rough order of value:

1. **The upstream bug report.** `resize_frame_windows` declining a shrink while
   `adjust_frame_size` commits it regardless is a genuine invariant break, and
   §8.3's recipe is small enough to paste into the report as-is.
2. **`set -g window-size manual`** plus a fixed `resize-window`, so client size
   stops driving window size at all. Costs the phone/DeX auto-fit that
   `README-termux.md` §5 exists for, and with §8.5 in place there is now little
   reason to pay that.

Also still open from §7.1: nothing forces the `eg` path to be used, and a bare
shell under `mosh-server` is still only *warned* about.

## 9. See also

- `README-termux.md` §1 — the one command, and why tmux sits under mosh
- `README-termux.md` §5 — `aggressive-resize on`, which is what makes the grouped
  views behave when phone and monitor differ in size
- `README-kinesis.md` — the other half of the phone-as-workstation input story

---
