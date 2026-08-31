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

## 7. See also

- `README-termux.md` §1 — the one command, and why tmux sits under mosh
- `README-termux.md` §5 — `aggressive-resize on`, which is what makes the grouped
  views behave when phone and monitor differ in size
- `README-kinesis.md` — the other half of the phone-as-workstation input story
