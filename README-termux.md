# Termux, DeX, and the phone-as-workstation

Everything worked out on 2026-08-12 while setting Zone up as the main machine.
Items marked **[verified]** were actually tested from Dionysus against Zone;
anything about the *phone* is untested, because there was no phone in the loop.

Context: Dionysus goes back to its employer, Zone becomes the main machine, and
the phone (Samsung + DeX + Termux) becomes a client. Related:
`~/code/DEX-SETUP.md`.

---

## 1. The one command

```bash
mosh zone -- tmux new-session -A -s main
```

`new-session -A` attaches if `main` exists and creates it otherwise, so the same
command works the first time and every time. **[verified]** idempotent, and the
session `main` already exists on Zone.

**mosh survives the network, tmux survives mosh.** A phone changes networks
constantly — wifi to cellular, cell to cell, sleep to wake — and plain SSH dies
on every one of those. If the phone dies entirely, the tmux session keeps
running on Zone and you reattach to exactly what you left.

---

## 2. Getting mosh working

```bash
pkg update && pkg install openssh mosh
```

Two things bite, in this order.

**The locale.** mosh refuses to start without a UTF-8 locale, and says so
obscurely:

```
mosh-server needs a UTF-8 native character set.
The locale specified by the environment is invalid.
```

Termux often starts with `LANG` unset:

```bash
echo 'export LANG=en_US.UTF-8'  >> ~/.bashrc
echo 'export LC_ALL=en_US.UTF-8' >> ~/.bashrc
source ~/.bashrc
```

mosh sends your *local* locale to the server, so it is the **phone's** `LANG`
that matters — Zone being `en_GB.UTF-8` will not save you.

**`mosh-server` must be findable over a non-interactive ssh.** mosh SSHes in and
runs `mosh-server`; if it is not on the *non-login* PATH you get a confusing
"Nothing received from server" hang rather than a clear error. **[verified]**
Zone is fine: `/usr/bin/mosh-server`, on the default PATH. Escape hatch if it
ever is not:

```bash
mosh --server=/usr/bin/mosh-server zone
```

**Test in stages**, so a failure says which layer broke:

```bash
ssh zone                       # 1. auth + network
ssh zone 'mosh-server --help'  # 2. server binary reachable non-interactively
mosh zone                      # 3. the UDP path
mosh zone -- tmux new-session -A -s main    # 4. the real thing
```

Step 3 is the one that fails on a hostile network — mosh needs **UDP
60000–61000** outbound. **[verified]** open from Dionysus; a mobile carrier can
still block it independently. Symptom: ssh authenticates fine, then mosh hangs
at "Connecting...". If that happens on cellular but not wifi, that is the cause,
and plain `ssh` + `tmux` still works as a fallback — you only lose roaming.

**[verified]** the `dexphone` ed25519 key is already in Zone's
`authorized_keys`. Auth is done; nothing to generate.

Termux `~/.ssh/config`:

```
Host zone
    HostName <zone-address>
    User joe
    IdentityFile ~/.ssh/id_ed25519
    ServerAliveInterval 30
```

---

## 3. SSHing *into* the phone

Yes, this works — Termux ships a real OpenSSH server.

```bash
pkg install openssh
passwd                  # or put a key in ~/.ssh/authorized_keys (better)
sshd                    # start it
whoami                  # your username, e.g. u0_a123
```

**Termux's sshd listens on 8022, not 22** — Android forbids unprivileged binding
below 1024. So: `ssh -p 8022 u0_a123@<phone-ip>`.

Two Android-specific gotchas: run `termux-wake-lock` and exempt Termux from
battery optimisation, or Android kills sshd when the screen sleeps. And `sshd`
does not survive a reboot unless you install the **Termux:Boot** addon.

**On a LAN that is the whole story. Over the internet it is not** — mobile
carriers put you behind CGNAT, so the phone has no reachable address. That is
the real obstacle, not Termux.

**Reach it through Zone instead.** **[verified]** Zone's sshd has
`allowtcpforwarding yes`, so:

```bash
# on the phone, keep this running
ssh -N -R 2222:localhost:8022 zone

# then from anywhere
ssh zone -t 'ssh -p 2222 u0_a123@localhost'
```

**[verified]** Zone has `gatewayports no`, so the tunnel binds to Zone's
loopback only. That is the safe default and is fine here, since you reach it by
SSHing into Zone first. Do not change it.

**Tailscale is the better answer if this becomes routine.** Not installed on
Zone as of today. It handles CGNAT properly, gives every device a stable
address, survives network changes, and needs no tunnel babysitting. One tailnet
across Zone + phone (+ Dionysus while it lasts) would make all of the above
simpler than the hand-rolled version.

Worth deciding *what for* first: moving files is often nicer via Syncthing, or
`termux-setup-storage` plus rsync **to** Zone. sshd into the phone earns its
keep when you want to *run* something there, or pull photos and recordings.

---

## 4. Keyboard: Dvorak on the Kinesis Advantage 2

**The situation.** Dvorak is done in *software* on the Linux side — sway sets
`xkb_layout us` / `xkb_variant dvorak` globally, and the Kinesis
(`10730:258:Kinesis_Advantage2_Keyboard`) gets a per-device override to
`~/.config/xkb/kinesis.xkb`: 1,960 lines, 219 key definitions. The keyboard
itself sends QWERTY.

That file is not plain Dvorak. `<AC02>` is a four-level semialphabetic key
giving `o` / `O` / **`Left`**, with a second group also mapping to `Left` —
**arrow keys on the home row via modifier levels**, plus dead keys on level 4.

**Android has no XKB, so none of that transfers.** No Termux package can help;
layout lives below the terminal, not in it.

Three options, in the order worth trying:

1. **Android's physical-keyboard layout → Dvorak** (Settings → General
   management → Physical keyboard). Instant, free, changes nothing on the Linux
   side. You get standard Dvorak and **lose the home-row arrows and levels 3–4**.
2. **Kinesis onboard remapping via the v-Drive.** Puts the layout in firmware so
   it transfers to any host with no config. Two problems: it would
   **double-translate under sway** (which already applies Dvorak) unless the
   Kinesis input block is changed to plain `us` and kept in sync forever; and the
   onboard system is a *scancode remapper* with no notion of modifier levels or
   dead keys, so the home-row arrows almost certainly cannot be reproduced.
3. **External Keyboard Helper Pro** (Android app). The only thing supporting
   genuinely custom physical-keyboard layouts with multiple levels — closest
   analogue to XKB. Paid, fiddly, untried.

Start with (1) and treat it as a test of how much the custom levels are actually
missed away from the desk. **Never do (1) and (2) at once** — that is the
double-translation case, and it produces convincing-looking garbage rather than
an obvious error.

---

## 5. Termux ergonomics

**Turn on the extra-keys row** — without it terminal Emacs is unusable on a
touch keyboard, because there is no ESC and no Ctrl. In
`~/.termux/termux.properties`:

```
extra-keys = [['ESC','/','-','HOME','UP','END','PGUP'], \
              ['TAB','CTRL','ALT','LEFT','DOWN','RIGHT','PGDN']]
```

Then `termux-reload-settings`. Matters less with a DeX hardware keyboard, but it
is what makes the phone-only fallback survivable.

**Zone's `~/.tmux.conf` is already tuned for this** **[verified]**, including
`aggressive-resize on` — resizes to the smallest attached client *per window*, so
attaching from the phone does not shrink the windows you are viewing on a
monitor. Also `mouse on` (touch scrolling), `escape-time 0` (no ESC lag over
mosh), 50k scrollback, vi copy-mode.

**Untested:** whether mathematical unicode (`⊣`, `⧄`, `≐`) renders in Termux or
comes out as boxes. Affects reading pattern files, not running anything.

---

## 6. The physical bit

The Advantage 2 is **wired USB-A**, and DeX uses the phone's single USB-C port,
which the monitor also wants. You need a USB-C hub with HDMI + USB-A **and power
passthrough** — driving a display plus a keyboard will flatten the phone
otherwise. Worth testing before committing to the setup.

---

## 7. Working with both machines at once

While Dionysus is still around: **Zone holds the session, both clients attach to
it.** Work lives on neither client. `aggressive-resize` stops the phone's small
screen from shrinking the desktop's windows.

That also makes the handover a non-event — when Dionysus goes back, one client
stops attaching and nothing else changes.
