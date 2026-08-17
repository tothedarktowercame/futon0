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
command works the first time and every time. **[verified]** idempotent.

**Correction 2026-08-17:** the earlier claim that "`main` already exists on Zone"
went stale — by that afternoon `tmux list-sessions` reported *no server running on
`/tmp/tmux-1000/default`*, leaving only a stale socket. **A tmux server exits when
its last session ends**, so a standing session is not self-sustaining and does not
survive a reboot either. `-A` makes the command work regardless, but §8's model
("Zone holds the session") is only true while a session is actually up. `main` was
recreated 2026-08-17 15:46 and is empty. If the model matters, it wants a systemd
user unit rather than trust.

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
`authorized_keys`. Auth is done; nothing to generate. **Re-verified 2026-08-17**
— the `dexphone` comment is present, 2 keys authorised in total.

**Verify Zone's host key on the phone rather than blind-accepting it.** The phone
has no prior `known_hosts` to cross-check against, which per
`README-secrets.md` §5 is exactly the case where a fingerprint has to be carried
in out-of-band. Zone's, read from Zone's own `/etc/ssh/ssh_host_*_key.pub` over an
authenticated session and cross-checked against Dionysus's `known_hosts`
(**both sources agree**, 2026-08-17):

```
ED25519  SHA256:rD4PiQkdU1UwjPsIeXiUKApqxK0azy+hiMyjl03+CFA
RSA      SHA256:+LGf1iffhmx9AlqbSRwD9AzWJZL1JCUqpFBa4FmolLU
```

Compare what the phone shows on first connect against the ED25519 line. Dionysus
is the second source, so **do this before the handback** — afterwards there is no
independent path left to confirm it.

### Zone-side prerequisites — all re-verified 2026-08-17

Nothing on Zone is blocking; the remaining work is entirely phone-side.

| | |
|---|---|
| sshd port | **22** — so `-R 2222:…` cannot collide, and 2222 is confirmed free |
| `allowtcpforwarding` | `yes` |
| `gatewayports` | `no` (tunnel binds Zone's loopback only — keep it) |
| `mosh-server` | `/usr/bin/mosh-server`, on the default non-login PATH |
| `tmux` | `/usr/bin/tmux` |
| locale | `LANG=en_GB.UTF-8` |
| `.tmux.conf` | all three claimed settings present; `mouse on`, `history-limit 50000` confirmed live |
| `dexphone` key | present in `authorized_keys` |

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

### **[verified] 2026-08-17 — the whole path works.** No longer theoretical.

```
Zone → ssh -p 2222 u0_a61@localhost                 → phone
anywhere → ssh zone -t "ssh -p 2222 u0_a61@localhost" → phone
```

Confirmed from Zone's side, not just by the prompt looking right: `ss -tlnp`
showed `127.0.0.1:2222` **and** `[::1]:2222` bound (loopback only — `gatewayports
no` doing its job), and the hop returned `REACHED:u0_a61 on Android`. The phone's
username is **`u0_a61`**.

Phone host key, verified two ways per `README-secrets.md` §5 — its own
`$PREFIX/etc/ssh/ssh_host_ed25519_key.pub`, and what Zone recorded on first
connect. Both agree:

```
ED25519  SHA256:semcbnLwtEmwp3Mx5ef3eHmuR//JWka8xrpFYF0mlEs
```

**Zone will not notice a dead tunnel.** `sshd -T` on Zone reports
**`clientaliveinterval 0`**, so it never probes idle clients. A sleeping phone
therefore leaves port 2222 **bound**, the next rebind fails, and you get a tunnel
that looks up but routes to a dead socket — silent-wrong, the worst shape. Fix it
from the phone end, which needs no server change:

```sh
ssh -N -R 2222:localhost:8022 zone \
    -o ServerAliveInterval=30 -o ServerAliveCountMax=3 \
    -o ExitOnForwardFailure=yes
```

`ServerAlive*` makes the phone notice and exit; `ExitOnForwardFailure` makes a
failed rebind **loud instead of pretending to work**. Wrap in a retry loop —
`autossh` is not installed on Zone and would have to be on the phone anyway.

**Don't retype any of this on a touch keyboard.** Two scripts live on Zone;
pull them rather than transcribing, so the content arrives byte-exact:

```sh
ssh zone cat phone-setup.sh  > s.sh && less s.sh && sh s.sh   # idempotent
ssh zone cat phone-tunnel.sh > t.sh && sh t.sh                # leave running
```

`phone-setup.sh` installs openssh, appends Zone's public key to the phone's
`authorized_keys` (with a trailing-newline guard so it cannot corrupt an existing
entry), starts `sshd` only if not already running, takes the wake-lock, and prints
the `u0_aNNN` username. They are also displayed in tmux `main:scratch` on Zone.

Clipboard *does* work in Termux (long-press, plus `termux-clipboard-get/set` once
`termux-api` and the Termux:API app are installed) — but a tmux copy lands in
tmux's own buffer **on Zone** and does not reach the phone's clipboard, and long
keys are where silent truncation costs an hour. Pulling the file avoids both.

Still manual, once each: battery-optimisation exemption, and the **Termux:Boot**
addon so `sshd` survives a reboot.

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

## 7. Passwords on the phone

The store now lives on **Zone**, not on Dionysus. **[verified]** 2026-08-17:

```
sec  ed25519/05B0D5246477D771  2026-08-17 [SC] [expires 2029-08-16]
ssb  cv25519/0B84F53B943B2184  [E]
~/.password-store   61 entries, git-initialised
```

61 logins exported from Firefox and imported as `firefox/<host>/<username>`;
**[verified]** all 61 are encrypted to the cv25519 subkey, so all are recoverable
with the passphrase. The old GPG key is dead — its passphrase was not remembered
and not cached — and its four `pass` entries were Linode credentials, which get
**rotated, not recovered**. That is the whole reason this store was rebuilt.

**The phone does not need a copy of the secret key.** `pass` encrypts each entry
to any number of recipients, so the phone gets its *own* key and is added as a
second recipient:

```bash
# on the phone (Termux)
pkg install gnupg pass git
gpg --quick-generate-key "Joe (phone) <holtzermann17@gmail.com>" default default 3y
gpg --export --armor > phone.pub          # move this to Zone; email or paste is fine

# on Zone
gpg --import phone.pub
pass init <zone-fingerprint> <phone-fingerprint>   # re-encrypts the whole store to both

# on the phone
git clone ssh://zone/~/.password-store ~/.password-store
```

`pass init` with several ids re-encrypts every entry, so this is additive and
costs nothing to defer — do it whenever the phone is actually set up.

**Untested**, because there was no phone in the loop: whether Termux's `pass`
finds a working pinentry (it may need `pinentry-tty` and
`export GPG_TTY=$(tty)` in `~/.bashrc`), and whether `gpg-agent` caching behaves
under Android's process lifecycle. Both are phone-side unknowns, not Zone-side.

**Do not** move the secret key between devices to save a step. Separate
per-device keys mean a lost phone is revoked by re-running `pass init` without
its fingerprint, rather than by rotating all 61 passwords.

## 8. Working with both machines at once

While Dionysus is still around: **Zone holds the session, both clients attach to
it.** Work lives on neither client. `aggressive-resize` stops the phone's small
screen from shrinking the desktop's windows.

That also makes the handover a non-event — when Dionysus goes back, one client
stops attaching and nothing else changes.
