# DeX + Termux → Zone

Dry run done 2026-08-12. Everything below marked **[verified]** was actually
tested from Dionysus against Zone, not assumed.

## The one command

From Termux, this is the whole thing:

```bash
mosh zone -- tmux new-session -A -s main
```

`new-session -A` attaches if `main` exists and creates it otherwise, so the same
command works the first time and every time. **[verified]** idempotent — running
it twice leaves one session, not two.

Session `main` is already running on Zone.

## Why mosh rather than ssh

A phone changes network constantly — wifi to cellular, cell to cell, sleep and
wake. SSH dies on every one of those; mosh survives all of them, because it is
UDP with client-side prediction and reconnects on its own.

**[verified]** mosh 1.4.0 is installed on both ends and **the UDP path
(60000–61000) is open** — tested with a real connection, since a firewall
blocking that range is the one thing that would sink this approach. It works.

The pairing that matters: **mosh survives the network, tmux survives mosh.** If
the phone dies entirely, the tmux session keeps running on Zone and you reattach
to exactly what you left.

## Phone side (Termux) — getting mosh running

```bash
pkg update && pkg install openssh mosh
```

That is genuinely most of it. Two things bite people, in this order:

**1. The locale.** mosh *refuses to start* without a UTF-8 locale and says so
obscurely:

```
mosh-server needs a UTF-8 native character set.
The locale specified by the environment is invalid.
```

Termux often starts with `LANG` unset. Fix it permanently:

```bash
echo 'export LANG=en_US.UTF-8' >> ~/.bashrc
echo 'export LC_ALL=en_US.UTF-8' >> ~/.bashrc
source ~/.bashrc
```

mosh passes your *local* locale to the server, so it is the phone's `LANG` that
matters here, not Zone's. **[verified]** Zone's side is fine — `en_GB.UTF-8`
with UTF-8 locales available.

**2. mosh-server must be findable over a non-interactive ssh.** mosh SSHes in and
runs `mosh-server`; if it is not on the *non-login* PATH you get
`command not found` and a confusing "Nothing received from server" hang.
**[verified]** Zone is fine — `/usr/bin/mosh-server`, on the default PATH.

If it ever is not, the escape hatch is to say where it lives:

```bash
mosh --server=/usr/bin/mosh-server zone
```

**Test in stages** so a failure tells you which layer broke:

```bash
ssh zone                      # 1. auth + network
ssh zone 'mosh-server --help' # 2. server binary reachable non-interactively
mosh zone                     # 3. the UDP path
mosh zone -- tmux new-session -A -s main   # 4. the real thing
```

Step 3 is the one that fails on a hostile network — mosh needs **UDP
60000–61000** outbound. **[verified]** open from here; a mobile carrier could
still block it, and the symptom is a hang at "Connecting..." after ssh
authenticates fine. If that happens on cellular but not wifi, that is the cause,
and plain `ssh` + `tmux` still works as a fallback.

**[verified]** your `dexphone` ed25519 key is already in Zone's
`authorized_keys`, so auth is done — nothing to generate.

Add to Termux's `~/.ssh/config`:

```
Host zone
    HostName <zone-address>
    User joe
    IdentityFile ~/.ssh/id_ed25519
    ServerAliveInterval 30
```

**Turn on the extra-keys row.** Without it Emacs is unusable on a touch
keyboard — there is no ESC and no Ctrl. In `~/.termux/termux.properties`:

```
extra-keys = [['ESC','/','-','HOME','UP','END','PGUP'], \
              ['TAB','CTRL','ALT','LEFT','DOWN','RIGHT','PGDN']]
```

Then `termux-reload-settings`. With a DeX hardware keyboard this matters less,
but it is what makes the phone-only fallback survivable.

## What is already set up on Zone

**[verified]** `~/.tmux.conf` is already tuned for exactly this — someone (you,
or an earlier session) got here first:

- `aggressive-resize on` — resizes to the *smallest attached client per window*,
  so when you attach from the phone and later from a DeX monitor, the windows you
  are not looking at stay full size. This is the setting that makes
  phone-and-desktop-at-once bearable.
- `mouse on` — touch scrolling and pane selection work
- `escape-time 0` — no ESC lag, which is otherwise painful over mosh
- `history-limit 50000`, truecolor, vi copy-mode, `|` and `-` splits

**[verified]** also present: emacs 29.3, git 2.43, bash 5.2, locale
`en_GB.UTF-8` (needed — the sigil work is unicode-heavy).

Not installed: `nvim`, `zsh`, `screen`. Say if you want any.

## Running both machines at once

While you still have Dionysus, the useful arrangement is that **Zone holds the
session and both clients attach to it**. Work does not live on either client.
Attach from Dionysus with the same command; `aggressive-resize` stops the phone's
small screen from shrinking your desktop windows.

That also makes the eventual handover a non-event: when Dionysus goes back, one
client stops attaching and nothing else changes.

## Known gaps

- **GUI-dependent workflows.** Playwright is fine headless. The JVM surfaces on
  `:7073` and the Emacs GUI need either nginx in front (viewing) or terminal
  Emacs (editing). Terminal Emacs is already there.
- **The `holtzermann17` token and a `github-holtz` SSH stanza** are still needed
  on Zone before the repo clones can happen. Dionysus reaches that account via a
  host alias, not plain `github.com`, so the key alone is not enough.
- **Font coverage on the phone** for mathematical unicode is untested — the
  sigils (`⊣`, `⧄`, `≐`) may render as boxes in Termux. Worth a look once you
  attach; it affects reading pattern files, not running anything.
