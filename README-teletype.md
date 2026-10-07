# Teletype: one keyboard, two phones, two screens

Written 2026-10-07, after the original README-teletype was lost with Zone's drive
(it was never pushed). The code is new: `contrib/teletype/teletype.el`.

## The problem

Each Samsung phone drives one DeX screen at a time, and the keyboard plugs into one
phone. Two monitors means two phones, but only one of them has a keyboard. What's
needed is a way to type on phone A and have the typing land on phone B's screen:
a teletype.

## The idea

No network layer is needed. Both phones are already terminals onto the same Emacs
daemon (README-termux.md §1: each Termux window gets its own `emacsclient -t`
frame). The daemon is already the switchboard. Teletype just changes where phone A's
keystrokes take effect:

```
phone A (keyboard)                     the one Emacs daemon            phone B (monitor 2)
emacsclient -t frame  ── keys ──▶  teletype-relay ── runs in ──▶  B's selected window
*teletype* printout   ◀── log ──                                  shows the result
```

## Window-manager mode: left and right monitors

The everyday way to use it. Each frame is given its place on the desk, and the
keyboard's focus moves between them like a window manager's:

| key (on the keyboard phone) | effect |
|---|---|
| `C-c <left>` | keyboard goes to the **left** monitor: everything typed runs there |
| `C-c <right>` | keyboard comes home to the **right**: nothing is relayed |
| `C-]` | toggle between the two |

The focused monitor's mode line turns green and says `◆typing here◆`. While the
keyboard is away, the home frame's mode line says `[keys → left]`. At home,
teletype does nothing at all, and the right frame is an ordinary Emacs frame. Focus
keys are never relayed. The printout is still logged, in the background, to
`*teletype*`.

```elisp
;; in the frame on the monitor without a keyboard
(teletype-set-position 'left)
;; in the frame of the phone with the keyboard
(teletype-wm-start 'right)
```

Tried on the two phones with DeX on 2026-10-07 (phone1 right with the keyboard,
phone2 left), through `contrib/teletype/tt-frame` (installed as `~/bin/tt-frame` on lucy, with `teletype.el` beside it): `tt-frame left` / `tt-frame right`
(aliases `screen` / `keyboard`), called from each phone by `sh tt-demo.sh [keyboard]` (`contrib/teletype/tt-demo.sh`, which goes phone → metameso → lucy).
Start the left frame first, on a freshly started daemon.

## One-shot mode

On the daemon, once (e.g. in the init file of `emacs-graph.service`):

```elisp
(load "~/code/futon0/contrib/teletype/teletype.el")
```

Then:

1. Phone B: open its frame (`eg`), pick the buffer you want on monitor 2.
2. Phone A: `M-x teletype-connect`. With one other frame it is picked
   automatically; otherwise choose by frame name and tty.
3. Type. Everything runs in B's selected window: text, `C-a`, `C-x C-s`,
   `M-x …`, arrow keys, query-replace. A's screen becomes the **printout**, a
   running log of what was sent (plain text inline, chords as `‹C-x C-s›`).
4. `C-]` (telnet's escape) ends the session. A is a normal frame again.

**Prompts appear on A.** When a relayed command asks for something (the `M-x`
prompt, a file name, query-replace's y/n), the question shows on phone A, where you
are typing, and the command then acts on B. That turned out to be required, not just
convenient: see below.

## How it works

- **Capture, A's terminal only.** `teletype-connect` sets A's
  `overriding-terminal-local-map` to a map of default bindings, so every key on that
  terminal goes to `teletype-relay`. The variable is terminal-local, so other
  frames, and B itself, are unaffected. `ESC`, `ESC O` and `ESC [` stay prefixes:
  terminals send arrow and function keys as those sequences, and they must reach
  `input-decode-map` intact to become `<up>`, `<f1>` and so on.
- **Complete the sequence against B's keymaps.** Further keys are read with
  `read-key` on A, and whether more are needed (`C-x` …) is decided with B's window
  selected, so B's mode maps apply.
- **Run in B, read on A.** The command runs under `with-selected-window` on B's
  window. While it runs, the input primitives (`read-from-minibuffer`,
  `read-event`, `read-char`, `read-char-exclusive`, `read-key-sequence`,
  `read-key-sequence-vector`) are advised to do their waiting on A's frame. Without
  that, Emacs waits on B's terminal, which has no keyboard: **"Terminal 1 is
  locked, cannot read from it"**. Early tests passed only because tmux delivered
  keys faster than Emacs asked for them; a 2-second pause between `C-x` and `C-w`
  is now in the test.

## Test

```sh
contrib/teletype/test-teletype.sh
```

Needs `emacs` and `tmux`. Two tmux sessions play the two phones, attached as
`emacsclient -t` frames to a private `emacs -Q` daemon, so your own Emacs is not
touched.

- One-shot relay: typing plus `C-a`; `M-x upcase-word`; query-replace with `y n !`;
  `C-x`, a 2-second pause, then `C-w` to write B's buffer; arrow-key decoding; `C-]`.
- Window-manager mode: typing stays home; `C-c <left>` sends it left; both mode
  lines show the focus; `C-]` toggles; `C-c <right>` comes home and is not relayed.

Last run: all 12 pass (Emacs 29.3 on lucy, 2026-10-07).

## Not yet / known limits

- One keyboard at a time (the home and target are global), and the keyboard's
  home is fixed by `teletype-wm-start`.
- Mouse and touch on B are not relayed (B's own touch input still works locally).
- Echo-area messages from relayed commands show on B's frame. Prompts show on A.
- Commands that create new frames, or wait for input through means other than the
  advised primitives, have not been tested.
- A daemon that was not started cleanly can hang. That happened once, after
  loading new code into a daemon with an old relay active. Kill it and start fresh.
- Future work: have `tt-frame` detect an unresponsive daemon (say after 5 s) and
  restart it; more than two monitors; an Agency hook so an agent can "type" into
  a screen the same way.
