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

## Use

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
touched. It checks: typing plus `C-a`; `M-x upcase-word`; query-replace with
`y n !`; `C-x`, a 2-second pause, then `C-w` to write B's buffer; arrow-key decoding;
and `C-]`. Last run: all 6 pass (Emacs 29.3 on lucy, 2026-10-07).

## Not yet / known limits

- **Not yet tried on the real phones with DeX.** The test uses tmux terminals.
- One session at a time (the target is global), and A → B only, not both ways.
- Mouse and touch on B are not relayed (B's own touch input still works locally).
- Echo-area messages from relayed commands show on B's frame. Prompts show on A.
- Commands that create new frames, or wait for input through means other than the
  advised primitives, have not been tested.
- Ideas: `teletype-swap` (flip direction), naming frames by phone, and an Agency
  hook so an agent can "type" into a screen the same way.
