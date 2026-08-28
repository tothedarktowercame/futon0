# Kinesis Advantage2 on a phone: onboard layouts, the v-Drive, and a third layer

Worked out on 2026-08-28, day one of the laptop-free setup. Items marked
**[verified]** were actually observed on the hardware; everything else is read from
the manual or inferred, and is flagged as such.

Context: Dionysus went back to IT, Zone is the machine, and a Samsung phone running
DeX and Termux is the client (see `README-termux.md`). The Kinesis Advantage2 plugs
into the phone. There is no X, so no XKB — every trick that used to live in a
keymap has to live either in the keyboard's own firmware or in Emacs.

The live layout is saved next to this file at `kinesis/dvorak.txt`, with
`kinesis/state.txt` and the now-unused `kinesis/qwerty.txt`. Restoring is a matter of
copying `dvorak.txt` back into `active/` on the v-Drive.

---

## 1. The rule that explains every failure

**Exactly one of the keyboard or the phone should apply Dvorak. Never both, never
neither.**

Three separate breakages on the first day were all violations of this:

- *Neither* — the phone silently reverted to a QWERTY layout, so Dvorak muscle memory
  produced QWERTY letters and every punctuation remap emitted its raw character.
- *Both* — the keyboard was switched to its onboard Dvorak while the phone was still
  translating, producing what looks like line noise but decodes cleanly by applying
  the Dvorak map twice. **[verified]** Typing the three letter rows gave
  `-wvlfuijpn ar.gcedybo s'htqxm,k;`, which is exactly double-Dvorak of
  `qwertyuiop asdfghjkl; zxcvbnm,./`.
- *Neither, again* — `Progm`+`F3` restored the keyboard to QWERTY, which fixed typing
  but silently deactivated the layout holding all the remaps.

### Which one should do it: the keyboard

Put Dvorak **in the keyboard** (`Progm`+`F4`, layout file `active/dvorak.txt`) and set
the phone's physical-keyboard layout to plain **English (US)**. Two reasons:

1. **Mounting the v-Drive re-enumerates the keyboard over USB, and Android responds by
   dropping the per-device keyboard layout back to Default.** **[verified]** Under the
   other arrangement, every single v-Drive mount silently broke the whole layout — which
   is maddening to debug, because the thing you changed and the thing that broke are
   unrelated.
2. In `dvorak.txt` the location tokens are *native Dvorak*, so a key is named by the
   letter it actually types. See §4.

Not the "international style" variant of English (US): its dead keys turn `'` into a
composing accent, so `'` then `w` yields `ẃ`. **[verified]** That breaks apostrophe in
Emacs and backtick in the shell.

---

## 2. Reaching the v-Drive from Android

`Progm`+`F1` mounts the keyboard's 4 MB onboard flash as a USB volume. The chord is
handled by the keyboard's own processor, so it still works while HID output is
disabled — you cannot lock yourself in. Touch also keeps working, which matters
because typing does not.

Termux **cannot** reach the volume by path. `/storage` and `/mnt/media_rw` are both
permission-denied to its uid, and no USB volume appears in `/proc/mounts`.
**[verified]** Removable volumes are reachable only through the Storage Access
Framework.

The `termux-saf-*` tools (from `termux-api`, plus the Termux:API app) do exactly this:

```sh
termux-saf-managedir          # one-time picker; grant the 'active' folder
termux-saf-ls    "$URI"
termux-saf-read  "$URI"
termux-saf-write "$URI"       # reads stdin, truncates the target
```

Volume UUID is `2E35-0346`; the `active/` tree URI is

```
content://com.android.externalstorage.documents/tree/2E35-0346%3Aactive/document/2E35-0346%3Aactive
```

### Three traps in this loop

- **SAF grants do not survive ejecting the volume.** **[verified]** `termux-saf-dirs`
  returns `[]` after every eject, so `managedir` must be re-run each time.
- **Fire the picker *before* mounting.** You cannot type once the v-Drive is up, so you
  cannot tell a remote agent to go. Launch the picker first; it notices the volume when
  it appears.
- **`termux-saf-write` returns success when writing into an ejected volume.**
  **[verified]** One write was silently lost this way. *Always read back, and treat an
  empty read as failure.*

Driving this from Zone over the reverse tunnel (`ssh -p 2222 joe@localhost`, see
`README-termux.md`) works well: Joe mounts and taps the picker, the agent reads,
edits and verifies.

---

## 3. Always read before you write

Onboard programming (`Progm`+`F12`) makes the keyboard **rewrite the entire layout
file in its own order**, normalising case and appending its own lines.
**[verified]** A blind overwrite therefore destroys any remap made by hand since the
last read. Read the file, transform it, write it back.

---

## 4. Tokens

Syntax is `[location]>[action]` for remaps, `{location}>{action}...` for macros. The
bracket style is what distinguishes them.

**Location tokens are layout-relative.** In `dvorak.txt` they are native Dvorak: the
key that types `o` is `[kp-o]`. In `qwerty.txt` the same physical key is `[kp-s]`.
This is the single largest source of confusion, and the reason the keyboard-side
Dvorak arrangement is worth having — you name keys the way you think of them.

**Action tokens are layout-independent** and are mostly the bare character:

```
=   \   /   ;   `   '        the character itself
HYPHEN  OBRACK  CBRACK       the exceptions that are words
```

There is no `[equal]`, `[equals]` or `[bslash]`; those parse as nothing and the key
silently falls back to its default action. **[verified]** — this cost two mount
cycles before the manual settled it.

USB HID decimal codes work anywhere an action token does (`[enter]>[88]`), which
sidesteps token-name guessing entirely.

The authoritative list is the Advantage2 User's Manual, Appendix 13.2, with the
location maps in 13.1 — those are *diagrams*, so they need rendering rather than text
extraction:

```sh
pdftoppm -r 200 -f 37 -l 37 -png adv2-manual.pdf p37
```

The `SmartSet App Help.pdf` sitting on the v-Drive itself is only the Windows app
manual and contains no token table.

---

## 5. The third layer

The keypad layer is a real second layer, entered by a modifier. To make a key a
*momentary* modifier rather than a latching toggle:

```
[delete]>[kpshift]
[kp-delete]>[kpshift]
```

**Layer-shift tokens must be mapped in both layers.** The manual states this in a
footnote to Fig 17, and omitting the `kp-` line is precisely why the first attempt
latched on with no way back. **[verified]**

`[kpshift]` has no tap action, so a key mapped to it cannot fire by accident — which
is what makes Delete and Enter safe choices for it.

### Three tiers of difficulty

Getting a character onto the third layer depends on what kind of character it is.
This is a property of USB HID, not of Kinesis.

**1. Unshifted characters — a plain remap.**

```
[kp-a]>[obrack]      hold + a  ->  [
```

**2. Shifted characters — impossible by remap; needs a macro.**

A remap assigns a keycode, and HID carries shift as a separate modifier bit in the
report, so a remap cannot assert it. The manual: *"there is no way to obtain a shifted
character/symbol via remapping."* A macro can hold Shift across a keystroke:

```
{kp-hyphen}>{-lshift}{`}{+lshift}      hold + -  ->  ~
```

**3. Characters with no keycode at all — the host must do it.**

An em dash has no HID keycode in any shift state, so the keyboard fundamentally
cannot emit one. Have the macro send an unused chord and let the host insert the
character:

```
{kp-\}>{-lctrl}{c}{+lctrl}{m}          keyboard sends C-c m
```

```elisp
;; ~/.emacs-graph/init.el  (the live profile: emacs -nw --with-profile graph;
;; ~/.emacs is only a chemacs bootstrap)
(defun my/insert-em-dash ()
  "Insert an em dash (U+2014)."
  (interactive)
  (insert 8212))
(global-set-key (kbd "C-c m") #'my/insert-em-dash)
```

This tier scales: any character in Unicode is now two lines, one in `dvorak.txt` and
one in `init.el`. The cost is that it only works inside Emacs — there is no universal
compose mechanism on Android.

### Macros are fragile in one specific way

**Onboard programming truncates macros.** **[verified]** After a `Progm`+`F12` remap,
the rewritten file ended mid-line at a bare `{kp-`, having eaten the tilde macro. The
file was 333 bytes on the drive and 333 bytes when read, so this was genuine
truncation and not a read artifact.

So: after any onboard remap, re-read the file and restore the macro lines. Better, do
remaps through the file in the first place.

---

## 6. The current layout

Both outer thumb keys are momentary modifiers; the inner ones keep Backspace and
Space. Enter moved to `[rctrl]`. Held under either thumb key:

```
'  ->  \                        ,  ->  Backspace
.  ->  up                       p  ->  Delete
o  ->  left                     y  ->  `
e  ->  down                     -  ->  ~     (the / ? ~ key)
u  ->  right                    \  ->  em dash
a  ->  [       i  ->  ]
```

The arrow keys form an inverted-T on the left home row, which they need to, because
the four physical arrow keys were given over to Ctrl and Alt in the top layer.

Two loose ends, both harmless:

- `{kp-\}` and `{kp\}` are both present as em-dash triggers. The manual's rule says
  keypad locations take a `kp-` prefix, but Fig 33 renders that key without one, and
  it is unclear whether that is real or kerning. One of the two is being ignored as
  unparseable; we have not determined which.
- ``[kp-y]>[`]`` and `{kp-/}` are leftovers from diagnosing the tilde. They work and do
  no harm.

Never tested: the tap-and-hold syntax, `[lshift]>[esc][t&h250][lshift]`, which gives a
key one action when tapped and another when held. It was written and then abandoned in
favour of a modifier with no tap action at all. It would be the way to get a thumb key
that is *both* Backspace and a modifier.

---

## 7. Recovery

- `Progm`+`F3` / `Progm`+`F4` — reselect the QWERTY / Dvorak layout, forcing a reload
  without closing the v-Drive.
- `Progm`+`Esc` — Status Report, types the current settings into the focused window.
- `Progm`+`F12` **while plugging in** — Rescue Reset. Boots without reading the
  v-Drive at all, giving stock QWERTY. Non-destructive; use this to prove the hardware
  is fine and isolate a fault to the layout file.
- `Progm`+`F9` **while plugging in** — Hard Reset. *Erases custom layouts.* Back up
  first; `kinesis/dvorak.txt` beside this file is that backup.

Changes take effect when the v-Drive closes. Eject from the Android notification
shade *first* — the volume is FAT, and pulling it out from under a buffered write is
how you corrupt the volume rather than just the file — then `Progm`+`F1`.
