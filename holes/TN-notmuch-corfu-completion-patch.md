# TN-notmuch-corfu-completion-patch

**Status:** draft, parked by Joe 2026-11-16 ("I'll come back to it later").
**Owner:** claude-3 prepared it; Joe sends anything upstream (it goes out under his name).

## The problem

In a notmuch compose buffer (`notmuch-message-mode`) with corfu, TAB on a
`To:` line sometimes offered spell-check words ("rob" → "robe") instead of
addresses, and sometimes gave notmuch's address prompt.

notmuch registers `notmuch-address-expand-name` in `message-completion-alist`.
That function does the completion itself (it prompts via
`notmuch-address-selection-function` and inserts) instead of returning
completion data. message.el handles such "old-style" completers specially in
`message-completion-function`: it calls the function directly, and once the
function has modified the buffer it adds it to
`message--old-style-completion-functions` and thereafter returns a wrapper
lambda.

Emacs 30+ also adds `ispell-completion-at-point` to text-mode buffers
(`text-mode-ispell-word-completion`), so it sits after
`message-completion-function` in `completion-at-point-functions`.

Three routes, simulated 2026-11-16 on a `To: rob` line (emacs-graph, Emacs
32.0.50, notmuch 0.38.3, corfu from straight):

| route | which completion source answers |
|---|---|
| plain TAB, no popup (`message-tab` → `completion-at-point`) | notmuch (old-style wrapper is called) |
| corfu's own collection (`corfu--capf-wrapper`, used while its popup is open) | **ispell**: corfu skips capfs that return a function |
| before notmuch's completer has once inserted something, and it finds no match (e.g. harvest still running) | **ispell**: it returned nil without modifying the buffer |

So it "worked sometimes": it depended on whether a corfu popup was already
open (e.g. after an ispell popup in Subject/body) or whether notmuch had ever
completed in this session.

## Upstream status

Checked 2026-11-16 against a fresh clone of notmuch master
(`git.notmuchmail.org/git/notmuch`, 0c42eb1, 2026-11-08, version.txt 0.40):
`emacs/notmuch-address.el` still has only `notmuch-address-expand-name`, plus
a company-specific path (`notmuch-address-use-company` → `notmuch-company`).
No completion-at-point function. Not yet checked: the notmuch mailing list
archive, for earlier patches or discussion. Do that first.

## Local fix (in use)

`~/.emacs-graph/notmuch-setup.el`, function `my/notmuch-address-capf`, added
buffer-locally via `notmuch-message-mode-hook`. It returns
`(start end table :exclusive t)` on address header lines (including
continuation lines and the part after the last comma), with the table over
`notmuch-address-options`, ignoring the typed string for matching because
notmuch already matches anywhere in name or address. Verified: wins on both
the `completion-at-point` route and corfu's own route; Subject and body still
get ispell.

## Proposed upstream patch

1. Add `notmuch-address-completion-at-point` to `emacs/notmuch-address.el`:
   same shape as the local capf, using `notmuch-address-completion-headers-regexp`
   and the existing begin-of-address search from `notmuch-address-expand-name`
   (`re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*"`), category `email`.
2. In `notmuch-address-setup`, register it in `message-completion-alist`
   (message.el accepts a function returning capf data there: if the result's
   car is an integer and the buffer is unchanged, it is used as capf data).
3. Default behaviour, the one design question for the maintainers: either an
   option (`notmuch-address-use-capf`, default nil) so existing users of the
   minibuffer prompt (`notmuch-address-selection-function`, ido/ivy setups)
   see no change, or make the capf the default. Propose the option; ask
   about the default in the cover letter.
4. Test in `test/T*-emacs*.sh` (their Emacs test harness): the bad case, i.e.
   with a fallback capf that offers words present after it, the address
   source must answer on a To: line.

## Process

notmuch takes patches by email: `git send-email` to `notmuch@notmuchmail.org`
(see `devel/` and the notmuch website's contributing page). Run their test
suite (`make test`, or the Emacs tests only) before sending.

## Next steps when resumed

1. Search the list archive for prior capf / corfu patches.
2. Write the patch and test in a local clone (e.g. `~/storage/src/notmuch` or
   `/tmp`), run the Emacs tests.
3. Hand Joe the `git send-email` command and a draft cover letter.
