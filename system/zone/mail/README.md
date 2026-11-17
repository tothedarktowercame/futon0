# Mail on zone: mbsync + notmuch + msmtp

Since 2026-11-15 the hyperreal.enterprises mailbox (Mail-in-a-Box) is mirrored
to `~/storage/mail/hyperreal/` and indexed by notmuch; Joe reads and sends in
Emacs (notmuch.el). Configs live outside the repo: `~/.mbsyncrc`, `~/.msmtprc`,
`~/.notmuch-config`, `~/.emacs-graph/notmuch-setup.el`.

| file | what it does |
|---|---|
| `mail-first-sync.sh` | one-shot: unlock gpg, first `mbsync -a` + `notmuch new`, enable the timer. Run in a real terminal. |
| `mail-classify.py` | notmuch post-new hook (`~/storage/mail/.notmuch/hooks/post-new`): tags new mail person / list / bulk / notify / github, plus spam / sent / draft / deleted by server folder. `keep.txt` and `blocked.txt` in `~/storage/mail/` override (address or `@domain`, subdomains included). |
| `mail-unsubscribe.py` | RFC 8058 one-click unsubscribe for given senders; also adds them to `blocked.txt`; logs to `~/storage/mail/unsubscribed.tsv`. `--dry-run` first. |
| `mail-trash.sh` | move messages matching a notmuch query to the server's Trash (bound to `d` in notmuch.el); the next sync expunges the original (`Expunge Both`). Never touches Sent. |
| `../user-units/mail-sync.{service,timer}` | every 5 min: `mbsync -a && notmuch new`; if the gpg passphrase is no longer cached it skips with a notice instead of failing. |

The mailbox password comes from `pass`; gpg-agent caches the passphrase for
24 h (`~/.gnupg/gpg-agent.conf`). When it lapses, Emacs shows a notice and
asks for the passphrase on the next send, or press `U` on the notmuch hello
screen.

Deliverability notes (blocklists, bounces) and the open notmuch/corfu upstream
patch: `holes/TN-notmuch-corfu-completion-patch.md` and the bounce analysis of
2026-11-17 (Microsoft S3150 on the IPv4 address; delisting filed by Joe).
