#!/usr/bin/env bash
# One-shot: unlock gpg, first mbsync + notmuch index, enable the sync timer.
# Run in a real terminal (pinentry needs a TTY). Safe to re-run.
# Configs: ~/.mbsyncrc ~/.msmtprc ~/.notmuch-config,
# tagging hook ~/storage/mail/.notmuch/hooks/post-new,
# units futon0/system/zone/user-units/mail-sync.{service,timer}.
set -euo pipefail

ENTRY=firefox/hyperreal.enterprises/joseph.corneli@hyperreal.enterprises
step() { printf '\n== %s\n' "$*"; }

[ -t 0 ] || { echo "Needs a terminal (pinentry). Run it in a shell, not via an agent." >&2; exit 1; }
export GPG_TTY=$(tty)

step "Unlocking gpg-agent (cached for 24 h)"
pass show "$ENTRY" >/dev/null

step "Checking login and listing server folders"
folders=$(mbsync -l hyperreal)
printf '%s\n' "$folders"
grep -qx 'Sent' <<<"$folders" || \
  echo "WARNING: no folder named 'Sent'; fix notmuch-fcc-dirs in ~/.emacs-graph/notmuch-setup.el"

step "First full sync (can take a while)"
mbsync -a

step "Indexing"
notmuch new

step "Enabling mail-sync.timer (every 5 min)"
systemctl --user daemon-reload
systemctl --user enable --now mail-sync.timer
systemctl --user list-timers mail-sync.timer --no-pager

step "Counts"
for q in '*' tag:inbox 'tag:inbox and tag:unread' tag:github tag:sent tag:spam; do
  printf '%-28s %s\n' "$q" "$(notmuch count "$q")"
done
echo
echo "Done. In Emacs: M-x notmuch"
