#!/usr/bin/env bash
# Move the messages matching a notmuch query to the server's Trash.
# The maildir file is moved into hyperreal/Trash/cur (mbsync UID stripped);
# the next mbsync uploads it to Trash and, with "Expunge Both", expunges the
# original on the server. Sent copies are never touched.
# usage: mail-trash.sh QUERY...      (bound to d in notmuch.el)
set -euo pipefail
[ $# -gt 0 ] || { echo "usage: $0 QUERY" >&2; exit 2; }
root="$HOME/storage/mail/hyperreal"
q="( $* ) and not folder:hyperreal/Sent and not folder:hyperreal/Trash"

notmuch tag +deleted -inbox -unread -- "$q"
n=0
while IFS= read -r f; do
  [ -e "$f" ] || continue
  b=$(basename "$f" | sed 's/,U=[0-9]*//')
  case "$b" in *:2,*) ;; *) b="$b:2,S" ;; esac
  mv -n -- "$f" "$root/Trash/cur/$b"
  n=$((n + 1))
done < <(notmuch search --exclude=false --output=files -- "$q")
notmuch new --quiet
echo "mail-trash: moved $n file(s) to Trash"
