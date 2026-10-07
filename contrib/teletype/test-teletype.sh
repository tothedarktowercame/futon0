#!/usr/bin/env bash
# Headless test for teletype.el: two tmux sessions stand in for the two phones,
# both emacsclient -t frames on a private Emacs daemon started with -Q.
# Usage: contrib/teletype/test-teletype.sh   (needs emacs and tmux; touches nothing else)
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
D=teletype-test-$$          # daemon / socket name
W=$(mktemp -d)
A="tmux send-keys -t ${D}A"
fail=0
check() {  # name expected actual
  if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1"; echo "     want: $2"; echo "     got:  $3"; fail=1; fi
}
line() { tmux capture-pane -p -t "${D}B" | sed -n "$1p"; }
cleanup() {
  emacsclient -s "$D" -e '(kill-emacs)' >/dev/null 2>&1
  tmux kill-session -t "${D}A" 2>/dev/null; tmux kill-session -t "${D}B" 2>/dev/null
  rm -rf "$W"
}
trap cleanup EXIT

emacs -Q --daemon="$D" -l "$HERE/teletype.el" >/dev/null 2>&1
tmux new-session -d -s "${D}B" -x 70 -y 12 "TERM=xterm-256color emacsclient -s $D -t"; sleep 1.5
tmux send-keys -t "${D}B" C-x b n o t e s Enter; sleep 0.7       # phone B shows buffer "notes"
tmux new-session -d -s "${D}A" -x 70 -y 12 "TERM=xterm-256color emacsclient -s $D -t"; sleep 1.5
$A Escape x; sleep 0.3; $A -l teletype-connect; $A Enter; sleep 0.8

$A -l 'hello from phone A'; $A Enter; $A -l 'second line'; sleep 0.3
$A C-a; $A -l '> '; sleep 0.8
check "typing and C-a reach B"            '> second line'      "$(line 3)"

$A Escape x; sleep 0.8; $A -l upcase-word; $A Enter; sleep 0.8
check "M-x prompts on A, acts on B"       '> SECOND line'      "$(line 3)"

$A 'M-<'; sleep 0.4; $A Escape %; sleep 0.8; $A -l o; $A Enter; sleep 0.5; $A -l 0; $A Enter; sleep 0.8
$A -l y; sleep 0.5; $A -l n; sleep 0.5; $A -l '!'; sleep 0.8
check "query-replace answers y n !"       'hell0 from ph0ne A' "$(line 2)"

$A C-x; sleep 2; $A C-w; sleep 0.8; $A -l "$W/relayed.txt"; $A Enter; sleep 1
check "C-x, pause, C-w writes B's buffer" 'hell0 from ph0ne A|> SEC0ND line|' "$(tr '\n' '|' < "$W/relayed.txt" 2>/dev/null)"

$A Up; sleep 0.3; $A -l '^'; sleep 0.6
check "arrow keys decode (<up>)"          'hell0 ^from ph0ne A' "$(line 2)"

$A C-]; sleep 0.4; $A -l local; sleep 0.4
check "C-] stops relaying"                'hell0 ^from ph0ne A' "$(line 2)"

[ $fail = 0 ] && echo "all teletype tests passed" || echo "teletype tests FAILED"
exit $fail
