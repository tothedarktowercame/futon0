#!/data/data/com.termux/files/usr/bin/sh
# Teletype demo: one keyboard, two phones (README-teletype.md in futon0).
#   1. on the monitor-2 phone:   sh tt-demo.sh
#   2. on the keyboard phone:    sh tt-demo.sh keyboard
# Type on the keyboard phone; it appears on the other.  C-] stops relaying,
# C-x C-c closes a frame.  Runs on lucy's Emacs, via metameso.
MODE=${1:-screen}
exec ssh -t -p 2222 joe@172.236.108.82 ssh -t lucy bin/tt-frame "$MODE"
