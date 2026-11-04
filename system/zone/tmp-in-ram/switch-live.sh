#!/bin/bash
# Move /tmp to RAM WITHOUT a reboot. Run only with agents idle (no Codex, no test runs).
# Dry run by default; pass --go to act. Needs sudo. Prepared 2026-11-03, NOT yet run.
set -eu
GO=${1:-}
run(){ echo "+ $*"; [ "$GO" = --go ] && "$@" || true; }
OLD=/mnt/oldtmp
# 1. keep a handle on the disk /tmp
run sudo mkdir -p $OLD
run sudo mount --bind /tmp $OLD
# /tmp sits on the shared root mount: without this, the tmpfs mounted on /tmp next
# propagates onto $OLD too and hides the old files there (first attempt, 2026-11-04 16:1xZ).
run sudo mount --make-private $OLD
# 2. RAM over /tmp (new opens see the tmpfs; open files and mmaps keep the old inodes)
run sudo mount -t tmpfs -o mode=1777,strictatime,nosuid,nodev,size=64G,nr_inodes=4m tmpfs /tmp
# 3. sockets cannot be copied: bind them back (tmux sessions, Xvfb :99, JVM attach)
for d in tmux-1000 .X11-unix; do
  run mkdir -p /tmp/$d; run sudo mount --bind $OLD/$d /tmp/$d
done
for s in $OLD/.java_pid*; do [ -e "$s" ] || continue
  run touch /tmp/$(basename $s); run sudo mount --bind $s /tmp/$(basename $s); done
# 4. copy live state (Agency files, seat session ids, bg logs, Claude task outputs, perf data)
run bash -c "cd $OLD && cp -a futon* .futon* invoke-trace.log promise-chains* substrate2-byns-* futon3c-bg futon-bounded-tests claude-1000 hsperfdata_joe sqlite-*libsqlitejdbc.so emacs* /tmp/ 2>/dev/null; true"
# 5. check: the sockets must be visible again, and the old handle must still be disk
run df -h /tmp
run ls /tmp/tmux-1000 /tmp/.X11-unix
run findmnt -R $OLD
echo "Afterwards: make it permanent for boots with tmp.mount (see README); the old disk /tmp"
echo "stays reachable at $OLD for cleanup (rm fixtures there, then umount $OLD)."
