# /tmp in RAM: APPLIED 2026-11-04 ~16:20Z, live (claude-16 + Joe)

Why: /tmp takes the test fixtures (48,604 entries, ~25 GB on 11-03) and most of the
futon3c JVM's own file writes (followup queue, invoke-jobs journal, promise chains).
The workspace rule already treats /tmp as emptied at every boot, so RAM changes no
meaning. The box has 251 GB of RAM, ~210 GB available.

## What lives in /tmp that matters
- Sockets (cannot be copied): `tmux-1000/default` (Joe's tmux sessions e1–e5),
  `.X11-unix/X99` (Xvfb for noVNC), `.java_pid*` (jcmd attach to the JVMs).
- Agency state, opened by path each write: `futon3c-invoke-jobs.edn` (77 MB) + `.journal`,
  `futon3c-followups.edn`, `futon3c-parked-on.edn`, `futon3c-promise-history-chains.edn`,
  `futon-bell.edn`, `invoke-trace.log`, `promise-chains*`, `substrate2-byns-*`.
- Seat session ids: 78 `futon-*session-id*` files (seat continuity across invokes).
- `futon3c-bg/` (bg.py job logs), `futon-bounded-tests/`, `claude-1000/` (Claude Code
  task outputs, written through open files by running sessions).
- The JVM's extracted `sqlite-*-libsqlitejdbc.so` (mapped; stays valid) and
  `hsperfdata_joe/` (mapped).

## Two routes
1. **At the next reboot (clean).** Install the drop-in and enable the unit:
   `sudo install -D -m 644 tmp.mount.d-size.conf /etc/systemd/system/tmp.mount.d/size.conf`,
   `sudo install -m 644 tmpfiles-tmp.conf /etc/tmpfiles.d/tmp.conf`,
   `sudo systemctl enable tmp.mount` (template: /usr/share/systemd/tmp.mount; may need
   `sudo cp /usr/share/systemd/tmp.mount /etc/systemd/system/` first). Nothing to migrate:
   a reboot empties /tmp anyway. Do NOT `systemctl start tmp.mount` on a running box.
2. **Live, without a reboot.** `switch-live.sh` (dry run by default; `--go` to act):
   bind the disk /tmp to /mnt/oldtmp, mount the tmpfs over /tmp, bind the socket dirs
   back, copy the state files. Run only with agents idle: a running test loses its
   fixture dir, and a running Claude session keeps writing task output to the old copy.
   Then do route 1 as well so the next boot keeps it.

## After either route
- `tmpfiles-tmp.conf` ages out anything older than 3 days except the state above,
  so fixtures cannot fill RAM between reboots.
- Watch `df -h /tmp`; the 64G size is a ceiling, not a reservation.
- Old disk /tmp (route 2): clean through /mnt/oldtmp, then `sudo umount /mnt/oldtmp`.

## Applied 2026-11-04
- First live run failed: /tmp sits on the shared root mount, so the tmpfs propagated onto
  /mnt/oldtmp and hid the old files; the socket dirs were bound back empty. Undone with
  `umount -l /tmp` within minutes (tmux, X99 and Agency checked afterwards). Agency writes
  made during those minutes went to the detached tmpfs and are gone.
- Fixed in the script (`mount --make-private /mnt/oldtmp`), tested on a scratch dir, re-run:
  tmpfs on /tmp (404 MB copied), sockets bound from disk, Agency 200, X99 ok, jcmd attach
  ok, codex-19 resumed its session.
- Route 1 installed too: /etc/systemd/system/tmp.mount (+ tmp.mount.d/size.conf) enabled,
  /etc/tmpfiles.d/tmp.conf installed.
- Still to do: the old disk /tmp (~25 GB, 49,092 entries, mostly fixtures) is reachable
  only at /mnt/oldtmp; delete there, then `sudo umount /mnt/oldtmp`. Do not delete
  `tmux-1000/`, `.X11-unix/` or `.java_pid*` there: the live sockets are bound from them
  until the next reboot (the binds are separate mounts, so unmounting /mnt/oldtmp is fine).
