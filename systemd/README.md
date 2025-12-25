# Zoom Sync systemd units

Copy the user-scoped unit files into `~/.config/systemd/user/` (or symlink
them) to trigger `zoom_sync.py` automatically whenever the Zoom R4 appears
under `/dev/disk/by-uuid/6970-2CEF`.

```bash
mkdir -p ~/.config/systemd/user
cp systemd/user-zoom-mount.service ~/.config/systemd/user/zoom-mount.service
cp systemd/user-zoom-mount.path ~/.config/systemd/user/zoom-mount.path
cp systemd/user-zoom-sync.service ~/.config/systemd/user/zoom-sync.service

systemctl --user daemon-reload
systemctl --user enable --now zoom-mount.path
```

`zoom-mount.path` watches for the SD card's UUID and ensures the
`zoom-mount.service` oneshot mounts the card once per insertion. The user
service creates `/media/joe/R4_SD` as a stable symlink even if `udisksctl`
chooses a suffixed mount point (for example `/media/joe/R4_SD1`). Once mounted,
the mount service kicks off `zoom-sync.service` for a single ingest run. After
the sync finishes it unmounts the card automatically; replugging mounts a new
instance and triggers another run. Disable the automation via
`systemctl --user disable --now zoom-mount.path` if you need to stop auto-ingest
temporarily.

## Automation contracts

These contracts define the intended behavior so future debugging can validate
each step independently.

- Mount contract: plugging in the Zoom R4 triggers `zoom-mount.path`, and
  `zoom-mount.service` mounts the card and exposes a stable mount path at
  `/media/joe/R4_SD` (or a symlink to the udisks mount).
- Trigger contract: a successful mount triggers exactly one sync run. The
  trigger is owned by `zoom-mount.service` (ExecStartPost).
- Sync contract: `zoom-sync.service` runs `scripts/zoom_sync.py` against the
  mounted card and only ingests new/updated files (old content is skipped via
  the catalog fingerprints).
- Progress contract: sync emits notifications and logs all output to
  `~/code/storage/zoomr4/meta/zoom_sync.log` (override with `ZOOM_SYNC_RUN_LOG`).
- Unmount contract: after sync completes, the card is unmounted automatically
  by `zoom-sync.service`.
- No-loop contract: the sync does not rerun until the card is unplugged and
  replugged.

## Debug checklist

If any contract fails, start here:

- Mount logs: `journalctl --user -u zoom-mount.service -b --no-pager | tail -n 80`
- Trigger logs: `journalctl --user -u zoom-mount.service -b --no-pager | tail -n 80`
- Sync logs: `journalctl --user -u zoom-sync.service -b --no-pager | tail -n 80`
- File log: `tail -n 200 /home/joe/code/storage/zoomr4/meta/zoom_sync.log`

## Git vitality timer

Install `user-git-vitality.service` + `.timer` the same way to refresh the
git-activity HUD snapshot every hour:

```bash
mkdir -p ~/.config/systemd/user
cp systemd/user-git-vitality.service ~/.config/systemd/user/git-vitality.service
cp systemd/user-git-vitality.timer ~/.config/systemd/user/git-vitality.timer

systemctl --user daemon-reload
systemctl --user enable --now git-vitality.timer
```

The timer calls `scripts/git_vitality_sync.sh`, which reruns the git-activity
Python helper followed by `clojure -M:vitality/git-summary` inside `../futon3`
so the HUD JSON stays fresh without manual steps.

## Vitality scanner timer

Use the `user-vitality-scanner.service` + `.timer` pair when you want the
filesystem/Tatami scan to publish straight into Futon3 every hour:

```bash
mkdir -p ~/.config/systemd/user
cp systemd/user-vitality-scanner.service ~/.config/systemd/user/vitality-scanner.service
cp systemd/user-vitality-scanner.timer ~/.config/systemd/user/vitality-scanner.timer

systemctl --user daemon-reload
systemctl --user enable --now vitality-scanner.timer
```

The service runs the Clojure scanner (`scripts/futon0/vitality/scanner.clj`) and writes
straight into `../futon3/resources/vitality/latest_scan.json`, so the Stack HUD
always sees a fresh `latest_scan.json` without manual copies. Adjust
`OnCalendar` inside the timer if you want a different cadence.

For more aggressive updates, enable the optional path unit so changes inside
`~/code/futon0`–`~/code/futon7` trigger a scan (throttled to once every 30s):

```bash
cp systemd/user-vitality-scanner.path ~/.config/systemd/user/vitality-scanner.path
systemctl --user daemon-reload
systemctl --user enable --now vitality-scanner.path
```

## Negative-space reminders

Four optional timers emit desktop reminders (via `notify-send`) so Tai Chi
departures and nightly shutdowns stay on track. Install them like any other
user unit:

```bash
mkdir -p ~/.config/systemd/user
cp systemd/user-negative-tai-chi-30.service ~/.config/systemd/user/negative-tai-chi-30.service
cp systemd/user-negative-tai-chi-30.timer ~/.config/systemd/user/negative-tai-chi-30.timer
cp systemd/user-negative-tai-chi-10.service ~/.config/systemd/user/negative-tai-chi-10.service
cp systemd/user-negative-tai-chi-10.timer ~/.config/systemd/user/negative-tai-chi-10.timer
cp systemd/user-negative-curfew-30.service ~/.config/systemd/user/negative-curfew-30.service
cp systemd/user-negative-curfew-30.timer ~/.config/systemd/user/negative-curfew-30.timer
cp systemd/user-negative-curfew-10.service ~/.config/systemd/user/negative-curfew-10.service
cp systemd/user-negative-curfew-10.timer ~/.config/systemd/user/negative-curfew-10.timer

systemctl --user daemon-reload
systemctl --user enable --now \
  negative-tai-chi-30.timer \
  negative-tai-chi-10.timer \
  negative-curfew-30.timer \
  negative-curfew-10.timer
```

- `negative-tai-chi-*` fire at 18:30 and 18:50 on Wednesdays/Thursdays so you
  remember to leave in time for the 19:00 class.
- `negative-curfew-*` fire at 20:30 and 20:50 every night to nudge you toward a
  21:00 shutdown.

All four units call `scripts/negative_space_notify.py`, which falls back to
stdout when `notify-send` is unavailable.
