#!/usr/bin/env bash
# Nightly incremental off-box backup of the futon1b evidence store to both hubs.
#
#   evidence-backup.sh            back up, verify, mark; prune on Sundays
#   evidence-backup.sh --dry-run  list what would be sent, send nothing
#
# restic deduplicates and compresses, so after the first run each night sends
# only what changed: the tail appended to the XTDB log and the few new Arrow
# table files (about 0.9 GB raw a day on 2026-11-03, much less after
# compression). The hubs need nothing installed; restic talks sftp.
#
# What is copied: log/ (XTDB 2's append-only transaction log, the source of
# truth: replaying it rebuilds every table), storage/ (the table files, kept so
# a restore need not replay from scratch), write-log.edn. What is not: the
# fts5 sidecar, which is derived and safe to rebuild (futon1b README-fts.md),
# changes in place, and is 12 GB; old .bak files; XTDB's .tmp uploads.
#
# The store is live while this runs. The log is append-only, so the copy holds
# a consistent prefix. A table file compacted away mid-run makes restic exit 3
# (snapshot saved, one file unreadable); that is logged and counted as success
# because the log alone restores the data.
#
# Restore: restic -r sftp:<hub>:/home/joe/backups/evidence/restic restore latest
#          --target <dir>   (password: pass zone/evidence-backup-restic)
#
# After a snapshot passes `restic check`, ~/backups/evidence/evidence-verified
# is touched on that hub. zone-health's evidence-backup check reads its age.
set -uo pipefail

STORE="${FUTON1B_STORE_DIR:-$HOME/code/futon1b/migration-store-21}"
export RESTIC_PASSWORD_FILE="${RESTIC_PASSWORD_FILE:-$HOME/.config/evidence-backup/restic-password}"
HUBS=(${EVIDENCE_BACKUP_HUBS:-metameso lucy})   # ~/.ssh/config aliases
REMOTE_DIR=/home/joe/backups/evidence
DRY=""; [ "${1:-}" = "--dry-run" ] && DRY="--dry-run"

[ -d "$STORE/log" ] || { echo "evidence-backup: no store at $STORE" >&2; exit 1; }
[ -r "$RESTIC_PASSWORD_FILE" ] || { echo "evidence-backup: no password file" >&2; exit 1; }

log_bytes=$(stat -c %s "$STORE/log/LOG")
rc_all=0
for hub in "${HUBS[@]}"; do
  repo="sftp:$hub:$REMOTE_DIR/restic"
  echo "== $hub: backup (log $log_bytes bytes)"
  ( cd "$STORE" && restic -r "$repo" backup $DRY --compression auto \
      --host zone --tag futon1b-evidence --tag "log-bytes=$log_bytes" \
      --exclude 'storage/v06/.tmp' \
      log storage write-log.edn )
  rc=$?
  if [ $rc -eq 3 ]; then
    echo "$hub: snapshot saved, some files unreadable (compaction during run)"
  elif [ $rc -ne 0 ]; then
    echo "$hub: BACKUP FAILED rc=$rc" >&2; rc_all=1; continue
  fi
  [ -n "$DRY" ] && continue
  # A killed restic process can leave a stale repository lock. `unlock`
  # removes only locks whose owning process is no longer alive; active locks
  # remain protected. Without this, a snapshot can be saved successfully for
  # nights while every subsequent exclusive `check` fails on the old lock.
  if ! restic -r "$repo" unlock; then
    echo "$hub: STALE-LOCK CLEANUP FAILED" >&2; rc_all=1; continue
  fi
  # Read back a rotating thirtieth of the data, so a month reads all of it.
  if restic -r "$repo" check --read-data-subset="$(( $(date +%-d) % 30 + 1 ))/30"; then
    ssh -o BatchMode=yes "$hub" "touch $REMOTE_DIR/evidence-verified"
    echo "$hub: verified"
  else
    echo "$hub: CHECK FAILED" >&2; rc_all=1; continue
  fi
  if [ "$(date +%u)" = 7 ]; then
    restic -r "$repo" forget --host zone --tag futon1b-evidence \
      --keep-daily 7 --keep-weekly 4 --keep-monthly 12 --prune || rc_all=1
  fi
done
exit $rc_all
