#!/usr/bin/env bash
set -euo pipefail

backup_root="/home/joe/code/backups"

# Sources to snapshot (absolute paths only).
sources=(
  "/home/joe/code/storage"
  "/home/joe/code/futon1/data"
)

keep_daily=7
keep_weekly=4
keep_monthly=12

usage() {
  cat <<'USAGE'
Usage: backup_snapshots.sh [--dry-run]

Creates daily snapshots and, when applicable, weekly/monthly snapshots.
Snapshots are stored under /home/joe/code/backups/{daily,weekly,monthly}.
USAGE
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
  usage
  exit 0
fi

dry_run=false
if [[ "${1:-}" == "--dry-run" ]]; then
  dry_run=true
fi

run() {
  if $dry_run; then
    echo "+ $*"
  else
    "$@"
  fi
}

list_snapshots() {
  local dir="$1"
  if [[ -d "$dir" ]]; then
    ls -1 "$dir" | sort
  fi
}

latest_snapshot() {
  local dir="$1"
  list_snapshots "$dir" | tail -n 1
}

prune_snapshots() {
  local dir="$1"
  local keep="$2"
  local items
  mapfile -t items < <(list_snapshots "$dir")
  local count=${#items[@]}
  if (( count <= keep )); then
    return
  fi
  local remove_count=$((count - keep))
  for ((i=0; i<remove_count; i++)); do
    local target="$dir/${items[i]}"
    run rm -rf -- "$target"
  done
}

sync_snapshot() {
  local kind="$1"
  local stamp="$2"
  local kind_dir="$backup_root/$kind"
  local snap_dir="$kind_dir/$stamp"

  run mkdir -p "$snap_dir"

  local prev_name
  prev_name=$(latest_snapshot "$kind_dir" || true)
  local link_arg=()
  if [[ -n "$prev_name" && "$prev_name" != "$stamp" ]]; then
    link_arg=(--link-dest="$kind_dir/$prev_name")
  fi

  local rsync_flags=(--archive --hard-links --delete --numeric-ids --relative)

  for src in "${sources[@]}"; do
    if [[ ! -e "$src" ]]; then
      echo "[backup] missing source: $src" >&2
      continue
    fi
    run rsync "${rsync_flags[@]}" "${link_arg[@]}" "$src" "$snap_dir/"
  done

  if ! $dry_run; then
    cat > "$snap_dir/backup-info.txt" <<INFO
kind: $kind
stamp: $stamp
date: $(date -u +%Y-%m-%dT%H:%M:%SZ)
sources:
$(printf '  - %s\n' "${sources[@]}")
INFO
  fi
}

run mkdir -p "$backup_root/daily" "$backup_root/weekly" "$backup_root/monthly"

stamp_daily=$(date +%F)
sync_snapshot "daily" "$stamp_daily"
prune_snapshots "$backup_root/daily" "$keep_daily"

# Weekly snapshot on Sundays.
if [[ "$(date +%u)" == "7" ]]; then
  stamp_weekly=$(date +%G-W%V)
  sync_snapshot "weekly" "$stamp_weekly"
  prune_snapshots "$backup_root/weekly" "$keep_weekly"
fi

# Monthly snapshot on first day of month.
if [[ "$(date +%d)" == "01" ]]; then
  stamp_monthly=$(date +%Y-%m)
  sync_snapshot "monthly" "$stamp_monthly"
  prune_snapshots "$backup_root/monthly" "$keep_monthly"
fi
