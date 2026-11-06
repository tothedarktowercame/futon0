#!/usr/bin/env bash
set -euo pipefail

ROOT=$(mktemp -d)
trap 'rm -rf "$ROOT"' EXIT
mkdir -p "$ROOT/bin" "$ROOT/store/log" "$ROOT/home/.config/evidence-backup"
printf 'log' > "$ROOT/store/log/LOG"
printf 'password' > "$ROOT/home/.config/evidence-backup/restic-password"

cat > "$ROOT/bin/restic" <<'EOF'
#!/usr/bin/env bash
set -eu
printf '%s\n' "$*" >> "$TEST_LOG"
case " $* " in
  *' backup '*) exit 0 ;;
  *' unlock '*) : > "$UNLOCKED"; exit 0 ;;
  *' check '*) test -f "$UNLOCKED" ;;
  *) exit 70 ;;
esac
EOF
cat > "$ROOT/bin/ssh" <<'EOF'
#!/usr/bin/env bash
set -eu
printf 'ssh %s\n' "$*" >> "$TEST_LOG"
EOF
chmod +x "$ROOT/bin/restic" "$ROOT/bin/ssh"

export HOME="$ROOT/home"
export PATH="$ROOT/bin:$PATH"
export FUTON1B_STORE_DIR="$ROOT/store"
export EVIDENCE_BACKUP_HUBS="test-hub"
export TEST_LOG="$ROOT/commands.log"
export UNLOCKED="$ROOT/unlocked"

"$(dirname "$0")/evidence-backup.sh" > "$ROOT/output.log"

unlock_line=$(grep -n ' unlock$' "$TEST_LOG" | cut -d: -f1)
check_line=$(grep -n ' check --read-data-subset=' "$TEST_LOG" | cut -d: -f1)
test -n "$unlock_line"
test -n "$check_line"
test "$unlock_line" -lt "$check_line"
grep -q '^test-hub: verified$' "$ROOT/output.log"
echo 'evidence-backup stale-lock ordering: PASS'
