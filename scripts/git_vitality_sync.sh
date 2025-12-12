#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
FUTON3_ROOT=$(cd -- "$REPO_ROOT/../futon3" 2>/dev/null && pwd || true)
OUTPUT_PATH="${FUTON3_ROOT}/resources/vitality/git_activity.json"

if [[ -z "$FUTON3_ROOT" || ! -d "$FUTON3_ROOT" ]]; then
  echo "[git-vitality] futon3 repo not found next to futon0; skipping" >&2
  exit 1
fi

mkdir -p -- "$(dirname -- "$OUTPUT_PATH")"

echo "[git-vitality] updating git_activity.json via scripts/git_activity.py"
python3 "$REPO_ROOT/scripts/git_activity.py" \
  --days 60 \
  --config "$REPO_ROOT/data/git_sources.json" \
  --output "$OUTPUT_PATH"

echo "[git-vitality] regenerating futon3 vitality summary"
(
  cd -- "$FUTON3_ROOT"
  clojure -M:vitality/git-summary
)

echo "[git-vitality] refresh complete at $(date --iso-8601=seconds)"
