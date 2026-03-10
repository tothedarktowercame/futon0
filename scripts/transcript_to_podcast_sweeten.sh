#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)

EASYEFFECTS_BIN="${EASYEFFECTS_BIN:-}"
if [[ -z "$EASYEFFECTS_BIN" ]]; then
  if command -v easyeffects >/dev/null 2>&1; then
    EASYEFFECTS_BIN="easyeffects"
  elif command -v flatpak >/dev/null 2>&1; then
    EASYEFFECTS_BIN="flatpak run com.github.wwmm.easyeffects"
  fi
fi

has_preset=false
has_bin=false
for arg in "$@"; do
  case "$arg" in
    --easyeffects-preset|--easyeffects-preset=*)
      has_preset=true
      ;;
    --easyeffects-bin|--easyeffects-bin=*)
      has_bin=true
      ;;
  esac
done

cmd=(python3 "$REPO_ROOT/scripts/transcript_to_podcast.py")
cmd+=("$@")

if [[ "$has_preset" == false ]]; then
  cmd+=(--easyeffects-preset "Podcast")
fi

if [[ -n "$EASYEFFECTS_BIN" && "$has_bin" == false ]]; then
  cmd+=(--easyeffects-bin "$EASYEFFECTS_BIN")
fi

exec "${cmd[@]}"
