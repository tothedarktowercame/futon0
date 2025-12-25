#!/usr/bin/env bash
set -euo pipefail

uuid="6970-2CEF"
device="/dev/disk/by-uuid/${uuid}"
stamp="/run/user/$(id -u)/zoom-sync.present"

if [ ! -e "$device" ]; then
  rm -f "$stamp"
  exit 0
fi

if [ -f "$stamp" ]; then
  exit 0
fi

echo "present" > "$stamp"
exec /usr/bin/systemctl --user start --no-block zoom-sync.service
