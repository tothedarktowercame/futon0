set -euo pipefail

cat > /usr/local/sbin/baldwin-linode <<'SH'
#!/bin/sh
set -eu

credential=/etc/baldwin-supervisor/linode-token
test -r "$credential"

LINODE_CLI_TOKEN="$(cat "$credential")"
test -n "$LINODE_CLI_TOKEN"
export LINODE_CLI_TOKEN

exec /opt/baldwin-linode-cli/bin/linode-cli "$@"
SH
chmod 0755 /usr/local/sbin/baldwin-linode

cat > /usr/local/sbin/baldwin-linode-auth-check <<'PY'
#!/usr/bin/env python3
import json
import pathlib
import urllib.request

root = pathlib.Path("/etc/baldwin-supervisor")
role = (root / "role").read_text().strip()
token = (root / "linode-token").read_text().strip()

if not token:
    raise SystemExit("FAIL: token is empty")

request = urllib.request.Request(
    "https://api.linode.com/v4/linode/instances?page_size=1",
    headers={
        "Authorization": f"Bearer {token}",
        "Accept": "application/json",
        "User-Agent": "baldwin-supervisor-preflight/1",
    },
)

with urllib.request.urlopen(request, timeout=20) as response:
    if response.status != 200:
        raise SystemExit(f"FAIL: API returned HTTP {response.status}")

    scopes = response.headers.get("X-OAuth-Scopes", "")
    payload = json.load(response)

scope_items = {item.strip() for item in scopes.split(",")}
if "*" not in scope_items and "linodes:read_write" not in scope_items:
    raise SystemExit(
        f"FAIL: {role}: token lacks linodes:read_write; reported scopes={scopes!r}"
    )

if "data" not in payload:
    raise SystemExit(f"FAIL: {role}: malformed Linode list response")

print(f"PASS: role={role}")
print(f"PASS: scopes={scopes}")
print("PASS: authenticated Linode API list request")
PY
chmod 0755 /usr/local/sbin/baldwin-linode-auth-check

cat > /etc/systemd/system/baldwin-linode-preflight.service <<'UNIT'
[Unit]
Description=Baldwin Linode credential and network preflight
Wants=network-online.target
After=network-online.target

[Service]
Type=oneshot
User=root
ExecStart=/usr/local/sbin/baldwin-linode-auth-check
NoNewPrivileges=true
PrivateTmp=true
ProtectHome=true

[Install]
WantedBy=multi-user.target
UNIT

systemctl daemon-reload
systemctl start baldwin-linode-preflight.service
systemctl --no-pager --full status baldwin-linode-preflight.service

/usr/local/sbin/baldwin-linode linodes list --json |
python3 -c '
import json, sys
for item in json.load(sys.stdin):
    print(item["id"], item["label"], item["status"])
'

stat -c '%U %G %a %n' \
  /etc/baldwin-supervisor \
  /etc/baldwin-supervisor/role \
  /etc/baldwin-supervisor/linode-token
```
