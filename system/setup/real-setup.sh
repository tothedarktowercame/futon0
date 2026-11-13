set -euo pipefail

# Repair the invalid API request.
sed -i 's/page_size=1/page_size=25/' \
  /usr/local/sbin/baldwin-linode-auth-check

grep -F 'instances?page_size=25' \
  /usr/local/sbin/baldwin-linode-auth-check

# Install linode-cli system-wide using pipx.
apt-get update
apt-get install -y pipx

PIPX_HOME=/opt/pipx \
PIPX_BIN_DIR=/usr/local/bin \
pipx install --force linode-cli

test -x /usr/local/bin/linode-cli
/usr/local/bin/linode-cli --version

# Verify the CLI using the protected token without printing account data.
LINODE_CLI_TOKEN="$(cat /etc/baldwin-supervisor/linode-token)" \
  /usr/local/bin/linode-cli account view --json >/dev/null

echo 'PASS: linode-cli authenticated'

# Re-run the independent API preflight.
systemctl daemon-reload
systemctl start baldwin-linode-preflight.service

systemctl show baldwin-linode-preflight.service \
  -p Result -p ExecMainCode -p ExecMainStatus --no-pager

journalctl -u baldwin-linode-preflight.service \
  -n 20 --no-pager -o cat -l
```
