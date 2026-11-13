set -euo pipefail

LINODE_CLI_TOKEN="$(cat /etc/baldwin-supervisor/linode-token)" \
  /usr/local/bin/linode-cli linodes list --json >/dev/null

echo 'PASS: linode-cli authenticated with Linode access'

systemctl start baldwin-linode-preflight.service

systemctl show baldwin-linode-preflight.service \
  -p Result -p ExecMainCode -p ExecMainStatus --no-pager

journalctl -u baldwin-linode-preflight.service \
  -n 20 --no-pager -o cat -l
