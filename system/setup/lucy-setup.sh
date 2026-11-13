set -euo pipefail

BALDWIN_ROLE="lucy-primary"

apt-get update
DEBIAN_FRONTEND=noninteractive apt-get install -y \
  ca-certificates python3 python3-venv

python3 -m venv /opt/baldwin-linode-cli
/opt/baldwin-linode-cli/bin/pip install --upgrade pip linode-cli

install -d -o root -g root -m 0700 /etc/baldwin-supervisor
printf '%s\n' "$BALDWIN_ROLE" > /etc/baldwin-supervisor/role
chmod 0600 /etc/baldwin-supervisor/role

read -r -s -p "Paste the dedicated Lucy Linode token: " BALDWIN_LINODE_TOKEN
printf '\n'
if [ -z "$BALDWIN_LINODE_TOKEN" ]; then
  echo "Refusing empty token" >&2
  exit 1
fi
umask 077
printf '%s' "$BALDWIN_LINODE_TOKEN" > /etc/baldwin-supervisor/linode-token
unset BALDWIN_LINODE_TOKEN
chmod 0600 /etc/baldwin-supervisor/linode-token
