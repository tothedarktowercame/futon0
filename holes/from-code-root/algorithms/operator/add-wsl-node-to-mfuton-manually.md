# Algorithm: Add WSL Node To mfuton Manually

Date: 2026-03-13
Status: active

## Purpose

Use this when an operator needs to add a local WSL2 Ubuntu instance as a
governed Unix service node for `mfuton`, using a manual bootstrap sequence
that:

- proves the real Windows-to-WSL transport seam
- provisions the dedicated node-control account
- establishes the canonical SSH alias
- and then returns control to the governed `agent_skills/wsl-node/` surface

This algorithm is intentionally operator-run. It is written for future
operators of other `mfuton` installations, not only this machine.

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/system-architecture/cross-platform-execution-modes-and-authority.flexiarg`
  - governs the authority split between `unix`, `windows-native`, and
    `windows-wsl` execution modes that this bootstrap lane must preserve
- `library/system-architecture/distributed-resource-planes-peer-collaboration-and-work-protocols.flexiarg`
  - governs classification of the WSL instance as a governed service-node
    resource extension rather than a replacement for the main installation
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the existing `agent_skills/wsl-node/` lane rather than
    inventing a parallel node-control surface

Inspected but non-owning candidates:
- `library/social/dual-os-script-surfaces-with-unix-authority.flexiarg`
  - relevant to script-parity posture, but not the direct owner of WSL
    node-bootstrap semantics

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Inputs

- a Windows workstation with `mfuton` checked out
- a WSL2 distro intended to become the node
- operator shell access on Windows
- some shell or agent access inside the target WSL distro
  - this may be direct shell access
  - or another local agent/session already running inside WSL
- the governed WSL skill family under:
  - `agent_skills/wsl-node/`

## Preconditions

1. The WSL node-addition lane is owned by:
   - `holes/missions/M-windows-wsl-service-node-hybrid-runtime/M-wsl-ssh-bootstrap-and-wrapper-surface.md`
2. The operator intends to use WSL as a governed `windows-wsl` service node,
   not to claim native Windows parity.
3. The operator can run PowerShell on Windows and shell commands inside the WSL
   distro.
4. The operator has `OpenSSH Client` on Windows and can install/enable
   `openssh-server` inside WSL if it is missing.

## Procedure

1. Load the current preferred identity defaults from the WSL contract note.
   - Preferred-default owner:
     - `docs/M-windows-wsl-service-node-hybrid-runtime/wrapper-and-service-contract.md`
   - Unless the installation already has an explicit local override, use the
     current preferred defaults:
     - node alias: `wsl-futon`
     - dedicated node-control account: `mfuton_nodectl`
     - Windows SSH key basename: `wsl_futon_nodectl_ed25519`
   - Use one consistent variable set through the run:

```powershell
$NodeAlias = "wsl-futon"
$NodectlUser = "mfuton_nodectl"
$KeyBaseName = "wsl_futon_nodectl_ed25519"
```

   - These values may be changed deliberately, but do not improvise multiple
     aliases or reuse an unrelated GitHub SSH identity.

2. Inspect the actual WSL target from Windows before touching SSH config.
   - Run:

```powershell
wsl.exe -l -v
```

   - Choose the intended distro name from the real output.
   - Stop if no suitable WSL2 distro exists yet.

3. Read the initial node fact bundle through `wsl.exe`, not speculative SSH.
   - From Windows, run:

```powershell
$Distro = "Ubuntu-24.04"
wsl.exe -d $Distro -- /bin/sh -lc 'printf "hostname=%s\nuser=%s\ndistro=%s\n" "$(hostname)" "$(whoami)" "${WSL_DISTRO_NAME:-unknown}"'
```

   - Record:
     - hostname
     - current bootstrap/operator user inside the distro
     - distro marker

4. Check whether `openssh-server` is already present and active inside WSL.
   - Inside the target WSL distro, run:

```bash
command -v sshd
systemctl is-enabled ssh
systemctl is-active ssh
ss -ltn | grep ':22'
```

   - If `sshd` is missing, install and enable it:

```bash
sudo apt-get update
sudo apt-get install -y openssh-server
sudo systemctl enable ssh
sudo systemctl start ssh
```

   - Re-run the presence and listener checks.
   - Stop if the SSH service still cannot be enabled or started; classify that
     blocker explicitly instead of continuing with alias setup.

5. Verify the Windows-side target before creating the long-term alias.
   - Run from Windows:

```powershell
Test-NetConnection 127.0.0.1 -Port 22 | Select-Object ComputerName,RemotePort,TcpTestSucceeded
ssh.exe -p 22 -o BatchMode=yes -o NumberOfPasswordPrompts=0 -o StrictHostKeyChecking=no -o UserKnownHostsFile=NUL <bootstrap-user>@127.0.0.1 /bin/sh -lc "hostname"
```

   - Expected intermediate result:
     - TCP reachability succeeds
     - SSH reaches the server and may fail at authentication
   - Treat that as success for host/port discovery.
   - Stop if the Windows side cannot reach `127.0.0.1:22`.

6. Generate the dedicated Windows SSH key for this node.
   - Run from Windows PowerShell:

```powershell
$OperatorSshDir = "$env:USERPROFILE/.ssh"
$KeyBase = "$OperatorSshDir/$KeyBaseName"
if (-not (Test-Path $KeyBase)) {
  ssh-keygen -t ed25519 -N "" -f $KeyBase -C $NodeAlias
}
Get-Content "$KeyBase.pub"
```

   - Expected outputs:
     - `<operator-home>/.ssh/wsl_futon_nodectl_ed25519`
     - `<operator-home>/.ssh/wsl_futon_nodectl_ed25519.pub`
   - This key is for the `wsl-futon` node only.

7. Provision the dedicated node-control user inside WSL.
   - Inside the target WSL distro, run:

```bash
id -u "$NODECTL_USER" >/dev/null 2>&1 || sudo useradd -m -s /bin/bash "$NODECTL_USER"
getent passwd "$NODECTL_USER"
```

   - Stop if the user cannot be created or does not get a normal home
     directory.
   - Before running that snippet, set `NODECTL_USER` in the WSL shell to the
     chosen node-control username if needed.

8. Authorize the dedicated public key for the node-control user.
   - Inside WSL, run:

```bash
sudo -u "$NODECTL_USER" mkdir -p "/home/$NODECTL_USER/.ssh"
sudo chmod 700 "/home/$NODECTL_USER/.ssh"
sudo touch "/home/$NODECTL_USER/.ssh/authorized_keys"
sudo chmod 600 "/home/$NODECTL_USER/.ssh/authorized_keys"
sudo chown -R "$NODECTL_USER:$NODECTL_USER" "/home/$NODECTL_USER/.ssh"
grep -qxF "$(cat /mnt/c/Users/$WIN_USER/.ssh/$KEY_BASENAME.pub)" "/home/$NODECTL_USER/.ssh/authorized_keys" || cat "/mnt/c/Users/$WIN_USER/.ssh/$KEY_BASENAME.pub" | sudo tee -a "/home/$NODECTL_USER/.ssh/authorized_keys" >/dev/null
sudo chown "$NODECTL_USER:$NODECTL_USER" "/home/$NODECTL_USER/.ssh/authorized_keys"
ls -ld "/home/$NODECTL_USER" "/home/$NODECTL_USER/.ssh"
ls -l "/home/$NODECTL_USER/.ssh/authorized_keys"
```

   - Before running that snippet, set `WIN_USER` in the WSL shell to the
     Windows operator username and `KEY_BASENAME` / `NODECTL_USER` to the
     chosen values if needed.
   - Stop if file ownership or permissions are wrong after this step.

9. Write the canonical Windows SSH alias.
   - Add this stanza to the operator's Windows SSH config:

```text
Host <node-alias>
    HostName 127.0.0.1
    Port 22
    User <node-control-user>
    IdentityFile C:/Users/<operator>/.ssh/<key-basename>
    IdentitiesOnly yes
    PreferredAuthentications publickey
    HostKeyAlias <node-alias>
```

   - Replace:
     - `<node-alias>` with the chosen node alias
     - `<node-control-user>` with the chosen node-control username
     - `<key-basename>` with the chosen key basename
     - `<operator>` with the actual Windows operator username
   - Do not keep using ad hoc `ssh.exe <bootstrap-user>@127.0.0.1 ...` calls
     once this alias exists.

10. Establish durable host trust under the alias.
    - Run from Windows:

```powershell
ssh.exe -o StrictHostKeyChecking=accept-new $NodeAlias /bin/sh -lc "hostname && whoami"
```

    - Expected result:
      - hostname of the WSL node
      - the chosen node-control username
    - Stop if this still fails; classify the exact failure:
      - key not authorized
      - wrong identity file
      - config parse problem
      - host-key mismatch

11. Return to the governed `mfuton` control surface.
    - From the `gh/mfuton` repo root, run:

```powershell
cmd /c "agent_skills\wsl-node\wsl-status.bat --json"
```

    - Expected result:
      - no `alias-missing`
      - no `Permission denied (publickey,password)`
      - identity fields reach the node through `wsl-futon`

12. Stop the node-addition algorithm at the service-node boundary.
    - Once `wsl-status` succeeds through the alias, treat the WSL node as
      added to `mfuton`.
    - Further hosted-service work such as `GitLab` bring-up belongs to the
      next mission/algorithm lane, not to this algorithm.

## Outputs

- one governed WSL node alias:
  - the chosen node alias
- one dedicated node-control account:
  - the chosen node-control account
- one dedicated Windows SSH key for that node
- one successful `agent_skills/wsl-node/wsl-status.bat --json` run through the
  alias
- a usable `windows-wsl` node-control surface for later hosted services

## Stop Conditions

Stop and classify the blocker if any of these happen:
- no suitable WSL2 distro exists
- `openssh-server` cannot be installed or started
- Windows cannot reach `127.0.0.1:22`
- the dedicated node-control user cannot be provisioned
- `authorized_keys` ownership or permissions remain wrong
- the canonical alias still fails after key authorization

## References

- `holes/missions/M-windows-wsl-service-node-hybrid-runtime/M-wsl-ssh-bootstrap-and-wrapper-surface.md`
- `docs/M-windows-wsl-service-node-hybrid-runtime/node-topology-and-first-pass-design.md`
- `docs/M-windows-wsl-service-node-hybrid-runtime/wrapper-and-service-contract.md`
- `agent_skills/wsl-node/wsl-status.bat`
- `agent_skills/wsl-node/wsl-exec.bat`
- `library/system-architecture/cross-platform-execution-modes-and-authority.flexiarg`
- `library/social/dual-os-script-surfaces-with-unix-authority.flexiarg`
