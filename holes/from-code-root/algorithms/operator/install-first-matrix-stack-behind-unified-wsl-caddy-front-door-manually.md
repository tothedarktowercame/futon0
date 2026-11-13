# Algorithm: Install First Matrix Stack Behind Unified WSL Caddy Front Door Manually

Date: 2026-03-18
Status: active

## Purpose

Use this when the operator needs to perform only the privileged WSL packet for
the first local Matrix-backed bringup slice under the multicast-and-virtual-
space component.

This algorithm is intentionally operator-executed:
- it contains only the operator-owned privileged steps for the current bounded
  first-slice local posture recorded in this artifact
- it stops by returning control to the agent for the non-privileged
  configuration, routing checks, and receipt writeback

This algorithm does not own the full bringup by itself. It does not replace:
- the Matrix child mission's service contract
- the governed WSL node-control lane
- the agent-side preparation or validation work

## Companion Flexiarg Resolution (2026-03-18)

Governing companion flexiargs for this algorithm:
- `library/system-architecture/cross-platform-execution-modes-and-authority.flexiarg`
  - governs the authority split between Windows-side agent control, WSL-side
    privileged installation, and the borrowed-node posture used here
- `library/system-architecture/distributed-resource-planes-peer-collaboration-and-work-protocols.flexiarg`
  - governs classification of this Matrix stack as a familiar component hosted
    on a governed distributed resource plane rather than as a separate
    installation
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the current WSL node-control lane, existing GitLab
    service, and one-web-server front-door posture rather than inventing a
    second parallel web-serving story

Inspected but non-owning candidates:
- `library/social/dual-os-script-surfaces-with-unix-authority.flexiarg`
  - relevant to Windows/WSL script posture, but not the direct owner of the
    privileged packet boundary

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Authority routing

- governing Matrix child mission:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-first-matrix-backed-room-widget-and-agent-bringup.md`
- current exact install packet:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-first-matrix-backed-room-widget-and-agent-bringup/first-local-install-packet-2026-03-18.md`
- current exact Conduit config:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-first-matrix-backed-room-widget-and-agent-bringup/first-conduit.toml`
- current exact Caddy config:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component/M-first-matrix-backed-room-widget-and-agent-bringup/first-Caddyfile`
- governing parent component mission:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-multicast-and-virtual-space-component.md`
- governed WSL node-control mission:
  `holes/missions/M-windows-wsl-service-node-hybrid-runtime/M-wsl-ssh-bootstrap-and-wrapper-surface.md`
- existing GitLab bringup/service-contract mission:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-first-wsl-hosted-gitlab-bringup-and-service-contract.md`
- governed node-control contract:
  `docs/M-windows-wsl-service-node-hybrid-runtime/wrapper-and-service-contract.md`
- official Conduit deployment docs:
  `https://docs.conduit.rs/deploying/generic.html`
- official Conduit Debian-package docs:
  `https://docs.conduit.rs/deploying/debian.html`
- official Conduit configuration docs:
  `https://docs.conduit.rs/configuration.html`
- official Conduit delegation docs:
  `https://docs.conduit.rs/delegation.html`
- official Caddy runtime docs:
  `https://caddyserver.com/docs/running`
- official Caddy Caddyfile docs:
  `https://caddyserver.com/docs/caddyfile`
- official GitLab NGINX settings docs:
  `https://docs.gitlab.com/omnibus/settings/nginx/`
- official GitLab configuration docs:
  `https://docs.gitlab.com/omnibus/settings/configuration/`

## Inputs

- a live governed WSL node-control lane:
  - SSH alias `wsl-futon`
  - WSL node-control account `mfuton_nodectl`
- an operator-owned privileged Unix account on the target WSL node
  - this is the account used for `sudo`
  - it is intentionally distinct from `mfuton_nodectl` unless the
    installation explicitly grants sudo to the control account
- an operator who can use elevated privilege on the WSL node
- the current WSL private-network service address
  - current local observed value: `192.168.165.188`

## Preconditions

1. The governed WSL node-control lane is live and usable.
2. The operator has a Unix account on the target WSL node that can perform
   privileged install and reconfiguration steps with `sudo`.
3. The target WSL distro is running with `systemd` as PID 1.
   - this algorithm uses `systemctl` as a real service-control boundary
   - do not proceed on a WSL distro where `systemd` is disabled or replaced
4. The current first-slice local posture is still:
   - one WSL-hosted managed edge web server
   - `GitLab` at `/`
   - Matrix under `/_matrix/*`
   - widget assets under `/widgets/matrix/`
5. The unresolved long-term privilege question for agent-driven WSL package
   install is still deferred.
   - this algorithm is the current bounded workaround
   - do not silently treat it as settlement of the later privilege model
6. The privileged WSL shell already has `curl` available.
   - this algorithm assumes `curl` is present rather than installing it
   - if `curl` is missing, stop and hand control back to the agent instead of
     improvising a package-install side path here

## Fixed local service contract for this first slice

- managed edge web server: `Caddy`
- edge host: the current WSL private IP
  - current local observed value: `192.168.165.188`
- routing posture:
  - `/` -> existing `GitLab`
  - `/_matrix/*` -> `Conduit`
  - `/.well-known/matrix/*` -> `Caddy`-served Matrix delegation files when
    required by the chosen first server-name posture
  - `/widgets/matrix/` -> first widget app static assets
- upstream posture:
  - GitLab bundled `NGINX` becomes a loopback-only upstream
  - recommended first loopback target: `127.0.0.1:8081`
  - `Conduit` remains loopback-only behind the proxy
  - recommended first loopback target: `127.0.0.1:6167`
- first widget asset root:
  - `/srv/my-familiar/widgets/matrix`
- privileged packet boundary:
  - install or rebind privileged services
  - write privileged config files
  - start/reload services
  - then stop and hand the stack back to the agent

## Operator Failure Rule

If any command in this procedure fails unexpectedly, stop and hand the exact
output back to the agent.

Do not improvise extra repair commands inside this algorithm unless the step
explicitly tells you to choose between two bounded branches.

## Procedure

1. Confirm that this run still matches the bounded first-slice local posture
   recorded in this algorithm.
   - Expected current values:
     - WSL private-IP front door: `http://192.168.165.188`
     - GitLab loopback upstream: `127.0.0.1:8081`
     - Conduit loopback upstream: `127.0.0.1:6167`
     - widget asset root: `/srv/my-familiar/widgets/matrix`
     - Conduit server name: `192.168.165.188`
   - If this run is no longer using those values, stop and hand control back
     to the agent instead of improvising a silent variant.

2. Open a privileged WSL shell for the target distro and privileged install
   account, then verify that sudo works there.
   - Current bound target values:
     - distro: `Ubuntu-24.04`
     - privileged install account: `rob`
   - From Windows, one acceptable entry command is:
     ```powershell
     wsl.exe -d Ubuntu-24.04 -u rob
     ```
   - If you are already inside that target WSL shell as `rob`, continue
     without reopening it.
   - Then run:
     ```bash
     sudo -v
     ```
   - Continue only if `sudo -v` succeeds.

3. Verify that this WSL distro is actually running with `systemd`.
   - Run:
     ```bash
     ps -p 1 -o comm=
     ```
   - Continue only if the result is exactly `systemd`.

4. Record the current node identity, current WSL IP, and current listeners
   before changing services.
   - Run:
     ```bash
     . /etc/os-release && printf '%s\n' "$PRETTY_NAME"
     hostname
     whoami
     hostname -I
     ss -ltnp | grep -E ':(80|8081|6167)\b' || true
     ```
   - Use this output to confirm:
     - you are on the intended node
       - current bound hostname expectation: `GARION`
     - you are using the intended admin account
       - current bound account expectation: `rob`
     - the current WSL IP still includes `192.168.165.188`
   - Keep the listener output so the agent can compare later if something goes
     wrong.
   - If `hostname` does not report `GARION` or `whoami` does not report `rob`,
     stop and hand the output back to the agent instead of continuing on an
     unexpected shell target.
   - If the current WSL IP does not include `192.168.165.188`, stop and hand
     the output back to the agent instead of silently continuing with a moved
     target.

5. Verify that the current node architecture still matches the fixed Conduit
   package architecture used by this first slice.
   - Run:
     ```bash
     uname -m
     ```
   - Continue only if the result is exactly `x86_64`.

6. Check whether `Caddy` is already installed; install it if it is not.
   - First run:
     ```bash
     dpkg -s caddy >/dev/null 2>&1 && echo caddy-installed || echo caddy-missing
     ```
   - If the result is `caddy-installed`, continue.
   - If the result is `caddy-missing`, run:
     ```bash
     sudo apt-get update
     sudo apt-get install -y caddy
     ```
   - After the install command completes, continue only if it succeeded.
   - If `apt-get install -y caddy` exits successfully but prints package-time
     service-start noise involving `systemctl`, continue anyway.
     - This algorithm treats the later explicit `systemctl enable --now caddy`
       and `systemctl reload caddy` commands as the real service-control
       boundary.

7. Install the exact Conduit package for this first slice.
   - Run:
     ```bash
     curl -L -o /tmp/matrix-conduit.deb "https://gitlab.com/api/v4/projects/famedly%2Fconduit/jobs/artifacts/master/raw/x86_64-unknown-linux-musl.deb?job=artifacts"
     sudo DEBIAN_FRONTEND=noninteractive apt-get install -y /tmp/matrix-conduit.deb
     ```
   - Continue only if the package install succeeds.
   - This bounded first slice intentionally uses a noninteractive package
     install because the package-generated `Debconf` config will be replaced in
     the later explicit `/etc/matrix-conduit/conduit.toml` step.

8. Edit `/etc/matrix-conduit/conduit.toml` so the active `[global]` entries
   are set to the following first-slice Conduit values.
   - Set the active `[global]` section entries to:
     ```toml
     [global]
     server_name = "192.168.165.188"
     database_backend = "rocksdb"
     database_path = "/var/lib/matrix-conduit/"
     address = "127.0.0.1"
     port = 6167
     max_request_size = 20_000_000
     allow_registration = true
     allow_federation = false
     allow_encryption = false
     allow_check_for_updates = true
     trusted_servers = []
     ```
   - The first local posture should keep Conduit loopback-only behind `Caddy`,
     which matches the Debian-package default described in the official docs.
   - This is a privileged file write.
     - Use your usual sudo-capable editor workflow so the saved file actually
       replaces `/etc/matrix-conduit/conduit.toml` with root-level write
       authority.

9. Reset the Conduit database directory for this bounded first-slice bringup
   before the first intended start under the final server name.
   - Purpose:
     the Debian package may already have initialized `/var/lib/matrix-conduit/`
     under a package-generated server-name/config posture before the final
     local config above was written.
   - On this node, `/var/lib/matrix-conduit` is a symlink to the real private
     data directory under `/var/lib/private/matrix-conduit`.
   - This first-slice algorithm assumes a fresh local Matrix bringup and does
     not preserve prior Conduit state.
   - Run:
     ```bash
     sudo systemctl stop matrix-conduit || true
     sudo find /var/lib/private/matrix-conduit -mindepth 1 -delete
     sudo ls -la /var/lib/private/matrix-conduit
     ```
   - Continue only if the final directory listing shows no remaining database
     entries beyond `.` and `..`.
   - If you intend to preserve prior Matrix state instead of resetting this
     first-slice bringup, stop here and hand control back to the agent instead
     of running the reset.

10. Inspect GitLab's current web-facing bind and proxy posture and edit
   `/etc/gitlab/gitlab.rb` only if needed.
   - Run:
     ```bash
     sudo grep -nE "^(external_url|nginx\\['listen_addresses'\\]|nginx\\['listen_port'\\]|nginx\\['listen_https'\\]|nginx\\['real_ip_trusted_addresses'\\]|nginx\\['real_ip_header'\\]|nginx\\['real_ip_recursive'\\])" /etc/gitlab/gitlab.rb || true
     ```
   - Purpose:
     determine whether GitLab is still in the older direct-bind posture or
     already matches the intended `Caddy`-fronted posture.
   - Desired current posture is exactly:
     ```ruby
     external_url "http://192.168.165.188"
     nginx['listen_addresses'] = ['127.0.0.1']
     nginx['listen_port'] = 8081
     nginx['listen_https'] = false
     nginx['real_ip_trusted_addresses'] = ['127.0.0.1']
     nginx['real_ip_header'] = 'X-Forwarded-For'
     nginx['real_ip_recursive'] = 'on'
     ```
   - If the grep output already shows all of those settings with those exact
     values, make no edit and continue to step 11.
   - If the grep output is missing any of those settings or shows conflicting
     values, edit `/etc/gitlab/gitlab.rb` so these effective settings are
     present exactly:
     ```ruby
     external_url "http://192.168.165.188"
     nginx['listen_addresses'] = ['127.0.0.1']
     nginx['listen_port'] = 8081
     nginx['listen_https'] = false
     nginx['real_ip_trusted_addresses'] = ['127.0.0.1']
     nginx['real_ip_header'] = 'X-Forwarded-For'
     nginx['real_ip_recursive'] = 'on'
     ```
     - Keep `GitLab` rooted at `/` as the current local top-level application
       path.
     - Keep `external_url` at the operator-facing front-door URL.
       - Do not change `external_url` to `127.0.0.1` just because bundled
         `NGINX` becomes a loopback upstream behind `Caddy`.
     - If older conflicting values exist elsewhere in `/etc/gitlab/gitlab.rb`,
       change them so the effective config matches the block above.
     - This is a privileged file write.
       - Use your usual sudo-capable editor workflow so the saved file
         actually replaces `/etc/gitlab/gitlab.rb` with root-level write
         authority.

11. Apply the current GitLab config.
   - Run:
     ```bash
     sudo gitlab-ctl reconfigure
     ```
   - Continue only if `gitlab-ctl reconfigure` succeeds.
   - The purpose of this command is to apply the current `/etc/gitlab/gitlab.rb`
     values. It does not invent a new posture by itself.

12. Create the widget asset directory and hand it to `mfuton_nodectl`.
   - Run:
     ```bash
     sudo mkdir -p /srv/my-familiar/widgets/matrix
     sudo chown -R mfuton_nodectl:mfuton_nodectl /srv/my-familiar/widgets
     sudo chmod -R 755 /srv/my-familiar/widgets
     ```
   - After this step, the directory should exist and be writable by
     `mfuton_nodectl`, but it should still be empty until the agent performs
     widget asset writeback.

13. Enable and restart `matrix-conduit`.
   - Run:
     ```bash
     sudo systemctl enable --now matrix-conduit
     sudo systemctl restart matrix-conduit
     ```
   - Continue only if both `systemctl` commands succeed.

14. Edit `/etc/caddy/Caddyfile` so it contains the following first-slice site
    block for `http://192.168.165.188`.
   - Add this site block:
     ```caddyfile
     http://192.168.165.188 {
         redir /widgets/matrix /widgets/matrix/ 308

         handle /_matrix/* {
             reverse_proxy 127.0.0.1:6167
         }

         handle_path /widgets/matrix/* {
             root * /srv/my-familiar/widgets/matrix
             try_files {path} /index.html
             file_server
         }

         handle {
             reverse_proxy 127.0.0.1:8081
         }
     }
     ```
   - If the default package-installed `:80` demo block is still present,
     remove or replace that default block so this Matrix/GitLab front-door
     block is the active intended site definition instead.
   - This is a privileged file write.
     - Use your usual sudo-capable editor workflow so the saved file actually
       replaces `/etc/caddy/Caddyfile` with root-level write authority.

15. Enable and reload `Caddy`.
   - Run:
     ```bash
     sudo systemctl enable --now caddy
     sudo systemctl reload caddy
     ```
   - Continue only if both `systemctl` commands succeed.

16. Run the three minimal privileged status commands and keep their output for
    the handoff.
    - Run:
      ```bash
      sudo systemctl status caddy --no-pager
      sudo systemctl status matrix-conduit --no-pager
      sudo gitlab-ctl status
      ```
    - If those commands report active/running services, stop here and hand the
      output back to the agent.
    - If one of them reports a failed service, stop here and hand the failing
      output back to the agent.
    - Interpretation:
      - `systemctl status caddy --no-pager` should report `active (running)`
        or an equivalent healthy active state
      - `systemctl status matrix-conduit --no-pager` should report
        `active (running)` or an equivalent healthy active state
      - `gitlab-ctl status` should show GitLab-managed services in healthy
        `run:` states rather than `down:` states
    - Do not add HTTP checks, browser checks, or room/widget checks here.

17. Stop after the privileged packet and wait for the agent.
    - If the services came up, stop here and wait for the agent to do:
      - non-privileged config inspection
      - HTTP routing checks
      - Element/client checks
      - widget directory/ownership check
      - widget asset install
      - widget receipt checks
      - mission writeback
    - If a service failed, hand the exact failing status/log output back to the
      agent and stop here instead of improvising deeper changes.

## Outputs

- one WSL-hosted managed `Caddy` edge service installed or confirmed
- one `Conduit` package install completed or confirmed
- GitLab rebound behind loopback-only bundled `NGINX`
- widget asset root created for later agent-managed writeback
- privileged config files installed:
  - `/etc/gitlab/gitlab.rb`
  - `/etc/matrix-conduit/conduit.toml`
  - `/etc/caddy/Caddyfile`
- services started or reloaded:
  - `caddy`
  - `matrix-conduit`
  - `gitlab-ctl reconfigure` completed
- widget asset root created and handed to `mfuton_nodectl`
  - this algorithm does not install widget files or validate the widget URL
- control returned to the agent for the non-privileged continuation

## Stop Conditions

Stop and ask before continuing if any of these happen:

- `sudo` is not available for the operator on the WSL node
- `systemd` is not active as PID 1 on the target WSL node
- `apt-get install caddy` fails
- the chosen `Conduit` package cannot be downloaded or installed
- `gitlab-ctl reconfigure` fails
- `matrix-conduit` fails to start after config install
- `caddy` fails to start or reload after config install

## Notes

- This algorithm is intentionally a privileged packet only.
- It is the current bounded workaround for the unresolved WSL privilege model.
- It should not absorb the agent-side preparation or validation work just
  because the operator had to run `sudo`.
- The first local front-door contract keeps `GitLab` at `/` and avoids pushing
  Matrix itself under an ad hoc `/service_name/` path.
- The first widget surface may still live under `/widgets/matrix/` because the
  widget is an application asset plane rather than the Matrix protocol itself.
- The operator packet intentionally stops before widget asset writeback.
  - do not ask the operator to validate `http://192.168.165.188/widgets/matrix/`
    until the agent has actually installed widget assets into
    `/srv/my-familiar/widgets/matrix`
