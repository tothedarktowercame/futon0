# Algorithm: Install Package-Managed GitLab On Borrowed WSL Node Manually

Date: 2026-03-13
Status: active

## Purpose

Use this when the operator wants to bring up the first local `GitLab` instance
on the governed WSL node using the official Linux package, exposing it to the
operator from Windows via the WSL node's private virtual-network IP while
leaving GitLab on its default package-managed paths.

Important posture note:
- this algorithm owns the first borrowed-instance bring-up slice only
- it does not own the later familiar-level access posture where the operator on
  Windows should reach the WSL-hosted GitLab instance without making it visible
  to the broader network
- that later dedicated-node/network-posture question is owned by:
  - `holes/missions/M-windows-wsl-service-node-hybrid-runtime/M-dedicated-my-familiar-wsl-node-provisioning-and-network-posture.md`

This is an operator-executed algorithm:
- the operator performs the privileged install steps manually
- the governed `wsl-futon` node-control lane remains the normal control seam
- this algorithm does not assume a second Codex session inside WSL

## Companion Flexiarg Resolution (2026-03-14)

Governing companion flexiargs for this algorithm:
- `library/system-architecture/cross-platform-execution-modes-and-authority.flexiarg`
  - governs the distinction between Windows-native authority claims and the
    narrower borrowed `windows-wsl` bring-up posture used here
- `library/system-architecture/distributed-resource-planes-peer-collaboration-and-work-protocols.flexiarg`
  - governs classification of the WSL-hosted GitLab instance as a distributed
    service/resource extension of the installation
- `library/foundational/do-not-recreate-the-wheel.flexiarg`
  - governs reuse of the official package-managed GitLab path and existing WSL
    node-control lane rather than inventing a local parallel install surface

Inspected but non-owning candidates:
- `library/social/dual-os-script-surfaces-with-unix-authority.flexiarg`
  - relevant to cross-OS script posture, but not the direct owner of the
    borrowed-instance GitLab bring-up contract

Execution-time rule:
- when this algorithm is run, the governing companion flexiargs above are part
  of its authority surface and should be read/applied rather than re-derived
  from scratch

## Authority routing

- governing bring-up mission:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-first-wsl-hosted-gitlab-bringup-and-service-contract.md`
- governing WSL node-control mission:
  `holes/missions/M-windows-wsl-service-node-hybrid-runtime/M-wsl-ssh-bootstrap-and-wrapper-surface.md`
- mission-local receipt/checklist:
  `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-first-wsl-hosted-gitlab-bringup-and-service-contract/first-localhost-gitlab-bringup-receipt-template.md`
- governed node-control contract:
  `docs/M-windows-wsl-service-node-hybrid-runtime/wrapper-and-service-contract.md`
- later dedicated-node/network-posture owner:
  `holes/missions/M-windows-wsl-service-node-hybrid-runtime/M-dedicated-my-familiar-wsl-node-provisioning-and-network-posture.md`
- official install source:
  `https://docs.gitlab.com/install/package/`
- official Linux package configuration source:
  `https://docs.gitlab.com/omnibus/settings/configuration/`
- official Linux package NGINX source:
  `https://docs.gitlab.com/omnibus/settings/nginx/`
- official health-check source:
  `https://docs.gitlab.com/administration/monitoring/health_check/`

## Inputs

- a live governed WSL node-control lane:
  - SSH alias `wsl-futon`
  - WSL node-control account `mfuton_nodectl`
- an operator-owned privileged Unix account on the target WSL node
  - this is the account used for `sudo`
  - it is intentionally distinct from `mfuton_nodectl` unless the
    installation explicitly grants sudo to the control account
- an operator who can use elevated privilege on the WSL node
- internet access from the WSL node to the GitLab package repository
- chosen bring-up posture:
  - package-managed GitLab
  - operator-accessible from Windows via the WSL node's private virtual-network IP
  - not visible to the broader network
  - default package paths

## Preconditions

1. The WSL SSH/bootstrap lane is live and usable.
2. The operator can reach the WSL node manually over `wsl-futon`.
3. The operator has a Unix account on the target node that can perform
   privileged install steps with `sudo`.
4. The target WSL distro is running with `systemd` as PID 1.
   - this algorithm relies on package-managed GitLab services under
     `systemd`
   - do not proceed on a WSL distro where `systemd` is disabled or replaced
5. `mfuton_nodectl` is treated as the governed node-control account, not as an
   implied admin account.
6. The operator intends a first local bring-up only:
   - no public exposure
   - no Docker requirement
   - no custom GitLab data-root override
7. The operator can identify the current WSL guest private IP on the WSL
   virtual network and verify that it is not one of the Windows machine's
   ordinary LAN adapter addresses.

## Fixed local service contract for this first borrowed-instance bring-up

- package: `gitlab-ce`
  - use `gitlab-ee` only in a later run if the operator explicitly wants the
    licensed edition
- external URL: `http://<wsl_private_ip>`
- exposure posture:
  - operator-accessible from Windows via the WSL private virtual-network IP
  - not broader-network visible
- package-managed paths remain at their defaults:
  - config: `/etc/gitlab/`
  - installed program tree: `/opt/gitlab/`
  - mutable data: `/var/opt/gitlab/`
  - logs: `/var/log/gitlab/`
- first temporary borrowed-instance acceptance checks:
  - package/service installed and running
  - `/users/sign_in` reachable at `http://<wsl_private_ip>` from inside WSL
  - Windows operator can also reach `/users/sign_in` at
    `http://<wsl_private_ip>`
- monitoring note:
  - GitLab monitoring endpoints on the WSL private IP may still need a
    follow-on configuration pass on this borrowed instance even after the
    operator-access contract is working
  - if the operator broadens the GitLab monitoring allowlist for the WSL
    private subnet during this slice, record that fact in the receipt and keep
    it as an explicit follow-on item rather than treating it as settled
- important temporary-contract note:
  - this is a borrowed-instance raw-IP operator-access slice
  - it is not the final stable naming contract for the dedicated familiar node

## Provisional Evidence-Type Mapping For This Run

Use existing evidence types first for this operator-provided receipt.

Suggested mapping:
- preflight host facts and configuration observations:
  - `evidence/type: coordination`
  - `evidence/claim-type: observation`
- install/reconfigure actions:
  - `evidence/type: coordination`
  - `evidence/claim-type: step`
- acceptance checks that prove the lane passed through its first gate:
  - `evidence/type: gate-traversal`
  - `evidence/claim-type: conclusion`
- blockers, failures, or divergences encountered during bring-up:
  - `evidence/type: coordination`
  - `evidence/claim-type: tension`
- post-run deferred items and reflective notes:
  - `evidence/type: reflection`
  - `evidence/claim-type: observation`

Optional extension:
- if the operator explicitly records a reusable discipline/pattern chosen
  during bring-up, use the standard pair:
  - `pattern-selection / observation`
  - `pattern-outcome / conclusion`

This is intentionally an experiment in human-provided evidence, not yet a
settled general evidence contract for all operator algorithms.

## Operator Failure Rule

If any command in this procedure fails unexpectedly, stop and hand the exact
output back to the agent.

Do not improvise extra repair commands inside this algorithm unless the step
explicitly tells you to choose between two bounded branches.

## Procedure

1. Verify that the governed node-control seam still works from Windows.
   - From Windows:
     ```bat
     ssh.exe wsl-futon /bin/sh -lc "hostname && whoami"
     ```
   - Continue only if the output shows the WSL node hostname and user
     `mfuton_nodectl`.

2. Open a privileged WSL shell and verify that sudo works there.
   - On a local same-machine WSL installation, the default form is:
     ```bat
     wsl.exe -d <target-distro> -u <operator-node-admin-user>
     ```
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

4. Record the current node identity and WSL virtual-network facts before
   installation.
   - Run:
     ```bash
     . /etc/os-release && printf '%s\n' "$PRETTY_NAME"
     hostname
     whoami
     hostname -I
     ip -4 addr show eth0
     ip route
     ```
   - Use this output to identify:
     - the WSL guest private IP
     - the gateway / host-side WSL virtual-network address
   - Keep the WSL guest private IP for the later `external_url`, NGINX bind,
     and Windows-side checks.

5. Add the official GitLab package repository.
   - Run:
     ```bash
     curl --silent "https://packages.gitlab.com/install/repositories/gitlab/gitlab-ce/script.deb.sh" | sudo bash
     ```
   - Continue only if that command succeeds.

6. Install `gitlab-ce` with the chosen WSL private IP as the external URL.
   - Run:
     ```bash
     sudo EXTERNAL_URL="http://<wsl_private_ip>" apt-get install -y gitlab-ce
     ```
   - Do not set `EXTERNAL_URL` globally in the shell; pass it only to this command.
   - If `apt` reports packages that are now no longer required and suggests
     `sudo apt autoremove`, treat that as an optional post-install cleanup
     note, not as part of first bring-up acceptance.
   - Continue only if the package install succeeds.

7. Edit `/etc/gitlab/gitlab.rb` so GitLab listens on the chosen WSL private IP.
   - Ensure these settings exist:
     ```ruby
     external_url "http://<wsl_private_ip>"
     nginx['listen_addresses'] = ['<wsl_private_ip>']
     ```
   - Do not move the package data roots for this first bring-up.
   - Keep `/etc/gitlab/`, `/var/opt/gitlab/`, and `/var/log/gitlab/` at package defaults.
   - This run should not bind GitLab to the Windows host's general LAN adapter
     address and should not bind it to every interface unless a later mission
     explicitly changes the contract.

8. Reconfigure GitLab after editing `/etc/gitlab/gitlab.rb`.
   - Run:
     ```bash
     sudo gitlab-ctl reconfigure
     ```
   - Continue only if `gitlab-ctl reconfigure` succeeds.

9. Run the packaged-service status check.
   - Run:
     ```bash
     sudo gitlab-ctl status
     ```
   - Continue only if the expected GitLab services report as running.

10. Verify the sign-in page is reachable from inside WSL.
   - Run:
     ```bash
     curl --silent --show-error --location --output /dev/null --write-out "%{http_code}\n" http://<wsl_private_ip>/users/sign_in
     ```
   - Continue only if the result is HTTP `200`.

11. Verify the same sign-in page is reachable from Windows.
    - Run:
      ```bat
      curl.exe --silent --show-error --location --output NUL --write-out "%%{http_code}\n" http://<wsl_private_ip>/users/sign_in
      ```
    - Continue only if the result is HTTP `200`.

12. Optionally probe GitLab monitoring endpoints and record the result as
    follow-on evidence only.
    - Skip this step entirely if you do not want to spend time on monitoring
      follow-on behavior during the first bring-up.
    - If you do run it, use commands such as:
      ```bash
      curl --silent --show-error --fail http://<wsl_private_ip>/-/health
      curl --silent --show-error --fail http://<wsl_private_ip>/health_check
      curl --silent --show-error --location http://<wsl_private_ip>/-/readiness
      curl --silent --show-error --location http://<wsl_private_ip>/-/liveness
      ```
    - If you broaden the monitoring allowlist for the WSL private subnet during
      this step, record that change explicitly.
    - If these endpoints fail, treat that as recorded follow-on divergence and
      continue; do not turn this first bring-up algorithm into a monitoring
      repair session.

13. Read the initial root password file.
    - Run:
      ```bash
      sudo ls -l /etc/gitlab/initial_root_password
      sudo sed -n '1,20p' /etc/gitlab/initial_root_password
      ```
    - Treat this as sensitive operator-only material.
    - GitLab says this file is removed automatically after 24 hours.
    - Continue only if the file exists and the current bootstrap password value
      is readable.

14. Complete the first `root` login and immediately rotate the password.
    - Open `http://<wsl_private_ip>/users/sign_in`.
    - Sign in as `root` using the password from
      `/etc/gitlab/initial_root_password`.
    - Immediately change the password.
    - Store the replacement password only in the operator's normal secret
      store or password manager.
    - Do not copy either the bootstrap password or the replacement password
      into `mfuton` artifacts, receipts, commit messages, or chat logs.
    - Record only safe evidence:
      - first root login completed
      - password changed immediately
      - replacement password stored only in operator secret store
      - bootstrap password not copied into committed artifacts
    - Continue only if the `root` sign-in succeeds and the password rotation
      is completed.

15. Return to the governed Windows-facing control seam and rerun the two
    Windows-side checks.
    - From Windows, rerun:
      ```bat
      ssh.exe wsl-futon /bin/sh -lc "hostname && whoami"
      ```
    - Then rerun:
      ```bat
      curl.exe --silent --show-error --location --output NUL --write-out "%%{http_code}\n" http://<wsl_private_ip>/users/sign_in
      ```
    - Continue only if:
      - the SSH command still reaches `wsl-futon`
      - the sign-in page still returns HTTP `200` from Windows

16. Fill the mission-local bring-up receipt.
    - Use:
      `holes/missions/M-mfuton-familiar-typed-surface-world-and-memory/M-my-familiar-distributed-resource-plane-extensions/M-gitlab-internal-familiar-component/M-first-wsl-hosted-gitlab-bringup-and-service-contract/first-localhost-gitlab-bringup-receipt-template.md`
    - Map each acceptance criterion to concrete evidence from this run.
    - Use the provisional evidence-type mapping in this algorithm unless the
      run clearly justifies a stronger local subtype.
    - Record any divergence or deferred item rather than silently normalizing
      it away.
    - Do not use this algorithm to redefine the receipt shape; the template
      remains the owner of the concrete receipt contract.

## Outputs

- one package-managed GitLab instance on the governed WSL node
- a concrete borrowed-instance WSL-private-IP service contract:
  - `external_url "http://<wsl_private_ip>"`
  - `nginx['listen_addresses'] = ['<wsl_private_ip>']`
- default GitLab package roots left intact:
  - `/etc/gitlab`
  - `/opt/gitlab`
  - `/var/opt/gitlab`
  - `/var/log/gitlab`
- first local bring-up evidence:
  - `gitlab-ctl status`
  - `http://<wsl_private_ip>/users/sign_in`
  - Windows operator can reach `http://<wsl_private_ip>/users/sign_in`
- if applicable, explicit evidence that a monitoring allowlist change was
  already attempted and still requires follow-up
- if applicable, an explicit note that `apt autoremove` was suggested and left
  as optional post-install cleanup rather than part of first bring-up

## Stop Conditions

Stop and ask before continuing if any of these happen:

- `sudo` is not available for the operator on the WSL node
- `systemd` is not active as PID 1 on the target WSL node
- the GitLab repository bootstrap fails
- the package install fails or is blocked by unsupported OS/package issues
- `gitlab-ctl reconfigure` fails
- the sign-in page does not return a successful HTTP status at the chosen WSL private IP

## Notes

- This first bring-up is intentionally `gitlab-ce` on package defaults.
- It is intentionally tied to the WSL private virtual-network IP for operator
  access from Windows.
- It is intentionally not meant to ride on the Windows host's general LAN
  adapter addresses.
- It is intentionally not the final naming or dedicated-node contract.
- Later work should replace the raw IP with a better stable local-only naming
  and dedicated-node posture rather than treating the borrowed instance as
  canonical.
- It is intentionally not containerized.
- The operator is the privileged installer for this wave, using an operator-
  owned sudo-capable Unix account on the node.
- `mfuton_nodectl` remains the governed control account and should not be
  assumed to have sudo.
- Later automation may reuse the resulting service, but this first algorithm
  does not grant broad privileged installation authority to `mfuton_nodectl`.
