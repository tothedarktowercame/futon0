# Clean Linode installation trial

Plan, 2026-09-09. Joe requested planning a clean Ubuntu installation on an
existing Linode after an initial Apollo trial. This document authorizes no
shutdown, rebuild, DNS change or deletion. No Linode was modified during the
SSH survey.

## Candidate hosts

| Host | Observed state | What must be preserved or retired explicitly |
|---|---|---|
| Lucy / `lucy-joe` | Ubuntu 24.04.3, 7941 MiB RAM, 77 GiB free | nginx sites `default`, `futon-services`, `musn-wss`, `vsat`; Docker/containerd; PostgreSQL 16; ngircd; user services `patterncloud`, `thelounge`, `irc-pass-shim`, and two ngircd agent bridges |
| Chicago / `metameso` | Ubuntu 20.04.1, 3898 MiB RAM, 24 GiB free | Running futon1b and Agency; `chicago-store`; actual Agency worktree `futon3c-agency-fixes-cpf`; nginx `metameso-org`; Docker/containerd; Postfix; desktop/display services and other installed applications |
| Hyperreal | Ubuntu 18.04.6, 1992 MiB RAM, 33 GiB free | Mail-in-a-Box, Dovecot, Postfix, spam filters and DKIM/DMARC; bind9/nsd DNS; nginx. Treat mail/DNS migration as separate work, not installation-test cleanup. |

These are systemd and listener observations, not a complete application/data
inventory. Container workloads, scheduled jobs, database sizes, external clients
and backup recoverability remain unverified. Password-free sudo was unavailable
through the surveyed SSH accounts on all three hosts; the root sudo permission
on Zone does not apply to them.

Lucy is the first candidate to investigate further for a complete compact trial:
it has the closest OS and memory configuration, and no Java service was visible.
It is still serving workshop applications, so it is not disposable. Chicago is
the useful later 4 GiB qualification target after its services and data have a
tested destination. Hyperreal should remain outside this FUTON installation
exercise because mail and DNS would become part of the outage.

## What counts as clean

Apollo tests anonymous public checkout, account-relative paths, distinct data
ownership and ports. It inherits the host's Java/Clojure/Babashka installations,
kernel, system packages and available RAM; it is not a clean OS test. Its earlier
staging account also has caches. Use a new declared dependency cache for this
trial and record that choice.

For the clean trial, boot a fresh Ubuntu image, provision tools from the install
manifest, fetch pinned public sources without Joe's credentials, initialize a
fresh store with public fixtures, and enforce the selected memory budget. Do not
restore application homes or dependency caches before the acceptance tests: that
would conceal missing installer inputs. Restore retained unrelated services only
under a separate, reviewed plan.

Use Ubuntu 24.04 LTS as the first baseline to match Apollo's toolchain, without
claiming it is the latest LTS. The exact provider image and package versions must
be selected and recorded at execution. An in-place OS upgrade preserves installed
state and therefore does not provide this clean test. Ubuntu only supports
sequential LTS upgrade steps; if upgrading Chicago in place instead, plan
20.04 → 22.04 → 24.04 and validate each step. See
[Ubuntu's server upgrade instructions](https://ubuntu.com/server/docs/how-to/software/upgrade-your-release/).

## Preparation before a shutdown decision

1. Select one host and enumerate the applications its users still need. Record
   services, containers, cron/timers, volumes, database stores, domains, DNS,
   certificates, mail queues, SSH access and firewall rules. Keep secrets out of
   the public inventory.
2. Record exact source identities, including worktree changes and untracked
   results. Chicago's canonical checkout does not identify its running Agency.
   Reconcile that before declaring any migration source authoritative.
3. Prepare application-consistent backups and an off-host copy. For XTDB, arrange
   a controlled stopped-store backup or an established supported backup operation;
   do not rsync a live mutable store and call it recoverable. Back up database
   and container data using their appropriate procedures.
4. Restore those backups to an isolated destination and prove retrieval. For
   retained FUTON data, verify identity lookup, text search and indexes. Record
   what would be lost after the recovery point and how final writes will be drained.
5. Confirm provider access, console access, image availability, backup/snapshot
   behavior, retained volumes and the rollback route. A provider rebuild is a
   destructive operation on the instance's disks: inspect its exact scope before
   choosing it. See the [provider rebuild API](https://techdocs.akamai.com/linode-api/reference/post-rebuild-linode-instance).
6. Present the concrete host, preserved services, verified backup identifiers,
   outage window, chosen Ubuntu image, acceptance commands and rollback steps to
   Joe. Approval is for that actual shutdown/rebuild, not a generic deploy plan.

## Execution after approval

Drain writers and invocations, stop the named services through an independent
control session, take the final recovery point, and verify it off-host. Perform
the selected clean-image rebuild only after that checkpoint. Retain the prior
backup and any rollback instance until acceptance and restored-service checks pass.

Provision an ordinary account, SSH access and the declared toolchain. Run the
same public fetch/configure/start/verify commands used on Apollo, with its own
manifest, provider image identity, fresh cache and resource budget recorded.
Test boot, public fixture write/read/search, WebSocket registration, a configured
agent task, stop/start recovery, and profile-specific rebuild behavior. Record
peak memory, swap, latency, disk usage and all failed checks.

If acceptance fails, retain logs and the failed-install receipt, stop only the
trial services, and follow the pretested restore procedure. Do not repeatedly
rebuild or erase evidence. Reconnect clients or alter DNS only after the serving
instance and any restored applications pass their own acceptance checks.

## Immediate next work

Complete Apollo source/runtime qualification first. In parallel with that local
work, the next remote survey should inventory Lucy's workshop and container data
and decide which services must survive. This plan does not assume those services
can be retired merely because no FUTON JVM is running.
