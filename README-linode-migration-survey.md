# Linode archive and fresh-Ubuntu survey

2026-09-09. Read-only SSH survey of the three Linodes reachable through Joe's
configured aliases. This is not a provider-account-wide fleet census. No services
were stopped, packages upgraded, DNS changed or application data copied. Only
inventory metadata was saved to Zone.

## Which machines need an OS migration?

| Host | Observed Ubuntu | Ubuntu Pro attached? | Assessment |
|---|---|---|---|
| Hyperreal | **18.04.6** | No | Oldest. Standard security maintenance ended May 2023; prepare a dedicated mail/DNS migration. |
| Metameso / Chicago | **20.04.1** | No | Standard security maintenance ended May 2025; first general-purpose archive/rebuild candidate. |
| Lucy / London | **24.04.3** | No | Standard security maintenance continues through May 2029. Reports `/var/run/reboot-required`; a clean rebuild is an installation-test choice, not an immediate OS lifecycle requirement. |

Support dates come from [Canonical's release-cycle table](https://ubuntu.com/about/release-cycle).
The point-release string alone does not measure package patch status. No
`apt update` was run, so this survey does not certify pending package updates.
The kernel versions observed were Hyperreal `4.15.0-213-generic`, Metameso
`6.14.3-x86_64-linode168`, Lucy `6.8.0-138-generic`: a recent provider kernel
does not make Metameso's Ubuntu userland current.

For FUTON, retain **24.04 LTS as the first tested clean-image target** unless we
explicitly choose and qualify 26.04. For Hyperreal, choose the Ubuntu release
supported by the selected Mail-in-a-Box migration path, not the FUTON default.
The upstream mail guide describes migrating old 18.04 installations through the
latest compatible 5x release to a new supported box; identify the installed
Mail-in-a-Box version before scheduling that operation.

## Metameso: first archive and restore rehearsal

Observed disk usage: **45 GiB** on a 71 GiB root filesystem. Joe's home is
approximately **21 GiB**, including **13 GiB** under `~/code` and **1.2 GiB**
under `~/public_html`.

| Component | Evidence / approximate size | Backup and restore requirement |
|---|---|---|
| FUTON substrate | Active `futon1b-server.service`; `~/code/futon1b/chicago-store`, **1.4 GiB** | Stop writes and the exact store process for a final consistent archive. Preserve primary store and derived-index metadata; restore and prove identity retrieval, memory/text search and index coherence. |
| Agency | Running from `~/code/futon3c-agency-fixes-cpf`, **41 MiB**; canonical checkout is a different directory | Archive actual worktree, Git metadata, uncommitted/untracked work, launch configuration, queues and agent state. Record loaded-source uncertainty; do not replace it with the canonical checkout and assume equivalence. |
| Proof/research work | `~/code/apm-lean`, **7.1 GiB**; other research repositories and run directories | Preserve source, worktree relationships, toolchain/package pins and results. Classify build caches only after separating them from irreplaceable outputs. Reproducibility of a cache is not evidence that every adjacent file is disposable. |
| Websites | nginx `metameso-org`, `/var/www/html`; `~/public_html` and historical site trees | Archive content and source plus nginx, TLS, domain/redirect mappings, scheduled jobs and application dependencies. Verify HTTP pages and assets after restore. |
| PostgreSQL | v12 main on 5432 is down; v13 main on 5433 is down with **binaries missing** | Preserve both data directories and configuration until their contents and owners are identified. Recover using compatible PostgreSQL binaries in an isolated environment before deciding what to retire. Do not start old clusters against new binaries. |
| Other services | Docker/containerd, Postfix, web and desktop-related services | Container inventory is blocked by socket permissions. Inventory volumes, mail queues, system jobs and other accounts with administrator access before calling the backup complete. |

No user cron entries or user backup timers were found. That is not proof that no
backups exist: root jobs, provider backups and external destinations have not
been inspected. A live rsync of `~/code` would include the running store and is
not a coherent final backup.

## Hyperreal: mail and DNS require their own migration

Observed root usage: **14 GiB**. Joe's home is only **32 MiB** and is not the
main application data. `/home/user-data` reports **2.7 GiB of readable data**,
including roughly **2 GiB** of backups, **511 MiB** of historical ownCloud backup
material and **118 MiB** of web content. Permission failures mean these numbers
are incomplete; the tiny readable mail count must not be interpreted as mailbox
size.

Running services include Mail-in-a-Box, Postfix, Dovecot, DKIM/DMARC and spam
filters, bind9/nsd DNS, nginx and PHP. Preserve mailboxes, account/alias databases,
DNS zone/custom records, TLS and signing keys, website data, and any contacts or
calendar data managed by this installation.

The encrypted backup directory has **46 entries**. Its newest observed Duplicity
incremental files were written **2026-09-09 at 02:33 UTC**. An active-name
`/etc/cron.d/mailinabox-nightly` entry exists. These establish recent backup
artifacts, not successful decryption or complete coverage.

`/home/user-data/backup/secret_key.txt` exists with mode **0600**, but Joe cannot
read it. Its contents were not accessed or printed. Securely obtain and escrow
the restore key separately from the encrypted archive before relying on the
backup. Preserve the complete required full-plus-incremental chain, not just the
latest incremental file.

Follow [Mail-in-a-Box's backup and migration procedure](https://mailinabox.email/maintenance.html):
confirm the compatible application versions, test decrypt/restore, preserve the
hostname, and plan mail delivery and DNS cutover. Verify mailbox counts and sample
messages, account login, sending/receiving, DNS, certificates and administration
status on the restored box. Keep a final-write cutover and rollback plan; restoring
an old snapshot after new mail arrives can lose that mail.

## Lucy: supported OS, substantial application state

Observed root usage: **73 GiB**. Joe's readable home data totals **52 GiB**,
including **44 GiB** in `~/code`. That home total excludes inaccessible Matrix
data and therefore is not a full-home backup-size measurement.

| Component | Evidence / approximate size | Backup and restore requirement |
|---|---|---|
| FUTON historical state | `~/code/storage` **17 GiB**; `futon1b/switchover-store` **7.6 GiB**; Agency durable state **11 MiB** | Identify store owners and any other writers, preserve stores and source/config together, and test retrieval. No Java process was visible, but verify writer absence at backup time. |
| Matrix | Running `matrix-synapse` and `matrix-postgres`; Compose project `~/services/matrix` | Preserve PostgreSQL dump and roles, Synapse configuration/secrets/signing identity, media and deployment files. Bind mounts are `synapse-data` and `postgres-data` under that project; Joe cannot read those host directories directly. |
| Matrix database | PostgreSQL 17; `synapse` approximately **18.6 MB decimal** | Read-only SQL size query succeeded. Size is not a backup; take a dump and prove restoration plus a Matrix application check. |
| VSAT | Project `~/vsat`; stopped `VSAT` web container, running PostgreSQL container `DB` | Named volume `vsat_postgres-vsat` contains PostgreSQL data. Preserve source/config and database roles/dumps; stopped web service does not make its database disposable. |
| VSAT databases | `vsat` **8.7 MB**, `vsp_live` **9.4 MB**, `vsat_live` **10.7 MB** decimal | Archive all three until the authoritative/live relationship is established. Verify expected application records and ownership/grants after restore. |
| Host PostgreSQL | v16 main, **online on 5433**, separate from both PG17 containers | Inventory databases and roles with administrator access, then back up and restore independently. Do not assume the container dumps cover it. |
| Workshop/IRC | patterncloud, The Lounge, ngircd, shim and bridges; nginx sites include `futon-services`, `musn-wss`, `vsat` | Preserve application source, configuration and state, TLS and service units. Test login and actual browser/IRC workflows after restoration. |

For PostgreSQL, logical database dumps need cluster-global objects such as roles
as well; see [PostgreSQL 17 pg_dumpall](https://www.postgresql.org/docs/17/app-pg-dumpall.html).
For Matrix, the database alone does not preserve the full service: include
configuration, signing identity and media as described in the
[Synapse backup guide](https://element-hq.github.io/synapse/develop/usage/administration/backups.html).

The directory named `~/lucy-futon3c-backup` is only **16 KiB**. Its name does not
establish a usable application backup. No verified whole-service backup was found
in the inspected user timers or cron entries.

## Archiving on Zone

Created a protected inventory location:

```text
/home/joe/backups/linode-migration/
  inventory-20260909/       metadata only, directory 0700 / JSON files 0600
```

Raw inventory JSON lives here, outside the public source repositories. It includes
the observed permissions and incomplete size results. No credentials, mailbox
contents, database dumps or application archives have been copied.

Future layout, to create when executing a backup:

```text
<host>/<capture-id>/
  inventory/               OS/packages, services, paths, source identities
  data/                    consistent archives and database dumps
  checksums/               file inventory, byte counts and SHA-256 manifest
  restore-check/           commands, results and coverage exceptions
```

Keep credentials and restore keys in protected storage and out of public receipts.
Record numeric owners, modes, ACLs, xattrs, symlinks and necessary hardlinks so
restoration does not silently change access. Capture all relevant accounts and
system configuration, not only Joe's readable home. Only mark caches as rebuildable
after checking that the source/artifact authority is retained and reachable.

Zone has approximately **204 GiB free**, on the same root filesystem used by the
live stack. The Linodes report approximately **132 GiB used** combined. A copy of
that data plus a 73 GiB Lucy restore workspace would already approach/exceed the
available space before safety reserve or growth. Full uncompressed root-device
images total approximately **281 GiB**, which will not fit. Compression and
sparse-copy savings are unmeasured and must not be assumed.

Start one host at a time, size the selected data with privileged access, and
budget archive + verification restore + an explicit free-space reserve. For
example, Metameso's observed 45 GiB used plus another 45 GiB for a rehearsal and
a provisional 50 GiB Zone reserve totals 140 GiB. That is planning arithmetic,
not a measured archive requirement. Arrange an independent retained backup or
provider recovery copy before destroying a source; two directories on Zone's
single disk are not independent recovery copies.

## Access and acceptance before reinstall

On all three remote hosts, `sudo -n -l` requests a password; direct root SSH with
the available key is denied. Zone's password-free root sudo does not confer remote
administration rights. Lucy's Docker access allowed container/database metadata
inspection; it was not used to obtain host-root access. Protected data and root
configuration inventory remain incomplete.

Before any reinstall:

1. Obtain the intended remote administrator access and provider-console access;
   complete the protected data, backup, DNS and service inventory.
2. For Metameso, archive and rehearse restoration first. In parallel, prepare
   Hyperreal's mail backup-key recovery and compatible migration procedure.
3. Capture an initial archive; stop/quiesce each application's writers for its
   final consistent capture using a separately scheduled outage. Save database
   dumps and preserve stopped-store snapshots as appropriate.
4. Verify transfer hashes and coverage, then actually restore and exercise the
   applications in isolation. Missing paths, unreadable files, failed dumps and
   failed restores block the rebuild. Copy success alone does not close this gate.
5. Present Joe with the exact host, image, preserved/retired applications,
   verified recovery points, outage window and rollback steps. Rebuild only after
   approval of that concrete operation.
6. Test the clean FUTON installer before restoring old operator environments;
   retain the archive separately, then restore required applications and prove
   their service-specific checks before traffic/DNS cutover.

Apollo CI is a separate follow-on: it should continuously check coherent public
source candidates after Inbox Zero work, but neither its passing tests nor an OS
rebuild proves these existing services have been backed up and restored.
