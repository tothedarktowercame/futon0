# Hardening: ideas to come back to

Written 2026-10-10. This is a collection of **ideas, not a plan**. Nothing here
is scheduled. Each idea gives its source and, where something has already been
done, what was done. The live checks are in `bb ~/code/futon0/scripts/zone-health.bb`;
run that before believing any status line below.

Sources gathered here:
- `metameso:~/notes/zone-outage-2026-10-06.md`, section "Hardening, once it is
  back" (line 75), plus its tracks A–D and its "Done" logs
- `~/README-ZONE-1.1.md` on zone (what the rebuild changed)
- `README-firewall.md` (mesh firewall, design only), `README-secrets.md`
  (keys and access), `README-bare-metal.md` (setup, disk scan)
- `holes/missions/M-landscape-positioning.md` §3.12 (cross-host recall)

## Status snapshot (zone-health, 2026-10-10)

11 of 19 checks pass. Failing:
- federation peers unreachable from zone
- inbox-zero timer not running; 10 repos with zone-only work outside its manifest
- 288 side branches on no remote
- 454 git checkouts in `/tmp`
- pass store and hubs diverged
- 12 of 12 timers still parked

Two readings worth recording:
- `evidence-backup` passes only because the one-off 10-08 extract is 27 h old.
  (10-10.) The nightly evidence backup was added 11-03, see §4.
- `drive-health` shows 3.54 TB written on the new drive about two days after
  install, mostly the restore. Watch whether the daily rate settles.

## 1. Getting back in when the box will not answer

The outage showed that zone could go dark with no way to look at its console.

- **Out-of-band console.** Ask hostplane for IPMI/BMC credentials (and whether
  they need a VPN or allowlist) while things are calm. *Not done.*
- **Serial console:** `console=ttyS0,115200` on the kernel cmdline, so
  serial-over-LAN captures boot. *Not done.*
- **Persistent journal:** journald `Storage=persistent`, so `journalctl -b -1`
  survives a crash. *Checked 11-03 (claude-5):* `/var/log/journal` exists,
  but the default 4G cap held only 6 days (oldest entry 10-28, up since
  10-20). In the 24 h to 11-03 22:55, `vsat-reader`'s crash loop wrote 1.06M
  of 1.54M lines; it is fixed (§4). Next loudest: `wip-scope-ledger` 172k,
  `futon1b-zone` 170k (a start and end line per request). Now
  `SystemMaxUse=20G`, `MaxRetentionSec=90day` in
  `/etc/systemd/journald.conf.d/zone-retention.conf` (tracked copy
  `system/zone/etc/journald-zone-retention.conf`). *Open:* quieten
  futon1b's per-request logging.
- **dropbear-initramfs**, only if the root disk is ever LUKS: SSH in to unlock
  after a reboot, so the box never waits silently on a passphrase. *Not applicable
  now.*
- **systemd watchdog:** `RuntimeWatchdogSec` in `/etc/systemd/system.conf`, to
  reboot on a hard hang. *Not done.*
- **Reverse tunnel out from zone** to a Linode, so a mistake in the inbound
  firewall cannot lock everyone out. *Done 11-05 (claude-16):*
  `zone-reverse-tunnel.service` (user unit, restarts itself) holds
  metameso `127.0.0.1:2224` → zone `:22`. To get in when zone's inbound path is
  broken, from a device whose key zone accepts without a from= pin (Joe's own
  devices): `ssh -J joe@172.236.108.82:2222 -p 2224 joe@127.0.0.1`. The jump
  makes the device's own key do the login. The hubs' keys cannot be used: zone
  pins them with from=<hub IP>, and through the tunnel the connection arrives
  from 127.0.0.1. *Tried 11-05 by Joe from his phone: after accepting the
  host key, he got a shell on zone.*
  The tunnel is kept honest by use, not only kept for emergencies (Joe, 11-05):
  every 10 min metameso's `zone-tunnel-check` goes back through it with the key
  `~/.ssh/zone-tunnel`, which zone's authorized_keys restricts
  (`from="127.0.0.1,::1",restrict,command=system/zone/tunnel-serve`) to `ping`
  and a read-only rsync of the web docroot; it mirrors the site to
  `~/mirror/zone-www` on metameso (409 MB), ready for static failover. Shell and
  writes through that key are refused (tested). zone-health check
  `reverse-tunnel` fails if the unit is down or metameso has not got through in
  30 min (tested live by stopping the unit). metameso side: `system/metameso/`.

## 2. Firewall

- **Host firewall on zone.** *Done 10-08:* ufw default-deny. Open ports are 22,
  80/443, mosh 60000–61000/udp, and 7070 from metameso and lucy only. Before the
  outage, 7070/7072/7073 were open to the internet.
- **Safety net when editing rules remotely.** *Done 11-05 (claude-16):* change
  the firewall with `sudo ufw-safe <ufw args>` instead of `ufw`. It saves
  `/etc/ufw`'s rule files, arms a transient timer, then makes the change; unless
  `sudo ufw-safe confirm` runs within 5 min (`-m N` to change), the timer puts
  the files back and reloads ufw. A second change while one is pending is
  refused, so the saved copy is always the last confirmed state. Log:
  `/var/log/ufw-safe.log`; source `system/zone/ufw-safe`, installed in
  `/usr/local/sbin/`. Tested on a harmless rule (deny 9/tcp): left alone it
  was removed after 1 min; confirmed, it stayed; then removed with the tool.
  The "second host" half is the reverse tunnel (§1), which does not depend on
  zone's inbound rules at all. Plain `ufw` still works and has no undo.
- **Mesh-wide firewall** for the other nodes (metameso, lucy, the mail box), per
  `README-firewall.md`. *Design only.* The 08-13 scan found Agency's 7070
  publicly reachable on more than one node.

## 3. Watching the hardware

- **smartd and disk-watch.** *Done 10-08:* daily short and weekly long tests;
  10-minute CSV of temperature, TB written, hours and errors; alerts at ≥75 °C,
  any media error, or >1 TB written in 24 h, posted to the Matrix room "zone
  alerts".
- **A dedicated alerts bot** in place of posting as @fumarimo. *Idea.*
- **Ask hostplane whether the T705 has a heatsink.** *Open.*
- **RAID1** if a second drive is ever affordable. The old drive took ~33.5 TB of
  writes in 4 months. *Idea; costs money.*
- **A second server** for redundancy and extra compute (two 128 GB boxes rather
  than one 256 GB box; Joe, 10-08), once there is revenue to pay for it. *Idea.*

## 4. Backups

- **Zone-only git work.** *Done 11-05:* the unified `futon-clean-worker.timer`
  runs at most every 15 minutes (plus post-commit requests), publishes Clean
  branches, and copies branches, tags, and per-worktree WIP refs to bare repos
  on metameso and lucy.  Successful recovery fingerprints let unchanged repos
  skip object construction and pushes; changed or failed repos take the full
  path, and a full reconciliation runs at least daily.  The 02:30 timer invokes
  this same worker as a backstop.
- **Evidence store, nightly, off-box.** *Done 11-03 (claude-5, `fa1618c`):*
  `evidence-backup.timer` (03:30 nightly) runs `scripts/evidence-backup.sh`,
  restic over sftp to `~/backups/evidence/restic` on metameso and lucy.
  Incremental: the XTDB log is append-only and table files never change, so a
  night sends the new log tail and new tables only (dry run: 64.3 GiB stored as
  5.7 GiB per hub). The fts5 sidecar is derived and not copied. Each run reads
  back a rotating 1/30 of the data; only a passing check touches
  `evidence-verified`, which zone-health reads. Keeps 7 daily, 4 weekly,
  12 monthly. Password: `pass zone/evidence-backup-restic` (Joe confirmed it
  decrypts). *Test restore 11-04 (claude-16):* from metameso's repository, snapshot
  `ba64f16b` (11-04 03:31), 5 table files and `write-log.edn` restored
  byte-identical to the live store, and the first 2 GiB of `log/LOG`
  streamed back with `restic dump` matched too (sha256). Run from zone with
  the hub as storage; a restore on a hub alone would need the password
  typed there, since the hubs cannot decrypt the pass store. Sample list:
  `~/notes/disk-writes/restore-sample.txt`. The middle ground until Joe
  decides whether fully streamed federated evidence is worth building.
- **VSATARCS reader database lost.** The public demos
  (`/vsatarcs` → `:8443/demos/`) are static and work again (11-03: the unit now
  runs from `~/worktrees/vsat/futon-zone-reader` on `demo/futon-zone`, so a
  branch switch in `~/vsat` cannot break it). The `/story/` pages render empty:
  the Compose PostgreSQL container and its volume are gone from zone, probably
  in the 10-06 drive failure. Rebuild per `docs/zone-demos.md` (compose db,
  migrations, `data/zone-futon-profile.json`, `vsat_reader` role,
  `scripts/zone/verify-reader.mjs`). *Deferred (Joe, 11-03):* wanted for more
  VSAT work, below hardening and the Ways of Working.
- **`~/code/storage`** (442 GB of experiment data) is still single-copy on
  zone, apart from Joe's Lenovo external drive. Writing to that drive via the
  phone was the snag (it is not FAT). *Needs a decision.* Joe, 11-05: the 2 TB
  SSD is mostly empty, so 447 GB fits; options are reformatting it, or adding a
  partition Android can write, and first checking how much of `storage` is
  needed at all. (termux-usb has read ext4 without root before, see the
  phone2 notes.) Ranked below the no-cost items in §1–§2 and §7.
- **The web site and its Caddy config.** *Done 11-05 (claude-16):*
  `system/zone/zone-site-backup`, nightly at 02:50 (`zone-site-backup.timer`),
  copies `/etc/caddy/Caddyfile`, `/var/www/zone.hyperreal.enterprises` and
  `/srv/{xiang-chat,rebecca-brief,hel-staging}` to `~/backups/zone-site/<date>/`
  on metameso and lucy, 14 dated snapshots hard-linked to each other (519 MB
  the first night, about 3 MB for an unchanged one). Not in git, because the
  Caddyfile holds bcrypt password hashes and futon0 is public on GitHub. A hub's
  snapshot gets a `.verified` marker only when its Caddyfile's sha256 matches
  the live one; zone-health check `site-backup` fails if either hub has no
  verified copy under 36 h old (tests: `scripts/zone-health_backup-paths_test.bb`).
  Not copied: `/srv/futon-element-current` (a symlink to a built Element bundle,
  rebuildable).
- **Hygiene that makes backups smaller:** pushing work (inbox-zero), side
  branches, and no work in `/tmp` (see the CLAUDE.md rule). These are failing
  zone-health checks.

## 5. Secrets and access

- **SSH keys only, no root login.** *Done 10-08 on zone.*
- **Per-device credentials, authorised server-side.** This is the rule in
  `README-secrets.md` §1. An *audit of access* (§2), meaning which keys each server
  accepts, is an idea worth repeating after the rebuild.
- **pass on every device, kept in sync.** Every entry is encrypted to three keys
  (zone, phone1, phone2). During the outage metameso served as the hub. *Merged 11-03 (claude-5):* zone's store and the metameso hub had
  each only added entries; merged as `fadbad9` and pushed to both (zone-health
  `pass-store`, `pass-hubs-agree` pass). *Done 11-05 (claude-16):* lucy is a
  second hub (`~/git/password-store.git`), and `system/zone/pass-sync` runs every
  15 min (`pass-sync.timer`). zone is the merge point: it fetches both hubs and
  any phone whose reverse tunnel is open, merges, pushes to its bare repo and
  both hubs, and has each reachable phone fast-forward. A merge conflict aborts
  the run with nothing pushed (tested: the same entry added differently on both
  phones). An entry added on phone2 reached zone, both hubs and phone1 in one run
  (tested, then removed). Nothing is decrypted; only encrypted files move. Both
  phones were 5 and 7 commits behind and are now level (67 entries; recent entries
  encrypted to all three keys). zone-health: `pass-hubs-agree` now covers lucy;
  `pass-sync` fails if no run has completed in an hour or a phone has not synced
  in 7 days (open its tunnel). phone1 also pushes to metameso by itself after a
  `pass insert`; that is harmless, since the next run merges it.
- **Linode root passwords** were reset during the outage. Check that the new ones
  are what the store holds.

## 6. Recall across hosts

From `M-landscape-positioning.md` §3.12. Work done on other hosts is hard to find
from zone: the 10-08 Tech Week analysis existed only in a Claude transcript on
metameso, and this file's main source was a notes file there.

- **Do agent transcripts and notes on metameso and lucy reach futon1b at all?**
  *Unchecked.* This decides whether the gap is ingestion or retrieval.
- **Federation peers unreachable from zone** (zone-health). This is related, and
  is the subject of `futon3c/holes/missions/M-federated-agency-hardening.md`
  (OPEN).

## 7. Services that are off

- **12 user timers parked since the outage** (APM, mission-wholeness, pattern
  index, …). The list is in `~/.config/systemd/enabled-before-outage/`. Joe
  decides each one: re-enable or retire.
- **The Arxana Clock does not show parked timers.** *Idea:* make it show them.

### Turn-back-on conditions (draft, claude-16, 2026-11-03; Joe decides)

Each timer was parked on 10-06 with nothing written about when to turn it back on.
A condition below is a proposal. Once Joe rules, the ruling goes beside the symlink
in `~/.config/systemd/enabled-before-outage/` so that the next reader finds it at
the switch.

| Timer | Cadence | What reads its output | Turn back on when | Proposal |
|---|---|---|---|---|
| `futon-sync-check-clean` | hourly | the inbox-zero way's monitor (reads "off" because of this) | its notice goes somewhere that acts on it, not an hourly repeat of 11 dirty repos | on, after routing |
| `wip-cascade-snapshot` | 1 min | Cascade Live | *On 11-04 (Joe).* Probes moved to `ways-of-working-probes.timer` (5 min); the publish assembles their results; the cascade graph re-walks only changed types | on |
| `warrant-rerun-worker` | 5 min, 2 in parallel | records-carry-warrant | a warrant rerun is queued and not drained | on if the queue is non-empty |
| `futon-pattern-index` | daily 05:30 | pattern retrieval (the patterns shown at the head of each turn) | patterns have changed since the last index | on |
| `mission-wholeness` | 15 min | live-C harmony for the WM | WM clicks resume (Joe's hold) | with clicks |
| `wm-visibility-projector` | 60 s | voxterm WM run view | WM clicks resume | with clicks |
| `mana-snapshot` | 5 min | not checked | a reader is named | ask |
| `futon1b-metaspace-sampler` | hourly | metaspace diagnosis | futon1b metaspace growth comes back | off until then |
| `wip-efe-field` | 6 h, heavy | `mission-efe-field.html` | someone reads that page | ask |
| `proof-2a-figures` | 30 s | p4ng PROOF-2a figures | PROOF-2a writing is active | off until then |
| `apm-land-certified` | 30 min | APM solver landing | APM runs resume | retire unless APM resumes |
| `apm-axiom-audit` | 2 h | APM axiom sweep | its script is restored: it was `/tmp/axiom_audit.py`, erased at the reboot | retire |

## 8. Readings, 2026-11-03

- zone-health: 9 of 19 checks pass. Since 11-03 the 13 checks that bear on
  machines, copies and access feed the "stays alive and paid" way on Cascade Live
  (`futon3c/scripts/ways-of-working-probe.py`, probe `zone-health`). Bills and
  runway are not covered.
- **Drive writes have not settled.** 10.5 TB were written in about 27 days, around
  0.39 TB a day, with one day at 0.68 TB. The old drive took about 0.28 TB a day
  (33.5 TB in 4 months) before it failed.
- **/tmp holds test fixtures, not work.** 4,927 git checkouts and 26 GB: 9,901
  `wm-production-repair*` and 9,202 `wm-runner-repair-*` entries made by WM tests
  that never remove them. The work-in-tmp check counts them as stranded work. The
  fix is in the tests (WM field).
- **Where the writes come from** (claude-16, 11-03, after a correction from
  claude-2; details in `~/notes/disk-writes/FINDINGS-2026-11-03.md`). The futon3c
  JVM's counter (973 GB in 3.7 days) includes every child it has reaped, so it
  covers the whole agent tree. Two sources of duplicate writing were found:
  Codex's sqlite copy of its own session logs (writing 18 to 114 times what it
  records), and the 643 MB inbox-zero `state.edn`, which is rewritten whole.
  Lean builds and futon1b account for real work.
- **About 114 GB a day is kernel-level** (ext4 journal and metadata, plus swap),
  charged to no unit in cgroup io.stat. Nearly all journal commits are forced by
  fsync. The main source found: Agency's followup queue rewrites and fsyncs
  `/tmp/futon3c-followups.edn` on every `GET /followups/ready`, even when nothing
  changed. Each Emacs REPL buffer polls that every 3 s.
  *Fixed 11-03* (futon3c `7876d59b8`, reloaded live): the JVM's forced writes fell
  from about 7 a second to 0.1 a second.
- **inbox-zero `state.edn` rewrites stopped 11-03** (Joe: a waste while the system
  does not work). The watcher was restarted live without its inbox-zero phase, and
  `FUTON3C_INBOX_ZERO_ENABLED` now defaults to false in `futon3c/scripts/dev-zone-env`
  (`25017e286`). The 643 MB file is left in place.
- **Codex, 11-03.** Two orphaned Agency `codex exec` processes (from 10-20 and
  10-23, unknown to Agency) were stopped. Codex's derived history and log databases
  (4.8 GB) were moved to `~/backups/codex-home/20261103T2110Z`, and Codex recreated
  them. *Idea:* a zone-health check for agent processes that Agency no longer knows.
- **/tmp is in RAM from 11-04** (tmpfs, 64 GB ceiling; enabled for boots too). Details and
  the cleanup still owed: `system/zone/tmp-in-ram/README.md`.
- **Old metameso (Linode 7925)** has been offline since the 09-29 cutover and is
  still billed (g6-standard-2, $24 a month). Its full backup on zone
  (`~/backups/linode-migration/metameso/20260914/data/`, 14 GB, plus the 1.4 GB
  chicago-store) matches its sha256. It is encrypted to zone's key and both phones.
  zone's key needs its passphrase, so a test restore was not run. The backup
  excludes `/var/lib/docker` (the old Zulip images); the 2021 Zulip backups in
  `/root` are included.
