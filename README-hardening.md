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
  There is still no nightly evidence backup.
- `drive-health` shows 3.54 TB written on the new drive about two days after
  install, mostly the restore. Watch whether the daily rate settles.

## 1. Getting back in when the box will not answer

The outage showed that zone could go dark with no way to look at its console.

- **Out-of-band console.** Ask hostplane for IPMI/BMC credentials (and whether
  they need a VPN or allowlist) while things are calm. *Not done.*
- **Serial console:** `console=ttyS0,115200` on the kernel cmdline, so
  serial-over-LAN captures boot. *Not done.*
- **Persistent journal:** journald `Storage=persistent`, so `journalctl -b -1`
  survives a crash. *Not checked.*
- **dropbear-initramfs**, only if the root disk is ever LUKS: SSH in to unlock
  after a reboot, so the box never waits silently on a passphrase. *Not applicable
  now.*
- **systemd watchdog:** `RuntimeWatchdogSec` in `/etc/systemd/system.conf`, to
  reboot on a hard hang. *Not done.*
- **Reverse tunnel out from zone** to a Linode (e.g. `box`), so a mistake in the
  inbound firewall cannot lock everyone out. *Not done.*

## 2. Firewall

- **Host firewall on zone.** *Done 10-08:* ufw default-deny. Open ports are 22,
  80/443, mosh 60000–61000/udp, and 7070 from metameso and lucy only. Before the
  outage, 7070/7072/7073 were open to the internet.
- **Safety net when editing rules remotely:** `iptables-apply`, or an
  `at now + 10 minutes` flush, plus a permanent allow rule for a second host.
  *Not done.*
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

- **Zone-only git work.** *Done 10-08:* `zone-git-backup.timer` (02:30 nightly)
  pushes branches, tags and a `refs/backup/wip` snapshot of every repo with
  zone-only work to bare repos on metameso and lucy.
- **Evidence store, nightly, off-box.** *Not done.* Start from
  `futon3c/scripts/backup_evidence.sh` or the live-copy pattern in
  `~/code/storage/backups/futon1b-2026-09-28/copy.sh`. Keep a few days; do a test
  restore weekly.
- **`~/code/storage`** (442 GB of experiment data) is still single-copy on
  zone, apart from Joe's Lenovo external drive. Writing to that drive via the
  phone was the snag (it is not FAT). *Needs a decision.*
- **The WIP web exhibit** (`/var/www/.../wip/`) and the main Caddy site block
  are not in git. *Idea:* an offline copy of the website and its data (Joe, 10-07).
- **Hygiene that makes backups smaller:** pushing work (inbox-zero), side
  branches, and no work in `/tmp` (see the CLAUDE.md rule). These are failing
  zone-health checks.

## 5. Secrets and access

- **SSH keys only, no root login.** *Done 10-08 on zone.*
- **Per-device credentials, authorised server-side.** This is the rule in
  `README-secrets.md` §1. An *audit of access* (§2), meaning which keys each server
  accepts, is an idea worth repeating after the rebuild.
- **pass on every device, kept in sync.** Every entry is encrypted to three keys
  (zone, phone1, phone2). During the outage metameso served as the hub. zone's copy and the metameso hub have diverged (3 Linode root-password
  commits on one side only). *Open:* merge them, make zone a second hub, and
  automate sync (Joe, 10-07: "the federation of passwords should also be set up").
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
| `wip-cascade-snapshot` | 30 min | Cascade Live; the way probes run inside it (about 70 s, 62 s of it zone-health) | now: the page is otherwise refreshed only by hand | on |
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
- **Old metameso (Linode 7925)** has been offline since the 09-29 cutover and is
  still billed (g6-standard-2, $24 a month). Its full backup on zone
  (`~/backups/linode-migration/metameso/20260914/data/`, 14 GB, plus the 1.4 GB
  chicago-store) matches its sha256. It is encrypted to zone's key and both phones.
  zone's key needs its passphrase, so a test restore was not run. The backup
  excludes `/var/lib/docker` (the old Zulip images); the 2021 Zulip backups in
  `/root` are included.

