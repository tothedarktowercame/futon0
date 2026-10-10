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
