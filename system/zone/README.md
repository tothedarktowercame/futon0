# zone system scripts (tracked copies)

Installed copies live in `/usr/local/sbin/` (root). Edit here, then
`sudo install -m 755 system/zone/<name> /usr/local/sbin/<name>`.

- `disk-watch` — NVMe health every 10 min (`disk-watch.timer`), CSV at
  `/var/log/disk-watch.csv`; alerts via `zone-notify` on heat, media errors,
  >1 TB in 24 h, and (2026-11-03, Joe) >11 GB written in one hour, with an
  `io-top` snapshot of what was writing under `/var/log/disk-watch-hot/`.
