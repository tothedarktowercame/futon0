# metameso side of zone's hardening (tracked copies)

Installed by hand on metameso: `zone-tunnel-check` to `~/bin/`, the `.service`
and `.timer` to `~/.config/systemd/user/` (enabled, linger is on).

- `zone-tunnel-check` (every 10 min): goes back into zone through the reverse
  tunnel zone holds open (`system/zone/user-units/zone-reverse-tunnel.service`)
  with the restricted key `~/.ssh/zone-tunnel`, pings, and mirrors zone's web
  docroot to `~/mirror/zone-www`. See futon0 README-hardening §1.
