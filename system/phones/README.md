# Phone-side tools (tracked copies)

Installed by copying into Termux's `$PREFIX/bin` (`/data/data/com.termux/files/usr/bin/`).

- `pass-import-browser-csv` (on phone2, 2026-11-05): adds a browser's exported
  passwords to pass as `chrome/<host>/<username>`, never overwriting, then
  shreds the CSV. The plaintext never leaves the phone; pass-sync carries the
  encrypted entries to zone and the hubs. See README-secrets §8.
