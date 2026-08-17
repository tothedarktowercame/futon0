# Secrets, keys, and access

Written 2026-08-17, during the Dionysus wind-down. Everything marked
**[verified]** was actually done or measured that day; anything else is
reasoning, and is marked as such. Related: `README-termux.md` (the phone as a
client), `holes/M-what-is-it-who-is-it-for.md` (what the day's survey was for).

The organising claim: **most secret-handling problems in this stack are access
problems, not storage problems.** The fix is usually one more authorised key, not
one more copy of a key.

---

## 1. The rule: per-device credentials, authorised server-side

Every device gets **its own** keypair, generated on that device, never leaving
it. Servers authorise several keys. Nothing is ever copied between devices.

Why this and not a shared key:

- **Revocation is one line.** Lost phone → delete its line from
  `authorized_keys`. With a shared key, losing one device means rotating
  everything, everywhere, which in practice means not doing it.
- **The blast radius is legible.** `authorized_keys` becomes an access list you
  can read. A shared key tells you nothing about who holds it.
- **It removes the bootstrap loop** (§4).

**[verified]** The stack already works this way in at least one place without
anyone writing it down: `futon3c/.admintoken` is **45 bytes on Zone and 33 on
Dionysus** — a per-site secret, not a shared one. `.fedtoken` (31 bytes) and
`futon3/.env` (1315) are identical across both, so those are shared; worth
knowing which is which before assuming.

### Adding a device

```bash
# on the new device only
ssh-keygen -t ed25519 -C "joe@<device>"

# on each server that device must reach — append, never overwrite
mkdir -p ~/.ssh && chmod 700 ~/.ssh
touch ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys
printf '%s\n' "<the new public key>" >> ~/.ssh/authorized_keys

# verify from the new device, then stop
ssh <server> 'echo ok:$(hostname)'
```

Keep the comment field. It is the only thing that later tells you whose key a
line is — see §2.

---

## 2. Audit *access*, not just backups

**"Is it backed up?" and "can I still get in?" are different questions**, and the
second one is easier to forget because nothing fails until it's too late.

**[verified] The lucy near-miss, 2026-08-17.** `lucy` (172.236.28.208:2222) had
**exactly one** authorised key, and it was Dionysus's `id_ed25519`
(comment `tothedarktowercame@gmail.com`). Lucy holds `futon5a.git` and
`cv-and-publications.git` — both private, neither on GitHub. Handing the laptop
back would have locked Joe out of both. The repos were fully backed up on
LenovoBackup and that would not have helped at all: the bytes were safe and the
host was unreachable.

Fixed by adding Zone's own key (1 → 2 keys). Found only because someone
enumerated it.

### The audit

```bash
# for each server: how many keys, and whose?
ssh <server> 'awk "{n++; print n\": \"\$1\" ... \"\$NF}" ~/.ssh/authorized_keys'
```

Then, for each line, identify the holder from the comment. **A key you cannot
attribute is a key you cannot safely remove**, which is how single-key servers
survive audits.

**Do this before any machine leaves**, not after. As of 2026-08-17: lucy 2 keys,
hyperreal 4, metameso 6 — all **[verified]** post-fix.

---

## 3. Tell secrets apart from identity assertions

Not everything called a token is a credential, and treating config as secret is
its own cost — it stops you committing things that should be committed.

**[verified]** In futon1b, the **penholder is not a secret.** `allowed-penholders`
is a set of names and `gates/authorize!` checks membership
(`futon1b_gates.clj:49,67`). Anyone who knows the name passes. It is an identity
assertion for attribution, not authentication. Protecting it buys nothing;
*relying* on it for authorisation would be a mistake.

**[verified]** `futon3/.env` is **tracked in git** and that is fine: 12 variables,
all paths, profiles, ports and penholder names, no secret-shaped names, and its
three long values are filesystem paths. Config-as-code, not a leak.

The check before panicking about a tracked file:

```bash
grep -oE '^[A-Za-z_][A-Za-z0-9_]*=' .env | sed 's/=$//'      # names only
awk -F= '/^[A-Za-z_]/ && length($2) > 30 {print $1}' .env    # long opaque values
```

Conversely, `.admintoken` and `.fedtoken` **are** secrets, and are correctly
gitignored — so they are on no remote, and exist only on each host plus the
backup drive.

---

## 4. The bootstrap loop — check it before trusting a store

**A secret store cannot hold the credential needed to reach the store.**

The pass store syncs by git over ssh. If the ssh private key lived *in* it, then
losing a device would mean needing the key to fetch the store that contains the
key. Per-device keys (§1) dissolve this: a new device generates its own key,
gets authorised server-side, and *then* pulls the store.

**[verified] The same shape, already realised.** The GPG key
`ed25519/1A372C814DC6AA39` could not be unlocked on 2026-08-17: the passphrase
was neither remembered nor agent-cached (`gpg --batch --pinentry-mode error`
→ `No PINentry`). Its four `~/.password-store` entries were unreadable. The most
likely place the passphrase survived was **Firefox — on the machine being handed
back**. That is the loop closing.

**The question to ask of any store, before relying on it:** *what do I need in
order to open this, and where does that thing live?* If the answer is "inside the
store", or "on one machine", it is not yet a store.

Corollary: **write the passphrase down physically.** Joe's replacement key
(2026-08-17) is a long phrase reducing to an acronym, recorded in a secret but
memorable non-digital location. That is the mitigation; there is no clever
substitute.

---

## 5. Verify host keys by two independent paths

A relayed host key trusted on a single path is where a MITM lives. Trust it only
when two independent sources agree.

**[verified] 2026-08-17**, relaying three host keys to Zone:

- **Source A** — ask each host for *its own* key over an already-authenticated
  session: `cat /etc/ssh/ssh_host_ed25519_key.pub`
- **Source B** — the entry already in a machine that has connected before:
  `grep -F "<key material>" ~/.ssh/known_hosts`

All three agreed, for metameso, hyperreal and lucy.

Two traps met that day:

- `ssh-keygen -F "[ip]:port"` did **not** find two of the three entries even
  though they were present. `known_hosts` was 64/67 lines **hashed** (`|1|…`).
  Grep the key material instead of relying on the lookup form.
- Checking that the key *material* is present is **not** the same as checking the
  host→key *binding*. A key can be in the file under an alias while a connection
  by IP still fails verification. Test by connecting, not by grepping.

A fresh device has no Source B. That is the one genuinely good reason to keep
**host fingerprints in the password store** — they are not secret, but they are
the trust anchor a new phone will lack.

---

## 6. Rotate rather than recover

API tokens have a property passwords lack: they can be regenerated at will. Use
it.

**[verified]** All four entries in the dead pass store were Linode credentials
(`15`, `linode/mark4-70b-20260618`, `…-exp`, `linode/api-token`). With the GPG
key unopenable they are gone — and that costs nothing, because the right response
to an unreadable API token is to **revoke and reissue**, which is also what you
should do when a machine goes back to an employer.

So: for anything rotatable, the recovery plan *is* rotation. Reserve real
preservation effort for things that cannot be reissued — private keys, recorded
media, mail.

---

## 7. What belongs in the password store

Short, because §1–§3 remove most candidates.

**Yes:**
- **Host fingerprints** for lucy, hyperreal, metameso — non-secret, but the trust
  anchor a new device has no other way to get (§5).
- **Credentials with no per-device form and no second copy** — `ZAI_API_KEY`,
  GitHub PATs, the Linode token *after* rotation.
- **One break-glass entry, explicitly labelled as emergency-only** — so the
  exception is visible rather than becoming the default.

**No:**
- Anything with a per-device equivalent — copying it forfeits §1.
- `.admintoken` (per-site), `.fedtoken`/`.env` (already on both hosts and the
  backup drive).
- Penholder names, and config that merely looks secret (§3).

### Multiple devices, no copied secrets

`pass` encrypts to any number of recipients, so each device gets its own GPG key
and is added as a recipient:

```bash
# on the new device
gpg --quick-generate-key "Joe (<device>) <holtzermann17@gmail.com>" default default 3y
gpg --export --armor > device.pub          # move to the store's host

# where the store lives
gpg --import device.pub
pass init <existing-fpr> <new-device-fpr>  # re-encrypts every entry to both
```

Additive, so it costs nothing to defer. **Never move the secret key between
devices** — a lost device is then revoked by re-running `pass init` without its
fingerprint, instead of rotating every entry. Full recipe:
`README-termux.md` §7.

---

## 8. Browser-held credentials

**[verified]** Firefox held **61 logins** in the snap profile
(`~/snap/firefox/common/.mozilla/firefox/vs4y52jw.default/`), and a home backup
that excluded `/snap/` nearly lost all of them — the only other copy was four
months old. `key4.db` (295 KB) plus `logins.json` (41 KB) are the pair that
matters; the rest of the 2.3 GB profile is cache.

Two operational facts:

- **The export is GUI-only.** `about:logins` → ⋯ → *Export Logins* → CSV, and it
  prompts for the OS password. It cannot be scripted, so it has to be scheduled
  as a human step.
- **The CSV is plaintext.** Import it and destroy it: `shred -vu -n 3 <file>`.
  **[verified]** 61 entries imported to `pass` as `firefox/<host>/<username>`,
  all 61 confirmed encrypted to the new cv25519 subkey, then the CSV shredded.

If a backup exclude list contains `/snap/` or `/var/lib/flatpak`, check what
credentials live under it before trusting the backup.

---

## 9. Checklist before a machine leaves

1. `authorized_keys` audit on **every** server it can reach — is it the only key
   anywhere? (§2)
2. Every repo pushed, remote-name-agnostic:
   `git log --branches --not --remotes` — and do **not** assume the remote is
   called `origin` (`futon1bi`'s is called `lucy`).
3. Browser credentials exported and imported (§8).
4. Can you unlock the GPG key from memory, *right now*, without the machine? (§4)
5. Rotate anything rotatable rather than preserving it (§6).
6. Host fingerprints recorded somewhere a fresh device can reach (§5).
