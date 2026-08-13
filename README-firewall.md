# Mesh Firewall — design note

**Status: DESIGN ONLY. Nothing here has been implemented.** Written 2026-08-13
from the Amsterdam box, which is exactly the wrong vantage point to apply it
from — see "Who drives this" below.

## Why this exists

While standing up `zone.hyperreal.enterprises` on the Amsterdam box we found it
has **no host firewall of any kind**: no `ufw`, no `nft`, and `iptables -S`
returns three bare `ACCEPT` policies. Everything bound to `0.0.0.0` is on the
public internet. That includes **Agency on :7070**, which was confirmed
reachable from four external nodes (AT, CH, ES, IN) during testing.

That is the immediate motivation, but a one-box fix is the wrong shape. The
Agency mesh is a federation — closing :7070 on one node while three others
answer it publicly buys nothing. Hence a mesh-wide design.

## The mesh as measured (2026-08-13)

Port lists below are **externally observed** from the Amsterdam box, not read
off each host's config. Treat them as a floor, not a complete inventory.

| node | address | role | externally open |
|---|---|---|---|
| `reliablesite` (ams) | `104.243.39.24` | this box; Caddy, Agency, llama-server | 22, 25, 80, 443, 7070, 7072, 7073 |
| linode `metameso` (us-ord) | `172.236.108.82` | **federation hub** — 42 of 55 roster agents proxy through it | 80, 443, 7070, 7072 |
| linode `ubuntu-eu-west` | `85.159.211.210` | **Mail-in-a-Box**: mail + authoritative DNS for all zones | 53, 80, 443, 465, 587, 993, 995, 4190 |
| linode `Hyperreal2` (gb-lon) | `172.236.28.208` | mesh node | 80, 443, 7070, 7073 |
| Joe's laptop | `161.73.4.62` *(today)* | the only host that reaches everything; SSH + mosh origin | n/a (client) |

Two probe limitations worth recording so nobody re-derives them:

- **Outbound TCP/25 is blocked by ReliableSite** (verified against
  `gmail-smtp-in.l.google.com` and `aspmx.l.google.com`: both time out). So the
  scan above *cannot* see :25 on the Linodes. The mail box certainly does listen
  on 25 — absence in that table is a blind spot, not a finding.
- **The mail box does not answer on :22.** Either SSH is on a non-standard port
  or it is already filtered. Resolve this before designing its rules; it is the
  one node where we do not currently know how we get in.

Also observed, and worth fixing independently of any firewall: this box runs
**Postfix bound to `0.0.0.0:25` and `[::]:25`** despite not being the mail
server. If it is only a local sending MTA it should be on loopback
(`inet_interfaces = loopback-only`). A firewall would paper over this; better to
close it at the source.

Loopback-only on this box, and therefore not a firewall concern at all:
`2019` (Caddy admin), `8090` (llama-server), `8081`, `6768`, `41523`, `53`
(systemd-resolved).

## Who drives this — and from where

**From the laptop, not from any node.** Two independent reasons:

1. It is the only host with a route to all four nodes.
2. Every node is a host you reach *over the thing you are reconfiguring*. Apply
   a rule from the box it applies to and a mistake is unrecoverable over the
   same channel that just died.

Corollary: **out-of-band access must be confirmed before the first rule
lands**, per node. Linode gives you Lish (serial console), which a Cloud
Firewall cannot block — that is a genuine safety net. **ReliableSite is the
risk**: unknown whether there is IPMI/KVM or a rescue mode. *Open question 1
below; do not touch the Amsterdam box's rules until it is answered.*

## Two enforcement planes

Deliberately not one mechanism. The three Linodes and the dedicated box have
different escape hatches, so they get different tools:

- **Linode Cloud Firewall** for the three Linodes. Enforced at the hypervisor,
  outside the guest, so a bad rule cannot wedge the host and Lish still works.
  Manageable via `linode-cli firewalls ...` and attachable to multiple linodes
  by ID. **There are currently zero Cloud Firewalls on the account** — this is
  greenfield, nothing to migrate.
- **Host `nftables`** for the Amsterdam box, which has no cloud plane. Higher
  risk, so it goes last and needs the canary pattern below.

Keep the *policy* single-sourced even though enforcement is split: one file
that lists the tiers, two renderers. Do not hand-maintain two rule sets that
are supposed to agree.

## The policy, as tiers

The useful abstraction is not per-host rules but four tiers, applied uniformly:

**Tier P — public.** Reachable from anywhere, by design.
`80`, `443` (all four nodes). Plus, on the mail box only, the mail/DNS set.

**Tier M — mesh-only.** Reachable *only* from the other node IPs.
`7070` (Agency), `7072`, `7073`. This is the whole point of the exercise.
A four-entry static allowlist: the three Linode IPs plus `104.243.39.24`.
Note the asymmetry — us-ord exposes 7072, gb-lon exposes 7073, Amsterdam
exposes both. Worth knowing whether that is intentional (*open question 3*)
before encoding it; the safe default is to allow the union within the mesh and
deny it outside.

**Tier A — admin.** `22/tcp` and mosh's `60000-61000/udp`. Both currently
required from Joe's laptop. See the dynamic-IP problem below.

**Tier L — loopback.** Everything else: not a firewall matter, fix the bind
address instead. Closing a port with a rule when you could simply not listen on
it is strictly worse — it hides the exposure from `ss`.

Default policy: `INPUT drop`, established/related accept, loopback accept,
ICMP echo accept (do not blackhole ping — you will want it for exactly this
kind of debugging). Egress stays open; there is no threat model here that
egress filtering serves, and it would break Let's Encrypt renewals and the
federation's outbound calls.

## The mail box is the exception — treat it last, or not at all

This is the constraint that most shapes the design. The Mail-in-a-Box at
`85.159.211.210` is not just a mail server; it is **the authoritative
nameserver for `hyperreal.enterprises`** (the registrar delegates to
`ns1.box`/`ns2.box`, both pointing at that IP) and for the other live zones.
So its required exposure is:

- `25/tcp` from **the entire internet** — any sending MTA on earth must connect.
  Not restrictable in any meaningful way.
- `53/tcp` and `53/udp` from **the entire internet** — every recursive resolver
  queries it. Also not restrictable.
- `80`, `443` from anywhere (webmail, the admin UI, ACME challenges).
- `587`, `465`, `993`, `995`, `4190` from wherever Joe actually reads mail —
  laptop, phone, possibly roaming. Tempting to restrict; in practice this is
  what breaks mail on a train.

Which leaves ~nothing to close. **The security gain on this node is close to
zero and the blast radius is the highest of any node** — a mistake takes out
both mail *and* DNS for every domain, including the record that makes
`zone.hyperreal.enterprises` resolve. MiaB also manages its own `ufw` and can
fight an externally-imposed rule set.

Recommendation: **leave the mail box alone in phase 1.** If it is ever brought
in, the only defensible rules are (a) fencing its SSH port, and (b) rate-limits,
which MiaB partly does already via fail2ban. Do not attach a Cloud Firewall to
it just for symmetry.

## The dynamic-laptop problem

Tier A wants to say "port 22 from Joe's laptop." But `161.73.4.62` is today's
address, and the laptop moves. Three options, in increasing order of how much
they actually solve:

1. **Leave 22 open to the world, harden the service.** Key-only auth
   (`PasswordAuthentication no`), fail2ban. Honest and robust; the residual
   risk with key-only SSH is small. Loses nothing but tidiness.
2. **Pin to a stable jump host.** Allow 22 only from the us-ord hub, reach
   everything else through it. Cheap, but it makes the hub a single point of
   failure for administration, and the hub is the busiest node.
3. **Overlay network (WireGuard, or Tailscale if you want it managed).** Every
   node gets a `wg0` address; SSH and the whole of Tier M bind to the overlay.

**Option 3 is the endgame and it makes Tier M disappear.** If Agency binds to
the WireGuard interface instead of `0.0.0.0`, then :7070 is not exposed at all
and needs no allowlist — the federation runs inside the tunnel, the laptop is
just another peer, and the dynamic-IP question dissolves because peers are
identified by key, not address. The public firewall then has only two tiers:
80/443 open, everything else closed.

That is more work than phase 1 should attempt. Suggested path: **do option 1
now** (it is compatible with everything), build the mesh allowlist for Tier M,
and treat WireGuard as phase 2 with the allowlist as its fallback.

## Rollout, safely

Order is chosen so that the first mistake is the cheapest:

1. **`Hyperreal2` (gb-lon)** — a mesh node, nothing else depends on it. Cloud
   Firewall. If it goes wrong, Lish gets you in and only that node is down.
2. **`metameso` (us-ord)** — the federation hub. Same mechanism, but 42 proxied
   agents ride on it, so verify the mesh reconverges before continuing.
3. **`reliablesite` (ams)** — host nftables, only after out-of-band access is
   confirmed. Highest lockout risk.
4. **The mail box** — probably never; see above.

**Canary + auto-revert** for every node, and especially for step 3. Before
applying, schedule the undo; only cancel it once you have re-verified access
from the laptop:

```bash
# on the target, BEFORE applying anything (illustrative, not tested)
echo 'nft flush ruleset' | at now + 10 minutes     # or a systemd timer
# ...apply rules...
# ...from the LAPTOP, verify ssh + mosh + the mesh still work...
atrm <job>                                          # only then cancel the undo
```

Verification after each node, all run **from the laptop**:

- `ssh` and `mosh` still land
- `curl -s localhost:7070/api/alpha/agents | jq .count` on each node — the
  roster count should return to ~55, not drop
- `https://zone.hyperreal.enterprises/` still 200
- mail still flows and `dig @85.159.211.210 hyperreal.enterprises SOA` answers
- external re-check of :7070 — should now **fail** from outside the mesh; that
  negative result is the actual deliverable, so record it

## Open questions — resolve before implementing

1. **Does ReliableSite provide IPMI/KVM or a rescue mode for
   `104.243.39.24`?** Blocking for step 3. If the answer is no, the Amsterdam
   box needs the canary timer treated as mandatory rather than advisory.
2. **What port does SSH use on the mail box?** It does not answer on 22, and we
   should not design rules for a host whose access path we cannot state.
3. **Is the 7072/7073 asymmetry intentional?** us-ord has 7072, gb-lon has 7073,
   Amsterdam has both. Need to know what each port *is* before allowlisting it.
4. **Does anything outside the mesh legitimately call :7070?** Inbound webhooks,
   a phone client, Termux (`README-termux.md`)? If yes, Tier M needs a
   documented exception and option 3 needs the laptop/phone as peers from day
   one.
5. **What are the `home-site` tags?** The roster reports `oxf` (34), `lon` (5),
   `chi` (2), `abl` (1). `lon`/`chi` plausibly map to gb-lon/us-ord, but `oxf`
   and `abl` do not obviously correspond to any node in the table — so either
   the site taxonomy is logical rather than physical, or there are hosts in this
   mesh that this note has not enumerated. **The design is not complete until
   that is settled**; a firewall built on an incomplete node list will silently
   partition something.

## Related

- `README-linode.md` — Linode provisioning; the Cloud Firewall work belongs
  alongside it.
- `README-bare-metal.md` — standing up a compute box; host-firewall setup should
  eventually become a step there rather than a retrofit.
- `README-boundary.md` — unrelated (persistence boundaries, not network ones);
  named similarly enough to be worth disambiguating.
