# Finding: you cannot sell labour to a volunteer organisation — you sell to whoever needs that population coordinated

**Date:** 2026-08-22 · **Corpus:** 32 cases · **Sub-population:** 9
**Origin:** Joe, 2026-08-22 — *"literally if the company is primarily asking for
volunteers to help them… it may be tricky to get paid to help them. (Unless they
are hiring volunteer managers etc.)"*

Checked against the corpus rather than asserted. Nine of the 32 cases involve a
volunteer, contributor, or donor population. They split cleanly, and the split
is not about the quality of the work.

## The two that asked for volunteers and never captured

| case | capture outcome | payer |
|---|---|---|
| `planetmath` | **`:zombie`** | possible donors, sponsors, or grant funders |
| `gnu-emacs-fsf` | `:sustained-noncommercially` | historical tape/manual purchasers, FSF donors |

Both have `:acceptance-event :none`. Both name a *hoped-for* payer rather than an
actual one — "possible donors", "supporters". Neither is a failure of merit.

## The seven where money moved — and who paid

| case | acceptance | who actually pays |
|---|---|---|
| `roblox-corporation` | `:per-unit` | players buying currency; brands buying exposure |
| `linden-lab-second-life` | `:per-unit` | residents paying memberships, **land**, transaction charges |
| `kaggle` | `:aggregate` | **the competition host** pays prizes and service fees |
| `gitcoin` | `:per-unit` | **the bounty backer**, then matching partners |
| `docker-inc` | `:none` | organisations buying governance, security, support |
| `redis-labs-redis-inc` | `:none` | customers buying managed service, support, commercial rights |
| `openhands-all-hands-ai` | `:aggregate` | enterprise control-plane budget |

**In every case where a contributor population exists and money moved, the payer
is the party who needs that population coordinated — never the population
itself.** Kaggle does not charge competitors, it charges the host. Gitcoin does
not charge contributors, it charges the backer. Roblox does not pay creators to
create; it sells the apparatus they create inside.

## The rule, and why it is cheap

*If an organisation's standing request to the world is "come and help us for
free", it has already told you it does not buy labour of that kind.* You are
competing with free, on the one axis where free wins.

Joe's parenthetical — *"unless they are hiring volunteer managers"* — is not an
exception to the rule. It is **the entire mechanism**. Coordination is the one
thing a volunteer population structurally cannot supply for itself, because
coordination requires continuity and accountability and volunteers are
intermittent by definition. So the coordination layer is exactly where the
money sits, and it is the only layer that is sellable into such an
organisation.

This is a **screening criterion observable from outside the building**, which
makes it cheaper than the capacity test (`records/batch-G-self.edn`,
`:recipient-capacity`): read the organisation's calls to action. "Volunteer with
us" and "we are procuring" are different sentences, published in public.

## Why this matters for FUTON specifically

`SCHEMA.md` row 6 (Environment) was added at Joe's prompting: *"the asset is the
apparatus that makes work legible and gradeable, not the output of any one
mission… its precondition is real but narrower than first stated: it needs a
population."*

Roblox, Second Life, Kaggle and Gitcoin are row-6 cases, and all four capture.
`planetmath` **had the population and never built or sold the apparatus** —
which is the precise shape of its `:zombie` terminal, and the reason it is the
most instructive case in the corpus for this stack rather than the most
embarrassing.

## To make this checkable

The claim is currently supported by reading 9 records, not by a conjecture the
checker can evaluate — there is no field to key on. Making it mechanical needs
one addition to the record shape:

```
labour-source   employed | contracted | volunteer | mixed
paid-layer      labour | coordination | none
```

With those, the conjecture is one line: *no case with
`labour-source = volunteer` has `paid-layer = labour`.* Re-encoding 32 records
is a real job and is not done here; recorded as the way to promote this finding
from a reading to a check.
