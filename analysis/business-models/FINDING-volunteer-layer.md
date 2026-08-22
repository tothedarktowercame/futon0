# Finding: the discriminator is demand-side obligation, not supply-side payment

**Date:** 2026-08-22 (rewritten same day) · **Corpus:** 32 cases
**Status:** supersedes this file's first version, which was keyed on the wrong
side of the transaction.

## The correction

The first version said: *volunteer organisations cannot buy labour, so sell them
coordination.* Joe broke it the same day:

> *"it's interesting to think about the coordination/contributor problem as not
> strictly a phase change when money gets involved. E.g. freelancing is
> actually, functionally, a lot like volunteering, you just get paid at the end
> of the day. So, code-for-hire platforms are not THAT different from PlanetMath."*

He is right, and the consequence is larger than a caveat. Volunteer, bounty
hunter, freelancer, contractor and employee form a **continuum of contributor
commitment** on the *supply* side: self-selecting, intermittent, choosing what
to pick up, with no continuity of employment. Payment moves a contributor along
that continuum; it does not change the platform's structural position, and it
is not a phase change. So supply-side payment cannot be the discriminator — and
the corpus says it is not.

## The natural experiment already in the corpus

| case | supply side | acceptance | capture | who is obliged |
|---|---|---|---|---|
| `planetmath` | volunteer | `:none` | **`:zombie`** | nobody — "*possible* donors, sponsors, or grant funders" |
| `gitcoin` | volunteer-shaped: self-selecting, unpaid until accepted | **`:per-unit`** | `:open` | **the bounty backer** |
| `andela` | trained engineers, matched | `:per-unit` | **`:scaled`** | the client's engineering/staffing line |
| `kaggle` | competitors, unpaid unless they win | `:aggregate` | `:acquired-sideways` | **the competition host** |
| `gnu-emacs-fsf` | volunteer | `:none` | `:sustained-noncommercially` | nobody — donors and supporters |

`gitcoin` against `planetmath` is decisive. The supply side is the same shape in
both — people who show up when they feel like it and are not paid to show up.
The terminals are opposite. The only difference is that someone on the **demand**
side posted a bounty and was thereby obliged to accept and pay for a unit.

## The rule, restated correctly

**What determines capture is whether the demand side carries a per-unit
obligation. The supply side's payment status is close to irrelevant.**

This is not a new finding; it is the corpus's own headline finding, which the
first version of this file managed to restate in the wrong vocabulary: all 32
cases terminate in `:interest`, and the recorded reason is **zero external
demand-side obligations**. `SPINE.md` already says it — *follow the chain until
it hits an obligation rather than an interest* — and the chain's obligation, if
it exists, is always on the demand side.

The coordination-layer observation survives, but has to be re-derived rather
than assumed. Coordination platforms capture **not because they solved the
supply side but because they sit where the demand-side obligation lands**:
Kaggle bills the host, Gitcoin the backer, Andela the client. None of them is
paid for labour; each is paid by the party that is obliged.

And PlanetMath's failure was never "it used volunteers". It is that **no one on
the demand side was ever obliged to accept a unit** — which is why its payer
field reads *possible* donors. A code-for-hire platform with an identical supply
side captures, because its client is obliged.

## What this does to the screening rule

The first version proposed reading an organisation's calls to action — "volunteer
with us" versus "we are procuring". That is a **proxy, and a weak one**. It is
cheap, so it is still worth a glance, but it can mislead in both directions:

- an organisation that asks for volunteers may still be sellable, if some funder
  or regulator on its demand side is obliged to a deliverable;
- an organisation that pays people may still be unsellable, if nobody is obliged
  to accept a *unit* — which is exactly the 32/32 result.

**The question to ask instead: who, on the demand side, is obliged — and to what
unit?** If that has no answer, what is being built is a calibration instrument
rather than a business, however much money is moving nearby.

## Withdrawn: the proposed schema addition

The first version proposed adding `labour-source` and `paid-layer` fields to all
32 records to make the claim checkable. **Withdrawn.** Those fields describe the
supply side, which the above shows is not the discriminator, so encoding them
would have bought a mechanical check on the wrong quantity. The corpus already
carries what matters — `:acceptance-event`, `:chain/:terminal` — and
`all-terminals-interest` already holds at n=32. No re-encoding is needed.
