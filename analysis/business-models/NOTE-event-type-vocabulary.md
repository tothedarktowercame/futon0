# The event-type vocabulary, and what earns a place in it

**Date:** 2026-08-27 · Joe's direction, worked through by claude-13.

> *"`:warm-customer-pays`, `:engaged`, and `:declined-ground-unknown` need to be
> part of a considerably bigger vocabulary. If all we optimize on is
> `:warm-customer-pays` we might even omit to do the outreach whereby the warm
> customer would be 'kept warm' (Kangaroo logic applied to customers!).
> Effectively all these keywords are event-types in something close to Deleuze's
> lingo. I.e. `:declined-ground-unknown` is still `:declined` and that's a
> 'critical point'."*

## There are already two vocabularies, and they are unrelated

**A — demand-side response classes** (`lights.bb`), **eight, flat**:
`:engaged` · `:interest-no-offer` · `:declined-on-capacity` ·
`:declined-on-merit` · `:silent` · `:declined-ground-unknown` ·
`:forked-after-engagement` · `:unknown`

**B — the capability ladder** (`futon0/holes/M-capability-levels.md`), **five,
chained**:

| rung | position | next rung |
|---|---|---|
| `cold-eoi-authored-outbox` | n=2 | a third lead drafted |
| `cold-eoi-sent` | n=2 | a third send |
| `cold-send-response` | n=1 | a second response, different lead |
| `cold-response-conversion` | n=1 verbal | a countersigned MoU |
| `warm-customer-pays` | n=1 | a second paying customer |

## The keeping-warm worry, answered precisely — and it lands on the machine

**In the operator's ledger the worry does not bite.** Vocabulary B *is* the
precondition chain: authored → sent → response → conversion → pays. The outreach
steps are named, counted, and each carries its own next rung. Optimising the
terminal rung cannot starve them, because they are scored in their own right.

**In the machine it bites completely.** None of the five rungs appears in
`futon2/src/futon2/aif/observation.clj`'s fourteen channels, and
`warm-customer-pays` appears zero times there and in `efe.clj`. So the machine
sees neither the terminal event nor its preconditions.

**And this is why the naive R5 fix is worse than doing nothing.** Adding a
`:warm-customer-pays` channel would give the machine the terminal rung and none
of the four beneath it. Then Joe's worry becomes real *inside the machine*: the
outreach dimensions carry no score, so no value of them changes any selection —
**every precondition axis is a line of ordinary points.** The same failure as τ,
one domain over, and self-inflicted.

That is the Kangaroo reading exactly: an LRU pouch evicts what has not been
touched; a criterion set that scores only terminal events evicts every action
whose payoff is not terminal.

## What earns a place in the vocabulary

Joe's `:declined-ground-unknown` observation gives the criterion. It is *still
`:declined`* — the decline is the critical point; the ground being unknown is a
refinement that changes no downstream action. So:

> **An event type earns its own place iff its distinction leads somewhere
> different. Otherwise it is a refinement, and belongs as a *typed absence* on
> its parent.**

Applied to vocabulary A:

| distinction | leads somewhere different? | verdict |
|---|---|---|
| `:declined-on-merit` vs `:declined-on-capacity` | **yes** — *"you cannot consult these buyers into buying"* versus *"gated on resources, not on merit"*; different offers follow | two types |
| `:declined-ground-unknown` vs `:declined` | **no** — it records that we cannot tell | **not a third type**: `:declined` with the ground marked absent (I2) |
| `:silent` vs `:unknown` | **yes** — *we asked and got nothing* versus *no offer was ever made*; ask versus wait | two types |

So vocabulary A is not too small; it is **mis-shaped**. It has a flat slot where
a parent-plus-absence belongs, which is exactly the shape that makes a decline
uncountable as a decline.

## How the current granularity was actually chosen

`lights.bb` says so in its own comment:

> *"`other` is a real slot, not a dumping ground: it holds the classes that exist
> once each and would otherwise each demand **a hue nobody can tell apart from
> its neighbour**."*

**The granularity of the event ontology is currently set by how many colours a
reader can distinguish.** That is a rendering constraint deciding an ontology —
a defensible figure decision that has quietly become the vocabulary. The
criterion above replaces it with one about consequence.

## What a bigger vocabulary needs

Not more entries. Three things:

1. **Structure.** B is a chain and A is a list. A chain states preconditions and
   so cannot be optimised terminally; a list cannot state anything.
2. **Parent/refinement, with refinements as typed absences.** `:declined` is the
   critical point; `-on-merit`, `-on-capacity`, `-ground-unknown` sit under it,
   the last being an absence rather than a sibling.
3. **The membership test above**, so growth is by consequence rather than by
   noticing a case. Otherwise the vocabulary grows by editing a list — I1 — and
   a list is the paradigm dimension with no singularity on it.

## Related

- `futon3c/holes/excursions/E-R5-red-ring-fill.md` — the ring; this is its vocabulary half.
- `NOTE-evaluate-column-and-R5.md` — Klarna, the criterion set that did not state its boundary.
- `p4ng/empirics-futon/NOTE-singularity-and-discrimination.md` — the criterion used above.
- `futon0/holes/M-capability-levels.md` — vocabulary B, the chain.
