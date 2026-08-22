# The four red rings as four problems

**Status:** specification, for dispatch. 2026-08-22 (claude-13 + Joe).
**Source:** `p4ng/empirics-futon/wr-overlay.edn` — 16 badges, 4 hollow.
**Joe, 2026-08-22:** *"think about what those interfaces might actually look
like, and what would change if they existed. Maybe those are 4 problems we can
solve!"*

The four hollow badges sit on named nodes of the AIF control map. Read together
they are **not four independent gaps** — they are three kinds of thing, and the
split matches `p4ng/SPINE-vertical.md` §8 (which instruments may be
self-supplied):

| ring | node | pattern | kind |
|---|---|---|---|
| **R5** | Expected free energy core | WR-25 | the **landing site** for a verdict from outside |
| **R6** | Candidate action space | WR-19 | the **generator** |
| **R8** | Present-fit mismatch | WR-27 | the **gain meter** |
| **R14** | Commitment temperature | WR-27 | the same meter, second site |

R8 and R14 carry the *same* pattern (WR-27, *a loop is born instrumented for
its gain*) at two sites. So this is three interfaces, one of them built twice.

---

## Ring 1 — R5 / EFE core / WR-25: the good-news channel

**Broken.** *"good news gets the same discipline as bad — `:warm-customer-pays`
satisfied, uncounted, unsurfaced."* A capability rung backed by a real invoice
is `:satisfied` and no consumer can see it: star titles read n=0 while the cold
ladder actually stands at 2 authored / 2 sent / 1 response / 1 verbal offer
unsigned, and `starmap_to_capability_graph.bb` does not project `:position`.

**Interface.** A satisfaction channel with the *same* shape as the failure
path: a rung flipping to `:satisfied` emits a typed evidence entry carrying its
witness reference, `:position` is projected by the star-map generator, and the
EFE core's preference term reads satisfied rungs rather than only deficits.

**What changes.** Today an action class that *worked* is indistinguishable from
one that was ignored — the same confound that produced an intrinsic value of
0.009 across 108 emissions. Good news currently has no route into the loop at
all, so the C vector can only ever learn from disappointment. Note what this
ring is and is not: it does **not** supply the verdict (that is SPINE layer 6,
external, `mh7`). It builds *everything except the judgement*, so that a verdict
arriving from outside has somewhere to land.

**Acceptance.** Count the satisfied-but-unsurfaced rungs today; after the
channel, that count is zero and `:warm-customer-pays` appears in both the
projected star position and the preference term.

## Ring 2 — R6 / candidate action space / WR-19: the tension-proposer

**Broken.** *"the candidate space is ranked, not proposed; tension-proposer
unbuilt."* ΔT is a sorry-only tie-breaker, so a manifold full of tension yields
nothing when the proposer registries are dry.

**Interface.** An S1 source that emits candidate actions at high-tension
regions, in the same shape the four fixed proposers emit, so `judge` needs no
change.

**What changes.** The first outing exhausted its proposer-fed queue into
`learn-action-class` before any outward open-mission surfaced; structural
pressure was applied only *after* four fixed proposers had supplied candidates,
and only to address-sorry candidates. With a generator, the field can drive the
head. This is also the serendipity mechanism (`p4ng/NOTE-bridge-as-surprisal.md`):
a Bridge is definitionally absent from the menu, so only a generative source can
reach one — a ranking signal never can.

**Acceptance.** Replay the first-outing scenario; the proposer must surface at
least one candidate the four fixed proposers do not, and `judge` must accept it
unmodified.

## Rings 3 and 4 — R8, R14 / WR-27: born instrumented for its gain

**Broken.** R8: *"the per-tick mismatch IS the gain reading, and the outer loop
has been dead since 2026-07-14."* R14: *"commitment temperature is explicitly a
gain; instrumented at birth or diagnosed retroactively."*

**Interface.** A gain meter emitting a typed reading per tick (present-fit
mismatch) and per commitment (temperature), with the loop **refusing to run
uninstrumented** — which is what *born* instrumented means, as against
diagnosed later.

**What changes.** Two things, one of them the point of the whole paper. First,
`futon2/scripts/wm_outer_loop.clj` currently rewards *follow-through* — "a new
`M-*.md` file appears in git" — which is a **compliance meter, not a gain
meter**, and it has been `ERROR nil` since 2026-07-14. Second, this is the
internal-criterion half of the instrument: its standard is *does the loop's
prediction match what happened*, which needs nobody outside to answer, so it can
be built in full. R14 additionally lets `nag`/`brief`/`silent` be chosen on a
measured commitment level instead of a policy constant — the operator-model gap
in `p4ng/sec-operator.tex`.

**Acceptance.** N ticks produce N mismatch readings; a tick that cannot emit one
**fails loudly** rather than silently; and the reward path reads the gain rather
than the file-appears count.

---

## Why these four and not four missions

The merged cascade was probed for current state on 2026-08-22 **by reading each
mission's `**Status:**` header — which was the wrong method.** Joe corrected it
immediately: `m3` M-text-sidecar reported *"OPEN — IDENTIFY drafted, awaiting
Joe's read"* and is in fact **DONE**. Verified by exercising the capability
rather than reading about it: `GET /api/alpha/evidence/text-search?q=…` returns
scored FTS5 hits, and a nonsense control returns `:count 0`. The cascade brief's
own `:partial — D1 LIVE` was closer to the truth than the mission file, and the
live endpoint was closer still.

**Method correction (DP5 again, third time today): probe a capability by
exercising it, not by reading its status header.** Status lines are written once
and drift; endpoints do not. Any re-probe of the remaining 22 should call the
thing.

With that said, the rings remain the dispatchable set — they are interfaces with
a named failing consumer and a test each — but the framing below was incomplete.

## The fourth kind: the retrieval leg (Joe, 2026-08-22)

R5 / R6 / R8 are all **control-loop** interfaces: landing site, generator, gain
meter. Text-sidecar is none of those, which is why it did not appear — it is a
**retrieval** capability, and Joe's point is that it *"is exactly the one that
enables digging into operator turns."*

It is done, and it already answers, approximately, the question D8 said was one
typed field away. `text-search` honours `author=`, so operator turns can be
searched by topic **today**. Measured 2026-08-22, `author=joe`, untruncated:

| query | joe-authored turns |
|---|---|
| `apm-demonstration` | **1041** |
| `war+machine` | 380 |
| `capability+star` | 147 |
| `futon-problems` | **93** |
| `text-sidecar` | 2 |

That is the futon-problems-vs-apm-demonstration comparison Joe imagined, at
roughly **11:1**, available with no new field at all.

**What it is not.** Mentions are not attention: these are lifetime counts, not
windowed; a long-running mission accumulates more of them; and a turn that
merely names a mission is scored the same as one that works on it. So the typed
session→mission binding (D8) still buys *exact* attribution. But the
approximation is free, it is live, and it should be the baseline any exact
measure has to beat — which is a better first deliverable than the binding
itself.

**Consequence for dispatch order.** A fourth packet, cheaper than the three
rings: an attention view over `text-search` with `author=` and a time window,
reported per mission. It has a consumer (the investment-case argument in
`p4ng/sec-operator.tex`), a test (counts reproduce), and it does not wait on
anything.

They also give the sentence Joe asked for, per ring: *using this stack let us
ship X, which achieves Y, with practical implication Z.* For ring 3 that reads:
shipped a per-tick gain meter, which replaced a compliance count that had been
dead for six weeks, so the loop can now tell a prediction that held from one
that did not.
