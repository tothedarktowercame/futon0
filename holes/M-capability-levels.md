# M-capability-levels

**Status:** HEAD — operator-shape captured 2026-08-15, not yet IDENTIFY.
**Gate:** operator-acceptance — HEAD must be recognised as faithful before
IDENTIFY hardens it into a gap statement.

Per `futon4/holes/mission-lifecycle.md`: HEAD preserves the operator's voice
and carries tensions forward. **It is not design.** Nothing below prescribes a
level scheme; the finding and the tensions are the payload.

---

## The finding

On 2026-08-15 four fields — `:scale`, `:position`, `:next-rung`, `:curriculum`
— were added to every capability in
`futon0/holes/missions/M-capability-star-map.graph.edn`. Counting the result:

```
n = 37 capabilities
  status            satisfied 23 · held 13 · active 1
  attested          24
  pre-registered    35
  with :grounding   23
  curriculum        IMPLICIT 37        ← all of them
  scale sited        5                 ← of 37
```

**Every capability in the register is `:curriculum :implicit`.** Not one has a
scale, a position on it, or a named next rung. The register is rigorous about
*attainment* — 35 of 37 pre-registered, 24 attested, 23 with a grounding path —
and silent about *acquirability*.

Five are now sited, all from evidence produced the same day:

| capability | position | next rung |
|---|---|---|
| `cold-eoi-authored-outbox` | n=2 (hyatt, henderson) | a third lead drafted |
| `cold-eoi-sent` | n=2; hyatt 2026-07-05 22:02 BST, message-id witnessed | a third send |
| `cold-send-response` | n=1 (henderson call 2026-08-05) | a second response, different lead |
| `cold-response-conversion` | n=1 verbal, £5000 March, unsigned | **a countersigned MoU** |
| `warm-customer-pays` | n=1 (VSAT PoC — invoiced, paid, ledgered) | a second paying customer |

The edit is downstream-neutral: `futon6/scripts/starmap_to_capability_graph.bb`
produces byte-identical output with and without the new fields (verified by
running it against both versions), and `:missions` / `:edges` are untouched.

## Operator-voice anchor

Joe, 2026-08-15, in his own words across the session that produced this file:

> "We need to develop a capability for developing capabilities."

> "Demonstrating a sales capability by showing an MoU or a contract is 'OK' but
> it doesn't turn it into a teachable or learnable skill, it gives an 'implicit
> curriculum' — which has been fine and has served my intrinsic-motivation-heavy
> style pretty well so far."

> "IF you are trying to develop a capability HOWEVER that capability exists at
> different levels ranging from beginner to professional or expert THEN make
> sure that you set your yardstick so that you encompass all levels BECAUSE
> otherwise you arbitrarily cap the level of capability you could acquire."

> "'No scale existed' is exactly the failure mode that low-floor-no-ceiling
> corrects — it would insist that we determine a scale, not 'yonder mountain'
> but 'yonder 8,611 metre mountain'."

> "Capabilities tend to have a phylogeny, and possibly a complex one... they do
> not just live in a linear chain."

## Governing patterns

Written the same day, from the same audit, in `futon3/library/capability/`:

- **`low-floor-no-ceiling`** — site the yardstick past yourself. **Rule 0 is the
  one this mission turns on: construct the scale. "No scale exists" is a task
  not done, not a fact about the world.** A scale whose top is set by someone in
  the room is not a scale.
- **`attainment-is-not-acquirability`** — the four fields, and why marking
  implicitness is the whole move.
- **`succession-is-not-abandonment`** — capabilities have a phylogeny; expect
  branching and reticulation, not a chain.
- **`the-instrument-selects-what-you-cultivate`** — why the register was blind
  in exactly one direction.
- **`derive-the-claim-from-the-evidence`** — statuses should be computed, and
  the manual joint is where drift enters.
- **`every-investment-is-a-bet-against-the-others`** — siting 32 scales is
  itself an investment, and it displaces something.

## What's already felt to be true

- **The implicit curriculum has worked.** It is what an intrinsically-motivated
  operator runs on; the motivation supplied its own next rung, so none needed
  writing. This mission does not ask for that to stop.
- **Some scales are trivially sitable.** The cold chain went from `:held` with
  `n=0` to five dated positions in an afternoon, because the units were obvious
  once asked for: n sent, n responses, n conversions, £.
- **A found level-scheme already exists in the stack.** `futon6/src/futon6/` — 25
  modules, 87 to 1293 lines, 20 with test files nobody wrote for practice. Its
  levels were set by the problems, not by a curriculum author.
- **Splitting a capability sites it.** `cold-response-conversion` became
  legible once the chain was split four ways and the frontier landed on a named
  artefact (`README-conversion.md` §3).

## Anti-glibness discipline

What would make this mission superficial:

- **Inventing scales to clear `:tbd`.** A yardstick whose top is chosen by the
  person being measured is the failure `low-floor-no-ceiling` names, not its
  remedy. `:tbd` is an honest state; a fabricated scale is not.
- **Reading 37/37 as an indictment.** It measures a register that was never
  designed to carry levels. The number is a finding about the instrument's
  scope, not about the operator's competence.
- **Filling `:next-rung` with a project.** "Improve sales" is not a rung; "a
  countersigned MoU" is. The rung must be a named artefact or a demonstrable
  event, or the field has re-created the vagueness it was added to remove.
- **Treating `:implicit` as a defect to be eliminated everywhere.** The pattern
  asks for it to be *recorded*, so the absence is countable. Some capabilities
  may reasonably stay implicit forever.
- **Siting scales that nobody will climb.** Per
  `every-investment-is-a-bet-against-the-others`, levelling 32 capabilities is
  real work displacing other real work.

## Working-economy position

**What this underwrites:** the claim that the stack develops capabilities rather
than accumulating attainments — that a capability here can be improved
deliberately, delegated, taught, or explained to someone outside the room with a
position and a next step attached, rather than only re-demonstrated.

**What underwrites it:** the four fields are in the EDN and parse; the
downstream generator is proven neutral; five capabilities are sited from
same-day evidence with witnesses; and the two governing patterns are written.

## Clarity-gap / carried-forward tensions

1. **Are all 32 sitable?** Rule 0 says "no scale exists" is a task — but some
   may genuinely resist. What are the units of `symbol-grounding`? Naming the
   unsitable ones honestly is a legitimate outcome; assuming them unsitable
   without trying is not.
2. **Who sets the top?** An external population is required for the top not to
   be self-chosen. For some capabilities here there may be no external
   population at all, which is a different problem from a missing scale.
3. **Does every capability need levels,** or only those under active
   development? Untested.
4. **`:position` is currently prose.** Whether it wants structure — a number, a
   unit, a date, a provenance tag distinguishing *derived* from
   *operator-attested* — is deferred to IDENTIFY, but the third provenance
   state is already named in `derive-the-claim-from-the-evidence`.
5. **Splitting changes the scale.** The conversion chain became sitable by
   being split four ways. Whether splitting is a general precondition for
   siting, or a special case, is open.
6. **Phylogeny is unrepresented.** The register has `:scope` and `:minted-by`
   but no descent structure, and capabilities branch and merge (the War Machine
   has several parents). Levels assume a linear order; the phylogeny may not be
   one, in which case "which level am I at" is the wrong question for some
   capabilities and a different shape is needed.

## Provenance

**On the name.** Filed first as `M-capability-ladders`, renamed to *levels* by
the operator the same day: clearer, and it borrows CodeSignal's own lingo. That
borrowing is apt rather than incidental — the assessment that started this
whole line of work is scored by *levels completed*, its five levels accumulate
so that each may break the last, and its instructions say plainly that you need
not complete them all to advance. That is exactly the shape a capability wants:
an ordered set of demonstrable rungs, a position among them, and no requirement
to be at the top for the position to mean something. The word arrives already
carrying the semantics.

Generated in session with `claude-6` on 2026-08-15, following the post-mortem
of `~/code/python-practice/` (a seven-day instrumented capability build whose
own yardstick capped at "competent beginner" without anyone deciding it). The
37/37 figure was produced by adding the four fields to the star-map EDN and
counting; it is a measurement, not an estimate. Backup of the pre-edit EDN:
`/tmp/starmap-backup-1786807050.edn`.
