# M-futonzero-prelim-practice

**Status:** HEAD — operator shape captured 2026-08-15. Not yet IDENTIFY.
**Phase discipline:** per `futon4/holes/mission-lifecycle.md`, HEAD preserves the
operator's voice and carries tensions forward. It is not design. Nothing below
specifies a curriculum.

---

## Operator-voice anchor

Joe, 2026-08-15:

> "Rather than deferring it, maybe we should take a prelim problem or two and see
> if we can adapt the python-practice routines to prelim-practice. […] What we
> don't have immediately is the 'why', which is maybe a good question to be
> asking *myself* after 25 years or so! […] Maybe it's interesting to think about
> a 'joe-class' version of what we have planned for Zai, even before we get
> through all of the Zai work, because the Joe-class version could inform how we
> think about the Zai work and even how we think about the Python work."

**The load-bearing claim is the last one: run the human version first because it
is cheaper and it informs the machine version** — not because it is a toy.

## Facts on the ground (verified 2026-08-15)

`~/code/apm-lean/problems/` holds **475 problem bundles**. Each carries:

```
problem.tex · problem.md · informal-solution.md · proof-outline.md
candidates/ · lean/ · status.json · README.md
```

- **462 of 475 have an `informal-solution.md`.**
- `status.json` carries `lean_closure: {classification, main, sorry_count,
  blocker, proved_helpers}` — where `blocker` is *prose naming what remains*,
  which is unusually good material.
- Some bundles carry **multiple candidate frames** with epoch-ms ids
  (`apm-v2-a01A01-1774990784258`), i.e. an attempt log with timestamps.

**Two data-quality facts that constrain any scale built from this:**

1. **The classification vocabulary is fragmented — 15 distinct values** across
   475 records: `partial` 267, `complete` 86, `informal-only` 65, `proved` 20,
   `partial-lean-proof` 10, `solved` 8, `complete-lean-proof` 5,
   `statement-defective` 3, and one-offs including
   `formalization-partial-missing-sylow-partition`. It needs collapsing to a
   normal form before it can order anything.
2. **The finer signals are sparse.** `sorry_count` appears in **2** records;
   `candidate_count` in **10**. Whatever the attempt history is, it is mostly
   not in `status.json`.

**A number that did not reconcile — RESOLVED 2026-08-15, same day.**
The operator's figure is **205/491 sorry-free**; grouping `status.json`'s
done-family labels gave **121 of 475**. The resolution is that
**`status.json`'s `classification` is a stale hand-authored label and the
operator's figure is derived live from the Lean sources.**

`futon0/contrib/stack-hud.el` (`stack-hud--apm-scan`, metric version
`current-lean-code.v1`) computes both figures from the filesystem on a 120s
cache: `:total` counts `.tex` files in `apm-lean/apm/`, and `:lean-clean`
counts problems whose `problems/ID/lean/` contains zero **executable proof
holes** — where a hole is a `sorry` token *or a definition-free `opaque`
declaration*, the latter introducing no `sorryAx` and so passing
`#print axioms` clean while still being a hole. Only `problems/ID/lean/` is
current; candidates and history are excluded.

Independently reproduced the same day with a separate implementation:

```
491  statements (.tex in apm/)          matches :total
475  problem bundles
462  with informal-solution.md ≥100B    matches :informal
438  with Lean material                 :lean-total
203  Lean-clean                         :lean-clean  (205 under stack-hud's
235  with holes                          stricter opaque test — a 2-problem
450  holes in total                      gap in the opaque heuristic, not a
                                         disagreement about the corpus)
```

**This is `capability/derive-the-claim-from-the-evidence` with the roles
reversed from the usual case:** here the *derived* number was right and the
*curated* register was stale, which is why the discrepancy resolved in
stack-hud's favour. The lesson stands unchanged — trust the thing computed from
the substrate — and `status.json`'s 15-value classification vocabulary should
be treated as commentary, not as a status.

**Consequence: rule 0 is already satisfied.** The 235 hole-carrying problems are
ordered by hole count, from an external, machine-checkable signal that nobody
in this room authored. `capability/low-floor-no-ceiling` asks for exactly that
and the Python work had to go looking for it; here it exists and is live.

## The register chain (verified 2026-08-15) — why teachability is checkable

Every Lean-clean problem carries the same four altitudes of one proof:

| register | artefact | present |
|---|---|---|
| statement | `problem.tex` | 205 / 212 |
| skeleton | `proof-outline.md` | **212 / 212** |
| prose proof | `informal-solution.md` | 202 / 212 |
| formal proof | `lean/` | 212 / 212, zero holes |

Two things follow, and they are the reason this scoping is strong.

**It is a chained specification, anchored at the evidence end.** Each level is a
translation of the one above across a *register boundary* — statement to
skeleton to prose to Lean — and a translation that must preserve meaning while
changing vocabulary is a genuine check, unlike a paraphrase at the same
altitude. The bottom link is externally verified: the Lean compiles or it does
not. So consistency between levels is checkable rather than a matter of taste,
and the whole chain is anchored by something nobody in the room authored. This
is the structure the operator named on 2026-08-15 as producing a gain-of-
function, occurring here naturally rather than by design.

**It yields exercises with an answer key, at the operator's own altitude.**
Given `problem.tex` + `proof-outline.md`, can the learner produce
`informal-solution.md`? That is a real exercise, its key already exists, and the
key's correctness is underwritten by the Lean beneath it. No held-out set is
required, because the exercise is *generated from* the solved problem rather
than reserved from it.

Note also that `proof-outline.md` is typically **longer than** the informal
solution (7,343 vs 2,958 bytes on `a00J01`). Whatever the outlines are, they
are not summaries, and what they actually contain is a MAP question.

## What's already felt to be true

- **The scale exists here, and it is externally set.** Unlike Python, where
  rule 0 of `capability/low-floor-no-ceiling` had to be applied retrospectively,
  this corpus comes with a machine-checkable closure signal nobody in the room
  authored. Lean either compiles or it does not.
- **There is a real oracle, and it can be compared on *how*, not only
  pass/fail.** Codex attempts exist, informal solutions exist for 462, and some
  problems carry several candidate frames. Per-problem method comparison is
  available in a way it never was for the Python drills.
- **The two axes from python-practice transfer.** *Problem difficulty* (can it
  be closed at all) and *conversion cost* (what it took) are exactly Sen's
  capability frontier and conversion factors, per
  `M-futonzero-capability.md` §2.1.

## Anti-glibness discipline

- **Contamination is largely dissolved by the operator's scoping (2026-08-15):
  work the *solved* problems and ask whether they are teachable.** The solution
  existing is then the precondition, not the leak. This mission is therefore
  **not** a measurement of whether Joe can solve prelims; it is
  `capability/attainment-is-not-acquirability` applied to a corpus that has the
  material — turning ~205 functionings into something with a scale, a position
  and a next rung.

  *Where it still bites, narrowly:* if Joe is also the learner on a problem
  whose informal solution he has read, that particular exercise measures
  recall, not acquisition. With 491 problems this is manageable by selection
  rather than by design, and it should be recorded per-problem rather than
  assumed away.

  **Operator report, 2026-08-15: he has read none of the solutions, formal or
  informal; uncertain about the outlines.** That uncertainty turns out not to
  matter, for two reasons. (a) Structurally, contamination in the *input*
  register is harmless — the exercise takes statement + outline as given and
  asks for the informal solution, so only the output register must be unseen,
  and it is. (b) Empirically, the outlines are not solutions: they are
  machine-generated formalization scaffolding, self-described as *"imported
  automatically and may need cleanup"*, carrying a `:goal` restatement and a
  `:terms` list with `needed-because` annotations. They supply the question and
  the vocabulary, not the argument.

  *Residual, stated so it is not lost:* some `:goal` fields decompose the task
  ("state the definition … and prove the limit"), which is a structural hint
  even though it is not a proof. Whether that hint is a leak or legitimate
  scaffolding is a DERIVE decision, not a fact.
- **Lean closure is not mathematical difficulty.** It measures *formalization
  burden*. A problem can be mathematically routine and formally brutal, or the
  reverse. Using closure as a difficulty proxy without saying so would site the
  scale on the wrong axis — the same error as measuring Python by drill level.
- **"The oracle did it differently" is not "the oracle did it better."**
  Method comparison needs an adjudicator that is neither participant, or it
  becomes self-assessment. This is `M-wm-demonstration`'s L3 barrier in
  miniature.
- **Fix the standard before the first attempt.** Per
  `M-wm-capability-claim.md`'s retrofitting trap: any threshold set after
  looking at results is worthless.
- **Do not psychologise the 25-year question.** See below.

## On "why" — what this instrument can and cannot address

The operator's remark has two readings and they should not be conflated.

**The methodological one, which is in scope:** *why this proof strategy rather
than another.* That is precisely the War Machine's selection-explanation
problem at human scale — `M-wm-demonstration`'s W-why node, where mechanism
explanation ("this term governed the pick") is recovered and justification
("this was the right pick") is open. A prelim attempt where the strategy choice
is recorded *before* the outcome is known is a small, cheap instance of exactly
that, and it may be the most transferable thing this mission produces.

**The personal one, which is not:** why do this at all, after 25 years. That is
the operator's question and this mission has no instrument for it and should
not pretend to one. It is recorded here because it was said, and because a
mission that silently dropped it would be dishonest about its own provenance.

## Working-economy position

**Underwrites:** the Zai work (this is its human-class control), the Python
sprint (which currently lacks an externally-set scale and here has one), and
`M-apm-demonstration`, which the operator reports is proving more tedious to
implement than expected.

**Underwritten by:** 475 bundles with informal solutions, an existing oracle, a
machine-checkable closure signal, and a domain in which the operator has 25
years of standing — which is what makes the human arm cheap to run.

**Cheapest of the three programmes**, and it is the only one where the subject
already knows the domain, so it isolates *retrieval and selection* from *domain
knowledge* in a way the Python work could not.

## Clarity-gap / carried-forward tensions

1. **205/491 versus 121/475.** Unresolved. Blocks any scale.
2. **Contamination boundary.** Which problems are genuinely unseen by the
   operator, given he wrote or reviewed much of this corpus? This may be the
   binding constraint, and it may be worse than it looks.
3. **Does "how" comparison need a rubric?** Or is a prose diff of two proof
   outlines enough to be informative? Untested either way.
4. **Formalization burden versus mathematical difficulty** — needs a second
   signal, or an explicit statement that only the first is being measured.
5. **Sparse attempt history.** If `candidate_count` is present in 10 of 475,
   the machine's own struggle is mostly unrecorded — which is the same
   not-logged-versus-not-done confusion diagnosed across the stack today.

## Provenance

Session with `claude-6`, 2026-08-15, at the end of a day that ran from the
python-practice post-mortem through the capability register, the conversion
chain, and `M-wm-demonstration`. Generated by inspecting
`~/code/apm-lean/problems/` directly — 475 `status.json` files parsed, not
sampled — after a day in which every conclusion drawn from a derived artefact
rather than the substrate turned out to need correcting.

## Next

IDENTIFY, once MAP has answered: **which ledger is authoritative, and how large
is the genuinely-unseen set?** Everything else waits on those two numbers.
