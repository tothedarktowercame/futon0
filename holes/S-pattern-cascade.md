# From spine to cascade: @why and @how on the war-room patterns

**Status:** specification, for dispatch. 2026-08-22 (claude-13 + Joe).
**Joe:** *"even though 'The spine: War-Room Patterns' is presented as a 'spine'
which suggests a linear order, maybe what would be helpful would be to organise
them further with @why and @how metadata … so that we could produce a pattern
cascade."*

## What already exists (checked, not assumed)

- **`@why` is already a convention** — 20 uses across the library. It is an
  **edge to another pattern**, not prose: `@why math-informal/local-to-global`
  on a formalization pattern means *this exists to serve that*. It points
  **up**, toward the more abstract.
- **An edge vocabulary already exists**: `@see` (48), `@ancestors` (35),
  `@illustrates` (23), `@up` (20), `@instantiates` (19), `@next` (19),
  `@childof` (10), `@specifies` (3).
- **A working cascade already exists**, in the math families: `math-strategy/*`
  ← `math-informal/*` ← `math-formalization-*`, three tiers joined by `@why`,
  over ~120 patterns. The thing Joe wants is not new — it is **unused in
  war-room**.
- **`@how` does not exist** (0 uses, 1{,}163 flexiargs). It is the one new field.
- **The war-room's 28 patterns carry zero edge fields of any kind.** That is
  precisely why they present as a linear spine: they have no other structure.

## The proposal

- **`@why <family/pattern>`** — up-edge to the pattern this one serves. Existing
  convention, applied to war-room for the first time.
- **`@how <ref>`** — down-edge to the **mechanism that realises it**: an
  R-number, a mission id, or an artefact path. Deliberately *not* the inverse of
  `@why` (that is derivable and would carry no new information); `@how` leaves
  the pattern layer and lands in the implementation.

A cascade then has the same shape as
`futon7/holes/M-futon-forward-model.backlog-cascade-merged-v0-brief.html`:
patterns are **boxes**, `@why` edges are **wires**, `@how` targets are
**terminals** — and a pattern with **no `@how` is a hole.**

## Why this matters more than tidiness

**CORRECTION (2026-08-22, after annotating): this is false as stated.** The
claim below was that a red ring means a pattern with no mechanism. Annotating
the ten spine patterns against `wr-overlay.edn` falsifies it: `WR-25` is hollow
at \(R5\) but **holds** at \(R9\) and \(R12\); `WR-19` is hollow at \(R6\)
but **holds** at \(R17\). A pattern can be satisfied by one mechanism and
unmet at another node.

So `@how` is a **set**, and a red ring is a *(node, pattern)* pair whose
requirement is unmet at that node --- not a pattern with nothing behind it. The
vocabulary now used in the flexiargs reflects that: `@how` lists the mechanisms
that hold, `@how-open` lists the nodes where the pattern's requirement is not
yet met.

The sharper finding survives, and is stronger for being narrower:
**`WR-27` is the only spine pattern with no holding mechanism at all** --- it is
`@how-open` at \(R8\) and \(R14\) and holds nowhere. `WR-19`, which the paper
leans on hardest, does hold at \(R17\); what it lacks is a *generative* source
at \(R6\), which is a different and more precise complaint than "unbuilt".

The superseded claim, kept for the record:

> **The four red rings are exactly the war-room patterns with no `@how`.**

| ring | node | pattern | missing mechanism |
|---|---|---|---|
| R5 | EFE core | WR-25 | good-news channel |
| R6 | candidate action space | WR-19 | tension-proposer |
| R8 | present-fit mismatch | WR-27 | gain meter (per tick) |
| R14 | commitment temperature | WR-27 | gain meter (per commitment) |

Today those rings are **hand-maintained** in
`p4ng/empirics-futon/wr-overlay.edn` — someone decided which badges are hollow
and wrote them down. With `@how` populated, hollowness is **derived**: a badge
is hollow iff its pattern has no `@how`. The figure stops being an assertion and
becomes a reading, which is the same move WR-8 makes everywhere else (typed
files are the source of truth; prose is regenerated).

That also makes the overlay *self-maintaining*: ship the tension-proposer, add
`@how` to WR-19, and the ring fills on the next regeneration without anyone
remembering to edit an EDN file.

## Dispatch shape

1. Add `@why` + `@how` to the 10 spine patterns first (WR-0, 4, 8, 9, 16, 19,
   24, 25, 26, 27) — the set the paper already carries.
2. A generator `gen_wr_cascade.bb` emitting the cascade graph from the
   flexiargs, in the brief's box/wire/terminal/hole vocabulary.
3. Re-derive `wr-overlay.edn` from `@how` presence rather than by hand; the
   four rings must come out unchanged, which is the acceptance test.

Step 3 is the one that pays: it converts Figure 2 from a hand-drawn claim into a
generated one, and it fails loudly if a pattern's mechanism is removed.


## Annotation as built (2026-08-22)

Ten spine patterns annotated. `@why` is an up-edge to the pattern served,
derived from the conclusions; `@how` is taken **only** from `wr-overlay.edn`'s
holding entries, never invented.

| pattern | `@why` | `@how` (holds) | `@how-open` |
|---|---|---|---|
| WR-0 | `root` | — | — |
| WR-4 | WR-0 | — | — |
| WR-8 | WR-0 | — | — |
| WR-9 | WR-8 | — | — |
| WR-16 | WR-4 | R2 | — |
| WR-19 | WR-0 | R17 | **R6** |
| WR-24 | WR-9 | R13, R15 | — |
| WR-25 | WR-9 | R9, R12 | **R5** |
| WR-26 | WR-8 | R20 | — |
| WR-27 | WR-16 | **—** | **R8, R14** |

`WR-0` carries `@why root` explicitly, so a generator can tell a terminal from
an unannotated file.

**Still open.** WR-4, WR-8 and WR-9 have no `@how` because they place no badge
on the control map --- they are not control-loop patterns, and their mechanisms
are artefacts (typed files, generators, the regeneration discipline) rather than
R-nodes. Assigning those honestly needs a pass over the artefacts, not a reading
of the overlay, and is not done here. Note that this paper is itself an instance
of WR-8's mechanism, which is a candidate `@how` but should be verified before
being asserted.
