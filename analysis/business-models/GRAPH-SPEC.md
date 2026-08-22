# GRAPH-SPEC — the typed-edge graph, and how it updates

**Date:** 2026-08-21 · **Status:** spec.
**Companions:** `RECORDS-SPEC.md` (record shape), `records/*.edn` (30 records),
`SCHEMA.md` (findings), `SPINE.md` (architecture).

## Why a graph, and what the join key is

Each record's `:chain` is a path — `:artifact -> :sponsor -> :payer ->
:terminal` — and all four slots are populated in all 30 records. But the slots
hold **prose descriptions, not node identities**:

```
andela        :sponsor "Andela matching and employment apparatus"
galois-inc    :sponsor "government programme or customer project"
trail-of-bits :sponsor "security-review engagement"
```

Build naively and you get ~120 unique strings with zero joins — a picture, not
a graph. The whole value is **shared nodes**.

**The join key is the budget line.** Look at the same three records' payers:

- "client company's engineering/staffing line item"
- "agency/customer R&D line item"
- "customer's security or product-engineering line item"

Every payer in the corpus is *a line item in somebody's budget*. Companies are
not the interesting nodes; **budget lines are**, and companies hang off them.
Two ventures are adjacent when they are paid from the same line — which is what
makes "who else could this serve" a graph query instead of a brainstorm.

## Node kinds

| kind | id source |
|---|---|
| `:case` | `:case-id` |
| `:budget-line` | controlled vocabulary (`budget-lines.edn`) |
| `:node-type` | the 7 rows (`:requirement` … `:market-exchange`) |
| `:terminal-kind` | `:interest` `:obligation-external` `:obligation-manufactured` `:none` |
| `:problem-holder` | **may be absent** — see `serves` |

Artifact and sponsor stay as *attributes* of the case node, not separate nodes,
until there is a reason for them to be shared. Minting a node per prose string
is the failure mode this spec exists to avoid.

## Edge types

| edge | from → to | typed by |
|---|---|---|
| `produces` | case → artifact-label | — |
| `adopts` | case → `:node-type` | primary / secondary |
| `pays` | case → case-sponsor | `:acceptance-event` (per-unit / aggregate / none) |
| `drawn-from` | case → `:budget-line` | **the join key** |
| `obliged-by` | case → `:terminal-kind` | external / manufactured / interest |
| `operates` | case → `:node-type` | only where `:seat :operating` (4 cases) |
| `serves` | case → `:problem-holder` | `:beneficiary` gradient |

### `serves` is structurally different

Every other edge connects things we have encoded. **`serves` is the only edge
whose target may lie outside the graph** — a problem-holder nobody has named.
It is `:unknown` in 24 of 30 records.

That gives the serendipity structure a graph form:

- **a dangling `serves` edge** (no target) = an unattached **capability** bank
- **a `:budget-line` with no incoming `drawn-from`** = an unattached **need**
- **a bridge** = a candidate edge joining the two

A join, not a metaphor — and gradeable, because the world either accepts the
join or does not.

## The budget-line vocabulary

`budget-lines.edn`:

```clojure
{:engineering-headcount
 {:label "Engineering / staffing headcount"
  :evidence [{:case :andela :quote "client company's engineering/staffing line item"}]}}
```

**Rules.**
1. **No line without evidence.** Every line carries ≥1 `{:case :quote}` pair,
   quoting the payer text verbatim from the record.
2. **No assignment without a quote.** A case's `:budget-line` is justified by
   its own payer text, not by what the company "obviously" is.
3. **`:unclassified` is a legitimate answer.** If a payer slot does not clearly
   belong to a line, assign `:unclassified` and say why. Forcing a fit
   manufactures adjacency, which is worse than no adjacency — the
   `:unknown`/`:indeterminate` discipline that kept the conjecture checker
   honest applies here with more force, because a false edge propagates.
4. Expect roughly a dozen lines. If the vocabulary approaches 30, it is not a
   vocabulary — it is the prose again, and the pass has failed.

## Updating topology

**The graph is DERIVED, never hand-edited.** `graph.bb` reads `records/*.edn`
plus `budget-lines.edn` and emits `graph.edn`. Adding a record re-derives
everything — same discipline as `check.bb`.

Each run also writes `graph-snapshot.edn` and **diffs against the previous
one**, because the interesting output is not the static picture but *what
changed when a node arrived*:

- budget lines that gained or lost an occupant
- `serves` edges that became attached (a bank got bridged)
- components that merged or split
- newly orphaned budget lines

**Report per run:**
- counts by node kind and edge type
- **occupancy per budget line, descending** — lines with several occupants are
  crowded; lines with one are either a niche or a mistake in the vocabulary
- **dangling `serves` edges**, by case — the capability banks
- **orphan budget lines** — the need banks
- connected components, and the largest
- the diff versus the previous snapshot (or "first run")

## Explicitly out of scope

**Do not merge with `futon6/data/business-landscape-embed.html`** (106 records:
org / vacancy / funder / capital / convening). Different namespace. A bad merge
creates false adjacency, which is the exact failure this spec is built to
avoid. When the two are joined it will be by explicit `same-as` edges, authored
one at a time with evidence — not in this pass.
