# H7c-2: the construction library's moves as move functions

Repo: /home/joe/code/futon2 (shared checkout, branch master, HEAD d1688d4b).
Other agents are working in this repo right now. **Stage explicit paths; never
`git commit -a`; never `git add .`.** You own exactly two new files:

  - `src/futon2/aif/construction_moves.clj`   (new)
  - `test/futon2/aif/construction_moves_test.clj` (new)

Do **not** edit `construction.clj`, `war_machine.clj`, `active_horizon_g.clj`
or anything else. If you think another file must change, say so in your report
instead of changing it.

## Goal

`futon2.aif.construction/construct` (read it first — `src/futon2/aif/construction.clj`)
runs a construction-level policy whose MOVES are injected functions. Today the
caller has to write those functions by hand. Implement the first four moves of
claude-7's library, in `/home/joe/code/futon3/library/cascade-construction/`:

  - `read-what-exists-first.flexiarg`
  - `borrow-a-sibling-cascade.flexiarg`
  - `order-by-what-each-step-needs.flexiarg`
  - `add-a-pattern-when-an-item-fits-no-class.flexiarg`  (here: add a CHECK for
    an unknown fact — the gap this one names, in the construction runtime, is a
    gating fact nothing observes)

**Read each flexiarg before implementing its move.** Each has an `@done` line;
that line is the move's termination condition and must be what makes the move
return `:no-move`. Each has an `@epistemic-value` line; that is what the move's
`:epistemic-estimate` records.

## Contract every move must satisfy

Each move is produced by a CONSTRUCTOR: a function taking injected data and
returning `(fn [family] -> result)`, so `construct` stays pure. `family` is a
vector of candidates; a candidate is

    {:precedence [pattern-id ...]     ; the cascade's ordered pattern ids
     :patterns   [pattern ...]        ; the pattern maps themselves
     :construction-receipt ...}       ; optional, carried verbatim

and a pattern is

    {:id pattern-id
     :guard {:needs #{token ...} :forbids #{token ...}}
     :produces #{token ...}}

(checks made by `futon2.aif.check-candidates` look like
 `{:id :check/f :kind :check :fact f :guard {:unknown #{f}} :opens f :produces #{} :theta θ}`.)

A move returns EITHER

    {:move-id :some-move
     :proposed-family [candidate ...]
     :epistemic-estimate {:value v :kind :state-information|:novelty :basis ...}
     :cost c}

or the typed no-move

    {:status :no-move :move-id :some-move :reason :some-keyword}

`construct` adds `:epistemic-estimate` to a move's value ONLY when its `:kind`
is `:novelty`; `:state-information` is recorded and not added, because
`active-horizon-g` already values a check's state information (read the
`evaluate-moves` docstring). **None of these four moves may use `:novelty`** —
all of them record `:state-information`, so none of them can inflate its own
value. Say in your report if you think that is wrong for one of them.

Moves must be PURE FUNCTIONS OF THEIR INPUTS: no file reads, no network, no
substrate access inside the returned move fn. Anything read from the world is
injected by the constructor's caller.

## The four moves

### 1. `read-what-exists` — "read the requirement, the implementation, the live state and prior attempts"

    (read-what-exists {:sources {:requirement {...} :implementation {...}
                                 :live-state {...} :prior-attempts {...}}
                       :available-patterns [pattern ...]
                       :cost 1})

`:sources` values are either `{:read? true :locus "..." }` or
`{:unavailable :reason-keyword}`. `:available-patterns` are the patterns those
sources establish exist for this target (injected — in production these come
from the target's declared cascade source, `resources/wm/cascade-sources/*.edn`,
loaded by `futon2.aif.cascade-sources`; do not read that file here).

Proposed family: the current family plus ONE new candidate, built from the best
existing candidate extended with the available patterns no candidate in the
family already uses (by pattern id), appended to `:precedence` in the order
given.

`:no-move` reasons: `:nothing-unread` when every available pattern is already
used by some candidate; `:sources-not-supplied` when `:sources` lacks any of
the four keys (per the `@done` line: read or *recorded unavailable* both count
as supplied; a missing key does not).

Record in the result `:sources-read` (the keys with `:read? true`) and
`:sources-unavailable`, and `:epistemic-estimate {:kind :state-information
:value <count of sources recorded> :basis :read-what-exists-first}`.

### 2. `borrow-a-sibling` — "walk the sibling's rows"

    (borrow-a-sibling {:sibling {:id ... :rows [{:row :observe :pattern-id ...} ...]}
                       :counterpart (fn [row family] pattern-or-nil)
                       :row-of (fn [pattern] row-keyword-or-nil)
                       :cost 1})

Walk the sibling's rows in order. A row is COVERED when some pattern in the
family has that row under `:row-of`. For the first uncovered row, call
`:counterpart`; if it returns a pattern, the proposed family is the family with
that pattern added to the best candidate (`:patterns` and the end of
`:precedence`); if it returns nil, that row is a recorded GAP and the move moves
on to the next uncovered row in the SAME call.

The walk is finite (the flexiarg's `@done`): when every row is covered or
recorded as a gap, return `:no-move` with reason `:sibling-rows-walked`. Return
`:gaps [row ...]` in the result either way — a gap is a finding, never filled
with an invented pattern.

### 3. `order-by-need` — "let the guards carry the order"

    (order-by-need {:cost 0})

No injected data. For each candidate, reorder `:precedence` so that a pattern
whose guard `:needs` token t comes after the pattern whose `:produces` contains
t. Use a stable topological sort: among patterns with no dependency between
them, keep the existing relative order (the flexiarg says: where two patterns
need nothing from each other, do not invent an order). A need produced by no
pattern in the candidate is an UNMET NEED: record it (`:unmet-needs`), do not
reorder around it — per the flexiarg these are exactly the check candidates
move 4 turns into checks. A dependency cycle: leave that candidate's order
unchanged and record it under `:cycles`.

`:no-move` reason `:already-ordered` when no candidate's `:precedence` changes.

### 4. `add-a-check` — "a gating unknown fact becomes a check"

    (add-a-check {:facts {fact true|false|:unknown} :want [token ...]
                  :check-theta 0.9 :cost 1})

Call `futon2.aif.check-candidates/check-patterns` with those inputs and the
family's patterns (union over candidates, de-duplicated by `:id`). It returns
`{:checks [...] :not-gating [...]}` or throws `ExceptionInfo` with a typed law
in `ex-data`. Take the FIRST returned check whose `:id` is not already in the
family, and add it to every candidate (`:patterns`, and `:precedence` — put the
check at the FRONT, since a check must be able to act before the patterns that
need its fact). One check per move, so the move budget stays meaningful.

`:no-move` reasons: `:no-unknown-gating-fact` (no checks, or all already
present); `:check-candidates-refused` with the law under `:detail` when
`check-patterns` refuses; `:inputs-not-supplied` when facts/want/check-theta
are missing.

`:epistemic-estimate {:kind :state-information :value <θ> :basis :check-candidates}`.

## Tests (`test/futon2/aif/construction_moves_test.clj`)

Pure, deterministic, no substrate, no registry, no network. Cover, at minimum:

- each move's happy path (the proposed family is what the spec above says);
- each move's `:no-move` reason, by its own `@done` condition;
- move 3's stable order: independent patterns keep their written order;
- move 3's unmet need is recorded, not reordered around;
- move 2's gap row is recorded and NOT filled;
- **termination**: one integration test that runs `construction/construct` with
  all four moves injected, a stub `:evaluate-g` (a pure fn of the candidate, e.g.
  fewer unmet needs = lower G), `:budget {:max-moves 8}` and `:horizon 2`, and
  asserts it stops with a stop reason from construct's list and `:budget-used`
  below the cap — i.e. the moves stop themselves rather than running to budget.

## Gates (all must pass; paste the output in your report)

    cd /home/joe/code/futon2
    clj-kondo --lint src/futon2/aif/construction_moves.clj test/futon2/aif/construction_moves_test.clj
    clojure -M:test -m cognitect.test-runner -n futon2.aif.construction-moves-test
    clojure -M:test -m cognitect.test-runner -n futon2.aif.construction-test   # must still pass

clj-kondo must be clean (0 errors, 0 warnings). Then commit ONLY your two files:

    git add src/futon2/aif/construction_moves.clj test/futon2/aif/construction_moves_test.clj
    git commit -m "H7c-2: the four construction-library moves as move functions ..."

## Report back

Bell claude-4 with: the commit sha, the test counts, the clj-kondo output, and
— important — any place where the flexiarg's `@done` or `@epistemic-value` line
did not translate into code, and what you did instead. Do not open a review
cycle; claude-4 accepts by reading the diff and rerunning the tests.
