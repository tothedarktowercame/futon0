# Algorithm: Next-Invariant Loop

Date: 2026-04-29
Status: active

## Authority routing

- methodology owner:
  `futon3/library/invariant-coherence/shape-first-identify.flexiarg`
- companion patterns:
  - `futon3/library/invariant-coherence/protocol-family-naming.flexiarg`
  - `futon3/library/invariant-coherence/subsumption-witness.flexiarg`
  - `futon3/library/invariant-coherence/bounded-disposition.flexiarg`
- inventory location:
  `futon3c/docs/structural-law-inventory.sexp`
- priority queue:
  `futon5a/data/stack-stereolithography-priority-queue.json`
- live apparatus:
  `futon3c/src/futon3c/logic/{probe,probe_taps,archaeology,ratchet}.clj`
  + `futon3c/src/futon3c/evidence/boundary.clj`
- mission lifecycle reference:
  `futon4/holes/mission-lifecycle.md`
- Codex handoff protocol:
  `futon3c/CLAUDE.md` § "Codex Handoff Protocol"
- worked traces:
  - `futon3c/holes/missions/M-archaeology-control.md` (subsumption-witness shape)
  - `futon3c/holes/missions/M-bounded-disposition.md` (bounded-disposition shape)

## Purpose

Use this when the operator wants to **move the structural-law inventory
queue forward by one rank**: pick the top-rated unsatisfied candidate
invariant, find the shape it's an instance of, ship the first sibling
as a worked example, and hand off the remaining siblings to Codex via
GitHub issues.

This algorithm makes the methodological move ("shape-first IDENTIFY")
operational. Without it, each new candidate invariant tends to be
implemented as a literal one-off; with it, candidates routinely produce
namespace-discriminated sibling families that exhibit the same protocol
across multiple artifact-classes — and the queue moves visibly.

The algorithm's correctness criterion: **after one run, the priority
queue's top-N has shifted (the candidate that was rank-1 has become
operational or partially-operational; some other candidate is now
rank-1) AND at least one Codex handoff issue is open continuing the
shape**.

Boundary of ownership:
- this algorithm owns ONE iteration of the queue-advance loop
- it does NOT own the meta-question "should we advance the queue at
  all?" (that's a planning-level call, owned by the operator)
- it does NOT own the implementation work itself (that's distributed
  across the nested patterns); it owns sequencing, scoping, and
  handoff structure.

## Inputs

- The current state of `futon3c/docs/structural-law-inventory.sexp`.
- The current state of
  `futon5a/data/stack-stereolithography-priority-queue.json`.
- The running futon3c JVM (Drawbridge nREPL is the live verification
  surface).
- An open GitHub remote for `futon3c` (handoffs use issues).

## Sequencing

### 1. Identify the next candidate

Read the priority queue's `summary.top-10` and walk down it skipping any
entries whose `:invariant/id` corresponds to an already-operational
sibling. The first remaining entry is the target.

```bash
jq '.summary.top-10' /home/joe/code/futon5a/data/stack-stereolithography-priority-queue.json
# Cross-reference against `:status operational` entries in the inventory.
```

Record:
- target `:invariant/id`
- target `:family/id`
- `priority/score` and `priority/explanation`
- `harvest/commit-count` (a signal of historical pressure)
- the `summary` text from the inventory

### 2. Probe live state

Before generalising, check whether the candidate has live signal NOW or
is a historical-pressure candidate (high harvest count, low current
state). For artifact-class candidates, run the natural enumeration:

- stashes: `for r in ~/code/futon*; do (cd "$r" && git stash list); done`
- branches: `git for-each-ref --count=20 refs/heads/`
- mission docs: `ls holes/missions/M-*.md | wc -l` per repo
- evidence shape: query the running JVM for the relevant tag

If live state is zero, note it: the candidate may be operationally
dormant (the autostash slice was — zero stashes when shipped). That
doesn't disqualify it; the invariant still bounds future state.

### 3. Apply shape-first IDENTIFY

Per `invariant-coherence/shape-first-identify.flexiarg`, ask: **"what
shape is this candidate one instance of?"** Sketch the protocol:

- For *witness* invariants: `is-A-X?(artifact, canonical-record) →
  bool` (subsumption-witness shape). Examples: autostash subsumption,
  deferred-stub vs operational-registry mismatch.
- For *triage* invariants: `(disposition-of A, |undecided A|, oldest
  age) → bool` (bounded-disposition shape). Examples: stash
  disposition + bound, branch disposition + bound.
- For *ordering* invariants: a strict-monotone-modulo-recorded-event
  predicate (coverage-ratchet shape). May fit some queue-shaped
  candidates.
- New shape: if none of the above fits, the candidate may exhibit a
  novel shape. That's a finding worth keeping; author a new pattern
  in `invariant-coherence/<shape>.flexiarg` (cf. `bounded-disposition`
  was new at draft-time).

Enumerate at least 2-3 plausible *sibling instances* of the shape
(per artifact-class, per write-class, per mission-class — whatever
discriminates). If only one instance can be named, mark as
`:special-case true` in the mission and proceed without generalising.

### 4. Decide scope

Three cases:

- **Reuse existing pattern**: shape already documented in
  `invariant-coherence/`. Skip to step 5; reference the pattern from
  the new mission.
- **New shape**: author a new `.flexiarg` in `invariant-coherence/`
  with vocabulary, exemplar table, anti-patterns, evidence-shape.
- **Ambiguous**: multiple plausible shapes. Pick the one with the
  largest exemplar table; mention the alternatives in the mission's
  IDENTIFY for honest record.

### 5. Open the mission

Create `futon3c/holes/missions/M-<shape>.md` (or extend an existing
mission if the shape extends one). The mission's IDENTIFY explicitly:

- Names the shape (with link to the `.flexiarg`).
- Lists the candidate sibling instances (artifact-class + A + D /
  predicate + bound).
- Names ONE first-sibling worked-example (typically the one with the
  most concrete enumeration / cleanest verification path).
- Names the remaining siblings as Codex handoff candidates.

Status: `IDENTIFY → INSTANTIATE` (the loop runs MAP / DERIVE inline).

### 6. Implement the first sibling

In `futon3c/src/futon3c/logic/<family-or-namespace>.clj` (typically
extend an existing namespace if one fits — `archaeology.clj` did
double duty for both subsumption-witness and bounded-disposition).
The check-fn factory:

- Returns `(fn [evidence-store] {:outcome :ok | :violation | :inactive
  :detail <map>})`.
- Catches Throwable — never lets the probe loop die.
- Includes `:invariant <I-...>` in detail so the canonical statement
  is queryable.

Add a public canonical statement `def I-<shape>` as a grep-verifiable
string.

### 7. Tests

`futon3c/test/futon3c/logic/<ns>_test.clj`:

- Empty-input → `:ok`
- Real-input-with-no-violations → `:ok`
- Real-input-with-violations → `:violation` with detail
- Edge case (nonexistent path / nil store / etc.)
- Canonical-statement grep verification
- Per-vocabulary parse paths if the check parses tags
- Updated registrar test (count goes up by N).

Run `clojure -X:test :nses '[<the-test-ns>]'` and confirm 0 failures.

### 8. Update inventory

`futon3c/docs/structural-law-inventory.sexp`:

- Use namespace IDs `<shape>/<artifact-class>` per
  `protocol-family-naming.flexiarg`.
- If absorbing narrower candidates (e.g. the bounded-disposition/stash
  absorbed two narrower candidates), remove the narrower entries —
  the ratchet treats removal as `:removed` (informational, not
  demotion).
- The first sibling lands at `:status :operational-when-enabled`
  (probe-tap) or `:status :operational` (if it has a strong-mode
  binding like a pre-commit hook).
- Each entry carries `:implemented-in / :enforced-at / :evidenced-by`
  triples.

### 9. Live verification

```bash
TOKEN=$(cat /home/joe/code/futon3c/.admintoken)
cat <<'EOF' > /tmp/v.edn
(do (require '[futon3c.logic.<ns> :as <alias>] :reload)
    (require '[futon3c.logic.probe :as probe])
    (let [registered (<alias>/register-<family>-taps! ...)]
      ...))
EOF
curl -s -X POST 'http://127.0.0.1:6768/eval' \
  -H 'Content-Type: application/edn' \
  -H "x-admin-token: $TOKEN" \
  --data-binary @/tmp/v.edn
```

If reloading existing code (e.g. modified by Codex), use `:reload-all`
to avoid stale-class `NoClassDefFoundError`.

### 10. Codex handoff

Open a GitHub issue per the protocol in
`futon3c/CLAUDE.md § "Codex Handoff Protocol"`:

- Title: `Codex: complete <shape>/{<sibling1>,<sibling2>,...}`
- `:in` (READ-ONLY) files with line-number pointers
- `:out` modifications (per file, specific function names + signatures)
- Shapes reference (the relevant types/protocols)
- Test expectations (specific `clojure -X:test` invocations + pass criteria)
- Criteria checklist (boxes Codex must check off)
- Non-goals (what NOT to do — common drift directions)

```bash
gh issue create --title "..." --body "$(cat <<'EOF' ... EOF
)"
```

Record the issue URL.

### 11. PSR + PUR + mission checkpoint

In `futon3c/holes/labs/M-<shape>/`:

- PSR `psr/<date>__derive__<sibling>.md` — pattern selections, decision,
  alternatives considered, confidence.
- PUR `pur/<date>__instantiate__<sibling>.md` — actions taken, outcome,
  prediction errors, invariants verified, connections.

Mission checkpoint at the bottom of the mission doc records
deliverables + live verification + test sweep + handoff issue URL.

### 12. Verify Codex's handoff (deferred — async)

When Codex claims the issue done, before closing:

- Run the same test sweep + bootstrap-load check.
- Verify each criteria-checklist box.
- Live-verify with `:reload-all` (Codex's edits land without reloading
  the running JVM; stale-class errors are common until first reload).
- Close the issue with a verification summary listing each criterion +
  pass evidence.

```bash
gh issue close <N> --comment "Verified end-to-end. ..."
```

## Stop-the-line discipline (preempts the loop)

The one-by-one queue-advance logic above assumes the queue is *running*.
There are conditions under which the queue is *stopped* — and when stopped,
there is no queue to jump; the concept of "queue-jumping" doesn't apply.
Stop-the-line is the right framing: pause the loop, hot-fix the
foundation, resume the loop afterward.

### Stop-the-line signals

The line is stopped when any of:

- The HUD widget reports STUCK with no live signal even though work has
  been done (e.g. tracer entries should exist but the substrate gave way).
- A foundational invariant is unsatisfied AND every other invariant in
  the queue silently relies on it (data persistence, registry replay,
  protocol-extension stability).
- A recurring data-loss incident is diagnosed (e.g. Evidence Landscape
  lost for two weeks because state lived in an atom without
  bootstrap-replay).
- A class of failure recurs across multiple sessions despite individual
  per-instance fixes — the per-instance fix isn't holding because the
  underlying class is unsatisfied.

When any signal fires:

1. **Pause the queue.** Do not pick the next priority-rank candidate.
2. **Diagnose to the foundation.** Trace the failure to its structural
   cause; the fix is at the foundation level, not at the
   per-incident level.
3. **Run a hot-fix mission.** Open `M-<foundation-name>` outside the
   normal queue. Mark its mission status as a hot-fix in response to
   a stop-the-line signal, with the signal recorded.
4. **Apply the structural form.** Foundation invariants are
   structural (grep-verifiable, pre-commit-blocking) rather than
   runtime watchdogs. A watchdog asks "did someone follow the
   discipline?"; an invariant makes the discipline impossible to
   violate.
5. **Resume the queue** at the original next-up candidate. Don't
   restart from rank-1; the queue's relative ordering is unchanged
   by the hot-fix.

### What hot-fix is NOT

- **Not a queue-jump.** Queue-jumping implies the queue is running and
  one item is being privileged ahead of others. Stop-the-line means
  the queue isn't running — there is nothing to jump past.
- **Not "this seems important."** The bar is structural: the queue's
  promise of forward motion is unreliable until this lands. Other
  invariants in flight aren't holding because of this one.
- **Not a workaround.** A hot-fix is at the foundation, not a
  patch to the symptom. If you find yourself adding a check that the
  symptom is absent, you're at watchdog-level, not invariant-level.
  Go deeper.

### Worked trace

The 2026-05-01 stop-the-line started with the HUD widget reporting
STUCK. Diagnosis revealed:

- Pipeline-tracer entries from the previous session vanished at JVM
  restart.
- `family-check-fns` registry was empty after restart (atom not
  bootstrapped from disk).
- Earlier Evidence Landscape was stored in an atom for ~2 weeks,
  silently losing data.

The pattern across all three: long-lived state in atoms without a
boot-replay path. The invariant `reachable-from-boot` (M-reachable-
from-boot, opened as a hot-fix) makes the discipline structural
(static check + pre-commit hook) rather than a runtime hope.

## Termination

The algorithm terminates when:

- The first sibling is operational (probe-tap or stronger).
- A Codex handoff issue is open for remaining siblings.
- The mission checkpoint is recorded.

(Codex's eventual completion of the handoff is a separate
asynchronous closure; the algorithm doesn't block on it.)

## Anti-patterns

- **Implementing the literal candidate sentence.** Defeats shape-first
  IDENTIFY. Always ask "what shape is this one instance of?" before
  writing code.
- **Inventing structural elements ("subfamily") to express shape
  groupings.** Namespace IDs (`<shape>/<instance>`) are sufficient.
- **Skipping the live-verification step.** Tests pass in a clean
  JVM; the running stack may have edge cases (stale-class issues,
  divergent state) that only surface live.
- **Bundling multiple shapes into one mission.** One mission per
  shape; one PSR per pattern selection; one PUR per applied
  instance. Bundling defeats the recurring-methodology point.
- **Auto-cleaning on detection.** The probe is read-only; cleanup is
  operator-driven. Conflating them creates race conditions.
- **Closing Codex issues without verifying.** Trust-but-verify: run
  tests, check live, confirm criteria, then close.

## Evidence-shape for trace

Each iteration of this algorithm produces a measurable trace:

- Mission file `M-<shape>.md` opened (timestamp + INSTANTIATE checkpoint).
- Pattern file in `invariant-coherence/<shape>.flexiarg` (added or
  referenced).
- Inventory: `<shape>/<sibling>` entry with `:status` reflecting binding mode.
- GitHub issue: open at first; closed when Codex completes; comment
  trail of verification.
- PSR + PUR pair under `holes/labs/M-<shape>/`.

After N iterations, the meta-pattern is visible:

- ≥ N missions, each opened by shape-first IDENTIFY.
- ≥ N namespaced sibling families.
- ≥ N closed Codex handoff issues with verification summaries.

When the trace plateaus (no candidate left rises above a "noise floor"
in priority/score), the algorithm has done its job and the inventory
queue is in steady-state.

## Worked traces (as of 2026-05-01)

- **Iteration 0 — M-archaeology-control** (subsumption-witness):
  three siblings `obsolescence-recognition/*`, all `:operational`.
  Closed issue #62.
- **Iteration 1 — M-bounded-disposition** (bounded-disposition):
  three siblings `bounded-disposition/*`, all
  `:operational-when-enabled`. Closed issue #63.
- **Iteration 2 — M-single-locus** (single-locus): three siblings
  `single-locus/*`. First sibling operational + Codex handed off.
  Closed issue #64.
- **Hot-fix (stop-the-line) — M-reachable-from-boot** (reachable-from-
  boot): foundational invariant for state durability; opened in
  response to HUD STUCK + Evidence Landscape data-loss diagnosis
  2026-05-01. Outside the normal queue cadence.

After the hot-fix lands, the next iteration resumes at whatever's
now rank-1 in the priority queue (likely a `checkin-on-exit` or
similar atomic-inspectable-units candidate).
