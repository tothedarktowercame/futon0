# M-the-futon-stack Q6 — R12 narrow-take-up: design choices

**Status:** DERIVE phase, pre-INSTANTIATE. Authored 2026-05-21 by claude-9
(session id is the current emacs-claude-repl session).
**Companion to:** `~/code/futon0/holes/missions/M-the-futon-stack.md` §Q6.
**Handoff source:** `~/code/futon0/holes/handoffs/r12-to-stack-Q6-2026-05-21.md`
(claude-4, session 5535ab92).
**Boundary:** narrow R12-unblocking subset of Q6, per Joe 2026-05-21.

This document records every non-mechanical choice made during DERIVE so a
later session can revise without re-walking the same forks.

---

## 1. Handoff correction: vocab file path

The handoff named `~/code/futon2/data/war-machine-terminal-vocabulary.edn`.
Actual location is `/home/joe/code/futon5a/data/war-machine-terminal-vocabulary.edn`
(with a 2026-04-26 split into `war-machine-strategic-vocabulary.edn` +
`war-machine-portfolio-vocabulary.edn` under M-war-machine-tuning TI-6 Path A).
Acknowledged 2026-05-21 by Joe; no action required beyond using the correct
path in future references.

## 2. No external EDN dependency for inferred hyperparameters

**Choice:** the per-action-class intrinsic-value table lives in an in-process
atom (`futon2.aif.intrinsic-values/state`), **not** in a v2 vocab EDN file.

**Why:** Joe 2026-05-21 — *"as the system run[s] we can get rid of any external
.edn dependencies — it seems OK to me to touch the inner loop if we need to."*
The original handoff's "ship a v2 EDN sibling" framing assumed
`futon2.aif.preferences` reads the vocab file at load time; in fact it inlines
all values as `def`s (`futon2/src/futon2/aif/preferences.clj:43-51`), so a v2
EDN would be epiphenomenal without rewiring.

**Consequence:** the design has to satisfy `feedback_reload_safety`
(atom-without-bootstrap-replay is the unsafe case). The atom rehydrates on
JVM startup from XTDB-persisted update records (§4 below) — XTDB IS the
bootstrap substrate.

## 3. Why β (not α or γ): inference target and substrate

The handoff text conflated three event streams; Joe's call lands on β.

| Stream | What it is | Substrate status | Indexes over |
|---|---|---|---|
| A | WM `:learn-action-class` emissions | Present in `wm-trace-*.edn` | `{:address-sorry, :open-mission, :fire-pattern}` |
| B | Operator follow-through on WM recs | Not captured directly; derivable | Same as A |
| C | VSATARCS writer-action consent decisions | `arxana-vsatarcs-consent.el` | `{:mission-doc-sync, :aif-edn-revision-entry, :story-update, :essay-revise, ...}` |

**Choice:** β (Stream A × Stream B-derived, credits WM action-classes).

**Why:** (a) the WM contract narrative
(`futon2/docs/futon-aif-completeness.md:285`) explicitly names "operator
follow-through on `:learn-action-class` recommendations" as the smallest
honest take-up path; (b) `:intrinsic-value` is already plumbed in WM
(`futon2/src/futon2/aif/efe.clj:173` subtracts it from G-risk;
`futon2/src/futon2/aif/policy.clj:94-95` max-keys over it) and unused in
VSATARCS — β replaces a hardcoded `0.1` (`action_proposer.clj:39`), α would
build a new consumption site; (c) Stream B is constructible from existing
substrate today (git, sorrys.edn, mission docs).

**Per Joe 2026-05-21:** the WM implementation is a *reference* for a later
port to VSATARCS. Both contracts can flip R12 status with cross-pointers:
WM via direct closure, VSATARCS via deferral-to-the-same-apparatus +
follow-on porting mission.

**What Stream C is for instead:** autopen-rule learning. The consent
dispatches are the right input for *which (class, match-type, reversibility)
tuples should be auto-confirmed without prompting* — a separate inference
that belongs to autopen / Phase-B/C work, not R12.

## 4. Stream B follow-through derivation per class

The honest per-class follow-through definitions, given current substrate:

### 4.1 `:address-sorry`

**Follow-through:** a sorry's `:status` transitions `:open → :addressed`
(or `:foreclosed`) within window W after the WM emits
`{:type :learn-action-class :target-class :address-sorry}` whose
context mentions or is plausibly about that sorry.

**Substrate today:**
- `futon2/data/sorrys.edn` carries `:status :open | :addressed | :foreclosed`.
- Current snapshot: 12 open, 0 addressed (as of 2026-05-21).
- **No `:addressed-at` timestamp field exists** — closures are detectable only
  by diffing the file across git history.

**Derivation method:** `git log -p futon2/data/sorrys.edn` → identify commits
that flip `:status :open` to `:status :addressed | :foreclosed` → commit
timestamp is the follow-through event.

**Caveat:** the join is fuzzy. The WM emits a class-level recommendation
(`:target-class :address-sorry`), not a per-sorry recommendation. The join
treats any `:address-sorry` emission as a single "did the operator address
*any* sorry in window W?" question — coarse but honest.

**Refinement deferred:** an `:addressed-at` field on sorrys.edn entries would
make this exact rather than git-derived. Not required for narrow R12.

### 4.2 `:open-mission`

**Follow-through:** a new mission doc file appears in `holes/missions/M-*.md`
across any of {futon0, futon2, futon3c, futon4, futon5a} within window W
after a `:learn-action-class :target-class :open-mission` emission.

**Substrate today:**
- `futon2/src/futon2/aif/mission_registry.clj` already enumerates missions by
  reading `**Status:**` lines.
- Mission docs are git-tracked; first-commit timestamp gives the open event.

**Derivation method:** `git log --diff-filter=A --name-only -- 'holes/missions/M-*.md'`
across the relevant repos → A-status (added) commits within window W are
follow-through events.

**Caveat:** new mission docs may be opened for reasons unrelated to a WM
emission. The class-level join treats correlation as evidence, not causation —
honest given the coarseness of the recommendation.

### 4.3 `:fire-pattern`

**Follow-through:** harder to define honestly than the other two.

**Substrate today:** the WM's `:fire-pattern` adapter is
`futon2/src/futon2/aif/pattern_registry.clj`, which treats *recent
context-retrieval certificates* as the addressable pattern surface.
"Firing" a pattern is mostly done by automated probe-taps, not by a discrete
operator action.

**Choice for narrow R12:** treat `:fire-pattern` as **"no substrate yet"**
and leave its intrinsic-value at the Beta(1, 1) prior mode (0.5) until a
later session designs an operator-fire-pattern event substrate. This makes
the R12 apparatus correct (the per-class table is the right shape) while
honestly acknowledging that one of three classes has no input today.

**Alternative considered:** parse commit messages for pattern references
(e.g., a commit message mentioning `pattern/...` within window W). Rejected
as too noisy — operator commit messages reference patterns for many reasons,
not specifically as follow-through to a WM rec.

**Refinement deferred:** an `:operator-fired-pattern` event substrate (whether
via probe-tap, explicit invocation, or sequence-of-commits heuristic) is a
sibling of the autopen-rule-learning work and probably co-designs with it.

## 5. Beta(α, β) update rule

**Choice:** per-class Beta posterior, uniform prior Beta(1, 1).

**Update:** for each `:learn-action-class :target-class T` emission in the
trace window:
- follow-through observed within window W → α[T] += 1
- no follow-through → β[T] += 1
- substrate unavailable for class T (e.g., `:fire-pattern` today) → no update

**Posterior summary:** intrinsic-value[T] = posterior mode = `(α-1)/(α+β-2)` for
α, β > 1; falls back to mean `α/(α+β)` otherwise. For Beta(1,1) both give 0.5.

**Window W:** default 14 days. Rationale: long enough to capture follow-through
on slow-paced operator work, short enough that intrinsic-value tracks current
rather than historical operator behavior. Configurable via
`futon2.aif.intrinsic-values/*window-days*`.

**Why posterior mode not mean:** for skewed Beta distributions (which is what
class differentials produce) the mode is closer to the "most plausible value"
the inner-loop should consult. Mean over-pulls toward 0.5 when α and β are
close.

**Why uniform prior:** the WM contract narrative is silent on a non-uniform
prior; Beta(1,1) is the maximum-entropy choice and avoids smuggling
operator assumptions into the inference. A later session can revise to a
weakly informative prior (e.g., Beta(0.5, 1.5) discouraging classes by
default) if class-blooming becomes a problem.

## 6. Atom + bootstrap-replay

**Choice:** atom in `futon2.aif.intrinsic-values`, persistence via
`code/v05/wm-hyperparameter-update` hyperedges in futon1a XTDB.

**Atom shape:**
```clojure
{:address-sorry  {:alpha 1.0 :beta 1.0 :intrinsic-value 0.5
                  :n-emissions 0 :n-followthrough 0 :as-of nil}
 :open-mission   {:alpha 1.0 :beta 1.0 :intrinsic-value 0.5
                  :n-emissions 0 :n-followthrough 0 :as-of nil}
 :fire-pattern   {:alpha 1.0 :beta 1.0 :intrinsic-value 0.5
                  :n-emissions 0 :n-followthrough 0 :as-of nil}}
```

**Hyperedge shape** (`code/v05/wm-hyperparameter-update`):
```clojure
{:hx/type "code/v05/wm-hyperparameter-update"
 :hx/props {:as-of "2026-05-21T12:00:00Z"
            :outer-loop-run-id "wm-ol:20260521T120000"
            :window-days 14
            :class :address-sorry
            :alpha-pre 1.0  :beta-pre 1.0
            :alpha-post 2.0 :beta-post 1.0
            :n-emissions-in-window 3
            :n-followthrough-in-window 1
            :intrinsic-value-pre 0.5
            :intrinsic-value-post 0.667
            :evidence-refs ["git:futon2:abc123:sorrys.edn"]}}
```

**Bootstrap-replay:** on `futon2.aif.intrinsic-values` ns load (or via explicit
`(rehydrate!)` call):
1. Query XTDB for all `code/v05/wm-hyperparameter-update` hyperedges.
2. Group by `:class`, sort by `:as-of`.
3. For each class, the latest record's `:alpha-post` / `:beta-post` /
   `:intrinsic-value-post` become the current atom state.
4. If no records exist for a class, atom holds Beta(1,1) prior.

**Why latest-record-wins not replay-all:** each outer-loop update IS already
the full posterior given all evidence up to its `:as-of`. Replaying every
record would double-count. (This is the standard property of conjugate
updates: the posterior IS the running summary.)

## 7. Outer-loop scheduler

**Choice:** new alias `:wm-outer-loop` in `futon2/deps.edn`, mirroring
`:wm-scheduled`. New ns `wm-outer-loop` in `futon2/scripts/`.

**Cadence:** daily (`0 4 * * *` recommended — runs at 4am after any nightly
work has settled). Installation is operator-action like `:wm-scheduled`.

**Dependencies:** `futon2/deps.edn` already includes
`org.babashka/http-client {:mvn/version "0.4.22"}` and `cheshire/cheshire` —
no new dep needed for the futon1a HTTP write path.

**Entry-point flow:**
1. Read trace window: `wm-trace-{date-W..today}.edn` files.
2. Extract `:learn-action-class` emissions per `:target-class`.
3. For each class with substrate (§4): derive follow-through evidence;
   compute Δα, Δβ.
4. Read pre-update atom via XTDB rehydration.
5. Compute post-update Beta params and intrinsic-value.
6. Write one `code/v05/wm-hyperparameter-update` hyperedge per class.
7. Update in-memory atom (will be picked up by the running JVM if both
   loops share the atom; the cron-script case rehydrates fresh per run).
8. Print one-line summary to stdout, mirror `wm_scheduled_run`'s style.

**Resilience:** wrap in top-level try/catch per `wm_scheduled_run` precedent;
any class without substrate skipped, not faulted.

## 8. Inner-loop wiring

**Change site:** `futon2/src/futon2/aif/action_proposer.clj:39`.

**Before:**
```clojure
{:type :learn-action-class
 :target-class target-class
 :intrinsic-value 0.1
 :rationale (str "no addressable entities for " target-class ...)}
```

**After:**
```clojure
(require '[futon2.aif.intrinsic-values :as iv])
;; ...
{:type :learn-action-class
 :target-class target-class
 :intrinsic-value (iv/credit-for target-class)
 :rationale (str "no addressable entities for " target-class ...)}
```

`iv/credit-for` returns the atom-current `:intrinsic-value` for the class,
defaulting to 0.5 (Beta(1,1) prior mode) for any class without a record.

**Downstream:** `efe.clj:173` and `policy.clj:94-95` already read
`:intrinsic-value` correctly. No further wiring change.

## 9. Honest scope of the R12-flip on day 1

**With only 2 days of `wm-trace-*.edn` history (R10 is `:scheduled-execution-ready`
but not running on cron yet), the Beta posterior will be Beta(1,1) for every
class on day 1.** The R12-flip is honest if it claims:

- The *apparatus* is in place: hyperparameters become hidden state via the
  outer loop. ✓
- The atom is durable across JVM restarts via XTDB bootstrap-replay. ✓
- The intrinsic-value literal `0.1` is replaced by atom-driven values. ✓
- For any class with ≥1 emission AND ≥1 follow-through in the trace window,
  the Beta posterior moves from the prior. ✓ (eventually, with R10 running)

It would NOT be honest to claim the system has *learned* anything useful
yet — that requires R10 actually running on cron for weeks and operator
behavior accumulating against emissions. Per `feedback_pm_scaffold_evidence_check.md`,
the v0 PR should distinguish prior from checked.

**Suggested R-row text** for both `vsatarcs-alignment-completeness.aif.edn:1584`
and `futon-aif-completeness.md:279`:
> `:status :satisfied-via-stack-level-Q6-narrow-take-up-apparatus`
> Pointer: `M-the-futon-stack.md` §Checkpoint-Q6-R12-narrow-take-up (TBD on
> shipping). Caveat: apparatus in place; Stream B follow-through inference
> runs but values reflect Beta(1,1) prior until R10 has accumulated trace
> emissions on cron.

## 10. Out of scope for this checkpoint

(Reiterates handoff §2 boundary, with current-session-specific notes.)

- The kill-switch, full debug surface, full reversal protocol.
- VSATARCS port of the same apparatus (deferred until WM reference works).
- Autopen-rule learning from Stream C consent decisions.
- An `:addressed-at` timestamp field on sorrys.edn entries.
- An `:operator-fired-pattern` event substrate for `:fire-pattern` class.
- Backfilling emission history before R10 graduated (would require running
  the WM retrospectively across months of substrate snapshots).
- Modifying `preferences.clj`'s static per-channel `pragmatic-weights` —
  per-channel hyperparameters are a separate R12 direction that needs a
  different input substrate than per-action-class.

## 11. Open questions for a later session

1. Should the outer loop also emit a *narrative* digest (e.g., "after 30 days,
   `:address-sorry` intrinsic-value rose from 0.5 to 0.71 over 12 emissions,
   8 of which had follow-through") for the operator's debug surface?
2. When VSATARCS gets its port, should the two outer loops share an atom
   namespace or stay independent? (Independent matches the bilateral
   discipline; shared compresses bootstrap-replay.)
3. Is window W = 14 days right? Some action classes have much longer natural
   follow-through delays.
4. Should `:fire-pattern` get a placeholder substrate (e.g., commit-msg parser)
   in this checkpoint, or is staying at Beta(1,1) more honest?

These are deferred for Joe's call or a later session's investigation.

## 12. Penholder constraint (discovered during INSTANTIATE)

futon1a enforces an allowed-penholder list via the `FUTON1A_ALLOWED_PENHOLDERS`
env var. On this machine the set is `{"api"}` (the only configured value).
A custom `"wm-outer-loop"` penholder produces a 403 `:forbidden` from layer-3
authorisation at `futon1a/auth/penholder.clj`.

**Choice for v0:** write as `penholder "api"` (matches what multi_watcher
and the elisp arxana-store do), and record true authorship in the props
map as `:provenance/author "wm-outer-loop"` so the records remain
attributable when queried. An env override `WM_OUTER_LOOP_PENHOLDER` lets
a later session swap penholder if a dedicated one is registered.

**Refinement deferred:** register `"wm-outer-loop"` (and any other dedicated
authors) by appending to `FUTON1A_ALLOWED_PENHOLDERS`. This requires either
a futon1a-runtime config reload or a natural restart of the orchestrating
JVM — neither was attempted in this checkpoint per the no-restart discipline.

## 13. Sliding-window vs incremental — caught during smoke

The first cut of `wm-outer-loop` was incremental: each run computed
`prior-entry ← (iv/current)` then `next-record ← prior + delta`. This
contradicts the §6 latest-record-wins rationale — back-to-back runs would
double-count the same window's evidence into ever-growing α, β.

**Fix landed:** sliding-window from-scratch. Each run starts from
`(iv/fresh-entry)` (Beta(1,1) prior) and computes the full posterior given
the current window of evidence. Idempotent on re-runs (verified by running
the loop twice consecutively; outputs identical). This makes
"latest-record-wins" structurally correct because every record IS the full
posterior given evidence up to its `:as-of` — replay-all would double-count
the same window across records.

Caught during smoke, not design. Documented here so a later session that
revisits the cadence (e.g., switching to incremental for richer evidence
weighting) understands the constraint.

## 14. Reload-safety of the changed code

- **`futon2/src/futon2/aif/intrinsic_values.clj`** — new ns, `defonce` atom,
  no protocol. Safe to Drawbridge-reload; the `defonce` preserves accumulated
  atom state across reloads.
- **`futon2/scripts/wm_outer_loop.clj`** — script (not loaded in the running
  JVM; invoked by `clojure -M:wm-outer-loop`). Each invocation is a fresh
  JVM; reload-safety not applicable.
- **`futon2/src/futon2/aif/action_proposer.clj`** — `defprotocol` ns.
  Unsafe to Drawbridge-reload per `feedback_drawbridge_protocol_reload`;
  change takes effect at next natural restart of the running futon3c JVM.
  In the meantime, fresh JVMs (tests, outer-loop, wm_scheduled_run) see
  the new behavior immediately.

## 15. §2.J reframe — what counts as a "WM emission" (landed 2026-05-21)

The handoff's original Stream B interpretation counted only
`:learn-action-class :target-class C` recommendations as emissions for
class C. Problem surfaced during smoke (and named in the v0.6 last-mile
update): for currently-addressable classes (e.g., `:address-sorry` after
§2.E v0.18+v0.19 closed), the WM never emits a gap-class wrapper — it
chooses the concrete `:address-sorry :target ...` action directly. That
left such classes with `n-emissions = 0` in every window, so their Beta
posterior stayed vacuously at Beta(1,1) prior forever, regardless of
operator activity.

**The reframe** (Joe directive 2026-05-21): an emission for class C is
**either** a `:learn-action-class :target-class C` entry in `:ranked-actions`
**or** the trace record's `:decision :action :type` = C. Counted per-record
at-most-once (union); meta types (`:no-op`, `:learn-action-class` itself,
`:abstain`) are stripped. Implementation: `extract-emissions` in
`wm_outer_loop.clj`.

**Interpretation:** an emission is a *WM tick that surfaced this class*
— either as a recommended gap to learn about (LAC) or as the actually-
chosen action (decision). "Follow-through" then becomes "operator engaged
with this class within window after the WM surfaced it." Symmetrically:
ignored emissions become β-evidence that the WM is over-recommending
relative to operator priority.

**Cap on followthrough:** `n-followthrough` is capped at `n-emissions`
inside `next-record` (defensive math). If the Stream B substrate detects
more follow-through events than the WM had opportunities to recommend
(e.g., operator addressed N sorries while WM ran fewer than N ticks),
the extras can't be attributed to WM recommendations. The uncapped count
is preserved in `:n-followthrough-observed` for legibility.

**Posterior consequence in today's live smoke:** `:address-sorry` went
from "permanently at prior" to "Beta(1, 11) → iv ≈ 0.083" after one outer-
loop run — the WM picked `:address-sorry` 10 times in the 14-day window;
no sorries were closed in git history; β += 10. The posterior pulling
sharply toward β IS the operator-legible signal: either the WM is
mis-prioritising or the operator hasn't been addressing sorries this
fortnight. Both are interesting; this is what R12 is for.

**Future direction noted (not implemented):** temporal decay on β-evidence
so older ignored-recommendations matter less than recent ones. Joe's
2026-05-21 call: per-class window-W (§11.c, §16 below) may subsume the
need; revisit if posterior-overshoot becomes a real problem. Recorded in
`~/code/futon7/holes/M-war-machine-aif-last-mile.md` §2.I as a future
sub-direction.

## 16. §11.c per-class window-W (landed 2026-05-21)

The original design used a uniform 14-day window across classes. Reasonable
default, but action classes have very different natural cadences:
- `:open-mission` — slow; 1 new mission per 2-4 weeks is common.
- `:address-sorry` — faster; operator could close one per day.
- `:fire-pattern` — substrate unavailable; window choice doesn't matter today.

**Implementation:** `default-window-days-by-class` map in `wm_outer_loop`;
defaults `{:address-sorry 14, :open-mission 30, :fire-pattern 14}`. Operator
overrides via `WM_OUTER_LOOP_WINDOW_BY_CLASS_EDN` env var (EDN-encoded map).
Trace read uses the MAX window across all classes (one read covers
everyone); per-class extraction filters records by class-specific
timestamp via `records-within-window`. The `:window-days` opt to
`run-once!` is retained as a uniform-window override (also exposed as
the first arg to `-main` for backward compatibility).

**Smoke result:** with the 30-day window for `:open-mission`, the smoke
picked up 20 mission additions (vs 1 at 14 days); cap on follow-through
brought it back to 16 (the emission count), but the operator now sees
the actual cadence of mission-opening more clearly.

**Caveat (startup artefact):** during the early period when wm-trace files
don't go back as far as the widest class window (today: 3 trace days vs
30-day mission window), follow-through events from before the trace
horizon are visible while emissions from that period aren't. The cap
keeps the math valid but the posterior may overshoot. As R10 cron
accumulates trace history, the artefact recedes naturally.

## 17. §11.d :fire-pattern substrate — decision: stay-honest (2026-05-21)

The handoff and design-choices §4.3 rejected a commit-msg-parser
substrate for `:fire-pattern` as "too noisy — operator commit messages
reference patterns for many reasons, not specifically as follow-through
to a WM rec."

Alternative explored 2026-05-21: count subsequent `context-retrieval`
evidence on the recommended pattern as follow-through. Rejected: retrieval
isn't firing, and the retrieval would have happened anyway based on
whatever the agent was working on.

**Decision:** `:fire-pattern` substrate stays `:unavailable`. The narrative
digest surfaces this honestly per run (`held at prior — substrate
refinement is a future R12 direction`). The class will move under R12
inference once a genuine operator-fire-pattern event substrate exists
— probably co-designed with autopen-rule learning since that's the
parallel "operator confirmed automation step" substrate work.

No code change. Documented to close the §11 open question.

