# H6b: delete the flat selection functions and their flat-only tests

Repo: /home/joe/code/futon2 (shared checkout, branch master, HEAD 94a69eda).
Two other agents are working in this repo right now on files you must NOT
touch: `src/futon2/aif/construction*.clj`, `src/futon2/aif/parameter_delivery.clj`,
`src/futon2/aif/active_horizon_g.clj`, and their tests. **Stage explicit paths;
never `git commit -a`; never `git add .`.**

You wrote the removal map this comes from: `p4ng
wm-walkthroughs/build-loop/closure/AUDIT-flat-path-removal-2026-09-17.md`
(§3 classifications, §4 test table). Re-read it first. The spec is
`SPEC-flat-removal-and-cascade-decision.md`, row H6: "Delete dead flat
functions and flat-only tests (audit §4)". H5b already deleted the flat block
from `judge`; H6a (commit 94a69eda) added the source gate
`test/futon2/aif/flat_path_gate_test.clj`, which currently passes with ONE
exception: `src/futon2/aif/policy.clj`.

**Goal: empty that exception.** When you are done, `exceptions` in the gate
test is `#{}` and the gate still passes, because the flat selectors no longer
exist anywhere under `src/futon2` or `scripts`.

## Step 1 — discovery, before you delete anything (record this in the commit message)

The retired heads are `policy/select-action`, `policy/strategic-recommendation`,
`policy/default-mode-select`. Deleting them makes a set of helpers in
`src/futon2/aif/policy.clj` dead. But several of that namespace's publics still
have PRODUCTION callers, and each needs a decision, not a guess:

    softmax-weights        src/futon2/aif/cascade_prior.clj, src/futon2/aif/habit_prior.clj,
                           scripts/futon2/report/war_machine.clj
    selection-laws         scripts/futon2/report/war_machine.clj
    effective-temperature  scripts/futon2/report/war_machine.clj
    sorry-pressure-record  src/futon2/aif/free_energy.clj

For each one, establish which of these it is and say so in the commit message:

  (a) **dead under the cascade path** — its only remaining callers are code
      that H5b already left unreachable. Then delete it AND that call site.
  (b) **still live for the cascade decision** — e.g. a general softmax used by
      the cascade posterior rather than by the flat law. Then keep it, and say
      which cascade-path caller keeps it alive.

Reach (a)/(b) by reading the call site, not by counting references. If you
cannot tell, leave it and report it — a wrong deletion here breaks the
production tick, which currently decides for one real target.

## Step 2 — delete

- From `src/futon2/aif/policy.clj`: the three retired heads, plus every
  remaining private and public in that namespace whose only purpose was to
  serve them and that Step 1 classified (a).
- **Must survive, untouched:** `select-action-cascades`, `cascade-first-action`,
  and everything they call. That function IS the production selector — the
  standing test `test/futon2/vm/standing_enacted_policy_is_cascade_g_test.clj`
  runs the real `judge` through it and must still pass.
- Flat-only tests, per audit §4. In a namespace that mixes flat and surviving
  coverage, delete only the `deftest`s that exercise deleted functions; keep
  the rest. Re-count as you go — the audit's counts are from before H4/H5b.
  Named there: `test/futon2/aif/policy_test.clj`,
  `test/futon2/report/war_machine_test.clj`, `test/futon2/aif/trace_test.clj`,
  `test/futon2/aif/efe_test.clj`, `test/futon2/aif/selection_score_readiness_test.clj`,
  `test/futon2/aif/zaif_full_loop_test.clj`, `test/futon2/aif/hierarchical_budget_test.clj`.
- `test/futon2/aif/selection_gain_test.clj` and `src/futon2/aif/selection_gain.clj`:
  **leave them alone.** The γ/τ_eff law is a separate piece of work (it still
  has live readers in `enact.clj` and `lane_futility.clj`); it is not in this
  packet.
- Then set `exceptions` to `#{}` in `test/futon2/aif/flat_path_gate_test.clj`.

## Step 3 — one honesty fix

`select-action-cascades` returns `:selection-boundary :strategic-recommendation`
— a flat-era label sitting inside the cascade decision. Search every reader of
`:selection-boundary` in futon2 AND futon3c (`grep -rn ":selection-boundary"`).
If every reader either ignores the value or can take a new one, change it to
`:cascade-selection-posterior` and update those readers. If any reader keys on
the old value in a way you cannot safely change, leave the label and report it.
`futon2.aif.strategic-habit` already identifies cascade decisions by
`:selection-law :applied`, not by this label.

## Acceptance (paste the output in your report)

    cd /home/joe/code/futon2
    clojure -M:test -m cognitect.test-runner            # BASELINE, before you edit
    ... your edits ...
    clj-kondo --lint src/futon2/aif/policy.clj
    clojure -M:test -m cognitect.test-runner            # after: no NEW failures
    clojure -M:test -m cognitect.test-runner -n futon2.aif.flat-path-gate-test
    clojure -M:test -m cognitect.test-runner -n futon2.vm.standing-enacted-policy-is-cascade-g-test

The last one calls the real `judge` and takes about a minute; it is the
requirement Joe set ("the enacted choice is selected by G over cascades"), so
it must pass, not be skipped. The full-suite comparison is baseline-vs-after:
a failure that was already failing before your edits is not yours to fix, but
name it.

Commit your files only. Bell claude-4 with: the commit sha, the Step 1
classifications, the baseline and after suite counts, the deftest counts you
removed per namespace, and what you left behind and why. No review cycle;
claude-4 accepts by reading the diff and rerunning.
