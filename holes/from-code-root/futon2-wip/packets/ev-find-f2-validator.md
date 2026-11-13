# EV-find-expectations, part 1: the external F2 expectation artifact and its validator

Repo: /home/joe/code/futon2 (shared checkout, master). Other agents are working
in it. **Stage explicit paths; never `git commit -a`; never `git add .`.**

You own exactly two NEW files:

  - `src/futon2/aif/find_expectations.clj`
  - `test/futon2/aif/find_expectations_test.clj`

**Do not edit** `find_receipt.clj`, `receipt_construction.clj` (codex-7's), or
any caller. If you believe one must change, say so in your report instead.

## The authority for this task

`holes/labs/wm-contract/runs/wm-08-external-f2-2026-09-16/TASK.md` (futon2
`371d89db`) — agreed by you as WM-08 owner, author codex-9, after source and
law assessment. **Read it first; it governs, not this packet, wherever the two
differ.** This packet narrows it to files and controls so it can be executed.

## Why the existing machinery does not already do this

`find-receipt/context` compiles expectations from the SAME interpretation
record the finder consumed, then `validate-result!` checks receipts against
them. That is a self-consistency check. The TASK says plainly: this machinery
is not credited as the independent external F2 check. What is missing is an
expectation artifact that exists BEFORE the finder's output and is bound to
independently captured occurrence context.

## What to build

1. **The artifact schema**, `:wm/find-expectations-v1`, read from EDN:

       {:schema :wm/find-expectations-v1
        :author {:id "..." :role :external-expectation-producer}
        :occurrence {:target <target id>
                     :target-source {:path "..." :sha256 "..."}
                     :repository-sha256 "..."
                     :pinned-at "..."
                     :source-digests {<source id> "<sha256>" ...}}
        :expected {<canonical pattern id, a family/name keyword>
                   {:clause-kind :if-clause
                    :acknowledged-clause {:text "..." :lines [start end]}
                    :route :structured-antecedent
                    :as-of {:target-sha256 "..." :repository-sha256 "..."
                            :pinned-at "..."}}
                   ...}}

   Each expected row carries clause-kind, the acknowledged clause as **text AND
   lines**, route and as-of — the TASK is explicit that lines are part of it.

2. **`validate-external!`**: compare the emitted result's receipts against the
   artifact on exactly the four comparison fields
   `[:clause-kind :acknowledged-clause :route :as-of]`, and refuse, typed
   (`{:finding :find/refusal :law :F2 :reason ...}`, as `find-receipt/need!`
   does), on any mismatch. It must ALSO check that the artifact's occurrence
   binding matches the result: target, target source sha256, repository digest,
   pinned-at. An artifact bound to a different occurrence is a refusal, not a
   pass.

   **There must be no optional-artifact path that silently skips the
   requirement.** A caller with no artifact is refused, not waved through. The
   TASK names this specifically.

3. **The author control is a DECLARED ROLE check, not authentication.** If the
   artifact's `:author :role` is the finder's or interpreter's own role (i.e.
   anything other than `:external-expectation-producer`), refuse
   `:self-supplied-expectations`. The TASK is explicit that this is a declared
   role control; do not pretend it authenticates anyone.

## The five controls — each is a test

1. correct independent input is **accepted**;
2. a self-supplied author is **rejected**;
3. a mismatched source or occurrence is **rejected**;
4. an unexpected selected pattern (a receipt with no expected row) is
   **rejected**;
5. a valid but **deliberately wrong expected clause** is rejected — same shape,
   wrong text or wrong lines.

Build the fixtures from the real captured bytes the existing find tests use
(read `test/futon2/aif/find_receipt_test.clj` first — reuse its repository and
record fixtures rather than inventing a second corpus).

## Gates (paste the output in your report)

    cd /home/joe/code/futon2
    clj-kondo --lint src/futon2/aif/find_expectations.clj test/futon2/aif/find_expectations_test.clj
    emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" -- --no-defaults src/futon2/aif/find_expectations.clj test/futon2/aif/find_expectations_test.clj
    clojure -M:test -m cognitect.test-runner -n futon2.aif.find-expectations-test
    clojure -M:test -m cognitect.test-runner -n futon2.aif.find-receipt-test   # must stay green

## Scope, stated as the TASK states it

Credit here is **input/validator machinery only**. This establishes no ordinary
serving use, no occurrence, and no checkbox change. A separate dispatch
captures a real occurrence's frozen context, and a third, independent agent
writes the expectations for it — you are building what those two will feed.

Bell claude-4 with the commit sha, the gate output, and anything in TASK.md you
could not implement as written and why. No review cycle; claude-4 accepts by
reading the diff and rerunning the tests.
