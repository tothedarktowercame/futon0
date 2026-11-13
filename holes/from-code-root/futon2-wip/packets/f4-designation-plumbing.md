# WM-08: the F4 designation artifact, authority-agnostic

Repo: /home/joe/code/futon2 (shared checkout, master). Stage explicit paths.
You own ONE new file and its test:

  - `src/futon2/aif/find_designation.clj`
  - `test/futon2/aif/find_designation_test.clj`

Do not edit `find_receipt.clj`, `find_expectations.clj`, `receipt_construction.clj`
or any caller.

## Why now, and what it must not do

Your AUTH-F4-scope position is accepted as the design basis, and I verified its
load-bearing claim directly: `amendedIdentity` (mathlib4
`DarkTower/WarMachine/F11AmendedCarrier.lean:358`) selects the whole repository
and returns the empty designation, and `amendedIdentityConformant` PROVES it
satisfies all four joint laws. So an F4 conjunct on the finder's own return
constrains nothing. That is a theorem, not an opinion, and it is the reason the
designation must come from outside the run.

**Joe settles AUTH-F4-scope, not this dispatch.** Build the plumbing so that it
is compatible with whichever authority he settles on and predetermines none of
them. Concretely: this namespace accepts a designation artifact, refuses a
self-supplied one, and reports honest vacuity. It never manufactures a
designation, never derives one from what fired, and never reads the
interpretation record.

## What to build

1. **Artifact `:wm/find-designation-v1`**, structurally parallel to
   `find-expectations`' artifact (read that namespace first and match its
   shape and its refusal style — one reader will meet both):

       {:schema :wm/find-designation-v1
        :author {:id "..." :role :designation-author}
        :occurrence {…exactly the binding FROZEN-CONTEXT.edn carries:
                     :target :target-source :repository-sha256 :pinned-at…}
        :designated #{<canonical family/name keywords>}
        :basis "<the authority's own words for why these do not apply>"}

2. **`read-artifact`** with structural validation, and
   **`designation-for`** `[occurrence artifact repository]` returning the
   designated set for `find-receipt/validate-result!`'s third argument:
   - no artifact ⇒ **`nil` and a recorded `:vacuous`**, which is the correct
     report, not a deficiency — `validate-result!` already takes nil;
   - an artifact whose author role is not `:designation-author` ⇒ refuse
     `:self-supplied-designation` (declared-role control, not authentication);
   - an occurrence binding that does not match ⇒ refuse
     `:occurrence-binding-mismatch`;
   - ids that are not canonical family/name keywords ⇒ refuse (the same rule
     `validate-result!` enforces);
   - **`:basis` is required and non-blank.** A designation with no stated
     reason is not negative knowledge about the tension, it is a bare list.

3. Refusals typed as in `find-expectations`:
   `{:finding :find/refusal :law :F4 :reason …}`.

## Tests

The five controls, mirroring the expectations ones: accepted with a valid
artifact; self-supplied author rejected; mismatched occurrence rejected;
non-canonical id rejected; missing `:basis` rejected. Plus: **no artifact gives
`nil` plus a recorded `:vacuous`, and that path is tested explicitly** — honest
vacuity is a behaviour here, not an omission.

Then one integration test: `find-receipt/validate-result!` with the designated
set from a valid artifact, showing `:f4 :discriminating` when the designation
intersects the repository and `:vacuous` when it is empty. Use the
`find-receipt-test/sample` corpus, as you did for the expectations.

## Gates

clj-kondo, check-parens, `find-designation-test`, and `find-receipt-test` plus
`find-expectations-test` still green. Commit only your two files.

Bell claude-4 with the sha, the gate output, and anything that could not be
built without presuming an authority Joe has not settled.
