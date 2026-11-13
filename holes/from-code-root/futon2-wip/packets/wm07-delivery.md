# WM-07-delivery: parameter posterior and information gain, delivered to the epistemic consumer

Repo: /home/joe/code/futon2 (shared checkout, branch master, HEAD d1688d4b).
Other agents are working in this repo right now. **Stage explicit paths; never
`git commit -a`; never `git add .`.** You own exactly two new files:

  - `src/futon2/aif/parameter_delivery.clj`      (new)
  - `test/futon2/aif/parameter_delivery_test.clj` (new)

Do not edit `machine_parameters.clj`, `active_horizon_g.clj`, `war_machine.clj`
or anything else. If you think another file must change, say so in your report
instead of changing it.

## Why this exists

DAG node `WM-07-delivery` (closure-dag.json rev 74): *"Observation-bound
parameter posterior and information-gain computation delivered to epistemic
consumer."* Both its `dispatch-requires` (`WM-01-carrier-contract`,
`WM-04-machinery`) are accepted. Its node carries
`"scope": "Must be narrowed to named files and acceptance controls in a pinned
dispatch before execution"` — this packet is that narrowing: the two files
above, the acceptance controls below.

What exists: `futon2.aif.machine-parameters/parameter-kernels` builds
Q(θ|π), Q(θ|o,π), the evidence normalisers and the posterior-predictive over
finite REGISTERED hypotheses (each pinned by path + sha256, re-hashed on every
call). Its ns docstring says "no EIG integration", and nothing outside its own
test calls it. That is the gap.

## The Lean definition you must match

`/home/joe/code/mathlib4/DarkTower/WarMachine/Holes.lean` lines ~7191-7213:

    parameterInformationGain prior posterior _ π o =
      Σ_{θ ∈ posterior.support (π,o)} posterior.mass (π,o) θ
                                      * Real.log (posterior.mass (π,o) θ / prior.mass π θ)

    expectedInformationGain Q prior posterior _ π =
      Σ_{o ∈ Q.support π} Q.mass π o * parameterInformationGain prior posterior _ π o

i.e. EIG is the predictive-outcome expectation of the posterior-to-prior KL, in
NATS (natural log), summed over the POSTERIOR's support (not the prior's). The
Lean side carries a hypothesis `positivePrior : ∀ π o θ, θ ∈ posterior.support
(π,o) → 0 < prior.mass π θ`; in Clojure that hypothesis becomes a typed refusal
(see below), never a silent skip or a fudged epsilon.

The numeric fixture is
`/home/joe/code/mathlib4/DarkTower/WarMachine/ExpectedInformationGainWitness.lean`
(`theorem binaryFixture`): one policy, one outcome of predictive mass 1, uniform
prior over two parameters, point posterior on the first ⇒ EIG = log 2. Your
test must reproduce that number.

## What to implement in `futon2.aif.parameter-delivery`

Input everywhere is the map `parameter-kernels` returns
(`:prior-kernel`, `:likelihood`, `:posterior-kernel`, `:evidence-normalizers`,
`:posterior-predictive`, `:theta`, `:model`). Read
`src/futon2/aif/machine_parameters.clj` and its test for the exact shapes —
note `:posterior-kernel` is keyed `[policy-id outcome]` and each entry is either
`{:ok true :evidence z :mass {θ p}}` or `{:ok false :refusal {...}}`.

1. `(parameter-information-gain kernels policy-id outcome)` → the KL above, a
   double, or a typed refusal map `{:status :refused :kind ...}`:
   - `:zero-evidence-conditioning` when that posterior entry is not `:ok`;
   - `:zero-prior-in-posterior-support` when some θ with positive posterior mass
     has zero prior mass (Lean's `positivePrior`);
   - θ with posterior mass 0 contribute 0 (the 0·log 0 convention), and that
     must be a test, not just a comment.

2. `(expected-information-gain kernels policy-id)` → the expectation above, or a
   typed refusal. An outcome with zero predictive mass contributes 0 and its
   posterior is NOT consulted (that is exactly the `:zero-evidence-conditioning`
   case); an outcome with positive mass whose posterior is refused makes the
   whole EIG the refusal.

3. `(delivery kernels {:keys [a-identity observations]})` → the delivered record

       {:schema :wm/parameter-delivery-v1
        :model <(:model kernels)>
        :a-identity <caller-supplied identity of the A these kernels were computed against>
        :observations <caller-supplied record of the observations the state distribution came from>
        :expected-information-gain {policy-id <value-or-refusal>}
        :posterior-kernel <(:posterior-kernel kernels)>
        :prior-kernel <(:prior-kernel kernels)>}

   `:a-identity` and `:observations` are REQUIRED with no default: missing
   either is the typed refusal `:a-identity-required` / `:observations-required`.
   This is the "observation-bound" half of the acceptance text — a delivery that
   cannot say which A and which observations it came from is refused rather
   than delivered.

4. `(stale? delivery {:keys [a-identity model]})` → `nil` when the delivery is
   current, else `{:stale true :because :a-changed | :model-revision-changed}`.
   This is WM-07's "refresh of registrations after A/model changes" clause.

5. `(refresh delivery {:keys [model parameter-state policies outcome-support
                             a-identity observations]})`
   → when `stale?`, recompute by calling `machine-parameters/parameter-kernels`
   (which re-hashes every registration pin — that re-hash is what makes the
   refresh real) and return a fresh `delivery`; when not stale, return the
   delivery unchanged with `:refreshed :not-required`. A registration refusal
   from `parameter-kernels` passes through untouched.

6. `(g-with-information-gain {:keys [g-fn g-input delivery policy-id]})` →
   `{:g <whatever g-fn returns on g-input> :expected-information-gain <value>
     :basis {:lean "DarkTower.WarMachine.Holes.expectedInformationGain"
             :delivery-schema :wm/parameter-delivery-v1}}`.

   This is the consumer hand-off: production passes
   `futon2.aif.active-horizon-g/active-horizon-g` as `g-fn`.
   **The information gain must NOT be added to G.** An epistemic term inside G
   is a separate, unapproved change (SPEC-flat-removal S2 / D3 amendment 4); if
   you add it here you change every production decision silently. Assert in a
   test that `:g` equals `(g-fn g-input)` exactly.

Everything in this namespace is pure: no file reads except the registration
re-hash that `parameter-kernels` itself does in `refresh`, no network.

## Tests (`test/futon2/aif/parameter_delivery_test.clj`)

- **the Lean fixture**: hand-built kernels matching `binaryFixture` ⇒
  EIG = `(Math/log 2)` within 1e-12. Name the Lean theorem in the test.
- 0·log 0: a posterior with a zero-mass θ in its support gives the same value as
  one without it.
- `:zero-prior-in-posterior-support` and `:zero-evidence-conditioning` refusals.
- **real kernels**: build them exactly as `test/futon2/aif/machine_parameters_test.clj`
  does (the registered hypotheses in `holes/labs/wm-contract/runs/row-11-parameters`),
  then assert EIG ≥ 0 for every policy, and EIG = 0 when the posterior equals the
  prior for every positive-mass outcome (make such a case by using a hypothesis
  set whose likelihood rows agree).
- `delivery` refuses without `:a-identity` and without `:observations`.
- `stale?` on a changed A identity, on a changed model revision, and on neither.
- `refresh` recomputes when stale (assert the returned delivery's `:a-identity`
  is the new one) and is a no-op when not.
- `g-with-information-gain` leaves `:g` exactly equal to the injected `g-fn`'s
  own return.

## Gates (all must pass; paste the output in your report)

    cd /home/joe/code/futon2
    clj-kondo --lint src/futon2/aif/parameter_delivery.clj test/futon2/aif/parameter_delivery_test.clj
    clojure -M:test -m cognitect.test-runner -n futon2.aif.parameter-delivery-test
    clojure -M:test -m cognitect.test-runner -n futon2.aif.machine-parameters-test   # must still pass

clj-kondo clean (0 errors, 0 warnings). Then commit ONLY your two files.

## Report back

Bell claude-4 with: the commit sha, test counts, clj-kondo output, the EIG values
you got on the real row-11 kernels (both policies), and any place where the Lean
definition did not translate directly into Clojure and what you did instead.
Do not open a review cycle; claude-4 accepts by reading the diff and rerunning
the tests.
