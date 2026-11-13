Stage 1 — THE CLEAN PROOF
--------------------------------

Let `μ_n` be the Lebesgue–Stieltjes measure associated with the nondecreasing function `f_n`; since `f_n ≥ 0` and `f = ∑ f_n` converges pointwise, the total measure `μ = ∑ μ_n` is finite on bounded intervals. Lebesgue’s differentiation theorem (or the Radon–Nikodym theorem applied to `μ`) says that every monotone function is differentiable almost everywhere and that its derivative coincides a.e. with the Radon–Nikodym derivative of the associated measure. Because `μ = ∑ μ_n`, the derivative of its distribution function `f` is the sum of the Radon–Nikodym derivatives of the `μ_n`, so `f'(x) = ∑ f_n'(x)` for almost every `x`.

Stage 2 — LEMMA DEPENDENCY GRAPH
--------------------------------

1. **Differentiability of monotone functions**
   - **Formal dependency**: `MeasureTheory.LebesgueDifferentiationTheorem` for monotone functions / `Monotone.ae_deriv_eq_slope` (or Radon–Nikodym derivative equals classical derivative a.e.)
   - **Informal dependency**: “Every monotone function is differentiable almost everywhere.”
   - **Why this becomes thinkable here**: each `f_n` is nondecreasing; we need their derivatives to exist a.e.
   - **Lean target/type**: `∀ᵐ x, HasDerivative f_n (fun x => f_n' x)` or, more realistically, existence of a function `g_n` with `AE`-eq derivative.
   - **Mathlib status/search terms**: likely Mathlib; search for “monotone a.e. differentiable”, “Stieltjes measure derivative”.
   - **Critical path**: yes — without it we cannot even talk about `f_n'` a.e.

2. **Measure decomposition**
   - **Formal dependency**: `Measure.map` / `Measure.sum` + Radon–Nikodym theorem: the measure induced by `f` equals the sum of the measures induced by the `f_n`.
   - **Informal dependency**: “Stieltjes measures add when the generating monotone functions add.”
   - **Why this becomes thinkable here**: pointwise equality `f = ∑ f_n` with nonnegative monotone summands suggests summing the associated measures.
   - **Lean target/type**: `μ = Measure.sum μ_n`, `μ.withDensity f' = ...`.
   - **Mathlib status/search terms**: probably Mathlib (Radon–Nikodym); search “Stieltjes measure sum”.
   - **Critical path**: yes — needed to pass from derivatives of the `f_n` to the derivative of `f`.

3. **Interchange of derivative and series**
   - **Formal dependency**: Monotone convergence for difference quotients or Radon–Nikodym linearity: `(∑ μ_n)' = ∑ μ_n'`.
   - **Informal dependency**: “Limits commute because of monotone convergence / linearity of Radon–Nikodym derivatives.”
   - **Why this becomes thinkable here**: difference quotients `(f(x+h)-f(x))/h` are sums of nonnegative terms, so monotone convergence applies.
   - **Lean target/type**: `∃ W`, `∀ᵐ x, deriv f x = ∑ deriv f_n x`.
   - **Mathlib status/search terms**: custom — search “ae_tendsto”, “sum_normed_deriv”.
   - **Critical path**: yes — delivers the final statement.

Stage 3 — LEAN FORMALIZATION
----------------------------
Blocked: locating the precise Mathlib lemmas for “monotone functions are a.e. differentiable” and for summing the corresponding Radon–Nikodym derivatives requires a deeper search. A quick `rg` over `.lake/packages/mathlib` for `"Monotone"` shows many hits but no obvious ready-made theorem, so setting up the measure-theoretic framework would exceed the allotted time this pass.

Stage 4 — FORMAL-TO-INFORMAL REVISION
-------------------------------------
The Lean obstacles underline the informal message: the proof hinges on measure-theoretic differentiation (monotone ⇒ a.e. differentiable) and linearity of Radon–Nikodym derivatives. These are the real “heavy moves”; once they’re granted, the rest is bookkeeping.
