Stage 1 — THE CLEAN PROOF
--------------------------------

*(a)* True. The Lebesgue σ-algebra is the completion of the Borel σ-algebra with respect to Lebesgue measure. Therefore, for any Lebesgue measurable `A`, there exists a Borel set `B` such that `m(A Δ B) = 0`, i.e., `m((A \ B) ∪ (B \ A)) = 0`.

*(b)* True. Approximate `f` by simple functions `s_k = ∑ α_{k,i} χ_{E_{k,i}}` converging a.e. to `f`. Replace each measurable set `E_{k,i}` by a Borel set `B_{k,i}` that differs from it only on a null set (using part (a)). The resulting simple functions `t_k = ∑ α_{k,i} χ_{B_{k,i}}` are Borel and still converge to `f` almost everywhere, so their pointwise limit `g` is Borel and equals `f` a.e.

Stage 2 — LEMMA DEPENDENCY GRAPH
--------------------------------

1. **Lebesgue completion lemma** (Mathlib: `NullMeasurableSet.toMeasurable_ae_eq` / `ae_eq_set`): any Lebesgue measurable set equals a Borel set a.e. Critical for (a).
2. **Simple-function approximation** (Mathlib: `aeMeasurable.approx_simple_function`): measurable functions are a.e. limits of simple functions with measurable supports.
3. **Replacement by Borel supports** (custom instantiation of lemma 1): swap each measurable support set of a simple function with a Borel representative differing only on a null set.
4. **Limit of Borel simple functions** (Mathlib: Borel σ-algebra closed under pointwise limits of simple functions): ensures the limiting function is Borel and equals the original a.e.

Stage 3 — LEAN FORMALIZATION
----------------------------
- `apm-lean/lean-proofs/a01J02/Main.lean` formalizes (a) as `exists_borel_ae_eq` (and the symmetric-difference corollary) using `NullMeasurableSet.toMeasurable_ae_eq` plus `ae_eq_set`.  
- `exists_borel_function_on_Icc` proves (b) via the `AEMeasurable.mk` construction, producing a Borel `g` that equals `f` a.e. on `[0,1]`.  
- `lake build` succeeds (only the usual unrelated warnings remain), so the Lean development is `sorry`-free.

Stage 4 — FORMAL-TO-INFORMAL REVISION
-------------------------------------
- The Lean realization shows that invoking `toMeasurable` (sets) and `AEMeasurable.mk` (functions) is the entire game—there’s no need to construct approximations manually once those APIs are on the table.  
- The main pitfall is remembering to interpret “Lebesgue measurable” as “null measurable” so those lemmas apply; after that, the rest is bookkeeping about almost-everywhere equality.

Stage 6 — INTEGRATE
-------------------
- **Connections**  
  - Completion arguments (`toMeasurable`, `AEMeasurable.mk`) show up in Vitali convergence, Dunford–Pettis compactness, and de la Vallée Poussin’s criterion, as well as in probability when replacing random variables by Borel versions before taking expectations.  
  - Simple-function approximations plus null-set replacements are the standard route to proving results about conditional expectation and Radon–Nikodym densities.
- **EXAM-DAY FIELD KIT**  
  1. `NullMeasurableSet.toMeasurable_ae_eq`: every Lebesgue measurable set equals a Borel set a.e.  
  2. `ae_eq_set`: converts a.e. equality into the two difference-measure-zero statements.  
  3. `AEMeasurable.mk`: given a Lebesgue measurable `f`, produce a Borel `g` with `f =ᵐ g`.  
  4. Simple-function approximation: any Lebesgue measurable `f` is an a.e. limit of simple functions with measurable supports.
- **ArSE Questions**  
  1. *Why is this hard?* Because you must recall that Lebesgue measurability means “Borel up to a null set”; without that completion perspective, the statements look mysterious.  
  2. *What is the key insight?* Use `toMeasurable` for sets and `AEMeasurable.mk` (or simple-function replacements) for functions to get Borel representatives.  
  3. *Why does step N work?* The a.e. equalities from `toMeasurable` feed into `ae_eq_set`, delivering the symmetric-difference measure-zero property the problem demands.  
  4. *What connects to this?* The same completion and null-set replacement techniques appear in Vitali convergence, Dunford–Pettis, de la Vallée Poussin, and probabilistic arguments about random variables.  
  5. *Where is intuition wrong?* It feels like you need to explicitly “fix” sets/functions by hand, but the surprise is that completion already packages the fix—invoking the right lemmas instantly produces the Borel versions.
