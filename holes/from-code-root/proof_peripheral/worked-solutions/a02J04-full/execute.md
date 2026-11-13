### EXECUTE

**Stage 1 — THE CLEAN PROOF**
--------------------------------

Let `E ⊂ ℝ` be measurable. Then

`τ(E) = μ(P⁻¹E)`,

and by definition of `μ`,

`τ(E) = ∬ 2 · 1_{0 ≤ y ≤ x ≤ 1} · 1_E(x) dx dy`.

Integrating first in `y`, for each fixed `x` with `0 ≤ x ≤ 1` the allowable
values of `y` run from `0` to `x`, so the slice has length `x`. Therefore

`τ(E) = ∫_E 2x 1_[0,1](x) dx`,

so `τ = (2x 1_[0,1](x)) · m`.

Now decompose Lebesgue measure `m` relative to `τ`.

The density `2x` is positive on `(0,1]`, zero at `0`, and zero off `[0,1]`.
So the singular part must live on the complement of `(0,1]`, namely

`(-∞,0] ∪ (1,∞)`.

Accordingly,

`m_s = m|_{(-∞,0] ∪ (1,∞)}`

and the absolutely continuous part is

`m_a = m|_(0,1]`.

On `(0,1]`, the reciprocal density is `1/(2x)`, so

`dm_a/dτ = 1/(2x)` almost everywhere on `(0,1]`.

Equivalently, as a whole-space formula,

`dm/dτ = (1/(2x)) · 1_(0,1](x)`  `τ`-almost everywhere,

and

`m = m|_{(-∞,0] ∪ (1,∞)} + τ.withDensity((1/(2x))1_(0,1])`.

**Stage 2 — LEMMA DEPENDENCY GRAPH**
--------------------------------

1. **Pushforward by vertical slicing**
   - **Formal dependency**: `fst` of a `withDensity` on `volume.prod volume`, plus Tonelli on `s × univ`.
   - **Informal dependency**: compute the marginal by integrating the triangular density over each vertical fiber.
   - **Why this becomes thinkable here**: the projection `P(x,y)=x` turns the geometry into a one-variable section integral.
   - **Lean target/type**: `τ = volume.withDensity tauDensity`.
   - **Mathlib status/search terms**: `Measure.fst_apply`, `setLIntegral_prod`, `withDensity_apply`, `withDensity_congr_ae`.
   - **Critical path**: yes.

2. **Support split for the singular part**
   - **Formal dependency**: `restrict_compl_add_restrict`, `MutuallySingular.mk`, `eq_singularPart`.
   - **Informal dependency**: once `τ` is supported on `(0,1]`, Lebesgue measure outside that region is forced into the singular part.
   - **Why this becomes thinkable here**: the pushforward density vanishes exactly where the reciprocal density is meaningless.
   - **Lean target/type**: `volume.singularPart τ = volume.restrict mSingSet`.
   - **Mathlib status/search terms**: `MutuallySingular`, `restrict_add_restrict_compl`, `eq_singularPart`.
   - **Critical path**: yes.

3. **Reciprocal density for the absolutely continuous part**
   - **Formal dependency**: `withDensity_mul`, `withDensity_indicator_one`, `eq_rnDeriv`.
   - **Informal dependency**: on the positive-density region, the reverse derivative is the reciprocal.
   - **Why this becomes thinkable here**: after the marginal is explicit, the decomposition is a uniqueness argument, not a second ad hoc computation.
   - **Lean target/type**: `dmDtau =ᵐ[τ] volume.rnDeriv τ`.
   - **Mathlib status/search terms**: `withDensity_mul`, `withDensity_apply_eq_zero`, `eq_rnDeriv`.
   - **Critical path**: yes.

**PROOF-PLAN.EDN**
```edn
{:goal "Compute τ = μ ∘ P⁻¹ explicitly, then prove the singular part and Radon–Nikodym derivative of Lebesgue measure relative to τ."
 :terms [{:name "marginal measure"
          :meaning "the first-coordinate image of the triangular measure"
          :needed-because "the problem is impossible until τ is explicit"}
         {:name "singular part"
          :meaning "the restriction of Lebesgue measure to the region where τ vanishes"
          :needed-because "the decomposition is in the direction m relative to τ"}
         {:name "Radon–Nikodym derivative"
          :meaning "the reciprocal density on the positive-density region"
          :needed-because "the absolutely continuous part must be expressed with respect to τ"}]
 :strategy [{:id :slice
             :formal-dependency "Tonelli on the section integral"
             :informal-dependency "integrate over the forgotten y-coordinate"
             :why-this-now "the projection turns the triangle into intervals Icc(0,x)"
             :lean-target "τ = volume.withDensity tauDensity"
             :mathlib-status "closed"
             :critical-path true}
            {:id :singular
             :formal-dependency "restrict decomposition plus uniqueness of singularPart"
             :informal-dependency "Lebesgue mass where τ vanishes must be singular"
             :why-this-now "the support of τ is now explicit"
             :lean-target "volume.singularPart τ = volume.restrict mSingSet"
             :mathlib-status "closed"
             :critical-path true}
            {:id :reciprocal
             :formal-dependency "withDensity multiplication and uniqueness of rnDeriv"
             :informal-dependency "invert the density only where it is positive"
             :why-this-now "after the pushforward is known, the reciprocal formula is canonical"
             :lean-target "dmDtau =ᵐ[τ] volume.rnDeriv τ"
             :mathlib-status "closed"
             :critical-path true}]
 :stage-status {:stage1 :done
                :stage2 :done
                :stage3 :done
                :stage4 :done}}
```

**Stage 3 — LEAN FORMALIZATION**
--------------------------------

Lean files:

```lean4-main
/home/joe/code/apm-lean/ApmCanaries/A02J04Full.lean
/home/joe/code/apm-lean/lean-proofs/worked/a02J04/Full.lean
```

What is actually closed there:

- the triangular density `μ` and its marginal `τ`;
- the section computation proving `τ = volume.withDensity tauDensity`;
- the singular witness showing `volume.restrict mSingSet ⟂ₘ τ`;
- the explicit decomposition
  `volume = volume.restrict mSingSet + τ.withDensity dmDtau`;
- the uniqueness steps
  `volume.singularPart τ = volume.restrict mSingSet`
  and
  `dmDtau =ᵐ[τ] volume.rnDeriv τ`.

Build status:

- `lake build ApmCanaries.A02J04Full` succeeds
- `sorry` count: `0`

This is the difference from the earlier scaffold version: the file no longer
stops at the candidate objects. It now closes the actual
`Measure.map`/`withDensity`/`rnDeriv` bridge and proves the real
measure-theoretic statement.

**Stage 4 — FORMAL-TO-INFORMAL REVISION**
--------------------------------

The full proof confirms that the genuinely hard step is not “guess the answer”
but “package the answer in the right uniqueness theorem.” The geometry suggests
the density immediately; the real formal work is:

- expressing the marginal as a `withDensity`,
- splitting Lebesgue measure against the support of that marginal,
- and then using uniqueness of `singularPart` and `rnDeriv` rather than trying
  to re-prove Radon–Nikodym from scratch.

That is exactly the extra layer the scaffold was missing.
