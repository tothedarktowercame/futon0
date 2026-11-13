### EXECUTE

**Stage 1 — THE CLEAN PROOF**
--------------------------------

Let `E ⊂ ℝ` be measurable. Then

`τ(E) = μ(P⁻¹E)`,

and by definition of `μ`,

`τ(E) = ∬ 2 · 1_{0 ≤ y ≤ x ≤ 1} · 1_E(x) dx dy`.

Integrating first in `y`, for each fixed `x` with `0 ≤ x ≤ 1` the allowable
values of `y` run from `0` to `x`, so the slice has length `x`. Therefore

`τ(E) = ∫_E 2x 1_[0,1](x) dx`.

So `τ` is absolutely continuous with respect to Lebesgue measure and has
density

`dτ/dm = 2x 1_[0,1](x)`.

Now decompose Lebesgue measure `m` relative to `τ`.

Write

`m = m|_[0,1] + m|_{(-∞,0) ∪ (1,∞)}`.

The second piece is singular with respect to `τ`, because `τ` is supported on
`[0,1]` and vanishes on `(-∞,0) ∪ (1,∞)`.

For the first piece, if `E ⊂ [0,1]` and `τ(E) = 0`, then

`∫_E 2x dx = 0`.

Since `2x > 0` for almost every `x ∈ (0,1]`, this forces `m(E) = 0`. Hence
`m|_[0,1] ≪ τ`. The Radon–Nikodym derivative is the reciprocal of the density
of `τ` on the support:

`d(m|_[0,1])/dτ = 1/(2x)` almost everywhere on `(0,1]`.

At `x=0`, the value is irrelevant because a single point has Lebesgue measure
zero. Thus one acceptable answer is:

- singular part:
  `m_s = m|_{(-∞,0) ∪ (1,∞)}`
- absolutely continuous part:
  `m_a = m|_[0,1]`
- Radon–Nikodym derivative:
  `dm_a/dτ = 1/(2x)` almost everywhere on `(0,1]`

equivalently, `1/(2x) · 1_(0,1](x)` if one wants a total-space formula.

**Stage 2 — LEMMA DEPENDENCY GRAPH**
--------------------------------

1. **Pushforward by vertical slicing**
   - **Formal dependency**: Fubini/Tonelli plus the definition of pushforward measure.
   - **Informal dependency**: when a projection forgets one coordinate, compute the image measure by integrating the original density over the fibers.
   - **Why this becomes thinkable here**: the measure is explicitly given on a triangular region and the map `P(x,y)=x` turns each fiber into a vertical segment.
   - **Lean target/type**: a statement identifying the candidate pushforward density `τ(E) = ∫_E (2x 1_[0,1](x)) dx`.
   - **Mathlib status/search terms**: likely needs custom wiring; search `Measure.map`, `Measure.restrict`, `lintegral_indicator`, `integral_indicator`.
   - **Critical path**: yes — this is the move that turns the geometry into one-variable measure theory.

2. **Support split for the Lebesgue decomposition**
   - **Formal dependency**: decompose a measure into its restriction to the support and its restriction to the complement.
   - **Informal dependency**: once a measure is supported on `[0,1]`, everything outside that set must contribute to the singular part of any opposite-direction decomposition.
   - **Why this becomes thinkable here**: after computing `τ`, we see immediately that it vanishes off `[0,1]`.
   - **Lean target/type**: a statement that `m = m|_[0,1] + m|_{(-∞,0) ∪ (1,∞)}` with the second part singular to `τ`.
   - **Mathlib status/search terms**: likely available in pieces; search `Measure.restrict`, `MutuallySingular`, `Measure.add_apply`.
   - **Critical path**: yes — this is the step that keeps the direction of the decomposition straight.

3. **Reciprocal density on the support**
   - **Formal dependency**: if `τ = f · m` with `f > 0` almost everywhere on a set, then `m` restricted to that set has density `1/f` with respect to `τ`.
   - **Informal dependency**: once one measure is obtained from another by multiplying by a positive density, the reverse derivative is the reciprocal on the positive-density region.
   - **Why this becomes thinkable here**: the slice computation yields the concrete density `2x`, which is positive almost everywhere on `(0,1]`.
   - **Lean target/type**: a statement identifying `dm/dτ = 1/(2x)` almost everywhere on `(0,1]`.
   - **Mathlib status/search terms**: likely partial; search `Measure.rnDeriv`, `withDensity`, `absolutelyContinuous`, `ae`.
   - **Critical path**: yes — it produces the actual derivative asked for.

**PROOF-PLAN.EDN**
```edn
{:goal "Compute τ = μ ∘ P⁻¹ explicitly, then decompose Lebesgue measure m with respect to τ and identify dm/dτ on the absolutely continuous part."
 :terms [{:name "pushforward measure"
          :meaning "the measure obtained by collapsing each vertical fiber of the triangle onto its x-coordinate"
          :needed-because "the problem is impossible until τ is made explicit"}
         {:name "Lebesgue decomposition"
          :meaning "split m into an absolutely continuous part and a singular part relative to τ"
          :needed-because "the problem asks for the decomposition of m with respect to τ, not vice versa"}
         {:name "Radon–Nikodym derivative"
          :meaning "the density of the absolutely continuous part of m relative to τ"
          :needed-because "the final answer must identify the reciprocal density on the support"}]
 :strategy [{:id :slice-the-triangle
             :formal-dependency "Fubini/Tonelli on the triangle"
             :informal-dependency "integrate over the forgotten coordinate to compute the pushforward"
             :why-this-now "P(x,y)=x turns the problem into a fiber-length computation"
             :lean-target "τ(E) = ∫_E (2 * x * 1_[0,1](x)) dx"
             :mathlib-status "custom wiring around Measure.map / indicator integrals"
             :critical-path true}
            {:id :split-by-support
             :formal-dependency "restrict a measure to the support and its complement"
             :informal-dependency "once τ vanishes off [0,1], everything there is automatically singular"
             :why-this-now "the pushforward density is visibly supported on [0,1]"
             :lean-target "m = m.restrict [0,1] + m.restrict ((-∞,0) ∪ (1,∞))"
             :mathlib-status "likely available in pieces via restrict/add/singular"
             :critical-path true}
            {:id :invert-the-density
             :formal-dependency "reciprocal Radon–Nikodym density on the positive-density region"
             :informal-dependency "reverse a positive density by taking its reciprocal almost everywhere"
             :why-this-now "τ has density 2x, positive almost everywhere on (0,1]"
             :lean-target "dm/dτ = (2 * x)⁻¹ on (0,1] a.e."
             :mathlib-status "partial `rnDeriv` support; likely custom glue"
             :critical-path true}]
 :stage-status {:stage1 :done
                :stage2 :done
                :stage3 :blocked
                :stage4 :done}}
```

**Stage 3 — LEAN FORMALIZATION**
--------------------------------

Lean file:

```lean4-main
/home/joe/code/apm-lean/lean-proofs/worked/a02J04/Main.lean
```

What is actually closed there:

- definitions of the candidate density `tauDensity`, the candidate derivative
  `dmDtau`, and the support sets `mAcSet`, `mSingSet`;
- algebraic lemmas showing:
  - `tauDensity x = 2x` on `[0,1]`,
  - `tauDensity x = 0` off `[0,1]`,
  - `dmDtau x = (2x)⁻¹` on `(0,1]`,
  - `dmDtau x * tauDensity x = 1` on `(0,1]`,
  - `[0,1] ∪ ((-∞,0) ∪ (1,∞)) = ℝ`,
  - these two regions are disjoint;
- a bundled theorem `workedAnswer_holds` establishing the candidate support
  split and reciprocal-density identity.

Build status:

- `lake env lean /home/joe/code/apm-lean/lean-proofs/worked/a02J04/Main.lean`
  succeeds with zero `sorry`.

Honest blocker:

The file does **not** yet formalize the full original theorem. The missing step
is the measure-theoretic identification

- `τ = (2x 1_[0,1]) · m`

together with the actual Lebesgue decomposition/Radon–Nikodym theorem stated in
measure-theoretic terms. That requires `Measure.map`, `withDensity`, and
`rnDeriv` wiring that was not completed here. So this is a real partial:
the candidate answer is formalized nontrivially, but the final bridge from the
candidate objects to the actual pushforward measure is still open.

**Stage 4 — FORMAL-TO-INFORMAL REVISION**
--------------------------------

The proof is conceptually simpler than it first looks: once the pushforward is
computed, the decomposition is almost forced by support considerations. The real
crux is not the Radon–Nikodym theorem itself but noticing that the problem asks
for `m` relative to `τ`, so one must first compute `τ` and then reverse the
density only on the region where that makes sense.

The fiddly part is bookkeeping, not invention. One has to keep three layers
separate: the geometric slice computation, the support split, and the reciprocal
density formula. The Lean work confirms that this bookkeeping layer is the right
place to isolate the formal scaffolding.
