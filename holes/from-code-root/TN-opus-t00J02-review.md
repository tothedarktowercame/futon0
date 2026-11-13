# TN: Independent review of the t00J02 proof effort

- **Reviewer:** Claude (Opus 5), at Joe's request, 2026-08-26
- **Subject:** the t00J02 campaign coordinated by `codex-18` and its helper strands
- **Repo reviewed:** `/home/joe/code/apm-lean-library-loop-t00J02-solver`,
  branch `qualification/library-loop-t00J02-solver`, HEAD `f9320ba3`
  (*Add rotation paths and vertex incidence interfaces*, 2026-08-26 21:23 UTC)
- **Context:** codex-18 was parked on three in-flight jobs at review time
  (`park-73dec3f6`). Findings are a snapshot at `f9320ba3`.

## Problem

> Let $\Sigma$ be a smooth surface of genus two and let $f:\Sigma\to\Sigma$ be a
> smooth map. Let $C\subset\Sigma$ be a smooth curve wrapping around one of the
> handles. Suppose $f$ is transverse to $C$. Prove that if every component of
> $f^{-1}(C)$ bounds an embedded disk in $\Sigma$, then the degree of $f$ is zero.

## What I checked

Not a read-through. The following were run:

| Check | Result |
|---|---|
| `lake env lean problems/t00J02/lean/Main.lean` | exit 0; one warning, `declaration uses 'sorry'` at line 248 (`apm_t00J02_bridge_1`) |
| `#print axioms` on `topHomologyEndomorphism_eq_zero_of_survivorComponents` and `capHomology_eq_zero_of_component_disks` | `[propext, Classical.choice, Quot.sound]` — nothing smuggled |
| Import closure of `Main.lean` computed and diffed against `ConstructionTargets/` | 18 modules reachable; 75 `.lean` files total (36,653 lines) |
| `grep -rl "C(S, S)" ConstructionTargets/*.lean` | **0 files** |
| `grep -rln "ContMDiff" ConstructionTargets/*.lean` | 2 files of 75 |
| `grep -rn "endomorphismCoefficient"` outside its home module | 2 occurrences, both as an assumed hypothesis |
| `git log --follow` on `problems/t00J02/lean/Main.lean` | `bridge_1` unchanged since `038731ad` stated it |

codex-18's build and axiom claims in `*codex-repl:codex-18*` reproduce. They are
accurate.

## Credit where due

The module-level discipline is real and unusual. Every construction target is
sorry-free and axiom-audited, and the seam `.md` documents repeatedly and
explicitly refuse to assume their own conclusions. Three examples:

- `CircleReflectionSignAssembly.md`: "Assuming the displayed equality would be
  exactly the missing circle-sign theorem in transported form, so it is not
  introduced as a field or hypothesis here."
- `ProductionTaggedFaceLedgerAssembly.md`: "No global partner property,
  cyclicity, or cancellation is assumed here."
- `OrientedSurfaceVertexLink.md`: "Adding either 'one orbit' or 'one positive
  block' to the present ledger would restate those conclusions rather than
  prove them."

The frozen statement in `Main.lean` is also a faithful and non-vacuous
formalization. `hasGenusTwo` is stated homologically (weaker hypothesis, so a
stronger theorem); `wrapsHandle` is `[C] ≠ 0` in `H₁`; `hasDegree` pins the
integer to the actual endomorphism of `H₂ ≅ ℤ` and is satisfiable, so the
conclusion is not vacuously true.

## Finding 1 — `f` never appears, and the crux is assumed

`grep -rl "C(S, S)" ConstructionTargets/*.lean` returns nothing. Across ~9,000
lines of transverse / cap / disk machinery, **no continuous self-map of the
surface occurs anywhere**. Only 2 of 75 files mention `ContMDiff` at all
(`TransversePreimageDuality`, `TransverseOverlapLinearization`); every module
named `Transverse*` besides those two carries `transverse` as an opaque
predicate field over a free cocycle `a` and a free chain `z`.

The chain terminates in `TransverseSurvivorComponents.lean:1161`, whose own
docstring calls it "the final rank-one top-homology consumer":

```lean
theorem topHomologyEndomorphism_eq_zero_of_survivorComponents
    {S α ι H₂ : Type} [TopologicalSpace S] [Fintype ι] [DecidableEq ι]
    [AddCommGroup H₂] [Module ℤ H₂]
    {U : α → Set S} {a : SingularOneCocycle (TopCat.of S)}
    {z : (SingularChains (TopCat.of S)).X 2}
    (A : ChartwiseCapAssignment (ι := ι) U a z)
    (P : InteriorBackEdgePairing (chartwiseCapTerm A))
    (locus : Set S) ...
    (D : SurvivorComponentDecomposition A P locus)
    (hloopZero : ∀ component, circleHandleClass (D.loop component) = 0)
    (orientation : TopHomologyOrientation H₂) (F : H₂ →ₗ[ℤ] H₂)
    (pairing intersection : ℤ)
    (hpairing : (SingularChains (TopCat.of S)).homologyπ 1
                  (capCycles a A.fundamentalCycle) = 0 → pairing = 0)
    (hnaturality : pairing =
      endomorphismCoefficient orientation F * intersection)
    (hintersection : intersection ≠ 0) : F = 0
```

`H₂`, `F`, `orientation`, `pairing` and `intersection` are free variables with
no relation to `S`, to `a`, or to any map. The geometric arguments `A`, `P`,
`locus`, `D` are used for exactly one purpose in the proof body: discharging the
antecedent of `hpairing`. Strip them and the remaining proof is `mul_eq_zero`
(see `endomorphism_eq_zero_of_vanishing_pairing_naturality`,
`OrientedSurfacePreimageDuality.lean:280`, which is that lemma verbatim).

`hnaturality` is the degree identity

$$\langle \mathrm{PD}(f^*\alpha),\ \mathrm{PD}(f^*\beta)\rangle \;=\; \deg(f)\cdot\langle\alpha,\beta\rangle$$

— the crux of the problem — assumed outright. It occurs in exactly two places in
the repository (`DiskBoundedPreimageDuality.lean:375` and the theorem above), as
a hypothesis both times, and is discharged nowhere.
`topHomologyEndomorphism_eq_zero_of_survivorComponents` has no consumers; it is a
leaf.

This is the same pattern the seam docs elsewhere correctly refuse. It is being
done here.

### 1a — the constructed pairing cannot support that identity

`apm_t00j02_surfacePoincareDuality` is built by transporting the standard
symplectic form on `ℤ⁴` through the *arbitrary* coordinate isomorphism that
`hasGenusTwo` supplies. `SurfacePoincareDuality` is literally
`toDual : H₁ ≃ₗ[ℤ] Module.Dual ℤ H₁` (`TransversePreimageDuality.lean:42`) —
i.e. *a* perfect pairing, chosen, not the geometric intersection form.

The degree identity is true only for the geometric form. So the pairing the
naturality theorems are proved against cannot in principle support the identity
being assumed about it. Closing `hnaturality` requires first identifying the
transported form with the cup-product-against-fundamental-class form, which is
not currently a stated target.

## Finding 2 — the trunk has not moved

`apm_t00J02_bridge_1` is unchanged since commit `038731ad` first stated it. The
four items under "Remaining boundary" in `problems/t00J02/proof-outline.md` are
word-for-word what they were:

1. Compare the constructed Poincaré-dual pullback class with the geometrically
   oriented components of the transverse inverse image.
2. Identify its pairing with the handle class with `endomorphismCoefficient`.
3. Prove from transversality that each inverse-image component is open, then
   orient the finite component family and compare its sum with the
   Poincaré-dual preimage class.
4. Formalize naturality / Poincaré duality to conclude that `f` induces zero on
   top singular homology.

None of the past week's work reduces any of the four.

Of the 75 modules, 32 t00J02-specific ones (~7,900 lines) sit outside `Main`'s
import closure; 29 of those 32 were touched on 2026-08-26 alone. Leaf growth is
normal for bottom-up library work — but here the leaves are growing away from a
trunk that has not moved, and each new seam doc names a *new* missing upstream
theory rather than closing one. Current tally of open upstream requirements:

- GL₂(ℝ) determinant-component paths
- sphere/disk reflection sign naturality
- vertex-link cyclic order and one-orbit connectedness
- convex-polygon boundary-order (stated as "or an equivalent polygonal
  Jordan-curve theorem")
- the whole `ChartwiseCapAssignment` + `InteriorBackEdgePairing` constructor

That is a simplicial oriented-surface theory plus a local degree theory — several
Mathlib PRs' worth — not a proof of t00J02.

`ChartwiseCapCancellation` is the honest version of this: a real, well-built
*conditional* interface whose own doc says "The module deliberately does not
claim that these data exist." Its remaining obligation, restated, is bridge_1's
geometric content.

## Finding 3 — two current obstructions are cheaper than the docs state

Both are live strands as of `f9320ba3`; worth relaying to codex-18 now.

### GL₂ paths

`PlaneLinearDeterminantComponents.md` plans a general elementary-factor
decomposition (pivot cases, Gaussian elimination) and reports that Mathlib has
"no polar/QR decomposition continuous in the input matrix."

Continuity in the input is not needed. The path is in `t` with `A` **fixed**. So:

1. Gram–Schmidt the two columns explicitly: `A = Q·R`, with `R` upper triangular
   and positive diagonal (positive because `det A > 0` and `|c₁| > 0`), and `Q`
   orthogonal with `det Q = +1`, hence a rotation.
2. `R_t = (1-t)·R + t·I` is upper triangular with positive diagonal for all
   `t ∈ [0,1]`, hence invertible throughout.
3. `Q` rotates to `I` by angle — `rotationPath` already exists in that file.

All explicit in 2×2 coordinates; no decomposition theorem needed. Plausibly ~150
lines against the multi-case pivot program currently planned.

### Convex polygon boundary order

`AffinePolygonalLinkCrossing.md` states the missing input as "a concrete
convex-boundary-order theorem (or an equivalent polygonal Jordan-curve
theorem)." That obstruction exists only because the file tries to *recover*
cyclic order from an arbitrary `convexHull` generator enumeration, which indeed
forgets it.

But the vertex link is being **constructed**. Choose the angular
parametrization at construction time; monotonicity of the linear functional on
each boundary arc then follows from angular monotonicity. This converts "Mathlib
lacks a theorem" into "we pick the order and prove ours is convex-positioned" —
which the file's own `TwoMonotoneSupportChains` is already shaped to consume.

## Recommendation

The infrastructure is sound and clean; the mathematical content proved toward
`bridge_1` is approximately zero; and the one place the argument is assembled
end-to-end does it by assuming the theorem.

**Before dispatching more parallel strands, have codex-18 write the conditional
`bridge_1`.** That is: actually apply the consumer chain to the frozen `f`, `c`,
`hS`, `htrans`, `hdisks`, leaving `sorry` only at the genuinely missing
geometric constructors. This forces `a`, `z`, `pairing`, `intersection` and `F`
to be instantiated with the real objects — which is exactly what would have
caught Finding 1 — and it converts a growing pile of leaves into a measurable
"N sorries remaining, here is each one" number.

**Second, cost the alternative route before committing further.** The classical
short proof is innermost-disk surgery: use the disk-bounding hypothesis to
homotope `f` off `C` one innermost component at a time; once `f` misses a point
`p ∈ C`, it factors through `Σ ∖ {p}`, and `H₂(Σ ∖ {p}) = 0` gives `deg f = 0`.
That avoids Poincaré duality entirely. I have **not** costed it — the surgery
induction may well be worse in Lean than the cap-product route — but the current
route needs a triangulated-surface theory, so the comparison is worth making
explicitly rather than by default.

## Note on the coordination pattern

Nothing here suggests dishonesty by codex-18. Its own documents say the
geometric middle is missing, and its per-strand independent verification is
genuine. The failure mode is subtler: three parallel strands all reporting
"builds, axiom-clean, no sorry" reads as progress, and the one number that would
show otherwise — distance from `bridge_1` — is not being tracked by anything.
A green strand that lands 300 sorry-free lines into a module nothing imports is
indistinguishable, in the bell summary, from one that closes a gap.
