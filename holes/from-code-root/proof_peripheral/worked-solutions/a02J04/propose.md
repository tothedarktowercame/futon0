### PROPOSE

**THE KEY INSIGHT**

Compute `τ` by integrating out the forgotten `y`-coordinate. For a measurable
set `E ⊂ ℝ`,
`τ(E) = μ(P⁻¹E)` is the `μ`-mass of all points in the triangle whose first
coordinate lies in `E`. Since the vertical slice at `x` has length `x` and the
density is `2`, the slice contributes mass `2x`. Therefore

`τ = (2x 1_[0,1](x)) m`.

Once that is known, the rest is a support decomposition:

- on `[0,1]`, `m` is absolutely continuous with respect to `τ`, with
  derivative `1 / (2x)` almost everywhere;
- on `(-∞,0) ∪ (1,∞)`, `τ` is zero, so Lebesgue measure there is singular with
  respect to `τ`.

So the entire problem collapses to "compute the pushforward density first, then
invert it on the support and peel off the outside-support part as singular."

**THE NAIVE APPROACH THAT FAILS**

The standard wrong move is to write immediately

`dm/dτ = 1/(2x)`

on all of `ℝ`. That is false for two reasons. First, outside `[0,1]`, the
measure `τ` is zero, so there is no absolutely continuous part there at all;
that mass must go into the singular component. Second, even on the support one
must only claim the derivative almost everywhere, because the point `x=0` is a
zero-density endpoint where the reciprocal formula is not literally defined.

Another unhelpful route is to attack the Radon–Nikodym derivative before
computing `τ`. That reverses the logical order. Until the pushforward measure is
explicit, there is nothing concrete to differentiate against.
