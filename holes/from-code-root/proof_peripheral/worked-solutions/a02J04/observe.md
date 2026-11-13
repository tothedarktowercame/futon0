### OBSERVE

**WHAT IS REALLY BEING ASKED**

The first task is to understand the pushforward measure `τ = μ ∘ P⁻¹` on the
`x`-axis. The original measure `μ` lives on the triangle `0 ≤ y ≤ x ≤ 1` with
density `2`, and the projection `P(x,y)=x` forgets the `y`-coordinate. So the
real question is: after collapsing each vertical slice of the triangle onto its
`x`-value, how much mass lands at each `x`?

The second task is deliberately asymmetric: we are not asked for the Lebesgue
decomposition of `τ` with respect to Lebesgue measure `m`; we are asked for the
Lebesgue decomposition of `m` with respect to `τ`. That means we must first
compute `τ`, then understand where `m` is absolutely continuous with respect to
it, and where `m` lives outside its support.

**WHY IT IS HARD**

The trap is that once one computes `τ`, it is very tempting to stop at
`dτ/dm = 2x 1_[0,1]`. But that only answers the easier opposite-direction
question. The actual derivative requested is `dm/dτ`, which only makes sense on
the region where `τ` sees mass.

The second difficulty is support-sensitivity. On `(0,1]`, the candidate density
for `τ` is positive, so `m` should be absolutely continuous there. Outside
`[0,1]`, the pushforward vanishes completely, so Lebesgue measure there must
become the singular part. The point `x = 0` is a harmless but conceptually easy
place to stumble: `τ({0}) = 0`, but also `m({0}) = 0`, so the derivative can be
defined however we like there without changing the answer almost everywhere.
