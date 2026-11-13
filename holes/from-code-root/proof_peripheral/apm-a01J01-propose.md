**THE KEY INSIGHT**
- Hölder’s inequality with exponents `(3, 3/2)` turns the uniform `L^3` bound into `L^1` control on small sets: `∫_E |f_n| ≤ (∫_E |f_n|^3)^{1/3} m(E)^{2/3} ≤ m(E)^{2/3}`. Choosing `δ = ε^{3/2}` makes this `< ε` uniformly in `n`, proving the sequence is uniformly integrable.
- For the counterexample, concentrate each `f_n` on an interval of length `1/n` with height `n`. The `L^1` norms stay `1`, but each `f_n` carries its entire mass on a tiny set, so no single `δ` works.

**THE NAIVE APPROACH THAT FAILS**
- Students often try to use the `L^1` bound directly: `∫_E |f_n| ≤ ∫_0^1 |f_n| ≤ 1`, which ignores the size of `E`. Without the `p>1` control, nothing prevents spikes, so this approach cannot produce a `δ` that works for all `n`—mirroring the failure exhibited in part (b).
