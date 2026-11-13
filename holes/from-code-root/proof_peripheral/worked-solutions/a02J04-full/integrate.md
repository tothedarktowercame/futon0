### INTEGRATE

**Connections**

This problem is a model example of three recurring themes.

First, pushforwards under projections are fiber integrals. In more advanced
settings this becomes the coarea formula, disintegration of measures, and
conditional expectation.

Second, support matters in Radon–Nikodym problems. Even when one measure has a
nice density with respect to Lebesgue measure, reversing the direction forces
you to split off the outside-support region as singular.

Third, this is a simple finite-dimensional instance of a common probabilistic
move: start with a joint law on `(x,y)`, project to one coordinate, and compute
the marginal density by integrating over the forgotten variable.

**EXAM-DAY FIELD KIT**

1. To compute a pushforward under a projection, integrate the original density
   over the fibers.
2. A triangular region `0 ≤ y ≤ x ≤ 1` gives vertical slice length `x`.
3. If `τ = f m`, then reversing direction gives `dm/dτ = 1/f` only on the
   region where `f > 0` almost everywhere.
4. Outside the support of `τ`, any remaining Lebesgue mass must become the
   singular part.
5. Values of the Radon–Nikodym derivative on measure-zero sets are irrelevant.

**ArSE Questions**

1. *Why is this hard?*
   **Q:** Why isn’t it enough to stop after computing `dτ/dm = 2x 1_[0,1]`?
   **A:** Because the problem asks for the decomposition of `m` with respect to `τ`, which is the opposite direction.

2. *What is the key insight?*
   **Q:** What single move unlocks the whole problem?
   **A:** Integrate over the forgotten `y`-coordinate first; once the pushforward density is explicit, the rest is a support split.

3. *Why does step N work?*
   **Q:** Why does the singular part have to be `m|_{(-∞,0) ∪ (1,∞)}`?
   **A:** Because `τ` vanishes outside `[0,1]`, so Lebesgue mass there cannot be absolutely continuous with respect to `τ`.

4. *What connects to this?*
   **Q:** Where else does this method appear?
   **A:** In marginals of joint distributions, conditional expectation, disintegration, and coarea-style formulas.

5. *Where is intuition wrong?*
   **Q:** What false instinct should we avoid?
   **A:** Thinking that once one density is known, the reverse derivative is its reciprocal everywhere rather than only on the positive-density support.
