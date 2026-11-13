# TN — Deep-research novelty check: Langton's λ = 1/2 as a fixed-point condition, for the meta-CA operator

**Date:** 2026-07-17
**Question:** Is the λ = 1/2 fixed-point result on the S₈ negation-permutation operator novel? What must it cite?
**Method:** deep-research harness (24 sources fetched, 108 claims extracted, 25 adversarially verified with 3-vote refutation) + independent exhaustive enumeration of all four claims.

---

## The object under review

An ECA rule is an 8-bit truth table `bit[k]`, k over the eight 3-cell neighbourhoods. Operator, parameterised by σ ∈ S₈ acting on the rule itself:

> `bit[σ(k)] := ¬ bit[k]`

40,320 operators. In the setting each lattice cell carries its own rule; the operator rewrites rules while rules evolve cell states.

**Claims checked:**
1. A Boolean fixed point exists **iff every cycle of σ is even**.
2. Number of such σ in S₂ₙ is `((2n−1)!!)²`; for S₈ that is `105² = 11,025` of `40,320` = 27.34%.
3. All-even σ with k cycles has exactly `2^k` fixed bytes; any-odd σ has none.
4. **Main claim:** every fixed point of every operator has exactly four 1-bits — **Langton's λ = 1/2, exactly.** 40,320 fixed points, zero violations.

**Independent verification (my own enumeration, `scratchpad/check.py`):** all four correct. Bonus identity that falls out and is *not* stated in the claims: Σ over all-even σ of `2^cycles = (2n)!` exactly (that is why the total fixed-point count is 40,320 = 8!). Both `((2n−1)!!)²` (weight 1) and `(2n)!` (weight 2^cycles) follow from the EGFs `1/√(1−x²)` and `1/(1−x²)` respectively — standard Flajolet–Sedgewick symbolic method.

---

## Verdict by tier

### Tier 1 — CITE, DON'T CLAIM (known results)

**Result 2 — the ((2n−1)!!)² count is classical.**
- **OEIS A001818** — "Squares of double factorials… Number of permutations in S₂ₙ in which all cycles have even length." Terms 1, 1, 9, 225, **11025**, …
- **Stanley, *Enumerative Combinatorics* Vol. 2 (1999), Problem 5.34(c)** — **EC2, not EC1.**
- **Riordan, *Combinatorial Identities* (1968), p. 217.**
- EGF `1/√(1−x²)` (the "set of even cycles" construction).

**Results 1 and 3 — Boolean-network / signed-graph textbook material.**
The operator `bit[σ(k)] := ¬bit[k]` *is* a Boolean network whose signed interaction digraph is σ with every arc negative.
- **"This is just X" paper:** **Aracena (2008), "Maximum Number of Fixed Points in Regulatory Boolean Networks," *Bull. Math. Biol.* 70(5):1398–1409.**
- Classical fact: an isolated **negative** cycle → **no** fixed point; an isolated **positive** cycle → **exactly two**. A negation-cycle of length ℓ has sign (−1)^ℓ, positive iff ℓ even → **this is Result 1**. Two-per-positive-cycle compounding to **2^k for k cycles → this is Result 3.**
- Equivalently: bipartiteness / 2-colourability of the cycle = Harary signed-graph balance.
- **Action:** frame Results 1 & 3 as an application of Robert/Aracena signed-cycle theory, not as new theorems.

### Tier 2 — THE CRUX (genuine novelty risk)

**No surviving source *derives* λ = 1/2** from an algebraic/fixed-point/operator condition. Every treatment found obtains it statistically:
- Langton (1990) — measured.
- **Sakai–Kanno (2002), arXiv:nlin/0204045** — competing NON-algebraic account: λ ≈ 0.5 is special because binomial sampling variance λ(1−λ) is maximal there. **Must cite and distinguish — this is the standard "why 0.5" story and it is not ours.**
- 2020 *Natural Computing* survey (arXiv:1607.02291) — λ defined as a pure counting statistic.

**⚠️ STRONGEST ADVERSARIAL HIT — read before writing related work:**
**Chan-López & Martín-Ruiz, arXiv:2604.00165 (2026), Proposition 2** runs the *same argument shape* — an alternation constraint over 𝔽₂ forcing balanced bits — whose survivors are **Rules 150 (10010110) and 105 (01101001), both Hamming-weight exactly 4, i.e. λ = 1/2 exactly** — without naming λ or noticing the balance.
- Verification agents voted this DOWN 0-3 (different apparatus: Hamming-weight orbit path vs σ-cycles; paper never states λ). **The harness flagged it as the strongest signal over its own vote — do NOT take comfort in the 0-3.** "They did the same move without noticing the consequence" is exactly the rediscovery a referee asserts.
- **Honest framing:** λ = 1/2 is forced by the complementation `¬` — any alternation-under-complement construction lands there by parity. Present it as an elementary/expected consequence of the construction, NOT a surprising theorem about rule space.

**CAVEAT — verify the citation exists:** arXiv:2604.00165 is future-dated (April 2026) and is the single load-bearing citation against novelty. Content-claims were vote-verified but existence was not independently confirmed. **Pull up Proposition 2 and read it before relying on it.**

### Tier 3 — THE OBJECT (the real contribution)

**Genuinely unanticipated.** Nothing found defines an operator `bit[σ(k)] := ¬bit[k]` on rule space.
- Canonical ECA rule-space symmetry group has **order 4** (reflection × complement, 256 → 88 classes), **not S₈** — **Svozil & Schaller (2025), arXiv:2512.08117.**
- Truth-table permutation appears only as a one-off illustration — **PICARD (Pavlic et al. 2014), arXiv:1405.4070, Fig. 4** swaps two bits — never a parameterised, enumerated, negation-carrying family with derived fixed-point structure.

**Cannot claim the substrate — cite and distinguish two genres:**
- **Per-cell rules = non-uniform / hybrid / ν-CA:** Pries et al. (1986), Hortensius et al. (1989), Chaudhuri et al. (1997), Dennunzio–Formenti–Provillard (2012, arXiv:1107.5228).
- **Rules changing during evolution = rule-changing / programmable / self-modifying CA:** Nandi et al. (1994), *"Edge of chaos in rule-changing cellular automata"* (Physica D 116:275, 1998), PICARD (2014).

**Adversarial trap on the substrate:** Dennunzio et al. prove a ν-CA drawing rules from a *finite* set (rν-CA — exactly the field over the 256 ECAs) is "just" a uniform CA on the product alphabet (states × rules). **A referee can collapse the field construction → locate ALL novelty in the S₈ operator's fixed-point structure, not in the field.**

---

## λ's current standing (for related work)

**Not discredited — demoted.** "Our attractors sit at Langton's critical value" reads as engaging a still-cited but non-sufficient order parameter.
- **MHC (1993) refuted *Packard's* GA-clustering claim specifically**, not λ's definition. λ survived as one axis among several.
- Successors to cite: **Wuensche's Z-parameter** (SFI WP 1994; *Complexity* 4(3):47–66, 1999) and **Voorhees's Θ** (1997). The 2020 survey lists λ, Z, Θ on equal footing.
- **Citation-hygiene traps:**
  - MHC 1993 *Complex Systems* 7:89 (arXiv:adap-org/9303003) is **distinct** from the 1994 SFI re-examination (arXiv:adap-org/9306003) — don't conflate. Cite MHC's own text; the two second-hand claims about *what exactly* MHC refuted were voted down 1-2 and 0-3.
  - Stanley **EC2**, not EC1.

---

## Open — do these yourself (unresolved by the search)

1. **Is 2^k-for-k-cycles textbook via Harary balance?** Q4/Q6 got no direct confirmation on the signed-graph framing. Given Tier 1, assume yes and pre-empt. Check Harary balance / conjunctive-disjunctive Boolean networks / sequential dynamical systems directly.
2. **Does λ = 1/2 arise trivially in ANY complementation-symmetric construction** (self-complementary rules 150/105/51/204, additive/reversible CA)? Not resolved — this is the whole ballgame for the λ claim. Targeted search of 𝔽₂/algebraic-ECA and self-complementary-rule literature needed.
3. **Propp rotor-routing / Cook Rule 110** (Q6): no evidence either way; genuinely open, low priority.

---

## Bottom line

The paper's weight must rest entirely on the **object** — the S₈ negation-permutation operator on rule space, presented as a new construction. Results 1–3 are known combinatorics / Boolean-network facts to cite cleanly. The λ = 1/2 result should be framed as a **forced, elementary consequence of the operator's complementation symmetry** (arXiv:2604.00165 is the near-miss that makes any stronger claim dangerous), explicitly reconciled with the post-MHC "λ is demoted, not dead" story. Framed that way it is honest and defensible; framed as a surprising discovery, expect a referee to produce the alternation-forces-balance folklore.

---

## Artifacts
- Full research report + per-claim vote tallies: `tasks/wuw37gf71.output`
- Independent enumeration script (verifies all four claims): `scratchpad/check.py`

## Key sources
- OEIS A001818 — https://oeis.org/A001818
- Aracena (2008) *Bull. Math. Biol.* 70(5):1398–1409 — https://link.springer.com/article/10.1007/s11538-008-9304-7
- Sakai–Kanno (2002) — https://arxiv.org/pdf/nlin/0204045
- Natural Computing survey (2020) — https://arxiv.org/pdf/1607.02291
- Chan-López & Martín-Ruiz (2026) [VERIFY EXISTS] — https://arxiv.org/pdf/2604.00165
- Svozil & Schaller (2025) — https://arxiv.org/abs/2512.08117
- PICARD (2014) — https://arxiv.org/abs/1405.4070
- Dennunzio et al. (2012, ν-CA) — https://arxiv.org/pdf/1107.5228
- MHC (1993) Complex Systems 7:89 — https://arxiv.org/abs/adap-org/9303003
- MHC (1994) SFI re-examination — https://arxiv.org/abs/adap-org/9306003
