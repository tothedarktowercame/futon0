# E-aif-daisyworld — a controllable microcosm of the futon stack (FutonZero testbed)

**Date:** 2026-06-23
**Status:** CHARTER + log. Phases 1–3a done; 3b (REINFORCE) = honest negative; **3b′ (EVOLUTION) = positive** —
learns, beats the hand heuristic ~72% on held-out coupled homeostasis, reproducible. "Can we learn policies?" → **yes**, by pure EC.
**Owner:** claude-2 (build) + Joe (direction).
**Home:** `futon0/scripts/futon0/futonzero/daisyworld*.clj`; viz in `futon0/data/daisyworld*.html`.
**Why it exists:** FutonZero v1 isn't yet AlphaZero — it lacks R2 (experience trains the prior), an adversary,
and an exogenous reward (the real stack has no simulator + self-grades `C` = "Goodhart's door";
`futon2/docs/futonzero-alphazero.md`). Daisyworld gives a **clean simulable board (G-SIM)** with a **grounded
reward (G-REWARD)**; coupling gives an **adversary**; so it's where the closed loop can actually be built and the
open FutonZero question tested (cf. `futon6/holes/excursions/E-learning-as-we-go-vs-futonzero.md`). Ties:
[[project_futonzero_generative_steering]], M-G-over-cascades (κ/L morphogenesis), Ostrom IAD (heat-commons).

## Log

- **Phase 1 — simulator (DONE).** `daisyworld.clj`: faithful Clojure port of the Agents.jl Daisyworld
  (local-heating `72·ln(absorbed)+80`, diffusion 0.5, seed-prob parabola peaking ~22.8°, death@25). Validated:
  homeostasis under `:default`; rebalance-then-collapse under `:ramp`. clj-kondo 0/0. = the **R4 forward-model**.
- **Phase 2 — G(π) player (DONE).** `daisyworld_player.clj`: selects *intervention policies* by rollout (R1
  prior, R4 forward-model, R5 EFE = |T−T*|+spread, R6 softmax-abstain; G(π)=discounted rollout, **not argmax**).
  Under `:change` forcing the player holds homeostasis far better — **reg-err 5.96 vs autonomous 14.78**. Viz
  (autonomous vs steered). Honest: R2 hooked-not-closed; shallow rollout; 14×14 toy grid.
- **Phase 3a — coupled worlds, local 3×3 export (DONE).** `daisyworld_coupled.clj`: two worlds, own suns, each
  reflects heat onto the other's **3×3 shadow patch** (Ostrom-local commons; backward-compatible `step` refactor
  adds an import field + records `reflected`). Validated: coupling is live + an **externality** — World B's
  reg-err worsens **6.88 (solo) → 10.17 (coupled to A)**. This makes the reward **exogenous** (B's homeostasis
  depends on A → can't self-grade). Two-world viz.
- **Phase 3b — self-play policy learning (HONEST NEGATIVE).** `daisyworld_learn.clj`: two worlds share one
  policy θ (linear-softmax over situation-features), trained by REINFORCE from realized homeostasis (coupled
  self-play). **Result: it does NOT learn** — across two principled variants (game-level reward, then per-step
  return-to-go), the trained policy is *worse* than untrained and the hand heuristic (trained ≈19 vs frozen ≈15
  vs heuristic ≈12), collapsing to a degenerate always-`:warm`/never-abstain corner. **The signal exists** (the
  heuristic beats random by ~25%), so a good policy is learnable in principle; naive policy-gradient just doesn't
  find it. A clean recordable negative in the FutonZero spirit (cf. arguing-worlds "ceremony"). **Diagnosis
  (Joe):** the deeper cause is **no substrate to learn over** — 3 crude actions (cool/warm/none), no
  pattern-language, no place to record learning. That motivates the microcosm.

- **Phase 3b′ — LEARN by EVOLUTION (POSITIVE, verified).** `daisyworld_evolve.clj`: Joe's reduction of "pattern"
  to its computational/game-theoretic core — **IF** = own 3×3 (local temp + density), **HOVER** = the shadow
  world's 3×3, **THEN** = response. A *pattern* becomes a local condition→action rule; a **genome** is the lookup
  table over (own-temp × own-density × shadow-temp) → {white,black,none} (27 entries). **No agent-in-the-loop** —
  patterns are evolvable data. Pure evolutionary computing: self-play coupling = adversary, homeostasis = grounded
  fitness (exogenous), mutation+selection = R2. **Result: it learns** — curve improves monotonically
  (5.91→3.05), and the evolved genome scores **3.62 reg-err on held-out (unseen-seed) coupled games vs hand
  heuristic 12.80 and random 43.62** — beats the heuristic by ~72%, **reproducibly**. Honest correction: the
  first run (7.00) didn't reproduce — caught a real bug (`rand-nth` used the global RNG) + single-seed
  overfitting; fixed (seeded GA + multi-seed training + held-out eval), now deterministic + robust. **So
  evolution succeeds where REINFORCE (3b) collapsed** — it was the method, and the FutonZero closed loop here is
  *not* ceremony (it beats the baseline on the grounded yardstick).

- **Hypothesis test — "does a Pattern world *program/steward* its Classic neighbour to stay alive longer?"
  (Joe). NOT SUPPORTED (toy scale).** In the collapse regime (hot start, both `:change`), the Classic neighbour
  is *slightly less* alive next to a Pattern world (mean-cover 0.501 vs 0.523; collapse-tick 26 vs 27) — mild
  exploitation, not stewardship. **Interpretation (the payoff):** our coupling is a pure **export externality**
  (staying cool = dumping reflected heat on the neighbour), so self-interest is *structurally anti-stewardship* =
  tragedy of the commons by construction (Ostrom). Stewardship would need a **mutualistic coupling** (the
  neighbour's health feeds back as a benefit). So the hypothesis is really a prediction about *coupling design*,
  and the toy faithfully reproduces the tragedy default. (Honest: small effect, 4 seeds — "no evidence of
  stewardship," not a strong refutation.) Not pursued deeper — it's a toy.

## Next phase — the MICROCOSM (simplified by 3b′)

Give the daisy the FutonZero ingredients so learning has structure + a home:
1. **Daisy pattern library** (`.flexiarg`) — the move vocabulary (structured multi-cell interventions:
   seed-white-cluster, firebreak-belt, cool-the-export-border, checkerboard-albedo, let-it-ride, …), each
   IF/HOWEVER/THEN. Patterns-as-moves → stronger action signal (fixes 3b's weak-action problem) + a vocabulary
   the prior learns over (fixes the collapse).
2. **Micro substrate-2** — a small EDN hypergraph: world-state + patterns + applied cascades + outcomes, so κ/L/C
   are queryable and the prior = the pattern co-application graph. R2 has a place to record learning.
3. **FutonZero loop** — cascade-rollout over patterns (real `G(π)`, slice-1/2 machinery), homeostasis = grounded
   reward, R2 trains the pattern-prior, recorded in the micro-substrate.

Result: a controllable **futon stack in miniature** where the closed-loop-learning question can be settled with
the full apparatus, and κ/L · cascade-rollout · coupled-exogenous-reward · meme-arrows all plug in.

## Falsifiable test (carried)

Does the closed loop (R2-trained pattern-prior over the microcosm) beat a frozen prior + the hand heuristic on
*held-out coupled homeostasis* (the grounded yardstick)? If not, record it honestly — as 3b already did.
