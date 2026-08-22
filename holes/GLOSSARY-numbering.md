# Numbering schemes in this workspace — a disambiguation

**Status:** reference. Written 2026-08-22 after Joe: *"it's confusing having
all of these layers, levels, and stages floating around."* It is.

This file does **not** renumber anything except one genuine same-file
collision (see the last section). It records what each scheme means and where
it is authoritative, so a bare number can be resolved.

## The schemes

| scheme | form | means | authority |
|---|---|---|---|
| **R-numbers** | R1–R18 | AIF mechanisms (belief update, EFE terms, precision…) | `ukrn-services-simulation/docs/aif-completeness.md`; the AIF control map |
| **WR-numbers** | WR-0–WR-27 | war-room rulings, as flexiargs | `futon3/library/war-room/*.flexiarg` |
| **Columns** | PERCEIVE, BELIEVE, EVALUATE, SELECT, ACT | **phases of one control loop**; sequential | AIF control map, Figure 2 of `p4ng/futon-2026.tex` |
| **Rows** | A–F | **domains**, not phases; each runs its own loop | `futon7/holes/M-futon-forward-model.backlog-cascade-merged-v0.edn` |
| **SPINE layers** | 1–7 | capability ledger → warrant → need shapes → occasions → bridge candidates → **valuation** → acceptance event. Vocabulary from the serendipity model | `futon0/analysis/business-models/SPINE.md` |
| **Thesis regions / star tiers** | T2, T3, T4, T5, T-inf | regions of the capability star map | `M-capability-star-map.md` |
| **Rungs** | per-star | steps of a star's ascent; flip to `:satisfied` | star map graph |
| **Waves** | wave 1, wave 2 | execution ordering within a cascade | merged cascade `:wave-order` |
| **Boxes / holes** | m1–m23, mh1–mh7 | missions and gaps in the *merged* cascade | merged cascade |
| **Parent boxes** | b1–b16, s1–s14 | the two parent cascades (general / stars) | `…backlog-cascade-v0.edn`, `…-stars-v0.edn` |
| **Tensions** | `:t1`–`:t3` | priced disagreements between the parents' wants | merged cascade `:tensions` |
| **G-numbers** | G1–G6 | *gaps* (renamed from P for problems, since patterns are generative) | `M-futon-problems.md` §1 |
| **Measures** | M1–M6 | pattern-uptake measures | `M-futon-problems.md` |
| **UKRNS axes** | D, A | delivery viability (x), architectural sustainability (y) | `ukrn-services-simulation` |
| **Label levels** | L0, L1, L3 | recognizer / label-contract levels | `M-marks-to-labels` (m2) |

## Live collisions — resolve by scope, not by renaming

- **`D`** is the worst. It means, in different files: a DERIVE item
  (`M-futon-problems` §2); an inherited discipline (same file — *fixed*, see
  below); a handoff item in `M-capability-zones-S3-handoff`; a source citation
  `[D1]`–`[D5]` in `analysis/business-models/cases/`; a handoff packet id in
  `p4ng/vetting/`; the FTS candidate surface `D1` (cascade m3); **delivery
  viability** (UKRNS axis); and **row D**, the operator loop. Eight readings.
  Always qualify: *DERIVE D3*, *row D*, *UKRNS D*.
- **`A`** is row A (certifying instrumentation) and the UKRNS
  architectural-sustainability axis.
- **`T`** is a thesis region (`T2`) and a tension (`:t1`). Case distinguishes
  them; do not rely on that in prose.
- **Bare small integers** are ambiguous between SPINE layers, waves,
  R-numbers, WR-numbers, G-numbers and measures. A bare "layer 6" or "wave 1"
  is only safe with its scheme named.

## Rows vs columns vs layers — the distinction worth holding

Three different *kinds* are all called "levels" in conversation:

- **columns** = phases of one loop (temporal; sequential);
- **rows** = domains (parallel; each runs the whole loop internally);
- **SPINE layers** = stages of value realisation (a capability becoming money).

Rows and SPINE layers share an axis but are not the same list — the mapping,
and the three rows that do not map, are worked out in
`p4ng/SPINE-vertical.md` §3.

## The one thing renamed

`M-futon-problems.md` used **D1–D5 for two different things in the same file**:
the inherited disciplines (§"Discipline") and the DERIVE items (§2). The
disciplines are now **DP1–DP5**. Cross-references updated (D8 cited
"D5 populated ≠ correct", now DP5). Nothing else was renumbered.
