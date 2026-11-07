#!/usr/bin/env python3
"""Certify System E engine step sequences as LeanEuclid proofs.

Given a found step sequence in the engine's (rule, args, bindings) format,
emit a LeanEuclid proof (theorem statement copied verbatim from
~/code/LeanEuclid/Book/PropNN.lean, `euclid_intros`, one `euclid_apply ...`
per step, `euclid_assert <prop>` steps for SMT-justified facts (I.4),
`use <witness>`, `euclid_finish`), check it with
`lake env lean` from ~/code/LeanEuclid, and report certified / not certified
with the reason and wall time.

Certification rule: certified iff `lake env lean` exits 0 AND the output
contains neither an error ("error:") nor the sorry warning
("declaration uses 'sorry'"). Both halves matter:
  - solver answers SAT on a false goal  -> "error: failed", exit 1;
  - solver answers unknown (timeout)     -> "error: Could not prove", exit 1;
  - a `sorry` left anywhere in the script (the failure mode the review
    flagged: euclid_finish's `admitGoal` on solver-unknown) can exit 0
    with only the warning, and only the sorry check catches it.
Every SMT success in LeanEuclid goes through the axiom SMT_VERIF; that axiom
in the dependency list is expected and is not itself a defect of the proof.

LeanEuclid is used read-only except the scratch directory tmp-certify/ under
it, which this tool creates and the caller deletes; nothing there is ever
committed. PATH must include LeanEuclid/venv/bin and ~/.local/bin (z3/cvc5),
LD_LIBRARY_PATH the elan Lean shared-library dir — env() sets both up.

Usage (from ~/code/futon0):
  python3 scripts/euclid_certify.py book        # Book replay-data controls
  python3 scripts/euclid_certify.py found       # search-found sequences
  python3 scripts/euclid_certify.py negatives   # negative controls
"""

import importlib.util
import os
import signal
import subprocess
import time
from pathlib import Path

FUTON0 = Path(__file__).resolve().parents[1]
LEAN_EUCLID = Path("~/code/LeanEuclid").expanduser()
SCRATCH = LEAN_EUCLID / "tmp-certify"

LEAN_NAME = {  # engine identifier -> Lean identifier (unicode hypothesis names)
    "c0": "c₀",
    "c1": "c₁",
}


def lean(x):
    return LEAN_NAME.get(x, x)


# ---------------------------------------------------------------------------
# Theorems: statements copied verbatim from ~/code/LeanEuclid/Book/PropNN.lean
# ---------------------------------------------------------------------------

PROP1_STATEMENT = """\
theorem proposition_1 : ∀ (a b : Point) (AB : Line),
  distinctPointsOnLine a b AB →
  ∃ c : Point, |(c─a)| = |(a─b)| ∧ |(c─b)| = |(a─b)| :="""

PROP2_STATEMENT = """\
theorem proposition_2 : ∀ (a b c : Point) (BC : Line),
  (distinctPointsOnLine b c BC) ∧ (a ≠ b) →
  ∃ l : Point, |(a─l)| = |(b─c)| :="""

PROP3_STATEMENT = """\
theorem proposition_3 : ∀ (a b c₀ c₁ : Point) (AB C : Line),
  distinctPointsOnLine a b AB ∧ distinctPointsOnLine c₀ c₁ C ∧ |(a─b)| > |(c₀─c₁)| →
  ∃ e : Point, between a e b ∧ |(a─e)| = |(c₀─c₁)| :="""

# Book proof step sequences (the euclid_apply lines of Book/PropNN.lean, in
# the engine's (rule, args, bindings) format; forward rules carry no
# bindings). These are the replay-data control on the emitter.
PROP1_BOOK_STEPS = [
    ("circle_from_points", ("a", "b"), {"C": "BCD"}),
    ("circle_from_points", ("b", "a"), {"C": "ACE"}),
    ("intersection_circles", ("BCD", "ACE"), {"c": "c"}),
    ("point_on_circle_onlyif", ("a", "b", "c", "BCD"), {}),
    ("point_on_circle_onlyif", ("b", "a", "c", "ACE"), {}),
]
PROP2_BOOK_STEPS = [
    ("line_from_points", ("a", "b"), {"L": "AB"}),
    ("proposition_1", ("a", "b", "AB"), {"d": "d"}),
    ("line_from_points", ("d", "a"), {"L": "AE"}),
    ("line_from_points", ("d", "b"), {"L": "BF"}),
    ("circle_from_points", ("b", "c"), {"C": "CGH"}),
    ("intersection_circle_line_extending_points", ("CGH", "BF", "b", "d"),
     {"g": "g"}),
    ("circle_from_points", ("d", "g"), {"C": "GKL"}),
    ("intersection_circle_line_extending_points", ("GKL", "AE", "a", "d"),
     {"g": "l"}),
    ("point_on_circle_onlyif", ("b", "c", "g", "CGH"), {}),
    ("point_on_circle_onlyif", ("d", "l", "g", "GKL"), {}),
    ("between_if", ("l", "a", "d"), {}),
    ("between_if", ("g", "b", "d"), {}),
]
PROP3_BOOK_STEPS = [
    ("proposition_2'", ("a", "c0", "c1", "C"), {"d": "d"}),
    ("circle_from_points", ("a", "d"), {"C": "DEF"}),
    ("intersection_circle_line_between_points", ("DEF", "AB", "a", "b"),
     {"e": "e"}),
    ("point_on_circle_onlyif", ("a", "d", "e"), {}),
]

# Assert steps: ("euclid_assert", ("<proposition text>",), {}) — a single
# string emitted verbatim after `euclid_assert ` (the Lean tactic that
# delegates a step's justification to SMT). Not an engine rule; used for the
# Book I.4 proof, whose justifications the engine holds as [SMT-bridge]
# stand-ins (packet P-euclid-certify-I4-asserts).
#
# Superposition's Book proof names its four fresh objects in one grouped
# binding `as (b', c', BC', DC')`; the emitter emits grouped bindings
# (parenthesised, comma-separated) for rules in GROUPED_BINDINGS_RULES.
GROUPED_BINDINGS_RULES = {"superposition", "intersections_circle_line"}

PROP4_STATEMENT = """\
theorem proposition_4 : ∀ (a b c d e f : Point) (AB BC AC DE EF DF : Line),
  formTriangle a b c AB BC AC ∧ formTriangle d e f DE EF DF ∧
  |(a─b)| = |(d─e)| ∧ |(a─c)| = |(d─f)| ∧ (∠ b:a:c = ∠ e:d:f) →
  |(b─c)| = |(e─f)| ∧ (∠ a:b:c = ∠ d:e:f) ∧ (∠ a:c:b = ∠ d:f:e) :="""

# The Book's own I.4 proof (Book/Prop04.lean): superposition names its fourth
# object DC' (the engine's PROP4_STEPS_DATA names it AC'); the four asserts
# are the SMT justifications the Book itself makes. Not an engine replay —
# reconciling the engine's two assert_* bridge steps with these four is a
# later packet.
PROP4_BOOK_STEPS = [
    ("superposition", ("a", "b", "c", "d", "e", "f", "AB", "BC", "AC", "DE"),
     {"b'": "b'", "c'": "c'", "BC'": "BC'", "DC'": "DC'"}),
    ("euclid_assert", ("b' = e",), {}),
    ("euclid_assert", ("∠ e:d:c' = ∠ e:d:f",), {}),
    ("euclid_assert", ("DC' = DF",), {}),
    ("euclid_assert", ("c' = f",), {}),
]

PROP5_STATEMENT = """\
theorem proposition_5 : ∀ (a b c d e : Point) (AB BC AC : Line),
  formTriangle a b c AB BC AC ∧ (|(a─b)| = |(a─c)|) ∧
  (between a b d) ∧ (between a c e) →
  (∠ a:b:c = ∠ a:c:b) ∧ (∠ c:b:d = ∠ b:c:e) :="""

# Book/Prop11.lean verbatim (proposition_11, not 11'/11''): erect a
# perpendicular to AB at the interior point c.
PROP11_STATEMENT = """\
theorem proposition_11 : ∀ (a b c : Point) (AB : Line),
  distinctPointsOnLine a b AB ∧ between a c b →
  exists f : Point, ¬(f.onLine AB) ∧ (∠ a:c:f = ∟) :="""

# The engine's own I.11 steps (euclid_engine.PROP11_STEPS_DATA, read live).
# between_symm_parts is the engine's citation of the NON-REVERSAL parts of
# between_symm (Diagrammatic.lean:45-46: between a b c -> between c b a ∧
# a ≠ b ∧ a ≠ c ∧ ¬(between b a c)). It is NOT a SystemE name, so it emits
# as `euclid_apply (between_symm <args>)` — the whole axiom, reversal
# conclusion included (harmless extra facts in context), via
# ENGINE_RULE_TO_LEAN. Chosen over euclid_assert of just the neq parts for
# the same reason as I.12's axiom steps: assert would delegate the fact to
# SMT without recording which axiom justified it (E-kimi-task-376 decision).
# The engine renames Lean's a b c d to u v w g (substitution collisions,
# see the comment above PROP11_STEPS_DATA); PROP11_ENGINE_RENAMES maps them
# back at emit time, after translation.
PROP11_ENGINE_CITABLE = [
    "between_symm", "point_between_points_shorter_than", "proposition_3",
    "proposition_1", "line_from_points", "proposition_8",
    "perpendicular_if",
]
PROP11_ENGINE_RENAMES = {"u": "a", "v": "b", "w": "c", "g": "d"}
# Names bound by the I.11 statement (not created by any step): h-sm facts
# that name only these are assertable from the start.
PROP11_GIVEN = {"a", "b", "c", "AB"}

# Book/Prop12.lean verbatim (disjunctive goal: one of the two angles at the
# foot h is right).
PROP12_STATEMENT = """\
theorem proposition_12 : ∀ (a b c : Point) (AB : Line),
  distinctPointsOnLine a b AB ∧ ¬(c.onLine AB) →
  exists h : Point, h.onLine AB ∧ (∠ a:h:c = ∟ ∨ ∠ b:h:c = ∟) :="""

# The engine's own I.12 steps (euclid_engine.PROP12_STEPS_DATA, read live).
# Every rule in PROP12_STEPS_DATA is Lean-citable; the two engine-only steps
# are SystemE axioms:
#   intersection_circle_line_1 (Diagrammatic.lean:297) and
#   perpendicular_if           (Transfer.lean:102)
# Both emit as `euclid_apply (<axiom> ...)` — euclid_apply accepts an axiom
# (Book precedent: Prop11.lean:20,37 applies perpendicular_if itself), and
# each axiom's conclusion (L.intersectsCircle α resp. ∠a:c:d = ∟) is the
# fact downstream steps/euclid_finish consume; euclid_assert would delegate
# the same fact to SMT without recording which axiom justified it.
# Engine arg orders match the Lean signatures:
#   intersection_circle_line_1 (a b α L) <- engine (d c EFG AB)
#     [d on EFG, c centre-inside, opposingSides d c AB]
#   perpendicular_if (a b c d L) <- engine (e g h c AB)
#     [between e h g, c off AB, ∠e:h:c = ∠c:h:g]
#   proposition_10 (a b AB)    <- engine (e g AB), witness h
PROP12_ENGINE_CITABLE = [
    "exists_point_opposite", "circle_from_points",
    "intersection_circle_line_1", "intersections_circle_line",
    "proposition_10", "line_from_points", "proposition_8",
    "perpendicular_if",
]


# Copied verbatim from euclid_engine.PROP5_STEPS_DATA (zai-11 owns the engine
# file and may have it mid-edit; the copy is pinned and test-asserted equal to
# the engine's data whenever the engine file parses). The final rule takes a
# Segment argument: engine args ("c", "e") render as the Lean segment (c─e).
PROP5_BOOK_STEPS = [
    ("point_between_points_shorter_than", ("AB", "b", "d", "c", "e"),
     {"a": "f"}),
    ("proposition_3", ("a", "e", "f", "a", "AC", "AB"), {"q": "g"}),
    ("line_from_points", ("c", "f"), {"L": "FC"}),
    ("line_from_points", ("b", "g"), {"L": "GB"}),
    ("proposition_4", ("a", "f", "c", "a", "g", "b", "AB", "FC", "AC",
                       "AC", "GB", "AB"), {}),
    ("proposition_4", ("f", "b", "c", "g", "c", "b", "AB", "BC", "FC",
                       "AC", "BC", "GB"), {}),
    ("sum_angles_onlyif", ("b", "a", "g", "c", "AB", "GB"), {}),
    ("sum_angles_onlyif", ("c", "a", "f", "b", "AC", "FC"), {}),
]

# Rules whose Lean signature ends in a Segment built from the engine's last
# two (Point) args (SystemE Theory/Constructions/Points.lean:28 for
# point_between_points_shorter_than, :43 for extend_point_longer — its
# (L b c (s : Segment)) shape is why the I.16b engine data carries the
# (b─e) segment as its two endpoints).
SEGMENT_TAIL_RULES = {"point_between_points_shorter_than",
                      "extend_point_longer"}

# Book/Prop13.lean verbatim: angles on a straight line sum to two rights.
PROP13_STATEMENT = """\
theorem proposition_13 : ∀ (a b c d : Point) (AB CD : Line),
  AB ≠ CD ∧ distinctPointsOnLine a b AB ∧ distinctPointsOnLine c d CD ∧ between d b c →
  ∠ c:b:a + ∠ a:b:d = ∟ + ∟ :="""

# The engine's own I.13 steps (euclid_engine.PROP13_STEPS_DATA, read live):
# a NESTED by_cases tree — Lean's outer split on ∠c:b:a = ∠a:b:d, then (in
# the ≠ branch) proposition_11' + line_from_points and an engine-only inner
# split on a's side of BE. Emitted as nested `by_cases <prop>` blocks (see
# emit_proof13). Engine-only rules:
#   pasch_3_opposing (d b c BE) -> euclid_apply (pasch_3 d b c BE): the
#     SystemE axiom itself (Diagrammatic.lean:163), conclusion
#     ¬(d.sameSide c BE). ENGINE_RULE_TO_LEAN.
#   opposing_same_side_trans (d c a BE) is DERIVED (from same_side_trans,
#     Diagrammatic.lean:127 area), not a SystemE name, so it emits as
#     `euclid_assert ¬(d.sameSide a BE)` — the conclusion its consumer
#     (triple_incidence_2's ¬(b.sameSide d M) premise) needs; SMT proves it
#     from pasch_3's ¬(d.sameSide c BE) and the branch's a.sameSide c BE.
#     BRIDGE_ASSERTS. Chosen over euclid_apply of same_side_trans because no
#     single SystemE axiom has this conclusion shape.
#   between_symm_parts -> euclid_apply of between_symm (as in I.11); the
#     mirror branch's engine order (between_symm c b d first) still
#     certifies: euclid_apply's premise discharge derives between c b d
#     from the hypothesis between d b c itself.
# The mirror branch's case assumption (same_side a d BE) is not free in
# Lean: the inner by_cases negative branch gives only ¬(a.sameSide c BE),
# so it is emitted as a CHECKED `euclid_assert a.sameSide d BE` (trichotomy
# is SMT-proved from the context).
# Every branch ends with euclid_finish (LA closes ∠cba + ∠abd = 2∟ from the
# sum_angles_onlyif rows and the right angle); a branch whose last step is a
# nested by_cases needs no finish of its own (the inner branches close it).
# NOTE: branch 1's perpendicular_if is NOT load-bearing for Lean —
# euclid_finish closes that branch alone (Book/Prop13.lean:12 does exactly
# that) — so "drop perpendicular_if" is not a viable bad case; the negative
# test asserts a false case fact instead.
PROP13_ENGINE_CITABLE = [
    "perpendicular_if", "proposition_11'", "line_from_points",
    "between_symm", "pasch_3", "triple_incidence_2", "sum_angles_onlyif",
    "perpendicular_onlyif",
]

# Book/Prop14.lean verbatim: if two straight lines meeting at b make the
# adjacent angles equal to two right angles with a common arm, the two
# 'outer' lines BC, BD are one straight line (conclusion BC = BD).
PROP14_STATEMENT = """\
theorem proposition_14 : ∀ (a b c d : Point) (AB BC BD : Line),
  distinctPointsOnLine a b AB ∧ distinctPointsOnLine b c BC ∧ distinctPointsOnLine b d BD ∧ (c.opposingSides d AB) ∧
  (∠ a:b:c + ∠ a:b:d) = ∟ + ∟ →
  BC = BD :="""

# The engine's own I.14 steps (euclid_engine.PROP14_STEPS_DATA, read live):
# an outer by_contra (assume neq_line BC BD — Lean's `by_contra` supplies
# exactly ¬(BC = BD)) containing a NESTED engine by_contra over
# same_side/opposing_sides a e BD, emitted as Lean's `by_cases a.sameSide e
# BD` (both engine branches prove False; (P → ⊥) ∧ (¬P → ⊥) ⊢ ⊥ is the
# split). See emit_proof14. Engine-only rules:
#   between_symm_parts (c b e), (e b c) -> euclid_apply (between_symm ...):
#     the whole axiom, neq parts included (same choice as I.11/I.13).
#   pasch_3_opposing (c b e AB) -> euclid_apply (pasch_3 c b e AB): the
#     SystemE axiom itself, conclusion ¬(c.sameSide e AB) — ENGINE_RULE_TO_LEAN.
#   opposing_opposing_same_side (d c e AB) is DERIVED -> CHECKED
#     euclid_assert d.sameSide e AB — BRIDGE_ASSERTS (see there).
#   triple_incidence_2 (AB BD BC b a d e) as is: SystemE axiom
#     (Diagrammatic.lean:199). Its ¬(b.sameSide d M) premise is exactly
#     what Lean's negative by_cases branch supplies (¬(a.sameSide e BD)),
#     so branch 2's engine fact (opposing_sides a e BD) needs NO assert —
#     the packet's "extra the engine used" is the opposing_opposing_same_side
#     assert above. euclid_apply's premise discharge SMT-derives the
#     remaining premises (¬(e.onLine BD) from BC ≠ BD, b ≠ a from the dpl).
# Branch 1 (a.sameSide e BD, free from the by_cases): sum_angles_onlyif
#   b a d e AB BD; the arithmetic (with proposition_13's row and the
#   hypothesis) forces ∠e:b:d = 0 against angle_pos — euclid_finish.
# Branch 2: triple_incidence_2 then sum_angles_onlyif b a e d AB BC;
#   euclid_finish. Both branches end with euclid_finish (no trailing
#   finish after the by_cases: the branches close the False goal that
#   by_contra installed).
PROP14_ENGINE_CITABLE = [
    "extend_point", "between_symm", "pasch_3", "proposition_13",
    "sum_angles_onlyif", "triple_incidence_2",
]

PROP15_ENGINE_CITABLE = [
    "between_symm", "proposition_13",
]

# Book/Prop15.lean verbatim: at the crossing of AB and CD (e between a, b
# and between d, c), the vertical angles are equal — both conjuncts.
PROP15_STATEMENT = """\
theorem proposition_15 : ∀ (a b c d e : Point) (AB CD : Line),
  distinctPointsOnLine a b AB ∧ distinctPointsOnLine c d CD ∧ e.onLine AB ∧ e.onLine CD ∧
  CD ≠ AB ∧ (between d e c) ∧ (between a e b) →
  (∠ a:e:c = ∠ d:e:b) ∧ (∠ c:e:b = ∠ a:e:d) :="""

# Book/Prop16.lean: the exterior angle exceeds each interior opposite angle
# — BOTH conjuncts. The ENGINE proves only the second (euclid_engine
# PROP16B_STEPS_DATA), so the I.16b-engine theorem below carries the SECOND
# conjunct alone as its statement, renamed proposition_16b; Prop16.lean is
# not changed. Statement hypotheses verbatim from Book/Prop16.lean:9-11.
PROP16_STATEMENT = """\
theorem proposition_16 : ∀ (a b c d : Point) (AB BC AC: Line),
  formTriangle a b c AB BC AC ∧ (between b c d) →
  (∠ a:c:d > ∠ c:b:a) ∧ (∠ a:c:d > ∠ b:a:c) :="""

PROP16B_STATEMENT = """\
theorem proposition_16b : ∀ (a b c d : Point) (AB BC AC: Line),
  formTriangle a b c AB BC AC ∧ (between b c d) →
  ∠ a:c:d > ∠ b:a:c :="""

# The engine's own I.16b steps (euclid_engine.PROP16B_STEPS_DATA, read
# live): flat — proposition_10 bisects AC at e, line BE, extend_point_longer
# past e to f' with |e-f'| > |b-e| (the (b─e) segment as a SEGMENT_TAIL —
# see SEGMENT_TAIL_RULES), proposition_3 cuts |b-e| off e-f' giving f,
# line FC, vertical angles at e (proposition_15), SAS (proposition_4) for
# ∠e:a:b = ∠e:c:f, and sum_angles_onlyif c a d f AC BC splits
# ∠a:c:d = ∠a:c:f + ∠f:c:d with ∠f:c:d > 0 — euclid_finish's LA closes the
# strict inequality. Every engine apply's args match Lean's second-conjunct
# bullet (Book/Prop16.lean:26-32) EXACTLY (checked 2026-11-06):
#   proposition_10 a c AC            == Prop16.lean:26
#   line_from_points b e as BE       == :27
#   extend_point_longer BE b e (b─e) == :28  (engine carries (b─e) as "b","e")
#   proposition_3 e f' b e BE BE     == :29
#   proposition_15 a c b f e AC BE   == :31
#   proposition_4 e b a e f c BE AB AC BE FC AC == :32
# Engine-only rules (mapped as at I.14):
#   between_symm_parts -> euclid_apply of between_symm (whole axiom).
#   pasch_3_opposing (b c d AC), (b e f AC) -> euclid_apply (pasch_3 ...):
#     the SystemE axiom, conclusions ¬(b.sameSide d AC) / ¬(b.sameSide f AC).
#   opposing_opposing_same_side (d b f AC) is DERIVED -> CHECKED
#     euclid_assert d.sameSide f AC (BRIDGE_ASSERTS) — the side fact
#     sum_angles_onlyif's premise needs; SMT proves it from the two pasch_3
#     conclusions.
PROP16B_ENGINE_CITABLE = [
    "proposition_10", "between_symm", "line_from_points",
    "extend_point_longer", "proposition_3", "pasch_3", "proposition_15",
    "proposition_4", "sum_angles_onlyif",
]

# Book/Prop17.lean:7-9 verbatim: any two angles of a triangle are together
# less than two right angles.
PROP17_STATEMENT = """\
theorem proposition_17 : ∀ (a b c : Point) (AB BC AC : Line),
  formTriangle a b c AB BC AC →
  ∠ a:b:c + ∠ b:c:a < ∟ + ∟ :="""

# The engine's own I.17 steps (euclid_engine.PROP17_STEPS_DATA, read live):
# flat — extend_point BC b c as d; between_symm_parts (emitted as the whole
# between_symm axiom, as at I.11/I.13–16b); proposition_16 a b c d AB BC AC
# then proposition_13 a c b d AC BC in exactly Lean's argument orders
# (Book/Prop17.lean:13-14). proposition_16's Lean conclusion is a
# CONJUNCTION, but euclid_apply of it needs no `as` and no conjunct
# selection — the emitted line is IDENTICAL to Book/Prop17.lean:13
# (euclid_apply puts both conjuncts in context; euclid_finish's LA uses
# the first, ∠a:c:d > ∠c:b:a, with ∠c:b:a = ∠a:b:c as canonical atoms).
# euclid_finish closes ∠a:b:c + ∠b:c:a < 2∟ from the proposition_13 row
# (∠b:c:a + ∠a:c:d = 2∟) and the inequality. No witness, no bridges.
PROP17_ENGINE_CITABLE = [
    "extend_point", "between_symm", "proposition_16", "proposition_13",
]

# Book/Prop18.lean:8-10 verbatim: the greater side subtends the greater
# angle.
PROP18_STATEMENT = """\
theorem proposition_18 : ∀ (a b c : Point) (AB BC AC : Line),
  formTriangle a b c AB BC AC ∧ (|(a─c)| > |(a─b)|) →
  (∠ a:b:c > ∠ b:c:a) :="""

# The engine's own I.18 steps (euclid_engine.PROP18_STEPS_DATA, read live):
# flat — proposition_3 a c a b AC AB as d (d on AC, between a d c,
# |a-d| = |a-b|); line_from_points b d as BD; proposition_16 b c d a
# BC AC BD; proposition_5' a b d AB BD AC — exactly Book/Prop18.lean:13-16's
# lines — then sum_angles_onlyif b a c d AB BC, NOT Lean's cited
# sum_angles_if (Transfer.lean:83): sum_angles_if consumes the angle-sum
# row and produces same-side facts; the engine must DERIVE the sum row
# ∠b:a:c = ∠b:a:d + ∠d:a:c (i.e. ∠a:b:c = ∠a:b:d + ∠d:b:c) that
# euclid_finish's arithmetic needs, which is the converse axiom
# sum_angles_onlyif (Transfer.lean:90). Its premises are the on_line/ne/
# neq_line facts of the configuration plus the two same-side facts
# (b.sameSide d BC, c.sameSide d AB), which euclid_apply's SMT must
# discharge from between a d c; if it cannot, the certify test reports it
# (no silent extra steps). No witness, no bridges, no euclid_assert.
PROP18_ENGINE_CITABLE = [
    "proposition_3", "line_from_points", "proposition_16", "proposition_5'",
    "sum_angles_onlyif",
]

# Book/Prop19.lean:7-9 verbatim: the greater angle is subtended by the
# greater side.
PROP19_STATEMENT = """\
theorem proposition_19 : ∀ (a b c : Point) (AB BC AC : Line),
  formTriangle a b c AB BC AC ∧ (∠ a:b:c > ∠ b:c:a) →
  (|(a─c)| > |(a─b)|) :="""

# The engine's own I.19 steps (euclid_engine.PROP19_STEPS_DATA, read
# live): a by_contra whose branch list IS the split — branch 1 assumes
# |(a─c)| = |(a─b)| and cites proposition_5' a b c AB BC AC; branch 2
# assumes |(a─b)| > |(a─c)| and cites proposition_18 a c b AC BC AB —
# exactly Book/Prop19.lean:12-16's shape (by_contra; by_cases
# |(a─c)| = |(a─b)|; two bullets of euclid_apply + euclid_finish).
# Lean's negative by_cases branch supplies only ¬(|(a─c)| = |(a─b)|),
# so the engine's branch-2 gt_len form IS needed as an assert and is
# emitted as a CHECKED euclid_assert (trichotomy from ¬> and ¬=);
# euclid_finish does not close branch 2 without it. No witness.
PROP19_ENGINE_CITABLE = [
    "proposition_5'", "proposition_18",
]

# Book/Prop20.lean:8-9 verbatim: any two sides of a triangle are together
# greater than the remaining side.
PROP20_STATEMENT = """\
theorem proposition_20 : ∀ (a b c : Point) (AB BC AC : Line),
  formTriangle a b c AB BC AC → |(b─a)| + |(a─c)| > |(b─c)| :="""

# The engine's own I.20 steps (euclid_engine.PROP20_STEPS_DATA, read
# live): flat — extend_point_longer AB b a (c─a) as d' (SEGMENT_TAIL;
# the engine carries the (c─a) segment as its two endpoints, in exactly
# Lean's argument order, Book/Prop20.lean:12), proposition_3 a d' a c
# AB AC as d, line_from_points d c as DC, proposition_5' a c d AC DC AB,
# sum_angles_onlyif c b d a BC DC, proposition_19 b c d BC DC AB —
# exactly Book/Prop20.lean:13-17's lines. euclid_finish's LA closes
# |b─a| + |a─c| > |b─c| from the sum_angles_onlyif row, proposition_5''s
# angle equality and proposition_19's length inequality. No witness, no
# bridges, no euclid_assert.
PROP20_ENGINE_CITABLE = [
    "extend_point_longer", "proposition_3", "line_from_points",
    "proposition_5'", "sum_angles_onlyif", "proposition_19",
]


PROPS = {
    "I.1": dict(imports=["SystemE"], statement=PROP1_STATEMENT,
                book_steps=PROP1_BOOK_STEPS, witness="c"),
    "I.2": dict(imports=["SystemE", "Book.Prop01"], statement=PROP2_STATEMENT,
                book_steps=PROP2_BOOK_STEPS, witness="l"),
    "I.3": dict(imports=["SystemE", "Book.Prop02"], statement=PROP3_STATEMENT,
                book_steps=PROP3_BOOK_STEPS, witness="e"),
    # I.4 has no existential witness (goal is a conjunction), and its proof
    # uses euclid_assert steps; the goal is closed by euclid_finish's SMT.
    "I.4": dict(imports=["SystemE"], statement=PROP4_STATEMENT,
                book_steps=PROP4_BOOK_STEPS, witness=None),
    # The ENGINE's own I.4 steps (euclid_engine.PROP4_STEPS_DATA, read live),
    # bridges translated to euclid_assert. Same statement/imports as I.4.
    "I.4-engine": dict(imports=["SystemE"], statement=PROP4_STATEMENT,
                       book_steps=None, witness=None, engine="I.4"),
    # I.5 has no existential witness (goal is a conjunction of angle
    # equalities), so witness=None: emit_proof emits no `use` line.
    "I.5": dict(imports=["SystemE", "Book.Prop03", "Book.Prop04"],
                statement=PROP5_STATEMENT, book_steps=PROP5_BOOK_STEPS,
                witness=None),
    # The ENGINE's own I.11 steps (euclid_engine.PROP11_STEPS_DATA, read
    # live, engine names renamed back to Lean's); statement/imports as
    # Book/Prop11.lean, witness f. The engine's SMT-sourced input
    # ("h-sm": not_on_line f AB) is emitted as a CHECKED euclid_assert
    # right after the step that constructs f.
    "I.11-engine": dict(imports=["SystemE", "Book.Prop01", "Book.Prop03",
                                 "Book.Prop08"],
                        statement=PROP11_STATEMENT, book_steps=None,
                        witness="f", engine="I.11",
                        engine_citable=PROP11_ENGINE_CITABLE,
                        # Lean's `use f` closes the goal itself (the h-sm
                        # assert and perpendicular_if conclusion are in
                        # context as hypotheses); a trailing euclid_finish
                        # would error "no goals to be solved".
                        finish=None),
    # The ENGINE's own I.12 steps (euclid_engine.PROP12_STEPS_DATA, read
    # live); statement/imports as Book/Prop12.lean, witness h.
    "I.12-engine": dict(imports=["SystemE", "Book.Prop08", "Book.Prop10"],
                        statement=PROP12_STATEMENT, book_steps=None,
                        witness="h", engine="I.12",
                        engine_citable=PROP12_ENGINE_CITABLE),
    # The ENGINE's own I.13 steps (euclid_engine.PROP13_STEPS_DATA, read
    # live): a nested by_cases tree, emitted by emit_proof13 (emitter=
    # "by_cases"); no existential witness, statement/imports as
    # Book/Prop13.lean. See the comment above PROP13_ENGINE_CITABLE.
    "I.13-engine": dict(imports=["SystemE", "Book.Prop11"],
                        statement=PROP13_STATEMENT, book_steps=None,
                        witness=None, engine="I.13",
                        engine_citable=PROP13_ENGINE_CITABLE,
                        emitter="by_cases"),
    # The ENGINE's own I.14 steps (euclid_engine.PROP14_STEPS_DATA, read
    # live): outer by_contra + inner split, emitted by emit_proof14
    # (emitter="by_contra"); no existential witness, statement/imports as
    # Book/Prop14.lean. See the comment above PROP14_ENGINE_CITABLE.
    "I.14-engine": dict(imports=["SystemE", "Book.Prop13"],
                        statement=PROP14_STATEMENT, book_steps=None,
                        witness=None, engine="I.14",
                        engine_citable=PROP14_ENGINE_CITABLE,
                        emitter="by_contra"),
    # The ENGINE's own I.15 steps (euclid_engine.PROP15_STEPS_DATA, read
    # live): flat — three between_symm_parts (emitted as the whole
    # between_symm axiom, as at I.11/I.13/I.14) then three proposition_13
    # citations in exactly Lean's argument orders (Book/Prop15.lean:12-14:
    # (a e c d AB CD), (d e a b CD AB), (c e a b CD AB)); euclid_finish's
    # LA closes the conjunction of vertical-angle equalities from the three
    # sum-to-2∟ rows. No witness, no by_cases, no bridges.
    "I.15-engine": dict(imports=["SystemE", "Book.Prop13"],
                        statement=PROP15_STATEMENT, book_steps=None,
                        witness=None, engine="I.15",
                        engine_citable=PROP15_ENGINE_CITABLE),
    # The ENGINE's own I.16 SECOND-CONJUNCT steps (euclid_engine
    # PROP16B_STEPS_DATA, read live): flat proof of ∠a:c:d > ∠b:a:c only
    # (the engine does not prove the first conjunct, so the theorem is
    # proposition_16b — the second conjunct of Book/Prop16.lean's
    # proposition_16 — and Prop16.lean is unchanged). Imports are exactly
    # those of Book/Prop16.lean:1-5. No witness, no by_cases; the derived
    # opposing_opposing_same_side step is a CHECKED euclid_assert. See the
    # comment above PROP16B_ENGINE_CITABLE.
    "I.16b-engine": dict(imports=["SystemE", "Book.Prop03", "Book.Prop04",
                                  "Book.Prop10", "Book.Prop15"],
                         statement=PROP16B_STATEMENT, book_steps=None,
                         witness=None, engine="I.16b",
                         engine_citable=PROP16B_ENGINE_CITABLE),
    # The ENGINE's own I.17 steps (euclid_engine.PROP17_STEPS_DATA, read
    # live): flat — extend_point, between_symm (whole axiom for the
    # engine's between_symm_parts), then proposition_16 and
    # proposition_13 in exactly Book/Prop17.lean:13-14's argument orders;
    # euclid_finish's LA closes the strict sum inequality. Statement and
    # imports verbatim from Book/Prop17.lean:1-9. No witness, no bridges.
    # See the comment above PROP17_ENGINE_CITABLE.
    "I.17-engine": dict(imports=["SystemE", "Book.Prop13", "Book.Prop16"],
                        statement=PROP17_STATEMENT, book_steps=None,
                        witness=None, engine="I.17",
                        engine_citable=PROP17_ENGINE_CITABLE),
    # The ENGINE's own I.18 steps (euclid_engine.PROP18_STEPS_DATA, read
    # live): flat — proposition_3 as d, line_from_points as BD,
    # proposition_16, proposition_5' in exactly Book/Prop18.lean:13-16's
    # argument orders, then sum_angles_onlyif (NOT sum_angles_if — the
    # engine derives the angle-sum row Lean's SMT discharges inside
    # sum_angles_if's premise); euclid_finish closes ∠a:b:c > ∠b:c:a.
    # Statement and imports verbatim from Book/Prop18.lean:1-10. No
    # witness, no bridges. See the comment above PROP18_ENGINE_CITABLE.
    "I.18-engine": dict(imports=["SystemE", "Book.Prop03", "Book.Prop05",
                                 "Book.Prop16"],
                        statement=PROP18_STATEMENT, book_steps=None,
                        witness=None, engine="I.18",
                        engine_citable=PROP18_ENGINE_CITABLE),
    # The ENGINE's own I.19 steps (euclid_engine.PROP19_STEPS_DATA, read
    # live): a by_contra whose two branches ARE the case split, emitted
    # by emit_proof19 (emitter="by_contra", split directly in the
    # by_contra's branch list — unlike I.14's nested-by_contra shape).
    # No existential witness, statement/imports as Book/Prop19.lean.
    # See the comment above PROP19_ENGINE_CITABLE.
    "I.19-engine": dict(imports=["SystemE", "Book.Prop05", "Book.Prop18"],
                        statement=PROP19_STATEMENT, book_steps=None,
                        witness=None, engine="I.19",
                        engine_citable=PROP19_ENGINE_CITABLE,
                        emitter="by_contra"),
    # The ENGINE's own I.20 steps (euclid_engine.PROP20_STEPS_DATA, read
    # live): flat — extend_point_longer as d', proposition_3 as d,
    # line_from_points as DC, proposition_5', sum_angles_onlyif,
    # proposition_19, in exactly Book/Prop20.lean:12-17's argument
    # orders; euclid_finish's LA closes |(b─a)| + |(a─c)| > |(b─c)|.
    # Statement and imports verbatim from Book/Prop20.lean:1-9. No
    # witness, no bridges. See the comment above PROP20_ENGINE_CITABLE.
    "I.20-engine": dict(imports=["SystemE", "Book.Prop03", "Book.Prop05",
                                 "Book.Prop19"],
                        statement=PROP20_STATEMENT, book_steps=None,
                        witness=None, engine="I.20",
                        engine_citable=PROP20_ENGINE_CITABLE),
}


# ---------------------------------------------------------------------------
# Proof emission
# ---------------------------------------------------------------------------

def _render_args(rule, args, term=lean):
    """Render engine args for one euclid_apply, gluing segment-tail rules.

    point_between_points_shorter_than ends in a Segment (Points.lean:28); the
    engine carries it as its two endpoint Points, Lean wants (x─y).
    """
    if rule in SEGMENT_TAIL_RULES and len(args) >= 2:
        head = " ".join(term(a) for a in args[:-2])
        seg = f"({term(args[-2])}─{term(args[-1])})"
        return " ".join(x for x in (head, seg) if x)
    return " ".join(term(a) for a in args)


def _step_lines(rule, args, bindings, term):
    """The proof line(s) for one step (shared by emit_step and emit_proof)."""
    if rule == "euclid_assert":
        return [f"  euclid_assert {term(args[0])}"]
    terms = _render_args(rule, args, term=term)
    names = " ".join(term(v) for v in bindings.values())
    if rule in GROUPED_BINDINGS_RULES and names:
        grouped = ", ".join(term(v) for v in bindings.values())
        return [f"  euclid_apply ({rule} {terms}) as ({grouped})"]
    if names:
        return [f"  euclid_apply ({rule} {terms}) as {names}"]
    return [f"  euclid_apply ({rule} {terms})"]


# ---------------------------------------------------------------------------
# Engine step translation ([SMT-bridge] -> euclid_assert)
# ---------------------------------------------------------------------------

# [SMT-bridge] engine rules are emitted as `euclid_assert` of their
# point-equality conclusion (peq x y -> "x = y"); see
# ~/notes/euclid-I4-reconcile.md (P-euclid-certify-engine-I4).
BRIDGE_ASSERTS = {
    "assert_point_eq_on_ray": lambda args: f"{args[0]} = {args[1]}",
    "assert_point_eq_angle": lambda args: f"{args[0]} = {args[1]}",
    # I.13: opposing_same_side_trans is a DERIVED engine rule (from
    # same_side_trans, Diagrammatic.lean:127 area), not a SystemE name.
    # Emitted as a CHECKED euclid_assert of the conclusion its consumer
    # (triple_incidence_2) needs; see the comment above PROP13_ENGINE_CITABLE.
    "opposing_same_side_trans": lambda args: f"¬({args[0]}.sameSide {args[2]} {args[3]})",
    # I.14: opposing_opposing_same_side is DERIVED (c, d opposingSides of L
    # and c, e opposingSides of L -> d.sameSide e L: both on the far side
    # from c), not a SystemE name. Emitted as a CHECKED euclid_assert of
    # its conclusion; BOTH branches consume it (branch 1's
    # sum_angles_onlyif side premise, branch 2's triple_incidence_2
    # c.sameSide d L premise). Args (d, c, e, AB) -> "d.sameSide e AB".
    "opposing_opposing_same_side": lambda args: f"{args[0]}.sameSide {args[2]} {args[3]}",
}

# Engine binding names that differ from the Book's, renamed at emit time only
# (the engine file names its fourth superposition object AC'; Book/Prop04.lean
# names it DC'. The engine file is zai-11's, so the reconciliation lives here.)
BINDING_RENAMES = {"AC'": "DC'"}

# Engine rule names that are not SystemE names but cite part of one axiom:
# emitted as an euclid_apply of that axiom (the extra conclusions land in
# context harmlessly). See the I.11 comment block above PROP11_ENGINE_CITABLE.
ENGINE_RULE_TO_LEAN = {"between_symm_parts": "between_symm",
                       # I.13: pasch_3_opposing IS pasch_3
                       # (Diagrammatic.lean:163); the "opposing" in the
                       # engine name refers to the ¬sameSide conclusion.
                       "pasch_3_opposing": "pasch_3"}

# h-sm (SMT-sourced) engine input facts, rendered as a CHECKED euclid_assert
# proposition text. Generalised only as far as I.11 needs (one fact shape).
HSM_FACT_ASSERTS = {
    "not_on_line": lambda a: f"¬({a[0]}.onLine {a[1]})",
}


def _insert_hsm_asserts(steps, hsm_facts, given):
    """Emit each SMT-sourced ('h-sm') input as an euclid_assert right after
    the step that creates the last object it names (a name in `given` —
    bound by the statement — is available from the start). Lean must prove
    the assert: the input the engine took on trust is CHECKED here.
    """
    steps = list(steps)
    for fact in hsm_facts:
        kind, args = fact[0], fact[1:]
        text = HSM_FACT_ASSERTS[kind](args)
        pos = 0
        for name in args:
            if name in given:
                continue
            for i, (_r, _a, b) in enumerate(steps):
                if name in b.values():
                    pos = max(pos, i + 1)
        steps.insert(pos, ("euclid_assert", (text,), {}))
    return steps


def citable_apply_rules():
    """Engine rules known to be Lean-citable (euclid_apply-able): every rule
    that appears in a PROPS book-step list. Grown as PROPS grows."""
    return {r for cfg in PROPS.values()
            for r in (cfg.get("engine_citable") or ())
            } | {r for cfg in PROPS.values()
                 if cfg.get("book_steps")
                 for (r, _a, _b) in cfg["book_steps"]}


def translate_engine_steps(steps):
    """Engine (rule, args, bindings) steps -> emittable steps.

    A bridge rule becomes an euclid_assert step; a citable rule keeps its
    apply shape with binding renames applied; anything else raises (a rule
    that is neither citable in Lean nor a known bridge must not be silently
    dropped or mis-emitted)."""
    citable = citable_apply_rules()
    out = []
    for rule, args, bindings in steps:
        rule = ENGINE_RULE_TO_LEAN.get(rule, rule)
        if rule in BRIDGE_ASSERTS:
            out.append(("euclid_assert", (BRIDGE_ASSERTS[rule](args),), {}))
        elif rule == "euclid_assert" or rule in citable:
            b2 = {k: BINDING_RENAMES.get(v, v) for k, v in bindings.items()}
            out.append((rule, args, b2))
        else:
            raise ValueError(
                f"engine rule {rule!r} is neither a citable euclid_apply "
                f"rule nor a known [SMT-bridge] (BRIDGE_ASSERTS)")
    return out


_ENGINE = None


def _engine():
    """Load euclid_engine lazily and cache it (monkeypatchable in tests)."""
    global _ENGINE
    if _ENGINE is None:
        spec = importlib.util.spec_from_file_location(
            "euclid_engine", FUTON0 / "scripts" / "euclid_engine.py")
        _ENGINE = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(_ENGINE)
    return _ENGINE


def engine_steps(prop):
    """The engine's own step data for prop, translated for emission.
    Read live from euclid_engine (never a hand copy)."""
    eng = _engine()
    if prop == "I.4" and hasattr(eng, "PROP4_STEPS_DATA"):
        return translate_engine_steps(eng.PROP4_STEPS_DATA)
    if prop == "I.11" and hasattr(eng, "PROP11_STEPS_DATA"):
        steps = translate_engine_steps(eng.PROP11_STEPS_DATA)
        rn = PROP11_ENGINE_RENAMES
        steps = [(r, tuple(rn.get(a, a) for a in args),
                  {k: rn.get(v, v) for k, v in b.items()})
                 for (r, args, b) in steps]
        hsm = [tuple(rn.get(x, x) for x in fact)
               for (fact, prov) in eng.PROP11_HYPS_DATA if prov == "h-sm"]
        return _insert_hsm_asserts(steps, hsm, PROP11_GIVEN)
    if prop == "I.12" and hasattr(eng, "PROP12_STEPS_DATA"):
        return translate_engine_steps(eng.PROP12_STEPS_DATA)
    if prop == "I.15" and hasattr(eng, "PROP15_STEPS_DATA"):
        return translate_engine_steps(eng.PROP15_STEPS_DATA)
    if prop == "I.16b" and hasattr(eng, "PROP16B_STEPS_DATA"):
        return translate_engine_steps(eng.PROP16B_STEPS_DATA)
    if prop == "I.17" and hasattr(eng, "PROP17_STEPS_DATA"):
        return translate_engine_steps(eng.PROP17_STEPS_DATA)
    if prop == "I.18" and hasattr(eng, "PROP18_STEPS_DATA"):
        return translate_engine_steps(eng.PROP18_STEPS_DATA)
    if prop == "I.20" and hasattr(eng, "PROP20_STEPS_DATA"):
        return translate_engine_steps(eng.PROP20_STEPS_DATA)
    raise ValueError(f"no engine step data registered for {prop}")


# ---------------------------------------------------------------------------
# I.13-engine: nested by_cases emission (E-kimi-task-381)
# ---------------------------------------------------------------------------

# Engine by_cases branch-assumption fact -> the Lean proposition text of the
# split (branch 1's assumption; later branches get theirs as CHECKED
# euclid_asserts). Grown as by_cases props grow.
CASE_FACT_LEAN = {
    "eq_ang": lambda a: f"∠ {a[0]}:{a[1]}:{a[2]} = ∠ {a[3]}:{a[4]}:{a[5]}",
    "same_side": lambda a: f"{a[0]}.sameSide {a[1]} {a[2]}",
    # I.19: the by_cases split is on a LENGTH equality.
    "eq_len": lambda a: f"|({a[0]}─{a[1]})| = |({a[2]}─{a[3]})|",
    "gt_len": lambda a: f"|({a[0]}─{a[1]})| > |({a[2]}─{a[3]})|",
}


def _case_fact_text(fact, term):
    return CASE_FACT_LEAN[fact[0]](tuple(term(x) for x in fact[1:]))


def engine_steps13():
    """The engine's PROP13_STEPS_DATA as a translated by_cases tree:
    ("by_cases", [(assumption-facts, steps)], conclusions) with flat steps
    translated and nested by_cases recursed. Read live from euclid_engine."""
    eng = _engine()

    def conv(steps):
        out = []
        for s in steps:
            if s[0] == "by_cases":
                _, branches, conclusions = s
                out.append(("by_cases",
                            [(list(facts), conv(bsteps))
                             for facts, bsteps in branches],
                            conclusions))
            else:
                out.extend(translate_engine_steps([s]))
        return out

    return conv(eng.PROP13_STEPS_DATA)


def _emit_block(steps, depth):
    """Emit (possibly nested) translated steps at the given indent depth.

    A ("by_cases", branches, _) step emits `by_cases <prop>` where prop is
    branch 1's assumption (in context in branch 1); each later branch's
    assumptions emit as CHECKED euclid_asserts. Each branch ends with
    euclid_finish unless its last step is itself a by_cases (whose inner
    branches close the goal).
    """
    pad = "  " * depth
    lines = []
    for s in steps:
        if s[0] != "by_cases":
            lines.extend(f"{pad}{l.strip()}"
                         for l in _step_lines(s[0], s[1], s[2], lean))
            continue
        _, branches, _ = s
        if not branches or len(branches[0][0]) != 1:
            raise ValueError("by_cases branch 1 must carry exactly one "
                             "assumption fact (the split proposition)")
        lines.append(f"{pad}by_cases {_case_fact_text(branches[0][0][0], lean)}")
        for i, (facts, bsteps) in enumerate(branches):
            blines = []
            if i > 0:
                # The negative branch of Lean's by_cases supplies only the
                # negation of the split proposition; the engine's branch
                # assumption is therefore asserted (SMT-CHECKED).
                blines.extend(f"euclid_assert {_case_fact_text(f, lean)}"
                              for f in facts)
            blines.extend(_emit_block(bsteps, 0))
            if not (bsteps and bsteps[-1][0] == "by_cases"):
                blines.append("euclid_finish")
            lines.append(f"{pad}. {blines[0]}")
            lines.extend(f"{pad}  {l}" for l in blines[1:])
    return lines


def emit_proof13():
    """The full Lean source for I.13-engine (nested by_cases emitter)."""
    cfg = PROPS["I.13-engine"]
    lines = [f"import {imp}" for imp in cfg["imports"]]
    lines.append("")
    lines.append("namespace Elements.Book1")
    lines.append("")
    lines.append(cfg["statement"])
    lines.append("by")
    lines.append("  euclid_intros")
    lines.extend(_emit_block(engine_steps13(), 1))
    lines.append("")
    lines.append("end Elements.Book1")
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# I.14-engine: outer by_contra + inner split emission (E-kimi-task-387)
# ---------------------------------------------------------------------------


def engine_steps14():
    """The engine's PROP14_STEPS_DATA as translated steps for emit_proof14.

    The data is a single outer by_contra whose one branch holds flat steps
    ending in a NESTED by_contra over same_side/opposing_sides a e BD.
    The nested by_contra becomes a ("by_cases", ...) node (both branches
    prove False, so (P -> ⊥) ∧ (¬P -> ⊥) ⊢ ⊥ IS the split, as in
    Book/Prop14.lean:14-18); branch 2's opposing_sides fact is DROPPED —
    Lean's negative by_cases branch supplies exactly ¬(a.sameSide e BD),
    which is the premise shape triple_incidence_2 needs. Flat steps are
    translated (bridges -> checked euclid_asserts).
    """
    eng = _engine()
    data = eng.PROP14_STEPS_DATA
    if (len(data) != 1 or data[0][0] != "by_contra"
            or len(data[0][1]) != 1):
        raise ValueError("PROP14_STEPS_DATA must be a single outer "
                         "by_contra with one branch")
    _, branches, _ = data[0]
    ((_, steps),) = branches
    out = []
    for s in steps:
        if s[0] in ("by_contra", "by_cases"):
            _, branches, conclusions = s
            conv = []
            for i, (facts, bsteps) in enumerate(branches):
                kept = list(facts) if i == 0 else []
                tb = []
                for bs in bsteps:
                    tb.extend(translate_engine_steps([bs]))
                conv.append((kept, tb))
            out.append(("by_cases", conv, conclusions))
        else:
            out.extend(translate_engine_steps([s]))
    return out


def emit_proof14():
    """The full Lean source for I.14-engine (by_contra emitter)."""
    cfg = PROPS["I.14-engine"]
    lines = [f"import {imp}" for imp in cfg["imports"]]
    lines.append("")
    lines.append("namespace Elements.Book1")
    lines.append("")
    lines.append(cfg["statement"])
    lines.append("by")
    lines.append("  euclid_intros")
    # Lean's by_contra supplies ¬(BC = BD) — the engine's outer-branch
    # assumption (neq_line BC BD). The by_cases branches inside close the
    # False goal; no trailing euclid_finish.
    lines.append("  by_contra")
    lines.extend(_emit_block(engine_steps14(), 1))
    lines.append("")
    lines.append("end Elements.Book1")
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# I.19-engine: by_contra whose branch list IS the split (E-kimi-task-406)
# ---------------------------------------------------------------------------


def engine_steps19():
    """The engine's PROP19_STEPS_DATA as translated steps for emit_proof19.

    Unlike I.14 (outer by_contra wrapping flat steps ending in a NESTED
    by_contra), I.19's by_contra carries the two case branches directly:
    branch 1 ([eq_len a c a b], proposition_5'), branch 2
    ([gt_len a b a c], proposition_18). They become the ("by_cases", ...)
    node's branches verbatim — branch 1's eq_len is the split proposition
    (Book/Prop19.lean:13's `by_cases |(a─c)| = |(a─b)|`), and branch 2's
    gt_len is KEPT (unlike I.14's dropped opposing_sides): Lean's
    negative branch supplies only ¬(|(a─c)| = |(a─b)|, so _emit_block
    emits it as a CHECKED euclid_assert (trichotomy with the by_contra
    hypothesis ¬(|(a─c)| > |(a─b)|)); euclid_finish cannot close branch 2
    from the bare negations' proposition_18 premise without it.
    """
    eng = _engine()
    data = eng.PROP19_STEPS_DATA
    if len(data) != 1 or data[0][0] != "by_contra":
        raise ValueError("PROP19_STEPS_DATA must be a single by_contra "
                         "whose branch list is the case split")
    _, branches, conclusions = data[0]
    if len(branches) != 2:
        raise ValueError("PROP19_STEPS_DATA by_contra must have exactly "
                         "two branches (the length trichotomy split)")
    conv = []
    for facts, bsteps in branches:
        tb = []
        for bs in bsteps:
            tb.extend(translate_engine_steps([bs]))
        conv.append((list(facts), tb))
    return [("by_cases", conv, conclusions)]


def emit_proof19():
    """The full Lean source for I.19-engine (by_contra emitter)."""
    cfg = PROPS["I.19-engine"]
    lines = [f"import {imp}" for imp in cfg["imports"]]
    lines.append("")
    lines.append("namespace Elements.Book1")
    lines.append("")
    lines.append(cfg["statement"])
    lines.append("by")
    lines.append("  euclid_intros")
    # Lean's by_contra supplies ¬(|(a─c)| > |(a─b)|); the by_cases branches
    # close the False goal, so no trailing euclid_finish (as at I.14).
    lines.append("  by_contra")
    lines.extend(_emit_block(engine_steps19(), 1))
    lines.append("")
    lines.append("end Elements.Book1")
    return "\n".join(lines) + "\n"


def emit_step(rule, args, bindings):
    """One euclid_apply line from an engine step."""
    return "\n".join(_step_lines(rule, args, bindings, lean))


def emit_proof(prop, steps, witness, statement=None, finish="euclid_finish"):
    cfg = PROPS[prop]
    # Search-found steps carry engine fresh names containing '$' (illegal in
    # Lean); rename them to v1, v2, ... in order of first appearance.
    rename = {}

    def nm(x):
        if isinstance(x, str) and "$" in x and x not in rename:
            rename[x] = f"v{len(rename) + 1}"
        return rename.get(x, x)

    def term(x):
        return lean(nm(x))

    lines = [f"import {cfg['imports'][0]}"]
    for imp in cfg["imports"][1:]:
        lines.append(f"import {imp}")
    lines.append("")
    lines.append("namespace Elements.Book1")
    lines.append("")
    lines.append(statement if statement is not None else cfg["statement"])
    lines.append("by")
    lines.append("  euclid_intros")
    for rule, args, bindings in steps:
        lines.extend(_step_lines(rule, args, bindings, term))
    if witness is not None:
        lines.append(f"  use {term(witness)}")
    if finish is not None:
        lines.append(f"  {finish}")
    lines.append("")
    lines.append("end Elements.Book1")
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# Lean checking
# ---------------------------------------------------------------------------

def env():
    e = dict(os.environ)
    e["PATH"] = (f"{LEAN_EUCLID / 'venv' / 'bin'}:{Path('~/.local/bin').expanduser()}"
                 f":{e.get('PATH', '')}")
    libdir = Path("~/.elan/toolchains/leanprover--lean4---v4.19.0/lib/lean")
    e["LD_LIBRARY_PATH"] = str(libdir.expanduser())
    return e


def check_lean(source, name, timeout=600):
    """Run `lake env lean` on the emitted proof.

    Returns dict(certified, rc, wall_s, reason, output). Certified iff
    rc == 0 and no "error:" line and no "declaration uses 'sorry'".
    """
    SCRATCH.mkdir(exist_ok=True)
    path = SCRATCH / f"{name}.lean"
    path.write_text(source)
    t0 = time.perf_counter()
    # own session, so a timeout kills lean too, not only lake (review,
    # claude-9: lean outlived subprocess.run's timeout as an orphan)
    proc = subprocess.Popen(
        ["lake", "env", "lean", str(path)],
        cwd=str(LEAN_EUCLID), env=env(), stdout=subprocess.PIPE,
        stderr=subprocess.PIPE, text=True, start_new_session=True)
    try:
        stdout, stderr = proc.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        # a solver that never answers is a refusal, not a certifier crash
        os.killpg(proc.pid, signal.SIGKILL)
        stdout, stderr = proc.communicate()
        return {"certified": False, "rc": None,
                "wall_s": round(time.perf_counter() - t0, 2),
                "reason": f"timeout after {timeout} s",
                "output": (stdout + stderr).strip(), "path": str(path)}
    wall = time.perf_counter() - t0
    out = (stdout + stderr).strip()
    reasons = []
    if "declaration uses 'sorry'" in out:
        reasons.append("declaration uses 'sorry'")
    for line in out.splitlines():
        if ": error:" in line or line.startswith("error:"):
            reasons.append(line.strip())
    if proc.returncode != 0:
        reasons.append(f"exit code {proc.returncode}")
    return {"certified": not reasons, "rc": proc.returncode,
            "wall_s": round(wall, 2), "reason": "; ".join(dict.fromkeys(reasons)),
            "output": out, "path": str(path)}


# ---------------------------------------------------------------------------
# Modes
# ---------------------------------------------------------------------------

def certify_book():
    results = {}
    for prop, cfg in PROPS.items():
        if cfg.get("emitter") == "by_cases":
            src = emit_proof13()
        elif cfg.get("emitter") == "by_contra":
            src = emit_proof14() if prop == "I.14-engine" else emit_proof19()
        elif cfg.get("engine"):
            src = emit_proof(prop, engine_steps(cfg["engine"]), cfg["witness"],
                             finish=cfg.get("finish", "euclid_finish"))
        else:
            src = emit_proof(prop, cfg["book_steps"], cfg["witness"],
                             finish=cfg.get("finish", "euclid_finish"))
        r = check_lean(src, f"book_{prop.replace('.', '').replace('-', '_')}")
        results[prop] = r
        _report(f"book {prop}", r)
    return results


def certify_found():
    """Certify the sequences found by euclid_search (BFS).

    Only runs when scripts/euclid_search.py is committed (the requisition's
    pending rule: zai-11 owns that file while it is uncommitted).
    """
    r = subprocess.run(["git", "-C", str(FUTON0), "ls-files",
                        "--error-unmatch", "scripts/euclid_search.py"],
                       capture_output=True)
    if r.returncode != 0:
        print("found-sequence: PENDING — scripts/euclid_search.py is not "
              "committed yet (zai-11 owns it); only the Book control "
              "sequences were certified.")
        return None
    spec = importlib.util.spec_from_file_location(
        "euclid_search", FUTON0 / "scripts" / "euclid_search.py")
    srch = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(srch)
    results = {}
    for prop, kw in srch.prop_searches().items():
        found, path, st = srch.search(**kw)
        if not found:
            results[prop] = {"certified": False,
                             "reason": "search did not find a sequence"}
            _report(f"found {prop}", results[prop])
            continue
        srch.verify_path(kw["hyps"], path, kw["goal"], kw.get("extra_la"))
        # witness: the point introduced by the final construction step
        witness = list(path[-1][2].values())[-1]
        res = check_lean(emit_proof(prop, path, witness),
                         f"found_{prop.replace('.', '')}")
        results[prop] = res
        _report(f"found {prop}", res)
    return results


def negatives():
    """(a) drop one construction step; (b) unprovable conclusion with
    exit-0/`sorry`-only variants."""
    out = {}
    # (a) I.1 without the second circle: intersection_circles loses its premise.
    steps = [s for s in PROP1_BOOK_STEPS
             if not (s[0] == "circle_from_points" and s[1] == ("b", "a"))]
    r = check_lean(emit_proof("I.1", steps, "c"), "neg_missing_step")
    out["drop-second-circle"] = r
    _report("negative drop-second-circle", r)
    # (b1) false conclusion the SMT solver refutes (SAT): "error: failed".
    bad = PROP1_STATEMENT.replace(
        "∃ c : Point, |(c─a)| = |(a─b)| ∧ |(c─b)| = |(a─b)|",
        "∃ c : Point, |(c─a)| = |(a─b)| ∧ ¬(|(c─b)| = |(a─b)|)")
    r = check_lean(emit_proof("I.1", PROP1_BOOK_STEPS, "c", statement=bad),
                   "neg_sat")
    out["sat-refuted"] = r
    _report("negative sat-refuted", r)
    # (b2) the exit-0 trap the review flagged: euclid_finish's solver-unknown
    # path closes with `admitGoal` (sorry). A hand-substituted sorry stands in
    # for it (in this LeanEuclid build solver-unknown also logs an error, so
    # the rc-0 case is demonstrated directly): only the sorry check catches it.
    r = check_lean(emit_proof("I.1", PROP1_BOOK_STEPS, "c", finish="sorry"),
                   "neg_sorry")
    out["sorry-only"] = r
    _report("negative sorry-only", r)
    return out


def _report(label, r):
    print(f"{label}: {'CERTIFIED' if r['certified'] else 'NOT certified'}"
          f" ({r.get('wall_s', '-')}s)"
          + ("" if r["certified"] else f" — {r['reason']}"))


def main():
    import sys
    mode = sys.argv[1] if len(sys.argv) > 1 else "book"
    if mode == "book":
        certify_book()
    elif mode == "found":
        certify_found()
    elif mode == "negatives":
        negatives()
    elif mode == "all":
        certify_book()
        certify_found()
        negatives()
    else:
        raise SystemExit(f"unknown mode {mode!r}")


if __name__ == "__main__":
    main()
