# Meta game brief for Fable

Notes from a voice conversation between Joe and Claude, Wednesday 11 November 2026. They build on the setter/solver meta game Joe worked out with Fable.

## The game

A setter gives a solver some rules and a problem. The solver has to find auxiliary constructions to solve it. This goes straight at the inventive part of Euclid.

- **Solvability:** the setter commits in advance to a hidden witness construction (a commitment scheme, as in cryptography). The witness is checked only if the solver gives up. This makes the setter behave like a hider in Hide and Seek: it has to actually be somewhere.
- **Scoring (Dixit-style):** like the storyteller in Dixit, the setter scores nothing if every solver cracks the problem or if none does. Problems should be hard to find but reachable.
- **Self-play:** the setter/solver pair supplies its own rising curriculum, AlphaZero-style.

## Cryptography analogy

Checking a solution is easy and finding one is hard: that's the trapdoor shape. The setter's witness plays the role of a private key. The elliptic curve group law is itself a chord-and-tangent construction, so Euclid and modern crypto sit close together.

## Prior work on equational treatments

Fable suggested nobody has given an equational treatment of Euclidean proofs. There is related work:

- Wu's method and Gröbner bases: algebraic, with none of Euclid's flavour.
- The area method (Chou, Gao, Zhang): short, readable proofs by rewriting.
- Pambuccian: quantifier-free axiomatizations in which constructions are operation symbols with precise geometric meaning, achieved by removing relation symbols rather than by Skolemizing.
- Beeson: a constructive Tarski geometry, including Skolemized Tarski and continuity of the Skolem functions.
- System E (Avigad, Dean, Mumma): construction rules are sequents Π ⇒ ∃x. Θ, and the proof body is quantifier-free. E's soundness and completeness go through the ruler-and-compass fragment of Tarski.

We found no explicit E-to-Pambuccian translation. Skolemizing E's construction rules is the obvious bridge to try, and could still be a real gap.

## Operations as functional relations

A hyperedge is an n-ary relation. A Pambuccian operation is a relation whose inputs determine its output, so it has a direction and a guaranteed witness. That is what makes it a move rather than a claim. E-graphs are hypergraphs whose edges are all functional.

## Clusions: from an equational to a transformational game

Joe's *clusions* generalize Nelson's transclusion to any operation that moves one thing into another, not only equality. An equation is a two-way clusion. Allowing one-way moves turns the game into rewriting, which fits Euclid: you can't un-draw a circle, but you can reuse it.

## Proof length and the board

Tarski-level proofs are long because Tarski has no circles: I.1 needs equidistance plus a continuity axiom. In System E, I.1 is two circles and a triangle, which matches Joe's engine. Plan: play the game in E, keep Tarski underneath for soundness, and let proved results become new moves (lemmas as macros, as Euclid does) so the effective board stays small.

## How E was found: co-exactness

Following Manders, Euclid trusts the diagram only for co-exact facts, the ones that survive a small wiggle (incidence, sidedness, intersection). Exact facts such as equal lengths always get argued in the text. E makes co-exact inference automatic.

The Tarski-to-E game in general: in any domain, choose the wiggle, make the invariant facts free, and make the rest into earned moves. Ko and superko in Go are a related invariance rule, banning repeated positions. The game needs something similar so solvers can't loop by re-deriving free facts.

## Joe's contribution: mine simp

Simp lemmas (along with norm\_num and positivity) record what the Mathlib community decided should be free. They give an empirical proxy for co-exactness in each area. Simp erases what a symmetry makes invisible (for example, 1 + (−1)). Co-exact facts are what local wiggles make invisible. Both are invariance under a transformation. **Recipe: pick the symmetries first. Whatever they leave unchanged becomes the free part of the game.**

## Toward mathematicizing mathematics

Co-exactness is a local symmetry: it holds on open sets of configurations. That points to gauge theory (fibre bundles, connections), which meets category theory through sheaves and fibrations. Each proof context gets local notation or coordinates, and a connection carries meaning between contexts.

- **Curvature:** loops of rewrites that don't return to the start. This shows where a setter can hide information at all.
- **Hardness:** the cost of the way back. This shows how well the information stays hidden.
- **Bounded agents (Joe's point):** life is finite, so hard means effectively impossible. Measure curvature relative to a solver's budget: a loop counts as open if this player, with this many moves, can't close it. The setter's job is to find loops just beyond the solver's horizon.
