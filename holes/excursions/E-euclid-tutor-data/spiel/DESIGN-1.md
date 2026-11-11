# EuclidZero design 1: from patches to structure (claude-9, 2026-11-11)

Joe, 2026-11-11, after Probes 10-14: "some of this is sounding a bit sloppy,
e.g., listing lots of illegal moves? I think we should be a bit more
strategic here." Then: "your 3-step plan is good." Nothing below is specific
to geometry; Euclid is the first domain.

## What the probes showed

Probes 10-14 (LEDGER.md) each removed one cost: win-in-one (P11), network
resume (RESUME-1), thread budget (THREADS-1), root-only checks (P13),
legality vs the guard (LEGAL-1), paging (P14). Stage 1 of the I.2 DEMO
curriculum now passes reliably; stage 2 does not get enough games. The costs
come from three choices in the game's design:

1. **A fixed menu of ids.** The network outputs a score for each of 371,361
   ids (I.2), of which ~500 are legal at a stage-2 position. Paging through
   the menu by prior drew 99.86% illegal ids (P14 interim). This is a
   representation problem, not a search problem.
2. **Aliases.** `name_point` and the point-creating rules give existing
   points new names. Each alias multiplies the candidate moves, and moves on
   aliased (degenerate) figures are exactly what the contradiction guard had
   to refuse (LEGAL-1: superposition on a,d_2,x_2 with x_2 = d_2).
3. **Full closure everywhere.** Applying a move runs the engine's full forward
   closure and arithmetic; that is most of a move's cost (WIN1-PROFILE: 1.87
   of 2.49 s). SMT-1 showed cvc5 can do the deductions from LeanEuclid's own
   encoding.

## The plan

Each step is tested on its own before the next; no long runs until step 2.

### Step 1: one name per object

The game works over the quotient by known point equality (`peq`): a name the
engine knows equals an earlier object is not a separate object for move
generation, encoding or observation. Moves are generated over distinct
objects only.

Shows: the legal-move count at the I.2 stage-1/2 prefixes and perturbed
starts, before/after; the 8-move I.2 witness and the 20 recorded stage-1 wins
still win; the LEGAL-1 reproduction cannot arise; no change to the engine's
semantics (the engine still records `peq`; only the game's view changes).

### Step 2: score legal moves, not a menu

The game generates candidate moves from each rule's premises (as enumeration
does now). The network scores each candidate from its description: a rule
embedding plus the roles its arguments fill (the notation "I.4 abc def", with
objects described by how they were made and what is known of them). The
policy is a softmax over the candidates. Nothing illegal is ever proposed;
the network's size does not depend on the number of possible moves; its input
is the dictionary of moves. Paging survives in a clean form: generate
candidates rule by rule in the order the network prefers rules.

This needs our own AlphaZero loop (search + training targets over candidate
lists); OpenSpiel's packaged loop assumes a fixed action space. Keep
OpenSpiel's game interface for play, CLI and watch pages.

Shows: untrained search wins stage 1 with the root win-in-one check; trained
on stage 1, the policy puts the winning move first on held-out perturbed
starts; per-move cost at the stage-2 prefix vs P13 (22.9 s) and P14.

### Step 3: deduction only where it pays

The search chooses among constructions. Whether the goal follows is checked
at the positions that need it (root, and nodes the search keeps returning
to), by the engine's closure or by cvc5, rather than closing every node.
Deduction-only moves (no new object) can then leave the search. Depends on
the cvc5 cost (SMT-1: 0.04-4 s per "follows", 5 s timeout per "unknown").

Shows: per-position cost; the same wins found; any position where cvc5 and
the engine disagree is logged and explained (SMT-1's I.4 finding is the
first).

## Later

- A second small domain, as Atari had many games; but one working example
  first ("one breakout example"), given our compute. Candidates to weigh then:
  propositional natural deduction, equational group theory, small Lean
  `Nat` lemmas.
- Coherent-logic prover as a constructive partner; Luna + cvc5 pairing.

## Status

- P14 (codex-26, pager on the old design) is finishing as a baseline; no more
  runs of that design.
- Step 1 packet: P-euclid-design-1-step1 (below in the ledger when sent).
