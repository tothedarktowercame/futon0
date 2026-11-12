# DESIGN-2 packet 8a: search values and first-play value

Date: 2026-11-12. No training was run. This packet changes the single-player
search value range from `[0, 1]` to `[-1, 1]`: goal is +1, while cap-out and a
dead end are -1. Actor receipts use the same targets. Replay receipt format 2
refuses the old format-1 records rather than silently treating their zeroes as
the new loss value.

For an unvisited child, PUCT now uses the parent's current mean value, or the
parent's network value before it has a visit, minus `first_play_reduction`.
The reduction is recorded in the loop config and defaults to zero. There is no
other selection change.

## Pure stub tree

The deterministic stub has 300 root moves and 20 replies. Root move 20 plus
reply 0 wins in two moves; every other two-move line caps out. `A` is a
uniform-prior, constant-zero model. `B` is a uniform-prior, constant-+0.9
model, reproducing the optimistic value head learned in packet 7. `before`
uses the former zero value for unvisited children; `after` uses first-play
value. Counts are distinct visited root children.

| model | simulations | before children | before win? | after children | after win? |
|---|---:|---:|---|---:|---|
| A: value 0 | 32 | 32 | no | 32 | no |
| A: value 0 | 256 | 256 | no | 256 | no |
| B: value +0.9 | 32 | **16** | no | **32** | no |
| B: value +0.9 | 256 | **21** | yes | **256** | no |

The old optimistic head locks onto each visited branch until its second-ply
loss lowers that branch. First-play value instead makes an unvisited branch
as promising as the parent's estimate, so uniform priors spend the budget on
breadth and do not revisit the winning first move within 256 simulations.
This is expected: breadth alone does not identify which branch deserves a
second visit.

With a 0.5 prior planted on root move 20 (the other 0.5 spread over 299
moves), the after-search visits one root child and finds the two-ply win in 32
simulations. Thus the search both explores under an uninformative optimistic
head and exploits a useful policy prior.

## Real rc13 stage-2 prefix

The step-39 packet-7 model was searched ten times from
`rc13/stage-2/prefix.json`, 32 simulations, root scan off. Before is packet
7b's receipt; after is this packet. The premise-certified list has 532 moves
and the aligned guarded target has 530 after two selected candidates are
refused.

| searches | before visited children | before visits | after visited children | after visits |
|---:|---:|---|---:|---|
| 1--10 | 4 each | 10, 10, 9, 3 | **8 each** | 5, 5, 5, 5, 4, 4, 3, 1 |

First-play value doubles real root breadth, though the trained policy remains
concentrated: 8 of 530 target entries receive visits. The full after receipt
is `loop-smoke/search-8a-real.json`.

## Gates

- Pure stub: legacy lock-in measured; planted-prior two-ply win found at 32.
- Candidate PUCT, replay, and loop tests: 15 passed.
- Old receipt format refused explicitly; cap-out/dead-end value is -1.
- Stage-1 trained snapshot evaluation: 10/10 wins with fresh Spiel and play
  checker replay.
