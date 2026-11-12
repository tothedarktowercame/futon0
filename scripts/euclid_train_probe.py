#!/usr/bin/env python3
"""Probe each new snapshot of a candidate-AlphaZero run on the I.2 stage-2 position.

    ~/.venvs/openspiel/bin/python euclid_train_probe.py RUN_DIR [--every 60] [--once]

Read-only.  Uses the rc13 stage-2 prefix and packet 8c's finding (CHECKS-8C.md):
of 527 first moves, 7 leave a win in one (index 24, circle_from_points, and
370-375, the triangle + superposition route).  For the newest snapshot it
prints:
  rank  - best prior rank among the 7 good first moves (of ~532);
  good  - value of the position after the circle move and after the triangle
          move (should rise towards +1 if the run learns what counts);
  dead  - mean value after first moves 0-9 (none leaves a win; should stay low);
  gap   - good mean minus dead mean.
Losses can fall while none of these move; these are the numbers that count.
"""
import argparse
import glob
import os
import sys
import time

import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import euclid_candidate_checks_8c as c8  # noqa: E402
import euclid_spiel  # noqa: E402

PREFIX = os.path.expanduser(
    "~/code/futon0/holes/excursions/E-euclid-tutor-data/spiel/rc13/stage-2/prefix.json")
GOOD = [24, 370, 371, 372, 373, 374, 375]
GOOD_POSITIONS = [24, 370]  # 370-375 reach one position
DEAD = list(range(10))


def build():
    root = c8.game(PREFIX).new_initial_state()
    moves = c8.candidates(root)
    children = {}
    for index in GOOD_POSITIONS + DEAD:
        successor, refusal = euclid_spiel.admit_candidate(
            root.engine_state, len(root.history_moves), moves[index])
        if refusal is not None:
            raise RuntimeError("first move %d refused: %s" % (index, refusal))
        child = c8.child(root, moves[index], successor)
        children[index] = (child, c8.candidates(child))
    return root, moves, children


def probe(snapshot, root, moves, children, cache):
    from flax import nnx
    import euclid_candidate_features as features
    import euclid_candidate_model as model_module
    model = model_module.CandidateModel(seed=0)
    model_module.load_plain_arrays(model, snapshot)
    run = nnx.jit(lambda network, inputs: network(inputs))
    if "root" not in cache:
        cache["root"] = model_module.batch_features([features.extract(root, moves).arrays])
        for index, (child, child_moves) in children.items():
            cache[index] = model_module.batch_features(
                [features.extract(child, child_moves).arrays])
    logits, _ = run(model, cache["root"])
    logits = np.asarray(logits[0, :len(moves)])
    order = np.argsort(-logits, kind="stable")
    ranks = np.empty(len(order), dtype=np.int64)
    ranks[order] = np.arange(1, len(order) + 1)
    values = {index: float(np.asarray(run(model, cache[index])[1]).ravel()[0])
              for index in children}
    good = [values[i] for i in GOOD_POSITIONS]
    dead = [values[i] for i in DEAD]
    return (int(min(ranks[i] for i in GOOD)), len(moves), good,
            sum(dead) / len(dead), sum(good) / len(good) - sum(dead) / len(dead))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("run_dir")
    parser.add_argument("--every", type=float, default=60.0)
    parser.add_argument("--once", action="store_true")
    args = parser.parse_args()
    root, moves, children = build()
    cache, seen = {}, None
    while True:
        snaps = sorted(glob.glob(os.path.join(args.run_dir, "snapshots", "generation-*")))
        if snaps and snaps[-1] != seen:
            seen = snaps[-1]
            rank, total, good, dead, gap = probe(seen, root, moves, children, cache)
            print("%s %s  rank %3d/%d  good circle %+.3f triangle %+.3f  dead %+.3f  gap %+.3f"
                  % (time.strftime("%H:%M:%S"), os.path.basename(seen), rank, total,
                     good[0], good[1], dead, gap), flush=True)
        if args.once:
            return
        time.sleep(args.every)


if __name__ == "__main__":
    main()
