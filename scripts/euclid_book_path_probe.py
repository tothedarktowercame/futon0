#!/usr/bin/env python3
"""DEMO diagnostic: how a scorer ranks the moves of Euclid's own proof.

    ~/.venvs/openspiel/bin/python euclid_book_path_probe.py --snapshot MODEL \
        --props 4 9 10 11 12 13 15 [--temperature 1.5] [--out FILE]

For each proposition, start from its givens and walk the engine's reference
proof (euclid_play.reference_steps). At each step, list the legal candidates,
find the Book move among them (same rule and arguments), and record its rank
and probability under the scorer (softmax of logits / T), then apply the Book
step with its own names. Stops at the first case-split step or the first Book
step that is not among the candidates. Used only to measure scorers, never to
guide a search (D3-E5-ASSESS.md).
"""
import argparse
import json
import math

import numpy as np

import euclid_candidate_model as models
import euclid_hindsight_prove as prove
import euclid_hindsight_train as train
import euclid_play

CASE = ("by_contra", "by_cases")


def probe(model, prop, temperature):
    state = train._state(prop)
    rows, path_cost, stop = [], 0.0, "end"
    for k, step in enumerate(euclid_play.reference_steps(prop)):
        rule = step[0]
        if rule in CASE:
            stop = f"case split at step {k}"
            break
        args = tuple(step[1])
        bindings = tuple(sorted(dict(step[2]).items())) if len(step) > 2 else ()
        legal = train.candidates(state)
        hits = [i for i, a in enumerate(legal) if a[0] == rule and tuple(a[1]) == args]
        if not hits:
            stop = f"step {k} {rule}{args} not among {len(legal)} candidates"
            break
        if model is None:
            scores = np.zeros(len(legal))
        else:
            scores = prove._model_scores(model, state, legal)
        costs = prove._softmax_costs(scores, temperature)
        order = np.argsort(-scores, kind="stable")
        rank_of = np.empty(len(order), dtype=int)
        rank_of[order] = np.arange(1, len(order) + 1)
        best = min(hits, key=lambda i: rank_of[i])
        path_cost += float(costs[best])
        rows.append({"step": k, "move": f"{rule}{args}", "candidates": len(legal),
                     "rank": int(rank_of[best]), "page": int((rank_of[best] - 1) // 10 + 1),
                     "p": float(math.exp(-costs[best])), "cost": float(costs[best])})
        state.apply_action_tuple((rule, args, bindings))
    return {"prop": prop, "steps": rows, "stop": stop, "path_cost": path_cost,
            "goal_reached": bool(stop == "end" and state.goal_reached())}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--snapshot")
    ap.add_argument("--generation0", action="store_true")
    ap.add_argument("--props", type=int, nargs="+", required=True)
    ap.add_argument("--temperature", type=float, default=1.0)
    ap.add_argument("--out")
    a = ap.parse_args()
    model = models.CandidateModel(seed=2026111202)
    if a.snapshot:
        models.load_plain_arrays(model, a.snapshot)
    results = [probe(model, p, a.temperature) for p in a.props]
    for r in results:
        print(f"I.{r['prop']}: {len(r['steps'])} steps scored, stop: {r['stop']}, "
              f"goal reached: {r['goal_reached']}, path cost {r['path_cost']:.2f}")
        for s in r["steps"]:
            print(f"   {s['step']:2d} {s['move'][:70]:70s} rank {s['rank']:5d}/{s['candidates']:<5d} "
                  f"page {s['page']:3d}  p={s['p']:.4f}")
    if a.out:
        with open(a.out, "w") as f:
            json.dump(results, f, indent=1)


if __name__ == "__main__":
    main()
