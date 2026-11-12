#!/usr/bin/env python3
"""Summarise DESIGN-3 E4a single and conjunctive hindsight records."""
from __future__ import annotations

import argparse
import collections
import glob
import json
from pathlib import Path
import random

import euclid_certify as certify
import euclid_hindsight as hindsight
import euclid_hindsight_filter as filters
import euclid_play as play


def _bucket(size):
    return str(size) if size in (2, 3) else "4+"


def _auxiliary_table(records):
    table = {}
    for kind in ("single", "conjunctive"):
        selected = [r for r in records
                    if ("conjunctive" if r.get("conjunctive") else "single") == kind]
        rows = {}
        for size in ("2", "3", "4+"):
            group = [r for r in selected if _bucket(r["needed_size"]) == size]
            with_aux = sum(bool(r.get("auxiliary_count")) for r in group)
            rows[size] = {"records": len(group), "with_auxiliary": with_aux,
                          "share": with_aux / len(group) if group else 0.0}
        table[kind] = rows
    return table


def _demo_auxiliary():
    result = {}
    sequences = {1: certify.PROP1_BOOK_STEPS,
                 2: certify.PROP2_BOOK_STEPS,
                 3: certify.PROP3_BOOK_STEPS}
    for proposition, sequence in sequences.items():
        moves = [(rule, tuple(args), tuple(sorted(bindings.items())))
                 for rule, args, bindings in sequence if bindings]
        goals = tuple(tuple(goal) for goal in play.GOALS[proposition])
        aux = hindsight.auxiliary_moves(moves, range(len(moves)), goals)
        result[f"I.{proposition}"] = {
            "construction_moves": len(moves), "auxiliary_moves": len(aux),
            "auxiliary_indices": aux,
            "share": len(aux) / len(moves), "label": "DEMO",
        }
    return result


def analyze(inputs, output, sample_seed=2026111204):
    cache = {}
    singles, pairs = {}, {}
    raw = collections.Counter()
    for path in sorted(glob.glob(inputs)):
        for line in open(path):
            record = json.loads(line)
            raw["records"] += 1
            identity = (record["proposition"], record["canonical_key"])
            if record.get("conjunctive"):
                raw["conjunctive"] += 1
                if not record.get("trivial_reasons"):
                    pairs.setdefault(identity, record)
                continue
            raw["single"] += 1
            if record["goal"][0] in ("pos_len", "neq"):
                continue
            if not filters.classify(record, cache):
                singles.setdefault(identity, record)
    retained = list(singles.values()) + list(pairs.values())
    rng = random.Random(sample_seed)
    sample = rng.sample(list(pairs.values()), min(10, len(pairs)))
    examples = []
    for record in sample:
        needed = set(record["needed_move_indices"])
        examples.append({
            "proposition": record["proposition"],
            "seed": record["run_seed"],
            "moves": [move["notation"] for i, move in enumerate(record["moves"])
                      if i in needed],
            "goals": record["goal"],
            "needed_size": record["needed_size"],
            "auxiliary_count": record["auxiliary_count"],
            "verified": hindsight.verify_record(record),
        })
    pair_sizes = collections.Counter(_bucket(r["needed_size"])
                                     for r in pairs.values())
    payload = {
        "raw": dict(raw),
        "distinct_single_nontrivial": len(singles),
        "distinct_conjunctive_nontrivial": len(pairs),
        "conjunctive_by_size": dict(pair_sizes),
        "conjunctive_i1_shape": sum(r.get("i1_shape", False)
                                    for r in pairs.values()),
        "auxiliary": _auxiliary_table(retained),
        "demo_auxiliary": _demo_auxiliary(),
        "examples": examples,
        "examples_verified": sum(row["verified"] for row in examples),
    }
    Path(output).write_text(json.dumps(payload, indent=2) + "\n")
    print(json.dumps(payload, sort_keys=True))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--inputs", required=True)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()
    analyze(args.inputs, args.output)


if __name__ == "__main__":
    main()
