#!/usr/bin/env python3
"""Summarise the DESIGN-3 E6a later-Book hindsight run."""
from __future__ import annotations

from collections import Counter
import json
from pathlib import Path
import random

import euclid_hindsight as hindsight
import euclid_hindsight_train as train


def analyse(root: Path, note: Path, sample_seed=2026111206):
    stats = Counter()
    by_prop_stats = {}
    for path in sorted(root.glob("worker-*-stats.json")):
        payload = json.loads(path.read_text())
        stats.update({k: v for k, v in payload.items()
                      if k != "by_proposition"})
        for proposition, values in payload["by_proposition"].items():
            target = by_prop_stats.setdefault(int(proposition), Counter())
            target.update(values)

    records = {p: Counter() for p in range(4, 26)}
    labels = {p: Counter() for p in range(4, 26)}
    book_matches = Counter()
    rng = random.Random(sample_seed)
    samples, eligible_seen = [], 0
    for path in sorted(root.glob("worker-*.jsonl")):
        with path.open() as stream:
            for line in stream:
                row = json.loads(line)
                p = row["proposition"]
                kind = "conjunctive" if row.get("conjunctive") else "single"
                records[p][(kind, row["needed_size"])] += 1
                reasons = row.get("trivial_reasons", [])
                labels[p]["non-trivial" if not reasons else "+".join(reasons)] += 1
                match = train.book_goal_match(row)
                if match is not None:
                    book_matches[(p, match[0])] += 1
                if not reasons:
                    eligible_seen += 1
                    if len(samples) < 10:
                        samples.append(row)
                    else:
                        slot = rng.randrange(eligible_seen)
                        if slot < 10:
                            samples[slot] = row

    lines = ["# DESIGN-3 E6a — hindsight theorems from I.4–I.25 givens", "",
             "Date: 2026-11-12. Label: **ZERO**. Eight workers sampled rules uniformly for six plies. Generation used only each proposition's givens, guarded rule palette and closure; no Book proof moves or target hints were supplied.", "",
             "## Per-proposition yield", "",
             "Record columns are `single/conjunctive` counts grouped by needed-set size.", "",
             "| proposition | runs | seconds/run | records by kind and needed size | triviality labels | slow replays dropped |",
             "|---:|---:|---:|---|---|---:|"]
    for p in range(4, 26):
        ps = by_prop_stats.get(p, Counter())
        run_count = ps["runs"]
        seconds = ps["wall_seconds"] / run_count if run_count else 0
        rec = ", ".join(f"{kind[0]}{size}={count}"
                        for (kind, size), count in sorted(records[p].items())) or "—"
        lab = ", ".join(f"{key}={value}"
                        for key, value in sorted(labels[p].items())) or "—"
        lines.append(f"| I.{p} | {run_count} | {seconds:.2f} | {rec} | {lab} | {ps['dropped_slow_replays']} |")

    lines += ["", "## Book-goal matches", "",
              f"Across all records, {sum(book_matches.values())} state a Book goal among I.1–I.25.", "",
              "| source givens | matched Book goal | records |",
              "|---:|---:|---:|"]
    for (source, book), count in sorted(book_matches.items()):
        lines.append(f"| I.{source} | I.{book} | {count} |")
    if not book_matches:
        lines.append("| — | — | 0 |")

    lines += ["", "## Ten sampled non-trivial theorems", "",
              f"Reservoir sample seed: `{sample_seed}`.", ""]
    for index, row in enumerate(samples, 1):
        needed = set(row["needed_move_indices"])
        moves = [move["notation"] for i, move in enumerate(row["moves"])
                 if i in needed]
        replay = hindsight.verify_record(row)
        lines.append(f"{index}. I.{row['proposition']}, needed {row['needed_size']}, fresh replay **{'yes' if replay else 'NO'}**: "
                     f"`{'`; `'.join(moves)}`. Goal: `{row['goal']}`.")

    lines += ["", "## Run receipt", "",
              f"- Runs completed: {stats['runs']}; records: {stats['records']}.",
              f"- Slow needed-set replays dropped at the 120-second cap: {stats['dropped_slow_replays']}.",
              f"- Aggregate worker wall time: {stats['wall_seconds']:.1f} seconds.",
              "- Output: `/home/joe/storage/euclidzero/d3-e6a/`."]
    note.parent.mkdir(parents=True, exist_ok=True)
    note.write_text("\n".join(lines) + "\n")


def main():
    analyse(Path("/home/joe/storage/euclidzero/d3-e6a"),
            Path(__file__).parents[1] /
            "holes/excursions/E-euclid-tutor-data/spiel/D3-E6A.md")


if __name__ == "__main__":
    main()
