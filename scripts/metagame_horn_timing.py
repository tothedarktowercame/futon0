#!/usr/bin/env python3
"""HORN-3 fixed-run classify timing helper."""

import argparse
import json
import statistics
import time
from pathlib import Path

import metagame_horn as horn
import metagame_horn_setter as setter


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=("old", "new"), required=True)
    parser.add_argument("--givens", type=int, required=True)
    parser.add_argument("--k", type=int, required=True)
    parser.add_argument("--runs", type=int, default=10)
    parser.add_argument("--out", type=Path, required=True)
    args = parser.parse_args()
    if args.mode == "old":
        horn.needed = horn.needed_by_replay
    times, records = [], 0
    started = time.monotonic()
    for index in range(args.runs):
        one = time.monotonic()
        found, _closures = setter.classify_run(
            args.givens, args.k, 2026111200 + args.givens * 1000 + args.k * 100 + index)
        times.append(time.monotonic() - one); records += len(found)
    result = {"mode": args.mode, "givens": args.givens, "K": args.k,
              "runs": args.runs, "elapsed": time.monotonic() - started,
              "median_seconds": statistics.median(times), "records": records,
              "run_seconds": times}
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    print(json.dumps({k: v for k, v in result.items() if k != "run_seconds"},
                     sort_keys=True))


if __name__ == "__main__":
    main()
