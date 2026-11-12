#!/usr/bin/env python3
"""Follow a candidate-AlphaZero run from its logs, in the terminal.

    python3 euclid_train_watch.py RUN_DIR [--every 30] [--once]

RUN_DIR holds logs/learner*.jsonl and logs/actor-*.jsonl (as written by
euclid_candidate_alpha_zero.py).  Each refresh prints one line: minutes since
the first log line, learner step, policy and value loss (mean of the last 10
steps), games per stage, stage-2 wins, and a sparkline of policy loss.
Read-only: it never touches the run.
"""
import argparse
import glob
import json
import os
import time

BARS = "▁▂▃▄▅▆▇█"


def _rows(pattern):
    out = []
    for path in sorted(glob.glob(pattern)):
        with open(path, encoding="utf-8") as stream:
            for line in stream:
                try:
                    out.append(json.loads(line))
                except json.JSONDecodeError:
                    pass  # a line being written
    return out


def _spark(values, width=40):
    if not values:
        return ""
    step = max(1, len(values) // width)
    sample = [sum(values[i:i + step]) / len(values[i:i + step])
              for i in range(0, len(values), step)][-width:]
    lo, hi = min(sample), max(sample)
    span = (hi - lo) or 1.0
    return "".join(BARS[int((v - lo) / span * (len(BARS) - 1))] for v in sample)


def summary(run_dir):
    learner = [r for r in _rows(os.path.join(run_dir, "logs", "learner*.jsonl"))
               if r.get("event") == "learner_step"]
    games = [r for r in _rows(os.path.join(run_dir, "logs", "actor-*.jsonl"))
             if r.get("event") == "game"]
    times = [r["time"] for r in learner + games if "time" in r]
    start = min(times) if times else time.time()
    last = learner[-10:]
    mean = (lambda key: sum(r.get(key, 0.0) for r in last) / len(last)) if last else (lambda key: float("nan"))
    stages = {}
    wins2 = 0
    for g in games:
        stage = g.get("stage", "?")
        stages[stage] = stages.get(stage, 0) + 1
        if stage == "stage-2" and g.get("value", -1) > 0:
            wins2 += 1
    stage_text = " ".join("%s:%d" % (k, v) for k, v in sorted(stages.items()))
    return ("%5.1f min  step %4d  policy %.3f  value %.4f  games %s  stage-2 wins %d  %s"
            % ((time.time() - start) / 60, learner[-1]["step"] if learner else 0,
               mean("policy"), mean("value"), stage_text or "-", wins2,
               _spark([r.get("policy", 0.0) for r in learner])))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("run_dir")
    parser.add_argument("--every", type=float, default=30.0)
    parser.add_argument("--once", action="store_true")
    args = parser.parse_args()
    while True:
        print(time.strftime("%H:%M:%S"), summary(args.run_dir), flush=True)
        if args.once:
            return
        time.sleep(args.every)


if __name__ == "__main__":
    main()
