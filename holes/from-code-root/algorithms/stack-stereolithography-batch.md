# Algorithm: Stack Stereolithography Batch

Date: 2026-04-26
Status: active
Owner mission: M-stack-stereolithography (futon5a)

## Purpose

Prepare the next bounded scout batch for the candidate invariant queue
without needing an interactive prompt. The batch prep is mechanical:
choose the next lane slice, skip already-scouted families, and emit a
worklist plus machine-readable manifest.

## Inputs

- `futon5a/holes/stack-stereolithography-render-v2.md`
- `futon3c/docs/structural-law-inventory.sexp`
- `futon5a/holes/stack-stereolithography-git-scout-1.md`

## Procedure

### Step 1: Prepare the next batch

```bash
cd /home/joe/code/futon5a
bb scripts/prepare_stack_stereolithography_batch.clj --lane zero-witness --n 10
```

Outputs:

- `futon5a/data/stack-stereolithography-batch.edn`
- `futon5a/holes/stack-stereolithography-batch.md`

### Step 1b: Prepare the full runbook

When the goal is to set up the whole candidate-family surface rather
than only the next bite:

```bash
cd /home/joe/code/futon5a
bb scripts/prepare_stack_stereolithography_runbook.clj
```

Outputs:

- `futon5a/data/stack-stereolithography-runbook.edn`
- `futon5a/holes/stack-stereolithography-runbook.md`
- `futon5a/holes/stack-stereolithography-runs/run-*.md`

### Step 2: Read the worklist

Open the generated markdown worklist. It gives:

- selected families in order
- lane used for each family
- precision proxy and mission-witness counts
- mission ids, witness quality, and actionability gradient
- firm exemplar plus watchlist exemplar / repo / strength / note
- candidate invariants under each family

### Step 3: Work the batch in order

For each selected family:

1. refresh Stream A witness
2. collect a bounded Stream B sample
3. update the scout note
4. classify the family:
   - `promote`
   - `split`
   - `re-keyword`
   - `park`
   - `needs-human-audit`

### Why Stream B is not yet fully automated

The runner is intentionally feature-complete on **Stream A** but only
preparatory on **Stream B**.

What is stable enough to automate:

- lane selection
- family ordering
- mission-feature witness
- inventory/watchlist context
- already-scouted skip logic

What is still judgment-heavy:

- building the right git search lexicon for a family
- deciding which commits are real law-signals rather than mere
  chronology
- classifying commits as workaround / repair / rediscovery / migration /
  expectation-drift / evidence-loss
- deciding whether the result implies `promote`, `split`, `re-keyword`,
  `park`, or a larger substrate move

So Stream B should eventually get a **harvester** script, but not yet a
fully automatic scorer that pretends grep results are strategic truth.

### Step 4: Re-run for the next bite

After the current scout note has been extended, re-run the same batch
prep command. The script auto-detects already-scouted families from the
existing scout note and skips them.

## Current lane policy

For `zero-witness`:

1. take unscouted strict zero-witness candidate families first
2. if the requested batch under-fills, widen to unscouted
   effectively-zero-witness families (`pp=0`, `missions<=1`)

## Decision rules

- If the strict lane under-fills, accept the smaller batch or widen via
  the documented fallback; do not invent extra work items.
- If the generated batch is empty, either switch lanes or add a new
  scout note so the skip logic has something fresh to compare against.
- If a family has already been scouted but materially changed in the
  render or inventory, re-open it manually as a fresh batch item.

## References

- `futon5a/scripts/prepare_stack_stereolithography_batch.clj`
- `futon5a/holes/missions/M-stack-stereolithography.md`
- `algorithms/apm-daily-batch.md`
