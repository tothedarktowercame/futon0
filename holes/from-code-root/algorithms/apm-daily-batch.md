# Algorithm: APM Daily Batch — Five Problems Through the Two-Agent Pipeline

Date: 2026-04-09
Status: active
Owner mission: M-apm-solutions (futon3c)

## Purpose

Process 5 APM prelim problems per day through the two-agent pipeline:
Claude produces informal proofs + Lean theorem statements + Mathlib
cross-references, then Codex attempts to close the sorry in the Lean
statements. The output accumulates into a LeanDojo-ready corpus for
the superpod.

Canonical storage rule:

- `data/apm-informal-proofs/` is transient batch output
- `apm-lean/problems/<problem-id>/` is the canonical handoff surface
- new batch output must not be written to `ApmCanaries/Frames/` or
  `lean-proofs/`

This algorithm assumes the unified JVM server is running (`make dev`
in futon3c) with both claude-1 and codex-1 registered as agents.

## Prerequisites

1. JVM server running: `make dev` in `/home/joe/code/futon3c/`
2. Agents registered: claude-1 and codex-1 visible in the agents window
3. Drawbridge accessible: `curl -s -H "x-admin-token: $(cat .admintoken)" -d '(+ 1 1)' http://localhost:6768/eval` returns `{:ok true, :value 2}`
4. apm-lean repo accessible: `/home/joe/code/apm-lean/` with `lake build` working

## Inputs

- `storage/apm/manifest.edn` — the 489-problem manifest
- `data/apm-informal-proofs/` — already-processed problems (skipped)
- `data/apm-formalization-patterns.edn` — pattern library (injected into prompts)
- `apm-lean/problems/` — canonical APM bundle corpus
- `apm-lean/docs/apm-problem-bundle-spec.md` — required on-disk bundle shape

## Procedure

### Step 1: Load the v3 conductor (once per JVM session)

Via Drawbridge:

```clojure
(do (remove-ns 'futon3c.dev.apm-conductor-v3)
    (load-file "dev/futon3c/dev/apm_conductor_v3.clj")
    :loaded)
```

Verify: `(futon3c.dev.apm-conductor-v3/batch-status)` returns `:not-running`.

### Step 2: Pick the next 5 problems

Check what's done:

```bash
ls data/apm-informal-proofs/*.md | wc -l
# Subtract empties:
find data/apm-informal-proofs/ -name '*.md' -size +100c | wc -l
```

The batch runner auto-skips completed files. If there are empty
placeholders from failed prior runs, delete them first:

```bash
find data/apm-informal-proofs/ -name '*.md' -empty -delete
```

### Step 3: Run the Claude batch (5 problems)

The batch runner auto-skips already-done files and picks the next N
unprocessed problems:

```clojure
(futon3c.dev.apm-conductor-v3/batch! :agent-id "claude-1" :n 5)
```

This dispatches problems sequentially. Each problem gets the
informal-proof + Lean-statement + Mathlib-cross-reference prompt.
No tool use, no Lean compilation, all inline. Expect ~2 minutes
per problem.

**Monitor progress:**

```clojure
(let [b (deref (var-get (resolve 'futon3c.dev.apm-conductor-v3/!batch)))]
  (select-keys b [:current-problem-id :done-count :total]))
```

**Stop after 5 problems** (or let it run if you have budget):

```clojure
(futon3c.dev.apm-conductor-v3/batch-stop!)
```

### Step 4: Verify Claude's output

Quick quality check on today's files:

```bash
for f in $(ls -t data/apm-informal-proofs/*.md | head -5); do
  id=$(basename "$f" .md)
  size=$(wc -c < "$f")
  sorry=$(grep -c 'sorry' "$f")
  refs=$(grep -oE '`[A-Z][a-zA-Z]+\.[a-zA-Z_]+`' "$f" | wc -l)
  echo "$id: ${size}B, ${sorry} sorry, ${refs} Mathlib refs"
done
```

Expected per file:
- Size: 5-13KB
- Sorry count: 3-10 (theorem statements, all `sorry`-bodied)
- Mathlib refs: 8-20

If any file is <100 bytes, delete it and re-run for that problem.

### Step 5: Normalize Claude's output into an APM bundle

For each of today's 5 problems, convert the raw Claude markdown into
the canonical `apm-lean/problems/<problem-id>/` bundle shape before
handing anything to Codex.

#### Step 5a: Choose the target bundle path

- If `apm-lean/problems/<problem-id>/` does not exist, create that
  directory as the canonical bundle root.
- If `apm-lean/problems/<problem-id>/` already exists and today's work
  is a distinct new attempt, create a new candidate bundle under:
  `apm-lean/problems/<problem-id>/candidates/daily-batch-<YYYYMMDD>-claude-1-codex-1/`
- Do not overwrite an existing canonical bundle in place unless you are
  explicitly continuing that exact bundle.

#### Step 5b: Populate the required files

Each new bundle or candidate must contain at least:

```text
problems/<problem-id>/
  problem.tex
  problem.md
  informal-solution.md
  proof-outline.md
  status.json
  lean/
    Main.lean
```

Populate them as follows:

- `problem.tex`: copy `apm-lean/apm/<problem-id>.tex` when available
- `problem.md`: concise problem statement plus provenance for this batch
- `informal-solution.md`: Claude's worked mathematical solution in prose
- `proof-outline.md`: theorem statement, dependency plan, Mathlib refs,
  and current blockers for formalization
- `lean/Main.lean`: extract Claude's Lean code block here
- `status.json`: record source batch date, agent ids, bundle path,
  sorry counts, and classification (`partial` or `complete`)

The raw Markdown file in `data/apm-informal-proofs/` is a batch trace,
not the canonical deliverable.

#### Step 5c: Extract Lean into the bundle

Minimal extraction example:

```bash
for f in $(ls -t data/apm-informal-proofs/*.md | head -5); do
  id=$(basename "$f" .md | sed 's/^apm-//')
  root=/home/joe/code/apm-lean/problems/$id   # or the candidate path chosen in Step 5a
  mkdir -p "$root/lean"
  sed -n '/^```lean$/,/^```$/p' "$f" | sed '1d;$d' > "$root/lean/Main.lean"
done
```

After extraction, fill in the rest of the bundle files before Codex
starts cleanup.

### Step 6: Hand off to Codex for sorry closure

For each problem, dispatch to codex-1 via the v3 stepper with a
narrow mechanical spec:

```clojure
(futon3c.dev.apm-conductor-v3/start!
  :agent-id "codex-1"
  :problem-id "<problem-id>")
```

Then continue with the cleanup instruction:

```clojure
(futon3c.dev.apm-conductor-v3/continue! "codex-1"
  "The Lean file at <bundle-root>/lean/Main.lean
   has N sorry. Close as many as you can. Preserve the problem bundle shape:
   update proof-outline.md and status.json if the Lean state changes materially.
   Use exact?, apply?, simp, field_simp, nlinarith. If a Mathlib lemma doesn't
   exist, write it locally. Don't create disconnected handoff files. Run
   lake build when appropriate.")
```

Alternatively, reference the Codex handoff protocol:

```bash
cat /home/joe/code/futon3c/holes/CODEX-HANDOFF-apm-cleanup.md
```

### Step 7: Record results

After Codex finishes each problem, record the outcome:

```bash
echo "{:problem-id \"$id\" :date \"$(date -I)\" :bundle-root \"<bundle-root>\" :claude-sorry N :codex-closed M :remaining R}" \
  >> data/apm-codex-cleanup-log.edn
```

Also update the bundle-local `status.json` so the on-disk problem bundle
remains self-describing without consulting the futon3c log.

### Step 8: Commit and push

```bash
cd /home/joe/code/futon3c
git add data/apm-informal-proofs/ data/apm-codex-cleanup-log.edn
git commit -m "APM daily batch: <ids> (N informal + M Lean improvements)"
git push origin master

cd /home/joe/code/apm-lean
git add problems/
git commit -m "APM Lean bundles: <ids> (M sorry closed by Codex)"
git push origin master
```

## Decision Rules

- **If Claude returns an empty file**: delete it and re-run. The batch
  runner will retry. If it fails twice, skip the problem and note it.
- **If Codex writes a handoff document instead of closing sorry**:
  re-dispatch with "Don't write a handoff. Close the admits." If it
  does it again, skip Codex for this problem and note it as a
  LeanDojo target instead.
- **If a problem has no Mathlib cross-references**: the theorem
  likely requires custom Mathlib extensions. Flag it as `:hard` lane
  for future M-diagramprover work.
- **If usage limit is hit mid-batch**: the batch runner stops
  naturally. Resume next day — it auto-skips completed files.

## Outputs

Per daily run:
- 5 new files in `data/apm-informal-proofs/apm-<id>.md`
- 5 new or updated bundle roots in `apm-lean/problems/<id>/`
  or `apm-lean/problems/<id>/candidates/<candidate-id>/`
- 0-5 updated `lean/Main.lean` files inside those bundles with some
  sorry closed
- 5 entries in `data/apm-codex-cleanup-log.edn`

## Stop Conditions

- All 489 problems processed (informal proofs complete)
- Usage budget exhausted for the week (resume next window)

## Cadence

5 problems/day × 7 days/week = 35/week.
489 problems / 35 per week ≈ 14 weeks to complete.

With usage budget constraints (~100 problems per weekly window),
realistic completion: 5-6 weeks.

## References

- `futon3c/dev/futon3c/dev/apm_conductor_v3.clj` — the v3 batch runner
- `futon3c/holes/CODEX-HANDOFF-apm-cleanup.md` — Codex cleanup protocol
- `apm-lean/docs/apm-problem-bundle-spec.md` — canonical bundle shape
- `futon3c/holes/missions/M-apm-solutions.md` — parent mission
- `futon3c/holes/missions/M-diagramprover.md` — pattern-driven proof search
