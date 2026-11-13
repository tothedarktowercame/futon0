# ArXiv Structure Mining

Last updated: 2026-05-20

This document specifies the current repeatable protocol for improving
arXiv paper structure coverage in `futon6` by combining:

- `Daisychain`: always advance to fresh papers after each audit pass
- `Distributed Proofreaders`: inspect only the residual uncovered
  sentences, not the already-covered majority of the paper
- `Term-Feature Residual Ranking`: use ordinary Stage 5 named-entity
  hits as features on uncovered sentences
- `Structure Seed Aggregation`: normalize those residuals into
  reusable candidate structure signatures

The protocol is designed to stand on its own outside the current
interactive session. A fresh Claude or Codex instance should be able to
follow it directly.

## 1. Goal

The goal is not to perfectly annotate every paper by hand.

The goal is to build a reusable **seed kit of structure patterns** for
Stage 5 discourse detection, then use that seed as the basis for a
future structure-learning stage analogous to Stage 5 NER.

The current protocol therefore treats uncovered sentences with known
mathematical vocabulary as especially valuable. They often indicate
that the content layer is already present while the discourse layer is
still missing.

The stopping rule is therefore:

- improve the general detector by adding only reusable patterns
- validate the lift on fresh papers
- stop once the seed is strong enough that further progress should come
  from learned structure, not continued manual regex expansion

## 2. Required Repos And Data

This protocol assumes:

- repo root: `/home/joe/code/futon6`
- batch archive: `/home/joe/code/storage/mark2/inbox/batch-008.tar.gz`

The protocol uses these checked-in scripts:

- [nlab-wiring.py](/home/joe/code/futon6/scripts/nlab-wiring.py)
- [build-uncovered-sentence-audit.py](/home/joe/code/futon6/scripts/build-uncovered-sentence-audit.py)
- [build-batch-008-qc-viewer.py](/home/joe/code/futon6/scripts/build-batch-008-qc-viewer.py)
- [superpod-job.py](/home/joe/code/futon6/scripts/superpod-job.py)

The protocol also relies on these tests:

- [test_nlab_wiring.py](/home/joe/code/futon6/tests/test_nlab_wiring.py)
- [test_build_uncovered_sentence_audit.py](/home/joe/code/futon6/tests/test_build_uncovered_sentence_audit.py)
- [test_superpod_job_smoke.py](/home/joe/code/futon6/tests/test_superpod_job_smoke.py)

It also relies on the live Stage 5 NER kernel:

- `/home/joe/code/storage/futon6/data/ner-kernel/terms.tsv`

## 3. Current Stage 5 Semantics

At the time of writing, Stage 5 discourse detection in `futon6` means:

- `scopes`: bindings, environments, typed arrows, quantified forms,
  definitional cues
- `wires`: argumentative or discourse connectors such as `since`,
  `therefore`, `next`
- `ports`: anaphoric references such as `this functor`, `the above`
- `labels`: reasoning or strategy markers such as `explicitly`,
  `one might ask`

The combined Stage 5 artifact is:

- `discourse-wiring.json`

Stage 5 also still emits:

- `ner-terms.json`
- `scopes.json`
- optional open-world term outputs such as
  `candidate-new-terms.jsonl` and `learned-term-dictionary.jsonl`

For this protocol, ordinary Stage 5 term hits are used as machine-facing
features on uncovered sentences, even though the human-facing comparison
demo remains scope-first rather than term-first.

The preserved scope-first comparison baseline is:

- [batch-008-scope-baseline.html](/home/joe/code/futon6/data/showcases/batch-008-scope-baseline.html)
- [batch-008-scope-baseline.json](/home/joe/code/futon6/data/showcases/batch-008-scope-baseline.json)

## 4. Core Protocol

### 4.1 Run the uncovered-sentence audit

From `/home/joe/code/futon6`:

```bash
python3 scripts/build-uncovered-sentence-audit.py
```

This does all of the following:

- selects fresh papers not already used in the daisychain ledger
- extracts raw eprint text from `batch-008.tar.gz`
- runs the current Stage 5 discourse detector over those papers
- measures sentence and character coverage
- surfaces only the residual uncovered sentences
- enriches each uncovered sentence with ordinary known-term hits from
  the Stage 5 NER kernel
- builds a normalized `structure_seed_template` and a more abstract
  `structure_seed_signature` for term-dense residuals
- aggregates cross-paper `structure_seed_candidates`
- advances the daisychain ledger

Primary outputs:

- [latest-audit.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/latest-audit.json)
- [latest-audit.html](/home/joe/code/futon6/data/showcases/distributed-proofreaders/latest-audit.html)
- [daisychain-ledger.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/daisychain-ledger.json)

The ledger is the mechanism that enforces freshness across iterations.

### 4.2 Read only the uncovered residuals

Inspect the generated JSON or HTML.

For each paper, look at:

- `discourse_coverage.sentence_coverage`
- `uncovered_sentence_count`
- `uncovered_sentences_with_known_terms`
- `uncovered_sentence_known_term_ratio`
- `top_uncovered_cues`
- `term_dense_uncovered_sentences`
- `uncovered_sentences`

Then also inspect the run-level:

- `structure_seed_candidates`

Do **not** optimize against already-covered sentences.

Do **not** optimize against one paper’s topic.

Instead, ask:

- is this residual a reusable structural form?
- is it term-dense but structurally unrecognized?
- is it a local typography or encoding issue?
- is it domain-specific language that should instead be handled by later
  structure learning?

The first two cases are the priority. The second case is often the best
input to the next structure-learning stage.

### 4.3 Add only general patterns

When patching [nlab-wiring.py](/home/joe/code/futon6/scripts/nlab-wiring.py):

- add only patterns that generalize across papers
- do not add one-off paper-specific text fragments
- prefer broad mathematical discourse constructions such as:
  - `Let \Foo be ...`
  - `Here $x$ denotes ...`
  - short arrow macros like `\ra`
  - sequencing discourse like `Next`, `Finally`
  - question-transition labels like `one might ask`

Every new pattern must get a focused regression test in
[test_nlab_wiring.py](/home/joe/code/futon6/tests/test_nlab_wiring.py).

### 4.4 Extract structure-seed candidates from term-dense residuals

The audit now emits two machine-facing normalizations for each uncovered
sentence:

- `structure_seed_template`: the sentence with known terms replaced by
  `<TERM>` and math/citations normalized
- `structure_seed_signature`: a more abstract cue-word skeleton used for
  cross-paper grouping

These signatures are collected at run level as:

- `structure_seed_candidates`

This is the bridge from manual proofreading to live structure learning.
It gives the next learner stage a concrete starter inventory of
recurrent residual constructions.

When reviewing these candidates:

- ignore one-off verbose signatures unless they are clearly central
- prefer signatures that recur across papers
- prefer signatures whose example sentences are term-dense
- treat them as learner seeds, not as ready-to-deploy regexes

### 4.5 Re-run the same papers without advancing the chain

To measure real lift on the same audited papers, rerun with explicit
paper IDs and no ledger advance.

Example:

```bash
python3 scripts/build-uncovered-sentence-audit.py \
  --paper-id 0711.4904v1 \
  --paper-id 0711.1887v1 \
  --paper-id 0803.0339v1 \
  --out-json data/showcases/distributed-proofreaders/latest-rerun.json \
  --out-html data/showcases/distributed-proofreaders/latest-rerun.html \
  --no-advance-ledger
```

Do the same for any other prior audited set.

This produces a proper before/after comparison on identical texts.

### 4.6 Advance to a fresh paper set

After validating the same-paper lift, run the audit again without
explicit paper IDs.

That should advance the daisychain and select unseen papers.

Example current outputs from this workflow:

- [latest-audit.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/latest-audit.json)
- [latest-rerun.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/latest-rerun.json)
- [next-audit.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/next-audit.json)
- [next-rerun.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/next-rerun.json)
- [third-audit.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/third-audit.json)
- [term-feature-rerun.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/term-feature-rerun.json)
- [term-feature-rerun.html](/home/joe/code/futon6/data/showcases/distributed-proofreaders/term-feature-rerun.html)
- [term-feature-fresh.json](/home/joe/code/futon6/data/showcases/distributed-proofreaders/term-feature-fresh.json)
- [term-feature-fresh.html](/home/joe/code/futon6/data/showcases/distributed-proofreaders/term-feature-fresh.html)

The `*-rerun.json` artifacts are same-paper validation runs.
The `*-audit.json` artifacts are daisychain-advanced fresh runs.
The `term-feature-*` artifacts are enriched runs that include known-term
residual ranking and structure-seed aggregation.

## 5. Reproducibility Checklist

Before claiming improvement, run:

```bash
python3 -m py_compile \
  scripts/nlab-wiring.py \
  scripts/build-uncovered-sentence-audit.py \
  scripts/superpod-job.py \
  tests/test_nlab_wiring.py \
  tests/test_build_uncovered_sentence_audit.py \
  tests/test_superpod_job_smoke.py

pytest -q \
  tests/test_nlab_wiring.py \
  tests/test_build_uncovered_sentence_audit.py \
  tests/test_superpod_job_smoke.py
```

At the time of writing, the current passing state is:

- `99 passed`

For a same-paper improvement claim, compare:

- old `*-audit.json`
- new `*-rerun.json`

using:

- `discourse_coverage.sentence_coverage`
- `uncovered_sentence_count`
- `uncovered_sentences_with_known_terms`
- `structure_seed_candidates`

For a generalization claim, inspect the next fresh daisychain batch.

## 6. Current Example Results

These are not the algorithm. They are current evidence that the
algorithm is working.

Same-paper improvements already observed:

- `0711.4904v1`: `0.7460 -> 0.7831`, uncovered `24 -> 18`
- `0711.1887v1`: `0.6823 -> 0.7188`, uncovered `27 -> 22`
- `0803.0339v1`: `0.9613 -> 0.9654`, uncovered `20 -> 18`
- `0801.4067v1`: `0.8258 -> 0.8356`, uncovered `29 -> 24`
- `0711.0687v1`: `0.9666 -> 0.9700`, uncovered `13 -> 11`

Term-feature enriched same-paper rerun:

- `0711.1739v1`: uncovered `30`, with known terms `30`, ratio `1.0`
- `0711.4904v1`: uncovered `18`, with known terms `12`, ratio `0.6667`
- `0801.4067v1`: uncovered `24`, with known terms `21`, ratio `0.875`

Fresh enriched daisychain batch:

- `0711.1330v1`: coverage `0.9795`, uncovered `4`, with known terms `3`
- `0803.1492v1`: coverage `0.8209`, uncovered `24`, with known terms `12`
- `0711.0898v1`: coverage `0.8722`, uncovered `29`, with known terms `25`

These results matter because they show that many remaining uncovered
sentences are already dense in known mathematics. That makes them strong
candidates for structure learning rather than evidence that the prose is
merely informal English.

One fresh third-batch result currently looks like this:

- `0712.0418v1` (`math.CT`): sentence coverage `0.7895`
- `0801.0350v1` (`math.LO`, `cs.CC`): `0.9164`
- `0711.0334v1` (`math.HO`, `math.GM`, `math.SP`): `0.4230`

That last paper is an important boundary condition:

- it is German
- it has encoding damage
- it demonstrates that manual English-oriented structure seeding has a
  limit

This is one reason the protocol is meant to terminate into
structure-learning rather than endless manual rule growth.

## 7. What Counts As A Good Patch

A good patch:

- improves coverage on the audited papers
- also survives the next daisychain batch
- reflects a recognizable reusable structure
- is backed by a small regression test
- improves the machine-facing residual signal for structure learning

A bad patch:

- hardcodes a phrase from one paper
- only helps the current paper set
- expands regexes in ways that obviously risk false positives
- tries to route around non-English or broken-encoding cases with fragile
  special cases

## 8. When To Stop

Stop manual seed growth when all of these are true:

- the detector is clearly better than the original sparse baseline
- further residuals are increasingly language-specific, encoding-specific,
  or too semantically rich for regexes
- fresh daisychain batches still show reasonable coverage on ordinary
  English mathematical prose
- the remaining gaps look like they want learned structure rather than
  more hand-authored structure cues

At that point, the next step is:

- build the **structure-learning analogue of Stage 5 NER**

That future stage should treat the current manual structure kit as a
starter seed, not as the final system.

In practical terms, that starter seed now includes:

- the manual discourse patterns in `scripts/nlab-wiring.py`
- the term-dense uncovered residuals in the audit artifacts
- the aggregated `structure_seed_candidates` signatures

## 9. Minimal Fresh-Instance Recipe

For a fresh Claude instance, the shortest reliable path is:

1. `cd /home/joe/code/futon6`
2. run the test suite above
3. run `python3 scripts/build-uncovered-sentence-audit.py`
4. inspect `latest-audit.json` and `latest-audit.html`
5. inspect `uncovered_sentences_with_known_terms`,
   `term_dense_uncovered_sentences`, and `structure_seed_candidates`
6. add only general detector patterns in `scripts/nlab-wiring.py`
7. add tests in `tests/test_nlab_wiring.py`
8. rerun the same papers with `--paper-id ... --no-advance-ledger`
9. compare old audit vs rerun metrics
10. run the audit again without overrides to advance the daisychain
11. stop after one or two such passes and move to structure learning

That is the current standalone algorithm.
