# Agent → operator reply pilot, 2026-09-21

Exploratory instrument development only. Full text, stored stance labels, recomputable embeddings, and a blind calibration exercise. No operational decisions or causal claims.

Notebook: https://zone.hyperreal.enterprises/marimo/?file=chat-operator-reply-pilot-20260921.py (authenticated). The business-ideas notebook links to it. Joe’s 20-pair exercise is first; analysis is hidden by default.

## Extraction and sample

Read the prior pilot `futon2/holes/labs/wm-contract/PILOT-redirection-geometry-2026-09-20.md` first. This study compares assistant-final/operator-reply pairs, not consecutive operator turns or top-three pattern labels.

Cutoff: **2026-09-21T17:12:00Z**. Read-only union of `~/.claude/projects/*/<session-id>.jsonl*`, including pre-compact snapshots. Deduplicate user/assistant messages by UUID; prefer original text over compacted placeholders, then longest text, then deterministic first path. Manifest records source paths, sizes, hashes, conflicting UUIDs, and exclusion-row locations. No 100-character evidence-store queries were used.

Only envelopes with both `Origin: operator` and `From: joe` qualify. Remove the outer envelope and Agent/User-message wrapper. Resume markers and wake checklists take precedence over that origin. Exclusion classes below are mutually exclusive, not an estimate of human turns. Tool-result rows and automatic messages are counted separately. Seven notebook-surface inputs are excluded because their displayed predecessor cells cannot be recovered reliably from the transcript, including two automatic agent-switch replays.

Pair with the latest nonempty, non-sidechain assistant `end_turn`, excluding tool-use blocks, provided it occurs after the previous substantive input. Fifteen operator inputs lack such a final and are not paired. This records temporal adjacency, **not a browser read receipt or proof that Joe answers that exact final**; some replies refer to earlier proposals or another thread. No raw thinking or tool text enters the measures.

Take chronological prefixes, round-robin allocating up to 100: **43 / 43 / 14**. Prefixes keep all earlier eligible corrections in the labelled set. This is session-stratified early-turn sampling, not a random or representative whole-session sample. The blind set alone is seeded random sampling: Python `random.Random(20260922).sample`, 20 without replacement.

| Session | ID | Files | Eligible | Sample | First eligible | Last eligible |
|---|---|---:|---:|---:|---|---|
| claude-12 | d158cebc-06aa-4763-8704-e216a5a39f5c | 63 | 158 | 43 | 2026-09-18T02:26:18.134Z | 2026-09-21T06:05:12.897Z |
| claude-4 | af24caa1-54d3-4f19-9d73-c8183eb9cb65 | 160 | 240 | 43 | 2026-09-15T21:37:16.786Z | 2026-09-20T19:57:36.531Z |
| claude-5 | de4c2047-bf32-4b18-bd55-8f97e94c6252 | 1 | 14 | 14 | 2026-09-21T15:21:22.440Z | 2026-09-21T17:07:16.086Z |

| Excluded class | claude-12 | claude-4 | claude-5 |
|---|---:|---:|---:|
| agent_bell | 90 | 260 | 1 |
| harness_housekeeping | 12 | 26 | 0 |
| notebook_display_unverified | 0 | 7 | 0 |
| operator_without_preceding_final | 8 | 7 | 0 |
| park_resume_marker | 179 | 228 | 5 |
| tool_result_or_empty | 1821 | 4880 | 62 |
| untyped_or_housekeeping | 11 | 39 | 0 |
| wake_checklist | 1 | 0 | 0 |

There are **412 resume-marker exclusions (179 / 228 / 5)**, plus one wake-checklist exclusion. The earlier SOURCES report’s 3,024 resume rows came from a different, broader evidence-store population and is not this sample’s denominator. UUID text conflicts occur for 47 / 133 / 0 IDs; compaction variants explain why a union must prefer full text.

## Frozen stance input

Labeller: **codex-14**. Manual single-labeller judgments using full operator replies and assistant opening/ending excerpts, expanded where needed. No automatic stance classifier was used. `stance_labels.csv` stores pair ID, primary, optional secondary (blank in this pilot), labeller and a one-line rationale. All six categories retain the supplied wording in the notebook. Distinguish a content-free go-on from agreement with a particular proposal; mixed replies get the dominant request. Clarifications and status questions fit this taxonomy poorly, so some redirect/new/amend choices are provisional. Joe’s blind labels are needed for calibration.

| Session | n | continue | accept | amend | redirect | reject | new |
|---|---:|---:|---:|---:|---:|---:|---:|
| claude-12 | 43 | 1 | 5 | 11 | 16 | 6 | 4 |
| claude-4 | 43 | 0 | 10 | 12 | 7 | 8 | 6 |
| claude-5 | 14 | 0 | 1 | 3 | 6 | 0 | 4 |

## Recomputable semantic measures

Encoder: `sentence-transformers/all-MiniLM-L6-v2`, same name and normalized embeddings as `futon3a/scripts/embed_text.py`. Baseline resolved revision `1110a243fdf4706b3f48f1d95db1a4f5529b4d41`, dimension 384. The JSON records package versions, input hashes and script hash. It embedded 5,519 unique chunks from 330 retained dialogue rows up to the last sampled reply in each session.

Contribution: split at sentence/newline boundaries, subdivide using tokenizer offsets into at most 254 content tokens for this encoder (256 including special tokens). Normalize each embedding. For each operator unit take maximum cosine against **all strictly earlier retained dialogue units**, including the paired agent final, all other earlier final assistant texts, and verified operator texts including orphan operator inputs. Thinking, tool blocks, incoming bells and housekeeping are excluded. This is the observed retained session, not off-screen context; notebook operator inputs are also missing. The paired-agent-only maxima are retained separately in `metrics-minilm.json` for future alternatives.

Contribution(t) = number of operator units with max cosine < t / number of operator units. Within a turn each unit gets equal weight; aggregate means weight turns equally. The current operator and future rows are excluded. A normalized mean of a long turn’s chunks is used for repeated correction; no tail is silently truncated.

Repeated correction: for redirect/reject primary labels, maximum cosine of that normalized mean to **any earlier eligible redirect/reject in the same session**. First corrections have null scores, not zero. Store the closest prior pair ID. Two pairs cross the default 0.80 outline threshold. Repeated topic or generic vocabulary can explain a high value; it is not proof of a repeated unheeded instruction.

### Instrument finding: contribution saturates

At t=0.75, even accept and continue average 1.00. This proxy is measuring different wording and speech acts, not establishing substantive novelty or creativity. It cannot support a useful ranking of stance categories. Negation, boilerplate, short acknowledgments, chunks and changing discourse roles all need calibration. This negative finding is part of the pilot.

| Stance | n | t=.55 | t=.65 | t=.75 | t=.85 |
|---|---:|---:|---:|---:|---:|
| continue | 1 | 0.667 | 1.000 | 1.000 | 1.000 |
| accept | 16 | 0.500 | 0.833 | 1.000 | 1.000 |
| amend | 26 | 0.651 | 0.886 | 0.941 | 0.955 |
| redirect | 29 | 0.610 | 0.828 | 0.971 | 0.990 |
| reject | 14 | 0.517 | 0.801 | 0.844 | 0.987 |
| new | 14 | 0.540 | 0.811 | 0.883 | 0.960 |

## Blind exercise and recomputation

20 seeded pairs show assistant excerpts (expandable full text), full operator replies and six-category dropdowns. No model labels or rationales appear in the exercise. Save Joe’s choices after pair 20; partial saves and revisions are supported. Only explicit submissions write `/home/joe/.local/share/futon-audits/operator-reply-pilot-2026-09-21/joe_labels.csv`, using atomic merge under a lock. This file is not a generated label set and is not committed. No Joe labels were entered during testing. Agreement says **awaiting labels** until all 20 exist; reveal analysis to see a confusion matrix (Codex rows, Joe columns) and Cohen’s kappa. Degenerate expected agreement gives undefined kappa.

Frozen: extraction, sample, blind selection, manual labels and baseline cosine outputs. Live: thresholds, contribution shares, correction outlines, plots and submitted-label agreement. On explicit encoder-form submission: the futon3a Python subprocess recomputes every embedding measure for the selected model/revision and redraws dependent views. Change/clear the revision when changing model. No stance labels are recomputed. Input hashes prevent silently mixing altered labels/text with old baseline metrics.

## August 30

All recovered pairs are September 15 or later; sampled pairs are therefore pre=0, post=43/43/14. Every time-series plot includes the August 30 facade-discovery marker, and the operator table states that a pre/post comparison is not estimable. Do not treat absent pre-period observations as zero operator activity. The existing commit CSV supplies the separate both-sides comparison below the pilot, with counts and daily rates for the 40 pre and 23 post days. It is a frozen descriptive comparison with a partial last day, not evidence of causation.

## Reproduction and validation

From `/home/joe/code`:

```sh
python3 futon0/analysis/audits/operator-reply-pilot-2026-09-21/extract_pairs.py --cutoff 2026-09-21T17:12:00Z
# Preserve/review the frozen manual stance CSV when rebuilding the sample.
futon3a/.venv/bin/python futon0/analysis/audits/operator-reply-pilot-2026-09-21/measure.py --revision 1110a243fdf4706b3f48f1d95db1a4f5529b4d41 --output futon0/analysis/audits/operator-reply-pilot-2026-09-21/metrics-minilm.json
python3 futon0/analysis/audits/operator-reply-pilot-2026-09-21/render.py
HF_HUB_OFFLINE=1 futon3a/.venv/bin/python futon0/analysis/audits/operator-reply-pilot-2026-09-21/test_pilot.py
cd marimo-zone
.venv/bin/python -m pytest -q tests/test_operator_reply_pilot.py
.venv/bin/marimo check --strict notebooks/chat-operator-reply-pilot-20260921.py notebooks/chat-business-ideas-20260921.py
```

Validation: five extraction/measurement tests, including the real MiniLM tokenizer/encoder on an over-limit sentence and exact-repeat input; five UI/storage/chart tests including invalid-write rejection, known kappa and all break markers. Initial notebook executed headlessly. The notebook helper’s explicit recompute path at the pinned model revision reproduced all 100 metric rows exactly. Authenticated browser returned HTTP 200, rendered the blind exercise and analysis, and changing the threshold from .75 to .76 updated the chart. Labels remain awaiting Joe. Notebook edits after browser opening used code mode and saved cells were verified on disk.

Files: full eligible pairs and retained dialogue; sampled pairs; manifest with per-row provenance/exclusions; frozen stance CSV; blind IDs; measured JSON and CSV; three SVGs; extractor, measurement, renderer and tests. Python implementation only. Transcripts and evidence stores were read-only. No futon1b deep-health request was made.
