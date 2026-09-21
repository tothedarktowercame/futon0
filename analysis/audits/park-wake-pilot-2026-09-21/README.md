# Park/wake coordination cost — 2026-09-21

[Live authenticated notebook](https://zone.hyperreal.enterprises/marimo/?file=chat-park-wake-pilot-20260921.py), linked from Business ideas and assigned to codex-14.

## Headline

Window **2026-09-07 through 2026-09-21**, inclusive; cutoff **2026-09-21T17:40:06.024923Z**. **26,808 logical API messages, 3,013 cost-bearing turns, 112 sessions, 829 wakes.** A wake can include real review or implementation; all its usage is not pure coordination overhead. No price weighting or plan-limit conversion is assumed.

| Component | Recorded tokens | wake | bell-in | operator | other |
| --- | --- | --- | --- | --- | --- |
| input_tokens | 100,303 | 22.66% | 11.60% | 41.81% | 23.93% |
| cache_read_input_tokens | 7,310,282,099 | 27.84% | 14.50% | 37.46% | 20.19% |
| cache_creation_input_tokens | 89,963,334 | 13.02% | 23.41% | 46.55% | 17.01% |
| output_tokens | 24,436,109 | 21.42% | 14.45% | 40.41% | 23.71% |

## Accounting unit and coverage

UUID deduplication alone overcounts: multiple content-block rows have distinct UUIDs but share one API `message.id` and repeat its complete usage object. We union snapshots by UUID, then count each logical API message once (UUID fallback when no ID). All blocks remain available for tool/text detection. Repeated API usage objects are identical in this snapshot. The notebook also shows the literal UUID-row sum and its trigger shares.

| Component | API-message sum | UUID-row sum | Inflation |
| --- | --- | --- | --- |
| input_tokens | 100,303 | 277,638 | 2.768× |
| cache_read_input_tokens | 7,310,282,099 | 15,020,028,045 | 2.055× |
| cache_creation_input_tokens | 89,963,334 | 233,639,353 | 2.597× |
| output_tokens | 24,436,109 | 61,691,690 | 2.525× |

There are 91 repeated API blocks crossing an intervening user boundary across the full archive. Primary accounting keeps an API message with the trigger before its first block; literal accounting gives each row its contemporaneous trigger. No cross-session API duplicate occurs in the default window. Missing assistant usage, missing token components, invalid JSON lines and sidechain occurrences are all zero in the extracted archive.

The frozen union covers **442 sessions, 8,817 turns, 74,386 API messages**. Earliest recovered row: **2026-08-20T21:30:14.628Z**, earlier than the packet's approximate August 22 date. Scope is `~/.claude/projects/*/*.jsonl*`, including `.pre-compact-*`; nested subagent files, deleted files and activity on other machines/accounts are outside that scope. `manifest.json` records source paths, line counts, hashes and UUID conflicts. Prefer original message text over compacted placeholders, then the more complete message object.

A turn starts at a user message other than a tool result; tool-result placeholders do not split it. Usage filtering uses API-message timestamps by UTC date. Boundary turns may be clipped and the last day is partial. Action flags describe the full observed parent turn, so some actions may lie outside its clipped cost window. No missing usage is inferred.

## Trigger rules

Ordered rules are editable in the notebook. Resume-marker lines are retained, but appended dependency-result transcripts are removed from rule text. Origin/identity rules search the current envelope only, not quoted body text.

| Priority | Class | Rule |
| --- | --- | --- |
| 1 | wake | Resume marker, or wake/checklist/deadline wording at start of body |
| 2 | bell-in | Origin: agent |
| 3 | operator | Origin: operator OR From: joe |
| 4 | other | Fallback, including harness-origin auto-bellbacks |

Exact regexes are in `rules.json`. The notebook shows other-envelope groups and the top 20 trigger prefixes for refinement. Cost-bearing turns: wake 829, bell-in 532, operator 1,104, other 548.

| Other origin | Surface | Turns |
| --- | --- | --- |
| harness | auto-bellback | 281 |
| <absent> | <absent> | 267 |

## What wakes did

These are conservative observable proxies, not a usefulness classifier. Executed Bash command tokens identify agency_send, POST park and POST invoke requests. Heredoc payloads, echoed commands and endpoints inside JSON prompt strings are not executed actions. Dispatch flags are **attempts**, not proof of success. Non-error tool results containing park IDs supply confirmed-ID observations. Arbitrary Python clients, wrappers and unfamiliar tools are not completely covered.

| Wake indicator (overlaps allowed) | Count | Share |
| --- | --- | --- |
| dispatched_work | 398 | 48.01% |
| reported_final_text | 807 | 97.35% |
| only_status_checks | 0 | 0.00% |

Final text is not proof that Joe received/read it. Strict status-only requires every tool to be a recognized read-only job check, no park/work dispatch, and at most 600 characters of final text. Its zero matches are a **lower bound under an incomplete detector**, not proof that no wakes merely poll. Exclusive buckets: 398 dispatch; 418 final-without-dispatch; 13 other/no-final; 0 strict status-only. Final-without-dispatch can include substantial work.

**Dispatch side:** 164 operator/bell turns attempted a park. These are their full-turn costs, not the isolated cost of issuing park:

| Component | Flagged-turn tokens | Share of all tokens |
| --- | --- | --- |
| input_tokens | 7,638 | 7.61% |
| cache_read_input_tokens | 384,047,224 | 5.25% |
| cache_creation_input_tokens | 4,933,145 | 5.48% |
| output_tokens | 1,619,236 | 6.63% |

## Per-wake distribution and context

| Component | Median | P90 | P95 | Max |
| --- | --- | --- | --- | --- |
| input_tokens | 12.0 | 42.4 | 130.0 | 610.0 |
| cache_read_input_tokens | 1,831,364.0 | 5,161,174.2 | 6,279,103.2 | 31,920,513.0 |
| cache_creation_input_tokens | 9,011.0 | 23,658.4 | 35,788.4 | 347,886.0 |
| output_tokens | 4,813.0 | 12,216.6 | 16,394.6 | 50,413.0 |

Spearman correlation of first-API cache-read tokens with total wake cache-read is **0.451**; with output tokens **-0.080**; with API-message count **-0.134**. There are 63 single-API wakes.

The cached-input relationship is partly mechanical: total cache reads include the first read and subsequent rereads. It is consistent with the suspected mechanism, not independent evidence of wasted work, causation or allowance cost. The output relationship is near zero. Scatter axes use log₁₀(1+tokens). First cache read omits uncached/newly cached input; the table also includes their sum as a fuller starting-context proxy.

## Top 10 sessions by cache-read wake share

No minimum-call filter here; the notebook allows one and can rank other components. Repeated agent names are different sessions.

| Agent | Session | Wake share | Wake tokens | All tokens | API messages |
| --- | --- | --- | --- | --- | --- |
| claude-3 | 355a71b9-163b-4f96-bebe-0497607deff0 | 80.53% | 92,010,364 | 114,259,747 | 408 |
| claude-2 | e5b0c82f-d57a-4441-aac4-b42d568b7070 | 60.50% | 38,371,434 | 63,425,560 | 157 |
| claude-15 | 9593f811-f96b-4582-a8f0-c462af80f0de | 56.05% | 629,753,193 | 1,123,513,890 | 3483 |
| claude-3 | 083f3138-4e0f-4d4b-991c-4fd0a0bc7c7c | 51.76% | 8,256,037 | 15,949,860 | 134 |
| claude-20 | b8aec109-71b7-43bf-a3d2-d7cf1e3f7c6a | 51.29% | 138,643,546 | 270,310,216 | 580 |
| claude-9 | 7d3a9423-a3ac-4777-a992-6b81f2fc3dc1 | 48.63% | 19,645,499 | 40,396,541 | 178 |
| claude-5 | de4c2047-bf32-4b18-bd55-8f97e94c6252 | 39.78% | 9,190,206 | 23,102,100 | 127 |
| claude-12 | d158cebc-06aa-4763-8704-e216a5a39f5c | 38.68% | 268,829,440 | 695,077,743 | 2169 |
| claude-4 | af24caa1-54d3-4f19-9d73-c8183eb9cb65 | 30.05% | 570,982,508 | 1,900,370,975 | 5372 |
| claude-2 | 8d27846b-b5b1-46b5-9e34-e31b0f034e52 | 25.61% | 13,328,469 | 52,050,588 | 230 |

## Batched example

`park-0456dffa-b338-4c48-afcd-8ad640e957ea`, dispatched by claude-5 at 17:13:15 on September 21, covered jobs `invoke-1790010836483-23001-e6516e78` and `invoke-1790010837682-23002-502e9a75`. The 17:22:13 wake, `cbf2b945-a2f0-48c9-b405-93500437397d`, contains both dependency job IDs. It used **2 API messages: input 4, cache read 481,227, cache write 2,233, output 633**. This demonstrates one recovered wake for two results; there is no measured counterfactual saving. The example is linked in the notebook and searchable in its per-wake table.

## Recompute and validation

Frozen: transcript union, logical usage events, tool-action extraction and provenance, stored as deterministic gzip JSONL. Live: date/rule submission reclassifies frozen events and recomputes all shares, statistics and tables; component/minimum-call controls redraw views. Source/action changes require an explicit extraction. The daily time series marks August 30 only when the window includes that date.

Commands, from `/home/joe/code`:

```sh
python3 futon0/analysis/audits/park-wake-pilot-2026-09-21/extract.py --cutoff 2026-09-21T17:40:06.024923Z
python3 futon0/analysis/audits/park-wake-pilot-2026-09-21/analyze.py
python3 futon0/analysis/audits/park-wake-pilot-2026-09-21/test_park_wake.py
python3 futon0/analysis/audits/park-wake-pilot-2026-09-21/validate_single_session.py
marimo-zone/.venv/bin/marimo check --strict marimo-zone/notebooks/chat-park-wake-pilot-20260921.py marimo-zone/notebooks/chat-business-ideas-20260921.py
```

Five tests cover the real extractor with duplicated snapshots/API blocks, tool-result boundaries, wake precedence, quoted instructions, false park detections in heredocs/JSON payloads, frozen rule/window recomputation, the actual batched wake and SVG markers. An independent claude-5 transcript recount matches every class/component: wake cache-read **9,190,206**, output **24,562**; all cache-read **23,102,100**, hence **39.78%** wake share.

The notebook executed headlessly. Authenticated browser returned HTTP 200 with five plots and no JS errors; changing the wake regex to a never-match expression produced zero wakes and restoring it returned 829. Edits after opening used code mode; saved cells were checked on disk. All implementation is Python. Transcripts remained read-only; no evidence-store mutation or deep-health request was made. Notebook routing was assigned to codex-14.

Artifacts: summary JSON, per-turn/per-session CSVs, component-specific SVGs, exact rules, source manifest, compressed records, extraction/analysis/validation scripts and tests. Token usage is not the plan-limit meter and does not establish the share of Joe's allowance consumed.
