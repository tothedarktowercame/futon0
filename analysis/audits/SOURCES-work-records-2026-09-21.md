# Sources of FUTON work records — 2026-09-21

Discovery only: no classifier, inferred R-node labels, or plot was produced.
Window: **2026-07-21 00:00:00 UTC ≤ timestamp < 2026-09-22 00:00:00 UTC**.
Queries ran on zone on September 21, beginning around 16:51 UTC; these are live
stores, not a transactionally synchronized historical snapshot. September 21 is
partial. Counts below are observed records, not estimates of missing work.
All service requests were GETs. No writes to Agency, futon1b, or source logs;
no deep-health call. Scratch query results were written only under `/tmp`.
Examples are projections of real rows; prose excerpts are explicitly truncated.
Commands assume `/home/joe/code` unless an absolute path is given.

## 1. Agency invoke jobs and persistence

Source: `GET http://localhost:7070/api/alpha/invoke/jobs?limit=10000`;
implementation `futon3c/src/futon3c/transport/http.clj`, especially
`invoke-job-public-view`, `create-invoke-job-ledger!`, `compact-invoke-jobs-ledger`,
and `invoke-jobs-store-path` (lines 414–640, 1559–1633, 2123–2160).
The requested limit exceeded the returned count, so the current job-order list
was exhausted: **2,305 jobs**, all with `created-at` in the window, spanning
**2026-08-27T13:46:49.833692951Z .. 2026-09-21T16:51:12.891870681Z**.
The independently read `/tmp/futon3c-invoke-jobs.edn` also held **2,305** jobs.
A later disk reread held 2,306 jobs (one additional request); the initial counts
below describe the earlier read, not an atomic cross-store snapshot.
This is retained coverage, not the number of invocations during the whole window.

Fields observed: `job-id`, `agent-id`, `caller`, `surface`, `mode`, `state`,
`created-at`, `started-at`, `finished-at`, `session-id`, `trace-id`,
`invocation/model`, `artifact-ref`, `execution`, `delivery`, `auto-bellback`,
`terminal-code`, `terminal-message`, `result`, `result-summary`, `events`,
`events-trimmed`, `trace/delivery-observation`. All jobs have identity, caller,
mode and execution maps; **2,138** have `started-at`, **2,136** `finished-at`,
**2,069** a non-null `session-id`. `execution.tool-events` and
`execution.command-events` are counts, not token costs. Both `executed` and
`executed?` spellings occur. No top-level token/usage/cost field or such execution
counter was found in these rows. Elapsed time can be derived when both timestamps
exist, but is not CPU time or operator effort.

`mode`: **1,417 brief**, **888 work**. Bell versus whistle is **`surface`**, not a
uniform `kind` column. On disk, **248** rows have `bell-type` (`request`: 228,
`assert`: 14, `query`: 6), and **6** have `ref`; the public list projection omits
these fields. These are semantic bell types, not the transport distinction.

Prompt text is present in **340 retained `events[type=prompt]`**, subject to event
truncation; **2,197 disk rows** retain `request-commission` with the original
normalized request (`agent-id`, `caller`, `surface`, `prompt`, optional model).
Public views omit that commission. **325** API rows retain `result`, **2,071**
`result-summary`; **1,892** declare `events-trimmed`. Source documents the result
cap as 8,000 characters and summary as 220: even unexpired result text is not an
unbounded transcript. `events` include accepted, running, prompt, tool_use, text,
done, failed, delivered and delivery-recorded; compaction removes intermediate
history. Work/brief is routing metadata, not a verified cognitive-function label.

Retention: terminal transcript detail expires after **24 hours**, and unreferenced
terminal tombstones after **7 days**. Active jobs and live parked dependencies are
exempt; old exceptional survivors do not establish continuous older coverage.
The EDN ledger is durable across JVM restarts but is a rewritten bounded snapshot,
not an append-only historical log. Source constants above establish these policy
durations; they are not empirical counts.

Additional disk records found:

| Source | Rows created in window | Earliest .. latest `created-at` |
|---|---:|---|
| `/tmp/futon3c-invoke-jobs.edn.bak-20260902-1900` | 7,583 | 2026-08-21T08:23:12.086918702Z .. 2026-09-02T18:47:00.724024187Z |
| `/tmp/futon3c-invoke-jobs.edn.f50-pre-direct-response-correction-20260828T1522Z.bak` | 3,244 | 2026-08-21T08:23:12.086918702Z .. 2026-08-28T15:21:31.778567223Z |
| `/tmp/futon3c-invoke-jobs.edn.commissions/*.edn` | 44 | 2026-09-13T16:47:09.330736615Z .. 2026-09-14T16:41:37.135837942Z |

These snapshots overlap; **do not add their row counts**. Commission archives
preserve `commission`, `job-join`, `request-digest`, `archive-digest`, `archived-at`
and `job-id`, not full event transcripts. They are not a two-month invocation log.
The default durable turn-queue file `/tmp/futon3c-durable-turn-queue.edn` also
exists: **11 entries** with `accepted-at` in the window, all in an August 28
snapshot. It contains routing/prompt/status records, not token billing.
`agency/turn_queue.clj` caps terminal history at 200 and prunes old entries; this
old file is not evidence of current queue completeness. No broader durable invoke
history was identified in the default ledger, its two adjacent backups, commission
archive, and default queue file inspected here.

Two API example rows (selected columns):

```jsonl
{"job-id": "auto-bellback-invoke-1787838335981-2376-9483deeb", "agent-id": "claude-clink-1", "caller": "auto-bellback", "surface": "auto-bellback", "mode": "brief", "created-at": "2026-08-27T13:46:49.833692951Z", "started-at": null, "finished-at": null, "session-id": null, "execution": {"executed?": false, "tool-events": 0, "command-events": 0}, "events-trimmed": "d13/compaction-20260902"}
{"job-id": "invoke-1790009057407-22993-61ec1a54", "agent-id": "codex-14", "caller": "claude-5", "surface": "bell", "mode": "work", "created-at": "2026-09-21T16:44:17.407750250Z", "started-at": "2026-09-21T16:44:18.450000399Z", "finished-at": "2026-09-21T16:47:38.091041139Z", "session-id": "01a0c489-1ec2-7553-9e83-52d0f2bea66e", "execution": {"executed": true, "tool-events": 20, "command-events": 20}, "events-trimmed": null}
```

Queries run (the JSON snapshots are scratch outputs, not source-store mutations):

```bash
python3 - <<'PY'
import json, urllib.request
from pathlib import Path
url='http://localhost:7070/api/alpha/invoke/jobs?limit=10000'
d=json.load(urllib.request.urlopen(url,timeout=30))
Path('/tmp/audit-jobs-api.json').write_text(json.dumps(d))
j=[r for r in d['jobs'] if '2026-07-21' <= r['created-at'] < '2026-09-22']
from collections import Counter
print(len(j), min(r['created-at'] for r in j), max(r['created-at'] for r in j))
print(Counter(k for r in j for k,v in r.items() if v is not None))
print(Counter(r['mode'] for r in j), Counter(r['surface'] for r in j))
print(Counter(e['type'] for r in j for e in r['events']))
PY
bb -e '(require (quote [clojure.edn :as e]) (quote [cheshire.core :as j])) (spit "/tmp/audit-jobs-disk.json" (j/generate-string (e/read-string (slurp "/tmp/futon3c-invoke-jobs.edn"))))'
bb -e '(require (quote [clojure.edn :as e]) (quote [cheshire.core :as j]) (quote [clojure.java.io :as io])) (spit "/tmp/audit-commissions.json" (j/generate-string (mapv #(e/read-string (slurp %)) (filter #(.isFile %) (file-seq (io/file "/tmp/futon3c-invoke-jobs.edn.commissions"))))))'
bb -e '(require (quote [clojure.edn :as e]) (quote [cheshire.core :as j])) (spit "/tmp/audit-turn-queue.json" (j/generate-string (e/read-string (slurp "/tmp/futon3c-durable-turn-queue.edn"))))'
```

Backup-count query executed as `bb /tmp/audit-backups.clj`:

```clojure
(require '[clojure.edn :as e] '[cheshire.core :as j])
(def paths ["/tmp/futon3c-invoke-jobs.edn.bak-20260902-1900" "/tmp/futon3c-invoke-jobs.edn.f50-pre-direct-response-correction-20260828T1522Z.bak"])
(defn summary [path]
  (let [jobs (vals (:jobs (e/read-string (slurp path))))
        rows (filter #(<= (compare "2026-07-21" (str (:created-at %))) 0 (compare "2026-09-22" (str (:created-at %)))) jobs)
        times (sort (map :created-at rows))]
    {:path path :rows (count rows) :earliest (first times) :latest (last times)
     :examples (mapv #(select-keys % [:job-id :agent-id :caller :created-at :started-at :finished-at :mode :surface :execution]) (take 2 rows))}))
(spit "/tmp/audit-backup-summary.json" (j/generate-string (mapv summary paths)))
```

Disk statistics query (run on scratch JSON decoded from EDN):

```python
import json, collections
j=list(json.load(open('/tmp/audit-jobs-disk.json'))['jobs'].values())
print(len(j), collections.Counter(k for r in j for k,v in r.items() if v is not None))
print(collections.Counter(r.get('bell-type') for r in j))
a=json.load(open('/tmp/audit-commissions.json'))
a=[r for r in a if '2026-07-21'<=r['job-join']['created-at']<'2026-09-22']
print(len(a), min(r['job-join']['created-at'] for r in a), max(r['job-join']['created-at'] for r in a))
q=json.load(open('/tmp/audit-turn-queue.json'))
print(sum('2026-07-21'<=r.get('accepted-at','')<'2026-09-22' for r in q['entries'].values()))
```

## 2. Token usage and monetary cost

Usage fields exist, but **no complete per-job, per-R-node cost table was found**.
No token sums or dollar estimates are made here.

| Source searched | Window usage rows | Distinct local sessions | Observed timestamp range |
|---|---:|---:|---|
| `~/.claude/projects/**/*.jsonl` (486 files scanned) | 80,602 | 439 | 2026-08-22T14:25:27.997Z .. 2026-09-21T16:51:21.914Z |
| `~/.codex/sessions/**/*.jsonl` (1724 files scanned) | 196,105 | 1,656 | 2026-08-04T16:25:22.554Z .. 2026-09-21T16:52:49.164Z |
| Claude `*.jsonl.pre-compact-*` (932 backups scanned separately) | 443,510 | 36 | 2026-08-20T21:30:16.696Z .. 2026-09-21T16:43:00.606Z |

Claude fields: row `timestamp`, `sessionId`, `requestId`, `uuid`, `cwd`;
`message.id`, `message.model`, `message.usage`. Usage includes `input_tokens`,
`output_tokens`, `cache_read_input_tokens`, `cache_creation_input_tokens`, nested
`cache_creation.ephemeral_1h_input_tokens` / `ephemeral_5m_input_tokens`,
`output_tokens_details.thinking_tokens`, `server_tool_use`, `iterations`,
`service_tier`, `speed`, `inference_geo` (schema varies).
**80,602 rows have 42,365 distinct `(sessionId,message.id)` keys**; the two examples
below deliberately show a repeated message. Backups have **38,995 distinct keys**,
with overlap between backups and current logs. Do not sum rows or add backup
counts to current counts. Key counts are not billed-call counts: synthetic
messages and usage updates need treatment before accounting.

Observed current-log models: `claude-opus-5`, `claude-fable-5`, `claude-sonnet-5`,
`claude-haiku-4-5-20251001`, `claude-opus-4-8`, `<synthetic>`. Coverage is local
Claude CLI sessions, including nested/subagent files, not every seat/host.
There is no complete historical `claude-N` seat → session → job mapping in each
usage row. Earliest observed usage, including backups, is August 20: the beginning
of the requested window is not covered by the inspected Claude usage records.

Codex fields: `timestamp`, optional `ordinal`, `type=event_msg`,
`payload.type=token_count`, `payload.info.total_token_usage` / `last_token_usage`,
with `input_tokens`, `cached_input_tokens`, `cache_write_input_tokens`,
`output_tokens`, `reasoning_output_tokens`, `total_tokens`.
`model_context_window` and rate-limit metadata are not consumed-token costs.
There are **196,105 rows with info**, plus **613 token_count rows without info**;
**196,097 distinct `(file stem,timestamp,ordinal)` diagnostic keys**.
Cumulative totals cannot be summed across events. Repeated snapshots, session
resumption and resets need handling. Coverage is local Codex rollout sessions,
not all hosts; historical `codex-N` seat IDs are not on each usage event.

Two Claude rows (content omitted) and two Codex rows:

```jsonl
{"path": "/home/joe/.claude/projects/-home-joe-code-futon3c/75d416f9-4401-4ec6-ab5f-37cb5eac2261.jsonl", "line": 11, "timestamp": "2026-08-23T15:37:14.294Z", "sessionId": "75d416f9-4401-4ec6-ab5f-37cb5eac2261", "message_id": "msg_011CeKvoYAf6YKwGkhnC3FNy", "model": "claude-fable-5", "usage": {"input_tokens": 2, "cache_creation_input_tokens": 19946, "cache_read_input_tokens": 15903, "output_tokens": 223}}
{"path": "/home/joe/.claude/projects/-home-joe-code-futon3c/75d416f9-4401-4ec6-ab5f-37cb5eac2261.jsonl", "line": 12, "timestamp": "2026-08-23T15:37:15.013Z", "sessionId": "75d416f9-4401-4ec6-ab5f-37cb5eac2261", "message_id": "msg_011CeKvoYAf6YKwGkhnC3FNy", "model": "claude-fable-5", "usage": {"input_tokens": 2, "cache_creation_input_tokens": 19946, "cache_read_input_tokens": 15903, "output_tokens": 223}}
```

```jsonl
{"path": "/home/joe/.codex/sessions/2026/08/04/rollout-2026-08-04T16-25-19-019fcd98-0238-7951-a769-ee1ceed3b3da.jsonl", "line": 13, "timestamp": "2026-08-04T16:25:22.554Z", "type": "event_msg", "payload": {"type": "token_count", "info": {"total_token_usage": {"input_tokens": 13652, "cached_input_tokens": 11008, "cache_write_input_tokens": 0, "output_tokens": 7, "reasoning_output_tokens": 0, "total_tokens": 13659}, "last_token_usage": {"input_tokens": 13652, "cached_input_tokens": 11008, "cache_write_input_tokens": 0, "output_tokens": 7, "reasoning_output_tokens": 0, "total_tokens": 13659}, "model_context_window": 258400}}}
{"path": "/home/joe/.codex/sessions/2026/08/04/rollout-2026-08-04T16-25-19-019fcd98-0238-7951-a769-ee1ceed3b3da.jsonl", "line": 27, "timestamp": "2026-08-04T19:56:03.694Z", "type": "event_msg", "payload": {"type": "token_count", "info": {"total_token_usage": {"input_tokens": 29429, "cached_input_tokens": 22016, "cache_write_input_tokens": 0, "output_tokens": 168, "reasoning_output_tokens": 25, "total_tokens": 29597}, "last_token_usage": {"input_tokens": 15777, "cached_input_tokens": 11008, "cache_write_input_tokens": 0, "output_tokens": 161, "reasoning_output_tokens": 25, "total_tokens": 15938}, "model_context_window": 258400}}}
```

**ZAI/ZAIF:** `futon3c/src/futon3c/agents/zai_api.clj:790–819,942–984`
translates response usage and persists per-round evidence with tags
`transcript,turn-round`. Fields: `body.turn-id`, `round`, `profile`, `calls`,
`final`, `text`, `cost/source`, `cost/model`, `cost/input-tokens`,
`cost/output-tokens`, `cost/total-tokens`, optional `cost/cached-input-tokens`,
`cost/reasoning-tokens`. **44,283 round rows** match the window; both sampled
rows actually carry cost, identify `zai-14`, and name model `glm-5.3`.
This is a round-row count, **not a verified count of rows with usage**: the full
44,283 bodies were not hydrated solely to count missing cost fields. Code omits
absent counters. `turn-start` evidence includes `dispatch-id` → `turn-id`, a
candidate explicit job join; its coverage was not counted.

Two ZAI examples (tool arguments and prose omitted):

```jsonl
{"evidence/id": "e-8158a365-4e5c-4481-9bc8-4b498f64b3a4", "evidence/at": "2026-09-19T01:54:19.975788597Z", "evidence/author": "zai-14", "evidence/session-id": "zai-4f07c148-fa85-454c-ade5-b2586a795c60", "body": {"turn-id": "zai-turn-0f3c967b-cb3a-4877-81e6-bb5fb4e9c5dc", "cost/input-tokens": 202729, "cost/source": "zai", "cost/cached-input-tokens": 202496, "cost/model": "glm-5.3", "cost/total-tokens": 202766, "round": 6, "event": "turn-round", "cost/reasoning-tokens": 0, "cost/output-tokens": 37, "profile": "zai"}}
{"evidence/id": "e-c92351fc-cebf-4638-844c-4a808f6381f0", "evidence/at": "2026-09-19T01:54:04.244327076Z", "evidence/author": "zai-14", "evidence/session-id": "zai-4f07c148-fa85-454c-ade5-b2586a795c60", "body": {"turn-id": "zai-turn-0f3c967b-cb3a-4877-81e6-bb5fb4e9c5dc", "cost/input-tokens": 201425, "cost/source": "zai", "cost/cached-input-tokens": 200832, "cost/model": "glm-5.3", "cost/total-tokens": 201642, "round": 3, "event": "turn-round", "cost/reasoning-tokens": 122, "cost/output-tokens": 217, "profile": "zai"}}
```

Other locations: `futon3c/dev/futon3c/dev.clj:3859–3862` returns Claude CLI
`usage` and `total_cost_usd` as `:usage` / `:total-cost-usd` from the runner;
`transport/http.clj:6896–6897` exposes them for cold-compaction responses. That is
a runtime field, not a demonstrated historical billing log. The inspected job
ledger does not retain it. `futon3c/scripts/claude-spend.py` derives estimates
from session logs and rates; it is not a billing export. Its comment saying ZAI
is unmeasurable is stale relative to the sampled round evidence.

Filename searches under `~/.claude` and `~/.codex` for `*billing*`, `*.csv`,
`*usage*.json`, `*cost*.json`, `*stats*` identified no billing export. No remote
host or billing portal was queried; absence is limited to this scope.

Usage scan run as `python3 /tmp/audit-usage-scan.py`:

```python
from pathlib import Path
import json,collections
lo,hi='2026-07-21','2026-09-22'
result={}
for provider,root,needle in [('claude','/home/joe/.claude/projects',b'"usage"'),('codex','/home/joe/.codex/sessions',b'"token_count"')]:
 files=list(Path(root).rglob('*.jsonl')); count=0; sessions=set(); keys=set(); models=collections.Counter(); examples=[]; earliest=None;latest=None;invalid=0; unique=set(); noinfo=0
 for p in files:
  with p.open('rb') as f:
   for lineno,line in enumerate(f,1):
    if needle not in line:continue
    try:d=json.loads(line)
    except (ValueError,UnicodeDecodeError): invalid+=1;continue
    t=str(d.get('timestamp',''))
    if not lo<=t<hi:continue
    if provider=='claude':
     msg=d.get('message',{}); usage=msg.get('usage')
     if d.get('type')!='assistant' or not isinstance(usage,dict):continue
     sid=d.get('sessionId',p.stem); key=(sid,msg.get('id')); models[msg.get('model','unknown')]+=1
     example={'timestamp':t,'sessionId':sid,'message_id':msg.get('id'),'model':msg.get('model'),'usage':usage}
    else:
     payload=d.get('payload',{})
     if d.get('type')!='event_msg' or payload.get('type')!='token_count':continue
     info=payload.get('info'); sid=p.stem
     if not isinstance(info,dict):noinfo+=1;continue
     usage=info;key=(sid,t,d.get('ordinal'))
     example={'timestamp':t,'type':d['type'],'payload':{'type':'token_count','info':info}}
    count+=1; sessions.add(str(sid));unique.add(key);keys.update(usage)
    earliest=min(earliest or t,t);latest=max(latest or t,t)
    if len(examples)<2:examples.append({'path':str(p),'line':lineno,**example})
 result[provider]={'files_scanned':len(files),'rows':count,'sessions':len(sessions),'unique_message_or_event_keys':len(unique),'usage_keys':sorted(keys),'models':dict(models),'earliest':earliest,'latest':latest,'invalid_candidate_lines':invalid,'token_events_without_info':noinfo,'examples':examples}
 print(provider,count,flush=True)
Path('/tmp/audit-usage-summary.json').write_text(json.dumps(result,indent=2))
```

The backup run used the same script with only the Claude tuple,
`rglob('*.jsonl.pre-compact-*')`, and output `/tmp/audit-usage-backups.json`.
ZAI query (same GETs were executed through urllib):

```bash
curl -fsS -H 'Accept: application/json' 'http://localhost:7073/api/alpha/evidence/count?since=2026-07-21&before=2026-09-22&tags=transcript%2Cturn-round'
curl -fsS -H 'Accept: application/json' 'http://localhost:7073/api/alpha/evidence?since=2026-07-21&before=2026-09-22&tags=transcript%2Cturn-round&limit=2'
rg --files /home/joe/.codex /home/joe/.claude -g '*stats*' -g '*usage*.json' -g '*cost*.json' -g '*billing*' -g '*.csv'
```

## 3. Operator turns, mission clocks, and retrieval labels

Source: futon1b **:7073**, `/api/alpha/evidence/count` and bounded cursor pages
of `/api/alpha/evidence`, filtered by `author=joe`, `since`, `before`.
The direct API uses **`tags` (plural)**. `futon1b/futon1b_evidence.clj:384–454`
defines filters, keyset cursors and projected counts. Source of Emacs records:
`futon3c/emacs/agent-chat.el:3025–3044,3129–3174`.

**9,762 Joe-authored evidence records**, confirmed both by `/count` and ten
sequential cursor pages; **9,762 distinct evidence IDs**. Within these:

- **9,405** have `body.event=chat-turn`, `body.role=user`; all contain text.
  **6,982** use `emacs-claude-repl`; **2,423** use `emacs-codex-repl`.
- **5** additional Marimo rows have `transport=marimo`, `direction=inbound`.
  Thus the explicitly observed Emacs-user + Marimo-inbound turn-row count is
  **9,410**, not the author-only count.
- **293** are session-start records; other author-only records include reviews
  and administrative evidence, not conversation turns.
- Of the 9,405 Emacs user-turn rows, **3,230** have `mission-id`, **4,018** have
  a `clocked-target`, **45** have `auto-clock-witness`.
- **3,024** contain the literal marker `--- resumed: parked dependencies complete`.
  They are wake/continuation-shaped inputs recorded as Joe/user. This is an exact
  marker count, **not a complete automatic-input detector**. A count of actual
  human-authored turns cannot honestly be inferred from `author=joe` alone.

Emacs user-turn timestamps span **2026-07-21T00:47:54.006042628Z ..
2026-09-21T16:50:29.472218084Z**. Fields: `evidence/id`, `evidence/at`,
`evidence/author`, `evidence/type`, `evidence/claim-type`, `evidence/session-id`,
`evidence/in-reply-to`, `evidence/subject`, `evidence/tags`, `evidence/body`;
body `event`, `transport`, `role`, `text`, optional `turn-id`, `mission-id`,
`clocked-mission`, `campaign-id`, `clocked-campaign`, `excursion-id`,
`clocked-excursion`, `clocked-target`, `auto-clock-witness`. No token or duration
cost field appeared in the union of these Joe-record body keys.

Two real turns (text excerpts):

```jsonl
{"evidence/id": "emacs-4576284168eb9650b83191775a0ec5e8", "evidence/at": "2026-09-21T06:05:12.498884209Z", "evidence/session-id": "d158cebc-06aa-4763-8704-e216a5a39f5c", "evidence/author": "joe", "body": {"event": "chat-turn", "transport": "emacs-claude-repl", "role": "user", "turn-id": "claude-12-turn-346", "mission-id": "M-a-sorry-enterprise", "clocked-mission": "M-a-sorry-enterprise", "clocked-target": "M-a-sorry-enterprise", "text_excerpt": "I guess here's the thing that's interesting for me. Which is that... Now I can use the ideas. With the kind of perceive, believe, evaluate, select, act. Loop. Which is broken down "}}
{"evidence/id": "emacs-2edf522239eb55774fb8bbc87c778feb", "evidence/at": "2026-09-21T16:49:32.532707772Z", "evidence/session-id": "355a71b9-163b-4f96-bebe-0497607deff0", "evidence/author": "joe", "body": {"event": "chat-turn", "transport": "emacs-claude-repl", "role": "user", "turn-id": "claude-3-turn-46", "text_excerpt": "improve-1a back from codex-11. Checklist: (1) git -C /home/joe/code/futon2 log main..fix/narrative-improve-1a; (2) scores byte-identical; historical updater held with reason; five "}}
```

Route A in the paper is the mission clock above. Route B is recorded
`context-retrieval` evidence, not a clock and not a measured R-node label.
Paper §7 is locally available at
`futon2/holes/labs/wm-contract/runs/fundamentals-checklist-2026-09-15/futon-2026.html:3285–3310`.
It distinguishes ranked retrieval from clock-in and says the interface uses rank 1,
not every candidate as a separate operator observation.
**25,810 context-retrieval-tagged records** match this window (all authors).
Sample retrieval `evidence/body` values are **EDN-encoded strings inside the JSON
response**, not JSON objects. Decoding their EDN exposes `event`, `agent-id`, `at`,
`turn`, `query`, `results`; candidates carry `id`, `title`, `score`, `rank`,
`retrieval-source`, `retrieval-method`. A consumer must normalize this mixed body
representation before joining. Two actual retrieval records, with full recorded bodies:

```jsonl
{"evidence/body": "{\"event\" \"context-retrieval\", \"agent-id\" \"codex-14\", \"at\" \"2026-09-21T16:50:35.420221329Z\", \"turn\" 6, \"query\" \"--- CURRENT TURN ---\\nSurface: emacs-repl\\nFrom: joe\\nTo: codex-14\\nOrigin: operator\\nCaller: joe\\n---\\n\\nI \", \"results\" [{:id \"musn/plan-before-tool\", :title \"Plan Before Tool Use\", :score 0.3232, :rank 1, :retrieval-source \"futon3a\", :retrieval-method \"embeddings\"} {:id \"iching/hexagram-49-ge\", :title \"䷰ 革 (Gé) - Revolution\", :score 0.3095, :rank 2, :retrieval-source \"futon3a\", :retrieval-method \"embeddings\"} {:id \"iching/hexagram-16-yu\", :title \"䷏ 豫 (Yù) - Enthusiasm\", :score 0.3053, :rank 3, :retrieval-source \"futon3a\", :retrieval-method \"embeddings\"}]}", "evidence/session-id": "01a0c489-1ec2-7553-9e83-52d0f2bea66e", "evidence/tags": ["invoke", "dev", "context-retrieval", "futon3a"], "evidence/at": "2026-09-21T16:50:35.420592786Z", "evidence/type": "coordination", "evidence/subject": {"ref/type": "agent", "ref/id": "codex-14"}, "evidence/author": "codex-14", "evidence/claim-type": "step", "evidence/id": "e-3c0a1fd5-a358-4b74-9fbf-f805d24ae2bd"}
{"evidence/body": "{\"event\" \"context-retrieval\", \"agent-id\" \"claude-5\", \"at\" \"2026-09-21T16:51:29.064187816Z\", \"turn\" 6, \"query\" \"So, sure, this was just the first plot, like I'm saying, so this could get us started. But what I'm \", \"results\" [{:id \"math-formalization-MG/chart-a-polytope-sphere-by-perimeter-walk\", :title \"Chart a Polytope Sphere by a Perimeter Walk\", :score 0.2522, :rank 1, :retrieval-source \"futon3a\", :retrieval-method \"embeddings\"} {:id \"math-formalization-CA/fourier-inversion-for-real-oscillatory-integrals\", :title \"Close Real Oscillatory Integrals by Fourier Inversion, Not Residues\", :score 0.2385, :rank 2, :retrieval-source \"futon3a\", :retrieval-method \"embeddings\"} {:id \"math-formalization-CA/layer-cake-crossover-split\", :title \"Layer-Cake Bound by a Crossover Split\", :score 0.2353, :rank 3, :retrieval-source \"futon3a\", :retrieval-method \"embeddings\"}]}", "evidence/session-id": "de4c2047-bf32-4b18-bd55-8f97e94c6252", "evidence/tags": ["invoke", "dev", "context-retrieval", "futon3a"], "evidence/at": "2026-09-21T16:51:29.064507355Z", "evidence/type": "coordination", "evidence/subject": {"ref/type": "agent", "ref/id": "claude-5"}, "evidence/author": "claude-5", "evidence/claim-type": "step", "evidence/id": "e-a28b65f7-62c3-4285-8257-689a21db0fb5"}
```

Queries run: `python3 /tmp/audit-evidence-scan.py` below. The initial separate
`/count?author=joe&since=2026-07-21&before=2026-09-22` returned 9,762.
This scan uses bounded pages and follows `next-cursor`, including on short pages;
it never calls unfiltered evidence or deep health.

```python
import urllib.request,urllib.parse,json
from pathlib import Path
base='http://localhost:7073/api/alpha/evidence'
window={'since':'2026-07-21','before':'2026-09-22'}
def get(suffix,params):
 url=base+suffix+'?'+urllib.parse.urlencode({**window,**params})
 with urllib.request.urlopen(urllib.request.Request(url,headers={'Accept':'application/json'}),timeout=60) as r:return json.load(r)
rows=[];cursor={};pages=0
while True:
 d=get('',{'author':'joe','limit':1000,**cursor});rows+=d['entries'];pages+=1
 print('joe page',pages,'rows',len(rows),flush=True)
 if 'next-cursor' not in d:break
 c=d['next-cursor'];cursor={'cursor-at':c['at'],'cursor-id':c['id']}
Path('/tmp/audit-joe.json').write_text(json.dumps(rows))
other={}
for tag in ['context-retrieval','mesh-edge','bell','park','park-resume','wake','scheduled-dispatch']:
 count=get('/count',{'tags':tag});sample=get('',{'tags':tag,'limit':2})
 other[tag]={'count':count,'sample':sample}
 print(tag,count,flush=True)
Path('/tmp/audit-evidence-tags.json').write_text(json.dumps(other))
```

Turn-field counts run on those returned rows:

```python
import json, collections
rows=json.load(open('/tmp/audit-joe.json'))
turns=[r for r in rows if r.get('evidence/body',{}).get('event')=='chat-turn'
       and r['evidence/body'].get('role')=='user']
print(len(rows), len({r['evidence/id'] for r in rows}), len(turns))
print(collections.Counter(r.get('evidence/body',{}).get('event') for r in rows))
print(collections.Counter(r['evidence/body'].get('transport') for r in turns))
for key in ['text','mission-id','clocked-target','auto-clock-witness']:
    print(key,sum(bool(r['evidence/body'].get(key)) for r in turns))
print('park-marker',sum('--- resumed: parked dependencies complete' in
    r['evidence/body'].get('text','') for r in turns))
print('marimo-inbound',sum(r.get('evidence/body',{}).get('transport')=='marimo'
    and r['evidence/body'].get('direction')=='inbound' for r in rows))
print(min(r['evidence/at'] for r in turns),max(r['evidence/at'] for r in turns))
print(collections.Counter(k for r in rows for k in r.get('evidence/body',{})))
```

## 4. Park/wake, bell, whistle events

The retained Agency job snapshot (§1) contains **1,319 `surface=bell`**,
**863 `surface=auto-bellback`**, and **38 `surface=whistle`** jobs created in the
window. These are distinct retained jobs, not the complete historical event
count, and auto-bellbacks must not be silently counted as new human requests.
No universal `kind=bell/whistle` column exists. Two examples:

```jsonl
{"job-id": "invoke-1790009472891-22996-45f5ee00", "agent-id": "codex-14", "caller": "claude-5", "created-at": "2026-09-21T16:51:12.891870681Z", "started-at": "2026-09-21T16:51:14.280444314Z", "finished-at": null, "mode": "work", "surface": "bell", "execution": {"executed?": false, "tool-events": 0, "command-events": 0}}
{"job-id": "invoke-1790008375009-22987-f01c596e", "agent-id": "zai-1", "caller": "wm-full-loop", "created-at": "2026-09-21T16:32:55.009972685Z", "started-at": "2026-09-21T16:32:56.492800793Z", "finished-at": "2026-09-21T16:33:00.071123274Z", "mode": "brief", "surface": "whistle", "execution": {"executed": false, "tool-events": 0, "command-events": 0}}
```

Park source: `/tmp/futon3c-parked-on.edn`, implemented by
`futon3c/src/futon3c/agency/parked_on.clj`. **55 outstanding records** have
`parked-at-ms` in the window; all have `released?=false`. The earliest is
2026-08-22T14:56:16.112000+00:00.
Fields: `id`, `agent`, `session`, `surface`, `mode`, `parked-at-ms`, `awaiting`,
`arrived`, `payload`, `budget`, `deadline-ms`, `timer-due-ms`, `coalesce-key`,
`released?`. The current snapshot also contains **103 ready-inbox items**, and
**0 leases**; ready items need not carry a release timestamp, so their window
count is **unavailable**, not 103 historical wakes. Released records are removed;
this store is not a park/wake event journal. `budget.resumes-left` is a control
budget, not token cost. Some War Machine payloads explicitly carry `node=R16`,
`lifecycle/stage=parked`, `attempt-id`, `repair-id`: these are recorded labels,
not labels inferred in this discovery.

Two outstanding park rows:

```jsonl
{"id": "park-8a097598-2745-4325-865c-63103bc2c60e", "agent": "war-machine", "session": null, "surface": "morning-brief", "parked-at-ms": 1789848188567, "mode": "between-turn", "payload": {"node": "R16", "lifecycle/stage": "parked", "attempt-id": "ea1-f9a0a2fabd9e1a60116e846608a0e56e0b6f4a8f23cab048bafdde39a10ec0f6--attempt-002", "repair-id": "repair-ea1-f9a0a2fabd9e1a60116e846608a0e56e0b6f4a8f23cab048bafdde39a10ec0f6--attempt-002-agent-unavailable"}, "budget": {"resumes-left": 1, "max-depth": 8}, "released?": false}
{"id": "park-d4b7815f-e6a4-4c31-b3e1-317051f98ddd", "agent": "war-machine", "session": null, "surface": "morning-brief", "parked-at-ms": 1789338166471, "mode": "between-turn", "payload": {"node": "R16", "lifecycle/stage": "parked", "attempt-id": "ea1-3f4cac241e58afd9b6eae48e78a2ac7f63925aa3fc05c7e3a3fd6d789d4637a9--attempt-003", "repair-id": "repair-ea1-3f4cac241e58afd9b6eae48e78a2ac7f63925aa3fc05c7e3a3fd6d789d4637a9--attempt-003-artifact-binding-mismatch"}, "budget": {"resumes-left": 1, "max-depth": 8}, "released?": false}
```

Wake evidence elsewhere: §3 found **3,024** exact resume-marker appearances
in Joe/user turn text. `futon3c/emacs/agent-repl-park.el:90–125` documents the
resume delivery path. This is not a unique park-ID count and does not establish
all wakes. **No complete historical park/wake total was obtained.**

Filtered evidence tag queries on :7073 returned **0** for each of `mesh-edge`,
`bell`, `park`, `park-resume`, `wake` in this window. These are scoped absence
results, not a claim that the events did not happen: bell/whistle jobs above
exist. `futon3c/src/futon3c/social/coordination_ledger.clj` defines mesh-edge
records (`edge/id`, `edge/kind`, `edge/from`, `edge/to`, `edge/surface`, `edge/at`,
optional `edge/ok?`, `edge/error`), but that tag query found no persisted rows in
this evidence store. `scheduled-dispatch` has **1** record in the same window;
its code carries `node=R10`, `process/stage=dispatched`, commission and receipt.
There cannot be two example rows for a source with zero or one counted row.
The scheduled-dispatch **first page was empty and incomplete**, after scanning
20,000 compact records, with a next cursor at 2026-09-16T13:13:48.299591255Z.
That does not contradict `/count=1` or establish absence. I did not keep paging
through the full store just to hydrate this optional example. Its body remains
**uninspected**; the R10/stage fields above are supported by source code, not by
a sampled live row. This was the bounded page response:

```json
{"entries": [], "count": 0, "limit": 2, "scanned": 20000, "next-cursor": {"at": "2026-09-16T13:13:48.299591255Z", "id": "e-3a2ffa5d-6c2c-4344-b2fe-a724c0c9defa"}, "incomplete": true, "scan/max": 20000}
```

Tag counts and samples were queried by the exact loop in §3. Disk park query:

```bash
bb -e '(require (quote [clojure.edn :as e]) (quote [cheshire.core :as j])) (spit "/tmp/audit-parks.json" (j/generate-string (e/read-string (slurp "/tmp/futon3c-parked-on.edn"))))'
python3 - <<'PY'
import json
from datetime import datetime,timezone
p=json.load(open('/tmp/audit-parks.json'))
lo=datetime(2026,7,21,tzinfo=timezone.utc).timestamp()*1000
hi=datetime(2026,9,22,tzinfo=timezone.utc).timestamp()*1000
r=[r for r in p['records'].values() if lo<=r['parked-at-ms']<hi]
print(len(r),min(r['parked-at-ms'] for r in r))
print(sum(len(v) for v in p['ready-inbox'].values()),len(p['leased']))
from collections import Counter
print(Counter(x['released?'] for x in r))
PY
```

## 5. Mission records and phase labels

Source: `GET http://localhost:7070/api/alpha/missions?include-turn-counts=false`
(**turn-count telemetry explicitly disabled**). Returns **219 current mission
records**, not a historical phase-event stream. **6** have `mission/date` in the
window; **21** have `mission/mtime` in the window. Those are separate predicates,
not 27 missions and not 21 phase transitions. `mtime` is filesystem/document
metadata and can change on checkout; neither date establishes a per-turn phase.

Fields: `mission/id`, `mission/title`, `mission/repo`, `mission/path`,
`mission/date`, `mission/mtime`, `mission/phase`, `mission/status`,
`mission/raw-status`, `mission/source`, `mission/owner`, `mission/summary`,
`mission/blocked-by`, `mission/cross-refs`, `mission/code-paths`, `mission/gates`,
`mission/psrs`, `mission/purs`, optional `mission/devmap-id`. No per-mission
cost field was returned. Observed phase frequencies:

- `complete`: 63

- `instantiate`: 34

- `identify`: 32

- `map`: 22

- `verify`: 17

- `derive`: 12

- `null/missing`: 11

- `document`: 10

- `unknown`: 9

- `head`: 7

- `argue`: 2

These are candidate labels already produced by the existing system, not a
new classifier. `futon3c/src/futon3c/peripheral/mission_control_backend.clj:339–362`
parses status text first, then body text, choosing the first matching phase in
ordered patterns DOCUMENT, INSTANTIATE, VERIFY, ARGUE, DERIVE, MAP, IDENTIFY,
HEAD, COMPLETE; otherwise unknown. Thus a phase can be a current textual
projection rather than an explicit historical stage transition. `build-inventory`
(lines 975–1005) reads substrate-2 with filesystem fallback and adds devmaps.
No demonstrated phase-at-turn join or R-node cost attribution was found here.

Two mission rows with `mission/date` in the window:

```jsonl
{"mission/id": "codex-sorry-loop", "mission/repo": "futon3c", "mission/path": "/home/joe/code/futon3c/holes/missions/M-codex-sorry-loop.md", "mission/date": "2026-07-28", "mission/mtime": "2026-07-29", "mission/phase": "map", "mission/status": "unknown", "mission/raw-status": "**CHARTERED at Joe's direction** (this conversation: \"produce a"}
{"mission/id": "latex-wysiwyg", "mission/repo": "futon3c", "mission/path": "/home/joe/code/futon3c/holes/missions/M-latex-wysiwyg.md", "mission/date": "2026-08-08", "mission/mtime": "2026-08-14", "mission/phase": "map", "mission/status": "unknown", "mission/raw-status": ":designed — ground measured on draft8, slices cut, nothing built yet."}
```

```bash
python3 - <<'PY'
import json,urllib.request,collections
from pathlib import Path
url='http://localhost:7070/api/alpha/missions?include-turn-counts=false'
d=json.load(urllib.request.urlopen(url,timeout=30))
Path('/tmp/audit-missions-api.json').write_text(json.dumps(d))
m=d['missions'];print(len(m))
for field in ['mission/date','mission/mtime']:
    print(field,sum('2026-07-21'<=str(r.get(field) or '')<'2026-09-22' for r in m))
print(collections.Counter(r.get('mission/phase') for r in m))
print(sorted(set(k for r in m for k in r)))
PY
```

## 6. futon2 commit prefix/type coverage

Source: canonical `/home/joe/code/futon2`, `git log --all`, deduplicated by SHA,
filtered on UTC committer timestamp `%ct` in the inclusive date window.
**3,851/5,861 commits = 65.71%** have an initial
colon-delimited prefix under the exact regex below. Prefixes are trimmed and
case-folded for counting; `Row 22` and `row 22` therefore combine. This is a
syntactic prefix census, **not a claim that every prefix is a semantic work type**:
`now`, row numbers and merge-description prefixes pass too. No R classifier was
built. Commits without this syntax are not asserted to be unlabelable.
The live count differs from the earlier plot's 5,851 because commits continued
to arrive during this discovery; the plot was not regenerated.

Top 15 prefixes:

| Prefix (case-folded) | Commits |
|---|---:|
| `wm-contract` | 623 |
| `registry` | 311 |
| `row 22` | 132 |
| `zaif-harness` | 131 |
| `row 14` | 88 |
| `row 16` | 77 |
| `row 15` | 73 |
| `worklist` | 64 |
| `row 18` | 62 |
| `library-loop` | 58 |
| `ledger` | 56 |
| `row 19` | 47 |
| `m-formal-war-machine` | 32 |
| `now` | 28 |
| `bulletin` | 25 |

Two actual matching commit rows (both demonstrate why syntax is not a work-type label):

```jsonl
{"sha": "27e7b982c6a6223ee50d1d3dfe4f0940dcac2ca1", "date": "2026-09-21", "subject": "Merge fix/narrative-improve-1a: record-only learning-trial receipts (improve-1 slice 1)"}
{"sha": "e6ef161ecd5db99eb287f285af0a96154efd1f9d", "date": "2026-09-21", "subject": "Merge improve-4 discovery update (ce164d8f): IAD as operator over preferences"}
```

Command run: `python3 /tmp/audit-prefix-scan.py`:

```python
import subprocess,re,json,collections
from datetime import datetime,timezone
from pathlib import Path
rows={}
pattern=r'^([A-Za-z][A-Za-z0-9 _./()#-]{0,79}):(?:\s|$)'
for line in subprocess.check_output(['git','-C','/home/joe/code/futon2','log','--all','--format=%H%x09%ct%x09%s'],text=True).splitlines():
 sha,t,subject=line.split('\t',2)
 day=datetime.fromtimestamp(int(t),timezone.utc).date().isoformat()
 if '2026-07-21'<=day<'2026-09-22':rows[sha]={'sha':sha,'date':day,'subject':subject}
counts=collections.Counter();examples=[]
for row in rows.values():
 m=re.match(pattern,row['subject'])
 if m:
  counts[m[1].strip().casefold()]+=1
  if len(examples)<2:examples.append(row)
result={'commits':len(rows),'prefixed':sum(counts.values()),'fraction':sum(counts.values())/len(rows),'regex':pattern,'top15':counts.most_common(15),'examples':examples}
Path('/tmp/audit-prefixes.json').write_text(json.dumps(result));print(json.dumps(result))
```

## Source comparison

| Source | Unit | Cost field (or none) | Candidate label fields | Coverage in window | Gaps |
|---|---|---|---|---|---|
| Agency live/disk jobs | Retained invocation | No tokens; start/finish and tool/command counts | agent, caller, mode, surface, prompt/commission, result, artifact; disk bell-type/ref | 2,305 retained jobs | 24h detail / 7d tombstone policy; older exceptions; not full window |
| Job backups / commission archive | Snapshot job / archived request | Execution counts and timestamps; no token ledger | request prompt, caller/agent, surface, artifact | Backups 7,583 and 3,244; archives 44 | Overlap; snapshots are not an append-only log |
| Claude current local logs | Usage-bearing assistant row | Input/output/cache/thinking counters | model, session, cwd, adjacent message/tool content | 80,602 rows / 439 sessions, Aug 22 onward | Duplicate message rows; no complete seat/job join; no early-window coverage |
| Claude pre-compaction backups | Copied usage-bearing row | Same counters | Same message/session context | 443,510 rows / 36 sessions, Aug 20 onward | Strong overlap between snapshots and current logs; not additive |
| Codex local rollouts | token_count event with info | Cumulative and last-token usage | rollout/session context; adjacent turns/tools | 196,105 rows / 1,656 files-as-sessions, Aug 4 onward | 613 no-info events; repeated/cumulative usage; no direct per-job attribution |
| ZAI/ZAIF evidence | Transcript round | cost/input-, output-, total-, cached-input-, reasoning-tokens; model/source | author, profile, turn-id, round, calls; dispatch join via turn-start | 44,283 round rows; two cost-positive examples checked | Full usage-field completeness and join coverage not counted |
| Joe-authored evidence | Evidence / nominal user turn | None | Text, session, turn-id, mission/campaign/excursion clock | 9,762 records; 9,410 explicit Emacs/Marimo turn rows | Includes 3,024 known resume-marker occurrences; true human count unknown; clock sparse |
| Context retrieval evidence | Retrieval event | None observed | Ranked pattern IDs/scores and turn/session context | 25,810 records | Patterns are not mission clocks or R-node labels; candidate ranks not separate turns |
| Park state / wake-shaped turns | Outstanding park / resume-marker turn | Resume control budget only, not cost | agent, payload, dependencies; some explicit R16 payloads | 55 parks dated in window; 3,024 marker turns | Released parks removed; 103 ready items not time-countable; no complete wake journal |
| Bell/whistle job surfaces | Retained routed job | Execution counts/timestamps only | surface, caller, target, mode | 1,319 bells + 863 auto-bellbacks + 38 whistles | Retention-limited; evidence tag queries separately empty |
| Scheduled-dispatch evidence | Dispatch receipt | Not inspected | R10, process/stage, commission/receipt in writer code | 1 counted record; first page incomplete/empty | Body not hydrated; no general label coverage demonstrated |
| Mission inventory | Current document projection | None | phase, raw-status, status, owner, code paths, PSR/PUR | 219 current; 6 document dates / 21 mtimes in window | Current text-derived phase is not historical phase-at-turn |
| futon2 Git | Unique commit | None | Colon prefix, subject, paths, trailers, committer date | 5,861 commits; 3,851 prefixed (65.71%) | Prefix syntax is heterogeneous; agent authorship and effort not encoded by commit count |

The biggest gap is not the lack of candidate labels or any token counters. It is
the lack of a complete, deduplicated, time-aligned join from job/session usage to
operator turns and their then-current mission/phase/R-node. No estimate is used
to fill that gap here.
