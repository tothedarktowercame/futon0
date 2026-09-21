# Dead-code discovery — 2026-09-21

Read-only discovery by codex-15 for claude-5, job `invoke-1790024125985-23067-8906d42f`. Nothing deleted, instrumented, loaded or restarted. Only this document is committed. Observations made approximately 20:54–21:04 UTC; a loaded-namespace list is a point-in-time observation, not lifetime execution history.

**Three different answers:** substrate-2 has static code edges under **`code/v05/calls`**, not `code/calls`; the existing read-only reflection route supplies **447 loaded namespaces**; no comprehensive per-var runtime-call census was found. Zero static references, absent from `all-ns`, and never executed are different propositions. This report supplies candidates for inspection, not a deletion authorization.

## A. Static graph: data exists, freshness is not certified

`futon1b/s2_s3.clj` is explicitly a synthetic seed/query exercise. Its `:code/calls` name is not evidence that the operational graph uses that type. `M-mission-scopes-into-substrate-2.md` is a mission charter describing the graph/mission integration, not an up-to-date code inventory. `futon5/holes/missions/M-differentiable-code.md` (MAP opened 2026-05-31) explicitly distinguishes the existing ingredients from a connected loop; it supplies no current runtime-call census.

Real GETs to `http://localhost:7073/api/alpha/hyperedges?type=TYPE&limit=1000` returned:

| Type | Rows returned | Interpretation |
|---|---:|---|
| code/calls | 0 | Wrong name for the observed production call projection. |
| code/requires | 0 | No rows found; historical ingestion of this type is not established. |
| code/ns-contains | 0 | No rows found under this name. |
| code/namespace; code/var | 0 each | No rows found under these names. |
| code/v05/calls | 1000 + cursor | Static symbol-reference edges exist. Not a total. |
| code/v05/contains | 1000 + cursor | Namespace/var containment exists. |
| code/v05/var | 1000 + cursor | Definition vertices exist. |
| code/v05/test | 1000 + cursor | Test vertices exist. |
| code/v05/namespace | 2375 across three pages | Full cursor walk at inspection, 2375 unique IDs. |

Responses use `count-exact? false`; do not report a limited page count as a corpus total. I followed `next-cursor` as `after` for the namespace census. A request to `/api/alpha/types` returned `Invalid token: :`; it did not provide a usable catalogue. No deep-health request was made.

The operational writers are `futon3/scripts/ingest_v05_to_futon1a.clj:406–466` and `ingest_one_file.clj:312–367`. They emit `code/v05/namespace`, `/var`, `/test`, `/calls`, `/coverage`, `/contains`, `/vocabulary-use`, and `/term-defines`. `parse-requires` supports symbol resolution; it does **not** imply that a `code/requires` edge is emitted. `calls` in this projector means resolved syntactic symbols, not measured invocation counts. Direction is also retained in a third `dir:source→target` endpoint. Old API names must not be used as negative evidence about those edges.

Historical ingestion is independently retained: `futon1b/seed/substrate-slice-manifest.edn` records a **2026-07-04 export from the live futon1a store**, including 150 sampled `code/v05/calls` and 150 each of contains/var/coverage. `migration/export.clj` includes both old and v05 names in a discovery list; a name in that list is not proof that the type was ever populated. I found no retained positive ingestion evidence for the literal `code/requires` type and do not turn that into “never ingested.”

Current namespace census by repo label, with a separate `code/v05/calls&repo=LABEL&limit=1` positive check for every named label:

| Repo label | Namespace rows | Call edge observed |
|---|---:|---|

| futon0-d | 34 | yes |
| futon1-d | 71 | yes |
| futon1a-d | 2 | yes |
| futon2-d | 891 | yes |
| futon3-d | 71 | yes |
| futon3a-d | 8 | yes |
| futon3b-d | 2 | yes |
| futon3c-d | 784 | yes |
| futon4-elisp-d | 5 | yes |
| futon5-d2 | 218 | yes |
| futon5a-d | 15 | yes |
| futon6-py-d | 229 | yes |
| futon7-d | 12 | yes |
| unknown | 33 | not attributed |

Repo labels are stored identifiers, not an assertion that their `-d` suffix names a current checkout. The census includes tests, lab scripts, historic and stale namespaces; it is not restricted to current `src` files.

**Freshness finding:** 2,342 namespace rows have `prop/source-file`; 58 are explicitly marked `prop/witness-stale`, with stale reason/time/last-known hash. The ordinary rows have no commit pin, ingestion timestamp or current content hash in the returned projection. Therefore neither complete freshness nor “last refreshed at X” can be certified. In the futon2-d subset, 43 recorded source paths are now absent, 42 marked stale; futon3c-d has five absent paths, all marked stale. The first futon3c-d call-edge sample still points into deleted `.generate_s2_census.py`, without a stale flag on that edge. Conversely, a call sample names today's `futon0/analysis/audits/park-wake-pilot-2026-09-21/analyze.py`. The graph is being populated but mixes current and historical structure. A stale-aware, source-pinned graph view is missing; a zero incoming-edge count from this graph alone cannot justify deletion.

All eight E6b namespaces have graph namespace vertices. That means they were indexed, not that they were called.

## Fresh clj-kondo comparison and candidate rules

Executed, without cache writes or a JVM:

```sh
clj-kondo --lint /home/joe/code/futon2/src /home/joe/code/futon3c/src   --cache false --config '{:analysis true :output {:format :json}}'
```

Tool: clj-kondo v2026.08.04. Output: 652 namespace definitions, 2,600 namespace usages, 11,807 var definitions and 179,310 var usages. This is a static reference analysis, not a call trace. The lint run also reports 404 errors (400 unresolved-symbol, four invalid-arity), 69 warnings and 23 informational findings; this is **not a green lint gate or proof of complete resolution**. Definitions/usages are still useful, but unresolved/generated/dynamic references can make apparent non-use false.

- Inspected futon2 HEAD: `c6ae03fc8d61a74a22fa37e82ffbc7a48a83032f`.
- Inspected futon3c HEAD: `a70c20e85f7ea64f4355b5d94fd78b367d288224`.

Methods, held apart:

1. **Unreferenced var:** a definition `(ns,name)` has no resolved var usage in the two src trees, excluding a self-reference from that same var. Result: **1,183 definition records**, listed below. These include entrypoints, macros and exported APIs; they are not all dead.
2. **Unrequired namespace:** no `namespace-usages` edge from another namespace in that same scope. Result: **206**, listed below.
3. **Broader static inbound:** union of cross-namespace namespace usages and var usages. Namespaces with none, plus the eight-member E6b cluster, form **212 inspection rows** below. E6b internal edges are retained, not erased.
4. **Supplemental textual check:** exact namespace-boundary matches across futon2/futon3c `src`, `test`, `dev`, and `scripts` code files. This catches literal `requiring-resolve`, `ns-resolve`, CLI strings and out-of-scope dev/test requires. It also finds comments: these are flagged text mentions, not automatically calls. Other repos, root-level launch configuration and constructed symbols remain outside this supplemental check.
5. **Conservative shortlist:** not loaded in the observed futon3c JVM, no external resolved inbound in the src analysis, and no non-test supplemental mention outside the candidate's own file (or outside E6b for its members). Result: **110**. This is a search priority only, especially for CLI entrypoints and other-JVM consumers.

The largest raw static misses illustrate why the extra checks matter: `mission-scope-ingest` (2,254 lines) has real `requiring-resolve` callers in `watcher/scope_reingest.clj:66` and `watcher/multi.clj:289`; `aif/stack-generator` has an HTTP `requiring-resolve` caller at `transport/http.clj:8342`. `watcher.multi`, `agents.apm-work-queue`, `agents.tickle-orchestrate`, `transport.irc`, `agents.codex-cli` and `logic.locus` are already loaded, and several are required from `dev`. None should be deleted on the basis of a src-only kondo miss.

### Top ten inspection priorities, after those checks

All ten are **loaded: no** in the snapshot, not “never loaded.” Tests and external commands may still need them. Lines count the whole file, including comments. Commit dates are committer dates; the shared Git author is not reliably the authoring agent. Agent identity is unknown except where a retained rollout commit-creation record supplies it.

| Namespace | Lines | Last commit date / Git author | Authoring agent | Static external inbound | Test text refs |
|---|---:|---|---|---|---:|

| `futon2.aif.full-loop-cli` | 737 | 2026-08-31T21:47:54+00:00 / Joseph Corneli | unknown | 0 in scope | 3 |
| `futon2.aif.bulletin` | 668 | 2026-09-08T15:42:35+00:00 / Joseph Corneli | unknown | 0 in scope | 1 |
| `futon3c.peripheral.war-machine-pilot` | 644 | 2026-09-17T13:53:28+00:00 / Joseph Corneli | unknown | 0 in scope | 7 |
| `futon2.aif.node-sim` | 511 | 2026-09-05T10:22:54+00:00 / Joseph Corneli | unknown | 0 in scope | 1 |
| `futon2.aif.arguing-worlds` | 459 | 2026-07-14T00:22:41+01:00 / Joseph Corneli | unknown | 0 in scope | 1 |
| `futon2.aif.machine-slow-feedback-evidence` | 429 | 2026-09-13T05:57:39+00:00 / Joseph Corneli | codex-23 | 0 outside E6b | 2 |
| `futon3c.peripheral.street-sweeper` | 429 | 2026-06-09T14:06:03+01:00 / Joseph Corneli | unknown | 0 in scope | 1 |
| `futon3c.apm.frame18-control` | 407 | 2026-08-23T19:18:15+00:00 / Joseph Corneli | unknown | 0 in scope | 4 |
| `futon3c.vsatarcs.feeder` | 405 | 2026-05-25T19:46:00+01:00 / Joseph Corneli | unknown | 0 in scope | 1 |
| `futon2.aif.work-target-store` | 394 | 2026-09-15T20:53:54+00:00 / Joseph Corneli | unknown | 0 in scope | 3 |

### E6b control

The eight-file cluster totals **2,492 lines**. All eight are absent from the observed `all-ns` list. Kondo finds **zero incoming namespace/var edges from outside the cluster** in the two src trees. It correctly retains internal dependencies (store-v2 uses store; capture uses store-v2/provenance; projection uses capture/provenance; completeness uses capture/projection). Supplemental test references exist. Thus the control passes at **cluster level**; claiming every file individually has zero references would be false.

| Namespace | Lines | Internal inbound namespaces | Loaded |
|---|---:|---|---|

| `futon2.aif.machine-slow-feedback-evidence` | 429 | none | no |
| `futon2.aif.machine-slow-feedback-store-v2` | 382 | futon2.aif.machine-slow-feedback-capture | no |
| `futon2.aif.machine-slow-state-carrier` | 361 | futon2.aif.machine-slow-feedback-provenance | no |
| `futon2.aif.machine-slow-feedback-store` | 317 | futon2.aif.machine-slow-feedback-store-v2 | no |
| `futon2.aif.machine-slow-feedback-provenance` | 300 | futon2.aif.machine-slow-feedback-capture, futon2.aif.machine-slow-feedback-retrospective-projection, futon2.aif.machine-slow-feedback-store-v2 | no |
| `futon2.aif.machine-slow-feedback-capture` | 283 | futon2.aif.machine-slow-feedback-completeness, futon2.aif.machine-slow-feedback-retrospective-projection | no |
| `futon2.aif.machine-slow-feedback-completeness` | 272 | none | no |
| `futon2.aif.machine-slow-feedback-retrospective-projection` | 148 | futon2.aif.machine-slow-feedback-completeness | no |

## B. Loaded namespaces and the eval refusal

`GET http://localhost:7070/api/alpha/reflect/namespaces` returned `ok: true`, `count: 447`. The handler is `futon3c/transport/http.clj:7345`; `futon3c/reflection/core.clj:51` directly maps `all-ns`. This route does not require or evaluate a requested namespace. It supplies actual loaded membership, not a guess from source imports. Use it for this question; no instrumentation is needed.

**The blanket premise that proof-eval is now forbidden does not hold from the canonical directory.** The ordinary script, with the existing environment and no supplied alternative credential, returned:

```text
cwd=/home/joe/code/futon3c
scripts/proof-eval.sh '(count (all-ns))'
{:ok true, :value 447}

cwd=/home/joe
/home/joe/code/futon3c/scripts/proof-eval.sh '(count (all-ns))'
forbidden
```

Cause reproduced: `proof-eval.sh:58–69` resolves token environment variables and then **`.admintoken relative to the caller's cwd`**, otherwise its default. It does not first move to its own repo. `dev/futon3c/dev/config.clj:28` uses the same precedence relative to the server cwd. Read-only process inspection found the server rooted at `/home/joe/code/futon3c`, token selected from its `.admintoken`, default loopback allowlist; current canonical-client and server-selected token values compare equal. `/home/joe/.admintoken` is absent. `repl/http.clj:36–55` returns the exact plain `forbidden` response on token mismatch or disallowed address. No secret value is recorded here.

This demonstrates a working-directory-dependent authentication mismatch **now**; it does not prove that Joe's restart changed a token, nor identify the cwd/environment of claude-5's earlier failed request. That historical cause is not recoverable from a bare 403. I did not rotate tokens, change configuration, use a privileged alternate endpoint or reload code. The two eval forms only read `all-ns`; loaded membership was obtained independently via the purpose-built GET route.

Loaded is not called: namespaces can initialize via startup requires, prior REPL exploration, registration, or tests. Conversely, an unloaded namespace may be an important lazy route, a CLI, or used in the separate futon1b JVM. Absent here means absent in this one futon3c process at this time.

## C. Called at runtime: proposal only

No retained comprehensive per-var call-counter surface was found in the inspected reflection routes or static graph. Agency jobs and WM receipts show named operations, and some record specific functions, but do not enumerate every actual invocation or uncalled function. Their absence cannot prove non-execution. No profiler or wrapper was started.

| Method | Honest evidence | Cost / limits |
|---|---|---|
| Existing Agency/WM receipts | Particular retained operation ran, where the receipt binds it | No new runtime overhead. Manual joins; sparse and self-reporting unless independently observed. Cannot establish negative coverage. |
| Scoped per-var counters in a **test JVM** | Wrapped entry was invoked during a named workload | Proposed cheapest exact positive control for a small suspect cluster: one counter increment plus wrapper dispatch per observed call; memory O(number of vars). Extra startup/workload run. Do not wrap macros or indiscriminately replace multimethods/primitive-specialized roots. Direct-linked calls, cached function values, protocol dispatch, generated classes and `recur` may bypass roots. Zero means only unobserved by this instrument/workload. |
| JFR sampling in an explicitly authorized serving interval | Sampled executing method stacks | Default settings are documented as low overhead; profile settings cost more. Bounded recording size/window controls disk cost, not a promised percentage. Sampling misses short/rare calls and cannot prove a var was never called; generated Clojure methods need source mapping. Starting a recording changes diagnostics state, so it was not done here. |
| Cloverage on selected tests in isolation | Instrumented forms exercised by those tests | Additional instrumented load/test run, counter overhead and potentially altered compilation/macro behavior. Cost must be benchmarked for these tests. Test coverage is not production coverage; no hit does not certify safe deletion. |

Recommendation: inspect E6b as a component first, then use a deliberately scoped isolated workload with counters only if the question is whether that workload enters it. Preserve source SHA, monitored var set, workload, duration, dropped/error observations and a **known-called control**. For “what runs in production,” a separately authorized bounded JFR recording supplies positive samples with less invasive changes than replacing serving var roots. Neither method turns absence of observations into proof of lifetime dead code.

Primary references: [Oracle jcmd/JFR command specification](https://docs.oracle.com/en/java/javase/21/docs/specs/man/jcmd.html) documents default/profile overhead and bounded recordings; [Cloverage's own repository](https://github.com/cloverage/cloverage) describes form-level test instrumentation. The relative cost estimates above are proposals, not measurements on this stack.

## Candidate inventory and false positives

Every row below is sorted by file lines. `review` denotes the conservative shortlist; `flag` means a static miss contradicted or qualified by loaded membership or supplemental mentions. Inbound counts are scoped to the two src trees. Text references are reported separately because comments are not calls. Last agent is unknown unless the preceding forensic rollout inventory directly ties that exact commit to a seat. Git author “Joseph Corneli” alone does not identify Joe or a specific agent.

Important exclusions: Lean definitions/imports are not Clojure vars and are not measured by this kondo run; their unusedness requires a separate Lean dependency analysis. Reader-conditionals, custom macros, unresolved symbols, generated registrations, `requiring-resolve`, `ns-resolve`, protocol/multimethod dispatch, test-only use, CLI entrypoints and consumers in other repositories all require follow-up before deletion.

<details><summary>212 namespace inspection rows with provenance and references</summary>

| Namespace / path | Lines | Status; loaded | Inbound namespaces | Supplemental code mentions; test mentions | Last commit / date / Git author / agent |
|---|---:|---|---|---|---|

| `futon3c.scripts.mission-scope-ingest`<br>`futon3c/src/futon3c/scripts/mission_scope_ingest.clj` | 2254 | flag; yes | 0 | 4 (futon3c/src/futon3c/watcher/scope_reingest.clj:66; futon3c/src/futon3c/watcher/multi.clj:289; futon3c/scripts/mission-scope-reingest.sh:29; futon3c/scripts/mission-scope-reingest.sh:37); tests 1 | `782d112b5b82dcc43bc29f0697fd3e80a2fc0f05` / 2026-08-25T09:37:02+00:00 / Joseph Corneli / unknown |
| `futon3c.watcher.multi`<br>`futon3c/src/futon3c/watcher/multi.clj` | 1772 | flag; yes | 0 | 9 (futon2/src/futon2/aif/mission_registry.clj:239; futon3c/src/futon3c/watcher/commit_ingest.clj:3; futon3c/src/futon3c/watcher/commit_ingest.clj:622; futon3c/src/futon3c/watcher/file_ingest.clj:3); tests 6 | `540cddce1c6b44629de1aff04513bcbf8fa66264` / 2026-09-17T22:41:31+00:00 / Joseph Corneli / unknown |
| `futon3c.agents.apm-work-queue`<br>`futon3c/src/futon3c/agents/apm_work_queue.clj` | 924 | flag; yes | 0 | 4 (futon3c/dev/futon3c/dev/apm_conductor_v2.clj:24; futon3c/dev/futon3c/dev/apm.clj:18; futon3c/dev/futon3c/dev/apm_conductor.clj:17; futon3c/dev/futon3c/dev/apm_conductor_v3.clj:18); tests 3 | `7774a839ee4d6b0bd2b0d046b94e58cbcfd6b15b` / 2026-07-04T19:05:47+01:00 / Joseph Corneli / unknown |
| `futon3c.agents.tickle-orchestrate`<br>`futon3c/src/futon3c/agents/tickle_orchestrate.clj` | 909 | flag; yes | 0 | 11 (futon3c/dev/futon3c/dev/fm.clj:7; futon3c/dev/futon3c/dev/apm.clj:17; futon3c/dev/futon3c/dev/ct.clj:7; futon3c/dev/futon3c/dev/ct.clj:175); tests 1 | `7635fdc6d947e6c3fbd0299a82808c64d4188dff` / 2026-09-03T11:03:06+00:00 / Joseph Corneli / unknown |
| `futon2.aif.full-loop-cli`<br>`futon2/src/futon2/aif/full_loop_cli.clj` | 737 | review; no | 0 | 0 (none); tests 3 | `9b0dac13457267522dcd6174733716059e6449af` / 2026-08-31T21:47:54+00:00 / Joseph Corneli / unknown |
| `futon3c.transport.irc`<br>`futon3c/src/futon3c/transport/irc.clj` | 706 | flag; yes | 0 | 4 (futon3c/dev/futon3c/dev/bootstrap.clj:26; futon3c/scripts/irc_live_gate.clj:28; futon3c/scripts/irc_chat_relay.clj:20; futon3c/scripts/irc_claude_relay.clj:14); tests 5 | `bac7422cdf3c213f7cf1e9261cd26b7d8368b7b1` / 2026-05-01T19:50:31+01:00 / Joseph Corneli / unknown |
| `futon3c.aif.stack-generator`<br>`futon3c/src/futon3c/aif/stack_generator.clj` | 693 | flag; no | 0 | 2 (futon3c/src/futon3c/transport/http.clj:8342; futon3c/src/futon3c/transport/http.clj:8357); tests 1 | `18fde4fe7a21b58d79587ae0bab20b779f2e6520` / 2026-09-17T18:46:01+00:00 / Joseph Corneli / unknown |
| `futon3c.agents.codex-cli`<br>`futon3c/src/futon3c/agents/codex_cli.clj` | 689 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev.clj:52); tests 13 | `baf11a492d14f31dd9e8f83833654e2cfe30f822` / 2026-09-21T19:02:54+00:00 / Joseph Corneli / unknown |
| `futon2.aif.bulletin`<br>`futon2/src/futon2/aif/bulletin.clj` | 668 | review; no | 0 | 0 (none); tests 1 | `d17f1088c5121b7bf6a2e641ef47525ea6372927` / 2026-09-08T15:42:35+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.locus`<br>`futon3c/src/futon3c/logic/locus.clj` | 660 | flag; yes | 0 | 2 (futon3c/src/futon3c/logic/metabolic_balance.clj:288; futon3c/dev/futon3c/dev/bootstrap.clj:18); tests 2 | `1b88d4dd792bc0e647d0c7d63ef29bd0ca9a5e0b` / 2026-07-13T20:26:06+01:00 / Joseph Corneli / unknown |
| `futon3c.proof.bridge`<br>`futon3c/src/futon3c/proof/bridge.clj` | 646 | flag; no | 0 | 3 (futon3c/src/futon3c/agents/tickle_orchestrate.clj:688; futon3c/src/futon3c/agents/tickle_orchestrate.clj:689; futon3c/scripts/proof-eval.sh:5); tests 1 | `4b05d8d151550ae1b02d0fce97e5a4833695021f` / 2026-03-20T17:45:19+00:00 / Robert Meyers / unknown |
| `futon3c.peripheral.war-machine-pilot`<br>`futon3c/src/futon3c/peripheral/war_machine_pilot.clj` | 644 | review; no | 0 | 0 (none); tests 7 | `d0cc8605ca744ec3c722147c749afd73a7f4e0b4` / 2026-09-17T13:53:28+00:00 / Joseph Corneli / unknown |
| `futon2.aif.run-narrative`<br>`futon2/src/futon2/aif/run_narrative.clj` | 626 | flag; no | 0 | 1 (futon2/src/futon2/aif/load_identity.clj:35); tests 5 | `c7a68fa9c9d85073cfc53e40951e346210676ec7` / 2026-09-21T18:13:55+00:00 / Joseph Corneli / unknown |
| `futon2.aif.mission-c`<br>`futon2/src/futon2/aif/mission_c.clj` | 623 | flag; no | 0 | 3 (futon2/src/futon2/aif/ruled_outcome_c.clj:99; futon2/scripts/wm_status_report.py:160; futon2/scripts/futon2/report/war_machine.clj:68); tests 2 | `6ca58f3c21371d6ce554770c18047a935fac2aa7` / 2026-09-03T16:51:03+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.cascade-real-live`<br>`futon3c/src/futon3c/logic/cascade_real_live.clj` | 593 | flag; yes | 0 | 2 (futon3c/src/futon3c/transport/http.clj:8476; futon3c/src/futon3c/transport/http.clj:8490); tests 1 | `d0741b044e3d4a697bd606e1eb00089f31d90673` / 2026-09-12T14:38:48+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.ftriangle-live-smoke`<br>`futon3c/src/futon3c/apm/ftriangle_live_smoke.clj` | 543 | flag; no | 0 | 2 (futon3c/scripts/apm-ftriangle-preflight.sh:7; futon3c/scripts/apm-ftriangle-run.sh:13); tests 1 | `5f86a89b53120579848a0a56f465831018c02bba` / 2026-08-28T09:41:13+00:00 / Joseph Corneli / unknown |
| `futon2.aif.node-sim`<br>`futon2/src/futon2/aif/node_sim.clj` | 511 | review; no | 0 | 0 (none); tests 1 | `9a06490226caf770d45cdebb37f9a899476da817` / 2026-09-05T10:22:54+00:00 / Joseph Corneli / unknown |
| `futon2.aif.task-belief-ladder`<br>`futon2/src/futon2/aif/task_belief_ladder.clj` | 507 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:86); tests 1 | `4e1fd48d1c53b5877bc8484cb7d7cface41a6f15` / 2026-09-05T08:30:00+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.scheduler`<br>`futon3c/src/futon3c/wm/scheduler.clj` | 482 | flag; no | 0 | 12 (futon3c/src/futon3c/aif/stack_generator.clj:425; futon3c/src/futon3c/aif/stack_generator.clj:487; futon3c/src/futon3c/wm/runner_service.clj:79; futon3c/src/futon3c/transport/http.clj:8173); tests 14 | `d0cc8605ca744ec3c722147c749afd73a7f4e0b4` / 2026-09-17T13:53:28+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.probe-taps`<br>`futon3c/src/futon3c/logic/probe_taps.clj` | 461 | flag; no | 0 | 1 (futon3c/src/futon3c/logic/probe.clj:128); tests 1 | `10d8145afb4d22ad624079d25cafa7c6a957a5fb` / 2026-07-10T09:16:20+01:00 / Joseph Corneli / unknown |
| `futon2.aif.arguing-worlds`<br>`futon2/src/futon2/aif/arguing_worlds.clj` | 459 | review; no | 0 | 0 (none); tests 1 | `9d8f2dee099382e19467c92152ed0805c1b0f337` / 2026-07-14T00:22:41+01:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.live-wm-selection`<br>`futon3c/src/futon3c/peripheral/live_wm_selection.clj` | 456 | flag; no | 0 | 4 (futon2/scripts/futon2/run_tick_once.clj:19; futon3c/src/futon3c/wm/runner_service.clj:186; futon3c/src/futon3c/transport/http.clj:8729; futon3c/scripts/run_live_wm_selection_verify.clj:3); tests 5 | `35022e4273aedf39365a4672b2e4648b3704b5aa` / 2026-09-15T13:52:43+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.metabolic-balance`<br>`futon3c/src/futon3c/logic/metabolic_balance.clj` | 433 | flag; yes | 0 | 3 (futon2/scripts/futon2/report/war_machine.clj:3873; futon3c/dev/futon3c/dev/bootstrap.clj:456; futon3c/dev/futon3c/dev/bootstrap.clj:481); tests 2 | `666ddd75e621591227d92a484afaf5ff3f4606eb` / 2026-05-25T19:46:00+01:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-slow-feedback-evidence`<br>`futon2/src/futon2/aif/machine_slow_feedback_evidence.clj` | 429 | review; no | 0 | 0 (none); tests 2 | `19b0773004e791b69d99986aae3798c6925a5301` / 2026-09-13T05:57:39+00:00 / Joseph Corneli / codex-23 |
| `futon3c.peripheral.street-sweeper`<br>`futon3c/src/futon3c/peripheral/street_sweeper.clj` | 429 | review; no | 0 | 0 (none); tests 1 | `e753fc754a940f8d4711c778101c9ce3a1e45f55` / 2026-06-09T14:06:03+01:00 / Joseph Corneli / unknown |
| `futon3c.live-efe-map`<br>`futon3c/src/futon3c/live_efe_map.clj` | 426 | flag; no | 0 | 1 (futon3c/src/futon3c/transport/http.clj:9172); tests 2 | `006e56f7c16b0872e3d6382052126c63f96e8614` / 2026-07-22T12:27:18+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.ratchet`<br>`futon3c/src/futon3c/logic/ratchet.clj` | 415 | flag; yes | 0 | 2 (futon3c/dev/futon3c/dev/bootstrap.clj:19; futon3c/scripts/check-coverage-ratchet.sh:43); tests 2 | `557efe47ff436bfe7dd25c55fb8883f58d1b8714` / 2026-05-01T19:50:14+01:00 / Joseph Corneli / unknown |
| `futon2.aif.selection-rationale`<br>`futon2/src/futon2/aif/selection_rationale.clj` | 411 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:82); tests 2 | `5ae5536c0c7cb8e53b342e20dee45e2a9e899534` / 2026-09-17T13:52:53+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.frame18-control`<br>`futon3c/src/futon3c/apm/frame18_control.clj` | 407 | review; no | 0 | 0 (none); tests 4 | `4993addf486e8f38c814129fb202e0a062d7cc66` / 2026-08-23T19:18:15+00:00 / Joseph Corneli / unknown |
| `futon3c.vsatarcs.feeder`<br>`futon3c/src/futon3c/vsatarcs/feeder.clj` | 405 | review; no | 0 | 0 (none); tests 1 | `c9fcfd2ec86fd6fdd80175fec99336e238622cc6` / 2026-05-25T19:46:00+01:00 / Joseph Corneli / unknown |
| `futon3c.aif.calibration`<br>`futon3c/src/futon3c/aif/calibration.clj` | 400 | flag; no | 0 | 2 (futon3c/src/futon3c/transport/http.clj:8225; futon3c/src/futon3c/transport/http.clj:8226); tests 4 | `7b71ba26cd785b67686c0729f1a251b6b4d9d18a` / 2026-06-12T11:49:22+01:00 / Joseph Corneli / unknown |
| `futon2.aif.work-target-store`<br>`futon2/src/futon2/aif/work_target_store.clj` | 394 | review; no | 0 | 0 (none); tests 3 | `e38ea7e5153ce30dc094106a9ab5a904d12d3165` / 2026-09-15T20:53:54+00:00 / Joseph Corneli / unknown |
| `futon3c.nlp.classical-pipeline`<br>`futon3c/src/futon3c/nlp/classical_pipeline.clj` | 388 | review; no | 0 | 0 (none); tests 1 | `7e9484d6c1d3508c501e00c7f91b58cdb7803eff` / 2026-07-13T09:45:11+00:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-slow-feedback-store-v2`<br>`futon2/src/futon2/aif/machine_slow_feedback_store_v2.clj` | 382 | review; no | futon2.aif.machine-slow-feedback-capture | 0 (none); tests 4 | `3914e9068b160c62f2f6e87f94a4ca2444f54577` / 2026-09-13T07:42:12+00:00 / Joseph Corneli / codex-23 |
| `futon2.aif.adapters.fulab`<br>`futon2/src/futon2/aif/adapters/fulab.clj` | 378 | review; no | 0 | 0 (none); tests 1 | `497dca72c4b57ba938c85821345ca0d322dbf57c` / 2026-09-02T09:31:48+00:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.proof-logic`<br>`futon3c/src/futon3c/peripheral/proof_logic.clj` | 377 | flag; no | 0 | 2 (futon2/scripts/futon2/report/war_machine.clj:5437; futon3c/src/futon3c/transport/http.clj:7672); tests 1 | `711ed8e6e1968f3deb9b2cb6b85b6747c9384228` / 2026-03-15T17:14:34+00:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.adapter`<br>`futon3c/src/futon3c/peripheral/adapter.clj` | 377 | review; no | 0 | 0 (none); tests 3 | `e5eb6712c31f7879d11ce65692c7c875e8fa61b6` / 2026-08-15T13:03:10+00:00 / Joseph Corneli / unknown |
| `futon2.aif.enact`<br>`futon2/src/futon2/aif/enact.clj` | 374 | flag; no | 0 | 5 (futon2/src/futon2/aif/trace.clj:561; futon2/scripts/gate_l1_legacy_cold.clj:12; futon2/scripts/gate_l1_legacy_cold.clj:28; futon2/scripts/wm_scheduled_run.clj:25); tests 7 | `5ae5536c0c7cb8e53b342e20dee45e2a9e899534` / 2026-09-17T13:52:53+00:00 / Joseph Corneli / unknown |
| `futon2.aif.mission-gauges`<br>`futon2/src/futon2/aif/mission_gauges.clj` | 367 | flag; no | 0 | 2 (futon2/scripts/futon2/report/war_machine.clj:69; futon2/scripts/futon2/report/war_machine.clj:1996); tests 2 | `38136445103f6b03bcdda0754b070b6edb8be17f` / 2026-09-08T17:52:10+00:00 / Joseph Corneli / unknown |
| `futon3c.process-watchdog`<br>`futon3c/src/futon3c/process_watchdog.clj` | 363 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev.clj:70); tests 1 | `a3ad5aaf205b3e983ad895499908f28ef6b51cd5` / 2026-05-25T20:02:20+01:00 / Joseph Corneli / unknown |
| `futon3c.apm.projection-watchdog`<br>`futon3c/src/futon3c/apm/projection_watchdog.clj` | 362 | flag; no | 0 | 1 (futon3c/scripts/apm-watch-projection.sh:49); tests 1 | `83337414bae153409347f108d35adc839c1a8d4e` / 2026-09-02T06:53:03+00:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-slow-state-carrier`<br>`futon2/src/futon2/aif/machine_slow_state_carrier.clj` | 361 | review; no | futon2.aif.machine-slow-feedback-provenance | 0 (none); tests 2 | `cbbee14e782fc13e1bc70ae202c0f1f440c1c983` / 2026-09-13T06:21:43+00:00 / Joseph Corneli / codex-23 |
| `futon2.aif.enumeration-completeness`<br>`futon2/src/futon2/aif/enumeration_completeness.clj` | 357 | flag; no | 0 | 2 (futon2/scripts/futon2/report/war_machine.clj:61; futon2/scripts/futon2/report/war_machine.clj:1735); tests 3 | `71fd1420ca826ed489331772f98840b04ea628bc` / 2026-09-19T00:09:55+00:00 / Joseph Corneli / unknown |
| `futon3c.aif.mission-delta-t`<br>`futon3c/src/futon3c/aif/mission_delta_t.clj` | 357 | flag; no | 0 | 4 (futon2/src/futon2/aif/mission_epistemic_value.clj:344; futon2/scripts/futon2/report/war_machine.clj:263; futon2/scripts/futon2/report/war_machine.clj:2170; futon3c/scripts/aif2_tension_spike.clj:15); tests 1 | `053e1a98e54aa243d03e51c3980da7fe44860757` / 2026-07-26T20:21:50+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.snapshot`<br>`futon3c/src/futon3c/logic/snapshot.clj` | 352 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev/bootstrap.clj:20); tests 2 | `1bf1f81a59ab0d43217e73caa9666a7b25a396c4` / 2026-05-03T19:08:18+01:00 / Joseph Corneli / unknown |
| `repl.http`<br>`futon3c/src/repl/http.clj` | 352 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev/bootstrap.clj:34); tests 2 | `916417e3d2918813707f5e63dbc478237207e526` / 2026-09-20T17:28:06+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.run4-series-queue`<br>`futon3c/src/futon3c/wm/run4_series_queue.clj` | 341 | review; no | 0 | 0 (none); tests 4 | `70a2805554ea8ac76bf0af2d2ae142faf5ed14af` / 2026-09-11T14:49:22+00:00 / Joseph Corneli / unknown |
| `futon3c.agents.tickle-work-queue`<br>`futon3c/src/futon3c/agents/tickle_work_queue.clj` | 341 | flag; yes | 0 | 2 (futon3c/dev/futon3c/dev.clj:57; futon3c/dev/futon3c/dev/ct.clj:8); tests 3 | `a14ebceab8411705d48647ca969fef239b02fa15` / 2026-09-03T11:07:31+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.library-loop-tools`<br>`futon3c/src/futon3c/apm/library_loop_tools.clj` | 333 | review; no | 0 | 0 (none); tests 1 | `99740c7d62c4acb371ca04874ef4c1da46fe4cf8` / 2026-09-05T11:59:59+00:00 / Joseph Corneli / unknown |
| `futon3c.portfolio-inference.scheduler`<br>`futon3c/src/futon3c/portfolio_inference/scheduler.clj` | 329 | flag; no | 0 | 1 (futon3c/src/futon3c/transport/http.clj:8318); tests 0 | `11a364b27671f80c23e8754327ceb8e5206ca30a` / 2026-05-03T19:08:18+01:00 / Joseph Corneli / unknown |
| `futon3c.agents.tickle-queue`<br>`futon3c/src/futon3c/agents/tickle_queue.clj` | 325 | flag; yes | 0 | 2 (futon3c/src/futon3c/blackboard.clj:863; futon3c/dev/futon3c/dev/fm.clj:8); tests 2 | `18a15de52e1e6df0d8b2560a4c83f37d7e8a8009` / 2026-03-10T12:09:26+00:00 / Joe Corneli / unknown |
| `futon2.aif.machine-slow-feedback-store`<br>`futon2/src/futon2/aif/machine_slow_feedback_store.clj` | 317 | review; no | futon2.aif.machine-slow-feedback-store-v2 | 0 (none); tests 3 | `79c957a2f1998c8b6948b07a04087fb81d4f610e` / 2026-09-13T05:44:33+00:00 / Joseph Corneli / codex-26 |
| `futon3c.agency.fed-uplink`<br>`futon3c/src/futon3c/agency/fed_uplink.clj` | 316 | flag; yes | 0 | 3 (futon3c/src/futon3c/agency/registry.clj:65; futon3c/src/futon3c/agency/registry.clj:69; futon3c/dev/futon3c/dev/bootstrap.clj:5); tests 1 | `5287650c12cd1006f67ae65898e15a1c0a2b225c` / 2026-08-14T10:33:07+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.substrate-metric-e1-invariants`<br>`futon3c/src/futon3c/logic/substrate_metric_e1_invariants.clj` | 312 | review; no | 0 | 0 (none); tests 1 | `3eb5d474aa6875d88ef4b1234ee5f9d7fbe698b6` / 2026-06-01T19:17:29+01:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-slow-feedback-provenance`<br>`futon2/src/futon2/aif/machine_slow_feedback_provenance.clj` | 300 | review; no | futon2.aif.machine-slow-feedback-capture, futon2.aif.machine-slow-feedback-retrospective-projection, futon2.aif.machine-slow-feedback-store-v2 | 0 (none); tests 3 | `b204d9097a622e9d611f44134bddc06ec9854139` / 2026-09-13T06:58:13+00:00 / Joseph Corneli / codex-23 |
| `futon3c.peripheral.dynamic-queries-rung4`<br>`futon3c/src/futon3c/peripheral/dynamic_queries_rung4.clj` | 300 | flag; no | 0 | 1 (futon3c/scripts/run_dynamic_queries_rung4_demo.clj:3); tests 1 | `35f1fef44481e2f06022c6a1d39e28080d8d3ff3` / 2026-07-27T07:50:39+01:00 / Joseph Corneli / unknown |
| `futon3c.agency.roster-store`<br>`futon3c/src/futon3c/agency/roster_store.clj` | 299 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev/bootstrap.clj:10); tests 1 | `f64629995e695009ae42abe3faa63dd51eeba62a` / 2026-08-26T10:55:09+00:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-slow-feedback-capture`<br>`futon2/src/futon2/aif/machine_slow_feedback_capture.clj` | 283 | review; no | futon2.aif.machine-slow-feedback-completeness, futon2.aif.machine-slow-feedback-retrospective-projection | 0 (none); tests 3 | `b129248be54d6f887764a8adc5aacdf4b5301546` / 2026-09-13T07:42:56+00:00 / Joseph Corneli / codex-23 |
| `futon2.aif.contextual-preferences`<br>`futon2/src/futon2/aif/contextual_preferences.clj` | 281 | review; no | 0 | 0 (none); tests 2 | `0b5cd41151b8615696c7cc29468234318a2abc0f` / 2026-09-12T14:03:42+00:00 / Joseph Corneli / unknown |
| `futon2.wm-run-lock`<br>`futon2/src/futon2/wm_run_lock.clj` | 278 | flag; no | 0 | 1 (futon2/scripts/futon2/run_tick_once.clj:12); tests 1 | `48b112c136d7d6b2fe61f1453752a73a0826230f` / 2026-09-01T18:04:34+00:00 / Joseph Corneli / unknown |
| `futon3c.agents.codex-code-logic`<br>`futon3c/src/futon3c/agents/codex_code_logic.clj` | 276 | flag; no | 0 | 2 (futon2/scripts/futon2/report/war_machine.clj:5439; futon3c/src/futon3c/transport/http.clj:7674); tests 1 | `17b34d33d1469ab06bd6d726c99804d40cdcfa9e` / 2026-03-10T07:14:33+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.wm-operator-lane-invariants`<br>`futon3c/src/futon3c/logic/wm_operator_lane_invariants.clj` | 275 | flag; no | 0 | 1 (futon3c/src/futon3c/wm/operator_lane.clj:20); tests 1 | `2c0c6c4f36c25839a5f132458f89ecabe5f3f681` / 2026-06-05T12:13:58+01:00 / Joseph Corneli / unknown |
| `futon3c.apm.csquare-synthetic-campaign`<br>`futon3c/src/futon3c/apm/csquare_synthetic_campaign.clj` | 274 | flag; no | 0 | 1 (futon3c/scripts/apm-csquare-run.sh:5); tests 1 | `79dc962df96a30861a32920a21179bdd73ad4f19` / 2026-09-05T11:33:14+00:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-slow-feedback-completeness`<br>`futon2/src/futon2/aif/machine_slow_feedback_completeness.clj` | 272 | review; no | 0 | 0 (none); tests 1 | `9aa7459721f768c5b791009e300819f3986f5d01` / 2026-09-13T08:19:35+00:00 / Joseph Corneli / codex-23 |
| `futon3c.logic.typed-bells-invariants`<br>`futon3c/src/futon3c/logic/typed_bells_invariants.clj` | 272 | review; no | 0 | 0 (none); tests 1 | `b4ed15f375623f769ad743fa00a4dac232d34b78` / 2026-06-11T11:06:12+01:00 / Joseph Corneli / unknown |
| `futon3c.portfolio-inference.service`<br>`futon3c/src/futon3c/portfolio_inference/service.clj` | 271 | review; no | 0 | 0 (none); tests 0 | `11a364b27671f80c23e8754327ceb8e5206ca30a` / 2026-05-03T19:08:18+01:00 / Joseph Corneli / unknown |
| `futon2.aif.active-horizon-g`<br>`futon2/src/futon2/aif/active_horizon_g.clj` | 270 | flag; no | 0 | 3 (futon2/src/futon2/aif/parameter_delivery.clj:139; futon2/src/futon2/aif/construction.clj:25; futon2/src/futon2/aif/construction.clj:196); tests 3 | `8063903b3b0d604105011b27acee4e2828d47a1b` / 2026-09-17T20:25:45+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.outing-invariants`<br>`futon3c/src/futon3c/logic/outing_invariants.clj` | 264 | flag; no | 0 | 1 (futon3c/src/futon3c/logic/aif2_invariants.clj:5); tests 0 | `073497a560f69345483593e32a53894b6487e17c` / 2026-05-30T21:03:57+01:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.strategic-embedding-experiment`<br>`futon3c/src/futon3c/peripheral/strategic_embedding_experiment.clj` | 255 | flag; no | 0 | 1 (futon3c/scripts/run_phase6b_embedding_experiment.clj:5); tests 1 | `8fb64ab2d86d8d8ba41bf85e63fe125b75fd5ca3` / 2026-08-31T13:05:03+00:00 / Joseph Corneli / unknown |
| `futon2.aif.scheduled-route-evidence`<br>`futon2/src/futon2/aif/scheduled_route_evidence.clj` | 249 | review; no | 0 | 0 (none); tests 1 | `213bf3299d3950c65305e2c5c41dbadfcbf1f774` / 2026-09-13T04:50:14+00:00 / Joseph Corneli / codex-23 |
| `futon3c.peripheral.strategic-canary`<br>`futon3c/src/futon3c/peripheral/strategic_canary.clj` | 246 | flag; no | 0 | 1 (futon3c/scripts/run_phase8_advice_only_canary.clj:3); tests 2 | `98f40cf8675095b79f6ab4dd78521b73836fd04a` / 2026-08-30T10:57:12+00:00 / Joseph Corneli / unknown |
| `futon2.aif.find-reconciliation`<br>`futon2/src/futon2/aif/find_reconciliation.clj` | 240 | review; no | 0 | 0 (none); tests 1 | `6dfc172cc20ac601b01d6ed20cf1e6dc4810afe6` / 2026-09-13T23:36:34+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.business-coupling-invariants`<br>`futon3c/src/futon3c/logic/business_coupling_invariants.clj` | 240 | flag; no | 0 | 1 (futon3c/src/futon3c/logic/outreach_intake_guard.clj:15); tests 1 | `f51b930bedf26491681d3e4f8bfe92c696327cd6` / 2026-06-03T19:42:11+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.arxana-bridge`<br>`futon3c/src/futon3c/logic/arxana_bridge.clj` | 240 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev/fm.clj:15); tests 1 | `22ea5a9f0cf506a2e087e3139a13ee72f0e6f4aa` / 2026-07-14T00:12:40+01:00 / Joseph Corneli / unknown |
| `futon2.aif.measured-a-annotation`<br>`futon2/src/futon2/aif/measured_a_annotation.clj` | 238 | review; no | 0 | 0 (none); tests 1 | `3cea94148e1a0770c3f60c61071e1aa5c85fa8b9` / 2026-09-14T11:52:09+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.pattern-revision-review`<br>`futon3c/src/futon3c/apm/pattern_revision_review.clj` | 237 | review; no | 0 | 0 (none); tests 1 | `5564c506a27e9a522af6d4d21c75a6f7b3412f9b` / 2026-09-11T19:44:14+00:00 / Joseph Corneli / unknown |
| `futon2.aif.precision`<br>`futon2/src/futon2/aif/precision.clj` | 233 | flag; no | 0 | 6 (futon2/src/futon2/aif/selection_gain.clj:4; futon2/src/futon2/aif/selection_gain.clj:67; futon2/src/futon2/aif/policy_free_energy.clj:74; futon2/scripts/futon2/report/war_machine.clj:83); tests 2 | `02e2cda7d0cbc52706046f84c3fcd19a2d060efa` / 2026-08-31T18:46:46+00:00 / Joseph Corneli / unknown |
| `futon2.aif.decision-gate`<br>`futon2/src/futon2/aif/decision_gate.clj` | 228 | flag; no | 0 | 5 (futon2/src/futon2/aif/strategic_habit.clj:49; futon2/scripts/futon2/report/war_machine.clj:43; futon2/scripts/futon2/report/war_machine.clj:6052; futon3c/src/futon3c/aif/live_recommendation.clj:9); tests 13 | `561d761ea321adadf8aee3291e3cb8f38aee1c53` / 2026-09-21T03:37:05+00:00 / Joseph Corneli / unknown |
| `futon3c.inbox-zero.batch-dispatch`<br>`futon3c/src/futon3c/inbox_zero/batch_dispatch.clj` | 225 | review; no | 0 | 0 (none); tests 2 | `8e72e6fecf82f90abd036354eb39ad9ccfeced8a` / 2026-09-14T20:12:47+00:00 / Joseph Corneli / unknown |
| `futon2.aif.evidence-emit`<br>`futon2/src/futon2/aif/evidence_emit.clj` | 221 | flag; no | 0 | 1 (futon2/scripts/wm_scheduled_run.clj:22); tests 1 | `5ae5536c0c7cb8e53b342e20dee45e2a9e899534` / 2026-09-17T13:52:53+00:00 / Joseph Corneli / unknown |
| `futon2.aif.selection-authoring-coupling`<br>`futon2/src/futon2/aif/selection_authoring_coupling.clj` | 219 | flag; no | 0 | 2 (futon2/scripts/couple_selection_to_authoring.clj:2; futon2/scripts/manual_overnight_flights.clj:13); tests 1 | `085aee68a5e7b49b11056c24ffd17f2a68a8a805` / 2026-07-07T11:43:42+01:00 / Joseph Corneli / unknown |
| `futon3c.watcher.replay`<br>`futon3c/src/futon3c/watcher/replay.clj` | 218 | flag; no | 0 | 1 (futon3c/src/futon3c/watcher/file_ingest.clj:77); tests 0 | `22ea5a9f0cf506a2e087e3139a13ee72f0e6f4aa` / 2026-07-14T00:12:40+01:00 / Joseph Corneli / unknown |
| `futon2.aif.actuator-a6`<br>`futon2/src/futon2/aif/actuator_a6.clj` | 215 | flag; no | 0 | 1 (futon2/scripts/wm_preflight.clj:18); tests 1 | `9d8f2dee099382e19467c92152ed0805c1b0f337` / 2026-07-14T00:22:41+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.mission-head-invariants`<br>`futon3c/src/futon3c/logic/mission_head_invariants.clj` | 215 | review; no | 0 | 0 (none); tests 0 | `0d3d0cbce557f741da1d6640edb890b278d5e569` / 2026-06-10T19:34:41+01:00 / Joseph Corneli / unknown |
| `futon3c.aif.chipwitz`<br>`futon3c/src/futon3c/aif/chipwitz.clj` | 214 | review; no | 0 | 0 (none); tests 1 | `0e743b0a84890fc11309da953b5c89f2e63e0409` / 2026-07-07T14:21:20+01:00 / Joseph Corneli / unknown |
| `futon3c.agents.zaif-arm-comparison`<br>`futon3c/src/futon3c/agents/zaif_arm_comparison.clj` | 213 | review; no | 0 | 0 (none); tests 1 | `c7bd2e245e1d3022f98ef3a619a566d3709e6ca6` / 2026-09-03T11:47:20+00:00 / Joseph Corneli / unknown |
| `futon3c.agency.invoke-lifecycle-reconciliation`<br>`futon3c/src/futon3c/agency/invoke_lifecycle_reconciliation.clj` | 212 | review; no | 0 | 0 (none); tests 2 | `d090c0da447ddc8cc0ca33b8b727c9fcd429ce27` / 2026-09-13T04:18:55+00:00 / Joseph Corneli / codex-26 |
| `futon2.aif.capability-zones`<br>`futon2/src/futon2/aif/capability_zones.clj` | 211 | flag; no | 0 | 3 (futon2/scripts/capability_zones_live_map.clj:7; futon2/scripts/capability_zones_harvest.clj:5; futon2/scripts/capability_zones_reassign_3d.clj:6); tests 1 | `d3aa42b41c48e9720ec0e7eafd00d4282ee6de88` / 2026-09-18T13:57:07+00:00 / Joseph Corneli / unknown |
| `futon2.aif2.tension`<br>`futon2/src/futon2/aif2/tension.clj` | 209 | review; no | 0 | 0 (none); tests 1 | `1bb811e1fd15ff1903aaa993c2073c27d71c169d` / 2026-06-08T12:39:30+01:00 / Joseph Corneli / unknown |
| `futon3c.transport.ws.replication`<br>`futon3c/src/futon3c/transport/ws/replication.clj` | 203 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev.clj:77); tests 1 | `7c659aa299d0512a466939e7ceb56806cec4888c` / 2026-02-23T19:38:02+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.bank-sweep`<br>`futon3c/src/futon3c/apm/bank_sweep.clj` | 200 | review; no | 0 | 0 (none); tests 1 | `457d48d43a077c953532619c42e2d9abc3fdd1e9` / 2026-08-26T17:36:46+00:00 / Joseph Corneli / unknown |
| `futon3c.agents.arse-work-queue`<br>`futon3c/src/futon3c/agents/arse_work_queue.clj` | 195 | flag; yes | 0 | 2 (futon3c/dev/futon3c/dev.clj:58; futon3c/dev/futon3c/dev/arse.clj:7); tests 0 | `0195c4e77a4f334a1aa6b44eff66b81229e105dc` / 2026-06-16T12:00:48+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.aif2-invariants`<br>`futon3c/src/futon3c/logic/aif2_invariants.clj` | 193 | flag; no | 0 | 3 (futon3c/src/futon3c/logic/cascade_real.clj:7; futon3c/src/futon3c/logic/business_coupling_invariants.clj:10; futon3c/src/futon3c/logic/capability_star_map_invariants.clj:6); tests 0 | `80e699a180b2dafb4e15b99997f38919a6160bc4` / 2026-06-03T21:26:05+01:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.wiring`<br>`futon3c/src/futon3c/diagramprover/wiring.clj` | 193 | review; no | 0 | 0 (none); tests 2 | `c474470fb26748193380bc0bd2765f7ae64db6cf` / 2026-08-15T18:21:38+00:00 / Joseph Corneli / unknown |
| `futon3c.agency.r9-genesis`<br>`futon3c/src/futon3c/agency/r9_genesis.clj` | 191 | review; no | 0 | 0 (none); tests 1 | `04343e0ebfcd3d49431ab392d992d4090d126e5b` / 2026-09-13T02:41:53+00:00 / Joseph Corneli / codex-23 |
| `futon3c.runtime.incidents`<br>`futon3c/src/futon3c/runtime/incidents.clj` | 188 | flag; yes | 0 | 4 (futon3c/src/futon3c/transport/http.clj:8497; futon3c/src/futon3c/transport/http.clj:8498; futon3c/src/futon3c/transport/http.clj:8515; futon3c/dev/futon3c/dev.clj:75); tests 4 | `20ae2d35587ba52b50cd2950dbb0038230cbf389` / 2026-07-22T12:27:18+01:00 / Joseph Corneli / unknown |
| `futon3c.runtime.agents`<br>`futon3c/src/futon3c/runtime/agents.clj` | 187 | flag; yes | 0 | 11 (futon3c/dev/futon3c/dev.clj:74; futon3c/dev/futon3c/dev/agents.clj:12; futon3c/dev/futon3c/dev/peripheral_agents.clj:9; futon3c/scripts/dual_agent_ws_live_gate_codex_external.clj:20); tests 2 | `63e706f2bf0f3e31ef7105351f01c464a51ba72b` / 2026-09-11T01:36:12+00:00 / Joseph Corneli / unknown |
| `futon3c.aif.emacs-bridge`<br>`futon3c/src/futon3c/aif/emacs_bridge.clj` | 186 | flag; no | 0 | 2 (futon3c/src/futon3c/transport/http.clj:8371; futon3c/src/futon3c/transport/http.clj:8384); tests 1 | `11a364b27671f80c23e8754327ceb8e5206ca30a` / 2026-05-03T19:08:18+01:00 / Joseph Corneli / unknown |
| `futon2.aif.observation-rates`<br>`futon2/src/futon2/aif/observation_rates.clj` | 185 | flag; no | 0 | 8 (futon2/src/futon2/aif/likelihood_precision.clj:14; futon2/src/futon2/aif/likelihood_precision.clj:194; futon2/src/futon2/aif/likelihood_precision.clj:219; futon2/scripts/wm04/pilot_result.clj:8); tests 7 | `a097e0b4112d80e7183221f88f98112feb6eccb9` / 2026-09-18T14:58:00+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.disposition-derive`<br>`futon3c/src/futon3c/logic/disposition_derive.clj` | 183 | review; no | 0 | 0 (none); tests 2 | `f03b6c15ed6db6e39037b3c4e6fc38ebfd0ca9b4` / 2026-05-04T12:21:21+01:00 / Joseph Corneli / unknown |
| `futon3c.wm.operator-lane-adapter`<br>`futon3c/src/futon3c/wm/operator_lane_adapter.clj` | 182 | flag; yes | 0 | 2 (futon3c/src/futon3c/transport/http.clj:8444; futon3c/src/futon3c/transport/http.clj:8462); tests 1 | `f643b95e68e4dc784baf7a7351e3d04f32860a7e` / 2026-06-06T18:46:34+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.mission-clean`<br>`futon3c/src/futon3c/logic/mission_clean.clj` | 182 | flag; no | 0 | 3 (futon3c/scripts/emit_mission_clean.sh:23; futon3c/scripts/emit_mission_clean.sh:52; futon3c/scripts/execute_offramp.sh:14); tests 1 | `4e57d5a575921ba09060b9bfd971c6d4651071e3` / 2026-07-10T07:01:54+01:00 / Joseph Corneli / unknown |
| `futon2.aif.on-demand-entrypoint`<br>`futon2/src/futon2/aif/on_demand_entrypoint.clj` | 180 | review; no | 0 | 0 (none); tests 1 | `ca9edc253c3e9966dbb465dc2f579bc177fe8401` / 2026-09-14T03:33:34+00:00 / Joseph Corneli / unknown |
| `futon2.aif.cascade-proposals`<br>`futon2/src/futon2/aif/cascade_proposals.clj` | 179 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:51); tests 2 | `7f50e3ff7c18ea3f496ae15dabe019657db9ff30` / 2026-09-21T04:16:55+00:00 / Joseph Corneli / unknown |
| `futon2.aif.interoceptive-manifest`<br>`futon2/src/futon2/aif/interoceptive_manifest.clj` | 179 | review; no | 0 | 0 (none); tests 1 | `2f39b95dacad11bd4d70ce906c5e513977cd2122` / 2026-09-13T03:17:23+00:00 / Joseph Corneli / codex-22 |
| `futon2.aif.revision-scanner`<br>`futon2/src/futon2/aif/revision_scanner.clj` | 175 | review; no | 0 | 0 (none); tests 2 | `7d9e8ff16d3506f4bef6f8c8b541e9b306126aa9` / 2026-09-21T18:25:31+00:00 / Joseph Corneli / unknown |
| `futon2.aif.mission-hole-wants`<br>`futon2/src/futon2/aif/mission_hole_wants.clj` | 173 | flag; no | 0 | 2 (futon2/src/futon2/aif/load_identity.clj:31; futon2/scripts/futon2/report/war_machine.clj:70); tests 3 | `1d20ca88808df02b3f919f2809b9630a53b97be9` / 2026-09-21T15:22:01+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.tracer`<br>`futon3c/src/futon3c/logic/tracer.clj` | 173 | review; no | 0 | 0 (none); tests 3 | `673419479bec6892a061d9b210eaf20f0da92dce` / 2026-07-13T19:26:14+01:00 / Joseph Corneli / unknown |
| `futon3c.analysis.memory-arm-e1`<br>`futon3c/src/futon3c/analysis/memory_arm_e1.clj` | 170 | review; no | 0 | 0 (none); tests 1 | `0170889b7a4edc3ad5f44ad86a66f00e9db35a86` / 2026-08-01T13:40:47+01:00 / Joseph Corneli / unknown |
| `futon3c.agency.bg-process`<br>`futon3c/src/futon3c/agency/bg_process.clj` | 165 | flag; no | 0 | 9 (futon3c/scripts/bg.py:7; futon3c/scripts/bg.py:260; futon3c/scripts/bg.py:297; futon3c/scripts/bg.py:300); tests 1 | `8bbb005be7955bd5854b3ca38bc22ef12b187380` / 2026-08-23T11:58:39+00:00 / Joseph Corneli / unknown |
| `futon2.aif.habit-prior`<br>`futon2/src/futon2/aif/habit_prior.clj` | 164 | flag; no | 0 | 2 (futon2/src/futon2/aif/cascade_prior.clj:4; futon2/scripts/futon2/report/war_machine.clj:64); tests 4 | `a09190961e9e52f0608ad60ce6c95ec44721aaad` / 2026-07-18T15:25:46+01:00 / Joseph Corneli / unknown |
| `futon2.aif.tripwire-calibration`<br>`futon2/src/futon2/aif/tripwire_calibration.clj` | 164 | review; no | 0 | 0 (none); tests 0 | `697bb45001bc255c8e45d11f1a67a5f1c61a21e3` / 2026-07-16T14:18:05+01:00 / Joseph Corneli / unknown |
| `futon3c.logic.outreach-intake-guard`<br>`futon3c/src/futon3c/logic/outreach_intake_guard.clj` | 164 | review; no | 0 | 0 (none); tests 1 | `9267c9c1e36f85e3af91be16e30f6ef662cfcbb2` / 2026-06-16T09:35:03+01:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.round-trip`<br>`futon3c/src/futon3c/peripheral/round_trip.clj` | 162 | review; no | 0 | 0 (none); tests 5 | `e721e9ee6ef94721362a32c057595de86c16aeb6` / 2026-02-11T15:23:05+00:00 / Joseph Corneli / unknown |
| `futon2.aif.anticipation`<br>`futon2/src/futon2/aif/anticipation.clj` | 160 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:44); tests 3 | `7a0a62c1ef26d24bd17f51b63353e0e0c67ccd69` / 2026-09-15T04:53:14+00:00 / Joseph Corneli / unknown |
| `futon2.aif.code-build-match`<br>`futon2/src/futon2/aif/code_build_match.clj` | 160 | review; no | 0 | 0 (none); tests 1 | `0d29c7061f17cc0e46c5a62748635a34d56f3f19` / 2026-09-17T19:16:54+00:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.outing`<br>`futon3c/src/futon3c/peripheral/outing.clj` | 160 | review; no | 0 | 0 (none); tests 0 | `bf6191a1d9417bdca37898071f583b3a7add4087` / 2026-05-30T21:34:29+01:00 / Joseph Corneli / unknown |
| `futon2.aif.grain-maps`<br>`futon2/src/futon2/aif/grain_maps.clj` | 159 | review; no | 0 | 0 (none); tests 1 | `6806cbf8ffcce3276a9ab7bca0c3f8b1bdf3f51f` / 2026-09-18T15:46:35+00:00 / Joseph Corneli / unknown |
| `futon3c.inbox-zero.attribution`<br>`futon3c/src/futon3c/inbox_zero/attribution.clj` | 158 | review; no | 0 | 0 (none); tests 1 | `520cefb150252f4d702d550013cb561083cbfc9c` / 2026-08-24T09:20:57+00:00 / Joseph Corneli / unknown |
| `futon2.aif.portfolio-action-proposer`<br>`futon2/src/futon2/aif/portfolio_action_proposer.clj` | 157 | flag; no | 0 | 1 (futon2/src/futon2/aif/survey_mission_value.clj:6); tests 2 | `e46ed61e752a64b7bd6ff138cbb06a976300a6a2` / 2026-09-03T15:20:25+00:00 / Joseph Corneli / unknown |
| `futon3c.evidence.threads`<br>`futon3c/src/futon3c/evidence/threads.clj` | 157 | flag; no | 0 | 4 (futon3c/src/futon3c/peripheral/memory_backend.clj:366; futon3c/src/futon3c/peripheral/memory_backend.clj:383; futon3c/src/futon3c/peripheral/memory_backend.clj:386; futon3c/scripts/discipline_live_gate.clj:13); tests 7 | `e0236a2495d1c28f842a8ae9076217af98ff9340` / 2026-02-10T20:12:19+00:00 / Joseph Corneli / unknown |
| `futon2.aif.interpretation-construction`<br>`futon2/src/futon2/aif/interpretation_construction.clj` | 151 | flag; no | 0 | 1 (futon2/src/futon2/aif/load_identity.clj:34); tests 3 | `1d20ca88808df02b3f919f2809b9630a53b97be9` / 2026-09-21T15:22:01+00:00 / Joseph Corneli / unknown |
| `futon2.aif.policy-free-energy`<br>`futon2/src/futon2/aif/policy_free_energy.clj` | 151 | flag; no | 0 | 2 (futon2/src/futon2/aif/free_energy.clj:11; futon2/scripts/futon2/report/war_machine.clj:78); tests 4 | `cb9a7f42fefa086823d9641f77c2778bd30e51c0` / 2026-09-01T11:54:42+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.run4-infrastructure-reconciliation`<br>`futon3c/src/futon3c/wm/run4_infrastructure_reconciliation.clj` | 151 | review; no | 0 | 0 (none); tests 1 | `65d77867367c7055018e996e8600733aaaad4799` / 2026-09-11T03:05:18+00:00 / Joseph Corneli / unknown |
| `futon3c.test-registry.validation-adapters`<br>`futon3c/src/futon3c/test_registry/validation_adapters.clj` | 150 | review; no | 0 | 0 (none); tests 2 | `224da7456c7783bfe1f6201d1d16d19dd4f4ef02` / 2026-09-19T16:13:03+00:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-slow-feedback-retrospective-projection`<br>`futon2/src/futon2/aif/machine_slow_feedback_retrospective_projection.clj` | 148 | review; no | futon2.aif.machine-slow-feedback-completeness | 0 (none); tests 2 | `1fbd9d5bd0b7f2469a6edc60f3a07d9653df781f` / 2026-09-13T07:54:35+00:00 / Joseph Corneli / codex-23 |
| `futon3c.flight.pretty-print`<br>`futon3c/src/futon3c/flight/pretty_print.clj` | 147 | flag; no | 0 | 1 (futon3c/scripts/flight_pretty_print.clj:2); tests 1 | `fe0ae24a59c79c2c50f2d9db4be9d8e25d88fbca` / 2026-06-12T17:47:05+01:00 / Joseph Corneli / unknown |
| `futon3c.scripts.mission-scope-view`<br>`futon3c/src/futon3c/scripts/mission_scope_view.clj` | 147 | flag; no | 0 | 4 (futon3c/scripts/mission-scope-view-fast.sh:4; futon3c/scripts/mission-scope-view-fast.sh:20; futon3c/scripts/mission-scope-view-fast.sh:23; futon3c/scripts/mission-scope-view-fast.sh:40); tests 3 | `053e1a98e54aa243d03e51c3980da7fe44860757` / 2026-07-26T20:21:50+01:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.mission-logic`<br>`futon3c/src/futon3c/peripheral/mission_logic.clj` | 147 | flag; no | 0 | 2 (futon2/scripts/futon2/report/war_machine.clj:5438; futon3c/src/futon3c/transport/http.clj:7673); tests 1 | `711ed8e6e1968f3deb9b2cb6b85b6747c9384228` / 2026-03-15T17:14:34+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.run4-boot`<br>`futon3c/src/futon3c/wm/run4_boot.clj` | 146 | flag; yes | 0 | 1 (futon3c/dev/futon3c/dev/bootstrap.clj:27); tests 1 | `a3396a4176d926dd3120d79c022b25285b1ea21f` / 2026-09-12T16:40:20+00:00 / Joseph Corneli / unknown |
| `futon3c.clock.turn-trigger`<br>`futon3c/src/futon3c/clock/turn_trigger.clj` | 145 | review; no | 0 | 0 (none); tests 1 | `dc685c5d221a7b3dc84efc08c8814ea01ba50a1f` / 2026-06-26T19:47:01+01:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.drive`<br>`futon3c/src/futon3c/peripheral/drive.clj` | 145 | review; no | 0 | 0 (none); tests 1 | `af8c92ef2236f7fb776f81d1dc6b10c9dc397504` / 2026-06-11T15:50:57+01:00 / Joseph Corneli / unknown |
| `futon2.aif.parameter-delivery`<br>`futon2/src/futon2/aif/parameter_delivery.clj` | 144 | flag; no | 0 | 2 (futon2/src/futon2/aif/construction.clj:119; futon2/src/futon2/aif/construction_moves.clj:301); tests 1 | `7a9daa0f2ef9850aff01f1631dbc93c59625b586` / 2026-09-21T04:28:49+00:00 / Joseph Corneli / unknown |
| `futon3c.agents.cascade-verifier-board`<br>`futon3c/src/futon3c/agents/cascade_verifier_board.clj` | 144 | review; no | 0 | 0 (none); tests 3 | `52856d8fe7530dfe0c38d06525bf8b9a1f49aee6` / 2026-09-12T13:56:19+00:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.causal.bow`<br>`futon3c/src/futon3c/diagramprover/causal/bow.clj` | 144 | review; no | 0 | 0 (none); tests 2 | `2dadc325c908080386579323c833aaed6bfdb92a` / 2026-08-03T08:39:52+01:00 / Joseph Corneli / unknown |
| `futon2.aif.categorical-state-close-attachment`<br>`futon2/src/futon2/aif/categorical_state_close_attachment.clj` | 142 | review; no | 0 | 0 (none); tests 1 | `787ce2d139b88ff7db743faf9adc17b583e6842f` / 2026-09-13T03:46:39+00:00 / Joseph Corneli / codex-24 |
| `futon3c.agency.r9-authority`<br>`futon3c/src/futon3c/agency/r9_authority.clj` | 142 | review; no | 0 | 0 (none); tests 1 | `7b591a4e71e54dcb2abdf88a4989d8a9481264fc` / 2026-09-13T02:51:20+00:00 / Joseph Corneli / codex-23 |
| `futon3c.agency.invoke-lifecycle-snapshot`<br>`futon3c/src/futon3c/agency/invoke_lifecycle_snapshot.clj` | 142 | review; no | 0 | 0 (none); tests 1 | `dd870146e87b339cb3346aa5ad96204be57a9755` / 2026-09-13T04:31:58+00:00 / Joseph Corneli / codex-23 |
| `futon2.aif.machine-predictive`<br>`futon2/src/futon2/aif/machine_predictive.clj` | 135 | review; no | 0 | 0 (none); tests 3 | `9aad9adfa09e2764df70f74edfa029f7a75b745d` / 2026-09-15T22:39:35+00:00 / Joseph Corneli / unknown |
| `futon3c.portfolio.effect`<br>`futon3c/src/futon3c/portfolio/effect.clj` | 133 | review; no | 0 | 0 (none); tests 0 | `ac93ed4682a32af1cd2a82535c4a8fac92c38a32` / 2026-06-24T14:05:33+01:00 / Joseph Corneli / unknown |
| `futon2.aif.cascade-observation-scoring`<br>`futon2/src/futon2/aif/cascade_observation_scoring.clj` | 132 | flag; no | 0 | 1 (futon2/src/futon2/aif/efe.clj:1052); tests 0 | `c322c22ff2556181d397193653a1550cef3c9070` / 2026-09-20T16:15:02+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.strategic-closure-specification`<br>`futon3c/src/futon3c/logic/strategic_closure_specification.clj` | 129 | review; no | 0 | 0 (none); tests 1 | `e9c36f6e2c518812408520109680d436084fb1b5` / 2026-06-03T21:25:56+01:00 / Joseph Corneli / unknown |
| `futon2.aif.observation-authority-resolver`<br>`futon2/src/futon2/aif/observation_authority_resolver.clj` | 128 | review; no | 0 | 0 (none); tests 2 | `a621c7c9e725228f45c7b6f41cad8d82ac68e57e` / 2026-09-16T12:58:23+00:00 / Joseph Corneli / unknown |
| `futon3c.aif.loop-learning`<br>`futon3c/src/futon3c/aif/loop_learning.clj` | 128 | flag; no | 0 | 2 (futon3c/src/futon3c/peripheral/war_machine_pilot.clj:584; futon3c/src/futon3c/peripheral/war_machine_pilot.clj:588); tests 1 | `d0cc8605ca744ec3c722147c749afd73a7f4e0b4` / 2026-09-17T13:53:28+00:00 / Joseph Corneli / unknown |
| `futon3c.metric.resolution-state`<br>`futon3c/src/futon3c/metric/resolution_state.clj` | 128 | review; no | 0 | 0 (none); tests 1 | `c93e5597581b6553f1f1de96800827da6a6f3579` / 2026-06-01T19:19:09+01:00 / Joseph Corneli / unknown |
| `futon2.aif.authority-buffer`<br>`futon2/src/futon2/aif/authority_buffer.clj` | 127 | review; no | 0 | 0 (none); tests 3 | `23614af7421c9f414f273f4d8a3b8fee115cf9b2` / 2026-09-13T05:02:54+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.invariant-runner`<br>`futon3c/src/futon3c/logic/invariant_runner.clj` | 127 | flag; yes | 0 | 4 (futon2/scripts/futon2/report/war_machine.clj:5440; futon3c/src/futon3c/transport/http.clj:7687; futon3c/src/futon3c/transport/http.clj:7688; futon3c/dev/futon3c/dev/fm.clj:16); tests 2 | `f485b506b30049649c791a9734c226ed946b82c6` / 2026-03-15T17:38:01+00:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.mission-control-shapes`<br>`futon3c/src/futon3c/peripheral/mission_control_shapes.clj` | 127 | review; no | 0 | 0 (none); tests 0 | `671685a635af5c90b74b2593df99844be4f04502` / 2026-06-03T19:03:24+01:00 / Joseph Corneli / unknown |
| `futon2.aif.cascade-g`<br>`futon2/src/futon2/aif/cascade_g.clj` | 125 | review; no | 0 | 0 (none); tests 2 | `17d1e92f45637f9a5e1e4d9ea42dbba41bdac383` / 2026-09-19T18:01:03+00:00 / Joseph Corneli / unknown |
| `futon2.aif.core-efe`<br>`futon2/src/futon2/aif/core_efe.clj` | 125 | flag; no | 0 | 1 (futon2/src/futon2/aif/node_sim.clj:34); tests 0 | `a43ca5c44908a55808f9efaba73435756a5425f2` / 2026-07-14T18:31:38+01:00 / Joseph Corneli / unknown |
| `futon2.aif.observation-admission`<br>`futon2/src/futon2/aif/observation_admission.clj` | 123 | flag; no | 0 | 4 (futon2/src/futon2/aif/observation_rates.clj:10; futon2/scripts/wm04/pilot_result.clj:5; futon2/scripts/wm04/pilot_result.clj:18; futon2/scripts/wm04/zai18_adjudicate.clj:7); tests 1 | `3f601f605e85b7521ab796c3365da7698f500cda` / 2026-09-17T14:52:03+00:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.rewrite`<br>`futon3c/src/futon3c/diagramprover/rewrite.clj` | 123 | review; no | 0 | 0 (none); tests 1 | `07115ef8522396ab1fa8e757fb657f052b1cdcd8` / 2026-08-02T12:43:07+01:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.causal.diagram`<br>`futon3c/src/futon3c/diagramprover/causal/diagram.clj` | 123 | review; no | 0 | 0 (none); tests 3 | `251d6bf6bb775c6411a2c9fb6f098449f96f3163` / 2026-08-02T13:47:57+01:00 / Joseph Corneli / unknown |
| `futon2.patchboard`<br>`futon2/src/futon2/patchboard.clj` | 121 | review; no | 0 | 0 (none); tests 1 | `2629ef260e158e45de73b2b3388a40c86617f776` / 2026-07-16T15:49:20+01:00 / Joseph Corneli / unknown |
| `futon3c.agents.memory-mcp-test`<br>`futon3c/src/futon3c/agents/memory_mcp_test.clj` | 121 | review; no | 0 | 0 (none); tests 1 | `4fa5cadfa65653096f687ef97b08d9f19aa5cc4a` / 2026-08-14T10:47:30+00:00 / Joseph Corneli / unknown |
| `futon2.aif.strategic-habit`<br>`futon2/src/futon2/aif/strategic_habit.clj` | 118 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:67); tests 1 | `dd4a3bbe0324f08cf57fd31ba65780ce9b01ad51` / 2026-09-17T14:47:25+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.outing`<br>`futon3c/src/futon3c/wm/outing.clj` | 113 | review; no | 0 | 0 (none); tests 1 | `8f4f6552d620bbcf05e367d13f952779934be026` / 2026-06-06T20:11:07+01:00 / Joseph Corneli / unknown |
| `futon2.aif.preference-discovery`<br>`futon2/src/futon2/aif/preference_discovery.clj` | 112 | review; no | 0 | 0 (none); tests 1 | `dbc0cbe601c7127bbb8e44cee216f2f65df01dac` / 2026-09-08T23:46:13+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.run4-historical-verification`<br>`futon3c/src/futon3c/wm/run4_historical_verification.clj` | 112 | review; no | 0 | 0 (none); tests 9 | `8bf149c5e75fb0b94897f0834b6dabf1f59593c6` / 2026-09-11T03:33:33+00:00 / Joseph Corneli / unknown |
| `futon2.aif.pattern-reliability`<br>`futon2/src/futon2/aif/pattern_reliability.clj` | 111 | review; no | 0 | 0 (none); tests 1 | `6decbf0a2226f6fcf0175d2f383c3c2dca410598` / 2026-09-17T13:04:00+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.library-lane-smoke`<br>`futon3c/src/futon3c/apm/library_lane_smoke.clj` | 109 | review; no | 0 | 0 (none); tests 1 | `ec6c005e44ef78052552a580fed8f8bf8cfac91e` / 2026-09-05T11:40:27+00:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-forward-influence`<br>`futon2/src/futon2/aif/machine_forward_influence.clj` | 108 | review; no | 0 | 0 (none); tests 1 | `384c57a5de003f7e5f2f27a424e97633e9d23541` / 2026-09-13T05:02:39+00:00 / Joseph Corneli / unknown |
| `futon3c.agency.selective-form-loader`<br>`futon3c/src/futon3c/agency/selective_form_loader.clj` | 108 | review; no | 0 | 0 (none); tests 1 | `26d5a1dc1c972ba0326fe219ea8a7e3db9e61d3e` / 2026-09-13T03:22:19+00:00 / Joseph Corneli / codex-23 |
| `futon3c.inbox-zero.confirm-intake`<br>`futon3c/src/futon3c/inbox_zero/confirm_intake.clj` | 107 | flag; no | 0 | 1 (futon3c/src/futon3c/transport/http.clj:9162); tests 1 | `f260f63980d8687d2b01054f6a788b41d90d5f05` / 2026-08-24T12:27:01+00:00 / Joseph Corneli / unknown |
| `futon2.aif2.preference`<br>`futon2/src/futon2/aif2/preference.clj` | 106 | review; no | 0 | 0 (none); tests 1 | `1bb811e1fd15ff1903aaa993c2073c27d71c169d` / 2026-06-08T12:39:30+01:00 / Joseph Corneli / unknown |
| `futon2.aif.coverage-check`<br>`futon2/src/futon2/aif/coverage_check.clj` | 105 | review; no | 0 | 0 (none); tests 0 | `f3861fea7fa58a52381d378b931e589a4f09395a` / 2026-08-27T15:36:00+00:00 / Joseph Corneli / unknown |
| `futon2.aif.cascade-evaluation-trace`<br>`futon2/src/futon2/aif/cascade_evaluation_trace.clj` | 105 | review; no | 0 | 0 (none); tests 1 | `c6e9dcaf4afec740da53e1018985e88bfb9c1e37` / 2026-09-19T20:42:34+00:00 / Joseph Corneli / unknown |
| `futon2.aif.calibration-cycle`<br>`futon2/src/futon2/aif/calibration_cycle.clj` | 105 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:58); tests 3 | `c77c802bb2bfb3047b52353c66b5869c39b2ad65` / 2026-09-18T14:02:38+00:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.portfolio-inference-shapes`<br>`futon3c/src/futon3c/peripheral/portfolio_inference_shapes.clj` | 105 | review; no | 0 | 0 (none); tests 0 | `11a364b27671f80c23e8754327ceb8e5206ca30a` / 2026-05-03T19:08:18+01:00 / Joseph Corneli / unknown |
| `futon3c.apm.learning-loop-dry-run`<br>`futon3c/src/futon3c/apm/learning_loop_dry_run.clj` | 102 | review; no | 0 | 0 (none); tests 1 | `f7c373f4a371dc9e44c5f28072be677475acb46b` / 2026-09-02T19:31:10+00:00 / Joseph Corneli / unknown |
| `futon2.aif.r17-offline`<br>`futon2/src/futon2/aif/r17_offline.clj` | 101 | review; no | 0 | 0 (none); tests 2 | `8fe540b99d4d279576d20494e0dd0f969b8afdd7` / 2026-08-20T16:57:02+01:00 / Joseph Corneli / unknown |
| `futon2.aif.cascade-order-check`<br>`futon2/src/futon2/aif/cascade_order_check.clj` | 95 | review; no | 0 | 0 (none); tests 1 | `0209948e19e3da11451de8176113eaaeb2067ebc` / 2026-08-28T09:33:01+00:00 / Joseph Corneli / unknown |
| `futon3c.watcher.flight-ingest`<br>`futon3c/src/futon3c/watcher/flight_ingest.clj` | 95 | flag; no | 0 | 1 (futon3c/scripts/ingest_flight_to_futon1a.clj:8); tests 1 | `22ea5a9f0cf506a2e087e3139a13ee72f0e6f4aa` / 2026-07-14T00:12:40+01:00 / Joseph Corneli / unknown |
| `futon3c.aif.invariant`<br>`futon3c/src/futon3c/aif/invariant.clj` | 92 | flag; no | 0 | 3 (futon2/scripts/futon2/report/war_machine.clj:5347; futon3c/src/futon3c/aif/mission_head.clj:124; futon3c/src/futon3c/aif/mission_head.clj:305); tests 0 | `8fb77203515a947494c087e79184b93773fdf418` / 2026-05-29T13:13:50+01:00 / Joseph Corneli / unknown |
| `futon3c.wm.run4-codex-fold`<br>`futon3c/src/futon3c/wm/run4_codex_fold.clj` | 92 | review; no | 0 | 0 (none); tests 1 | `49550790d8fbef0ee4ed66c7ecf1382b53c95850` / 2026-09-12T16:25:59+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.cascade-dry-run`<br>`futon3c/src/futon3c/apm/cascade_dry_run.clj` | 89 | flag; no | 0 | 1 (futon3c/scripts/apm-cascade-dry-run.sh:16); tests 1 | `1cc861f912454c3d5b90ca4eba02987e8e8e0a3c` / 2026-08-26T17:39:39+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.invariant-queue-freshness`<br>`futon3c/src/futon3c/logic/invariant_queue_freshness.clj` | 88 | flag; no | 0 | 1 (futon3c/src/futon3c/watcher/freshness.clj:8); tests 1 | `5692eaddcf54fa8792cf7bd9bf485eb6aab3a1ec` / 2026-06-03T21:25:52+01:00 / Joseph Corneli / unknown |
| `futon3c.apm.library-lane-queue`<br>`futon3c/src/futon3c/apm/library_lane_queue.clj` | 86 | flag; no | 0 | 1 (futon3c/scripts/library_lane_run.clj:27); tests 1 | `a122c12f19e902c461e246180860eb670224926f` / 2026-08-23T14:25:58+00:00 / Joseph Corneli / unknown |
| `futon2.aif.deposit-preflight`<br>`futon2/src/futon2/aif/deposit_preflight.clj` | 85 | review; no | 0 | 0 (none); tests 1 | `855953a9d65549b70be7f74d29020827a49ce8a7` / 2026-09-15T03:55:19+00:00 / Joseph Corneli / unknown |
| `futon3c.social.authenticate`<br>`futon3c/src/futon3c/social/authenticate.clj` | 85 | review; no | 0 | 0 (none); tests 6 | `1d533f0263064d59c3ef5054bfe9e319ec204d0f` / 2026-02-10T17:05:48+00:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.rmdiagram`<br>`futon3c/src/futon3c/diagramprover/rmdiagram.clj` | 84 | review; no | 0 | 0 (none); tests 1 | `9a3970f461ec6d1fb44fa980a047410c807ba1ad` / 2026-08-02T15:46:02+01:00 / Joseph Corneli / unknown |
| `futon2.aif.fold-clean`<br>`futon2/src/futon2/aif/fold_clean.clj` | 83 | flag; no | 0 | 1 (futon2/scripts/futon2/aif/l2_verify.clj:16); tests 0 | `edad60ad6080af681878d72b5d90e292c364130a` / 2026-06-28T12:48:48+01:00 / Joseph Corneli / unknown |
| `futon3c.social.validate`<br>`futon3c/src/futon3c/social/validate.clj` | 83 | review; no | 0 | 0 (none); tests 2 | `e0236a2495d1c28f842a8ae9076217af98ff9340` / 2026-02-10T20:12:19+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.code-identity`<br>`futon3c/src/futon3c/wm/code_identity.clj` | 82 | flag; yes | 0 | 2 (futon2/scripts/run_readiness.py:177; futon3c/src/futon3c/wm/runner_service.clj:64); tests 3 | `dcddfeda2a60a2db941bb7844c771e9531bc55c5` / 2026-08-31T22:54:13+00:00 / Joseph Corneli / unknown |
| `futon3c.peripheral.memory-trials`<br>`futon3c/src/futon3c/peripheral/memory_trials.clj` | 81 | review; no | 0 | 0 (none); tests 1 | `a7537ca69561868d1a3f53a370fd5ce20741d62b` / 2026-07-23T11:33:44+01:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.causal.cohort-guard`<br>`futon3c/src/futon3c/diagramprover/causal/cohort_guard.clj` | 80 | review; no | 0 | 0 (none); tests 1 | `65fef3abb7c280264492d40ef2c9ca4db324394e` / 2026-08-03T11:22:01+01:00 / Joseph Corneli / unknown |
| `futon2.aif.work-target-predictor-input`<br>`futon2/src/futon2/aif/work_target_predictor_input.clj` | 76 | review; no | 0 | 0 (none); tests 1 | `a187331318db7acd2336dec402bbe4da537335d8` / 2026-09-17T19:56:29+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.operator-bulletin`<br>`futon3c/src/futon3c/wm/operator_bulletin.clj` | 76 | flag; no | 0 | 2 (futon3c/src/futon3c/wm/operator_lane_adapter.clj:6; futon3c/src/futon3c/transport/http.clj:8445); tests 3 | `1273b55150db1652c1ba1a81a7f0a65e711f809b` / 2026-06-08T12:00:17+01:00 / Joseph Corneli / unknown |
| `futon2.aif.controller-authority`<br>`futon2/src/futon2/aif/controller_authority.clj` | 75 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:42); tests 2 | `a24cb621e2f1a6e8cf6567f8784a9d8b2aed5c70` / 2026-09-15T13:52:43+00:00 / Joseph Corneli / unknown |
| `futon3c.wm.run4-report-service`<br>`futon3c/src/futon3c/wm/run4_report_service.clj` | 75 | review; no | 0 | 0 (none); tests 2 | `b7ea620a379ed7c1f369f41ab87199c4be3393bb` / 2026-09-10T19:35:07+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.campaign-supervisor`<br>`futon3c/src/futon3c/apm/campaign_supervisor.clj` | 75 | review; no | 0 | 0 (none); tests 1 | `b23efe8830bd2d41e3b7490248c82281faeae440` / 2026-08-20T15:41:51+00:00 / Joseph Corneli / unknown |
| `futon2.aif.adapters.interest-network`<br>`futon2/src/futon2/aif/adapters/interest_network.clj` | 74 | review; no | 0 | 0 (none); tests 0 | `b6e70912a532938dffb3f2568a1f14f6306d4cf5` / 2026-07-17T22:28:29+01:00 / Joseph Corneli / unknown |
| `futon3c.agency.hop-events`<br>`futon3c/src/futon3c/agency/hop_events.clj` | 74 | flag; no | 0 | 2 (futon3c/src/futon3c/agency/registry.clj:762; futon3c/src/futon3c/agency/registry.clj:769); tests 0 | `e912021de94ba7a28e1115f3bc189dad48dbc70d` / 2026-05-25T23:04:23+01:00 / Joseph Corneli / unknown |
| `futon2.aif.beta-habit`<br>`futon2/src/futon2/aif/beta_habit.clj` | 70 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:46); tests 2 | `4ddb70948daf3e56c5f06c32742fddbfc277e815` / 2026-09-09T19:58:22+00:00 / Joseph Corneli / unknown |
| `futon2.aif.policy-prefix-evidence`<br>`futon2/src/futon2/aif/policy_prefix_evidence.clj` | 70 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:77); tests 1 | `acc4f3c4640b4092710cceaf9d88f50acaeb0663` / 2026-09-21T04:59:12+00:00 / codex-1 / unknown |
| `futon3c.agents.mfuton-invoke-override`<br>`futon3c/src/futon3c/agents/mfuton_invoke_override.clj` | 70 | flag; yes | 0 | 2 (futon3c/dev/futon3c/dev.clj:53; futon3c/dev/futon3c/dev/invoke.clj:13); tests 8 | `5c2a58ee7a1e94d87ae139b526cb677d052bd64b` / 2026-05-31T13:01:31+01:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-policy-set`<br>`futon2/src/futon2/aif/machine_policy_set.clj` | 69 | review; no | 0 | 0 (none); tests 1 | `e9c66d641f250ccda1d1418253b170899e80c6eb` / 2026-09-12T21:00:44+00:00 / Joseph Corneli / unknown |
| `futon2.aif.repair-history-replay`<br>`futon2/src/futon2/aif/repair_history_replay.clj` | 67 | flag; no | 0 | 1 (futon2/src/futon2/aif/repair_evaluators.clj:67); tests 1 | `8cd5f442c9200f6f1012c16952ddc9ce13412a99` / 2026-09-21T05:36:21+00:00 / Joseph Corneli / unknown |
| `futon3c.logic.operational-readiness`<br>`futon3c/src/futon3c/logic/operational_readiness.clj` | 67 | review; no | 0 | 0 (none); tests 1 | `ac0dfb57b2f17be6c3b2d4f5691dc2a9ab3a8bf5` / 2026-05-04T12:28:44+01:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.causal.guard`<br>`futon3c/src/futon3c/diagramprover/causal/guard.clj` | 64 | review; no | 0 | 0 (none); tests 1 | `ad21beda20cae91d9a58a005feaf8db320c98a4c` / 2026-08-02T15:43:26+01:00 / Joseph Corneli / unknown |
| `futon3c.wm.r10-click-adapter`<br>`futon3c/src/futon3c/wm/r10_click_adapter.clj` | 60 | flag; no | 0 | 1 (futon3c/src/futon3c/transport/http.clj:8806); tests 2 | `5c4f17e112b738e0c8bb353e162ea0c070fd4795` / 2026-09-19T21:26:11+00:00 / Joseph Corneli / unknown |
| `futon2.aif.exact-belief-adapter`<br>`futon2/src/futon2/aif/exact_belief_adapter.clj` | 56 | review; no | 0 | 0 (none); tests 3 | `c300c67bf82d47dad47e3a513e0b0b90d9eda312` / 2026-09-20T18:54:22+00:00 / Joseph Corneli / unknown |
| `futon3c.transport.bootstrap-handler-migration`<br>`futon3c/src/futon3c/transport/bootstrap_handler_migration.clj` | 52 | review; no | 0 | 0 (none); tests 1 | `7a2a8e4e3bd61c138521fe9c71534712f56971a8` / 2026-09-11T01:43:41+00:00 / Joseph Corneli / unknown |
| `futon2.aif.cross-ledger-identity`<br>`futon2/src/futon2/aif/cross_ledger_identity.clj` | 49 | review; no | 0 | 0 (none); tests 1 | `4c776dc20e33cd50a17f0ed292d068a869cf8c28` / 2026-09-12T21:00:29+00:00 / Joseph Corneli / unknown |
| `futon2.aif.machine-accumulation`<br>`futon2/src/futon2/aif/machine_accumulation.clj` | 38 | flag; no | 0 | 1 (futon2/scripts/futon2/report/war_machine.clj:66); tests 1 | `01a5e8a4a9ba1c47bdf296a9dd832e66bca23dc7` / 2026-09-12T19:07:22+00:00 / Joseph Corneli / unknown |
| `futon2.aif.cascade-observation-route`<br>`futon2/src/futon2/aif/cascade_observation_route.clj` | 37 | review; no | 0 | 0 (none); tests 1 | `0093adcb7a7181518bbfd9e3aeb2f1b3dfc311ff` / 2026-09-19T19:22:18+00:00 / Joseph Corneli / unknown |
| `futon3c.apm.frame-park-decisions`<br>`futon3c/src/futon3c/apm/frame_park_decisions.clj` | 34 | review; no | 0 | 0 (none); tests 0 | `f387ac00cef8a98a5e2de233d30b6818b3028c90` / 2026-08-26T11:50:47+00:00 / Joseph Corneli / unknown |
| `futon2.aif.policy-depth`<br>`futon2/src/futon2/aif/policy_depth.clj` | 32 | flag; no | 0 | 2 (futon2/scripts/futon2/report/war_machine.clj:45; futon2/scripts/futon2/report/war_machine.clj:5816); tests 3 | `4e76f94f074c3582f76efe961708a155bb6c3c0a` / 2026-09-09T19:55:01+00:00 / Joseph Corneli / unknown |
| `futon3c.diagramprover.rule`<br>`futon3c/src/futon3c/diagramprover/rule.clj` | 21 | review; no | 0 | 0 (none); tests 2 | `52cb3fae056a5546ebd4475f95650c5ecbf1813a` / 2026-08-02T12:39:56+01:00 / Joseph Corneli / unknown |
| `futon2.aif.engine`<br>`futon2/src/futon2/aif/engine.clj` | 19 | review; no | 0 | 0 (none); tests 0 | `a19451f8d0c55b95fc5b44a57936e3990e50fad1` / 2026-01-08T10:48:21+00:00 / Joseph Corneli / unknown |
| `futon2.aif.adapters.ants`<br>`futon2/src/futon2/aif/adapters/ants.clj` | 18 | review; no | 0 | 0 (none); tests 0 | `a19451f8d0c55b95fc5b44a57936e3990e50fad1` / 2026-01-08T10:48:21+00:00 / Joseph Corneli / unknown |
| `futon2.aif.adapters.futon5-mca`<br>`futon2/src/futon2/aif/adapters/futon5_mca.clj` | 18 | review; no | 0 | 0 (none); tests 0 | `a19451f8d0c55b95fc5b44a57936e3990e50fad1` / 2026-01-08T10:48:21+00:00 / Joseph Corneli / unknown |
</details>

<details><summary>206 unrequired namespaces in the two src trees</summary>

```text
futon2.aif.active-horizon-g
futon2.aif.actuator-a6
futon2.aif.adapters.ants
futon2.aif.adapters.fulab
futon2.aif.adapters.futon5-mca
futon2.aif.adapters.interest-network
futon2.aif.anticipation
futon2.aif.arguing-worlds
futon2.aif.authority-buffer
futon2.aif.beta-habit
futon2.aif.bulletin
futon2.aif.calibration-cycle
futon2.aif.capability-zones
futon2.aif.cascade-evaluation-trace
futon2.aif.cascade-g
futon2.aif.cascade-observation-route
futon2.aif.cascade-observation-scoring
futon2.aif.cascade-order-check
futon2.aif.cascade-proposals
futon2.aif.categorical-state-close-attachment
futon2.aif.code-build-match
futon2.aif.contextual-preferences
futon2.aif.controller-authority
futon2.aif.core-efe
futon2.aif.coverage-check
futon2.aif.cross-ledger-identity
futon2.aif.decision-gate
futon2.aif.deposit-preflight
futon2.aif.enact
futon2.aif.engine
futon2.aif.enumeration-completeness
futon2.aif.evidence-emit
futon2.aif.exact-belief-adapter
futon2.aif.find-reconciliation
futon2.aif.fold-clean
futon2.aif.full-loop-cli
futon2.aif.grain-maps
futon2.aif.habit-prior
futon2.aif.interoceptive-manifest
futon2.aif.interpretation-construction
futon2.aif.machine-accumulation
futon2.aif.machine-forward-influence
futon2.aif.machine-policy-set
futon2.aif.machine-predictive
futon2.aif.machine-slow-feedback-completeness
futon2.aif.machine-slow-feedback-evidence
futon2.aif.measured-a-annotation
futon2.aif.mission-c
futon2.aif.mission-gauges
futon2.aif.mission-hole-wants
futon2.aif.node-sim
futon2.aif.observation-admission
futon2.aif.observation-authority-resolver
futon2.aif.observation-rates
futon2.aif.on-demand-entrypoint
futon2.aif.parameter-delivery
futon2.aif.pattern-reliability
futon2.aif.policy-depth
futon2.aif.policy-free-energy
futon2.aif.policy-prefix-evidence
futon2.aif.portfolio-action-proposer
futon2.aif.precision
futon2.aif.preference-discovery
futon2.aif.r17-offline
futon2.aif.repair-history-replay
futon2.aif.revision-scanner
futon2.aif.run-narrative
futon2.aif.scheduled-route-evidence
futon2.aif.selection-authoring-coupling
futon2.aif.selection-rationale
futon2.aif.strategic-habit
futon2.aif.task-belief-ladder
futon2.aif.tripwire-calibration
futon2.aif.work-target-predictor-input
futon2.aif.work-target-store
futon2.aif2.preference
futon2.aif2.tension
futon2.patchboard
futon2.wm-run-lock
futon3c.agency.bg-process
futon3c.agency.fed-uplink
futon3c.agency.hop-events
futon3c.agency.invoke-lifecycle-reconciliation
futon3c.agency.invoke-lifecycle-snapshot
futon3c.agency.r9-authority
futon3c.agency.r9-genesis
futon3c.agency.roster-store
futon3c.agency.selective-form-loader
futon3c.agents.apm-work-queue
futon3c.agents.arse-work-queue
futon3c.agents.cascade-verifier-board
futon3c.agents.codex-cli
futon3c.agents.codex-code-logic
futon3c.agents.memory-mcp-test
futon3c.agents.mfuton-invoke-override
futon3c.agents.tickle-orchestrate
futon3c.agents.tickle-queue
futon3c.agents.tickle-work-queue
futon3c.agents.zaif-arm-comparison
futon3c.aif.calibration
futon3c.aif.chipwitz
futon3c.aif.emacs-bridge
futon3c.aif.invariant
futon3c.aif.loop-learning
futon3c.aif.mission-delta-t
futon3c.aif.stack-generator
futon3c.analysis.memory-arm-e1
futon3c.apm.bank-sweep
futon3c.apm.campaign-supervisor
futon3c.apm.cascade-dry-run
futon3c.apm.csquare-synthetic-campaign
futon3c.apm.frame-park-decisions
futon3c.apm.frame18-control
futon3c.apm.ftriangle-live-smoke
futon3c.apm.learning-loop-dry-run
futon3c.apm.library-lane-queue
futon3c.apm.library-lane-smoke
futon3c.apm.library-loop-tools
futon3c.apm.pattern-revision-review
futon3c.apm.projection-watchdog
futon3c.clock.turn-trigger
futon3c.diagramprover.causal.bow
futon3c.diagramprover.causal.cohort-guard
futon3c.diagramprover.causal.diagram
futon3c.diagramprover.causal.guard
futon3c.diagramprover.rewrite
futon3c.diagramprover.rmdiagram
futon3c.diagramprover.rule
futon3c.diagramprover.wiring
futon3c.evidence.threads
futon3c.flight.pretty-print
futon3c.inbox-zero.attribution
futon3c.inbox-zero.batch-dispatch
futon3c.inbox-zero.confirm-intake
futon3c.live-efe-map
futon3c.logic.aif2-invariants
futon3c.logic.arxana-bridge
futon3c.logic.business-coupling-invariants
futon3c.logic.cascade-real-live
futon3c.logic.disposition-derive
futon3c.logic.invariant-queue-freshness
futon3c.logic.invariant-runner
futon3c.logic.locus
futon3c.logic.metabolic-balance
futon3c.logic.mission-clean
futon3c.logic.mission-head-invariants
futon3c.logic.operational-readiness
futon3c.logic.outing-invariants
futon3c.logic.outreach-intake-guard
futon3c.logic.probe-taps
futon3c.logic.ratchet
futon3c.logic.snapshot
futon3c.logic.strategic-closure-specification
futon3c.logic.substrate-metric-e1-invariants
futon3c.logic.tracer
futon3c.logic.typed-bells-invariants
futon3c.logic.wm-operator-lane-invariants
futon3c.metric.resolution-state
futon3c.nlp.classical-pipeline
futon3c.peripheral.adapter
futon3c.peripheral.drive
futon3c.peripheral.dynamic-queries-rung4
futon3c.peripheral.live-wm-selection
futon3c.peripheral.memory-trials
futon3c.peripheral.mission-control-shapes
futon3c.peripheral.mission-logic
futon3c.peripheral.outing
futon3c.peripheral.portfolio-inference-shapes
futon3c.peripheral.proof-logic
futon3c.peripheral.round-trip
futon3c.peripheral.strategic-canary
futon3c.peripheral.strategic-embedding-experiment
futon3c.peripheral.street-sweeper
futon3c.peripheral.war-machine-pilot
futon3c.portfolio-inference.scheduler
futon3c.portfolio-inference.service
futon3c.portfolio.effect
futon3c.process-watchdog
futon3c.proof.bridge
futon3c.runtime.agents
futon3c.runtime.incidents
futon3c.scripts.mission-scope-ingest
futon3c.scripts.mission-scope-view
futon3c.social.authenticate
futon3c.social.validate
futon3c.test-registry.validation-adapters
futon3c.transport.bootstrap-handler-migration
futon3c.transport.irc
futon3c.transport.ws.replication
futon3c.vsatarcs.feeder
futon3c.watcher.flight-ingest
futon3c.watcher.multi
futon3c.watcher.replay
futon3c.wm.code-identity
futon3c.wm.operator-bulletin
futon3c.wm.operator-lane-adapter
futon3c.wm.outing
futon3c.wm.r10-click-adapter
futon3c.wm.run4-boot
futon3c.wm.run4-codex-fold
futon3c.wm.run4-historical-verification
futon3c.wm.run4-infrastructure-reconciliation
futon3c.wm.run4-report-service
futon3c.wm.run4-series-queue
futon3c.wm.scheduler
repl.http
```

</details>

<details><summary>1,183 unreferenced var definition records (self-reference excluded)</summary>

These are scoped analysis misses, not a safe-delete list. File/row locate each definition.

| Var | File:line | Private? |
|---|---|---|
| `futon2.aif.a4a/model-uncertainty-for-produces` | `futon2/src/futon2/aif/a4a.clj:239` | False |
| `futon2.aif.a4a-substrate/capability-edge-query` | `futon2/src/futon2/aif/a4a_substrate.clj:28` | False |
| `futon2.aif.a4a-substrate/capability-query` | `futon2/src/futon2/aif/a4a_substrate.clj:23` | False |
| `futon2.aif.a4a-substrate/discharge-query` | `futon2/src/futon2/aif/a4a_substrate.clj:37` | False |
| `futon2.aif.a4a-substrate/mint-slush-candidates!` | `futon2/src/futon2/aif/a4a_substrate.clj:235` | False |
| `futon2.aif.a4a-substrate/mint-stars!` | `futon2/src/futon2/aif/a4a_substrate.clj:115` | False |
| `futon2.aif.a4a-substrate/read-corpus` | `futon2/src/futon2/aif/a4a_substrate.clj:45` | False |
| `futon2.aif.action-proposer/bootstrap-proposer` | `futon2/src/futon2/aif/action_proposer.clj:48` | False |
| `futon2.aif.action-proposer/compose-proposers` | `futon2/src/futon2/aif/action_proposer.clj:63` | False |
| `futon2.aif.action-proposer/proposer-id` | `futon2/src/futon2/aif/action_proposer.clj:31` | False |
| `futon2.aif.active-horizon-g/active-horizon-g` | `futon2/src/futon2/aif/active_horizon_g.clj:169` | False |
| `futon2.aif.actuator-a3/a3-live-tests` | `futon2/src/futon2/aif/actuator_a3.clj:407` | False |
| `futon2.aif.actuator-a3/bindings-for-mission` | `futon2/src/futon2/aif/actuator_a3.clj:199` | False |
| `futon2.aif.actuator-a3/build-match` | `futon2/src/futon2/aif/actuator_a3.clj:353` | False |
| `futon2.aif.actuator-a3/discharge-query` | `futon2/src/futon2/aif/actuator_a3.clj:454` | False |
| `futon2.aif.actuator-a3/discharge-query-form` | `futon2/src/futon2/aif/actuator_a3.clj:462` | False |
| `futon2.aif.actuator-a3/finalize-discharge!` | `futon2/src/futon2/aif/actuator_a3.clj:502` | False |
| `futon2.aif.actuator-a3/mission-open-hole-count` | `futon2/src/futon2/aif/actuator_a3.clj:821` | False |
| `futon2.aif.actuator-a3/parse-args` | `futon2/src/futon2/aif/actuator_a3.clj:835` | False |
| `futon2.aif.actuator-a3/proof-form` | `futon2/src/futon2/aif/actuator_a3.clj:264` | False |
| `futon2.aif.actuator-a3/render-package` | `futon2/src/futon2/aif/actuator_a3.clj:829` | False |
| `futon2.aif.actuator-a3/review-partial` | `futon2/src/futon2/aif/actuator_a3.clj:791` | False |
| `futon2.aif.actuator-a3/run-a3!` | `futon2/src/futon2/aif/actuator_a3.clj:859` | False |
| `futon2.aif.actuator-a3/verify-builder-result` | `futon2/src/futon2/aif/actuator_a3.clj:654` | False |
| `futon2.aif.actuator-a6/closure-falsifier` | `futon2/src/futon2/aif/actuator_a6.clj:164` | False |
| `futon2.aif.actuator-a6/rank-with-star-status` | `futon2/src/futon2/aif/actuator_a6.clj:130` | False |
| `futon2.aif.actuator-a6/witness-live` | `futon2/src/futon2/aif/actuator_a6.clj:201` | False |
| `futon2.aif.adapters.ants/AntsAdapter` | `futon2/src/futon2/aif/adapters/ants.clj:5` | False |
| `futon2.aif.adapters.ants/map->AntsAdapter` | `futon2/src/futon2/aif/adapters/ants.clj:5` | False |
| `futon2.aif.adapters.ants/new-adapter` | `futon2/src/futon2/aif/adapters/ants.clj:17` | False |
| `futon2.aif.adapters.fulab/FulabAdapter` | `futon2/src/futon2/aif/adapters/fulab.clj:260` | False |
| `futon2.aif.adapters.fulab/map->FulabAdapter` | `futon2/src/futon2/aif/adapters/fulab.clj:260` | False |
| `futon2.aif.adapters.fulab/new-adapter` | `futon2/src/futon2/aif/adapters/fulab.clj:376` | False |
| `futon2.aif.adapters.futon5-mca/Futon5McaAdapter` | `futon2/src/futon2/aif/adapters/futon5_mca.clj:5` | False |
| `futon2.aif.adapters.futon5-mca/map->Futon5McaAdapter` | `futon2/src/futon2/aif/adapters/futon5_mca.clj:5` | False |
| `futon2.aif.adapters.futon5-mca/new-adapter` | `futon2/src/futon2/aif/adapters/futon5_mca.clj:17` | False |
| `futon2.aif.adapters.interest-network/enrich-candidates` | `futon2/src/futon2/aif/adapters/interest_network.clj:61` | False |
| `futon2.aif.anticipation/anticipation-snapshot` | `futon2/src/futon2/aif/anticipation.clj:133` | False |
| `futon2.aif.anticipation/load-anticipations` | `futon2/src/futon2/aif/anticipation.clj:41` | False |
| `futon2.aif.anticipation/time-pressure` | `futon2/src/futon2/aif/anticipation.clj:108` | False |
| `futon2.aif.arguing-worlds/experiment-runner` | `futon2/src/futon2/aif/arguing_worlds.clj:217` | False |
| `futon2.aif.arguing-worlds/greedy-eps-sampler` | `futon2/src/futon2/aif/arguing_worlds.clj:410` | False |
| `futon2.aif.arguing-worlds/incumbent-sampler` | `futon2/src/futon2/aif/arguing_worlds.clj:399` | False |
| `futon2.aif.arguing-worlds/random-under-budget-sampler` | `futon2/src/futon2/aif/arguing_worlds.clj:429` | False |
| `futon2.aif.arguing-worlds/referee-field-harness` | `futon2/src/futon2/aif/arguing_worlds.clj:337` | False |
| `futon2.aif.arguing-worlds/run-sampler-field` | `futon2/src/futon2/aif/arguing_worlds.clj:453` | False |
| `futon2.aif.arguing-worlds/uniform-best-of-k-sampler` | `futon2/src/futon2/aif/arguing_worlds.clj:436` | False |
| `futon2.aif.arguing-worlds/write-circumstances!` | `futon2/src/futon2/aif/arguing_worlds.clj:304` | False |
| `futon2.aif.authority-buffer/capture!` | `futon2/src/futon2/aif/authority_buffer.clj:66` | False |
| `futon2.aif.authority-buffer/resolve-pointer!` | `futon2/src/futon2/aif/authority_buffer.clj:98` | False |
| `futon2.aif.belief/*carry-belief?*` | `futon2/src/futon2/aif/belief.clj:995` | False |
| `futon2.aif.belief/bootstrap-from-stack-annotations` | `futon2/src/futon2/aif/belief.clj:489` | False |
| `futon2.aif.belief/channel-emission-matrix` | `futon2/src/futon2/aif/belief.clj:969` | False |
| `futon2.aif.belief/classify-entity-repos-from-stack-annotations` | `futon2/src/futon2/aif/belief.clj:767` | False |
| `futon2.aif.belief/classify-entity-tags-from-stack-annotations` | `futon2/src/futon2/aif/belief.clj:561` | False |
| `futon2.aif.belief/classify-entity-ticks-from-stack-annotations` | `futon2/src/futon2/aif/belief.clj:800` | False |
| `futon2.aif.belief/model-manifest` | `futon2/src/futon2/aif/belief.clj:286` | False |
| `futon2.aif.belief/most-likely-status` | `futon2/src/futon2/aif/belief.clj:447` | False |
| `futon2.aif.belief/observation-model-identity` | `futon2/src/futon2/aif/belief.clj:207` | False |
| `futon2.aif.belief/predict-observation` | `futon2/src/futon2/aif/belief.clj:1199` | False |
| `futon2.aif.belief/r3d-aggregate-driver` | `futon2/src/futon2/aif/belief.clj:1122` | False |
| `futon2.aif.belief/reconcile-belief-carry` | `futon2/src/futon2/aif/belief.clj:510` | False |
| `futon2.aif.belief/valid-initial-prior?` | `futon2/src/futon2/aif/belief.clj:268` | False |
| `futon2.aif.beta-habit/carry` | `futon2/src/futon2/aif/beta_habit.clj:38` | False |
| `futon2.aif.beta-habit/enabled?` | `futon2/src/futon2/aif/beta_habit.clj:11` | False |
| `futon2.aif.beta-habit/preconditions!` | `futon2/src/futon2/aif/beta_habit.clj:26` | False |
| `futon2.aif.bmr/bmr` | `futon2/src/futon2/aif/bmr.clj:140` | False |
| `futon2.aif.bulletin/-main` | `futon2/src/futon2/aif/bulletin.clj:658` | False |
| `futon2.aif.bulletin/append-trace-review!` | `futon2/src/futon2/aif/bulletin.clj:265` | False |
| `futon2.aif.c-vector/ensure-belly-fresh!` | `futon2/src/futon2/aif/c_vector.clj:322` | False |
| `futon2.aif.c-vector/freshness-check` | `futon2/src/futon2/aif/c_vector.clj:292` | False |
| `futon2.aif.c-vector/goal-outcome-risk` | `futon2/src/futon2/aif/c_vector.clj:354` | False |
| `futon2.aif.c-vector/predictive-goal-outcome-risk` | `futon2/src/futon2/aif/c_vector.clj:632` | False |
| `futon2.aif.c-vector/predictive-goal-outcome-risk-kl` | `futon2/src/futon2/aif/c_vector.clj:688` | False |
| `futon2.aif.calibration-cycle/admit-apparatus!` | `futon2/src/futon2/aif/calibration_cycle.clj:96` | False |
| `futon2.aif.capability-zones/declared-coverage` | `futon2/src/futon2/aif/capability_zones.clj:66` | False |
| `futon2.aif.capability-zones/seeds-3d` | `futon2/src/futon2/aif/capability_zones.clj:144` | False |
| `futon2.aif.capability-zones/zone-of-action` | `futon2/src/futon2/aif/capability_zones.clj:196` | False |
| `futon2.aif.capability-zones/zone-of-action-3d` | `futon2/src/futon2/aif/capability_zones.clj:205` | False |
| `futon2.aif.cascade-evaluation-trace/validate-record` | `futon2/src/futon2/aif/cascade_evaluation_trace.clj:87` | False |
| `futon2.aif.cascade-g/total-g` | `futon2/src/futon2/aif/cascade_g.clj:104` | False |
| `futon2.aif.cascade-habit-store/attach-habits` | `futon2/src/futon2/aif/cascade_habit_store.clj:54` | False |
| `futon2.aif.cascade-habit-store/record-selection!` | `futon2/src/futon2/aif/cascade_habit_store.clj:120` | False |
| `futon2.aif.cascade-model-manifest/build-manifest` | `futon2/src/futon2/aif/cascade_model_manifest.clj:408` | False |
| `futon2.aif.cascade-model-manifest/cascade-kernel` | `futon2/src/futon2/aif/cascade_model_manifest.clj:343` | False |
| `futon2.aif.cascade-model-manifest/horizon-g` | `futon2/src/futon2/aif/cascade_model_manifest.clj:542` | False |
| `futon2.aif.cascade-model-manifest/independent-belief` | `futon2/src/futon2/aif/cascade_model_manifest.clj:130` | False |
| `futon2.aif.cascade-model-manifest/observation-row` | `futon2/src/futon2/aif/cascade_model_manifest.clj:161` | False |
| `futon2.aif.cascade-model-manifest/preference-distribution` | `futon2/src/futon2/aif/cascade_model_manifest.clj:491` | False |
| `futon2.aif.cascade-model-manifest/preference-fn` | `futon2/src/futon2/aif/cascade_model_manifest.clj:647` | False |
| `futon2.aif.cascade-model-manifest/preference-spec` | `futon2/src/futon2/aif/cascade_model_manifest.clj:451` | False |
| `futon2.aif.cascade-model-manifest/token-belief-at` | `futon2/src/futon2/aif/cascade_model_manifest.clj:1176` | False |
| `futon2.aif.cascade-model-manifest/token-belief-at-runtime-authority` | `futon2/src/futon2/aif/cascade_model_manifest.clj:1167` | False |
| `futon2.aif.cascade-model-manifest/transition-row` | `futon2/src/futon2/aif/cascade_model_manifest.clj:116` | False |
| `futon2.aif.cascade-observation-route/run` | `futon2/src/futon2/aif/cascade_observation_route.clj:8` | False |
| `futon2.aif.cascade-observation-scoring/rank-cascade-actions` | `futon2/src/futon2/aif/cascade_observation_scoring.clj:99` | False |
| `futon2.aif.cascade-order-check/check-cascade-order` | `futon2/src/futon2/aif/cascade_order_check.clj:68` | False |
| `futon2.aif.cascade-policy/candidate-space` | `futon2/src/futon2/aif/cascade_policy.clj:272` | False |
| `futon2.aif.cascade-policy/composition-blind?` | `futon2/src/futon2/aif/cascade_policy.clj:167` | False |
| `futon2.aif.cascade-policy/select-over-cascades` | `futon2/src/futon2/aif/cascade_policy.clj:197` | False |
| `futon2.aif.cascade-policy/selected-only-temperament` | `futon2/src/futon2/aif/cascade_policy.clj:18` | False |
| `futon2.aif.cascade-prior/shadow-rank` | `futon2/src/futon2/aif/cascade_prior.clj:175` | False |
| `futon2.aif.cascade-prior/state-stats` | `futon2/src/futon2/aif/cascade_prior.clj:120` | False |
| `futon2.aif.cascade-problems/assemble` | `futon2/src/futon2/aif/cascade_problems.clj:196` | False |
| `futon2.aif.cascade-problems/substrate-targets` | `futon2/src/futon2/aif/cascade_problems.clj:37` | False |
| `futon2.aif.cascade-proposals/-main` | `futon2/src/futon2/aif/cascade_proposals.clj:125` | False |
| `futon2.aif.cascade-proposals/load-supply` | `futon2/src/futon2/aif/cascade_proposals.clj:115` | False |
| `futon2.aif.cascade-proposals/record-supply` | `futon2/src/futon2/aif/cascade_proposals.clj:130` | False |
| `futon2.aif.cascade-sources/load-declared` | `futon2/src/futon2/aif/cascade_sources.clj:133` | False |
| `futon2.aif.cascade-sources/with-context-fn` | `futon2/src/futon2/aif/cascade_sources.clj:207` | False |
| `futon2.aif.categorical-state-close-attachment/attach-all!` | `futon2/src/futon2/aif/categorical_state_close_attachment.clj:129` | False |
| `futon2.aif.categorical-state-close-attachment/inspect-close` | `futon2/src/futon2/aif/categorical_state_close_attachment.clj:29` | False |
| `futon2.aif.categorical-state-observation/subject-digest` | `futon2/src/futon2/aif/categorical_state_observation.clj:114` | False |
| `futon2.aif.categorical-state-observation/validate-observations!` | `futon2/src/futon2/aif/categorical_state_observation.clj:333` | False |
| `futon2.aif.close-loop/act-gate-for` | `futon2/src/futon2/aif/close_loop.clj:121` | False |
| `futon2.aif.code-build-match/aif-grounded-loop-match` | `futon2/src/futon2/aif/code_build_match.clj:157` | False |
| `futon2.aif.construction-moves/add-a-check` | `futon2/src/futon2/aif/construction_moves.clj:234` | False |
| `futon2.aif.construction-moves/borrow-a-sibling` | `futon2/src/futon2/aif/construction_moves.clj:76` | False |
| `futon2.aif.construction-moves/read-what-exists` | `futon2/src/futon2/aif/construction_moves.clj:25` | False |
| `futon2.aif.construction-moves/with-parameter-information-gain` | `futon2/src/futon2/aif/construction_moves.clj:297` | False |
| `futon2.aif.contextual-preferences/derive-binding` | `futon2/src/futon2/aif/contextual_preferences.clj:77` | False |
| `futon2.aif.contextual-preferences/replay-episode` | `futon2/src/futon2/aif/contextual_preferences.clj:268` | False |
| `futon2.aif.controller-authority/authorize` | `futon2/src/futon2/aif/controller_authority.clj:19` | False |
| `futon2.aif.core-efe/g-efe` | `futon2/src/futon2/aif/core_efe.clj:94` | False |
| `futon2.aif.coverage-check/summarize` | `futon2/src/futon2/aif/coverage_check.clj:95` | False |
| `futon2.aif.cross-ledger-identity/join-close-to-trace` | `futon2/src/futon2/aif/cross_ledger_identity.clj:18` | False |
| `futon2.aif.decision-gate/emit!` | `futon2/src/futon2/aif/decision_gate.clj:217` | False |
| `futon2.aif.deposit-preflight/-main` | `futon2/src/futon2/aif/deposit_preflight.clj:79` | False |
| `futon2.aif.efe/ambiguity` | `futon2/src/futon2/aif/efe.clj:46` | True |
| `futon2.aif.efe/rank-local-preference-actions` | `futon2/src/futon2/aif/efe.clj:1426` | False |
| `futon2.aif.efe/retired-control-mode` | `futon2/src/futon2/aif/efe.clj:114` | False |
| `futon2.aif.efe/select-star-map-action` | `futon2/src/futon2/aif/efe.clj:1420` | False |
| `futon2.aif.efe/selection-trace-step` | `futon2/src/futon2/aif/efe.clj:400` | False |
| `futon2.aif.enact/close-loop!` | `futon2/src/futon2/aif/enact.clj:323` | False |
| `futon2.aif.engine/new-engine` | `futon2/src/futon2/aif/engine.clj:5` | False |
| `futon2.aif.engine/select-pattern` | `futon2/src/futon2/aif/engine.clj:11` | False |
| `futon2.aif.engine/update-beliefs` | `futon2/src/futon2/aif/engine.clj:14` | False |
| `futon2.aif.enumeration-completeness/*enumeration-assert?*` | `futon2/src/futon2/aif/enumeration_completeness.clj:35` | False |
| `futon2.aif.enumeration-completeness/completeness-record` | `futon2/src/futon2/aif/enumeration_completeness.clj:331` | False |
| `futon2.aif.epistemic-value/policy-information-gains` | `futon2/src/futon2/aif/epistemic_value.clj:92` | False |
| `futon2.aif.evidence-emit/emit!` | `futon2/src/futon2/aif/evidence_emit.clj:207` | False |
| `futon2.aif.exact-belief-adapter/synthetic-mixture-update` | `futon2/src/futon2/aif/exact_belief_adapter.clj:48` | False |
| `futon2.aif.find-designation/designation-for` | `futon2/src/futon2/aif/find_designation.clj:127` | False |
| `futon2.aif.find-expectations/build-artifact` | `futon2/src/futon2/aif/find_expectations.clj:159` | False |
| `futon2.aif.find-expectations/external-expectations-checked?` | `futon2/src/futon2/aif/find_expectations.clj:231` | False |
| `futon2.aif.find-receipt/local` | `futon2/src/futon2/aif/find_receipt.clj:50` | False |
| `futon2.aif.find-reconciliation/certificate` | `futon2/src/futon2/aif/find_reconciliation.clj:60` | False |
| `futon2.aif.find-reconciliation/certificate-drift` | `futon2/src/futon2/aif/find_reconciliation.clj:104` | False |
| `futon2.aif.find-reconciliation/pin-drift` | `futon2/src/futon2/aif/find_reconciliation.clj:185` | False |
| `futon2.aif.find-reconciliation/read-pin-expectation` | `futon2/src/futon2/aif/find_reconciliation.clj:127` | False |
| `futon2.aif.find-reconciliation/report` | `futon2/src/futon2/aif/find_reconciliation.clj:195` | False |
| `futon2.aif.focus-receipt/attach` | `futon2/src/futon2/aif/focus_receipt.clj:103` | False |
| `futon2.aif.fold/closes?` | `futon2/src/futon2/aif/fold.clj:224` | False |
| `futon2.aif.fold/valid-fold-output?` | `futon2/src/futon2/aif/fold.clj:34` | False |
| `futon2.aif.fold-clean/->edn` | `futon2/src/futon2/aif/fold_clean.clj:60` | False |
| `futon2.aif.fold-clean/carries-resolvable?` | `futon2/src/futon2/aif/fold_clean.clj:69` | False |
| `futon2.aif.fold-clean/fold->clean` | `futon2/src/futon2/aif/fold_clean.clj:36` | False |
| `futon2.aif.free-energy/channel-prediction-error` | `futon2/src/futon2/aif/free_energy.clj:280` | False |
| `futon2.aif.free-energy/infer-mode` | `futon2/src/futon2/aif/free_energy.clj:431` | False |
| `futon2.aif.full-loop-cli/-main` | `futon2/src/futon2/aif/full_loop_cli.clj:719` | False |
| `futon2.aif.full-loop-cohort/cell?` | `futon2/src/futon2/aif/full_loop_cohort.clj:50` | False |
| `futon2.aif.full-loop-cohort/execution-authority` | `futon2/src/futon2/aif/full_loop_cohort.clj:163` | False |
| `futon2.aif.full-loop-cohort/map->PinnedPreregistration` | `futon2/src/futon2/aif/full_loop_cohort.clj:197` | False |
| `futon2.aif.full-loop-cohort/write-ledger!` | `futon2/src/futon2/aif/full_loop_cohort.clj:724` | False |
| `futon2.aif.full-loop-cohort/write-ledgers!` | `futon2/src/futon2/aif/full_loop_cohort.clj:779` | False |
| `futon2.aif.full-loop-runner/artifact-window-tolerance-ms` | `futon2/src/futon2/aif/full_loop_runner.clj:88` | False |
| `futon2.aif.full-loop-runner/repair-entry` | `futon2/src/futon2/aif/full_loop_runner.clj:1290` | True |
| `futon2.aif.full-loop-runner/strategic-selection!` | `futon2/src/futon2/aif/full_loop_runner.clj:973` | False |
| `futon2.aif.grain-maps/coverage` | `futon2/src/futon2/aif/grain_maps.clj:129` | False |
| `futon2.aif.grain-maps/pull-zeroed` | `futon2/src/futon2/aif/grain_maps.clj:152` | False |
| `futon2.aif.grain-maps/pushforward-eq-mission-weight` | `futon2/src/futon2/aif/grain_maps.clj:114` | False |
| `futon2.aif.grain-maps/total-pulled` | `futon2/src/futon2/aif/grain_maps.clj:145` | False |
| `futon2.aif.habit-prior/attach-log-priors` | `futon2/src/futon2/aif/habit_prior.clj:121` | False |
| `futon2.aif.habit-prior/fold-records` | `futon2/src/futon2/aif/habit_prior.clj:86` | False |
| `futon2.aif.habit-prior/state-stats` | `futon2/src/futon2/aif/habit_prior.clj:91` | False |
| `futon2.aif.hierarchical-budget-adapter/replay` | `futon2/src/futon2/aif/hierarchical_budget_adapter.clj:98` | False |
| `futon2.aif.interoceptive-activation/process-census-edn` | `futon2/src/futon2/aif/interoceptive_activation.clj:71` | False |
| `futon2.aif.interoceptive-activation/production-controller-status!` | `futon2/src/futon2/aif/interoceptive_activation.clj:332` | False |
| `futon2.aif.interoceptive-activation/read-test-participation!` | `futon2/src/futon2/aif/interoceptive_activation.clj:135` | False |
| `futon2.aif.interoceptive-manifest/production-manifest!` | `futon2/src/futon2/aif/interoceptive_manifest.clj:158` | False |
| `futon2.aif.interoceptive-manifest/test-snapshot` | `futon2/src/futon2/aif/interoceptive_manifest.clj:154` | False |
| `futon2.aif.interoceptive-store-lock/with-lock-at` | `futon2/src/futon2/aif/interoceptive_store_lock.clj:99` | False |
| `futon2.aif.interoceptive-store-lock/with-store-lock` | `futon2/src/futon2/aif/interoceptive_store_lock.clj:96` | False |
| `futon2.aif.interpretation-construction/construct` | `futon2/src/futon2/aif/interpretation_construction.clj:62` | False |
| `futon2.aif.interpretation-request/tension-citations` | `futon2/src/futon2/aif/interpretation_request.clj:69` | False |
| `futon2.aif.intrinsic-values/apply-update!` | `futon2/src/futon2/aif/intrinsic_values.clj:116` | False |
| `futon2.aif.intrinsic-values/persist-record!` | `futon2/src/futon2/aif/intrinsic_values.clj:253` | False |
| `futon2.aif.intrinsic-values/reset-to-prior!` | `futon2/src/futon2/aif/intrinsic_values.clj:123` | False |
| `futon2.aif.kernel-example/join-counts` | `futon2/src/futon2/aif/kernel_example.clj:34` | False |
| `futon2.aif.lane-futility/chosen-ranked-action` | `futon2/src/futon2/aif/lane_futility.clj:244` | False |
| `futon2.aif.lane-futility/dry-run-bulletins` | `futon2/src/futon2/aif/lane_futility.clj:315` | False |
| `futon2.aif.lane-futility/futility-summary` | `futon2/src/futon2/aif/lane_futility.clj:221` | False |
| `futon2.aif.lane-futility/hand-counts` | `futon2/src/futon2/aif/lane_futility.clj:226` | False |
| `futon2.aif.lane-futility/historical-gamma-report` | `futon2/src/futon2/aif/lane_futility.clj:284` | False |
| `futon2.aif.lane-futility/indexed-futility-summary` | `futon2/src/futon2/aif/lane_futility.clj:214` | False |
| `futon2.aif.lane-futility/summary-matches-hand-counts?` | `futon2/src/futon2/aif/lane_futility.clj:236` | False |
| `futon2.aif.lane-futility/synthetic-futility-summary` | `futon2/src/futon2/aif/lane_futility.clj:336` | False |
| `futon2.aif.lane-futility/synthetic-paying-gamma-report` | `futon2/src/futon2/aif/lane_futility.clj:307` | False |
| `futon2.aif.likelihood-precision/temper-a` | `futon2/src/futon2/aif/likelihood_precision.clj:99` | False |
| `futon2.aif.limb-evidence/validate-limb-bundle` | `futon2/src/futon2/aif/limb_evidence.clj:207` | False |
| `futon2.aif.live-c/cascade-spec` | `futon2/src/futon2/aif/live_c.clj:359` | False |
| `futon2.aif.live-c/derive-live-c` | `futon2/src/futon2/aif/live_c.clj:204` | False |
| `futon2.aif.live-c/family-schedule` | `futon2/src/futon2/aif/live_c.clj:337` | False |
| `futon2.aif.live-c/read-sources` | `futon2/src/futon2/aif/live_c.clj:104` | False |
| `futon2.aif.live-c/stale?` | `futon2/src/futon2/aif/live_c.clj:254` | False |
| `futon2.aif.machine-accumulation/initialize` | `futon2/src/futon2/aif/machine_accumulation.clj:7` | False |
| `futon2.aif.machine-accumulation/recurrence-valid?` | `futon2/src/futon2/aif/machine_accumulation.clj:37` | False |
| `futon2.aif.machine-budget-mapping/replay` | `futon2/src/futon2/aif/machine_budget_mapping.clj:215` | False |
| `futon2.aif.machine-forward-influence/verify-forward-influence` | `futon2/src/futon2/aif/machine_forward_influence.clj:55` | False |
| `futon2.aif.machine-model/require-outcome-vocabulary` | `futon2/src/futon2/aif/machine_model.clj:277` | False |
| `futon2.aif.machine-model/require-producer` | `futon2/src/futon2/aif/machine_model.clj:286` | False |
| `futon2.aif.machine-policy-set/compare-projections!` | `futon2/src/futon2/aif/machine_policy_set.clj:47` | False |
| `futon2.aif.machine-policy-set/read-projected-policy-set` | `futon2/src/futon2/aif/machine_policy_set.clj:63` | False |
| `futon2.aif.machine-portfolio-restriction/replay` | `futon2/src/futon2/aif/machine_portfolio_restriction.clj:120` | False |
| `futon2.aif.machine-predictive/declared-outcome-a` | `futon2/src/futon2/aif/machine_predictive.clj:85` | False |
| `futon2.aif.machine-predictive/predictive-outcome-kernel` | `futon2/src/futon2/aif/machine_predictive.clj:103` | False |
| `futon2.aif.machine-q/describe-refusal` | `futon2/src/futon2/aif/machine_q.clj:467` | False |
| `futon2.aif.machine-slow-feedback-completeness/validate` | `futon2/src/futon2/aif/machine_slow_feedback_completeness.clj:72` | False |
| `futon2.aif.machine-slow-feedback-evidence/validate-transition` | `futon2/src/futon2/aif/machine_slow_feedback_evidence.clj:290` | False |
| `futon2.aif.machine-slow-feedback-evidence/verify-feedback` | `futon2/src/futon2/aif/machine_slow_feedback_evidence.clj:368` | False |
| `futon2.aif.machine-slow-feedback-store/capture` | `futon2/src/futon2/aif/machine_slow_feedback_store.clj:306` | False |
| `futon2.aif.machine-slow-feedback-store/compare-and-commit!` | `futon2/src/futon2/aif/machine_slow_feedback_store.clj:266` | False |
| `futon2.aif.machine-slow-feedback-store/initialize!` | `futon2/src/futon2/aif/machine_slow_feedback_store.clj:243` | False |
| `futon2.aif.machine-slow-feedback-store/recover` | `futon2/src/futon2/aif/machine_slow_feedback_store.clj:305` | False |
| `futon2.aif.machine-slow-feedback-store-v2/capture` | `futon2/src/futon2/aif/machine_slow_feedback_store_v2.clj:369` | False |
| `futon2.aif.machine-slow-feedback-store-v2/commit!` | `futon2/src/futon2/aif/machine_slow_feedback_store_v2.clj:322` | False |
| `futon2.aif.machine-slow-feedback-store-v2/initialize!` | `futon2/src/futon2/aif/machine_slow_feedback_store_v2.clj:298` | False |
| `futon2.aif.machine-slow-feedback-store-v2/isolated-store` | `futon2/src/futon2/aif/machine_slow_feedback_store_v2.clj:153` | False |
| `futon2.aif.machine-slow-feedback-store-v2/recover` | `futon2/src/futon2/aif/machine_slow_feedback_store_v2.clj:368` | False |
| `futon2.aif.machine-slow-feedback-store-v2/release!` | `futon2/src/futon2/aif/machine_slow_feedback_store_v2.clj:179` | False |
| `futon2.aif.machine-transition/controlled-transition-kernel` | `futon2/src/futon2/aif/machine_transition.clj:11` | False |
| `futon2.aif.mana-gate/award!` | `futon2/src/futon2/aif/mana_gate.clj:94` | False |
| `futon2.aif.mana-gate/consume!` | `futon2/src/futon2/aif/mana_gate.clj:118` | False |
| `futon2.aif.mana-gate/ledger` | `futon2/src/futon2/aif/mana_gate.clj:146` | False |
| `futon2.aif.matched-observation-evidence/synthetic-matched-evidence` | `futon2/src/futon2/aif/matched_observation_evidence.clj:27` | False |
| `futon2.aif.measured-a-annotation/validate` | `futon2/src/futon2/aif/measured_a_annotation.clj:223` | False |
| `futon2.aif.memory-contract/agent-attribution-corpus` | `futon2/src/futon2/aif/memory_contract.clj:407` | False |
| `futon2.aif.mission-c/c-mis` | `futon2/src/futon2/aif/mission_c.clj:422` | False |
| `futon2.aif.mission-c/read-criteria` | `futon2/src/futon2/aif/mission_c.clj:377` | False |
| `futon2.aif.mission-c/risk-mis` | `futon2/src/futon2/aif/mission_c.clj:558` | False |
| `futon2.aif.mission-epistemic-value/field-readings` | `futon2/src/futon2/aif/mission_epistemic_value.clj:326` | False |
| `futon2.aif.mission-epistemic-value/record-for` | `futon2/src/futon2/aif/mission_epistemic_value.clj:339` | False |
| `futon2.aif.mission-gauges/reading` | `futon2/src/futon2/aif/mission_gauges.clj:346` | False |
| `futon2.aif.mission-hole-wants/merge-into-sources` | `futon2/src/futon2/aif/mission_hole_wants.clj:118` | False |
| `futon2.aif.mission-registry/mission-enumerator-proposer` | `futon2/src/futon2/aif/mission_registry.clj:556` | False |
| `futon2.aif.mission-registry/ticket-enumerator-proposer` | `futon2/src/futon2/aif/mission_registry.clj:653` | False |
| `futon2.aif.mission-registry/upsert-mission-record!` | `futon2/src/futon2/aif/mission_registry.clj:301` | False |
| `futon2.aif.mission-registry/work-target-status` | `futon2/src/futon2/aif/mission_registry.clj:641` | False |
| `futon2.aif.morning-brief/addenda` | `futon2/src/futon2/aif/morning_brief.clj:267` | False |
| `futon2.aif.morning-brief/addendum!` | `futon2/src/futon2/aif/morning_brief.clj:234` | False |
| `futon2.aif.morning-brief/objective-order` | `futon2/src/futon2/aif/morning_brief.clj:17` | False |
| `futon2.aif.morning-brief/queue-operator-gate!` | `futon2/src/futon2/aif/morning_brief.clj:135` | False |
| `futon2.aif.morning-brief/unseen-belief-events` | `futon2/src/futon2/aif/morning_brief.clj:289` | False |
| `futon2.aif.move-class-intensity/intensity-value` | `futon2/src/futon2/aif/move_class_intensity.clj:134` | False |
| `futon2.aif.move-class-intensity/score-bundle` | `futon2/src/futon2/aif/move_class_intensity.clj:137` | False |
| `futon2.aif.node-sim/carriers!` | `futon2/src/futon2/aif/node_sim.clj:161` | False |
| `futon2.aif.node-sim/simulate-node` | `futon2/src/futon2/aif/node_sim.clj:382` | False |
| `futon2.aif.observation/observe` | `futon2/src/futon2/aif/observation.clj:103` | False |
| `futon2.aif.observation/sense->vector` | `futon2/src/futon2/aif/observation.clj:148` | False |
| `futon2.aif.observation-admission/adjudication` | `futon2/src/futon2/aif/observation_admission.clj:28` | False |
| `futon2.aif.observation-admission/admit` | `futon2/src/futon2/aif/observation_admission.clj:44` | False |
| `futon2.aif.observation-admission/review` | `futon2/src/futon2/aif/observation_admission.clj:35` | False |
| `futon2.aif.observation-authority-resolver/build-resolver!` | `futon2/src/futon2/aif/observation_authority_resolver.clj:71` | False |
| `futon2.aif.observation-rates/sourced-rates` | `futon2/src/futon2/aif/observation_rates.clj:154` | False |
| `futon2.aif.on-demand-entrypoint/-main` | `futon2/src/futon2/aif/on_demand_entrypoint.clj:177` | False |
| `futon2.aif.parameter-delivery/g-with-information-gain` | `futon2/src/futon2/aif/parameter_delivery.clj:136` | False |
| `futon2.aif.parameter-delivery/refresh` | `futon2/src/futon2/aif/parameter_delivery.clj:114` | False |
| `futon2.aif.parameter-novelty/read-inputs` | `futon2/src/futon2/aif/parameter_novelty.clj:52` | False |
| `futon2.aif.pattern-registry/open-patterns` | `futon2/src/futon2/aif/pattern_registry.clj:262` | False |
| `futon2.aif.pattern-registry/pattern-enumerator-proposer` | `futon2/src/futon2/aif/pattern_registry.clj:350` | False |
| `futon2.aif.pattern-reliability/attest-patterns` | `futon2/src/futon2/aif/pattern_reliability.clj:97` | False |
| `futon2.aif.pattern-reliability/not-realised-note` | `futon2/src/futon2/aif/pattern_reliability.clj:15` | False |
| `futon2.aif.pattern-reliability/seed-counts` | `futon2/src/futon2/aif/pattern_reliability.clj:43` | False |
| `futon2.aif.pattern-reliability/trace-belongs-to-r` | `futon2/src/futon2/aif/pattern_reliability.clj:11` | False |
| `futon2.aif.policy/select-budgeted-actions` | `futon2/src/futon2/aif/policy.clj:27` | False |
| `futon2.aif.policy-depth/anticipation` | `futon2/src/futon2/aif/policy_depth.clj:24` | False |
| `futon2.aif.policy-depth/configured` | `futon2/src/futon2/aif/policy_depth.clj:13` | False |
| `futon2.aif.policy-free-energy/f-pi-vector` | `futon2/src/futon2/aif/policy_free_energy.clj:146` | False |
| `futon2.aif.policy-precision-carry/advance` | `futon2/src/futon2/aif/policy_precision_carry.clj:80` | False |
| `futon2.aif.policy-precision-carry/family` | `futon2/src/futon2/aif/policy_precision_carry.clj:50` | False |
| `futon2.aif.policy-prefix-evidence/evaluate-synthetic` | `futon2/src/futon2/aif/policy_prefix_evidence.clj:17` | False |
| `futon2.aif.policy-prefix-evidence/production-ranked` | `futon2/src/futon2/aif/policy_prefix_evidence.clj:56` | False |
| `futon2.aif.portfolio-action-proposer/dry-run-portfolio` | `futon2/src/futon2/aif/portfolio_action_proposer.clj:140` | False |
| `futon2.aif.portfolio-action-proposer/portfolio-action-proposer` | `futon2/src/futon2/aif/portfolio_action_proposer.clj:106` | False |
| `futon2.aif.precision/initial-precision-state` | `futon2/src/futon2/aif/precision.clj:87` | False |
| `futon2.aif.precision/salience-for` | `futon2/src/futon2/aif/precision.clj:201` | False |
| `futon2.aif.precision/update-precision-state` | `futon2/src/futon2/aif/precision.clj:160` | False |
| `futon2.aif.precision/weighted-error` | `futon2/src/futon2/aif/precision.clj:212` | False |
| `futon2.aif.preference-discovery/extract` | `futon2/src/futon2/aif/preference_discovery.clj:30` | False |
| `futon2.aif.preference-discovery/extract-landscape` | `futon2/src/futon2/aif/preference_discovery.clj:78` | False |
| `futon2.aif.preference-module/compare-satisfaction` | `futon2/src/futon2/aif/preference_module.clj:131` | False |
| `futon2.aif.preference-module/instantiate` | `futon2/src/futon2/aif/preference_module.clj:57` | False |
| `futon2.aif.preferences/channel-health-signs` | `futon2/src/futon2/aif/preferences.clj:72` | False |
| `futon2.aif.preferences/mode-prior` | `futon2/src/futon2/aif/preferences.clj:35` | False |
| `futon2.aif.preferences/point-mass-divergence` | `futon2/src/futon2/aif/preferences.clj:512` | False |
| `futon2.aif.r17-offline/replay` | `futon2/src/futon2/aif/r17_offline.clj:98` | False |
| `futon2.aif.r9-checker/checker-admission-status` | `futon2/src/futon2/aif/r9_checker.clj:10` | False |
| `futon2.aif.realized-outcome/categorical-outcome` | `futon2/src/futon2/aif/realized_outcome.clj:108` | False |
| `futon2.aif.realized-outcome/conforms?` | `futon2/src/futon2/aif/realized_outcome.clj:140` | False |
| `futon2.aif.realized-outcome/legs-readable?` | `futon2/src/futon2/aif/realized_outcome.clj:103` | False |
| `futon2.aif.realized-outcome/mixed-vocabulary?` | `futon2/src/futon2/aif/realized_outcome.clj:84` | False |
| `futon2.aif.realized-outcome/normalize` | `futon2/src/futon2/aif/realized_outcome.clj:126` | False |
| `futon2.aif.realized-outcome/realized-outcome` | `futon2/src/futon2/aif/realized_outcome.clj:121` | False |
| `futon2.aif.realized-recording/adapter-error` | `futon2/src/futon2/aif/realized_recording.clj:22` | False |
| `futon2.aif.realized-recording/persist!` | `futon2/src/futon2/aif/realized_recording.clj:228` | False |
| `futon2.aif.realized-recording/preference-readings` | `futon2/src/futon2/aif/realized_recording.clj:149` | False |
| `futon2.aif.realized-recording/step-envelope` | `futon2/src/futon2/aif/realized_recording.clj:244` | False |
| `futon2.aif.realized-recording/terminal-disposition` | `futon2/src/futon2/aif/realized_recording.clj:188` | False |
| `futon2.aif.repair-history-replay/-main` | `futon2/src/futon2/aif/repair_history_replay.clj:66` | False |
| `futon2.aif.repair-obligation/dismiss-condition-cleared!` | `futon2/src/futon2/aif/repair_obligation.clj:975` | False |
| `futon2.aif.repair-obligation/dismiss-echo!` | `futon2/src/futon2/aif/repair_obligation.clj:1082` | False |
| `futon2.aif.repair-obligation/dismiss-fixture-pollution!` | `futon2/src/futon2/aif/repair_obligation.clj:1159` | False |
| `futon2.aif.repair-obligation/dismiss-superseded-attempt!` | `futon2/src/futon2/aif/repair_obligation.clj:1246` | False |
| `futon2.aif.repair-obligation/dismiss-unexecuted!` | `futon2/src/futon2/aif/repair_obligation.clj:1016` | False |
| `futon2.aif.repair-obligation/dismiss-wontfix!` | `futon2/src/futon2/aif/repair_obligation.clj:939` | False |
| `futon2.aif.repair-obligation/record-historical-verification!` | `futon2/src/futon2/aif/repair_obligation.clj:740` | False |
| `futon2.aif.repair-obligation/repair-derived-state` | `futon2/src/futon2/aif/repair_obligation.clj:1633` | False |
| `futon2.aif.revision-scanner/-main` | `futon2/src/futon2/aif/revision_scanner.clj:174` | False |
| `futon2.aif.rollout/drift-roots` | `futon2/src/futon2/aif/rollout.clj:70` | False |
| `futon2.aif.rollout/expand-policies` | `futon2/src/futon2/aif/rollout.clj:593` | False |
| `futon2.aif.rollout/greedy-one-step` | `futon2/src/futon2/aif/rollout.clj:691` | False |
| `futon2.aif.rollout/move-cost` | `futon2/src/futon2/aif/rollout.clj:451` | False |
| `futon2.aif.rollout/move-cost-events` | `futon2/src/futon2/aif/rollout.clj:374` | False |
| `futon2.aif.rollout/ranked-survivors` | `futon2/src/futon2/aif/rollout.clj:446` | False |
| `futon2.aif.rollout/renormalize-priors` | `futon2/src/futon2/aif/rollout.clj:394` | False |
| `futon2.aif.rollout/seed-roots` | `futon2/src/futon2/aif/rollout.clj:80` | False |
| `futon2.aif.ruled-outcome-c/require-machine-preference` | `futon2/src/futon2/aif/ruled_outcome_c.clj:178` | False |
| `futon2.aif.ruled-outcome-c/unsupported-risk` | `futon2/src/futon2/aif/ruled_outcome_c.clj:198` | False |
| `futon2.aif.run-narrative/-main` | `futon2/src/futon2/aif/run_narrative.clj:622` | False |
| `futon2.aif.run-participants/read-role` | `futon2/src/futon2/aif/run_participants.clj:39` | False |
| `futon2.aif.run4-route-conformance/conforms-routes?` | `futon2/src/futon2/aif/run4_route_conformance.clj:74` | False |
| `futon2.aif.scheduled-route-evidence/file-authority` | `futon2/src/futon2/aif/scheduled_route_evidence.clj:57` | False |
| `futon2.aif.scheduled-route-evidence/verify-route!` | `futon2/src/futon2/aif/scheduled_route_evidence.clj:118` | False |
| `futon2.aif.scoring-input-receipts/validate-record` | `futon2/src/futon2/aif/scoring_input_receipts.clj:101` | False |
| `futon2.aif.scoring-input-receipts/with-preference-audit` | `futon2/src/futon2/aif/scoring_input_receipts.clj:29` | False |
| `futon2.aif.selection-authoring-coupling/parse-args` | `futon2/src/futon2/aif/selection_authoring_coupling.clj:200` | False |
| `futon2.aif.selection-authoring-coupling/run-once!` | `futon2/src/futon2/aif/selection_authoring_coupling.clj:156` | False |
| `futon2.aif.selection-authoring-coupling/stable-summary` | `futon2/src/futon2/aif/selection_authoring_coupling.clj:146` | False |
| `futon2.aif.selection-gain/coerce-state` | `futon2/src/futon2/aif/selection_gain.clj:248` | False |
| `futon2.aif.selection-gain/selection-gain-for` | `futon2/src/futon2/aif/selection_gain.clj:265` | False |
| `futon2.aif.selection-rationale/emit!` | `futon2/src/futon2/aif/selection_rationale.clj:358` | False |
| `futon2.aif.selection-rationale/read-store` | `futon2/src/futon2/aif/selection_rationale.clj:392` | False |
| `futon2.aif.sorry-registry/sorry-enumerator-proposer` | `futon2/src/futon2/aif/sorry_registry.clj:110` | False |
| `futon2.aif.strategic-habit/carry` | `futon2/src/futon2/aif/strategic_habit.clj:103` | False |
| `futon2.aif.strategic-habit/enabled?` | `futon2/src/futon2/aif/strategic_habit.clj:8` | False |
| `futon2.aif.strategic-habit/require-promotable` | `futon2/src/futon2/aif/strategic_habit.clj:110` | False |
| `futon2.aif.substrate/hyperedges-by-end` | `futon2/src/futon2/aif/substrate.clj:119` | False |
| `futon2.aif.substrate/relations` | `futon2/src/futon2/aif/substrate.clj:138` | False |
| `futon2.aif.survey-mission-value/discharge-shape` | `futon2/src/futon2/aif/survey_mission_value.clj:392` | False |
| `futon2.aif.survey-mission-value/near-miss-phrasings` | `futon2/src/futon2/aif/survey_mission_value.clj:125` | False |
| `futon2.aif.survey-mission-value/readings-from-missions` | `futon2/src/futon2/aif/survey_mission_value.clj:355` | False |
| `futon2.aif.task-belief-ladder/apply-ladder` | `futon2/src/futon2/aif/task_belief_ladder.clj:350` | False |
| `futon2.aif.task-belief-ladder/field-context` | `futon2/src/futon2/aif/task_belief_ladder.clj:167` | False |
| `futon2.aif.task-belief-ladder/refusal-reasons` | `futon2/src/futon2/aif/task_belief_ladder.clj:311` | False |
| `futon2.aif.task-belief-ladder/refusal-tension` | `futon2/src/futon2/aif/task_belief_ladder.clj:440` | False |
| `futon2.aif.temporal-hierarchy/hierarchical-rollout` | `futon2/src/futon2/aif/temporal_hierarchy.clj:165` | False |
| `futon2.aif.token-belief-carry/domain-inputs` | `futon2/src/futon2/aif/token_belief_carry.clj:10` | False |
| `futon2.aif.token-belief-predecessor/inspect-trace` | `futon2/src/futon2/aif/token_belief_predecessor.clj:15` | False |
| `futon2.aif.trace/read-all-traces` | `futon2/src/futon2/aif/trace.clj:720` | False |
| `futon2.aif.trace/recent-trace-records` | `futon2/src/futon2/aif/trace.clj:731` | False |
| `futon2.aif.trace/reduce-traces` | `futon2/src/futon2/aif/trace.clj:748` | False |
| `futon2.aif.trace/trace-field-evidence` | `futon2/src/futon2/aif/trace.clj:372` | False |
| `futon2.aif.trace/wm-version-of` | `futon2/src/futon2/aif/trace.clj:353` | False |
| `futon2.aif.tripwire/check!` | `futon2/src/futon2/aif/tripwire.clj:854` | False |
| `futon2.aif.tripwire/composition-baseline` | `futon2/src/futon2/aif/tripwire.clj:76` | False |
| `futon2.aif.tripwire/set-wire-enabled!` | `futon2/src/futon2/aif/tripwire.clj:100` | False |
| `futon2.aif.tripwire-calibration/-main` | `futon2/src/futon2/aif/tripwire_calibration.clj:151` | False |
| `futon2.aif.work-target-belief/read-declaration` | `futon2/src/futon2/aif/work_target_belief.clj:34` | False |
| `futon2.aif.work-target-predictor-input/predictor-input` | `futon2/src/futon2/aif/work_target_predictor_input.clj:31` | False |
| `futon2.aif.work-target-store/commit!` | `futon2/src/futon2/aif/work_target_store.clj:313` | False |
| `futon2.aif.work-target-store/initialize!` | `futon2/src/futon2/aif/work_target_store.clj:262` | False |
| `futon2.aif.work-target-store/open-store` | `futon2/src/futon2/aif/work_target_store.clj:16` | False |
| `futon2.aif.work-target-store/resolve-reference` | `futon2/src/futon2/aif/work_target_store.clj:379` | False |
| `futon2.aif.work-target-tick/build-proposal` | `futon2/src/futon2/aif/work_target_tick.clj:176` | False |
| `futon2.aif2.preference/credit` | `futon2/src/futon2/aif2/preference.clj:96` | False |
| `futon2.aif2.preference/default-consent` | `futon2/src/futon2/aif2/preference.clj:69` | False |
| `futon2.aif2.preference/reduces-to-static?` | `futon2/src/futon2/aif2/preference.clj:102` | False |
| `futon2.aif2.tension/emits-nothing-without-signal?` | `futon2/src/futon2/aif2/tension.clj:165` | False |
| `futon2.aif2.tension/read-curvature-signal` | `futon2/src/futon2/aif2/tension.clj:198` | False |
| `futon2.aif2.tension/tension-proposer` | `futon2/src/futon2/aif2/tension.clj:143` | False |
| `futon2.patchboard/analyse` | `futon2/src/futon2/patchboard.clj:79` | False |
| `futon2.patchboard/clamp-shift-wiring` | `futon2/src/futon2/patchboard.clj:42` | False |
| `futon2.patchboard/identity-wiring` | `futon2/src/futon2/patchboard.clj:39` | False |
| `futon2.patchboard/metaca-terminals` | `futon2/src/futon2/patchboard.clj:14` | False |
| `futon2.wm-run-lock/-main` | `futon2/src/futon2/wm_run_lock.clj:247` | False |
| `futon2.wm-run-lock/call-with-run-lock` | `futon2/src/futon2/wm_run_lock.clj:234` | False |
| `futon2.wm-run-lock/refused?` | `futon2/src/futon2/wm_run_lock.clj:242` | False |
| `futon3c.agency.agent-pouch/allow-monster!` | `futon3c/src/futon3c/agency/agent_pouch.clj:151` | False |
| `futon3c.agency.agent-pouch/clear!` | `futon3c/src/futon3c/agency/agent_pouch.clj:360` | False |
| `futon3c.agency.agent-pouch/disallow-monster!` | `futon3c/src/futon3c/agency/agent_pouch.clj:155` | False |
| `futon3c.agency.agent-pouch/feed-turn!` | `futon3c/src/futon3c/agency/agent_pouch.clj:1020` | False |
| `futon3c.agency.agent-pouch/joey-eligible-or-compact?` | `futon3c/src/futon3c/agency/agent_pouch.clj:250` | False |
| `futon3c.agency.agent-pouch/make-unsolicited-sink` | `futon3c/src/futon3c/agency/agent_pouch.clj:62` | False |
| `futon3c.agency.agent-pouch/note-monster-cold!` | `futon3c/src/futon3c/agency/agent_pouch.clj:186` | False |
| `futon3c.agency.agent-pouch/set-unsolicited-sink!` | `futon3c/src/futon3c/agency/agent_pouch.clj:73` | False |
| `futon3c.agency.bg-process/forget!` | `futon3c/src/futon3c/agency/bg_process.clj:157` | False |
| `futon3c.agency.bg-process/kill!` | `futon3c/src/futon3c/agency/bg_process.clj:145` | False |
| `futon3c.agency.bg-process/launch!` | `futon3c/src/futon3c/agency/bg_process.clj:72` | False |
| `futon3c.agency.bg-process/list-tasks` | `futon3c/src/futon3c/agency/bg_process.clj:127` | False |
| `futon3c.agency.bg-process/tail` | `futon3c/src/futon3c/agency/bg_process.clj:135` | False |
| `futon3c.agency.clock-decision/finish!` | `futon3c/src/futon3c/agency/clock_decision.clj:343` | False |
| `futon3c.agency.clock-decision/start!` | `futon3c/src/futon3c/agency/clock_decision.clj:284` | False |
| `futon3c.agency.clock-lineage/clock-dispatch!` | `futon3c/src/futon3c/agency/clock_lineage.clj:163` | False |
| `futon3c.agency.clock-lineage/clock-edit!` | `futon3c/src/futon3c/agency/clock_lineage.clj:179` | False |
| `futon3c.agency.clock-store/evidence-clock-fields` | `futon3c/src/futon3c/agency/clock_store.clj:253` | False |
| `futon3c.agency.clock-store/reset-store!` | `futon3c/src/futon3c/agency/clock_store.clj:48` | False |
| `futon3c.agency.fed-uplink/start-uplink!` | `futon3c/src/futon3c/agency/fed_uplink.clj:253` | False |
| `futon3c.agency.federation/configure-from-env!` | `futon3c/src/futon3c/agency/federation.clj:96` | False |
| `futon3c.agency.federation/install-hook!` | `futon3c/src/futon3c/agency/federation.clj:495` | False |
| `futon3c.agency.federation/prune-stale-uplink-site!` | `futon3c/src/futon3c/agency/federation.clj:891` | False |
| `futon3c.agency.federation/remove-hook!` | `futon3c/src/futon3c/agency/federation.clj:501` | False |
| `futon3c.agency.federation/reset-sync-state!` | `futon3c/src/futon3c/agency/federation.clj:1170` | False |
| `futon3c.agency.federation/start-sync-daemon!` | `futon3c/src/futon3c/agency/federation.clj:1130` | False |
| `futon3c.agency.federation/sync-peers!` | `futon3c/src/futon3c/agency/federation.clj:1076` | False |
| `futon3c.agency.followup-queue/clear!` | `futon3c/src/futon3c/agency/followup_queue.clj:31` | False |
| `futon3c.agency.followup-queue/snapshot` | `futon3c/src/futon3c/agency/followup_queue.clj:32` | False |
| `futon3c.agency.hop-events/log-hop-event!` | `futon3c/src/futon3c/agency/hop_events.clj:57` | False |
| `futon3c.agency.invariants/warn-queue-hardening!` | `futon3c/src/futon3c/agency/invariants.clj:52` | False |
| `futon3c.agency.invoke-controls/interrupt!` | `futon3c/src/futon3c/agency/invoke_controls.clj:42` | False |
| `futon3c.agency.invoke-controls/snapshot` | `futon3c/src/futon3c/agency/invoke_controls.clj:71` | False |
| `futon3c.agency.invoke-ingress-controller/acknowledge-resume!` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:325` | False |
| `futon3c.agency.invoke-ingress-controller/close-intake!` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:284` | False |
| `futon3c.agency.invoke-ingress-controller/controller` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:164` | False |
| `futon3c.agency.invoke-ingress-controller/defer-resume!` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:296` | False |
| `futon3c.agency.invoke-ingress-controller/file-deferred-store` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:71` | False |
| `futon3c.agency.invoke-ingress-controller/initialize-file-store!` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:132` | False |
| `futon3c.agency.invoke-ingress-controller/release-controller!` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:203` | False |
| `futon3c.agency.invoke-ingress-controller/reopen!` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:317` | False |
| `futon3c.agency.invoke-ingress-controller/verification-snapshot` | `futon3c/src/futon3c/agency/invoke_ingress_controller.clj:341` | False |
| `futon3c.agency.invoke-lifecycle-reconciliation/file-snapshot-resolver` | `futon3c/src/futon3c/agency/invoke_lifecycle_reconciliation.clj:20` | False |
| `futon3c.agency.invoke-lifecycle-reconciliation/reconcile` | `futon3c/src/futon3c/agency/invoke_lifecycle_reconciliation.clj:82` | False |
| `futon3c.agency.invoke-lifecycle-snapshot/boundary` | `futon3c/src/futon3c/agency/invoke_lifecycle_snapshot.clj:29` | False |
| `futon3c.agency.invoke-lifecycle-snapshot/capture!` | `futon3c/src/futon3c/agency/invoke_lifecycle_snapshot.clj:92` | False |
| `futon3c.agency.invoke-lifecycle-snapshot/capture-resolvers` | `futon3c/src/futon3c/agency/invoke_lifecycle_snapshot.clj:135` | False |
| `futon3c.agency.invoke-lifecycle-snapshot/mutate!` | `futon3c/src/futon3c/agency/invoke_lifecycle_snapshot.clj:71` | False |
| `futon3c.agency.invoke-lifecycle-snapshot/register-provider!` | `futon3c/src/futon3c/agency/invoke_lifecycle_snapshot.clj:56` | False |
| `futon3c.agency.job-tree/adopt-process!` | `futon3c/src/futon3c/agency/job_tree.clj:444` | False |
| `futon3c.agency.job-tree/clear!` | `futon3c/src/futon3c/agency/job_tree.clj:641` | False |
| `futon3c.agency.job-tree/forget-job!` | `futon3c/src/futon3c/agency/job_tree.clj:372` | False |
| `futon3c.agency.job-tree/start!` | `futon3c/src/futon3c/agency/job_tree.clj:652` | False |
| `futon3c.agency.logic/ag-2-phantomo` | `futon3c/src/futon3c/agency/logic.clj:409` | False |
| `futon3c.agency.logic/ag-7-unaccepting-servero` | `futon3c/src/futon3c/agency/logic.clj:454` | False |
| `futon3c.agency.logic/ag-8-roster-incompleteo` | `futon3c/src/futon3c/agency/logic.clj:460` | False |
| `futon3c.agency.logic/agent-has-typed-ido` | `futon3c/src/futon3c/agency/logic.clj:358` | False |
| `futon3c.agency.logic/build-live-db` | `futon3c/src/futon3c/agency/logic.clj:844` | False |
| `futon3c.agency.logic/check-registry` | `futon3c/src/futon3c/agency/logic.clj:849` | False |
| `futon3c.agency.logic/find-phantoms` | `futon3c/src/futon3c/agency/logic.clj:754` | False |
| `futon3c.agency.logic/find-roster-incomplete` | `futon3c/src/futon3c/agency/logic.clj:782` | False |
| `futon3c.agency.logic/find-session-collisions` | `futon3c/src/futon3c/agency/logic.clj:764` | False |
| `futon3c.agency.logic/find-unpropagated` | `futon3c/src/futon3c/agency/logic.clj:759` | False |
| `futon3c.agency.logic/find-unreachable-connected` | `futon3c/src/futon3c/agency/logic.clj:770` | False |
| `futon3c.agency.logic/invoking-has-timestampo` | `futon3c/src/futon3c/agency/logic.clj:482` | False |
| `futon3c.agency.logic/route-consistento` | `futon3c/src/futon3c/agency/logic.clj:472` | False |
| `futon3c.agency.logic/violations?` | `futon3c/src/futon3c/agency/logic.clj:794` | False |
| `futon3c.agency.parked-on/clear!` | `futon3c/src/futon3c/agency/parked_on.clj:99` | False |
| `futon3c.agency.r9-authority/configured-resolvers` | `futon3c/src/futon3c/agency/r9_authority.clj:109` | False |
| `futon3c.agency.r9-genesis/unresolved-host-event-stub` | `futon3c/src/futon3c/agency/r9_genesis.clj:177` | False |
| `futon3c.agency.r9-genesis/verify-candidate` | `futon3c/src/futon3c/agency/r9_genesis.clj:68` | False |
| `futon3c.agency.registry/backpack` | `futon3c/src/futon3c/agency/registry.clj:1888` | False |
| `futon3c.agency.registry/backpack-add!` | `futon3c/src/futon3c/agency/registry.clj:1875` | False |
| `futon3c.agency.registry/backpack-clear!` | `futon3c/src/futon3c/agency/registry.clj:1882` | False |
| `futon3c.agency.registry/current-inhabitant` | `futon3c/src/futon3c/agency/registry.clj:919` | False |
| `futon3c.agency.registry/current-peripheral` | `futon3c/src/futon3c/agency/registry.clj:914` | False |
| `futon3c.agency.registry/deregister-agent!` | `futon3c/src/futon3c/agency/registry.clj:662` | False |
| `futon3c.agency.registry/hop!` | `futon3c/src/futon3c/agency/registry.clj:773` | False |
| `futon3c.agency.registry/hop-back!` | `futon3c/src/futon3c/agency/registry.clj:855` | False |
| `futon3c.agency.registry/hop-stack` | `futon3c/src/futon3c/agency/registry.clj:924` | False |
| `futon3c.agency.registry/reap-expired!` | `futon3c/src/futon3c/agency/registry.clj:1253` | False |
| `futon3c.agency.registry/reconcile-stale-invoking!` | `futon3c/src/futon3c/agency/registry.clj:1400` | False |
| `futon3c.agency.registry/reset-registry!` | `futon3c/src/futon3c/agency/registry.clj:241` | False |
| `futon3c.agency.registry/set-on-idle!` | `futon3c/src/futon3c/agency/registry.clj:259` | False |
| `futon3c.agency.registry/set-on-invoke-complete!` | `futon3c/src/futon3c/agency/registry.clj:252` | False |
| `futon3c.agency.registry/shutdown-all!` | `futon3c/src/futon3c/agency/registry.clj:1868` | False |
| `futon3c.agency.roster-store/install-registry-watch!` | `futon3c/src/futon3c/agency/roster_store.clj:242` | False |
| `futon3c.agency.roster-store/restore-on-boot!` | `futon3c/src/futon3c/agency/roster_store.clj:260` | False |
| `futon3c.agency.selective-form-loader/activate-http-retention!` | `futon3c/src/futon3c/agency/selective_form_loader.clj:103` | False |
| `futon3c.agency.selective-form-loader/current-serving-preflight` | `futon3c/src/futon3c/agency/selective_form_loader.clj:65` | False |
| `futon3c.agency.selective-form-loader/http-sha256` | `futon3c/src/futon3c/agency/selective_form_loader.clj:8` | False |
| `futon3c.agency.selective-form-loader/load-transactionally!` | `futon3c/src/futon3c/agency/selective_form_loader.clj:72` | False |
| `futon3c.agency.selective-form-loader/read-pinned-forms` | `futon3c/src/futon3c/agency/selective_form_loader.clj:28` | False |
| `futon3c.agency.selective-form-loader/required-http-forms` | `futon3c/src/futon3c/agency/selective_form_loader.clj:9` | False |
| `futon3c.agency.turn-queue/accept-and-drain!` | `futon3c/src/futon3c/agency/turn_queue.clj:552` | False |
| `futon3c.agency.turn-queue/accept-block!` | `futon3c/src/futon3c/agency/turn_queue.clj:714` | False |
| `futon3c.agency.turn-queue/clear!` | `futon3c/src/futon3c/agency/turn_queue.clj:168` | False |
| `futon3c.agency.turn-queue/resume-pending-drainers!` | `futon3c/src/futon3c/agency/turn_queue.clj:654` | False |
| `futon3c.agents.apm-work-queue/apm-phase-validator` | `futon3c/src/futon3c/agents/apm_work_queue.clj:699` | False |
| `futon3c.agents.apm-work-queue/emit-apm-evidence!` | `futon3c/src/futon3c/agents/apm_work_queue.clj:881` | False |
| `futon3c.agents.apm-work-queue/make-classify-prompt` | `futon3c/src/futon3c/agents/apm_work_queue.clj:493` | False |
| `futon3c.agents.apm-work-queue/make-execute-prompt` | `futon3c/src/futon3c/agents/apm_work_queue.clj:374` | False |
| `futon3c.agents.apm-work-queue/make-integrate-prompt` | `futon3c/src/futon3c/agents/apm_work_queue.clj:503` | False |
| `futon3c.agents.apm-work-queue/make-observe-prompt` | `futon3c/src/futon3c/agents/apm_work_queue.clj:285` | False |
| `futon3c.agents.apm-work-queue/make-propose-prompt` | `futon3c/src/futon3c/agents/apm_work_queue.clj:303` | False |
| `futon3c.agents.apm-work-queue/make-target-check-prompt` | `futon3c/src/futon3c/agents/apm_work_queue.clj:320` | False |
| `futon3c.agents.apm-work-queue/make-validate-prompt` | `futon3c/src/futon3c/agents/apm_work_queue.clj:480` | False |
| `futon3c.agents.apm-work-queue/next-unprocessed` | `futon3c/src/futon3c/agents/apm_work_queue.clj:858` | False |
| `futon3c.agents.apm-work-queue/queue-status` | `futon3c/src/futon3c/agents/apm_work_queue.clj:843` | False |
| `futon3c.agents.apm-work-queue/subject-summary` | `futon3c/src/futon3c/agents/apm_work_queue.clj:919` | False |
| `futon3c.agents.arse-work-queue/emit-arse-evidence!` | `futon3c/src/futon3c/agents/arse_work_queue.clj:154` | False |
| `futon3c.agents.arse-work-queue/entities-by-problem` | `futon3c/src/futon3c/agents/arse_work_queue.clj:185` | False |
| `futon3c.agents.arse-work-queue/make-review-prompt` | `futon3c/src/futon3c/agents/arse_work_queue.clj:81` | False |
| `futon3c.agents.arse-work-queue/next-unprocessed` | `futon3c/src/futon3c/agents/arse_work_queue.clj:134` | False |
| `futon3c.agents.arse-work-queue/queue-manifest` | `futon3c/src/futon3c/agents/arse_work_queue.clj:190` | False |
| `futon3c.agents.arse-work-queue/queue-status` | `futon3c/src/futon3c/agents/arse_work_queue.clj:120` | False |
| `futon3c.agents.cascade-verifier-board/run-live!` | `futon3c/src/futon3c/agents/cascade_verifier_board.clj:139` | False |
| `futon3c.agents.codex-cli/event->activity` | `futon3c/src/futon3c/agents/codex_cli.clj:148` | False |
| `futon3c.agents.codex-cli/event->ledger-event` | `futon3c/src/futon3c/agents/codex_cli.clj:77` | False |
| `futon3c.agents.codex-cli/execution-evidence?` | `futon3c/src/futon3c/agents/codex_cli.clj:243` | False |
| `futon3c.agents.codex-cli/make-invoke-fn` | `futon3c/src/futon3c/agents/codex_cli.clj:556` | False |
| `futon3c.agents.codex-code-logic/announcement-backed-by-jobo` | `futon3c/src/futon3c/agents/codex_code_logic.clj:114` | False |
| `futon3c.agents.codex-code-logic/build-live-db` | `futon3c/src/futon3c/agents/codex_code_logic.clj:95` | False |
| `futon3c.agents.codex-code-logic/query-violations` | `futon3c/src/futon3c/agents/codex_code_logic.clj:268` | False |
| `futon3c.agents.codex-code-logic/running-job-implies-invokingo` | `futon3c/src/futon3c/agents/codex_code_logic.clj:108` | False |
| `futon3c.agents.codex-code-logic/running-session-alignedo` | `futon3c/src/futon3c/agents/codex_code_logic.clj:120` | False |
| `futon3c.agents.inbox-zero-board-live/-main` | `futon3c/src/futon3c/agents/inbox_zero_board_live.clj:128` | False |
| `futon3c.agents.memory-mcp/-main` | `futon3c/src/futon3c/agents/memory_mcp.clj:173` | False |
| `futon3c.agents.memory-mcp-test/codex-adapter-shaped-call-reaches-writer-with-controller-authorship` | `futon3c/src/futon3c/agents/memory_mcp_test.clj:103` | False |
| `futon3c.agents.memory-mcp-test/configured-seats-have-mathematics-domain-and-scribe-is-exclusive` | `futon3c/src/futon3c/agents/memory_mcp_test.clj:12` | False |
| `futon3c.agents.memory-mcp-test/mcp-tool-list-teaches-required-contract` | `futon3c/src/futon3c/agents/memory_mcp_test.clj:19` | False |
| `futon3c.agents.memory-mcp-test/memory-call-stamps-controller-identity-and-domain` | `futon3c/src/futon3c/agents/memory_mcp_test.clj:32` | False |
| `futon3c.agents.memory-mcp-test/memory-search-queries-explicit-store-and-never-records` | `futon3c/src/futon3c/agents/memory_mcp_test.clj:55` | False |
| `futon3c.agents.memory-mcp-test/unknown-mcp-tool-still-returns-invalid-params` | `futon3c/src/futon3c/agents/memory_mcp_test.clj:96` | False |
| `futon3c.agents.mfuton-invoke-override/claude-role-codex-opts` | `futon3c/src/futon3c/agents/mfuton_invoke_override.clj:11` | False |
| `futon3c.agents.mfuton-invoke-override/maybe-record-delivery!` | `futon3c/src/futon3c/agents/mfuton_invoke_override.clj:47` | False |
| `futon3c.agents.mfuton-prompt-override/maybe-fm-dispatch-message` | `futon3c/src/futon3c/agents/mfuton_prompt_override.clj:191` | False |
| `futon3c.agents.mfuton-prompt-override/maybe-math-irc-invoke-prompt` | `futon3c/src/futon3c/agents/mfuton_prompt_override.clj:175` | False |
| `futon3c.agents.mfuton-prompt-override/maybe-task-prompt` | `futon3c/src/futon3c/agents/mfuton_prompt_override.clj:206` | False |
| `futon3c.agents.tickle/invoke!` | `futon3c/src/futon3c/agents/tickle.clj:287` | False |
| `futon3c.agents.tickle/start-watchdog!` | `futon3c/src/futon3c/agents/tickle.clj:310` | False |
| `futon3c.agents.tickle-logic/build-live-db` | `futon3c/src/futon3c/agents/tickle_logic.clj:251` | False |
| `futon3c.agents.tickle-logic/escalation-backed-by-pageo` | `futon3c/src/futon3c/agents/tickle_logic.clj:286` | False |
| `futon3c.agents.tickle-logic/page-backed-by-assignmento` | `futon3c/src/futon3c/agents/tickle_logic.clj:280` | False |
| `futon3c.agents.tickle-logic/page-cause-valido` | `futon3c/src/futon3c/agents/tickle_logic.clj:293` | False |
| `futon3c.agents.tickle-logic/page-target-valido` | `futon3c/src/futon3c/agents/tickle_logic.clj:272` | False |
| `futon3c.agents.tickle-logic/query-watchdog-probe-candidates` | `futon3c/src/futon3c/agents/tickle_logic.clj:570` | False |
| `futon3c.agents.tickle-orchestrate/fetch-issue!` | `futon3c/src/futon3c/agents/tickle_orchestrate.clj:213` | False |
| `futon3c.agents.tickle-orchestrate/fetch-open-issues!` | `futon3c/src/futon3c/agents/tickle_orchestrate.clj:238` | False |
| `futon3c.agents.tickle-orchestrate/kick-queue!` | `futon3c/src/futon3c/agents/tickle_orchestrate.clj:623` | False |
| `futon3c.agents.tickle-orchestrate/run-batch!` | `futon3c/src/futon3c/agents/tickle_orchestrate.clj:635` | False |
| `futon3c.agents.tickle-orchestrate/start-fm-conductor!` | `futon3c/src/futon3c/agents/tickle_orchestrate.clj:846` | False |
| `futon3c.agents.tickle-orchestrate/stop-fm-conductor!` | `futon3c/src/futon3c/agents/tickle_orchestrate.clj:905` | False |
| `futon3c.agents.tickle-queue/add-task!` | `futon3c/src/futon3c/agents/tickle_queue.clj:40` | False |
| `futon3c.agents.tickle-queue/agent-healthy?` | `futon3c/src/futon3c/agents/tickle_queue.clj:246` | False |
| `futon3c.agents.tickle-queue/agent-task` | `futon3c/src/futon3c/agents/tickle_queue.clj:235` | False |
| `futon3c.agents.tickle-queue/all-tasks` | `futon3c/src/futon3c/agents/tickle_queue.clj:66` | False |
| `futon3c.agents.tickle-queue/assigned-count` | `futon3c/src/futon3c/agents/tickle_queue.clj:213` | False |
| `futon3c.agents.tickle-queue/clear!` | `futon3c/src/futon3c/agents/tickle_queue.clj:221` | False |
| `futon3c.agents.tickle-queue/complete-task!` | `futon3c/src/futon3c/agents/tickle_queue.clj:110` | False |
| `futon3c.agents.tickle-queue/drain-pending!` | `futon3c/src/futon3c/agents/tickle_queue.clj:202` | False |
| `futon3c.agents.tickle-queue/enqueue!` | `futon3c/src/futon3c/agents/tickle_queue.clj:150` | False |
| `futon3c.agents.tickle-queue/fail-task!` | `futon3c/src/futon3c/agents/tickle_queue.clj:133` | False |
| `futon3c.agents.tickle-queue/failing-agents` | `futon3c/src/futon3c/agents/tickle_queue.clj:252` | False |
| `futon3c.agents.tickle-queue/format-queue` | `futon3c/src/futon3c/agents/tickle_queue.clj:275` | False |
| `futon3c.agents.tickle-queue/idle-unassigned` | `futon3c/src/futon3c/agents/tickle_queue.clj:259` | False |
| `futon3c.agents.tickle-queue/peek-pending` | `futon3c/src/futon3c/agents/tickle_queue.clj:185` | False |
| `futon3c.agents.tickle-queue/pending-count` | `futon3c/src/futon3c/agents/tickle_queue.clj:212` | False |
| `futon3c.agents.tickle-queue/pick-task!` | `futon3c/src/futon3c/agents/tickle_queue.clj:88` | False |
| `futon3c.agents.tickle-queue/pop-pending!` | `futon3c/src/futon3c/agents/tickle_queue.clj:190` | False |
| `futon3c.agents.tickle-queue/remove-task!` | `futon3c/src/futon3c/agents/tickle_queue.clj:56` | False |
| `futon3c.agents.tickle-queue/snapshot` | `futon3c/src/futon3c/agents/tickle_queue.clj:216` | False |
| `futon3c.agents.tickle-queue/task-count` | `futon3c/src/futon3c/agents/tickle_queue.clj:214` | False |
| `futon3c.agents.tickle-work-queue/emit-ct-evidence!` | `futon3c/src/futon3c/agents/tickle_work_queue.clj:258` | False |
| `futon3c.agents.tickle-work-queue/entities-by-complexity` | `futon3c/src/futon3c/agents/tickle_work_queue.clj:301` | False |
| `futon3c.agents.tickle-work-queue/golden-entity-ids` | `futon3c/src/futon3c/agents/tickle_work_queue.clj:316` | False |
| `futon3c.agents.tickle-work-queue/load-golden-expected` | `futon3c/src/futon3c/agents/tickle_work_queue.clj:332` | False |
| `futon3c.agents.tickle-work-queue/make-review-prompt` | `futon3c/src/futon3c/agents/tickle_work_queue.clj:145` | False |
| `futon3c.agents.tickle-work-queue/next-unprocessed` | `futon3c/src/futon3c/agents/tickle_work_queue.clj:235` | False |
| `futon3c.agents.tickle-work-queue/queue-status` | `futon3c/src/futon3c/agents/tickle_work_queue.clj:218` | False |
| `futon3c.agents.zai-api/transcript-persistence-status` | `futon3c/src/futon3c/agents/zai_api.clj:889` | False |
| `futon3c.agents.zaif-actand/append-refused-prediction-tension` | `futon3c/src/futon3c/agents/zaif_actand.clj:202` | False |
| `futon3c.agents.zaif-actand/calibration-density-with-kin` | `futon3c/src/futon3c/agents/zaif_actand.clj:168` | False |
| `futon3c.agents.zaif-actand/read-calibration-sessions` | `futon3c/src/futon3c/agents/zaif_actand.clj:271` | False |
| `futon3c.agents.zaif-arm-adapters/arm-b-mapping` | `futon3c/src/futon3c/agents/zaif_arm_adapters.clj:83` | False |
| `futon3c.agents.zaif-arm-comparison/comparison-report` | `futon3c/src/futon3c/agents/zaif_arm_comparison.clj:188` | False |
| `futon3c.agents.zaif-controller/persistence-status` | `futon3c/src/futon3c/agents/zaif_controller.clj:296` | False |
| `futon3c.agents.zaif-controller/receipt-learning-summary` | `futon3c/src/futon3c/agents/zaif_controller.clj:181` | False |
| `futon3c.agents.zaif-inputs/make-hydrator` | `futon3c/src/futon3c/agents/zaif_inputs.clj:254` | False |
| `futon3c.agents.zaif-inputs/reset-gamma-cache!` | `futon3c/src/futon3c/agents/zaif_inputs.clj:121` | False |
| `futon3c.aif.calibration/-main` | `futon3c/src/futon3c/aif/calibration.clj:392` | False |
| `futon3c.aif.calibration/flight-stratification` | `futon3c/src/futon3c/aif/calibration.clj:315` | False |
| `futon3c.aif.chipwitz/autopen-rate` | `futon3c/src/futon3c/aif/chipwitz.clj:135` | False |
| `futon3c.aif.chipwitz/chipwitz-gate` | `futon3c/src/futon3c/aif/chipwitz.clj:193` | False |
| `futon3c.aif.chipwitz/threshold-adjustment` | `futon3c/src/futon3c/aif/chipwitz.clj:167` | False |
| `futon3c.aif.discipline-events/append-event!` | `futon3c/src/futon3c/aif/discipline_events.clj:36` | False |
| `futon3c.aif.emacs-bridge/open-target` | `futon3c/src/futon3c/aif/emacs_bridge.clj:171` | False |
| `futon3c.aif.flight-record/backfill-record` | `futon3c/src/futon3c/aif/flight_record.clj:268` | False |
| `futon3c.aif.flight-record/compose-flight-record` | `futon3c/src/futon3c/aif/flight_record.clj:71` | False |
| `futon3c.aif.flight-record/spec-version` | `futon3c/src/futon3c/aif/flight_record.clj:24` | False |
| `futon3c.aif.flight-record/write-flight-record!` | `futon3c/src/futon3c/aif/flight_record.clj:347` | False |
| `futon3c.aif.invariant/check-aif-head-law` | `futon3c/src/futon3c/aif/invariant.clj:69` | False |
| `futon3c.aif.invariant/get-aif-head` | `futon3c/src/futon3c/aif/invariant.clj:38` | False |
| `futon3c.aif.invariant/register-aif-head!` | `futon3c/src/futon3c/aif/invariant.clj:26` | False |
| `futon3c.aif.invariant/unregister-aif-head!` | `futon3c/src/futon3c/aif/invariant.clj:33` | False |
| `futon3c.aif.loop-learning/loop-learning-pass` | `futon3c/src/futon3c/aif/loop_learning.clj:102` | False |
| `futon3c.aif.mission-delta-t/delta-t-mission` | `futon3c/src/futon3c/aif/mission_delta_t.clj:303` | False |
| `futon3c.aif.mission-delta-t/reset-type-cache!` | `futon3c/src/futon3c/aif/mission_delta_t.clj:145` | False |
| `futon3c.aif.mission-head/MissionAifHead` | `futon3c/src/futon3c/aif/mission_head.clj:259` | False |
| `futon3c.aif.mission-head/map->MissionAifHead` | `futon3c/src/futon3c/aif/mission_head.clj:259` | False |
| `futon3c.aif.mission-head/mission-select-pattern` | `futon3c/src/futon3c/aif/mission_head.clj:310` | False |
| `futon3c.aif.mission-head/mission-update-beliefs` | `futon3c/src/futon3c/aif/mission_head.clj:319` | False |
| `futon3c.aif.observe/obs->vector` | `futon3c/src/futon3c/aif/observe.clj:183` | False |
| `futon3c.aif.repl-trace/add-learning!` | `futon3c/src/futon3c/aif/repl_trace.clj:149` | False |
| `futon3c.aif.repl-trace/gamma` | `futon3c/src/futon3c/aif/repl_trace.clj:159` | False |
| `futon3c.aif.repl-trace/now-iso` | `futon3c/src/futon3c/aif/repl_trace.clj:107` | False |
| `futon3c.aif.repl-trace/read-frame` | `futon3c/src/futon3c/aif/repl_trace.clj:178` | False |
| `futon3c.aif.stack-generator/generate-live` | `futon3c/src/futon3c/aif/stack_generator.clj:653` | False |
| `futon3c.analysis.memory-arm-e1/-main` | `futon3c/src/futon3c/analysis/memory_arm_e1.clj:167` | False |
| `futon3c.apm.analyst-campaign/append-series-input!` | `futon3c/src/futon3c/apm/analyst_campaign.clj:134` | False |
| `futon3c.apm.authority-port/resolve-revision` | `futon3c/src/futon3c/apm/authority_port.clj:57` | False |
| `futon3c.apm.bank-sweep/sweep-to-master!` | `futon3c/src/futon3c/apm/bank_sweep.clj:119` | False |
| `futon3c.apm.campaign-batch/issue` | `futon3c/src/futon3c/apm/campaign_batch.clj:7` | False |
| `futon3c.apm.campaign-machine/terminal-frame-statuses` | `futon3c/src/futon3c/apm/campaign_machine.clj:22` | False |
| `futon3c.apm.campaign-qualification/read-plan` | `futon3c/src/futon3c/apm/campaign_qualification.clj:117` | False |
| `futon3c.apm.campaign-reconcile/terminal-job-states` | `futon3c/src/futon3c/apm/campaign_reconcile.clj:12` | False |
| `futon3c.apm.campaign-runner/run-batch!` | `futon3c/src/futon3c/apm/campaign_runner.clj:158` | False |
| `futon3c.apm.campaign-supervisor/tick!` | `futon3c/src/futon3c/apm/campaign_supervisor.clj:13` | False |
| `futon3c.apm.campaign-trace/emit!` | `futon3c/src/futon3c/apm/campaign_trace.clj:466` | False |
| `futon3c.apm.campaign-trace/from-durable-state` | `futon3c/src/futon3c/apm/campaign_trace.clj:411` | False |
| `futon3c.apm.campaign-trace/review-passes-from-live` | `futon3c/src/futon3c/apm/campaign_trace.clj:299` | False |
| `futon3c.apm.cascade-dry-run/-main` | `futon3c/src/futon3c/apm/cascade_dry_run.clj:75` | False |
| `futon3c.apm.checked-handoff/grade-receipt` | `futon3c/src/futon3c/apm/checked_handoff.clj:85` | False |
| `futon3c.apm.coined-pattern/publish-file!` | `futon3c/src/futon3c/apm/coined_pattern.clj:83` | False |
| `futon3c.apm.conductor-binding/reset-bindings!` | `futon3c/src/futon3c/apm/conductor_binding.clj:239` | False |
| `futon3c.apm.countdown-control/autonomous-problem-list-step!` | `futon3c/src/futon3c/apm/countdown_control.clj:2505` | False |
| `futon3c.apm.countdown-control/cancel-regulator-scheduler!` | `futon3c/src/futon3c/apm/countdown_control.clj:1860` | False |
| `futon3c.apm.countdown-control/dry-run-v2-launch` | `futon3c/src/futon3c/apm/countdown_control.clj:1511` | False |
| `futon3c.apm.countdown-control/f20-one-off-config` | `futon3c/src/futon3c/apm/countdown_control.clj:86` | False |
| `futon3c.apm.countdown-control/f21-one-off-config` | `futon3c/src/futon3c/apm/countdown_control.clj:98` | False |
| `futon3c.apm.countdown-control/f22-one-off-config` | `futon3c/src/futon3c/apm/countdown_control.clj:112` | False |
| `futon3c.apm.countdown-control/finalize-solver-progress-retry!` | `futon3c/src/futon3c/apm/countdown_control.clj:2167` | False |
| `futon3c.apm.countdown-control/launch-all-open-nontopology-autonomous!` | `futon3c/src/futon3c/apm/countdown_control.clj:2587` | False |
| `futon3c.apm.countdown-control/launch-m-five!` | `futon3c/src/futon3c/apm/countdown_control.clj:2544` | False |
| `futon3c.apm.countdown-control/launch-m-five-v2!` | `futon3c/src/futon3c/apm/countdown_control.clj:2557` | False |
| `futon3c.apm.countdown-control/launch-m-five-v2-autonomous!` | `futon3c/src/futon3c/apm/countdown_control.clj:2575` | False |
| `futon3c.apm.countdown-control/regulator-status` | `futon3c/src/futon3c/apm/countdown_control.clj:1854` | False |
| `futon3c.apm.countdown-control/run-live-preflight!` | `futon3c/src/futon3c/apm/countdown_control.clj:715` | False |
| `futon3c.apm.countdown-control/set-alight-batch!` | `futon3c/src/futon3c/apm/countdown_control.clj:1896` | False |
| `futon3c.apm.countdown-control/start-regulator!` | `futon3c/src/futon3c/apm/countdown_control.clj:1867` | False |
| `futon3c.apm.csquare-synthetic-campaign/result` | `futon3c/src/futon3c/apm/csquare_synthetic_campaign.clj:250` | False |
| `futon3c.apm.csquare-synthetic-campaign/start!` | `futon3c/src/futon3c/apm/csquare_synthetic_campaign.clj:234` | False |
| `futon3c.apm.cycle-harness/memory-store` | `futon3c/src/futon3c/apm/cycle_harness.clj:18` | False |
| `futon3c.apm.cycle-harness/run-cycle!` | `futon3c/src/futon3c/apm/cycle_harness.clj:152` | False |
| `futon3c.apm.durable-coordinator/cancel-scheduler!` | `futon3c/src/futon3c/apm/durable_coordinator.clj:970` | False |
| `futon3c.apm.durable-coordinator/retry!` | `futon3c/src/futon3c/apm/durable_coordinator.clj:335` | False |
| `futon3c.apm.durable-coordinator/transition-stop-cause` | `futon3c/src/futon3c/apm/durable_coordinator.clj:104` | False |
| `futon3c.apm.frame-cycle-contract/read-contract` | `futon3c/src/futon3c/apm/frame_cycle_contract.clj:7` | False |
| `futon3c.apm.frame-cycle-handlers/make-handlers` | `futon3c/src/futon3c/apm/frame_cycle_handlers.clj:254` | False |
| `futon3c.apm.frame-park-decisions/-main` | `futon3c/src/futon3c/apm/frame_park_decisions.clj:30` | False |
| `futon3c.apm.frame18-control/-main` | `futon3c/src/futon3c/apm/frame18_control.clj:397` | False |
| `futon3c.apm.ftriangle-live-smoke/arm-isolated-coordinator!` | `futon3c/src/futon3c/apm/ftriangle_live_smoke.clj:129` | False |
| `futon3c.apm.ftriangle-live-smoke/run-live!` | `futon3c/src/futon3c/apm/ftriangle_live_smoke.clj:524` | False |
| `futon3c.apm.jit-queue-coordinator/recover!` | `futon3c/src/futon3c/apm/jit_queue_coordinator.clj:182` | False |
| `futon3c.apm.jit-queue-coordinator/release-store-read-hold!` | `futon3c/src/futon3c/apm/jit_queue_coordinator.clj:193` | False |
| `futon3c.apm.jit-queue-coordinator/status` | `futon3c/src/futon3c/apm/jit_queue_coordinator.clj:189` | False |
| `futon3c.apm.jit-queue-coordinator/stop!` | `futon3c/src/futon3c/apm/jit_queue_coordinator.clj:186` | False |
| `futon3c.apm.job-port/active-states` | `futon3c/src/futon3c/apm/job_port.clj:6` | False |
| `futon3c.apm.job-port/settling-states` | `futon3c/src/futon3c/apm/job_port.clj:7` | False |
| `futon3c.apm.job-port/terminal-states` | `futon3c/src/futon3c/apm/job_port.clj:8` | False |
| `futon3c.apm.learning-loop-dry-run/dry-run!` | `futon3c/src/futon3c/apm/learning_loop_dry_run.clj:11` | False |
| `futon3c.apm.library-lane/lanes` | `futon3c/src/futon3c/apm/library_lane.clj:93` | False |
| `futon3c.apm.library-lane-adapters/codex-seat-types` | `futon3c/src/futon3c/apm/library_lane_adapters.clj:18` | False |
| `futon3c.apm.library-lane-adapters/codex-workspace-roles` | `futon3c/src/futon3c/apm/library_lane_adapters.clj:17` | False |
| `futon3c.apm.library-lane-coordinator/hydrate-control-authority!` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:169` | False |
| `futon3c.apm.library-lane-coordinator/migrate-pending-phase-intent!` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:318` | False |
| `futon3c.apm.library-lane-coordinator/migrate-solver-assignment!` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:214` | False |
| `futon3c.apm.library-lane-coordinator/resume!` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:367` | False |
| `futon3c.apm.library-lane-coordinator/retire-superseded-preflight-intent!` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:255` | False |
| `futon3c.apm.library-lane-coordinator/start!` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:141` | False |
| `futon3c.apm.library-lane-coordinator/status` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:163` | False |
| `futon3c.apm.library-lane-coordinator/stop!` | `futon3c/src/futon3c/apm/library_lane_coordinator.clj:166` | False |
| `futon3c.apm.library-lane-queue/run-queue!` | `futon3c/src/futon3c/apm/library_lane_queue.clj:29` | False |
| `futon3c.apm.library-lane-smoke/result` | `futon3c/src/futon3c/apm/library_lane_smoke.clj:92` | False |
| `futon3c.apm.library-lane-smoke/start!` | `futon3c/src/futon3c/apm/library_lane_smoke.clj:72` | False |
| `futon3c.apm.library-loop-adapter/deps` | `futon3c/src/futon3c/apm/library_loop_adapter.clj:322` | False |
| `futon3c.apm.library-loop-exec/-main` | `futon3c/src/futon3c/apm/library_loop_exec.clj:445` | False |
| `futon3c.apm.library-loop-tools/-main` | `futon3c/src/futon3c/apm/library_loop_tools.clj:321` | False |
| `futon3c.apm.live-job-driver/collection-dispositions` | `futon3c/src/futon3c/apm/live_job_driver.clj:776` | False |
| `futon3c.apm.live-job-driver/collection-process-outcomes` | `futon3c/src/futon3c/apm/live_job_driver.clj:732` | False |
| `futon3c.apm.live-job-driver/collection-submission-outcomes` | `futon3c/src/futon3c/apm/live_job_driver.clj:736` | False |
| `futon3c.apm.live-job-driver/reopen-posthoc-rejection` | `futon3c/src/futon3c/apm/live_job_driver.clj:205` | False |
| `futon3c.apm.live-job-driver/scan-durable-references` | `futon3c/src/futon3c/apm/live_job_driver.clj:581` | False |
| `futon3c.apm.live-job-driver/wall-clock-budget-exhausted?` | `futon3c/src/futon3c/apm/live_job_driver.clj:396` | False |
| `futon3c.apm.live-preflight-runtime/parse-report` | `futon3c/src/futon3c/apm/live_preflight_runtime.clj:151` | False |
| `futon3c.apm.live-promotion/authorize-review-recovery!` | `futon3c/src/futon3c/apm/live_promotion.clj:202` | False |
| `futon3c.apm.live-proof-phases/resume-solver-remediation-live!` | `futon3c/src/futon3c/apm/live_proof_phases.clj:381` | False |
| `futon3c.apm.live-regulator/stop!` | `futon3c/src/futon3c/apm/live_regulator.clj:185` | False |
| `futon3c.apm.live-solver-rounds/repair-checkpoint!` | `futon3c/src/futon3c/apm/live_solver_rounds.clj:568` | False |
| `futon3c.apm.memory-caption-store/schema` | `futon3c/src/futon3c/apm/memory_caption_store.clj:22` | False |
| `futon3c.apm.pattern-revision-review/collect!` | `futon3c/src/futon3c/apm/pattern_revision_review.clj:202` | False |
| `futon3c.apm.pattern-revision-review/dispatch!` | `futon3c/src/futon3c/apm/pattern_revision_review.clj:169` | False |
| `futon3c.apm.pattern-revision-review/prepare!` | `futon3c/src/futon3c/apm/pattern_revision_review.clj:72` | False |
| `futon3c.apm.phase-status/closure-findings` | `futon3c/src/futon3c/apm/phase_status.clj:142` | False |
| `futon3c.apm.problem-queue-supervisor/decommission` | `futon3c/src/futon3c/apm/problem_queue_supervisor.clj:187` | False |
| `futon3c.apm.problem-queue-supervisor/pause-after-active` | `futon3c/src/futon3c/apm/problem_queue_supervisor.clj:475` | False |
| `futon3c.apm.problem-queue-supervisor/resume-parked-frames` | `futon3c/src/futon3c/apm/problem_queue_supervisor.clj:217` | False |
| `futon3c.apm.problem-queue-supervisor/resume-paused` | `futon3c/src/futon3c/apm/problem_queue_supervisor.clj:487` | False |
| `futon3c.apm.projection-watchdog/-main` | `futon3c/src/futon3c/apm/projection_watchdog.clj:344` | False |
| `futon3c.apm.promotion-pipeline/validate-publication-accounting` | `futon3c/src/futon3c/apm/promotion_pipeline.clj:399` | False |
| `futon3c.apm.promotion-pipeline/validate-review` | `futon3c/src/futon3c/apm/promotion_pipeline.clj:356` | False |
| `futon3c.apm.qualification/run-qualification!` | `futon3c/src/futon3c/apm/qualification.clj:113` | False |
| `futon3c.apm.queued-frame-adapter/void-exhausted-role-terminal!` | `futon3c/src/futon3c/apm/queued_frame_adapter.clj:313` | False |
| `futon3c.apm.role-memory-search/enforce-holdout` | `futon3c/src/futon3c/apm/role_memory_search.clj:46` | False |
| `futon3c.apm.role-memory-search/recorded-result-ids-for-job` | `futon3c/src/futon3c/apm/role_memory_search.clj:167` | False |
| `futon3c.apm.role-memory-search/recorded-surfaced-ids-for-job` | `futon3c/src/futon3c/apm/role_memory_search.clj:243` | False |
| `futon3c.apm.role-memory-search/validate-claims` | `futon3c/src/futon3c/apm/role_memory_search.clj:296` | False |
| `futon3c.apm.role-memory-search/validate-pattern-accounting` | `futon3c/src/futon3c/apm/role_memory_search.clj:328` | False |
| `futon3c.apm.toolchain-port/elaborate` | `futon3c/src/futon3c/apm/toolchain_port.clj:55` | False |
| `futon3c.apm.transport-conformance/adapt-legacy-finding` | `futon3c/src/futon3c/apm/transport_conformance.clj:330` | False |
| `futon3c.apm.transport-conformance/authoritative-absence?` | `futon3c/src/futon3c/apm/transport_conformance.clj:56` | False |
| `futon3c.apm.transport-conformance/conformant?` | `futon3c/src/futon3c/apm/transport_conformance.clj:261` | False |
| `futon3c.apm.transport-conformance/f63-historical-certificate` | `futon3c/src/futon3c/apm/transport_conformance.clj:295` | False |
| `futon3c.apm.transport-conformance/f64-successful-visibility` | `futon3c/src/futon3c/apm/transport_conformance.clj:317` | False |
| `futon3c.apm.transport-conformance/f64-transport-failure` | `futon3c/src/futon3c/apm/transport_conformance.clj:306` | False |
| `futon3c.apm.transport-conformance/replay-directory` | `futon3c/src/futon3c/apm/transport_conformance.clj:489` | False |
| `futon3c.apm.transport-conformance/retry-then-success-findings` | `futon3c/src/futon3c/apm/transport_conformance.clj:264` | False |
| `futon3c.apm.transport-conformance/schedule-retry` | `futon3c/src/futon3c/apm/transport_conformance.clj:85` | False |
| `futon3c.apm.transport-conformance/transport-failure?` | `futon3c/src/futon3c/apm/transport_conformance.clj:51` | False |
| `futon3c.apm.typed-role-submission/evidence-required-by-phase` | `futon3c/src/futon3c/apm/typed_role_submission.clj:120` | False |
| `futon3c.apm.typed-role-submission/new-token` | `futon3c/src/futon3c/apm/typed_role_submission.clj:216` | False |
| `futon3c.apm.typed-role-submission/validator-evidence-fields-by-phase` | `futon3c/src/futon3c/apm/typed_role_submission.clj:152` | False |
| `futon3c.apm.typed-role-submission/validator-schema-findings` | `futon3c/src/futon3c/apm/typed_role_submission.clj:195` | False |
| `futon3c.blackboard/blackboard-eval!` | `futon3c/src/futon3c/blackboard.clj:352` | False |
| `futon3c.blackboard/project-processes!` | `futon3c/src/futon3c/blackboard.clj:1468` | False |
| `futon3c.blackboard/set-agents-window-display!` | `futon3c/src/futon3c/blackboard.clj:124` | False |
| `futon3c.blackboard/set-external-hud-enabled!` | `futon3c/src/futon3c/blackboard.clj:135` | False |
| `futon3c.bridge/record-triangle!` | `futon3c/src/futon3c/bridge.clj:153` | False |
| `futon3c.clock.turn-trigger/add-rider!` | `futon3c/src/futon3c/clock/turn_trigger.clj:48` | False |
| `futon3c.clock.turn-trigger/belly-refresh-rider` | `futon3c/src/futon3c/clock/turn_trigger.clj:105` | False |
| `futon3c.clock.turn-trigger/remove-rider!` | `futon3c/src/futon3c/clock/turn_trigger.clj:49` | False |
| `futon3c.clock.turn-trigger/start!` | `futon3c/src/futon3c/clock/turn_trigger.clj:117` | False |
| `futon3c.clock.turn-trigger/stop!` | `futon3c/src/futon3c/clock/turn_trigger.clj:142` | False |
| `futon3c.cyder/derivation-phases` | `futon3c/src/futon3c/cyder.clj:207` | True |
| `futon3c.cyder/register-missions!` | `futon3c/src/futon3c/cyder.clj:253` | False |
| `futon3c.cyder/stop-all!` | `futon3c/src/futon3c/cyder.clj:164` | False |
| `futon3c.diagramprover.causal.admg/children` | `futon3c/src/futon3c/diagramprover/causal/admg.clj:67` | False |
| `futon3c.diagramprover.causal.bow/all-bow-receipts` | `futon3c/src/futon3c/diagramprover/causal/bow.clj:141` | False |
| `futon3c.diagramprover.causal.cohort-guard/guard!` | `futon3c/src/futon3c/diagramprover/causal/cohort_guard.clj:68` | False |
| `futon3c.diagramprover.causal.diagram/canonical?` | `futon3c/src/futon3c/diagramprover/causal/diagram.clj:118` | False |
| `futon3c.diagramprover.causal.dsep/implied-independencies` | `futon3c/src/futon3c/diagramprover/causal/dsep.clj:119` | False |
| `futon3c.diagramprover.causal.guard/guard!` | `futon3c/src/futon3c/diagramprover/causal/guard.clj:49` | False |
| `futon3c.diagramprover.causal.receipts/all-receipts` | `futon3c/src/futon3c/diagramprover/causal/receipts.clj:439` | False |
| `futon3c.diagramprover.causal.surgery/with-leaks` | `futon3c/src/futon3c/diagramprover/causal/surgery.clj:29` | False |
| `futon3c.diagramprover.causal.surgery/without-leaks` | `futon3c/src/futon3c/diagramprover/causal/surgery.clj:30` | False |
| `futon3c.diagramprover.graph/add-inputs` | `futon3c/src/futon3c/diagramprover/graph.clj:60` | False |
| `futon3c.diagramprover.graph/add-outputs` | `futon3c/src/futon3c/diagramprover/graph.clj:63` | False |
| `futon3c.diagramprover.graph/compose` | `futon3c/src/futon3c/diagramprover/graph.clj:252` | False |
| `futon3c.diagramprover.graph/generator` | `futon3c/src/futon3c/diagramprover/graph.clj:240` | False |
| `futon3c.diagramprover.matcher/find-iso` | `futon3c/src/futon3c/diagramprover/matcher.clj:163` | False |
| `futon3c.diagramprover.rewrite/rule-applications` | `futon3c/src/futon3c/diagramprover/rewrite.clj:120` | False |
| `futon3c.diagramprover.rmdiagram/canonical?` | `futon3c/src/futon3c/diagramprover/rmdiagram.clj:81` | False |
| `futon3c.diagramprover.rmdiagram/dag->rmdiagram` | `futon3c/src/futon3c/diagramprover/rmdiagram.clj:15` | False |
| `futon3c.diagramprover.rmdiagram/rmdiagram->dag` | `futon3c/src/futon3c/diagramprover/rmdiagram.clj:60` | False |
| `futon3c.diagramprover.rmgraph/in-edges` | `futon3c/src/futon3c/diagramprover/rmgraph.clj:18` | False |
| `futon3c.diagramprover.rmgraph/out-edges` | `futon3c/src/futon3c/diagramprover/rmgraph.clj:19` | False |
| `futon3c.diagramprover.rule/converse` | `futon3c/src/futon3c/diagramprover/rule.clj:20` | False |
| `futon3c.diagramprover.rule/left-linear?` | `futon3c/src/futon3c/diagramprover/rule.clj:16` | False |
| `futon3c.diagramprover.rule/make-rule` | `futon3c/src/futon3c/diagramprover/rule.clj:5` | False |
| `futon3c.diagramprover.wiring/conformance` | `futon3c/src/futon3c/diagramprover/wiring.clj:107` | False |
| `futon3c.diagramprover.wiring/ingest` | `futon3c/src/futon3c/diagramprover/wiring.clj:13` | False |
| `futon3c.diagramprover.wiring/multiply-written` | `futon3c/src/futon3c/diagramprover/wiring.clj:72` | False |
| `futon3c.diagramprover.wiring/phase-chain-findings` | `futon3c/src/futon3c/diagramprover/wiring.clj:161` | False |
| `futon3c.diagramprover.wiring/read-never-written` | `futon3c/src/futon3c/diagramprover/wiring.clj:54` | False |
| `futon3c.diagramprover.wiring/written-never-read` | `futon3c/src/futon3c/diagramprover/wiring.clj:36` | False |
| `futon3c.dispatch-with-recall/-main` | `futon3c/src/futon3c/dispatch_with_recall.clj:1581` | False |
| `futon3c.dispatch-with-recall/proposal-hit?` | `futon3c/src/futon3c/dispatch_with_recall.clj:672` | False |
| `futon3c.evidence.backend/AtomBackend` | `futon3c/src/futon3c/evidence/backend.clj:113` | False |
| `futon3c.evidence.backend/map->AtomBackend` | `futon3c/src/futon3c/evidence/backend.clj:113` | False |
| `futon3c.evidence.boundary/append-default!` | `futon3c/src/futon3c/evidence/boundary.clj:438` | False |
| `futon3c.evidence.futon1b-backend/Futon1bBackend` | `futon3c/src/futon3c/evidence/futon1b_backend.clj:441` | False |
| `futon3c.evidence.futon1b-backend/map->Futon1bBackend` | `futon3c/src/futon3c/evidence/futon1b_backend.clj:441` | False |
| `futon3c.evidence.http-backend/HttpBackend` | `futon3c/src/futon3c/evidence/http_backend.clj:60` | False |
| `futon3c.evidence.http-backend/map->HttpBackend` | `futon3c/src/futon3c/evidence/http_backend.clj:60` | False |
| `futon3c.evidence.invariant/check-store-backing` | `futon3c/src/futon3c/evidence/invariant.clj:42` | False |
| `futon3c.evidence.store/append!` | `futon3c/src/futon3c/evidence/store.clj:113` | False |
| `futon3c.evidence.store/compact-ephemeral!` | `futon3c/src/futon3c/evidence/store.clj:206` | False |
| `futon3c.evidence.store/get-entry` | `futon3c/src/futon3c/evidence/store.clj:72` | False |
| `futon3c.evidence.store/get-forks` | `futon3c/src/futon3c/evidence/store.clj:171` | False |
| `futon3c.evidence.store/get-reply-chain` | `futon3c/src/futon3c/evidence/store.clj:160` | False |
| `futon3c.evidence.store/recent-activity` | `futon3c/src/futon3c/evidence/store.clj:176` | False |
| `futon3c.evidence.store/reset-store!` | `futon3c/src/futon3c/evidence/store.clj:29` | False |
| `futon3c.evidence.threads/project-thread` | `futon3c/src/futon3c/evidence/threads.clj:44` | False |
| `futon3c.evidence.threads/thread-conjectures` | `futon3c/src/futon3c/evidence/threads.clj:111` | False |
| `futon3c.evidence.threads/thread-forks` | `futon3c/src/futon3c/evidence/threads.clj:72` | False |
| `futon3c.evidence.threads/thread-patterns` | `futon3c/src/futon3c/evidence/threads.clj:133` | False |
| `futon3c.flight.pretty-print/flight-path-for-run-id` | `futon3c/src/futon3c/flight/pretty_print.clj:131` | False |
| `futon3c.flight.pretty-print/latest-flight-path` | `futon3c/src/futon3c/flight/pretty_print.clj:138` | False |
| `futon3c.flight.pretty-print/render-file` | `futon3c/src/futon3c/flight/pretty_print.clj:126` | False |
| `futon3c.inbox-zero.attribution/attribute-state` | `futon3c/src/futon3c/inbox_zero/attribution.clj:149` | False |
| `futon3c.inbox-zero.batch-dispatch/execute-batch!` | `futon3c/src/futon3c/inbox_zero/batch_dispatch.clj:213` | False |
| `futon3c.inbox-zero.board-consumer/-main` | `futon3c/src/futon3c/inbox_zero/board_consumer.clj:266` | False |
| `futon3c.inbox-zero.confirm-intake/confirm-attribution!` | `futon3c/src/futon3c/inbox_zero/confirm_intake.clj:41` | False |
| `futon3c.inbox-zero.followup-validity/still-current?` | `futon3c/src/futon3c/inbox_zero/followup_validity.clj:73` | False |
| `futon3c.inbox-zero.sweeper/start-loop!` | `futon3c/src/futon3c/inbox_zero/sweeper.clj:770` | False |
| `futon3c.inbox-zero.sweeper/stop-loop!` | `futon3c/src/futon3c/inbox_zero/sweeper.clj:761` | False |
| `futon3c.inbox-zero.turn-promotion/launch-at-turn-end!` | `futon3c/src/futon3c/inbox_zero/turn_promotion.clj:389` | False |
| `futon3c.inbox-zero.turn-promotion/set-mode!` | `futon3c/src/futon3c/inbox_zero/turn_promotion.clj:237` | False |
| `futon3c.inbox-zero.witness/publish-successful-edit!` | `futon3c/src/futon3c/inbox_zero/witness.clj:117` | False |
| `futon3c.live-efe-map/build-response` | `futon3c/src/futon3c/live_efe_map.clj:385` | False |
| `futon3c.logic.aif2-invariants/installed-anyo` | `futon3c/src/futon3c/logic/aif2_invariants.clj:84` | True |
| `futon3c.logic.aif2-invariants/run-verify` | `futon3c/src/futon3c/logic/aif2_invariants.clj:174` | False |
| `futon3c.logic.archaeology/check-autostash-on-load!` | `futon3c/src/futon3c/logic/archaeology.clj:385` | False |
| `futon3c.logic.archaeology/check-branch-disposition-on-load!` | `futon3c/src/futon3c/logic/archaeology.clj:740` | False |
| `futon3c.logic.archaeology/check-deferred-stub-on-load!` | `futon3c/src/futon3c/logic/archaeology.clj:400` | False |
| `futon3c.logic.archaeology/check-mission-doc-disposition-on-load!` | `futon3c/src/futon3c/logic/archaeology.clj:920` | False |
| `futon3c.logic.archaeology/check-pipeline-tracer-on-load!` | `futon3c/src/futon3c/logic/archaeology.clj:415` | False |
| `futon3c.logic.archaeology/check-stash-disposition-on-load!` | `futon3c/src/futon3c/logic/archaeology.clj:577` | False |
| `futon3c.logic.archaeology/register-archaeology-control-taps!` | `futon3c/src/futon3c/logic/archaeology.clj:957` | False |
| `futon3c.logic.arxana-bridge/emit-aggregate!` | `futon3c/src/futon3c/logic/arxana_bridge.clj:215` | False |
| `futon3c.logic.arxana-bridge/reconcile-aggregate!` | `futon3c/src/futon3c/logic/arxana_bridge.clj:221` | False |
| `futon3c.logic.business-coupling-invariants/run-verify` | `futon3c/src/futon3c/logic/business_coupling_invariants.clj:218` | False |
| `futon3c.logic.capability-star-map-extractor/extract-write-and-verify!` | `futon3c/src/futon3c/logic/capability_star_map_extractor.clj:466` | False |
| `futon3c.logic.capability-star-map-invariants/run-verify` | `futon3c/src/futon3c/logic/capability_star_map_invariants.clj:187` | False |
| `futon3c.logic.cascade-real/contract-db` | `futon3c/src/futon3c/logic/cascade_real.clj:141` | False |
| `futon3c.logic.cascade-real-live/cascade-real-graph` | `futon3c/src/futon3c/logic/cascade_real_live.clj:552` | False |
| `futon3c.logic.cascade-real-live/cascade-real-summary` | `futon3c/src/futon3c/logic/cascade_real_live.clj:218` | False |
| `futon3c.logic.cascade-real-live/verify-live` | `futon3c/src/futon3c/logic/cascade_real_live.clj:176` | False |
| `futon3c.logic.disposition-derive/combined-state` | `futon3c/src/futon3c/logic/disposition_derive.clj:165` | False |
| `futon3c.logic.disposition-derive/derive-from-active-missions` | `futon3c/src/futon3c/logic/disposition_derive.clj:115` | False |
| `futon3c.logic.disposition-edn/summary` | `futon3c/src/futon3c/logic/disposition_edn.clj:155` | False |
| `futon3c.logic.invariant-queue-freshness/check` | `futon3c/src/futon3c/logic/invariant_queue_freshness.clj:73` | False |
| `futon3c.logic.invariant-runner/render-aggregate` | `futon3c/src/futon3c/logic/invariant_runner.clj:125` | False |
| `futon3c.logic.invariant-runner/render-report` | `futon3c/src/futon3c/logic/invariant_runner.clj:82` | False |
| `futon3c.logic.invariant-runner/run-aggregate` | `futon3c/src/futon3c/logic/invariant_runner.clj:86` | False |
| `futon3c.logic.inventory/check-compliance` | `futon3c/src/futon3c/logic/inventory.clj:307` | False |
| `futon3c.logic.inventory/laws-by-family` | `futon3c/src/futon3c/logic/inventory.clj:292` | False |
| `futon3c.logic.inventory/laws-by-status` | `futon3c/src/futon3c/logic/inventory.clj:283` | False |
| `futon3c.logic.inventory/non-operational-statuses` | `futon3c/src/futon3c/logic/inventory.clj:304` | True |
| `futon3c.logic.locus/check-agent-routing-locus-on-load!` | `futon3c/src/futon3c/logic/locus.clj:595` | False |
| `futon3c.logic.locus/check-artifact-live-copy-locus-on-load!` | `futon3c/src/futon3c/logic/locus.clj:611` | False |
| `futon3c.logic.locus/check-mission-home-locus-on-load!` | `futon3c/src/futon3c/logic/locus.clj:580` | False |
| `futon3c.logic.locus/register-locus-taps!` | `futon3c/src/futon3c/logic/locus.clj:632` | False |
| `futon3c.logic.mana-session/balance-for-session` | `futon3c/src/futon3c/logic/mana_session.clj:91` | False |
| `futon3c.logic.mana-session/reachable?` | `futon3c/src/futon3c/logic/mana_session.clj:82` | False |
| `futon3c.logic.metabolic-balance/check-working-tree-pressure-on-load!` | `futon3c/src/futon3c/logic/metabolic_balance.clj:380` | False |
| `futon3c.logic.metabolic-balance/default-undecided-bound` | `futon3c/src/futon3c/logic/metabolic_balance.clj:93` | False |
| `futon3c.logic.metabolic-balance/register-metabolic-balance-taps!` | `futon3c/src/futon3c/logic/metabolic_balance.clj:410` | False |
| `futon3c.logic.mission-clean/emit-mission-clean!` | `futon3c/src/futon3c/logic/mission_clean.clj:160` | False |
| `futon3c.logic.mission-head-invariants/run-verify` | `futon3c/src/futon3c/logic/mission_head_invariants.clj:196` | False |
| `futon3c.logic.operational-readiness/enabled-but-not-firing-cleanly` | `futon3c/src/futon3c/logic/operational_readiness.clj:58` | False |
| `futon3c.logic.operational-readiness/ready?` | `futon3c/src/futon3c/logic/operational_readiness.clj:51` | False |
| `futon3c.logic.outing-invariants/run-verify` | `futon3c/src/futon3c/logic/outing_invariants.clj:244` | False |
| `futon3c.logic.outing-invariants/verdict` | `futon3c/src/futon3c/logic/outing_invariants.clj:206` | False |
| `futon3c.logic.outreach-intake-guard/run-verify` | `futon3c/src/futon3c/logic/outreach_intake_guard.clj:142` | False |
| `futon3c.logic.probe/backoff-cadence-ms` | `futon3c/src/futon3c/logic/probe.clj:71` | False |
| `futon3c.logic.probe/probe-loop-running?` | `futon3c/src/futon3c/logic/probe.clj:313` | False |
| `futon3c.logic.probe/probe-now!` | `futon3c/src/futon3c/logic/probe.clj:322` | False |
| `futon3c.logic.probe/registered-family-ids` | `futon3c/src/futon3c/logic/probe.clj:115` | False |
| `futon3c.logic.probe/start-probe-loop!` | `futon3c/src/futon3c/logic/probe.clj:268` | False |
| `futon3c.logic.probe/stop-probe-loop!` | `futon3c/src/futon3c/logic/probe.clj:302` | False |
| `futon3c.logic.probe/unregister-family-check!` | `futon3c/src/futon3c/logic/probe.clj:109` | False |
| `futon3c.logic.probe/with-autoshutter-probe` | `futon3c/src/futon3c/logic/probe.clj:333` | False |
| `futon3c.logic.probe-taps/make-live-agency-state-source` | `futon3c/src/futon3c/logic/probe_taps.clj:127` | False |
| `futon3c.logic.probe-taps/register-default-taps!` | `futon3c/src/futon3c/logic/probe_taps.clj:408` | False |
| `futon3c.logic.probe-taps/register-deferred-taps!` | `futon3c/src/futon3c/logic/probe_taps.clj:434` | False |
| `futon3c.logic.ratchet/-main` | `futon3c/src/futon3c/logic/ratchet.clj:304` | False |
| `futon3c.logic.ratchet/check-on-load!` | `futon3c/src/futon3c/logic/ratchet.clj:342` | False |
| `futon3c.logic.ratchet/emit-demotion-event!` | `futon3c/src/futon3c/logic/ratchet.clj:135` | False |
| `futon3c.logic.snapshot/clear-hud-render!` | `futon3c/src/futon3c/logic/snapshot.clj:188` | False |
| `futon3c.logic.snapshot/record-hud-render!` | `futon3c/src/futon3c/logic/snapshot.clj:193` | False |
| `futon3c.logic.snapshot/snapshot-hud-render-on-load!` | `futon3c/src/futon3c/logic/snapshot.clj:341` | False |
| `futon3c.logic.snapshot/snapshot-inventory-on-load!` | `futon3c/src/futon3c/logic/snapshot.clj:289` | False |
| `futon3c.logic.snapshot/snapshot-registry-on-load!` | `futon3c/src/futon3c/logic/snapshot.clj:314` | False |
| `futon3c.logic.snapshot/snapshot-repo-refs-on-load!` | `futon3c/src/futon3c/logic/snapshot.clj:327` | False |
| `futon3c.logic.strategic-closure-specification/check` | `futon3c/src/futon3c/logic/strategic_closure_specification.clj:112` | False |
| `futon3c.logic.substrate-metric-e1-invariants/run-verify` | `futon3c/src/futon3c/logic/substrate_metric_e1_invariants.clj:295` | False |
| `futon3c.logic.tracer/emit-pipeline-tracers!` | `futon3c/src/futon3c/logic/tracer.clj:88` | False |
| `futon3c.logic.tracer/emit-tracer-closed!` | `futon3c/src/futon3c/logic/tracer.clj:101` | False |
| `futon3c.logic.tracer/ensure-default-tracers!` | `futon3c/src/futon3c/logic/tracer.clj:140` | False |
| `futon3c.logic.tracer/pipeline-prototype-path` | `futon3c/src/futon3c/logic/tracer.clj:42` | False |
| `futon3c.logic.typed-bells-invariants/run-verify` | `futon3c/src/futon3c/logic/typed_bells_invariants.clj:252` | False |
| `futon3c.logic.wm-operator-lane-invariants/run-verify` | `futon3c/src/futon3c/logic/wm_operator_lane_invariants.clj:257` | False |
| `futon3c.metric.e1/action-intensity` | `futon3c/src/futon3c/metric/e1.clj:145` | False |
| `futon3c.metric.e1/hop-distance` | `futon3c/src/futon3c/metric/e1.clj:96` | False |
| `futon3c.metric.e1/lazy-random-walk-measure` | `futon3c/src/futon3c/metric/e1.clj:79` | False |
| `futon3c.metric.e1/propose-here?` | `futon3c/src/futon3c/metric/e1.clj:132` | False |
| `futon3c.metric.e1/strain-rollup` | `futon3c/src/futon3c/metric/e1.clj:116` | False |
| `futon3c.metric.e1-report/report` | `futon3c/src/futon3c/metric/e1_report.clj:180` | False |
| `futon3c.metric.resolution-state/resolution-state` | `futon3c/src/futon3c/metric/resolution_state.clj:108` | False |
| `futon3c.mission-control.service/configure!` | `futon3c/src/futon3c/mission_control/service.clj:99` | False |
| `futon3c.mission-control.service/reset-service!` | `futon3c/src/futon3c/mission_control/service.clj:132` | False |
| `futon3c.nlp.classical-pipeline/-main` | `futon3c/src/futon3c/nlp/classical_pipeline.clj:383` | False |
| `futon3c.nlp.classical-pipeline/spot-terms` | `futon3c/src/futon3c/nlp/classical_pipeline.clj:76` | False |
| `futon3c.peripheral.adapter/claude-tools` | `futon3c/src/futon3c/peripheral/adapter.clj:118` | False |
| `futon3c.peripheral.adapter/codex-detect-exit` | `futon3c/src/futon3c/peripheral/adapter.clj:374` | False |
| `futon3c.peripheral.adapter/codex-instruction-section` | `futon3c/src/futon3c/peripheral/adapter.clj:333` | False |
| `futon3c.peripheral.adapter/codex-tool-call->action` | `futon3c/src/futon3c/peripheral/adapter.clj:216` | False |
| `futon3c.peripheral.adapter/codex-tools` | `futon3c/src/futon3c/peripheral/adapter.clj:128` | False |
| `futon3c.peripheral.adapter/peripheral-to-claude` | `futon3c/src/futon3c/peripheral/adapter.clj:77` | True |
| `futon3c.peripheral.adapter/tool-call->action` | `futon3c/src/futon3c/peripheral/adapter.clj:197` | False |
| `futon3c.peripheral.alfworld/ALFWorldPeripheral` | `futon3c/src/futon3c/peripheral/alfworld.clj:227` | False |
| `futon3c.peripheral.alfworld/alfworld-state` | `futon3c/src/futon3c/peripheral/alfworld.clj:70` | True |
| `futon3c.peripheral.alfworld/map->ALFWorldPeripheral` | `futon3c/src/futon3c/peripheral/alfworld.clj:227` | False |
| `futon3c.peripheral.arse/ArsePeripheral` | `futon3c/src/futon3c/peripheral/arse.clj:168` | False |
| `futon3c.peripheral.arse/map->ArsePeripheral` | `futon3c/src/futon3c/peripheral/arse.clj:168` | False |
| `futon3c.peripheral.chat/ChatPeripheral` | `futon3c/src/futon3c/peripheral/chat.clj:22` | False |
| `futon3c.peripheral.chat/map->ChatPeripheral` | `futon3c/src/futon3c/peripheral/chat.clj:22` | False |
| `futon3c.peripheral.cycle/CyclePeripheral` | `futon3c/src/futon3c/peripheral/cycle.clj:429` | False |
| `futon3c.peripheral.cycle/map->CyclePeripheral` | `futon3c/src/futon3c/peripheral/cycle.clj:429` | False |
| `futon3c.peripheral.deploy/DeployPeripheral` | `futon3c/src/futon3c/peripheral/deploy.clj:66` | False |
| `futon3c.peripheral.deploy/map->DeployPeripheral` | `futon3c/src/futon3c/peripheral/deploy.clj:66` | False |
| `futon3c.peripheral.discipline/DisciplinePeripheral` | `futon3c/src/futon3c/peripheral/discipline.clj:142` | False |
| `futon3c.peripheral.discipline/map->DisciplinePeripheral` | `futon3c/src/futon3c/peripheral/discipline.clj:142` | False |
| `futon3c.peripheral.drive/await-drive` | `futon3c/src/futon3c/peripheral/drive.clj:108` | False |
| `futon3c.peripheral.drive/drive!` | `futon3c/src/futon3c/peripheral/drive.clj:45` | False |
| `futon3c.peripheral.drive/register-toy-cycles!` | `futon3c/src/futon3c/peripheral/drive.clj:122` | False |
| `futon3c.peripheral.dynamic-queries-rung4/coupled-propagation` | `futon3c/src/futon3c/peripheral/dynamic_queries_rung4.clj:180` | False |
| `futon3c.peripheral.edit/EditPeripheral` | `futon3c/src/futon3c/peripheral/edit.clj:76` | False |
| `futon3c.peripheral.edit/map->EditPeripheral` | `futon3c/src/futon3c/peripheral/edit.clj:76` | False |
| `futon3c.peripheral.emacs-cursor/EmacsCursorPeripheral` | `futon3c/src/futon3c/peripheral/emacs_cursor.clj:176` | False |
| `futon3c.peripheral.emacs-cursor/map->EmacsCursorPeripheral` | `futon3c/src/futon3c/peripheral/emacs_cursor.clj:176` | False |
| `futon3c.peripheral.explore/ExplorePeripheral` | `futon3c/src/futon3c/peripheral/explore.clj:63` | False |
| `futon3c.peripheral.explore/map->ExplorePeripheral` | `futon3c/src/futon3c/peripheral/explore.clj:63` | False |
| `futon3c.peripheral.live-wm-selection/validated-selection` | `futon3c/src/futon3c/peripheral/live_wm_selection.clj:439` | False |
| `futon3c.peripheral.memory-lifecycle/challenge-memory!` | `futon3c/src/futon3c/peripheral/memory_lifecycle.clj:513` | False |
| `futon3c.peripheral.memory-lifecycle/retract-memory!` | `futon3c/src/futon3c/peripheral/memory_lifecycle.clj:638` | False |
| `futon3c.peripheral.memory-lifecycle/retrieval-to-use-ms` | `futon3c/src/futon3c/peripheral/memory_lifecycle.clj:35` | False |
| `futon3c.peripheral.memory-lifecycle/supersede-memory!` | `futon3c/src/futon3c/peripheral/memory_lifecycle.clj:559` | False |
| `futon3c.peripheral.memory-trials/retrieval-case-row` | `futon3c/src/futon3c/peripheral/memory_trials.clj:69` | False |
| `futon3c.peripheral.memory-trials/run-bounded!` | `futon3c/src/futon3c/peripheral/memory_trials.clj:28` | False |
| `futon3c.peripheral.mentor/MentorPeripheral` | `futon3c/src/futon3c/peripheral/mentor.clj:293` | False |
| `futon3c.peripheral.mentor/map->MentorPeripheral` | `futon3c/src/futon3c/peripheral/mentor.clj:293` | False |
| `futon3c.peripheral.mission-backend/MissionBackend` | `futon3c/src/futon3c/peripheral/mission_backend.clj:869` | False |
| `futon3c.peripheral.mission-backend/init-mission!` | `futon3c/src/futon3c/peripheral/mission_backend.clj:940` | False |
| `futon3c.peripheral.mission-backend/map->MissionBackend` | `futon3c/src/futon3c/peripheral/mission_backend.clj:869` | False |
| `futon3c.peripheral.mission-backend/mission-tools` | `futon3c/src/futon3c/peripheral/mission_backend.clj:855` | False |
| `futon3c.peripheral.mission-control/MissionControlPeripheral` | `futon3c/src/futon3c/peripheral/mission_control.clj:143` | False |
| `futon3c.peripheral.mission-control/map->MissionControlPeripheral` | `futon3c/src/futon3c/peripheral/mission_control.clj:143` | False |
| `futon3c.peripheral.mission-control-backend/audit-coverage-correspondence` | `futon3c/src/futon3c/peripheral/mission_control_backend.clj:1477` | False |
| `futon3c.peripheral.mission-control-backend/build-inventory-with-turn-counts` | `futon3c/src/futon3c/peripheral/mission_control_backend.clj:1151` | False |
| `futon3c.peripheral.mission-control-backend/trace-all-components` | `futon3c/src/futon3c/peripheral/mission_control_backend.clj:1798` | False |
| `futon3c.peripheral.mission-control-shapes/PortfolioReview` | `futon3c/src/futon3c/peripheral/mission_control_shapes.clj:97` | False |
| `futon3c.peripheral.mission-control-shapes/TensionEntry` | `futon3c/src/futon3c/peripheral/mission_control_shapes.clj:116` | False |
| `futon3c.peripheral.mission-logic/check-mission-state` | `futon3c/src/futon3c/peripheral/mission_logic.clj:145` | False |
| `futon3c.peripheral.mission-logic/violations?` | `futon3c/src/futon3c/peripheral/mission_logic.clj:141` | False |
| `futon3c.peripheral.mission-shapes/OperationKind` | `futon3c/src/futon3c/peripheral/mission_shapes.clj:48` | False |
| `futon3c.peripheral.mission-shapes/tool-operation-kind` | `futon3c/src/futon3c/peripheral/mission_shapes.clj:83` | False |
| `futon3c.peripheral.mission-shapes/valid?` | `futon3c/src/futon3c/peripheral/mission_shapes.clj:260` | False |
| `futon3c.peripheral.night-shift/NightShiftPeripheral` | `futon3c/src/futon3c/peripheral/night_shift.clj:166` | False |
| `futon3c.peripheral.night-shift/map->NightShiftPeripheral` | `futon3c/src/futon3c/peripheral/night_shift.clj:166` | False |
| `futon3c.peripheral.night-shift/spike-check` | `futon3c/src/futon3c/peripheral/night_shift.clj:224` | False |
| `futon3c.peripheral.night-shift-backend/NightShiftBackend` | `futon3c/src/futon3c/peripheral/night_shift_backend.clj:645` | False |
| `futon3c.peripheral.night-shift-backend/map->NightShiftBackend` | `futon3c/src/futon3c/peripheral/night_shift_backend.clj:645` | False |
| `futon3c.peripheral.night-shift-shapes/ambient-tools` | `futon3c/src/futon3c/peripheral/night_shift_shapes.clj:41` | False |
| `futon3c.peripheral.night-shift-shapes/phase-tools` | `futon3c/src/futon3c/peripheral/night_shift_shapes.clj:70` | False |
| `futon3c.peripheral.outing/finalize-outing!` | `futon3c/src/futon3c/peripheral/outing.clj:112` | False |
| `futon3c.peripheral.outing/live-regression-ok?` | `futon3c/src/futon3c/peripheral/outing.clj:149` | False |
| `futon3c.peripheral.outing/record-cycle!` | `futon3c/src/futon3c/peripheral/outing.clj:79` | False |
| `futon3c.peripheral.outing/register-live-family!` | `futon3c/src/futon3c/peripheral/outing.clj:155` | False |
| `futon3c.peripheral.outing/start-outing!` | `futon3c/src/futon3c/peripheral/outing.clj:28` | False |
| `futon3c.peripheral.portfolio-inference/PortfolioInferencePeripheral` | `futon3c/src/futon3c/peripheral/portfolio_inference.clj:147` | False |
| `futon3c.peripheral.portfolio-inference/map->PortfolioInferencePeripheral` | `futon3c/src/futon3c/peripheral/portfolio_inference.clj:147` | False |
| `futon3c.peripheral.portfolio-inference-shapes/FamilyAggregation` | `futon3c/src/futon3c/peripheral/portfolio_inference_shapes.clj:63` | False |
| `futon3c.peripheral.portfolio-inference-shapes/MissionFeatureEntry` | `futon3c/src/futon3c/peripheral/portfolio_inference_shapes.clj:44` | False |
| `futon3c.peripheral.portfolio-inference-shapes/PromotionCandidate` | `futon3c/src/futon3c/peripheral/portfolio_inference_shapes.clj:93` | False |
| `futon3c.peripheral.problem/CheckoutProvisioningBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1153` | False |
| `futon3c.peripheral.problem/EvidenceRequiredProblemPeripheral` | `futon3c/src/futon3c/peripheral/problem.clj:1799` | False |
| `futon3c.peripheral.problem/GroundControlBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1282` | False |
| `futon3c.peripheral.problem/ProblemCycleBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1505` | False |
| `futon3c.peripheral.problem/ProblemStateBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1052` | False |
| `futon3c.peripheral.problem/map->CheckoutProvisioningBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1153` | False |
| `futon3c.peripheral.problem/map->EvidenceRequiredProblemPeripheral` | `futon3c/src/futon3c/peripheral/problem.clj:1799` | False |
| `futon3c.peripheral.problem/map->GroundControlBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1282` | False |
| `futon3c.peripheral.problem/map->ProblemCycleBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1505` | False |
| `futon3c.peripheral.problem/map->ProblemStateBackend` | `futon3c/src/futon3c/peripheral/problem.clj:1052` | False |
| `futon3c.peripheral.proof-backend/ProofBackend` | `futon3c/src/futon3c/peripheral/proof_backend.clj:1216` | False |
| `futon3c.peripheral.proof-backend/init-problem!` | `futon3c/src/futon3c/peripheral/proof_backend.clj:1290` | False |
| `futon3c.peripheral.proof-backend/map->ProofBackend` | `futon3c/src/futon3c/peripheral/proof_backend.clj:1216` | False |
| `futon3c.peripheral.proof-backend/proof-tools` | `futon3c/src/futon3c/peripheral/proof_backend.clj:1099` | False |
| `futon3c.peripheral.proof-dag/build-adjacency` | `futon3c/src/futon3c/peripheral/proof_dag.clj:18` | True |
| `futon3c.peripheral.proof-dag/build-reverse-adjacency` | `futon3c/src/futon3c/peripheral/proof_dag.clj:28` | True |
| `futon3c.peripheral.proof-dag/depends-chain` | `futon3c/src/futon3c/peripheral/proof_dag.clj:136` | False |
| `futon3c.peripheral.proof-dag/edge-consistency?` | `futon3c/src/futon3c/peripheral/proof_dag.clj:170` | False |
| `futon3c.peripheral.proof-dag/impact-score` | `futon3c/src/futon3c/peripheral/proof_dag.clj:122` | False |
| `futon3c.peripheral.proof-dag/reachable-from` | `futon3c/src/futon3c/peripheral/proof_dag.clj:131` | False |
| `futon3c.peripheral.proof-logic/check-proof-state` | `futon3c/src/futon3c/peripheral/proof_logic.clj:373` | False |
| `futon3c.peripheral.proof-logic/edge-symmetrico` | `futon3c/src/futon3c/peripheral/proof_logic.clj:188` | False |
| `futon3c.peripheral.proof-logic/false-is-terminalo` | `futon3c/src/futon3c/peripheral/proof_logic.clj:214` | False |
| `futon3c.peripheral.proof-logic/status-valido` | `futon3c/src/futon3c/peripheral/proof_logic.clj:197` | False |
| `futon3c.peripheral.proof-logic/violations?` | `futon3c/src/futon3c/peripheral/proof_logic.clj:364` | False |
| `futon3c.peripheral.proof-shapes/OperationKind` | `futon3c/src/futon3c/peripheral/proof_shapes.clj:45` | False |
| `futon3c.peripheral.proof-shapes/mandatory-gates` | `futon3c/src/futon3c/peripheral/proof_shapes.clj:423` | False |
| `futon3c.peripheral.proof-shapes/tool-operation-kind` | `futon3c/src/futon3c/peripheral/proof_shapes.clj:80` | False |
| `futon3c.peripheral.proof-shapes/valid?` | `futon3c/src/futon3c/peripheral/proof_shapes.clj:444` | False |
| `futon3c.peripheral.pull-receipts/pull-surfaced-ids` | `futon3c/src/futon3c/peripheral/pull_receipts.clj:80` | False |
| `futon3c.peripheral.real-backend/RealBackend` | `futon3c/src/futon3c/peripheral/real_backend.clj:1074` | False |
| `futon3c.peripheral.real-backend/map->RealBackend` | `futon3c/src/futon3c/peripheral/real_backend.clj:1074` | False |
| `futon3c.peripheral.reflect/ReflectPeripheral` | `futon3c/src/futon3c/peripheral/reflect.clj:66` | False |
| `futon3c.peripheral.reflect/map->ReflectPeripheral` | `futon3c/src/futon3c/peripheral/reflect.clj:66` | False |
| `futon3c.peripheral.round-trip/run-and-verify` | `futon3c/src/futon3c/peripheral/round_trip.clj:117` | False |
| `futon3c.peripheral.strategic-canary/advice-only` | `futon3c/src/futon3c/peripheral/strategic_canary.clj:242` | False |
| `futon3c.peripheral.strategic-cascade/budgeted-facet-frontier` | `futon3c/src/futon3c/peripheral/strategic_cascade.clj:387` | False |
| `futon3c.peripheral.strategic-embedding-experiment/run-experiment` | `futon3c/src/futon3c/peripheral/strategic_embedding_experiment.clj:226` | False |
| `futon3c.peripheral.street-sweeper/run-full-sweep` | `futon3c/src/futon3c/peripheral/street_sweeper.clj:251` | False |
| `futon3c.peripheral.street-sweeper/spike-check` | `futon3c/src/futon3c/peripheral/street_sweeper.clj:97` | False |
| `futon3c.peripheral.street-sweeper/sweep-summary` | `futon3c/src/futon3c/peripheral/street_sweeper.clj:412` | False |
| `futon3c.peripheral.street-sweeper-backend/SweeperBackend` | `futon3c/src/futon3c/peripheral/street_sweeper_backend.clj:716` | False |
| `futon3c.peripheral.street-sweeper-backend/list-cg-bindings` | `futon3c/src/futon3c/peripheral/street_sweeper_backend.clj:56` | False |
| `futon3c.peripheral.street-sweeper-backend/map->SweeperBackend` | `futon3c/src/futon3c/peripheral/street_sweeper_backend.clj:716` | False |
| `futon3c.peripheral.street-sweeper-shapes/file-size-bytes` | `futon3c/src/futon3c/peripheral/street_sweeper_shapes.clj:244` | False |
| `futon3c.peripheral.street-sweeper-shapes/file-size-bytes` | `futon3c/src/futon3c/peripheral/street_sweeper_shapes.clj:465` | True |
| `futon3c.peripheral.street-sweeper-shapes/sentinel-defer-this` | `futon3c/src/futon3c/peripheral/street_sweeper_shapes.clj:209` | False |
| `futon3c.peripheral.street-sweeper-shapes/sentinel-include-this` | `futon3c/src/futon3c/peripheral/street_sweeper_shapes.clj:210` | False |
| `futon3c.peripheral.street-sweeper-shapes/sentinel-keep-here` | `futon3c/src/futon3c/peripheral/street_sweeper_shapes.clj:208` | False |
| `futon3c.peripheral.street-sweeper-shapes/substantive-tools` | `futon3c/src/futon3c/peripheral/street_sweeper_shapes.clj:59` | False |
| `futon3c.peripheral.test-runner/TestPeripheral` | `futon3c/src/futon3c/peripheral/test_runner.clj:114` | False |
| `futon3c.peripheral.test-runner/map->TestPeripheral` | `futon3c/src/futon3c/peripheral/test_runner.clj:114` | False |
| `futon3c.peripheral.tools/MockBackend` | `futon3c/src/futon3c/peripheral/tools.clj:113` | False |
| `futon3c.peripheral.tools/map->MockBackend` | `futon3c/src/futon3c/peripheral/tools.clj:113` | False |
| `futon3c.peripheral.tools/recorded-calls` | `futon3c/src/futon3c/peripheral/tools.clj:135` | False |
| `futon3c.peripheral.war-machine-pilot/begin-live-cycle!` | `futon3c/src/futon3c/peripheral/war_machine_pilot.clj:328` | False |
| `futon3c.peripheral.war-machine-pilot/close-live-cycle!` | `futon3c/src/futon3c/peripheral/war_machine_pilot.clj:493` | False |
| `futon3c.peripheral.war-machine-pilot/run-observe-cycle` | `futon3c/src/futon3c/peripheral/war_machine_pilot.clj:115` | False |
| `futon3c.peripheral.war-machine-pilot/spike-check` | `futon3c/src/futon3c/peripheral/war_machine_pilot.clj:168` | False |
| `futon3c.peripheral.war-machine-pilot-backend/PilotBackend` | `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj:571` | False |
| `futon3c.peripheral.war-machine-pilot-backend/map->PilotBackend` | `futon3c/src/futon3c/peripheral/war_machine_pilot_backend.clj:571` | False |
| `futon3c.peripheral.war-machine-pilot-shapes/substantive-tools` | `futon3c/src/futon3c/peripheral/war_machine_pilot_shapes.clj:69` | False |
| `futon3c.peripheral.wm-memory/decision-keyed-external-check-entry` | `futon3c/src/futon3c/peripheral/wm_memory.clj:86` | False |
| `futon3c.peripheral.wm-memory/record-decision-keyed-external-check!` | `futon3c/src/futon3c/peripheral/wm_memory.clj:92` | False |
| `futon3c.peripheral.wm-memory/record-episode!` | `futon3c/src/futon3c/peripheral/wm_memory.clj:68` | False |
| `futon3c.peripheral.wm-memory/witnessed-projection-triple` | `futon3c/src/futon3c/peripheral/wm_memory.clj:99` | False |
| `futon3c.portfolio-inference.scheduler/set-period!` | `futon3c/src/futon3c/portfolio_inference/scheduler.clj:311` | False |
| `futon3c.portfolio-inference.scheduler/start!` | `futon3c/src/futon3c/portfolio_inference/scheduler.clj:250` | False |
| `futon3c.portfolio-inference.scheduler/stop!` | `futon3c/src/futon3c/portfolio_inference/scheduler.clj:293` | False |
| `futon3c.portfolio-inference.service/configure!` | `futon3c/src/futon3c/portfolio_inference/service.clj:99` | False |
| `futon3c.portfolio-inference.service/list-sessions` | `futon3c/src/futon3c/portfolio_inference/service.clj:131` | False |
| `futon3c.portfolio-inference.service/reset-service!` | `futon3c/src/futon3c/portfolio_inference/service.clj:125` | False |
| `futon3c.portfolio-inference.service/run-review!` | `futon3c/src/futon3c/portfolio_inference/service.clj:243` | False |
| `futon3c.portfolio-inference.service/status` | `futon3c/src/futon3c/portfolio_inference/service.clj:118` | False |
| `futon3c.portfolio.adjacent/adjacent?` | `futon3c/src/futon3c/portfolio/adjacent.clj:16` | False |
| `futon3c.portfolio.adjacent/critical-path` | `futon3c/src/futon3c/portfolio/adjacent.clj:78` | False |
| `futon3c.portfolio.adjacent/structural-summary` | `futon3c/src/futon3c/portfolio/adjacent.clj:85` | False |
| `futon3c.portfolio.adjacent/what-if-complete` | `futon3c/src/futon3c/portfolio/adjacent.clj:71` | False |
| `futon3c.portfolio.effect/apply-portfolio-action` | `futon3c/src/futon3c/portfolio/effect.clj:17` | False |
| `futon3c.portfolio.effect/execute-effect!` | `futon3c/src/futon3c/portfolio/effect.clj:42` | False |
| `futon3c.portfolio.effect/portfolio-action->evidence` | `futon3c/src/futon3c/portfolio/effect.clj:116` | False |
| `futon3c.portfolio.heartbeat/fetch-heartbeat` | `futon3c/src/futon3c/portfolio/heartbeat.clj:106` | False |
| `futon3c.portfolio.heartbeat/post-bid!` | `futon3c/src/futon3c/portfolio/heartbeat.clj:116` | False |
| `futon3c.portfolio.heartbeat/post-clear!` | `futon3c/src/futon3c/portfolio/heartbeat.clj:127` | False |
| `futon3c.portfolio.logic/query-consistency` | `futon3c/src/futon3c/portfolio/logic.clj:441` | False |
| `futon3c.portfolio.logic/query-pattern-co-occurrence` | `futon3c/src/futon3c/portfolio/logic.clj:237` | False |
| `futon3c.portfolio.logic/query-unblocked-by` | `futon3c/src/futon3c/portfolio/logic.clj:204` | False |
| `futon3c.portfolio.logic/uncoveredo` | `futon3c/src/futon3c/portfolio/logic.clj:378` | False |
| `futon3c.portfolio.observe/obs->vector` | `futon3c/src/futon3c/portfolio/observe.clj:256` | False |
| `futon3c.portfolio.observe/turn-zone` | `futon3c/src/futon3c/portfolio/observe.clj:14` | True |
| `futon3c.process-watchdog/start!` | `futon3c/src/futon3c/process_watchdog.clj:293` | False |
| `futon3c.process-watchdog/stop!` | `futon3c/src/futon3c/process_watchdog.clj:327` | False |
| `futon3c.process-watchdog/tick!` | `futon3c/src/futon3c/process_watchdog.clj:360` | False |
| `futon3c.proof.bridge/canonical` | `futon3c/src/futon3c/proof/bridge.clj:133` | False |
| `futon3c.proof.bridge/canonical-update!` | `futon3c/src/futon3c/proof/bridge.clj:138` | False |
| `futon3c.proof.bridge/conjecture-add!` | `futon3c/src/futon3c/proof/bridge.clj:300` | False |
| `futon3c.proof.bridge/conjecture-refine!` | `futon3c/src/futon3c/proof/bridge.clj:365` | False |
| `futon3c.proof.bridge/conjecture-test!` | `futon3c/src/futon3c/proof/bridge.clj:326` | False |
| `futon3c.proof.bridge/conjectures` | `futon3c/src/futon3c/proof/bridge.clj:399` | False |
| `futon3c.proof.bridge/corpus-check` | `futon3c/src/futon3c/proof/bridge.clj:267` | False |
| `futon3c.proof.bridge/cycle-advance!` | `futon3c/src/futon3c/proof/bridge.clj:173` | False |
| `futon3c.proof.bridge/cycle-begin!` | `futon3c/src/futon3c/proof/bridge.clj:167` | False |
| `futon3c.proof.bridge/cycle-get` | `futon3c/src/futon3c/proof/bridge.clj:180` | False |
| `futon3c.proof.bridge/cycles` | `futon3c/src/futon3c/proof/bridge.clj:185` | False |
| `futon3c.proof.bridge/dag-check` | `futon3c/src/futon3c/proof/bridge.clj:194` | False |
| `futon3c.proof.bridge/dag-impact` | `futon3c/src/futon3c/proof/bridge.clj:199` | False |
| `futon3c.proof.bridge/failed-route!` | `futon3c/src/futon3c/proof/bridge.clj:245` | False |
| `futon3c.proof.bridge/gate-check` | `futon3c/src/futon3c/proof/bridge.clj:208` | False |
| `futon3c.proof.bridge/heuristic-add!` | `futon3c/src/futon3c/proof/bridge.clj:494` | False |
| `futon3c.proof.bridge/heuristic-retire!` | `futon3c/src/futon3c/proof/bridge.clj:519` | False |
| `futon3c.proof.bridge/heuristics` | `futon3c/src/futon3c/proof/bridge.clj:542` | False |
| `futon3c.proof.bridge/ledger` | `futon3c/src/futon3c/proof/bridge.clj:149` | False |
| `futon3c.proof.bridge/make-dispatch-envelope` | `futon3c/src/futon3c/proof/bridge.clj:421` | False |
| `futon3c.proof.bridge/mentor-guidance` | `futon3c/src/futon3c/proof/bridge.clj:568` | False |
| `futon3c.proof.bridge/mode` | `futon3c/src/futon3c/proof/bridge.clj:116` | False |
| `futon3c.proof.bridge/mode!` | `futon3c/src/futon3c/proof/bridge.clj:122` | False |
| `futon3c.proof.bridge/reload!` | `futon3c/src/futon3c/proof/bridge.clj:90` | False |
| `futon3c.proof.bridge/save!` | `futon3c/src/futon3c/proof/bridge.clj:107` | False |
| `futon3c.proof.bridge/status-validate` | `futon3c/src/futon3c/proof/bridge.clj:255` | False |
| `futon3c.proof.bridge/summary` | `futon3c/src/futon3c/proof/bridge.clj:621` | False |
| `futon3c.proof.bridge/tryharder-close!` | `futon3c/src/futon3c/proof/bridge.clj:225` | False |
| `futon3c.proof.bridge/tryharder-create!` | `futon3c/src/futon3c/proof/bridge.clj:218` | False |
| `futon3c.proof.bridge/tryharder-list` | `futon3c/src/futon3c/proof/bridge.clj:236` | False |
| `futon3c.proof.bridge/tryharder-status` | `futon3c/src/futon3c/proof/bridge.clj:231` | False |
| `futon3c.reflection.envelope/valid?` | `futon3c/src/futon3c/reflection/envelope.clj:65` | False |
| `futon3c.runtime.agents/make-http-handler` | `futon3c/src/futon3c/runtime/agents.clj:168` | False |
| `futon3c.runtime.agents/make-ws-callbacks` | `futon3c/src/futon3c/runtime/agents.clj:175` | False |
| `futon3c.runtime.agents/make-ws-handler` | `futon3c/src/futon3c/runtime/agents.clj:182` | False |
| `futon3c.runtime.agents/register-claude!` | `futon3c/src/futon3c/runtime/agents.clj:92` | False |
| `futon3c.runtime.agents/register-codex!` | `futon3c/src/futon3c/runtime/agents.clj:79` | False |
| `futon3c.runtime.agents/register-tickle!` | `futon3c/src/futon3c/runtime/agents.clj:104` | False |
| `futon3c.runtime.incidents/health` | `futon3c/src/futon3c/runtime/incidents.clj:49` | False |
| `futon3c.runtime.incidents/incidents` | `futon3c/src/futon3c/runtime/incidents.clj:178` | False |
| `futon3c.runtime.incidents/install-default-handler!` | `futon3c/src/futon3c/runtime/incidents.clj:154` | False |
| `futon3c.scripts.mission-scope-ingest/-main` | `futon3c/src/futon3c/scripts/mission_scope_ingest.clj:2139` | False |
| `futon3c.scripts.mission-scope-ingest/maintain-mission!` | `futon3c/src/futon3c/scripts/mission_scope_ingest.clj:2129` | False |
| `futon3c.scripts.mission-scope-view/-main` | `futon3c/src/futon3c/scripts/mission_scope_view.clj:138` | False |
| `futon3c.social.authenticate/resolve-identity` | `futon3c/src/futon3c/social/authenticate.clj:33` | False |
| `futon3c.social.bells/ring-standup!` | `futon3c/src/futon3c/social/bells.clj:62` | False |
| `futon3c.social.mode/validate-transition` | `futon3c/src/futon3c/social/mode.clj:75` | False |
| `futon3c.social.peripheral/load-peripherals` | `futon3c/src/futon3c/social/peripheral.clj:22` | False |
| `futon3c.social.persist/reset-sessions!` | `futon3c/src/futon3c/social/persist.clj:17` | False |
| `futon3c.social.shapes/shapes` | `futon3c/src/futon3c/social/shapes.clj:425` | False |
| `futon3c.social.validate/validate-outcome` | `futon3c/src/futon3c/social/validate.clj:33` | False |
| `futon3c.substrate.client/hyperedge-by-id` | `futon3c/src/futon3c/substrate/client.clj:212` | False |
| `futon3c.substrate.client/hyperedge-page-limit` | `futon3c/src/futon3c/substrate/client.clj:29` | False |
| `futon3c.substrate.client/partial-result?` | `futon3c/src/futon3c/substrate/client.clj:115` | False |
| `futon3c.test-registry/-main` | `futon3c/src/futon3c/test_registry.clj:965` | False |
| `futon3c.test-registry/directory-sha` | `futon3c/src/futon3c/test_registry.clj:211` | False |
| `futon3c.test-registry/test-namespace-of` | `futon3c/src/futon3c/test_registry.clj:302` | False |
| `futon3c.test-registry.validation/-main` | `futon3c/src/futon3c/test_registry/validation.clj:244` | False |
| `futon3c.test-registry.validation/close-revalidation!` | `futon3c/src/futon3c/test_registry/validation.clj:140` | False |
| `futon3c.test-registry.validation-adapters/-main` | `futon3c/src/futon3c/test_registry/validation_adapters.clj:135` | False |
| `futon3c.transport.bootstrap-handler-migration/migrate!` | `futon3c/src/futon3c/transport/bootstrap_handler_migration.clj:21` | False |
| `futon3c.transport.encyclopedia/clear-cache!` | `futon3c/src/futon3c/transport/encyclopedia.clj:17` | False |
| `futon3c.transport.http/!stale-job-reaper` | `futon3c/src/futon3c/transport/http.clj:2375` | True |
| `futon3c.transport.http/active-invoke-job-counts-consistency` | `futon3c/src/futon3c/transport/http.clj:1784` | False |
| `futon3c.transport.http/active-invoke-job-counts-full-scan` | `futon3c/src/futon3c/transport/http.clj:1768` | False |
| `futon3c.transport.http/bind-unbound-invoke-request!` | `futon3c/src/futon3c/transport/http.clj:1678` | False |
| `futon3c.transport.http/configure-invoke-ingress-controller!` | `futon3c/src/futon3c/transport/http.clj:267` | False |
| `futon3c.transport.http/finalizer-written-states` | `futon3c/src/futon3c/transport/http.clj:346` | False |
| `futon3c.transport.http/first-matching-ref` | `futon3c/src/futon3c/transport/http.clj:991` | True |
| `futon3c.transport.http/known-invoke-job-states` | `futon3c/src/futon3c/transport/http.clj:361` | False |
| `futon3c.transport.http/pattern-id->collection-name` | `futon3c/src/futon3c/transport/http.clj:7814` | True |
| `futon3c.transport.http/reconfigure-handler!` | `futon3c/src/futon3c/transport/http.clj:9266` | False |
| `futon3c.transport.http/reset-invoke-jobs!` | `futon3c/src/futon3c/transport/http.clj:408` | False |
| `futon3c.transport.http/stale-job-threshold-ms` | `futon3c/src/futon3c/transport/http.clj:2237` | False |
| `futon3c.transport.http/start-server!` | `futon3c/src/futon3c/transport/http.clj:9647` | False |
| `futon3c.transport.irc/make-relay-bridge` | `futon3c/src/futon3c/transport/irc.clj:430` | False |
| `futon3c.transport.irc/start-irc-server!` | `futon3c/src/futon3c/transport/irc.clj:540` | False |
| `futon3c.transport.protocol/render-peripheral-event` | `futon3c/src/futon3c/transport/protocol.clj:651` | False |
| `futon3c.transport.ws/connected-agents` | `futon3c/src/futon3c/transport/ws.clj:623` | False |
| `futon3c.transport.ws/send-peripheral-event!` | `futon3c/src/futon3c/transport/ws.clj:48` | False |
| `futon3c.transport.ws.invoke/connected-observer-ids` | `futon3c/src/futon3c/transport/ws/invoke.clj:144` | False |
| `futon3c.transport.ws.invoke/default-timeout-ms` | `futon3c/src/futon3c/transport/ws/invoke.clj:9` | False |
| `futon3c.transport.ws.invoke/set-late-result-handler!` | `futon3c/src/futon3c/transport/ws/invoke.clj:171` | False |
| `futon3c.transport.ws.invoke/unregister!` | `futon3c/src/futon3c/transport/ws/invoke.clj:101` | False |
| `futon3c.transport.ws.replication/start!` | `futon3c/src/futon3c/transport/ws/replication.clj:165` | False |
| `futon3c.vsatarcs.feeder/start!` | `futon3c/src/futon3c/vsatarcs/feeder.clj:367` | False |
| `futon3c.watcher.commit-ingest/ingest-all-commits!` | `futon3c/src/futon3c/watcher/commit_ingest.clj:601` | False |
| `futon3c.watcher.file-ingest/fixture-sorry-roundtrip` | `futon3c/src/futon3c/watcher/file_ingest.clj:814` | False |
| `futon3c.watcher.file-ingest/invalidate-mission-id-cache!` | `futon3c/src/futon3c/watcher/file_ingest.clj:249` | False |
| `futon3c.watcher.file-ingest/post-relation!` | `futon3c/src/futon3c/watcher/file_ingest.clj:320` | False |
| `futon3c.watcher.flight-ingest/ingest-projection!` | `futon3c/src/futon3c/watcher/flight_ingest.clj:72` | False |
| `futon3c.watcher.multi/arm-scope-lane!` | `futon3c/src/futon3c/watcher/multi.clj:102` | False |
| `futon3c.watcher.multi/disarm-scope-lane!` | `futon3c/src/futon3c/watcher/multi.clj:110` | False |
| `futon3c.watcher.multi/query-repo-vars-by-file` | `futon3c/src/futon3c/watcher/multi.clj:1335` | False |
| `futon3c.watcher.multi/reset-scope-lane-override!` | `futon3c/src/futon3c/watcher/multi.clj:122` | False |
| `futon3c.watcher.multi/retract-flexiarg!` | `futon3c/src/futon3c/watcher/multi.clj:824` | False |
| `futon3c.watcher.multi/start!` | `futon3c/src/futon3c/watcher/multi.clj:1643` | False |
| `futon3c.watcher.multi/stop!` | `futon3c/src/futon3c/watcher/multi.clj:1728` | False |
| `futon3c.watcher.multi/tick!` | `futon3c/src/futon3c/watcher/multi.clj:1757` | False |
| `futon3c.watcher.projections.essay/src-exts` | `futon3c/src/futon3c/watcher/projections/essay.clj:8` | False |
| `futon3c.watcher.projections.flight/collect-file` | `futon3c/src/futon3c/watcher/projections/flight.clj:195` | False |
| `futon3c.watcher.replay/replay-all!` | `futon3c/src/futon3c/watcher/replay.clj:209` | False |
| `futon3c.wm.code-identity/install-for-test!` | `futon3c/src/futon3c/wm/code_identity.clj:82` | False |
| `futon3c.wm.code-identity/load-file-recorded!` | `futon3c/src/futon3c/wm/code_identity.clj:51` | False |
| `futon3c.wm.code-identity/reset-for-test!` | `futon3c/src/futon3c/wm/code_identity.clj:81` | False |
| `futon3c.wm.code-identity/status` | `futon3c/src/futon3c/wm/code_identity.clj:73` | False |
| `futon3c.wm.needs-you/emit-proctor-finding!` | `futon3c/src/futon3c/wm/needs_you.clj:234` | False |
| `futon3c.wm.needs-you/sorry-joe-line` | `futon3c/src/futon3c/wm/needs_you.clj:107` | False |
| `futon3c.wm.operator-bulletin/build-bulletin` | `futon3c/src/futon3c/wm/operator_bulletin.clj:35` | False |
| `futon3c.wm.operator-bulletin/newly-acknowledged` | `futon3c/src/futon3c/wm/operator_bulletin.clj:53` | False |
| `futon3c.wm.operator-bulletin/render-bulletin` | `futon3c/src/futon3c/wm/operator_bulletin.clj:68` | False |
| `futon3c.wm.operator-lane/lanes` | `futon3c/src/futon3c/wm/operator_lane.clj:27` | False |
| `futon3c.wm.operator-lane-adapter/operator-items` | `futon3c/src/futon3c/wm/operator_lane_adapter.clj:178` | False |
| `futon3c.wm.outing/run-cycle-gates!` | `futon3c/src/futon3c/wm/outing.clj:87` | False |
| `futon3c.wm.r10-click-adapter/commissioned-click!` | `futon3c/src/futon3c/wm/r10_click_adapter.clj:20` | False |
| `futon3c.wm.run4-boot/materialize` | `futon3c/src/futon3c/wm/run4_boot.clj:104` | False |
| `futon3c.wm.run4-codex-fold/make-port` | `futon3c/src/futon3c/wm/run4_codex_fold.clj:26` | False |
| `futon3c.wm.run4-deployment-preflight/-main` | `futon3c/src/futon3c/wm/run4_deployment_preflight.clj:67` | False |
| `futon3c.wm.run4-historical-qualification/produce!` | `futon3c/src/futon3c/wm/run4_historical_qualification.clj:137` | False |
| `futon3c.wm.run4-historical-successor/->TerminalSuccessorAuthority` | `futon3c/src/futon3c/wm/run4_historical_successor.clj:10` | False |
| `futon3c.wm.run4-historical-verification/admit!` | `futon3c/src/futon3c/wm/run4_historical_verification.clj:30` | False |
| `futon3c.wm.run4-infrastructure-reconciliation/publish!` | `futon3c/src/futon3c/wm/run4_infrastructure_reconciliation.clj:127` | False |
| `futon3c.wm.run4-report-service/report!` | `futon3c/src/futon3c/wm/run4_report_service.clj:16` | False |
| `futon3c.wm.run4-series-queue/recover!` | `futon3c/src/futon3c/wm/run4_series_queue.clj:294` | False |
| `futon3c.wm.run4-series-queue/resume!` | `futon3c/src/futon3c/wm/run4_series_queue.clj:325` | False |
| `futon3c.wm.run4-series-queue/start!` | `futon3c/src/futon3c/wm/run4_series_queue.clj:280` | False |
| `futon3c.wm.run4-series-queue/stop!` | `futon3c/src/futon3c/wm/run4_series_queue.clj:309` | False |
| `futon3c.wm.run4-terminal-evidence/terminal-evidence-port` | `futon3c/src/futon3c/wm/run4_terminal_evidence.clj:387` | False |
| `futon3c.wm.scheduler/ensure-started!` | `futon3c/src/futon3c/wm/scheduler.clj:478` | False |
| `futon3c.wm.scheduler/request-tick!` | `futon3c/src/futon3c/wm/scheduler.clj:336` | False |
| `futon3c.wm.scheduler/request-window!` | `futon3c/src/futon3c/wm/scheduler.clj:460` | False |
| `futon3c.wm.scheduler/set-period!` | `futon3c/src/futon3c/wm/scheduler.clj:442` | False |
| `futon3c.wm.scheduler/snapshot-for-days` | `futon3c/src/futon3c/wm/scheduler.clj:201` | False |
| `futon3c.wm.scheduler/stop!` | `futon3c/src/futon3c/wm/scheduler.clj:18` | False |
| `futon3c.wm.scheduler/stop!` | `futon3c/src/futon3c/wm/scheduler.clj:426` | False |
| `repl.http/start!` | `futon3c/src/repl/http.clj:320` | False |
| `repl.http/stop!` | `futon3c/src/repl/http.clj:348` | False |
</details>

<details><summary>Loaded namespace snapshot (447 names)</summary>

```text
babashka.http-client
babashka.http-client.interceptors
babashka.http-client.internal
babashka.http-client.internal.helpers
babashka.http-client.internal.multipart
babashka.http-client.internal.version
borkdude.dynaload
camel-snake-kebab.core
camel-snake-kebab.internals.alter-name
camel-snake-kebab.internals.macros
camel-snake-kebab.internals.misc
camel-snake-kebab.internals.string-separator
cemerick.drawbridge
cheshire.core
cheshire.factory
cheshire.generate
cheshire.generate-seq
cheshire.parse
clj-http.client
clj-http.conn-mgr
clj-http.cookies
clj-http.core
clj-http.headers
clj-http.links
clj-http.multipart
clj-http.util
clojure.core
clojure.core.logic
clojure.core.logic.pldb
clojure.core.logic.protocols
clojure.core.protocols
clojure.core.reducers
clojure.core.server
clojure.core.specs.alpha
clojure.data.json
clojure.datafy
clojure.edn
clojure.instant
clojure.java.data
clojure.java.io
clojure.java.shell
clojure.main
clojure.pprint
clojure.reflect
clojure.set
clojure.spec.alpha
clojure.spec.gen.alpha
clojure.stacktrace
clojure.string
clojure.template
clojure.test
clojure.tools.logging
clojure.tools.logging.impl
clojure.tools.nrepl
clojure.tools.nrepl.ack
clojure.tools.nrepl.bencode
clojure.tools.nrepl.middleware
clojure.tools.nrepl.middleware.interruptible-eval
clojure.tools.nrepl.middleware.load-file
clojure.tools.nrepl.middleware.pr-values
clojure.tools.nrepl.middleware.session
clojure.tools.nrepl.misc
clojure.tools.nrepl.server
clojure.tools.nrepl.transport
clojure.tools.reader
clojure.tools.reader.default-data-readers
clojure.tools.reader.edn
clojure.tools.reader.impl.commons
clojure.tools.reader.impl.errors
clojure.tools.reader.impl.inspect
clojure.tools.reader.impl.utils
clojure.tools.reader.reader-types
clojure.uuid
clojure.walk
clojure.xml
cognitect.anomalies
cognitect.transit
datascript.db
flatland.ordered.map
flatland.ordered.set
futon.flexiarg.projection
futon.notions
futon.text
futon1b-evidence
futon1b-gates
futon1b-graph
futon1b-request-executor
futon1b-server
futon1b-text
futon1b-xt
futon2.aif.action-identity
futon2.aif.action-proposer
futon2.aif.belief
futon2.aif.c-fold-config
futon2.aif.cascade-model-manifest
futon2.aif.close-retention
futon2.aif.conditioned-trajectory
futon2.aif.disposition-risk
futon2.aif.evidence-manifest
futon2.aif.exact-belief-core
futon2.aif.fold
futon2.aif.forward-model
futon2.aif.full-loop-cohort
futon2.aif.interoceptive-store-lock
futon2.aif.intrinsic-values
futon2.aif.likelihood-precision
futon2.aif.load-identity
futon2.aif.machine-q
futon2.aif.memory-contract
futon2.aif.mission-registry
futon2.aif.observation
futon2.aif.preference-module
futon2.aif.realized-recording
futon2.aif.repair-obligation
futon2.aif.ruled-outcome-c
futon2.aif.run4-task-pin
futon2.aif.substrate
futon3.gate.auth
futon3.gate.errors
futon3.gate.evidence
futon3.gate.exec
futon3.gate.pattern
futon3.gate.pipeline
futon3.gate.shapes
futon3.gate.task
futon3.gate.util
futon3.gate.validate
futon3.inbox-zero.escalation
futon3.inbox-zero.gates
futon3.inbox-zero.projection
futon3.inbox-zero.promote-exec
futon3.inbox-zero.promote-push
futon3.inbox-zero.promotion
futon3.inbox-zero.state
futon3.inbox-zero.watcher
futon3b.query.relations
futon3b.query.transcript
futon3c.agency.agent-pouch
futon3c.agency.bell-router
futon3c.agency.clock-decision
futon3c.agency.clock-lineage
futon3c.agency.clock-store
futon3c.agency.fed-uplink
futon3c.agency.federation
futon3c.agency.followup-queue
futon3c.agency.frame-seats
futon3c.agency.inbox
futon3c.agency.invariants
futon3c.agency.invoke-activity
futon3c.agency.invoke-controls
futon3c.agency.invoke-ingress-controller
futon3c.agency.job-tree
futon3c.agency.mesh-qa
futon3c.agency.parked-on
futon3c.agency.registry
futon3c.agency.roster-store
futon3c.agency.turn-queue
futon3c.agency.warrant
futon3c.agents.apm-work-queue
futon3c.agents.arse-work-queue
futon3c.agents.codex-activity
futon3c.agents.codex-cli
futon3c.agents.memory-provisioning
futon3c.agents.mfuton-invoke-override
futon3c.agents.mfuton-prompt-override
futon3c.agents.tickle
futon3c.agents.tickle-orchestrate
futon3c.agents.tickle-queue
futon3c.agents.tickle-work-queue
futon3c.agents.zai-api
futon3c.agents.zaif-controller
futon3c.agents.zaif-inputs
futon3c.aif.live-recommendation
futon3c.aif.mission-head
futon3c.aif.observe
futon3c.apm.authority-port
futon3c.apm.bank
futon3c.apm.bank-driver
futon3c.apm.campaign-machine
futon3c.apm.campaign-trace
futon3c.apm.checked-handoff
futon3c.apm.conductor
futon3c.apm.conductor-binding
futon3c.apm.conductor-open
futon3c.apm.conductor-surface
futon3c.apm.cycle-harness
futon3c.apm.durable-coordinator
futon3c.apm.fault-taxonomy
futon3c.apm.frame-cycle-contract
futon3c.apm.generated-contract
futon3c.apm.jit-queue-coordinator
futon3c.apm.job-port
futon3c.apm.job-state
futon3c.apm.library-lane
futon3c.apm.library-lane-adapters
futon3c.apm.library-lane-coordinator
futon3c.apm.library-lane-effects
futon3c.apm.library-lane-launch
futon3c.apm.library-lane-phases
futon3c.apm.library-lane-runner
futon3c.apm.library-loop-checkpoint
futon3c.apm.library-loop-runner
futon3c.apm.live-job-driver
futon3c.apm.live-launch-preparation
futon3c.apm.live-preflight
futon3c.apm.live-preflight-runtime
futon3c.apm.live-proof-phases
futon3c.apm.live-regulator
futon3c.apm.live-solver-rounds
futon3c.apm.memory-access-gate
futon3c.apm.memory-caption-store
futon3c.apm.phase-status
futon3c.apm.preregistration
futon3c.apm.problem-queue-supervisor
futon3c.apm.promotion-pipeline
futon3c.apm.role-memory-search
futon3c.apm.semantic-progress-watchdog
futon3c.apm.solver-shelf-canary
futon3c.apm.toolchain-port
futon3c.apm.typed-role-submission
futon3c.apm.workspace-build
futon3c.apm.workspace-lifecycle
futon3c.blackboard
futon3c.bridge
futon3c.cyder
futon3c.dev
futon3c.dev.agents
futon3c.dev.apm
futon3c.dev.apm-conductor
futon3c.dev.apm-conductor-v2
futon3c.dev.apm-conductor-v3
futon3c.dev.apm-dispatch
futon3c.dev.apm-frames
futon3c.dev.arse
futon3c.dev.bootstrap
futon3c.dev.config
futon3c.dev.ct
futon3c.dev.fm
futon3c.dev.invoke
futon3c.dev.irc
futon3c.dev.mentor
futon3c.dev.mfuton-frontiermath
futon3c.dev.peripheral-agents
futon3c.dispatch-with-recall
futon3c.enrichment.query
futon3c.evidence.backend
futon3c.evidence.boundary
futon3c.evidence.futon1b-backend
futon3c.evidence.invariant
futon3c.evidence.store
futon3c.evidence.subject
futon3c.inbox-zero.followup-validity
futon3c.inbox-zero.sweeper
futon3c.inbox-zero.turn-promotion
futon3c.inbox-zero.witness
futon3c.logic.archaeology
futon3c.logic.arxana-bridge
futon3c.logic.cascade-real
futon3c.logic.cascade-real-live
futon3c.logic.disposition-edn
futon3c.logic.invariant-runner
futon3c.logic.inventory
futon3c.logic.locus
futon3c.logic.mana-session
futon3c.logic.metabolic-balance
futon3c.logic.obligation
futon3c.logic.probe
futon3c.logic.ratchet
futon3c.logic.snapshot
futon3c.marks
futon3c.mfuton-mode
futon3c.mission-control.service
futon3c.peripheral.alfworld
futon3c.peripheral.arse
futon3c.peripheral.chat
futon3c.peripheral.common
futon3c.peripheral.cycle
futon3c.peripheral.deploy
futon3c.peripheral.discipline
futon3c.peripheral.edit
futon3c.peripheral.emacs-cursor
futon3c.peripheral.evidence
futon3c.peripheral.explore
futon3c.peripheral.memory-backend
futon3c.peripheral.memory-lifecycle
futon3c.peripheral.memory-recall
futon3c.peripheral.memory-write
futon3c.peripheral.mentor
futon3c.peripheral.mentor-map
futon3c.peripheral.mission
futon3c.peripheral.mission-backend
futon3c.peripheral.mission-control
futon3c.peripheral.mission-control-backend
futon3c.peripheral.mission-shapes
futon3c.peripheral.night-shift
futon3c.peripheral.night-shift-backend
futon3c.peripheral.night-shift-shapes
futon3c.peripheral.problem
futon3c.peripheral.proof
futon3c.peripheral.proof-backend
futon3c.peripheral.proof-dag
futon3c.peripheral.proof-shapes
futon3c.peripheral.pull-receipts
futon3c.peripheral.real-backend
futon3c.peripheral.reflect
futon3c.peripheral.registry
futon3c.peripheral.runner
futon3c.peripheral.test-runner
futon3c.peripheral.tools
futon3c.portfolio.adjacent
futon3c.portfolio.affect
futon3c.portfolio.core
futon3c.portfolio.heartbeat
futon3c.portfolio.logic
futon3c.portfolio.observe
futon3c.portfolio.perceive
futon3c.portfolio.policy
futon3c.process-watchdog
futon3c.reflection.core
futon3c.reflection.envelope
futon3c.runtime.agents
futon3c.runtime.incidents
futon3c.scripts.mission-scope-ingest
futon3c.social.bells
futon3c.social.coordination-ledger
futon3c.social.dispatch
futon3c.social.mode
futon3c.social.peripheral
futon3c.social.persist
futon3c.social.presence
futon3c.social.shapes
futon3c.social.whistles
futon3c.substrate.client
futon3c.substrate.read-health
futon3c.transport.encyclopedia
futon3c.transport.http
futon3c.transport.irc
futon3c.transport.peripheral-events
futon3c.transport.protocol
futon3c.transport.ws
futon3c.transport.ws.invoke
futon3c.transport.ws.replication
futon3c.util.cwd
futon3c.watcher.commit-ingest
futon3c.watcher.file-ingest
futon3c.watcher.freshness
futon3c.watcher.multi
futon3c.watcher.projections.elisp
futon3c.watcher.projections.essay
futon3c.watcher.projections.flexiarg
futon3c.watcher.projections.python
futon3c.watcher.roots
futon3c.watcher.scope-reingest
futon3c.wm.code-identity
futon3c.wm.guardrails
futon3c.wm.machinery-execution-cohort
futon3c.wm.operator-lane-adapter
futon3c.wm.ordinary-click-budget
futon3c.wm.run4-attempt-admission
futon3c.wm.run4-boot
futon3c.wm.run4-deployment-config
futon3c.wm.run4-deployment-preflight
futon3c.wm.run4-effective-environment
futon3c.wm.run4-execution-cohort
futon3c.wm.run4-historical-action
futon3c.wm.run4-historical-projection
futon3c.wm.run4-historical-successor
futon3c.wm.run4-pinned-run-config
futon3c.wm.run4-realized-recording
futon3c.wm.run4-run-visibility
futon3c.wm.run4-series-controller
futon3c.wm.run4-series-service
futon3c.wm.run4-terminal-evidence
futon3c.wm.run4-terminal-projection
futon3c.wm.run4-trusted-entry
futon3c.wm.runner-service
malli.core
malli.error
malli.impl.regex
malli.impl.util
malli.registry
malli.sci
malli.util
meme.arrow
meme.core
meme.schema
migration.ingest
migration.transform
next.jdbc
next.jdbc.connection
next.jdbc.default-options
next.jdbc.prepare
next.jdbc.protocols
next.jdbc.result-set
next.jdbc.sql-logging
next.jdbc.transaction
org.httpkit.client
org.httpkit.encode
org.httpkit.server
org.httpkit.sni-client
org.httpkit.utils
potemkin
potemkin.collections
potemkin.macros
potemkin.namespaces
potemkin.types
potemkin.utils
potemkin.walk
repl.eval-sandbox
repl.http
riddley.compiler
riddley.walk
ring.middleware.cookies
ring.middleware.keyword-params
ring.middleware.nested-params
ring.middleware.params
ring.middleware.session
ring.middleware.session.memory
ring.middleware.session.store
ring.util.codec
ring.util.io
ring.util.parsing
ring.util.request
ring.util.response
ring.util.time
ring.websocket.protocols
sidecar.store
sidecar.validation
slingshot.slingshot
slingshot.support
user
xtdb.api
xtdb.authn
xtdb.backtick
xtdb.basis
xtdb.error
xtdb.mirrors.time-literals
xtdb.next.jdbc
xtdb.node
xtdb.protocols
xtdb.serde
xtdb.serde.types
xtdb.table
xtdb.time
xtdb.tx-ops
xtdb.types
zai-memory-1b
```

</details>

## Reproduction and retained observation digests

All scratch analysis stayed under `/tmp/deadcode15`; only this report is committed. Re-run the kondo command above and GET namespace snapshot to obtain new current observations; do not treat their results as timeless. The HTTP graph namespace walk uses `type=code/v05/namespace&limit=1000`, repeatedly adding `after=next-cursor`, and checks unique `hx/id`. Per-repo positive call probes use `type=code/v05/calls&repo=LABEL&limit=1`. No whole-corpus call-edge absence claim is made.

- analysis.json SHA256 `31c2667f6cd8bd94f0d7ca5cd4554031afd13645b447006ac284f1f42bfb3c2c`.
- loaded.json SHA256 `7e2fb3e2d4b27b53728c41b2a44caa051516580232308bb9760f61a334ff4d77`.
- namespaces-all.json SHA256 `2f076c4e36dea5f5ad2ab59cbbbdcb08b5ac084f568007823c0e3ec9d9387333`.
- calls-by-repo.json SHA256 `602d698de3f5db32ad66528ca7b436934dc20c964dc5941883be1592cad6f9fc`.
