# README-air — creating and destroying "air"-class agents

**Air agent** = a zai-type Agency agent whose chat backend is an **on-prem
OpenAI-compatible endpoint** (llama.cpp serving a GLM-Air-class MoE) instead
of the Z.ai cloud API. Same adapter (`futon3c.agents.zai-api`), same tools,
same memory seam — different (local, free, slower) model serving.

First instance: `air-1` on Zone (`ams`), 2026-08-06, GLM-4.5-Air UD-Q4_K_XL
under llama.cpp on `:8090`, ~4 tok/s CPU. This file records the programmatic
recipe.

## Prerequisites

1. **A running futon3c Agency JVM** on the box (`:7070`), with Drawbridge
   (`:6768`) and its admin token (`FUTON3C_ADMIN_TOKEN` / `ADMIN_TOKEN` /
   `.admintoken` in the JVM's cwd; the code fallback is `change-me` — set a
   real one).
2. **An OpenAI-compatible server holding the model.** House pattern:

   ```bash
   llama-server -m <model>.gguf --port 8090 --host 127.0.0.1 \
     -c 32768 -t <cores-2> --jinja --reasoning-budget 0 --alias glm-4.5-air
   ```

   `--reasoning-budget 0` matters: GLM thinking-mode otherwise eats the
   completion budget and the adapter's callers see empty content.
   `--alias` is the model name the agent config must match.
3. The Agency's **evidence store** must be live (the adapter refuses to
   build an invoke-fn without it) — it is read from the running system, so
   nothing to do beyond the JVM being healthy.

## Why not the normal registration endpoints?

`POST /api/alpha/agents` and `/agents/auto` thread `model` but **not
`base-url`/`api-key`** into `zai-api/make-invoke-fn` (verified 2026-08-06),
so HTTP-registered zai agents always get the cloud default. Until that gap
is closed, air agents are created via Drawbridge, where the invoke-fn is
built with explicit options.

## Create

```bash
cd ~/code/futon3c   # the JVM's repo (token + drawbridge live here)
AGENT=air-1
BASE_URL=http://127.0.0.1:8090/v1
MODEL=glm-4.5-air

curl -s -H "x-admin-token: $(cat .admintoken)" -H "Content-Type: text/plain" \
  --data-binary @- "http://127.0.0.1:6768/eval" <<CLOJURE
(let [store @futon3c.dev/!evidence-store]
  (futon3c.agency.registry/register-agent!
    {:agent-id {:id/value "${AGENT}" :id/type :continuity}
     :type :zai
     :invoke-fn (futon3c.agents.zai-api/make-invoke-fn
                  {:agent-id "${AGENT}"
                   :session-file "/tmp/futon-zai-session-id-${AGENT}"
                   :evidence-store store
                   :base-url "${BASE_URL}"
                   :api-key "local"
                   :model "${MODEL}"
                   :timeout-ms 900000})
     :capabilities [:explore :edit :test :coordination/execute]
     :metadata {:endpoint "on-prem ${MODEL} @ ${BASE_URL}"
                :registered-by "$(whoami) $(date -I)"}}))
CLOJURE
```

Notes:

- **Name it bare** (`air-1`, not `ams-air-1`). Federation qualifies ids with
  the site prefix on export; registering a pre-qualified id recreates the
  ghost-duplication mess. (Related: names outside the `<type>-<N>` shape
  used to be re-prefixed on every federation hop — `ams-ams-air-1` — fixed
  in futon3c `fdb89a86`, 2026-08-06. Run at least that revision everywhere.)
- `:timeout-ms 900000` (15 min) is CPU allowance: a tool-heavy turn at
  ~4 tok/s is slow. Cloud-speed defaults (300 s) will bite.
- `api-key` can be any non-empty string for llama.cpp; it is also a probe —
  if a completion succeeds with a junk key, you are provably NOT talking to
  the cloud.

## Verify

```bash
# roster
curl -s localhost:7070/api/alpha/agents | python3 -c \
  "import json,sys; print('air-1' in json.load(sys.stdin)['agents'])"

# live round trip (blocks; minutes at CPU speed)
echo "Reply with one short sentence." | \
  python3 ~/code/futon3c/scripts/agency_send.py --from <your-id> --to air-1 --kind whistle
```

Ground truth that the LOCAL server answered: watch the llama-server log for
slot activity during the whistle (`grep "slot launch" <server log>`). Do
NOT trust the model's own answer about which endpoint it runs on — it
cannot introspect that and will confabulate (observed: `air-1` claimed to
be "claude-16 on the Web War Machine endpoint").

## Destroy

```bash
AGENT=air-1
# 1. deregister locally (this also stops federation announcing it)
curl -s -X DELETE http://localhost:7070/api/alpha/agents/${AGENT}
# 2. session hygiene (optional — next create reuses it otherwise)
rm -f /tmp/futon-zai-session-id-${AGENT}
```

Peer sites hold **proxies** (`ams-air-1` elsewhere). After local deletion
they go stale and are pruned by the missed-announce machinery; to remove
them immediately:

```bash
curl -s -X DELETE http://<peer>:7070/api/alpha/agents/ams-${AGENT}
```

## Caveats

- **Registration is JVM-memory only.** An Agency restart forgets the agent;
  re-run the Create block. If an air agent becomes standing infrastructure,
  the durable home for its construction is the JVM's boot path (dev.clj),
  not this recipe.
- **Throughput:** one CPU box = one token budget. An air agent competes
  with anything else using the same llama-server (e.g. mining loops); its
  slot decodes at single-stream speed minus contention. Coordination turns:
  fine. Chatty workloads: use the cloud seat.
- **Model swap** = destroy + create with a different `--alias`/`MODEL`, or
  serve two models on two ports and register two agents. The Agency treats
  them as distinct agents, which is exactly right for A/B comparisons
  (e.g. air vs cloud on the same prompts).
