# Deploying FUTON

This is the application-deployment companion to
[`README-bare-metal.md`](README-bare-metal.md). Bare-metal setup answers “can
this box run FUTON?” Deployment answers the harder question:

> Which exact code, configuration, and data are the serving processes using
> now, and what evidence proves it?

The first concrete route is **laptop → zone-joe**, with Zone treated as
production. The model deliberately says **source** and **target**, not laptop
and Zone. The source may later be Zone or a replacement laptop; the target may
be Lucy, Chicago, or another box without changing the protocol.

This is a design and runbook. Sections marked **not yet implemented** are gates,
not claims that tooling exists.

---

## 1. The deployment invariant

A deployment has four independently drifting states:

| surface | question | insufficient evidence |
|---|---|---|
| source | What did we intend to deploy? | “I committed it” |
| target disk | What bytes reached the target? | “rsync exited zero” |
| serving process | What code and configuration is the live JVM using? | “the files look right” or a fresh JVM passes |
| semantic state | Can the live service retrieve the data and derived indexes the feature requires? | “the XTDB files arrived” |

Deployment is complete only when all four agree with one recorded manifest and
the checks run against the **serving process** pass.

This rule comes from two measured failures on 2026-08-04:

- corrected Clojure was committed and present on disk but never hot-swapped;
  a fresh JVM saw the fix while the live server served stale code for hours;
- memory content reached Zone but `psr_search` could not retrieve it because
  the required index state had not travelled or been rebuilt.

“Committed”, “copied”, “loaded”, and “retrievable” are therefore four different
claims. Do not collapse them into “deployed”.

---

## 2. What a release contains

The deployment unit is a release closure, not a repository and not a directory
copy. Its manifest records:

1. **Source closure.** Every repository and subdirectory reachable through
   `:local/root`, plus non-classpath source consumed by watchers or scripts.
2. **Identity.** Repository commit, branch, and a content digest of the files
   actually packaged. The digest remains meaningful on targets where `.git` is
   intentionally absent.
3. **Dependency locks.** `deps.edn`, `bb.edn`, `lean-toolchain`,
   `lake-manifest.json`, and any other lock or toolchain file in scope.
4. **Runtime definition.** The systemd units, launch scripts, safe environment
   allowlist, ports, site code, JVM options, and expected process topology.
5. **Persistent state.** Store identity, schema/migration level, backup or
   recovery point, and compatibility requirements. Secrets are referenced by
   name and presence/mode, never embedded in the manifest.
6. **Semantic derived state.** Search indexes, watcher projections, or other
   rebuildable structures whose absence changes observable answers. Record
   their watermark and rebuild/verification procedure.
7. **Acceptance probes.** Checks that produce a value unique to the new
   release, evaluated through the actual serving process.

A future machine-readable manifest should be EDN. This sketch is illustrative,
not an implemented schema:

```clojure
{:release/id "20260804T210000Z-329dafad"
 :source/created-at #inst "2026-08-04T21:00:00Z"
 :source/repos [{:path "futon3c"
                 :commit "329dafad7a051f4d84db21217459ffbc9bddb1b9"
                 :tree-sha256 "..."}
                {:path "futon1b" :commit "..." :tree-sha256 "..."}]
 :runtime {:profile :zone
           :site "ams"
           :service "futon3c-zone.service"
           :service-sha256 "..."
           :expected-jvms 1}
 :state [{:name :substrate
          :path "~/.local/share/futon1b/ams-store"
          :schema "..."
          :recovery-point "..."}]
 :derived [{:name :evidence-fts
            :disposition :rebuild-or-catch-up
            :watermark "..."}]
 :verification {:forms-sha256 "..."}}
```

### Clean-source rule

Routine production releases contain committed source. Refuse a release if any
repo in its closure is dirty or if the manifest names a commit that has not
reached the source-of-truth remote. A commit SHA alone is not enough: local-root
repos can be at unrelated commits, so every member of the closure is recorded.

This is stricter than the proof-work transfer described in
`README-bare-metal.md`, which intentionally moves unfinished state. Moving an
operator’s work and deploying production are different operations.

---

## 3. Required layout change on Zone

**Not yet implemented; routine deployment is blocked on this.**

The current Zone launcher defaults to:

```text
FUTON1B_STORE_DIR=$HOME/code/futon1b/ams-store
```

This mixes mutable production data into the code tree. It makes an atomic
release switch impossible: replacing `~/code` would also replace, hide, or
delete the live store. Adding rsync exclusions would preserve bytes but would
not restore global coherence; the release would still be an in-place mixture
of old and new files.

Move mutable state outside the release tree before automating deployments:

```text
~/.local/share/futon1b/ams-store       authoritative XTDB state
~/.local/state/futon3c/                Agency roster and durable queues
~/.local/state/futon-deploy/           manifests and deployment receipts
~/deploy/releases/<release-id>/code/   immutable release trees
~/code -> ~/deploy/releases/<release-id>/code
```

The migration needs its own tested change to `scripts/dev-zone-env`, a store
backup, a stopped-store move, and a boot/retrieval verification. Do not move a
live XTDB store, and do not let two processes open it: XTDB2 is single-process.

Until that migration is complete, source may be copied to Zone for explicitly
supervised development, but call it a **sync/hot-swap**, not a repeatable
production deployment. The receipt must say that activation was non-atomic.

---

## 4. Deployment lifecycle

The lifecycle is **plan → build → stage → activate → prove → record**. A failed
step stops the deployment; later checks do not bless an earlier failure.

### 4.1 Plan and preflight

On the source:

- compute the complete local-root and watcher closure as in
  `README-bare-metal.md`;
- review every dirty repository; routine deployment requires all closure
  members clean;
- choose one release ID and produce the manifest before copying;
- run repo-specific tests, syntax checks, and builds;
- record exact tool versions used to build or validate artifacts.

On the target, collect a baseline before mutation:

```bash
ssh zone-joe '
  set -eu
  hostname
  systemctl --user is-active futon3c-zone.service
  systemctl --user show futon3c-zone.service \
    -p MainPID -p ExecMainStartTimestamp -p NRestarts
  pgrep -af "[c]lojure.main -m futon3c.dev"
  curl -fsS http://127.0.0.1:7070/health | jq .
  curl -fsS http://127.0.0.1:7074/health | jq .'
```

Also establish:

- the current release receipt matches what is serving;
- no production-only edits exist on the target;
- no invoke jobs, migrations, index builds, or writes are in a critical phase;
- there is enough space for a complete staged release and retained predecessor;
- the state backup/recovery point is valid;
- any schema change is backward-compatible with rollback, or rollback is
  explicitly declared unavailable.

If the current serving state cannot be identified, stop. Unknown baseline plus
new deployment produces two unknowns, not one known state.

### 4.2 Build and package

Package from the clean source closure into a fresh staging directory. Exclude
`.git`, caches, corpora, logs, secrets, and mutable stores, but never exclude a
file merely because it is inconveniently large without proving it is outside
the runtime closure.

Generate a sorted content inventory and hash the inventory itself. Verify it
after transfer. The target should not need Git to attest the delivered bytes.

The package operation must fail if a file changes while it is being hashed or
copied. A deploy assembled from an actively edited workspace has no coherent
identity even if every individual copy succeeded.

### 4.3 Stage

Transfer into a new release directory, never into `~/code`:

```text
~/deploy/releases/<release-id>.staging/code
```

On the target:

- recompute and compare the complete content inventory;
- resolve Clojure and Lean dependencies from committed locks without updating
  those locks;
- validate systemd units with `systemd-analyze --user verify`;
- run cold-process tests only as **staging tests**, not as serving evidence;
- allow writes only to declared build/cache paths, then recompute the deployed
  source inventory;
- rename `.staging` to the final release directory only after every staging
  gate passes, and treat the resulting source tree as read-only.

Do not put credentials into a release. The service resolves `.admintoken`, the
Codex/Claude login, and `.zai-key` from stable per-user locations whose presence
and permissions are checked separately.

### 4.4 Choose activation mode

| change | activation | reason |
|---|---|---|
| ordinary function bodies in reload-safe namespaces | controlled Drawbridge `load-file` | preserves live JVM state and redefines vars |
| several dependent namespaces | controlled reload in dependency order, then live probe | one reload does not reload dependencies or dependents |
| route table, captured callback/closure, startup wiring, classpath, dependencies, JVM flags, environment, systemd unit | planned service restart | the old process cannot acquire these changes by redefining vars |
| persistent schema or store layout | explicit migration plan | code activation alone cannot make state compatible |
| search/projection changes | rebuild or catch up plus query oracle | copied primary data does not imply retrievability |

Do not use a restart merely to avoid understanding reload semantics. Equally,
do not hot-reload code whose correctness depends on startup capture.

#### Hot-swap path

Drawbridge is loopback-only on Zone. Run the target release’s own helper over
SSH so the target-local token never leaves the box:

```bash
ssh zone-joe \
  'cd "$HOME/code/futon3c" && bash scripts/proof-eval.sh -' <<'CLJ'
(load-file "/home/joe/code/futon3c/src/the/changed_namespace.clj")
CLJ
```

Never put the token on a shared command line or in the deployment receipt.

The reload result only proves evaluation completed. It does not prove the
request path dereferences the new var.

#### Restart path

Restart only after draining work and recording the old PID. Because the current
agent session may itself be routed through Agency, the restart must be launched
from a separate operator shell or other independent control path. The agent
inside the JVM must never be tasked with restarting its own transport.

After the restart, prove:

- the old PID exited and exactly one new serving JVM exists;
- systemd reports the intended unit and no restart loop;
- ports 7070, 7074, and loopback Drawbridge have the intended owners;
- local agents re-registered over WebSocket;
- federation peers see the `ams-*` identities;
- durable queues and store state recovered.

### 4.5 Prove in the serving process

Fresh-process tests are necessary but cannot close this gate. Use Drawbridge to
resolve each changed var in the live JVM and evaluate a release-specific value
or behavior:

```clojure
{:release "<release-id>"
 :pid (.pid (java.lang.ProcessHandle/current))
 :changed-var-present?
 (boolean (resolve 'the.changed.namespace/the-new-var))
 :new-behavior
 ((resolve 'the.changed.namespace/release-probe))}
```

Prefer behavioral witnesses over namespace presence. A var may be loaded while
the live handler still holds an old captured function. Exercise the same HTTP,
Agency, memory, or proof route that users will call and require an answer unique
to the new release.

The proposed `boot-context :namespace-drift` detector in
`futon3c/holes/labs/M-loud-failure/hot-swap-drift-detector.md` is **not yet
implemented**. It should be built before deploys become routine. Its job is to
make disk-vs-loaded drift loud; it must not auto-reload. Even once built, a zero
drift count complements rather than replaces the behavioral witness.

### 4.6 Prove semantic state

For every deployed data-backed capability, test both primary storage and the
actual retrieval path. For memory this means at least:

- retrieve a known record by identity from the serving store;
- retrieve the same record through `memory_search`;
- retrieve a known PSR through `psr_search` using a query that depends on its
  index;
- compare index watermark/count with the primary store’s stable census;
- after a rebuild, wait for replay/catch-up completion before judging a low
  count as data loss.

An index may be disposable in the recovery sense while still being mandatory
for correct service. “Rebuildable” does not mean “optional at deploy time”. The
manifest must say whether each derived structure is transferred, rebuilt, or
caught up, and the release gate must verify the resulting query behavior.

For Agency, exercise a real local invoke and confirm the whole route:

```text
source operator → Zone Agency → ams agent → durable job result
                         ↓
                federation roster/status
```

Health endpoints alone do not prove invocation, result delivery, or remote HUD
propagation.

### 4.7 Record the receipt

Write an append-only deployment receipt outside the release tree containing:

- operator, source host, target host, start/end time;
- previous and new release IDs and manifest hashes;
- activation mode and changed namespaces;
- old/new PID and process start time;
- state recovery point and migration/index actions;
- exact staging, serving-process, retrieval, and federation probe results;
- rollback availability and deadline;
- outcome: `succeeded`, `rolled-back`, or `failed`.

“Succeeded with verification pending” is not a successful deployment.

---

## 5. Rollback

Keep the previous immutable release and its manifest. Rollback switches code to
that release and repeats the same activation and serving-process proof; changing
the symlink alone does not change an already-loaded JVM.

Rollback is valid only when persistent state remains compatible. Before a
forward-only migration, write in the receipt that application rollback is not
available and specify restore/recovery instead. Never run old code against new
state merely to get the service green.

If hot-swap verification fails, reload the prior release’s namespace files in
dependency order and prove the old behavior in the serving JVM. If restart
activation fails, restore the previous release pointer through the independent
operator session and start it once. Repeated blind restarts destroy evidence and
can compound state damage.

---

## 6. Minimal laptop → Zone acceptance gate

A routine deployment is accepted only when all of these are true:

- [ ] complete multi-repo release manifest created from clean commits
- [ ] source tests/builds pass
- [ ] staged target inventory equals the manifest
- [ ] persistent state is outside the release tree and has a recovery point
- [ ] old serving PID and baseline release are known
- [ ] activation mode is justified per changed surface
- [ ] exactly one intended serving JVM exists after activation
- [ ] live JVM resolves every changed var and returns a new-release witness
- [ ] HTTP/Agency behavior unique to the release passes
- [ ] primary memory, `memory_search`, and `psr_search` probes pass
- [ ] index watermark/census is coherent after catch-up
- [ ] `ams-*` agents reconnect and a real invoke returns a durable result
- [ ] London/Chicago and the operator surface observe the intended federation state
- [ ] receipt written; previous release retained; rollback status explicit

Anything less is a sync, a partial activation, or an experiment—not a completed
production deployment.

---

## 7. Implementation order

The safest path to an executable deploy command is:

1. Move Zone’s XTDB store outside `~/code` and verify backup, restart, search,
   and index catch-up behavior.
2. Implement the `:namespace-drift` serving-process instrument.
3. Define and validate the EDN release-manifest schema, including multi-repo
   closure and derived-state entries.
4. Build read-only `plan` and `verify` commands first. They should describe the
   current source, disk, process, and state divergence without changing it.
5. Add package/stage and content-inventory verification.
6. Add explicit hot-swap activation with per-namespace live witnesses.
7. Add versioned-release restart activation and rollback only after the state
   path is independent.
8. Make the deployment receipt append-only and surface the active release in
   `boot-context` and health/orientation output.

That order addresses the expensive failure first: before making deployment
easy, make it impossible for “what is actually running?” to remain unanswered.
