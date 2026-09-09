# Public installation readiness

Initial investigation, 2026-09-09, for a collaborator installing FUTON on Ubuntu.
This is an implementation brief, not a working installer or a claim that a fresh
installation has passed. No services were started, stopped, or reconfigured.

The August migration provides useful toolchain and dependency guidance, but its
commands cannot yet be handed to a new user unchanged. The principal remaining
work is a public source/data manifest, portable runtime configuration, and
repeatable installation and removal with serving-process verification.

Follow-up: the [Apollo application trial](README-apollo-trial.md) now records
anonymous source installation and actual validation results, including the
public-release mismatch blocking Agency loading. The
[clean Linode plan](README-clean-linode.md) covers the proposed fresh OS trial.

## Existing material and discrepancies

| Material | Reuse | Correction needed |
|---|---|---|
| [Bare-metal runbook](README-bare-metal.md) | Ubuntu 24.04 experience; Java/Clojure/Babashka/clj-kondo setup; transitive local dependency discovery | Agency section still says `-M:dev`, 13 directories, embedded futon1b and port 7074. Current alias is `:dev-serve`; the conservative scan finds 14 directories; current host runs separate services. Its rsync exclusions and transfer of operator work do not define a public release. |
| [Deployment design](README-deploy.md) | Source, disk, process and retrieval must agree; manifest, receipts, rollback and state separation | Explicitly a design with unimplemented gates; store example is older than today's `migration-store-21`. |
| [Setup notes](README-setup.md) | Historical MUSN setup | Starts with nginx/MUSN; this host serves the public site through Caddy. It is not the current installation entry point. |
| [Zone launcher](../futon3c/scripts/dev-zone-env) | Existing configuration variables and agent/service wiring | Host IP, federation peers, agent executable paths, automatic registrations, promotion policy and mutable paths are operator-specific. It says standalone futon1b but still refuses startup when `futon1b-server.service` is active. Resolve that contradiction rather than removing the check in an installer. |
| [JVM provenance ticket](../futon3c/holes/tickets/T-jvm-provenance-before-multi-jvm.md) | Earlier Apollo/Dionysus isolation and reproducible-run proposal | Still open; live `GET /api/alpha/provenance` returned HTTP 404 on September 9. |

Source inspection anchors: futon0 `72e72cc`, futon3c `0f4ec840`, futon1b
`2ef1886`. These identify checkout HEADs, not a clean release or loaded code.

## Public repository inventory

An unauthenticated GitHub API request verified `visibility: public` for every
repository below on September 9. All URLs are under
`https://github.com/tothedarktowercame/`; use HTTPS clones without Joe's GitHub
credentials. Pin a reviewed commit per repository, not whichever default branch
happens to be current when installing.

| Repository | Default branch reported by GitHub | Dependency directory/directories |
|---|---|---|
| futon0 | main | `futon0` |
| futon1 | main | `futon1/apps/common`, `nlp-interface`, `graph-memory`, `charon` |
| futon1b | master | `futon1b` |
| futon2 | main | `futon2`, `futon2/web/war-machine` |
| futon3 | main | `futon3/inbox-zero-lib` |
| futon3a | main | `futon3a` |
| futon3b | main | `futon3b` |
| futon3c | master | `futon3c` |
| futon4 | main | `futon4/dev/web/webarxana` |
| futon5 | main | `futon5` |

This is a conservative recursive text scan of `:local/root` entries starting at
futon3c, including aliases: **14 directories in ten repositories**. It is not
yet an EDN/alias-aware release resolver or a cold classpath validation. Public
availability was checked independently of local SSH remotes. Remote content at
the final pinned commits still needs validation; local files can include work
that has not reached those commits.

`futon1a` and `futon6` also returned public. They are outside that scanned
classpath closure, but may be needed by selected features and source watchers.
Unauthenticated requests for `futon5a`, `futon7`, and `futon7a` returned 404:
exclude them from the public clone list. A 404 establishes anonymous
unavailability, not whether a repository is private or absent.

The closure alone does not cover the whole application. For example,
`futon3c/src/futon3c/aif/stack_generator.clj` and parts of `transport/http.clj`
read futon5a data, including absolute `/home/joe/code` paths. The bare-metal
runbook also requires a broader source-watcher inventory. Audit these consumers
before advertising the entire public stack as complete. A missing private input
must produce an explicit unsupported capability or a public replacement contract,
not fabricated data or silently successful empty output.

GitHub returned no detected license for these repositories. This is not proof
that license files are absent; review distribution terms and bundled material
before publishing a release. Do not copy private stories, stores, agent sessions,
credentials, or the working directory wholesale into the install bundle.

## Ubuntu, packages and capacity

Use **Ubuntu 24.04 LTS on x86-64 as the first validation target**, matching the
August migration and this running host. This is an observed platform, not a
cross-version support claim. Ubuntu's own installation baseline is documented
[here](https://ubuntu.com/server/docs/tutorial/basic-installation/); it does not
establish FUTON's memory requirements.

Candidate prerequisites to validate in a fresh installation:

- Base: `git`, `curl`, `ca-certificates`, `rsync`, `jq`, `python3`, `unzip`,
  `zstd`, `ripgrep`, `time`, `make`, and `openjdk-21-jdk-headless`.
- Install Clojure CLI, Babashka and clj-kondo separately, with recorded versions
  and verified artifacts. The migration runbook has the existing procedures.
- systemd user services for lifecycle management; Emacs for the operator UI and
  the required `futon4/dev/check-parens.el` development gate.
- Web UI builds: Node/npm, using the selected applications' lockfiles; discover
  and pin compatible versions instead of inheriting Joe's NVM directory.
- Agent use: the collaborator's own provider account/credentials and executable
  configuration. Verify basic storage and HTTP separately from paid agent calls.
- Proof work: a separately declared Elan/Lean/Lake profile, pinned toolchains and
  Mathlib packages, plus the build tools specified in the bare-metal runbook.
  Do not require GPU tooling for the initial coordination/storage installation.
- Caddy/domain/TLS only for a published web surface. A local installation should
  have an explicit loopback address plan before public ingress is configured.

No measured full-stack minimum exists in the material inspected. The current
zone launcher budgets futon3c **12 GiB heap + 4 GiB direct memory**; futon1b's
`:server` alias budgets **4 GiB + 3 GiB**. Those ceilings total **23 GiB** before
metaspace, native allocations, agents, compilers and the OS. They are not idle RSS
or a guarantee that the process cannot use more memory. Preserve the documented
`MALLOC_ARENA_MAX=2` setting; see [futon1b memory evidence](../futon1b/README.md#memory-requirements).

Installation should use explicit memory profiles, not a universal 32 GiB target.
The follow-up [SSH survey and profile plan](README-install-profiles.md) records a
currently running two-service installation on the roughly 4 GiB Chicago host,
with older source and substantial swap use. The first current-source complete
installation candidate is `compact-8g`; a separate `agency-4g` profile requires
an external substrate. All profiles still need workload qualification. Measure
idle, one-agent work, restart/recovery and index rebuild under enforced budgets
before claiming supported capacity. Concurrent Lean work requires its own budget.

Disk requirement is also unqualified: measure public checkout, dependency cache,
build outputs, fresh store, indexes and one retained release/backup separately.
futon4's GitHub repository-size field alone is approximately 1.1 GiB of reported
repository storage; that is not an installed-size measurement. Avoid inheriting
large historical corpora merely because they happen to be on this server.

## Apollo feasibility and port isolation

Read-only observations on September 9:

- `apollo` exists, UID 1002, home `/home/apollo`; account description is
  `futon1b staging (xtdb 2.2.0 trial)`.
- `/home/apollo/code/futon1b` already exists. Treat it as previous work requiring
  inventory, not an empty install destination. The earlier ticket's empty-home
  observation no longer describes the filesystem.
- Two Java processes were visible, both owned by Joe, with approximately 7.4 and
  2.8 GiB RSS. No Apollo Java process was visible in that census.
- Host RAM: 249 GiB total, approximately 231 GiB available. Shared filesystem:
  approximately 211 GiB free, 89% used. This is ample RAM for investigation but
  disk growth and retention still need budgeting.
- TCP listeners include 7070 (Agency), 7072/7073 (substrate surfaces), 6768
  (loopback Drawbridge), 8081 (voice), and Caddy 80/443/2019, plus SSH, mail,
  DNS and editor-related ports. Another Unix account shares this network namespace.

Proposed loopback ports for a future Apollo trial: Agency **7270**, futon1b
health **7272**, futon1b API **7273**, Drawbridge **6968**. These were absent from
the observed listener list; they are neither reserved nor fully validated. Prior
Apollo staging used 7273, so inspect its existing configuration first. Preflight
must check all listeners again immediately before activation and fail on conflict.
Also inventory IRC, WebArxana, War Machine and shadow-cljs HTTP/nREPL ports.

Use one instance configuration to derive both bind ports and every caller URL,
including `FUTON3C_SELF_URL`, Agency base, substrate aliases and followup URLs.
Give the instance its own site identity, tokens, roster, queues, indexes, store,
logs and build outputs. Do not inherit Joe's federation peers. A new store must
be initialized and queried through the actual instance, never pointed at Joe's
`migration-store-21`.

**Joe authorized Apollo staging in the follow-up on September 9.** The existing
`server-guard` still counts futon3c processes across users, so the implementation
must be reconciled with the authorized topology before startup. The prior
provenance ticket supplies the structural requirement: declared source identities
and tracked reloads per process. Validate instance-aware ownership checks before
launching a second stack; do not omit guards to make the trial start. Account
access is resolved: Joe has `(root) NOPASSWD: ALL`, and
`sudo -n runuser -u apollo -- COMMAND` runs staging commands as Apollo.

## Implementation sequence and acceptance

1. Produce an explicit public feature/source/data manifest. Resolve local roots
   with selected aliases; enumerate watcher, subprocess, story and generated-asset
   inputs. Fail for undeclared/unavailable required inputs. Choose public starter
   data and record exact source commits without including Joe's personal state.
2. Implement read-only `plan` and `doctor`: package/tool versions, source identity,
   available disk/RAM, paths, all ports and URL consistency, credentials by presence
   only, store ownership, and declared process topology. Keep diagnosis runnable
   even when the application cannot boot.
3. Reconcile runtime configuration structurally: account-relative paths, external
   mutable state, service dependencies and the stale embedded-store guard. Complete
   provenance and the instance-policy change before Apollo activation.
4. Build `fetch`/`configure`/`start`/`verify`/`stop`/`uninstall` against the same
   manifest. Anonymous fetch must work without Joe's caches or credentials.
   Repeated configure must preserve local credentials and state. Store initialization
   must refuse existing unrelated stores. Stop must target exact units/processes,
   never a global `pkill`.
5. Validate from a fresh Ubuntu environment: resolve dependencies, boot both
   services, inspect source provenance, write/retrieve/search a public fixture,
   register an agent over WebSocket, then run one explicitly configured provider
   interaction and retain its result. Stop/start must recover the fixture and
   durable state; successful `/health` alone is insufficient.
6. Exercise teardown/reinstall: remove only installation-owned services and code;
   preserve data by default and require an explicit separately scoped purge for
   deletion. Do not remove system-wide packages or alter Caddy's existing site.
   Verify Joe's listeners and service identities before and after an Apollo trial.
7. Record the tested resource envelope and receipts, then write the short supported
   installation guide. A bootstrap script should not claim production deployment
   until the [deployment design](README-deploy.md)'s applicable gates pass.

## First walkthrough, after installation works

The useful demonstration is a traceable cycle: inspect a mission and its evidence,
ask an agent to do a small task, retain its result, then retrieve that result in a
later session. That shows why coordination, persistent memory and the system map
belong together. Each step needs a public fixture and an observable outcome.

The existing [cascade page](https://zone.hyperreal.enterprises/wip/pipeline-pattern-cascade.html)
can guide the selection of mission clusters. Its
[publisher](../futon3c/scripts/publish-cascade-snapshot.sh) fetches live endpoints
and inlines their data into a static snapshot for Caddy. It is not a live browser
connection to the stack; a new empty installation will not reproduce Joe's graph
automatically. Keep source dates, unavailable sections and demonstrated outcomes
visible rather than treating completed-mission labels as fresh-install acceptance.

[VSATARCS](../futon4/README-vsatarcs.md) is particularly concrete follow-on work:
its reader is public but its default anthology lives in the anonymously unavailable
`futon5a/holes/stories`. Publish reviewed starter stories in a public repository,
configure the reader to use them, and test the landing page and cross-story links.
Refreshing the prose alone will not make that walkthrough installable.
