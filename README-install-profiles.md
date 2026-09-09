# Installation profiles and Linode observations

Read-only SSH survey, 2026-09-09, following Joe's request to support smaller
machines and use Apollo as the installation staging account. These are observed
configurations, not evidence that the latest stack is qualified on every host.

## What is actually on the Linodes

| SSH host | OS | RAM MiB | Available MiB | Swap used MiB | Observed runtime |
|---|---|---:|---:|---:|---|
| `metameso` (Chicago) | Ubuntu 20.04.1 | 3898 | 1111 | 2001 | futon1b and futon3c running |
| `lucy-joe` (London) | Ubuntu 24.04.3 | 7941 | 7031 | 0 | No Java processes visible; no loaded futon user units |
| `hyperreal` | Ubuntu 18.04.6 | 1992 | 1226 | 115 | No Java processes or `~/code/futon1b`, `~/code/futon3c` found by this survey |

The survey used `free`, `ss`, `/proc`, selected non-secret configuration lines,
`systemctl --user list-units`, and `jcmd VM.flags`. It made no service changes.
Old OS versions are observations, not proposed support targets.

Chicago's futon1b process (PID 242786 at inspection) has **1536 MiB heap and
768 MiB direct memory**, confirmed in both `/proc` and `jcmd`. Its RSS was
approximately 1470 MiB, with 498 MiB swapped. The active user unit runs
`~/code/futon1b`, store `chicago-store`, API port 7074. Its checkout HEAD was
`c9c40f4`; its `:server` alias contains those smaller limits.

Chicago's Agency process (PID 3686193) has **no explicit `-Xmx` on its command
line**. `jcmd` reports `MaxHeapSize=1023410176` (976 MiB); no explicit direct-memory
limit was reported. RSS was approximately 554 MiB, with 98 MiB swapped. Its
actual working directory is `~/code/futon3c-agency-fixes-cpf`, not the canonical
`~/code/futon3c` whose HEAD is `b47ceaa1`. The survey does not establish a loaded
source identity for that JVM. Do not copy this launch arrangement as the new
installation design.

Lucy now has approximately 8 GiB. Its futon1b checkout (`8168ba8`) has **4096 MiB
heap and 1536 MiB direct** in `deps.edn`; the installed unit selects `:server`.
Its futon3c checkout (`accd71d9`) has Makefile defaults **1536 MiB heap and
640 MiB direct**, while `dev-zone-env` still defaults to **12 GiB and 4 GiB**.
Those are different entry points, not simultaneously observed runtime settings.
The installed files alone cannot prove which configuration last served traffic.

## Profiles to qualify

The machine-readable authority for this first planning tool is
[`config/install-profiles.json`](config/install-profiles.json). Sizes are MiB.
Every profile is marked `qualification-pending`.

| Profile | Agency heap/direct | Local substrate heap/direct | Extra reserve | Planned total | Purpose |
|---|---:|---:|---:|---:|---|
| `agency-4g` | 1536 / 640 | external | 1536 | 3712 | Small coordination host using a separately provisioned substrate |
| `compact-8g` | 1536 / 640 | 1536 / 768 | 2048 | 6528 | First complete headless installation trial |
| `rebuild-16g` | 1536 / 640 | 4096 / 3072 | 3072 | 12416 | Test the substrate's larger rebuild budget |
| `zone-32g` | 12288 / 4096 | 4096 / 3072 | 6144 | 29696 | Preserve the current Zone ceilings as an optional large profile |

The reserve covers planning headroom for OS, JVM/native overhead and limited
agent activity; it is not a measured maximum. Neither heap nor direct-memory
ceilings bound total process RSS. Swap is not counted toward capacity. The small
profiles assume one agent at a time and no concurrent local Lean/frontend builds;
launchers still need to enforce that workload policy. `agency-4g` is explicitly
not the entire stack on one machine.

A **complete current-source 4 GiB profile remains a qualification question**.
Chicago proves smaller deployments exist, but the observed swap use, different
Agency checkout and missing explicit memory cap prevent calling it a reproducible
profile. The current compact pair's planning budget already exceeds 4 GiB. Test
an intentionally budgeted topology and representative workloads before adding
that profile; do not reduce buffers until a boot merely happens to succeed.

Host sizing must be selected without editing shared `deps.edn` for each machine.
futon3c already has host-selectable heap/direct settings. futon1b still bakes its
server budget into `:server`. Separate its correctness flags from host sizing in
the runtime implementation, then assert effective values using `jcmd VM.flags`.
Do not assume an earlier `-J` option overrides a later alias option. Preserve
Arrow access flags, native allocation controls, store exclusivity and index
correctness when adding small-machine profiles.

## Runnable planning and diagnosis

From futon0:

```bash
python3 scripts/install-plan.py plan --user apollo --profile compact-8g
python3 scripts/install-plan.py doctor --user apollo --profile compact-8g
python3 scripts/install-plan.py plan --profile agency-4g \
  --substrate-url https://YOUR-SUBSTRATE-HOST
```

The commands emit JSON; they do not write files or start anything. `plan` exits
zero when it can construct a valid proposal. `doctor` returns **2 while release
gates remain unresolved**, even if host checks pass; `launch_ready` remains false.
It checks host available RAM, prospective loopback binds, existing destinations,
target identity and tool presence. It reports free disk without claiming a
qualified disk requirement. Tool-version checks, cgroup limits, runtime identity,
remote substrate connectivity, complete optional port enumeration, and source/data
closure are still subsequent work. A successful port probe does not reserve a port.

Profiles are proposals only: no command yet applies the budgets to a JVM.
The `--agency-port`, `--substrate-port`, `--substrate-health-port` and
`--drawbridge-port` options let a single plan derive the relevant URLs. Duplicate
ports, privileged ports and invalid instance names are rejected. External
substrate URLs must not contain credentials; credential provisioning is separate.

## Apollo staging

Joe has authorized Apollo as the trial account. Plan a new, isolated installation
at `/home/apollo/futon-install/trial/code`; leave the existing
`/home/apollo/code/futon1b` and its `staging-store` intact. Proposed state/config
paths are under Apollo's `.local/share/futon/trial`, `.local/state/futon/trial`
and `.config/futon/trial`. The plan defaults to loopback 7270/7272/7273/6968.

Account access is resolved. `sudo -n -l` shows `(root) NOPASSWD: ALL` for Joe.
Direct `sudo -u apollo` is not covered by that run-as rule, but
`sudo -n runuser -u apollo -- COMMAND` works through the authorized root account.
Use this route for Apollo-owned staging work; Joe still cannot write directly
into Apollo's home.

The planning script and profile catalog are staged, owned by Apollo, under
`/home/apollo/futon-install/tools/{scripts,config}`. Running `doctor` as Apollo
passed its host, identity, tool-presence and prospective port checks. It still
returns 2 and `launch_ready: false` for the outstanding release/runtime gates.
The subsequent [application trial](README-apollo-trial.md) fetched the public
repositories and ran dependency, HTTP and search checks. Agency loading failed
on a public-source mismatch; persistent services have not been started.

Beyond ports, activation needs separate store/index ownership, identities,
credentials, queues and source provenance. Existing process guards must be changed
to enforce the authorized instance topology before services are launched; omitting
them would discard the protection against loading or stopping the wrong instance.
No guards or live services were changed in this survey.

Qualification should run in this order: enforce the selected resource budget;
verify effective JVM flags; cold boot with a public fixture; write/read/search;
one agent task; restart and recover; rebuild when the profile claims that workload;
record peak RSS, cgroup memory events, swap and latency. This provides the evidence
for publishing a small-machine installation rather than relying on host labels.
