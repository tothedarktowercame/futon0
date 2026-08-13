# Standing up a compute box for futon experiments

Repeatable setup and teardown for running futon batch experiments on a fresh remote
machine. Written 2026-08-03 while standing up `zone-joe` (AMD EPYC 4545P, Ubuntu 24.04);
the "bare metal" part matters less than the repeatability.

**Why bother:** the local box runs one experiment at a time and the Agency job cap is
~30 minutes. A 5,400-run grid takes ~6.9 wall-hours single-threaded. On 16 cores it is
under an hour. Every experiment in `M-exotype-xenotype-eoc` that got cut down to fit
(Slice 5 dropped from N=100 to N=4 seeds) was cut for want of exactly this.

---

## 0. Probe before you install

Do this first. It takes 30 seconds and it decides the whole approach.

```bash
ssh -o BatchMode=yes zone-joe '
  hostname; nproc; grep MemTotal /proc/meminfo
  lscpu | grep -E "Model name|Core\(s\)|Thread"
  systemd-detect-virt || echo bare-metal
  cat /etc/os-release | grep PRETTY_NAME
  echo -n "sudo: "; sudo -n true 2>/dev/null && echo passwordless || echo "needs password"
  df -h /home | tail -1
  env | grep -i proxy || echo "no proxy set"
  for u in https://archive.ubuntu.com https://download.clojure.org \
           https://repo1.maven.org https://github.com; do
    printf "%-32s " "$u"; curl -sS -o /dev/null -w "%{http_code}\n" "$u"
  done'
```

**Read `nproc` as THREADS, not cores.** A 16-core EPYC 4545P reports 32. Reporting that
back as "more cores than you specified" is wrong and confusing — check `lscpu` for
`Core(s) per socket`.

**Check RAM against what was ordered.** On zone-joe `MemTotal` came to 123.4 GiB against
a commissioned 256 GB. `sudo dmidecode -t memory | grep -E "Size:|Locator:|Maximum"`
showed **2 × 64 GB installed, 2 slots empty, `Maximum Capacity: 128 GB`** — i.e. the
board caps at half the ordered amount, so it was not a missing-sticks problem but an
undeliverable spec. Worth raising with the provider; `dmidecode` needs root.

---

## 1. sudo

For these experiments root is needed **once**, for package installation. Two honest
options:

- **No sudo at all.** JDK as a user-local Temurin tarball, Clojure CLI with
  `--prefix ~/.local`. Nothing system-wide changes. Sufficient if the box only runs
  batch Clojure.
- **Full passwordless sudo:**
  ```bash
  echo 'joe ALL=(root) NOPASSWD: ALL' | sudo tee /etc/sudoers.d/90-joe-nopasswd
  sudo chmod 0440 /etc/sudoers.d/90-joe-nopasswd
  sudo visudo -c        # ALWAYS validate before logging out
  ```

**Do not bother scoping NOPASSWD to `apt-get` and calling it safer.** It is not a
security boundary: apt installs packages whose maintainer scripts run arbitrary
commands as root. Choose between "no sudo" and "full sudo"; the middle is theatre.

---

## 2. Toolchain

Install the **base profile** on every FUTON compute box. Add the JVM or Lean
profile below according to the work the box will run; do not install a project's
entire historical dependency universe merely because it is mentioned somewhere in
the repository.

```bash
ssh zone-joe '
  sudo env DEBIAN_FRONTEND=noninteractive apt-get update -qq
  sudo env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
    git rsync curl ca-certificates time

  # JVM/Clojure experiment profile (futon5)
  sudo env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
    openjdk-21-jdk-headless dmidecode imagemagick

  # Lean proof profile (futon6 + apm-lean)
  sudo env DEBIAN_FRONTEND=noninteractive apt-get install -y -qq \
    build-essential python3 python3-venv python3-pip jq zstd unzip ripgrep'
```

The environment assignment belongs **after** `sudo`; otherwise sudo may discard it
and package configuration will still try to open a dialog on a non-interactive SSH
connection.

On 2026-08-04, `archive.ubuntu.com` accepted a TCP connection from zone-joe but
stalled transfers (APT made no progress for several minutes and a 12-second direct
`curl` probe timed out). `mirrors.edge.kernel.org` returned the same Ubuntu-signed
Noble metadata at normal speed. Preserve the original source and switch mirrors;
this changes transport, not package provenance or signature verification:

```bash
ssh zone-joe '
  set -eu
  curl -fsSL --max-time 12 -o /dev/null \
    https://mirrors.edge.kernel.org/ubuntu/dists/noble/InRelease
  sudo cp --update=none /etc/apt/sources.list \
    /etc/apt/sources.list.pre-futon-zone-joe
  sudo sed -i \
    "s|http://archive.ubuntu.com/ubuntu/|https://mirrors.edge.kernel.org/ubuntu/|g" \
    /etc/apt/sources.list
  sudo apt-get -o Acquire::Retries=2 -o Acquire::https::Timeout=30 update'
```

Do not switch mirrors merely to silence an APT error. First probe both endpoints;
if the mirror cannot deliver the signed release metadata, stop.

**`imagemagick` is not optional and it fails LATE.** The experiment drivers shell out to
`convert` to turn `.ppm` output into `.png`. Nothing detects its absence until the
rendering step, which runs *after* the whole grid — so on 2026-08-03 a 5,212-run grid
computed for ~45 minutes and then died with

```
Execution error (IOException) at java.lang.ProcessImpl/forkAndExec
Exec failed, error: 2 (No such file or directory)
```

which names neither `convert` nor the missing binary. Only the checkpoint's
resumability saved the run. `time` is likewise absent by default on Ubuntu server:
`/usr/bin/time -f ...` fails while the bash builtin still works, which is confusing in
timing scripts — use `date +%s` arithmetic if you would rather not install it.

**Check the full set of external binaries a driver shells out to before a long run:**

```bash
grep -rhoE 'sh/sh "[a-z0-9-]+"' scripts/ | sort -u   # locally: what gets exec'd
ssh zone-joe 'for c in convert java clojure bb git; do printf "%-10s " $c; command -v $c || echo MISSING; done'
```

### Clojure CLI — expect `download.clojure.org` to be blocked

On zone-joe every host returned 200 **except** `download.clojure.org`, which returned
**403** with no proxy configured. The official install instructions therefore fail. Get
the identical installer from GitHub instead:

```bash
ssh zone-joe '
  VER=$(curl -sS https://api.github.com/repos/clojure/brew-install/releases/latest \
        | grep -m1 "\"tag_name\"" | sed -E "s/.*\"([0-9.]+)\".*/\1/")
  cd /tmp && curl -sSLO "https://github.com/clojure/brew-install/releases/download/${VER}/linux-install.sh"
  chmod +x linux-install.sh && sudo ./linux-install.sh
  clojure --version'
```

A `curl: (23) Failure writing output to destination` from inside the installer is
harmless — it still completes. Verify with `clojure --version`.

### Babashka is a SEPARATE install and you need it

Easy to miss, because `clojure` working makes the box look ready. It is not: `futon5`
has its own **`bb.edn`** (with a `commons-math3` dep and the `org.babashka/go-sqlite3`
pod), and a number of `futon5/scripts/*.clj` are babashka scripts rather than JVM
Clojure. Installing only the Clojure CLI leaves those silently unrunnable.

```bash
ssh zone-joe '
  cd /tmp && curl -sSLO https://raw.githubusercontent.com/babashka/babashka/master/install
  chmod +x install && sudo ./install
  bb --version'
```

Pulls from GitHub releases, so the `download.clojure.org` 403 does not affect it.

Two things to check afterwards, because bb fails differently from `clojure`:

- **`bb.edn` deps and pods resolve separately** from `deps.edn`. Run `bb -e '(+ 1 1)'`
  from inside `futon5/` so the `bb.edn` deps and the sqlite pod are actually fetched;
  pods download on first use and will otherwise fail mid-run.
- **`bb.edn` `:paths` may differ from `deps.edn` `:paths`** (futon5's bb.edn uses
  `["." "bb" "src" "resources"]`), so a script can resolve under one and not the other.

Rule of thumb: bb for orchestration (partitioning, merging, monitoring — ~10 ms
startup), JVM Clojure for the experiments themselves (they need the full dependency
graph). Do not assume one covers the other.

**Pin the version if bb touches results.** The installer takes whatever is latest, so
boxes drift apart: on 2026-08-03 the local box had **bb 1.12.208** and the freshly
installed zone-joe had **1.13.219**. Harmless for orchestration, NOT harmless if a bb
script computes anything that lands in an artifact — that silently breaks the §4
byte-identical guarantee across machines. To pin:

```bash
sudo ./install --version 1.12.208
```

Same applies to the JDK. Record both versions alongside any result set:

```bash
ssh zone-joe 'java -version 2>&1 | head -1; clojure --version; bb --version'
# 2026-08-03 zone-joe: openjdk 21.0.11 / Clojure CLI 1.12.5.1664 / babashka 1.13.219
```

### Lean proof profile — Elan, Lean, Lake, and mathlib

There are two separate pins. **Elan** manages toolchains and is pinned in the box
manifest; **Lean** is selected by the repository's `lean-toolchain`. As of
2026-08-04 the matching local setup is:

```
Elan 4.2.1
Lean 4.29.0-rc8
Lake 5.0.0-src+513160e
mathlib 5ee50502f950564a1bc3f4a0bb7809b94e65a49a
```

Install Elan as the ordinary user, not root. Pinning Elan avoids the same silent
box-to-box drift described for Babashka above:

```bash
ssh zone-joe '
  set -eu
  cd /tmp
  curl -fsSLO https://github.com/leanprover/elan/releases/download/v4.2.1/elan-x86_64-unknown-linux-gnu.tar.gz
  tar -xzf elan-x86_64-unknown-linux-gnu.tar.gz
  ./elan-init -y --default-toolchain none
  "$HOME/.elan/bin/elan" --version'
```

Do not install a distro `lean` package and do not set a global Lean version to
stand in for the project pin. From inside `apm-lean`, Elan reads
`lean-toolchain` (`leanprover/lean4:v4.29.0-rc8`). Lake then reads the committed
`lake-manifest.json`, whose resolved mathlib revision is the hash above. In
particular, **do not run `lake update` while deploying**: that rewrites the lock
and turns setup into a dependency upgrade.

After syncing the repos (§3), fetch the locked dependencies and precompiled
mathlib cache, then build:

```bash
ssh zone-joe '
  set -eu
  export PATH="$HOME/.elan/bin:$PATH"
  cd "$HOME/code/apm-lean"
  elan toolchain install "$(cat lean-toolchain)"
  lake exe cache get
  lake build'
```

`lake exe cache get` needs `curl`, `git`, and `zstd`; compiling cache misses needs
the C/C++ build toolchain. The `apm-lean/scripts/` importers and the futon6 proof
frame tools (`scripts/frontiermath/{init-proof-frame-workspace,promote-proof-frame-lean}.py`
and `scripts/apm_proof_audit.py`) use Python 3's standard library. They do **not**
need futon6's 7.8 GiB transformer virtualenv, GPU extras, or 22 GiB `data/` tree.
Install `futon6`'s Python package in a venv only when doing NLP/graph enrichment:

```bash
ssh zone-joe '
  cd "$HOME/code/futon6"
  python3 -m venv .venv
  .venv/bin/pip install --upgrade pip
  .venv/bin/pip install -e ".[dev]"'
```

The legacy `apm-lean/pipeline/run-problem.sh` generation lane also shells out to
the `claude` CLI. That is a separate, optional runtime dependency and requires an
interactive login or an explicitly provisioned API credential. Never rsync local
Claude/Codex credential directories to a compute box. Lean checking and the
futon6/APM proof-frame workflow do not require that CLI.

### Agent profile — Claude, Codex, and the FUTON Zai harness

Claude and Codex are user-local tools. Pin Node/NVM and CLI versions just like
the proof toolchain; an unqualified global npm install makes boxes drift. The
2026-08-04 operator-box manifest is NVM 0.40.3, Node 24.9.0, npm 11.7.0, and
Codex 0.145.0:

```bash
ssh zone-joe '
  set -eu
  export NVM_DIR="$HOME/.nvm"
  if [ ! -s "$NVM_DIR/nvm.sh" ]; then
    curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh \
      | PROFILE="$HOME/.bashrc" bash
  fi
  . "$NVM_DIR/nvm.sh"
  nvm install 24.9.0
  nvm alias default 24.9.0
  npm install -g npm@11.7.0
  npm install -g @openai/codex@0.145.0
  codex --version'
```

There is deliberately no third-party or official “Zai CLI” in this profile. A
FUTON Zai agent is the custom API-backed harness in
`futon3c/src/futon3c/agents/zai_api.clj`. It runs on the futon3c Clojure
classpath, calls `https://api.z.ai/api/coding/paas/v4`, and resolves its key from
`ZAI_API_KEY`, `~/.zaikey`, or `~/.zai-key` in that order. Deploy its source
closure as described in §3; do not install `@z_ai/coding-helper`.

Authentication is an explicit operator step. Do not copy an entire CLI state
directory. Authenticate Codex interactively:

```bash
ssh -t zone-joe '
  export NVM_DIR="$HOME/.nvm"; . "$NVM_DIR/nvm.sh"
  codex login --device-auth'
```

Provision the Z.AI key only when the operator explicitly places that credential
in deployment scope. Copy that single file, not a config directory, and enforce
the mode before loading the harness:

```bash
scp ~/.zai-key zone-joe:/home/joe/.zai-key
ssh zone-joe 'chmod 0600 "$HOME/.zai-key"; test -s "$HOME/.zai-key"'
```

Never put the token itself in this manifest, command arguments, logs, or shell
history.

NVM is loaded by `.bashrc` for interactive shells. A raw `ssh zone-joe command`
does not run that interactive initialization on Ubuntu. For automation, source
`$HOME/.nvm/nvm.sh` explicitly as above. The same issue can hide an otherwise
installed `~/.local/bin/claude`; set `PATH="$HOME/.local/bin:$PATH"` when invoking
Claude non-interactively.

---

## 3. Sync the repos — **compute the transitive closure first**

**This is the step that will waste your afternoon if you skip it.** `deps.edn` uses
`:local/root` siblings, and those siblings have `:local/root` deps of their own. Syncing
futon5's three direct deps is not enough: futon3a pulls `futon1/apps/nlp-interface`,
and resolution fails with

```
Error building classpath. Local lib nlp-interface/nlp-interface not found
```

Compute the closure rather than chasing errors one at a time:

```python
# scripts/local-dep-closure.py  (run locally)
import re, pathlib
root = pathlib.Path('/home/joe/code')
seen, queue = set(), ['futon5']          # <-- roots you actually want
while queue:
    r = queue.pop(0)
    if r in seen: continue
    seen.add(r)
    d = root / r / 'deps.edn'
    if not d.exists(): continue
    for m in re.findall(r':local/root "([^"]+)"', d.read_text()):
        p = (root / r / m).resolve()
        try: rel = str(p.relative_to(root))
        except ValueError: rel = str(p)
        if rel not in seen: queue.append(rel)
for s in sorted(seen): print(s)
```

For futon5 as of 2026-08-03 the closure is **seven** directories:

```
futon5  futon2  futon3a
futon1/apps/common  futon1/apps/nlp-interface
futon1/apps/charon  futon1/apps/graph-memory
```

Then sync. `--relative` preserves the `futon1/apps/...` nesting; excluding `.git` and
big binaries cuts ~3.9 GB to ~1.1 GB:

```bash
cd /home/joe/code
rsync -az --exclude '.git' --exclude 'target' --exclude '.cpcache' \
      --exclude '*.pdf' --exclude '*.png' --exclude '*.ppm' \
      futon5 futon2 futon3a zone-joe:~/code/
rsync -az --exclude '.git' --exclude 'target' --exclude '.cpcache' --relative \
      futon1/apps/common futon1/apps/nlp-interface \
      futon1/apps/charon futon1/apps/graph-memory zone-joe:~/code/
```

Warm the Maven cache (`repo1.maven.org` and clojars must be reachable — they were):

```bash
ssh zone-joe 'cd ~/code/futon5 && clojure -P'
```

### Sync the Lean proof profile

`apm-lean` and `futon6` are siblings: the APM import tools default to
`../futon6`, and futon6 proof-frame state lives under `futon6/.state/`. Preserve
that layout. The remote remains a deployment, not a second source of truth, so
exclude `.git` and rebuildable caches but include the current working tree and
`.state`:

```bash
cd /home/joe/code
rsync -az --exclude '.git' --exclude '.venv' --exclude '.pytest_cache' \
      --exclude '__pycache__' --exclude 'data' --exclude 'tmp' \
      futon6/ zone-joe:~/code/futon6/
rsync -az --exclude '.git' --exclude '.lake' --exclude '.cache' \
      --exclude '__pycache__' \
      apm-lean/ zone-joe:~/code/apm-lean/
```

This intentionally carries uncommitted source/state so an in-progress proof can
move with the operator. It intentionally does not make that state authoritative:
retrieve it before teardown and commit locally. On a reused box, inspect before
adding `--delete`; blindly deleting remote-only proof work violates that rule.

Check the pins and the source transfer before warming caches:

```bash
sha256sum apm-lean/lean-toolchain apm-lean/lake-manifest.json
ssh zone-joe 'sha256sum ~/code/apm-lean/lean-toolchain ~/code/apm-lean/lake-manifest.json'

# No output means every included file matches. This is read-only.
rsync -aznci --exclude '.git' --exclude '.venv' --exclude '.pytest_cache' \
      --exclude '__pycache__' --exclude 'data' --exclude 'tmp' \
      futon6/ zone-joe:~/code/futon6/
rsync -aznci --exclude '.git' --exclude '.lake' --exclude '.cache' \
      --exclude '__pycache__' \
      apm-lean/ zone-joe:~/code/apm-lean/
```

### Sync the FUTON Zai harness

The Zai harness is futon3c code, not an external executable. Its **base**
`deps.edn` closure is ten source directories:

```
futon3c  futon3b  futon1b  futon0  futon2  futon3a
futon1/apps/common  futon1/apps/nlp-interface
futon1/apps/graph-memory  futon1/apps/charon
```

The `:dev` alias has additional local roots (`futon5`, WebArxana, and War
Machine); they are not required to load or call `futon3c.agents.zai-api`. Sync
the base closure without copying XTDB stores, corpora, caches, or frontend build
trees:

```bash
cd /home/joe/code
rsync -az --exclude '.git' --exclude '.venv' --exclude '.cpcache' \
      --exclude '.clj-kondo' --exclude '.lsp' --exclude '.shadow-cljs' \
      --exclude 'target' --exclude 'node_modules' --exclude 'tmp' \
      --exclude 'data' \
      futon3c futon3b futon0 futon2 futon3a zone-joe:~/code/
rsync -az --exclude '.git' --exclude '.cpcache' \
      --exclude 'migration-store*' --exclude 'migration-export*' \
      --exclude 'textprobe' futon1b zone-joe:~/code/
rsync -az --relative --exclude '.git' --exclude '.cpcache' \
      futon1/apps/common futon1/apps/nlp-interface \
      futon1/apps/graph-memory futon1/apps/charon zone-joe:~/code/
```

Warm the base classpath and prove both the namespace and credential resolver are
functional without making an API call:

```bash
ssh zone-joe '
  set -eu
  cd "$HOME/code/futon3c"
  clojure -P
  clojure -M -e '\''
    (require (quote [futon3c.agents.zai-api :as zai]))
    (assert (string? (zai/resolve-api-key)))
    (println :zai-api-ready)'\'''
```

This gate intentionally does not start futon3c or register an agent. Starting a
remote Agency service is a separate deployment decision with its own ports,
stores, and restart-safety checks.

### Agency worker profile — the actual deployment closure

Loading the Zai namespace and running Agency are different profiles. Agency is
started with `-M:dev`, whose local roots add these three directories to the ten
directory base closure above, even when the headless profile disables both CLJS
watches and the WebArxana server:

```
futon5
futon4/dev/web/webarxana
futon2/web/war-machine
```

That makes **13 classpath directories** the minimum deployable source closure.
The running process also maintains an in-JVM source watcher over the first-class
FUTON repositories (`futon0`, `futon1`, `futon1a`, `futon2`, `futon3`,
`futon3a`, `futon3b`, `futon3c`, `futon4`, `futon5`, `futon5a`, `futon6`,
`futon7`, and `futon7a`). Missing watcher roots do not prevent the HTTP server
from binding, but they make the box an incomplete model of the stack. A box
advertised as a general Agency worker should therefore carry those source trees;
a Zai-only harness smoke test need not.

For zone-joe, sync the missing classpath root and the first-class watcher roots
without copying Git history, dependency caches, corpora, or stores:

```bash
cd /home/joe/code
rsync -az --exclude '.git' --exclude '.m2' --exclude '.venv' --exclude '.cpcache' \
  --exclude '.clj-kondo' --exclude '.lsp' --exclude '.shadow-cljs' \
  --exclude 'target' --exclude 'node_modules' --exclude 'data' \
  --exclude 'tmp' --exclude 'audio' --exclude '*.pdf' --exclude '*.png' \
  --exclude '*.ppm' --exclude '*.webm' --exclude '*.mp3' --exclude '*.mp4' \
  --exclude '*.wav' \
  futon1a futon3 futon4 futon5a futon7 futon7a zone-joe:~/code/
```

Do not copy an existing XTDB store to make the worker look populated. The zone
profile creates a fresh, explicitly named `~/code/futon1b/ams-store`. XTDB2 is
single-process: the embedded node and `futon1b-server.service` must never open
the same store. The launcher refuses to start if that separate service is
active. `FUTON1B_BIND_HOST=127.0.0.1` keeps the substrate/evidence API private
to the serving JVM; only Agency port 7070 is exposed to federation peers.

The supported topology on zone-joe is one managed JVM containing Agency and
futon1b. Install the inert unit files, create a box-local Drawbridge token, and
enable user lingering so the service survives logout and starts at boot:

```bash
ssh zone-joe '
  set -eu
  cd "$HOME/code/futon3c"
  install -Dm644 scripts/systemd/units/futon-services.slice \
    "$HOME/.config/systemd/user/futon-services.slice"
  install -Dm644 scripts/systemd/units/futon-agents.slice \
    "$HOME/.config/systemd/user/futon-agents.slice"
  install -Dm644 scripts/systemd/units/futon3c-zone.service \
    "$HOME/.config/systemd/user/futon3c-zone.service"
  umask 077
  head -c 32 /dev/urandom | base64 > .admintoken
  systemd-analyze --user verify \
    "$HOME/.config/systemd/user/futon-services.slice" \
    "$HOME/.config/systemd/user/futon-agents.slice" \
    "$HOME/.config/systemd/user/futon3c-zone.service"
  systemctl --user daemon-reload
  sudo loginctl enable-linger "$USER"
  systemctl --user enable --now futon3c-zone.service'
```

The service deliberately does not depend on the laptop-specific
`futon3c-server.service`, which starts `dev-laptop-env` and wants a separate
futon1b unit. Claude and Codex child processes are launched through
`run-agent-scope` into the bounded `futon-agents.slice`; otherwise they inherit
the protected server cgroup. The Zai harness has no child CLI and remains inside
the serving JVM. `ExecStartPost` creates a Zai lane through
`POST /api/alpha/agents/auto` after the startup Codex lane is addressable, or
reuses the restored Zai lane on a restart. The roster and durable turn queue are
stored under `~/.local/state/futon3c/`, not `/tmp`.

`FUTON3C_SITE=ams` is the Amsterdam area code. Agency keeps one local record per
lane and resolves the qualified name as an alias, so both `zai-1` and
`ams-zai-1` reach the same session rather than creating two agents. Startup
Claude/Codex are exposed as `ams-claude-1` and `ams-codex-1`; federation exports
the qualified names. Verify the topology, store, identities, and peer view:

```bash
ssh zone-joe '
  set -eu
  curl -fsS http://127.0.0.1:7070/health | jq .
  curl -fsS http://127.0.0.1:7074/health | jq .
  curl -fsS http://127.0.0.1:7070/api/alpha/agents | jq .
  curl -fsS http://127.0.0.1:7070/api/alpha/agents/ams-zai-1 | jq .
  test "$(pgrep -fc "[c]lojure.main -m futon3c.dev")" -eq 1
  systemctl --user is-active futon3c-zone.service
  systemctl --user is-enabled futon3c-zone.service'
```

Port 7070 binds on all interfaces because London and Chicago must be able to
mirror the worker. Restrict inbound TCP/7070 to the federation peers in the
provider firewall; use an SSH tunnel for operator access. Drawbridge remains
loopback-only on 6768 and uses the per-box token.

---

## 4. **Verify determinism BEFORE trusting any result**

Non-negotiable. A remote box can differ in JVM version, default locale, or — the real
hazard — in how parallelism assigns seeds to workers. Any of these silently makes remote
numbers non-comparable to local ones, and it surfaces three slices later as an
unexplained discrepancy.

Pick an experiment that is **already committed with a known hash**, re-run it remotely,
and diff byte-for-byte:

```bash
ssh zone-joe 'cd ~/code/futon5 && clojure -M scripts/exotype_policy_slice5.clj run /tmp/remote.edn /tmp/remote.md'
scp zone-joe:/tmp/remote.edn /tmp/remote.edn
sha256sum /tmp/remote.edn reports/exotype-policy-slice5.edn   # MUST match
```

If it does not match, **stop**. Remote results are not comparable to existing ones and
mixing them into one table is a defect. Either fix the cause or re-run the earlier
slices remotely too.

### Verify the Lean deployment

Do not accept `lean --version` as proof that the workflow is ready. It does not test
the locked mathlib checkout, downloaded oleans, native toolchain, or project module
layout. The acceptance gate is the complete project build plus syntax checks for the
standard-library bridge scripts:

```bash
ssh zone-joe '
  set -eu
  export PATH="$HOME/.elan/bin:$PATH"
  cd "$HOME/code/apm-lean"
  test "$(lean --version | sed -n "s/Lean (version \([^,]*\).*/\1/p")" = "4.29.0-rc8"
  lake build

  t=$(mktemp -d); trap '\''rm -rf "$t"'\'' EXIT
  PYTHONPYCACHEPREFIX="$t/pycache" python3 -m py_compile \
    ../futon6/scripts/apm_proof_audit.py \
    ../futon6/scripts/frontiermath/init-proof-frame-workspace.py \
    ../futon6/scripts/frontiermath/promote-proof-frame-lean.py \
    scripts/import-proof-frame.py scripts/bulk-import-proof-frames.py'
```

As deployed on 2026-08-04 this completed **8,245 Lake jobs** successfully. Linter
warnings in `C2Analysis.lean` and `C4Functional.lean` are the current local baseline;
errors or a changed job graph are not.

---

## 5. Parallelising — partition by CONDITION, not by seed

The tempting move is many workers sharing one checkpoint file. Do not: it races, and it
corrupts precisely the determinism section 4 just established, in a way that still looks
like a valid artifact.

**Partition by condition, one checkpoint file per process, merge afterwards.** This also
preserves per-condition seed ORDER, which is what makes byte-identical comparison to a
single-threaded run possible at all.

Leave enough headroom for the JVMs: with 16 cores use ~12 workers, not 32. Peak RSS per
run in these experiments is ~2 GB, so memory is not the binding constraint; cores are.

Use `nohup`/`systemd-run` so work survives the SSH session:

```bash
ssh zone-joe 'cd ~/code/futon5 && nohup ./run-partition.sh 0 12 > /tmp/p0.log 2>&1 &'
```

---

## 6. Getting results back

```bash
rsync -az zone-joe:~/code/futon5/reports/ /home/joe/code/futon5/reports-remote/
```

Land them into git **locally**, after review — the remote box is a compute surface, not
a source of truth. Do not `git push` from it.

---

## 7. Teardown

```bash
# 1. Stop new work, then retrieve everything. Nothing on the box is authoritative.
ssh zone-joe 'systemctl --user disable --now futon3c-zone.service'
rsync -az zone-joe:~/code/futon5/reports/ ./reports-remote/
rsync -az zone-joe:~/code/apm-lean/ ./apm-lean-remote-recovery/
rsync -az zone-joe:~/code/futon6/.state/ ./futon6-state-remote-recovery/
rsync -az zone-joe:~/code/futon1b/ams-store/ ./ams-store-remote-recovery/
rsync -az zone-joe:~/.local/state/futon3c/ ./ams-agency-state-remote-recovery/

# 2. Confirm you have it, THEN remove code and caches.
ssh zone-joe 'rm -rf ~/code ~/.m2 ~/.gitlibs ~/.cpcache /tmp/*.edn /tmp/*.md
              rm -rf ~/.local/state/futon3c
              rm -f ~/.zai-key ~/.zaikey
              rm -f ~/.config/systemd/user/futon3c-zone.service
              systemctl --user daemon-reload'

# 3. If you granted sudo, revoke it.
ssh zone-joe 'sudo loginctl disable-linger "$USER"
              sudo rm -f /etc/sudoers.d/90-joe-nopasswd
              sudo visudo -c'

# 4. Packages installed system-wide, if the box is being handed on:
ssh zone-joe 'sudo apt-get remove -y openjdk-21-jdk-headless dmidecode
              sudo rm -f /usr/local/bin/clj /usr/local/bin/clojure
              sudo rm -rf /usr/local/lib/clojure
              rm -rf "$HOME/.elan"'
```

**Capture before you decommission.** Anything not rsynced back is gone, and a
half-finished checkpoint file is often the most valuable thing on the box.

---

## Gotchas, in the order they will bite you

| symptom | cause | fix |
|---|---|---|
| `nproc` disagrees with the spec | it counts threads | read `lscpu` `Core(s) per socket` |
| RAM far below what was ordered | DIMMs unpopulated, or board caps below spec | `sudo dmidecode -t memory`; check `Maximum Capacity` |
| Clojure install 403s | `download.clojure.org` blocked, no proxy | fetch `linux-install.sh` from GitHub releases |
| `apt-get` sits with no download progress | configured Ubuntu archive is stalling | probe it and a signed Ubuntu mirror; preserve and change the source only after confirming (§2) |
| `Local lib X not found` | `:local/root` deps are **transitive** | compute the closure (§3), do not chase one at a time |
| a `scripts/*.clj` won't run though `clojure` works | it is a **babashka** script; bb is a separate install | §2, and run `bb -e '(+ 1 1)'` inside futon5 to fetch bb.edn deps + pods |
| remote bb version ≠ local | the installer always takes **latest** | pin it if bb touches results (see below) |
| `lean --version` shows the wrong version | command was run outside the project, or a distro Lean shadows Elan | put `~/.elan/bin` first and run inside `apm-lean` |
| mathlib changes during setup | `lake update` followed the `master` input revision and rewrote the lock | deploy the committed `lake-manifest.json`; use `lake exe cache get`, never `lake update` |
| mathlib cache fails to unpack | `zstd` or native build tools are absent | install the complete Lean profile (§2) before warming the cache |
| remote numbers differ subtly | JVM/locale/parallel seed assignment | the §4 byte-identical gate, before any science |
| corrupted grid artifact | parallel workers sharing a checkpoint | one checkpoint per partition, merge after |
| work dies when SSH drops | foreground process | `nohup`, or `systemd-run` for >30 min |
| agent "cannot commit" on the remote | the rsync excluded `.git`, so it is **not a repo** | commit LOCALLY after rsyncing results back; never ask a remote agent to commit |
| a long job's park fires but the job is still running | the Agency treats `overrun` as resolved; the park releases with no result | monitor the ARTIFACT as well as the park, for anything over the soft cap |
| checkpoint file stops growing | large runs write per-seed, so gaps are minutes | check CPU ticks before concluding stuck (see below) |

## Is it stuck, or just slow?

A flat checkpoint is NOT evidence of a wedged run. On 2026-08-03 a Slice-6d job showed
**+0 bytes of checkpoint over 50 s while burning +59,983 CPU ticks** — about 12 cores
saturated. The runs were width 160 x 24,000 steps, eight times the cost of the baseline
cell, so minutes pass between per-seed writes.

Check CPU delta before declaring anything stuck:

```bash
ssh zone-joe 'P=$(pgrep -f <driver> | head -1)
  A=$(stat -c %s <checkpoint>); C1=$(awk "{print \$14+\$15}" /proc/$P/stat)
  sleep 50
  B=$(stat -c %s <checkpoint>); C2=$(awk "{print \$14+\$15}" /proc/$P/stat)
  echo "checkpoint +$((B-A)) bytes  cpu +$((C2-C1)) ticks  elapsed $(ps -o etime= -p $P)"'
```

CPU climbing + checkpoint flat = working. Both flat = investigate. Killing a job that is
merely slow discards harvestable work, and these checkpoints are resumable anyway.

## Nothing on the remote is committed — or committable

`.git` is excluded from the sync (it is most of the 3.9 GB). **The remote tree is
therefore not a git repository**, and an agent told to "commit early" there cannot. The
consequence is easy to miss: artifacts produced on the remote exist ONLY there until
someone rsyncs them back, and §7's teardown would delete them.

Rule: **remotes compute, the local box commits.** Say so explicitly in any handoff that
runs work remotely, and retrieve before teardown.

## Timing reference (2026-08-03, futon5 MetaCA experiments)

- one 120-step run at width 80: **0.18–0.29 s**
- 13 seed-runs/minute single-threaded on the local box
- the 54-condition × N=100 Slice 5 grid: **~6.9 wall-hours** single-threaded
- same grid on 16 cores, partitioned: **well under an hour**
