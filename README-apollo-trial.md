# Apollo public-source installation trial

2026-09-09. **Public source fetch and dependency resolution passed; Agency cannot
load from the selected public commits. Persistent service startup and recovery
validation have not passed.** Joe's serving processes were not changed.

## Reproduce the source installation

The pinned candidate is
[`config/public-install-candidate.json`](config/public-install-candidate.json).
It records ten public FUTON repositories and futon4's public `reazon` submodule
at `377e879202943eba10b812f372d6cb00aa344fe9`. It is explicitly unqualified.

```bash
python3 scripts/install-fetch.py \
  --manifest config/public-install-candidate.json \
  --destination /PATH/TO/NEW/code
```

The fetcher checks anonymous GitHub visibility, accepts explicit HTTPS URLs and
full commit hashes, disables inherited Git configuration and credential prompts,
and checks submodule pins/URLs against the parent tree. It refuses existing
destinations, undeclared submodules and nested submodules not supported by this
manifest schema. It never pulls over existing work. `fetch-receipt.json` records
the manifest digest, per-repository identity and success/failure. Fetch success
is not installation acceptance.

On this host the command ran as Apollo through
`sudo -n runuser -u apollo -- …`. The resulting clean checkouts are at
`/home/apollo/futon-install/trial/code`, approximately **3.4 GiB** after source
checkout and dependency preparation. An initial incomplete attempt is retained
at `trial-incomplete-submodule`: its failure identified the missing submodule
manifest entry. That failed tree was not silently relabelled as successful.

## Tests and evidence

Machine-readable local receipt:
`/home/apollo/futon-install/validation.json`.

| Check | Result |
|---|---|
| Anonymous fetch of all ten repositories plus submodule | Passed; source receipt manifest SHA-256 `9d36caffb650e795ac75f87a57a5269bddf0b2adabde1370f4e199aca53a6641` |
| Checkout identities after tests | Ten HEADs match the candidate, ten working trees clean |
| futon1b `clojure -P -M:server` | Passed |
| futon3c `clojure -P -M:dev-serve` | Passed |
| futon1b `test-a1a2` | **42/42 passed**; real ephemeral HTTP endpoints and in-memory store, not a persistent instance |
| futon1b `test-text-search` | **All passed**, including independent search oracle agreement |
| futon3c cold `(require 'futon3c.dev)` | **Failed**, missing function in public futon3 dependency |
| Existing Agency `server-guard assert-zero` | **Refused** because Joe's serving Agency already exists |
| Persistent boot, agent invoke and stop/start recovery | **Not run**; the prerequisites above failed |

All Clojure commands used `-Srepro` and
`-Sdeps '{:mvn/local-repo "/home/apollo/futon-install/cache/m2"}'`, so dependency
resolution did not use Joe's Maven cache or Apollo's old root-owned `.m2` cache.
The new cache occupied approximately 163 MiB after these checks. System Java,
Clojure CLI and Babashka were inherited: Java 21.0.11, Clojure CLI 1.12.5.1664,
Babashka 1.13.219. This is an account-isolation trial, not a clean OS result.

Logs are `agency-dependencies.log`, `agency-load-check.log`,
`substrate-search-test.log` and `fetch.log` under `/home/apollo/futon-install`.
The HTTP test's 42/42 result was observed in the execution transcript before the
first checkout was retained; no separate raw log was saved for that test.
The six Python installer tests also pass, including preservation of an existing
destination, rejection of credential URLs/path escapes, port conflicts and
read-only planning.

## Concrete release blocker

At public futon3c `683908c782486a978db89b9fafee105c9b272e3c`, loading
`futon3c.inbox-zero.turn-promotion` fails at line 280:

```text
No such var: promote-exec/execute-plan-with-refresh!
```

The candidate's public futon3 commit
`bfa8a9c45ef2989114de4f35ecfe748b5eeb761f` does not contain that function.
Joe's local futon3 contains it, introduced in the committed change
`9ef7bc181549fef54d3598c2e063cd6c3a324a1a` ("Self-heal stale inbox promotion
plans"). That change includes implementation and tests. An unauthenticated GitHub
API lookup for this exact commit returned **422**, so its public availability
was not established. No repositories were pushed during this trial.

The repair is to review and publish a coherent source release containing the
required futon3 implementation, then update the candidate pin and repeat the
cold load and relevant promotion tests. Do not copy Joe's local implementation
into the supposedly public checkout or remove the call to make compilation pass.
There may be further release mismatches after this first one is corrected.

## Shared-host runtime prerequisite

The public Agency guard still enforces one serving process across all users.
Joe has authorized Apollo staging, but the guard has not been updated to express
that topology. Complete the
[instance-provenance design](../futon3c/holes/tickets/T-jvm-provenance-before-multi-jvm.md)
and implement instance-aware guards with tests for two different accounts/stores,
duplicate instances, occupied ports, source mismatch and wrong-instance stop/reload.
Keep the existing production instance protected. No guard was omitted or changed
in this trial.

The [clean Linode plan](README-clean-linode.md) addresses the separate OS-install
question. A fresh host removes the local two-Agency conflict, but cannot repair
the incompatible public commits. Resolve that release mismatch before scheduling
an outage to try the installer.
