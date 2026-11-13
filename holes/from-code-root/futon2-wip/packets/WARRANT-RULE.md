## Standing rule for every futon2 dispatch (Joe, 2026-09-17)

**You run the tests and hand back a WARRANT. The reviewer does not rerun them.**

A pasted test count is not evidence: it cannot be checked later, it goes stale
silently, and it makes the reviewer redo your work. A Test Registry warrant can
be checked by anyone, at any time, and it goes stale by itself the moment
someone edits a file in its load closure.

For each namespace you touched, after your commit:

    cat > /tmp/reg-<ns>.edn <<'CFG'
    {:agency-url "http://localhost:7070"
     :repo-root "/home/joe/code/futon2"
     :command ["clojure" "-M:test" "-n" "<the.test.namespace>"]
     :author "<your agent id>"
     :artifact-dir "/home/joe/code/storage/test-registry"
     :code-paths ["src"]
     :test-paths ["test"]
     :output :json}
    CFG
    cd /home/joe/code/futon3c && clojure -M -m futon3c.test-registry run /tmp/reg-<ns>.edn

The command shape is fixed: `["clojure" "-M:<aliases>" "-n" "<ns>"]`, exactly
four elements. Anything else is refused `:explicit-namespace-required`.

**Report the `evidence/id` of each warrant** (`test-registry-…`) and its
recorded results. Register AFTER your final commit — a warrant registered
before it is stale on arrival.

If a warrant comes back `warrant? false`, say so and say why. A refused warrant
reported honestly is worth more than a green count.
