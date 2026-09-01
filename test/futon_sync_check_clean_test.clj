(require '[babashka.fs :as fs]
         '[babashka.process :as proc]
         '[cheshire.core :as json]
         '[clojure.test :refer [deftest is run-tests testing]])

(System/setProperty "futon.sync.library" "true")
;; Declared BEFORE the load-file below, which is what defines them. Without
;; this clj-kondo reports four unresolved symbols, because it cannot follow a
;; runtime load-file -- and a linter that is permanently red on a file stops
;; being read. The order matters: load-file's defs replace these unbound vars,
;; so declaring after it would leave them unbound.
(declare repo-status fetch? clean-verdict cmd-check-clean)

;; Resolve the script relative to THIS file rather than by absolute path, so
;; the test travels with the repo instead of only working on one box. The
;; original fixture hardcoded /home/joe/code/futon0 and lived in /tmp, which is
;; the same failure this check exists to catch: a check that cannot be re-run.
(load-file (str (fs/path (fs/parent (fs/parent (fs/real-path *file*)))
                         "scripts" "futon-sync.clj")))

(defn sh [dir & args]
  (let [r (apply proc/shell {:dir (str dir) :out :string :err :string
                             :continue true} args)]
    (when-not (zero? (:exit r))
      (throw (ex-info (:err r) {:args args :dir (str dir)})))
    r))

(defn git! [dir & args] (apply sh dir "git" args))

(defn commit! [dir message & [epoch]]
  (spit (str (fs/path dir "work.txt")) (str message "\n") :append true)
  (git! dir "add" "work.txt")
  (if epoch
    (let [r (proc/shell {:dir (str dir) :out :string :err :string
                         :continue true
                         :extra-env {"GIT_AUTHOR_DATE" (str "@" epoch)
                                     "GIT_COMMITTER_DATE" (str "@" epoch)}}
                        "git" "commit" "-m" message)]
      (when-not (zero? (:exit r)) (throw (ex-info (:err r) {}))))
    (git! dir "commit" "-m" message)))

(defn init-repo! [root name upstream?]
  (let [repo (fs/path root name)]
    (fs/create-dirs repo)
    (git! repo "init" "-b" "main")
    (git! repo "config" "user.email" "test@example.invalid")
    (git! repo "config" "user.name" "Test")
    (commit! repo "initial")
    (when upstream?
      (let [bare (fs/path root (str name ".git"))]
        (git! root "init" "--bare" (str bare))
        (git! repo "remote" "add" "origin" (str bare))
        (git! repo "push" "-u" "origin" "main")
        (git! bare "symbolic-ref" "HEAD" "refs/heads/main")))
    repo))

(defn status [repo label]
  (repo-status {:label label :abs-path (str repo)}))

(deftest acceptance-matrix
  (let [root (fs/create-temp-dir {:prefix "futon-sync-check-clean-"})
        now (System/currentTimeMillis)]
    (try
      (with-redefs [fetch? false]
        (testing "a: two-day-old untracked file fails clause 1"
          (let [repo (init-repo! root "old-file" true)
                p (fs/path repo "old.txt")]
            (spit (str p) "old")
            (java.nio.file.Files/setLastModifiedTime
              p (java.nio.file.attribute.FileTime/fromMillis
                  (- now (* 48 60 60 1000))))
            (is (= [1] (mapv :clause (:failures
                                      (clean-verdict (status repo "old-file") now)))))))

        (testing "b: behind upstream fails clause 2"
          (let [repo (init-repo! root "behind" true)
                peer (fs/path root "behind-peer")]
            (git! root "clone" (str (fs/path root "behind.git")) (str peer))
            (git! peer "config" "user.email" "test@example.invalid")
            (git! peer "config" "user.name" "Test")
            (commit! peer "remote commit")
            (git! peer "push")
            (git! repo "fetch")
            (let [s (status repo "behind")]
              (is (= [2] (mapv :clause (:failures
                                        (clean-verdict s now)))
                     ) (pr-str (select-keys s [:ahead :behind :upstream :sync-error]))))))

        (testing "c: five recent commits ahead pass"
          (let [repo (init-repo! root "recent-ahead" true)]
            (dotimes [i 5] (commit! repo (str "recent " i)))
            (is (:clean (clean-verdict (status repo "recent-ahead") now)))))

        (testing "d: oldest unpushed commit three days old fails clause 3"
          (let [repo (init-repo! root "stale-ahead" true)
                old-sec (quot (- now (* 72 60 60 1000)) 1000)]
            (commit! repo "stale" old-sec)
            (dotimes [i 4] (commit! repo (str "new " i)))
            (is (= [3] (mapv :clause (:failures
                                      (clean-verdict (status repo "stale-ahead") now)))))))

        (testing "e: no upstream fails clause 4"
          (let [repo (init-repo! root "no-upstream" false)]
            (is (= [4] (mapv :clause (:failures
                                      (clean-verdict (status repo "no-upstream") now)))))))

        (testing "f: fully clean repo passes"
          (let [repo (init-repo! root "clean" true)]
            (is (:clean (clean-verdict (status repo "clean") now)))))

        (testing "g: JSON and human render the same verdict set"
          (let [clean (init-repo! root "render-clean" true)
                bad (init-repo! root "render-bad" false)
                repos [{:label "render-clean" :abs-path (str clean)}
                       {:label "render-bad" :abs-path (str bad)}]
                human (with-out-str (cmd-check-clean repos {:now-ms now}))
                json-out (with-out-str (cmd-check-clean repos {:now-ms now :json? true}))
                parsed (json/parse-string json-out true)
                verdicts (into {} (map (juxt :repo :clean) (:repos parsed)))]
            (is (= {"render-clean" true "render-bad" false} verdicts))
            (is (re-find #"render-clean\s+PASS" human))
            (is (re-find #"render-bad\s+FAIL" human))))

        (testing "h: feature HEAD without upstream does not fail clean default"
          (let [repo (init-repo! root "feature-head" true)]
            (git! repo "switch" "-c" "agent-work")
            (let [s (status repo "feature-head")
                  verdict (clean-verdict s now)
                  human (with-out-str
                          (cmd-check-clean
                            [{:label "feature-head" :abs-path (str repo)}]
                            {:now-ms now}))]
              (is (:clean verdict))
              (is (= "main" (:default-branch verdict)))
              (is (re-find #"INFO: HEAD is not the default branch \(main\)"
                           human)))))

        (testing "i: unresolved default branch fails clause 4 distinctly"
          (let [repo (init-repo! root "no-default" false)]
            (git! repo "branch" "-m" "topic-only")
            (let [verdict (clean-verdict (status repo "no-default") now)
                  failure (first (:failures verdict))]
              (is (= 4 (:clause failure)))
              (is (= "no-default-branch" (:reason failure))))))

        (testing "j: merged worktree fails clause 5 as dead"
          (let [repo (init-repo! root "dead-worktree" true)
                wt (fs/path root "dead-worktree-agent")]
            (git! repo "worktree" "add" "-b" "already-done" (str wt) "main")
            (let [repos [{:label "dead-worktree" :abs-path (str repo)}]
                  failures (:failures (clean-verdict (status repo "dead-worktree") now))
                  human (with-out-str (cmd-check-clean repos {:now-ms now}))
                  json-out (with-out-str
                             (cmd-check-clean repos {:now-ms now :json? true}))
                  json-reasons (mapv :reason
                                     (get-in (json/parse-string json-out true)
                                             [:repos 0 :failures]))]
              (is (some #(and (= 5 (:clause %))
                              (= "dead-worktree" (:reason %))) failures))
              (is (re-find #"clause 5: dead worktree" human))
              (is (some #{"dead-worktree"} json-reasons)))))

        (testing "k: worktree with unmerged commit is INFO, not failure"
          (let [repo (init-repo! root "live-worktree" true)
                wt (fs/path root "live-worktree-agent")]
            (git! repo "worktree" "add" "-b" "agent-live" (str wt) "main")
            (commit! wt "unmerged")
            (let [verdict (clean-verdict (status repo "live-worktree") now)]
              (is (:clean verdict))
              (is (some #(= "unmerged-worktree" (:reason %)) (:info verdict))))))

        (testing "l: detached ancestor worktree fails clause 5 as dead"
          (let [repo (init-repo! root "detached-dead" true)
                wt (fs/path root "detached-dead-agent")]
            (git! repo "worktree" "add" "--detach" (str wt) "main")
            (let [failure (some #(when (= "dead-worktree" (:reason %)) %)
                                (:failures (clean-verdict
                                             (status repo "detached-dead") now)))
                  worktree (first (:worktrees failure))]
              (is (= 5 (:clause failure)))
              (is (= "detached" (:branch worktree))))))

        (testing "m: detached worktree with unmerged commit is INFO"
          (let [repo (init-repo! root "detached-live" true)
                wt (fs/path root "detached-live-agent")]
            (git! repo "worktree" "add" "--detach" (str wt) "main")
            (commit! wt "detached unmerged")
            (let [verdict (clean-verdict (status repo "detached-live") now)
                  info (some #(when (= "unmerged-worktree" (:reason %)) %)
                             (:info verdict))]
              (is (:clean verdict))
              (is (= "detached" (get-in info [:worktree :branch]))))))

        (testing "n: worktree outside sibling tree fails clause 5"
          (let [repo (init-repo! root "off-tree" true)
                container (fs/path root "not-a-sibling")
                wt (fs/path container "off-tree-agent")]
            (fs/create-dirs container)
            (git! repo "worktree" "add" "-b" "off-tree-agent" (str wt) "main")
            (let [failures (:failures (clean-verdict (status repo "off-tree") now))]
              (is (some #(and (= 5 (:clause %))
                              (= "worktree-off-sibling-tree" (:reason %)))
                        failures)))))

        (testing "o: no extra worktrees leaves the verdict unchanged"
          (let [repo (init-repo! root "no-extra-worktrees" true)
                verdict (clean-verdict (status repo "no-extra-worktrees") now)]
            (is (:clean verdict))
            (is (empty? (:info verdict)))
            (is (not-any? #(= 5 (:clause %)) (:failures verdict))))))
      (finally (fs/delete-tree root)))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
