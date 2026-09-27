#!/usr/bin/env bb
;; mana_snapshot_uncertainty_test.bb — tests for the uncertain-pressure
;; merge in scripts/mana-snapshot.bb (C8, M-inbox-zero-claim-lifecycle N3).
;; Run: bb scripts/mana_snapshot_uncertainty_test.bb   (from futon0 root)

(require '[clojure.test :refer [deftest is run-tests]]
         '[clojure.string :as str]
         '[babashka.fs :as fs])

;; Load the producer WITHOUT its -main entry form: strip the shebang AND the
;; trailing (when (= *file* ...) ...) entry guard so load-string cannot
;; regenerate the real snapshot.
(def script-path "scripts/mana-snapshot.bb")
(def script-source
  (str/join "\n"
            (remove #(or (str/starts-with? (str/trim %) "(when (= *file*")
                         (str/starts-with? (str/trim %) "(apply -main"))
                    (rest (str/split-lines (slurp script-path))))))
(load-string script-source)

(defn- fixture-edn [dir]
  (let [f (str (fs/path dir "uncertain-pressure.edn"))]
    (spit f (pr-str {:at (java.util.Date. 1789000000000)
                     :generated-by "futon3c.inbox-zero.sweeper"
                     :interval-ms 1800000
                     :drilldown "/home/joe/code/storage/inbox-zero/operator-backlog.edn"
                     :repos [{:label "futon3c-d"
                              :root (.getCanonicalPath (io/file "/tmp/n3-alias-target"))
                              :dirty-count 10 :untracked 3 :remainder 5}]}))
    f))

(deftest missing-input-is-unavailable-never-zero
  (let [u (load-uncertainty "/nonexistent/n3.edn")]
    (is (= :missing (:status u)))
    (is (= [{:abs-path "/x"}] (merge-uncertainty [{:abs-path "/x"}] u))
        "no :uncertain key is minted for missing input")))

(deftest malformed-input-is-unavailable-never-zero
  (let [dir (str (fs/create-temp-dir))
        f (str (fs/path dir "bad.edn"))]
    (spit f "{not edn")
    (is (= :malformed (:status (load-uncertainty f))))
    (spit f (pr-str {:no-repos true}))
    (is (= :malformed (:status (load-uncertainty f))))))

(deftest fresh-input-merges-by-canonical-root
  (let [dir (str (fs/create-temp-dir))
        target (str (fs/path dir "real"))]
    (fs/create-dirs target)
    (let [link (str (fs/path dir "alias"))]
      (fs/create-sym-link link target)
      (let [f (str (fs/path dir "u.edn"))]
        (spit f (pr-str {:at (java.util.Date.)
                         :interval-ms 1800000
                         :drilldown "/storage/operator-backlog.edn"
                         :repos [{:label "futon3c-d" :root link
                                  :dirty-count 10 :untracked 3 :remainder 5}]}))
        (let [u (load-uncertainty f)
              merged (merge-uncertainty [{:abs-path target :repo "futon3c-d"}
                                         {:abs-path (str target "-other") :repo "futon3c"}]
                                        u)]
          (is (= :available (:status u)))
          (is (false? (:stale? u)))
          ;; the alias in the feed and the real path in the snapshot join
          (is (= 10 (get-in (first merged) [:uncertain :dirty-count])))
          ;; a different worktree with a similar label does NOT merge
          (is (nil? (:uncertain (second merged))))
          (is (= "/storage/operator-backlog.edn" (:drilldown u))))))))

(deftest stale-input-is-merged-but-flagged
  (let [dir (str (fs/create-temp-dir))
        f (str (fs/path dir "u.edn"))]
    (spit f (pr-str {:at (java.util.Date. 1000000000000) ;; 2001
                     :interval-ms 1800000
                     :repos [{:label "x" :root dir
                              :dirty-count 4 :untracked 0 :remainder 0}]}))
    (let [u (load-uncertainty f)
          merged (first (merge-uncertainty [{:abs-path dir}] u))]
      (is (= :available (:status u)))
      (is (true? (:stale? u)))
      (is (= 4 (get-in merged [:uncertain :dirty-count])))
      (is (true? (:uncertain-stale merged))))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (zero? (+ fail error)) 0 1)))
