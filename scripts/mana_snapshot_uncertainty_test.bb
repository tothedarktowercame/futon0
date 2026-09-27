#!/usr/bin/env bb
;; mana_snapshot_uncertainty_test.bb — tests for the uncertain-pressure
;; seam (scripts/mana_uncertainty.clj; C8, N4). Loads the pure namespace
;; directly — no producer evaluation, no entry-guard stripping, and no
;; code path here can regenerate the real snapshot.
;; Run: bb scripts/mana_snapshot_uncertainty_test.bb   (from futon0 root)

(require '[clojure.test :refer [deftest is testing run-tests]]
         '[babashka.fs :as fs])

(load-file "scripts/mana_uncertainty.clj")
(require 'mana-uncertainty)
(def u-load mana-uncertainty/load-uncertainty)
(def u-merge mana-uncertainty/merge-uncertainty)

(def temp-dirs (atom []))
(defn- temp-dir []
  (let [d (str (fs/create-temp-dir))]
    (swap! temp-dirs conj d)
    d))
(defn- cleanup! [] (doseq [d @temp-dirs] (fs/delete-tree d)))

(defn- feed-file
  ([dir rows] (feed-file dir rows {}))
  ([dir rows extra]
   (let [f (str (fs/path dir "uncertain-pressure.edn"))]
     (spit f (pr-str (merge {:at (java.util.Date.)
                             :generated-by "futon3c.inbox-zero.sweeper"
                             :interval-ms 1800000
                             :drilldown "/storage/operator-backlog.edn"
                             :repos rows}
                            extra)))
     f)))

(defn- row [root n]
  {:label "x" :root root :dirty-count n :untracked 0
   :remainder 0 :paths (vec (repeat n {:path "p" :mtime-ms 1}))})

(deftest missing-input-is-unavailable-never-zero
  (let [u (u-load "/nonexistent/n4.edn")]
    (is (= :missing (:status u)))
    (is (= [{:abs-path "/x"}] (u-merge [{:abs-path "/x"}] u))
        "no :uncertain key is minted for missing input")))

(deftest malformed-inputs-are-typed-never-fresh
  (let [dir (temp-dir)]
    ;; owner's executable counterexample: negative count
    (is (= :invalid-row
           (:reason (u-load (feed-file dir [{:root "/tmp" :dirty-count -7}])))))
    ;; missing timestamp
    (is (= :missing-timestamp
           (:reason (u-load (feed-file dir [(row dir 1)] {:at nil})))))
    ;; future timestamp
    (is (= :future-timestamp
           (:reason (u-load (feed-file dir [(row dir 1)]
                                       {:at (java.util.Date.
                                             (+ (System/currentTimeMillis)
                                                60000))})))))
    ;; non-positive interval
    (is (= :invalid-interval
           (:reason (u-load (feed-file dir [(row dir 1)] {:interval-ms 0})))))
    ;; untracked > dirty
    (is (= :invalid-row
           (:reason (u-load (feed-file dir [(assoc (row dir 1)
                                                   :untracked 5)])))))
    ;; duplicate canonical roots
    (is (= :duplicate-root
           (:reason (u-load (feed-file dir [(row dir 1) (row dir 1)])))))
    ;; unreadable EDN
    (let [f (str (fs/path dir "bad.edn"))]
      (spit f "{not edn")
      (is (= :unreadable (:reason (u-load f)))))))

(deftest fresh-input-merges-by-canonical-root-and-unions-extras
  (let [dir (temp-dir)
        target (str (fs/path dir "real"))
        _ (fs/create-dirs target)
        link (str (fs/path dir "alias"))]
    (fs/create-sym-link link target)
    (let [f (feed-file dir [(assoc (row link 10)
                                   :label "futon3c-d" :untracked 3 :remainder 5)
                            (assoc (row (str (fs/path dir "extra")) 7)
                                   :label "sweep-only")])
          _ (fs/create-dirs (str (fs/path dir "extra")))
          ;; rewrite feed now that extra/ exists (canonicalization needs it)
          f (feed-file dir [(assoc (row link 10)
                                   :label "futon3c-d" :untracked 3 :remainder 5)
                            (assoc (row (str (fs/path dir "extra")) 7)
                                   :label "sweep-only")])
          u (u-load f)
          merged (u-merge [{:abs-path target :repo "futon3c-d" :P 1.0}
                           {:abs-path (str target "-other") :repo "futon3c"}]
                          u)]
      (is (= :available (:status u)))
      (is (false? (:stale? u)))
      (is (= 3 (count merged)) "union: two manifest repos + one feed-only root")
      (is (= 10 (get-in (first merged) [:uncertain :dirty-count])))
      (is (nil? (:uncertain (second merged))) "similar label, different root: no merge")
      (let [extra (nth merged 2)]
        (is (true? (:uncertain-only extra)))
        (is (= "sweep-only" (:repo extra)))
        (is (nil? (:P extra)) "no invented pressure measurement")
        (is (= 7 (get-in extra [:uncertain :dirty-count])))))))

(deftest stale-input-is-merged-but-flagged-with-feed-interval
  (let [dir (temp-dir)
        f (feed-file dir [(row dir 4)]
                     {:at (java.util.Date. (- (System/currentTimeMillis)
                                              (* 3 1800000)))})
        u (u-load f)
        merged (first (u-merge [{:abs-path dir}] u))]
    (is (= :available (:status u)))
    (is (true? (:stale? u)))
    (is (= 4 (get-in merged [:uncertain :dirty-count])))
    (is (true? (:uncertain-stale merged)))))

(let [{:keys [fail error]} (run-tests)]
  (cleanup!)
  (System/exit (if (zero? (+ fail error)) 0 1)))
