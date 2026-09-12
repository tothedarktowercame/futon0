#!/usr/bin/env bb
;; inbox-zero-delta — status-CHANGE notifier for the hourly cleanliness gate.
;;
;; Problem it fixes (audit 2026-09-12): the hourly check-clean gate has been
;; FAILing into journald since 2026-09-01 — 248 consecutive failures, OnFailure
;; empty, no consumer. A permanently red lamp with nobody watching is the
;; inverted form of the "monitor that degrades toward fine" failure this
;; README documents: the signal exists but cannot be seen.
;;
;; This runs AFTER each gate run (wired via OnSuccess=/OnFailure= so it fires
;; either way) and reports only CHANGES: a repo newly failing, a repo
;; recovered, or the stack reaching clean. Steady state is silent, which is
;; what makes a change visible.
;;
;;   scripts/inbox-zero-delta.bb [--json-report PATH]
;;
;; State: data/inbox-zero-delta-state.json (last per-repo failure signature).
;; Log:   data/inbox-zero-delta.log (one line per change, kept).
;; Notify: notify-send when available (same fallback-to-stdout behaviour as
;;         scripts/negative_space_notify.py); plain print otherwise.

(require '[babashka.fs :as fs]
         '[babashka.process :as proc]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def repo-root (str (fs/parent (fs/parent (fs/real-path *file*)))))
(def state-path (str (fs/path repo-root "data" "inbox-zero-delta-state.json")))
(def log-path (str (fs/path repo-root "data" "inbox-zero-delta.log")))
(def report-path (or (some-> (drop-while #(not= "--json-report" %) *command-line-args*)
                             second
                             not-empty)
                     "/tmp/inbox-zero-report.json"))

(defn- run-gate []
  ;; The gate exits 1 on dirty; that IS its report. We only want the JSON.
  (let [res (proc/shell {:continue true :out :string :err :string}
                        "/usr/local/bin/bb"
                        (str (fs/path repo-root "scripts" "futon-sync.clj"))
                        "check-clean" "--json")]
    (if (str/blank? (:out res))
      (do (binding [*out* *err*] (println "gate produced no stdout:" (:err res)))
          nil)
      (try (json/parse-string (:out res) true)
           (catch Exception e
             (binding [*out* *err*] (println "unparseable gate output:" (ex-message e)))
             nil)))))

(defn- signature [report]
  ;; repo -> sorted set of failure clause/reason strings. INFO lines are NOT
  ;; part of the signature: they churn (worktree lists, off-tree frames) and
  ;; would make every hour look like a change.
  (into (sorted-map)
        (map (fn [r] [(:repo r)
                      (vec (sort (map #(str (:clause %) "/" (:reason %))
                                     (:failures r))))]))
        (:repos report)))

(defn- diff [old new]
  (let [changed (for [[repo sig] new
                      :let [prev (get old repo ::absent)]
                      :when (not= prev sig)]
                  (cond
                    (= sig []) {:repo repo :kind :recovered}
                    (= prev ::absent) {:repo repo :kind :new-failure :sig sig}
                    :else {:repo repo :kind :changed :sig sig}))]
    (vec (sort-by :repo changed))))

(defn- append-log! [lines]
  (spit log-path (str/join "\n" lines) :append true)
  (spit log-path "\n" :append true))

(defn- notify! [title body]
  (let [exe (-> (proc/shell {:continue true :out :string} "which notify-send") :out str/trim)]
    (if (str/blank? exe)
      (println title "-" body)
      (proc/shell {:continue true} exe "--urgency" "critical" title body))))

(defn -main []
  (fs/create-dirs (fs/parent (fs/path state-path)))
  (if-let [report (run-gate)]
    (let [new (signature report)
          ;; String keys, matching the gate's string repo labels (keywordizing
          ;; here made every lookup ::absent, so each run reported the whole
          ;; stack as newly failed -- caught during bring-up 2026-09-12).
          old (when (fs/exists? state-path)
                (try (get (json/parse-string (slurp state-path)) "signature")
                     (catch Exception _ nil)))
          changes (if old (diff old new) [])
          clean? (every? empty? (vals new))]
      (cond
        (nil? old)
        (do (spit state-path (json/generate-string
                              {:signature new :at (str (java.time.Instant/now))}))
            (println "baseline recorded;"
                     (count (filter seq (vals new))) "repo(s) failing"))

        (seq changes)
        (let [lines (for [{:keys [repo kind sig]} changes]
                      (str (str/replace (str (java.time.Instant/now)) #"T" " ")
                           " " (name kind) " " repo
                           (when (seq sig) (str " [" (str/join ", " sig) "]"))))
              clean-now? (and clean? (seq changes))]
          (append-log! lines)
          (notify! (if clean-now? "Inbox zero: stack CLEAN" "Inbox zero status changed")
                   (str/join "; " (map #(str (:repo %) " " (name (:kind %))) changes)))
          (println (str/join "\n" lines))
          (spit state-path (json/generate-string
                            {:signature new :at (str (java.time.Instant/now))})))

        :else
        (println "no change;" (count (filter seq (vals new))) "repo(s) still failing")))
    (System/exit 2)))

(-main)
