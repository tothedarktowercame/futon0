(ns futon0.futonzero.cli
  "FutonZero CLI: capability trajectory observer.

   Usage:
     bb futonzero.clj observe <agent>
     bb futonzero.clj profile <agent> [--days N]
     bb futonzero.clj trajectory <agent> [--days N]
     bb futonzero.clj report <agent> [--days N]
     bb futonzero.clj agents [--days N]

   Invariant: D-I5 (read-only observer).
   Pattern:   corps/working-where-others-can-see"
  (:require [futon0.futonzero.observe :as observe]
            [futon0.futonzero.profile :as profile]
            [futon0.futonzero.trajectory :as trajectory]
            [futon0.futonzero.report :as report]
            [cheshire.core :as json])
  (:import (java.time Instant Duration)))

;; ---------------------------------------------------------------------------
;; Argument parsing
;; ---------------------------------------------------------------------------

(defn- parse-args [args]
  (loop [args args
         out  {:command nil :agent nil :days 7}]
    (if (empty? args)
      out
      (let [[flag & rest] args]
        (cond
          (= flag "--days")
          (recur (drop 1 rest) (assoc out :days (Integer/parseInt (first rest))))
          (nil? (:command out))
          (recur rest (assoc out :command flag))
          (nil? (:agent out))
          (recur rest (assoc out :agent flag))
          :else
          (recur rest out))))))

;; ---------------------------------------------------------------------------
;; Window computation
;; ---------------------------------------------------------------------------

(defn- window-bounds
  "Compute current and baseline window boundaries from days parameter."
  [days]
  (let [now      (Instant/now)
        current-from (.minus now (Duration/ofDays days))
        baseline-from (.minus now (Duration/ofDays (* 2 days)))
        baseline-to   current-from]
    {:current  {:from (str current-from) :to (str now)}
     :baseline {:from (str baseline-from) :to (str baseline-to)}}))

;; ---------------------------------------------------------------------------
;; Commands
;; ---------------------------------------------------------------------------

(defn cmd-observe
  "Observe current functionings for an agent."
  [agent days]
  (let [since (str (.minus (Instant/now) (Duration/ofDays days)))
        records (observe/observe-evidence agent since)
        hx-records (observe/observe-hyperedges)]
    (println (str "Evidence-derived functionings for " agent ": " (count records)))
    (println (str "Hyperedge-derived functionings (infrastructure): " (count hx-records)))
    (println)
    (when (seq records)
      (println "Functioning types observed:")
      (doseq [[t rs] (sort-by key (group-by :type records))]
        (println (str "  " t ": " (count rs)))))
    (when (empty? records)
      (println (str "No evidence entries found for agent '" agent "' in last " days " days.")))))

(defn cmd-agents
  "List all agents with evidence in the given window."
  [days]
  (let [since (str (.minus (Instant/now) (Duration/ofDays days)))
        agent-map (observe/observe-all-agents since)]
    (if (seq agent-map)
      (do
        (println (str "Agents with evidence in last " days " days:"))
        (println)
        (doseq [[agent records] (sort-by key agent-map)]
          (let [types (count (distinct (map :type records)))]
            (println (str "  " agent ": " (count records) " records, "
                          types " functioning types")))))
      (println "No agents found with evidence in the given window."))))

(defn cmd-profile
  "Build and display a capability profile for an agent."
  [agent days]
  (let [windows (window-bounds days)
        since   (get-in windows [:current :from])
        records (observe/observe-evidence agent since)]
    (if (seq records)
      (let [prof (profile/build-profile
                   agent
                   (get-in windows [:current :from])
                   (get-in windows [:current :to])
                   records)]
        (println (str "Capability Profile: " agent))
        (println (str "Window: " (get-in prof [:window :from]) " → " (get-in prof [:window :to])))
        (println (str "Functioning breadth: " (:functioning-breadth prof)))
        (println (str "Capability breadth:  " (:capability-breadth prof)
                      " (reproducible in ≥2 sessions)"))
        (println (str "Capability ratio:    " (format "%.2f" (double (:capability-ratio prof)))))
        (println (str "Column coverage:     " (:column-coverage prof)))
        (println)
        (println "Functioning types:")
        (doseq [[t s] (sort-by key (:functioning-types prof))]
          (println (str "  " t ": " (:count s)
                        " (" (:sessions s) " sessions"
                        (when (:reproducible? s) ", reproducible")
                        ")")))
        (println)
        (let [disc (:discipline prof)]
          (println "Discipline:")
          (println (str "  PSR: " (:psr-count disc) (when-not (:psr-available? disc) " (not available)")))
          (println (str "  PUR: " (:pur-count disc) (when-not (:pur-available? disc) " (not available)")))
          (println (str "  PAR: " (:par-count disc) (when-not (:par-available? disc) " (not available)")))
          (println (str "  Continuity: " (format "%.2f" (double (:continuity-ratio disc)))
                        " (" (:chain-completions disc) "/" (:chain-starts disc) " chains completed)")))

        ;; Save snapshot
        (let [path (report/write-profile prof)]
          (println)
          (println (str "Profile saved: " path))))
      (println (str "No evidence entries found for agent '" agent "' in last " days " days.")))))

(defn cmd-trajectory
  "Compute trajectory between baseline and current window."
  [agent days]
  (let [windows (window-bounds days)
        b-since (get-in windows [:baseline :from])
        c-since (get-in windows [:current :from])
        b-records (observe/observe-evidence agent b-since)
        c-records (observe/observe-evidence agent c-since)]
    ;; Split records into baseline and current windows
    (let [b-to (get-in windows [:baseline :to])
          baseline-recs (filter #(and (:timestamp %)
                                      (< (compare (:timestamp %) b-to) 0))
                                (or b-records []))
          current-recs  (filter #(and (:timestamp %)
                                      (>= (compare (:timestamp %) b-to) 0))
                                (or c-records []))]
      (if (empty? baseline-recs)
        (println (str "Insufficient baseline: no evidence for '" agent
                      "' in baseline window (" (get-in windows [:baseline :from])
                      " → " b-to ")."))
        (let [baseline (profile/build-profile
                         agent
                         (get-in windows [:baseline :from])
                         b-to
                         baseline-recs)
              current  (profile/build-profile
                         agent
                         (get-in windows [:current :from])
                         (get-in windows [:current :to])
                         current-recs)
              traj     (trajectory/trajectory-report baseline current)]

          ;; Write report
          (let [{:keys [markdown json]} (report/write-report traj)]
            (println (report/report-markdown traj))
            (println "---")
            (println (str "Report saved: " markdown))
            (println (str "JSON saved:   " json))))))))

(defn cmd-report
  "Alias for trajectory — generate full report."
  [agent days]
  (cmd-trajectory agent days))

;; ---------------------------------------------------------------------------
;; Main dispatch
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [{:keys [command agent days]} (parse-args args)]
    (case command
      "observe"    (if agent
                     (cmd-observe agent days)
                     (println "Usage: futonzero observe <agent> [--days N]"))
      "profile"    (if agent
                     (cmd-profile agent days)
                     (println "Usage: futonzero profile <agent> [--days N]"))
      "trajectory" (if agent
                     (cmd-trajectory agent days)
                     (println "Usage: futonzero trajectory <agent> [--days N]"))
      "report"     (if agent
                     (cmd-report agent days)
                     (println "Usage: futonzero report <agent> [--days N]"))
      "agents"     (cmd-agents days)
      (println "FutonZero — Capability Monitor for the Self-Representing Stack

Usage:
  futonzero observe <agent> [--days N]    Observe current functionings
  futonzero profile <agent> [--days N]    Build capability profile
  futonzero trajectory <agent> [--days N] Compute trajectory between windows
  futonzero report <agent> [--days N]     Generate full trajectory report
  futonzero agents [--days N]             List agents with evidence

Default window: 7 days. Baseline is the preceding window of equal size."))))
