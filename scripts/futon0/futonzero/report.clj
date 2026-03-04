(ns futon0.futonzero.report
  "Emit markdown and JSON capability trajectory reports.

   Pattern:   corps/working-where-others-can-see
   Theory:    Local artifacts — inspectable, diffable, archivable"
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import (java.io File)
           (java.time LocalDate)))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:private storage-root
  (str (System/getProperty "user.home") "/code/storage/futon0/futonzero"))

;; ---------------------------------------------------------------------------
;; Markdown report generation
;; ---------------------------------------------------------------------------

(defn- section [title body]
  (str "## " title "\n\n" body "\n\n"))

(defn- bullet [items]
  (str/join "\n" (map #(str "- " %) items)))

(defn- table [headers rows]
  (let [header-line (str "| " (str/join " | " headers) " |")
        sep-line    (str "| " (str/join " | " (map (fn [_] "---") headers)) " |")
        row-lines   (map (fn [row] (str "| " (str/join " | " row) " |")) rows)]
    (str/join "\n" (concat [header-line sep-line] row-lines))))

(defn report-markdown
  "Generate markdown trajectory report from a TrajectoryReport."
  [{:keys [agent baseline current delta baldwin warnings phase-expectations]}]
  (let [window-str (fn [p] (str (get-in p [:window :from] "?")
                                " → " (get-in p [:window :to] "?")))
        sb (StringBuilder.)]

    ;; Header
    (.append sb (str "# Capability Trajectory: " agent "\n\n"))
    (.append sb (str "**Generated:** " (str (LocalDate/now)) "\n"))
    (.append sb (str "**Baseline window:** " (window-str baseline) "\n"))
    (.append sb (str "**Current window:** " (window-str current) "\n\n"))

    ;; Summary
    (.append sb (section "Summary"
                  (str (table ["Metric" "Baseline" "Current" "Δ"]
                         [["Functioning breadth"
                           (str (:functioning-breadth baseline))
                           (str (:functioning-breadth current))
                           (str (:net-freedom-delta delta))]
                          ["Capability breadth"
                           (str (:capability-breadth baseline))
                           (str (:capability-breadth current))
                           (str (- (:capability-breadth current 0) (:capability-breadth baseline 0)))]
                          ["Capability ratio"
                           (format "%.2f" (double (:capability-ratio baseline 0)))
                           (format "%.2f" (double (:capability-ratio current 0)))
                           (format "%+.2f" (double (- (:capability-ratio current 0)
                                                      (:capability-ratio baseline 0))))]
                          ["Column coverage"
                           (str (count (:column-coverage baseline)))
                           (str (count (:column-coverage current)))
                           ""]]))))

    ;; Freedom expansion
    (.append sb (section "Freedom Expansion"
                  (if (pos? (:freedom-expansion delta))
                    (str "**+" (:freedom-expansion delta) " freedoms**\n\n"
                         (when (seq (:new-functionings delta))
                           (str "New functioning types:\n"
                                (bullet (:new-functionings delta)) "\n\n"))
                         (when (seq (:strengthened delta))
                           (str "Strengthened (now reproducible):\n"
                                (bullet (:strengthened delta)))))
                    "No new freedoms in this window.")))

    ;; Freedom regression
    (when (pos? (:freedom-regression delta))
      (.append sb (section "Freedom Regression"
                    (str "**-" (:freedom-regression delta) " freedoms**\n\n"
                         (when (seq (:lost-functionings delta))
                           (str "Lost functioning types:\n"
                                (bullet (:lost-functionings delta)) "\n\n"))
                         (when (seq (:weakened delta))
                           (str "Weakened (no longer reproducible):\n"
                                (bullet (:weakened delta))))))))

    ;; Baldwin phase
    (.append sb (section "Baldwin Phase"
                  (str "**" (name (:phase baldwin)) "**\n\n"
                       "Diversity rate: " (format "%.2f" (:diversity-rate baldwin)) "\n"
                       "Reproducibility rate: " (format "%.2f" (:reproducibility-rate baldwin)) "\n\n"
                       (when (seq (:signals baldwin))
                         (str (bullet (:signals baldwin)) "\n\n"))
                       "Expected at this phase: " (:expected phase-expectations ""))))

    ;; Warnings
    (when (seq warnings)
      (.append sb (section "Phase-Inappropriate Patterns"
                    (bullet warnings))))

    ;; Discipline
    (when-let [dd (:discipline-delta delta)]
      (.append sb (section "Discipline"
                    (let [disc (:discipline current)]
                      (str (table ["Metric" "Count" "Δ" "Available?"]
                             [["PSR filed" (str (:psr-count disc 0))
                               (str (:psr dd 0)) (str (:psr-available? disc false))]
                              ["PUR filed" (str (:pur-count disc 0))
                               (str (:pur dd 0)) (str (:pur-available? disc false))]
                              ["PAR completed" (str (:par-count disc 0))
                               (str (:par dd 0)) (str (:par-available? disc false))]
                              ["Continuity ratio" (format "%.2f" (double (:continuity-ratio disc 0)))
                               (format "%+.2f" (double (:continuity dd 0))) ""]]))))))

    ;; Functioning type detail
    (.append sb (section "Functioning Types (Current Window)"
                  (let [types (:functioning-types current)]
                    (if (seq types)
                      (table ["Type" "Count" "Sessions" "Reproducible?" "First" "Last"]
                             (map (fn [[t s]]
                                    [(str t)
                                     (str (:count s))
                                     (str (:sessions s))
                                     (if (:reproducible? s) "✓" "—")
                                     (or (:first-seen s) "?")
                                     (or (:last-seen s) "?")])
                                  (sort-by key types)))
                      "No functionings observed."))))

    (str sb)))

;; ---------------------------------------------------------------------------
;; JSON report
;; ---------------------------------------------------------------------------

(defn report-json
  "Generate JSON trajectory report from a TrajectoryReport."
  [trajectory-report]
  (json/generate-string trajectory-report))

;; ---------------------------------------------------------------------------
;; File output
;; ---------------------------------------------------------------------------

(defn- ensure-dir [^File dir]
  (when-not (.exists dir) (.mkdirs dir)))

(defn write-report
  "Write trajectory report as markdown and JSON to storage."
  [trajectory-report]
  (let [agent  (:agent trajectory-report)
        date   (str (LocalDate/now))
        dir    (File. (str storage-root "/trajectories/" agent))
        md-file  (File. dir (str date ".md"))
        json-file (File. dir (str date ".json"))]
    (ensure-dir dir)
    (spit md-file (report-markdown trajectory-report))
    (spit json-file (report-json trajectory-report))
    {:markdown (str md-file)
     :json     (str json-file)}))

;; ---------------------------------------------------------------------------
;; Profile snapshot output
;; ---------------------------------------------------------------------------

(defn write-profile
  "Write a CapabilityProfile snapshot to storage."
  [profile]
  (let [agent (:agent profile)
        date  (str (LocalDate/now))
        dir   (File. (str storage-root "/profiles/" agent))
        file  (File. dir (str date ".json"))]
    (ensure-dir dir)
    (spit file (json/generate-string profile))
    (str file)))
