(ns futon0.futonzero.profile
  "Aggregate FunctioningRecords into CapabilityProfiles and DisciplineProfiles.

   Invariant: C3 (reproducibility requires ≥2 distinct sessions).
   Pattern:   futon-theory/local-gain-persistence
   Theory:    Sen's capability approach — capability = reproducible functioning"
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; FunctioningStats aggregation
;; ---------------------------------------------------------------------------

(defn- functioning-stats
  "Compute FunctioningStats for a seq of FunctioningRecords of the same type."
  [records]
  (let [sessions    (->> records (map :session-id) (remove nil?) distinct count)
        timestamps  (->> records (map :timestamp) sort)
        conversions (->> records (map :conversion) (remove nil?))]
    {:count          (count records)
     :first-seen     (first timestamps)
     :last-seen      (last timestamps)
     :reproducible?  (>= sessions 2)
     :sessions       sessions
     :conversion-median (when (seq conversions)
                          ;; Median of conversion maps — take middle element
                          (nth (sort-by #(or (:duration-ms %) 0) conversions)
                               (quot (count conversions) 2)))}))

;; ---------------------------------------------------------------------------
;; DisciplineProfile
;; ---------------------------------------------------------------------------

(defn discipline-profile
  "Compute DisciplineProfile from FunctioningRecords in a window."
  [agent from to records]
  (let [by-type (group-by :type records)
        count-of (fn [t] (count (get by-type t [])))

        psr-count   (count-of "discipline/psr-filed")
        pur-count   (count-of "discipline/pur-filed")
        par-count   (count-of "discipline/par-completed")
        corrections (count-of "discipline/correction-made")

        ;; Chain analysis: sessions started vs completed
        starts      (count-of "project/session-started")
        completions (count-of "project/session-completed")
        continuity  (if (pos? starts)
                      (double (/ (min completions starts) starts))
                      0.0)]
    {:agent              agent
     :window             {:from from :to to}
     :psr-count          psr-count
     :pur-count          pur-count
     :par-count          par-count
     :correction-count   corrections
     :chain-starts       starts
     :chain-completions  completions
     :continuity-ratio   continuity
     ;; Availability flags (§D-3)
     :psr-available?     (pos? psr-count)
     :pur-available?     (pos? pur-count)
     :par-available?     (pos? par-count)}))

;; ---------------------------------------------------------------------------
;; CapabilityProfile
;; ---------------------------------------------------------------------------

(defn build-profile
  "Build a CapabilityProfile from FunctioningRecords for one agent in a window."
  [agent from to records]
  (let [;; Filter to window
        in-window (filter (fn [r]
                            (and (:timestamp r)
                                 (or (nil? from) (>= (compare (:timestamp r) from) 0))
                                 (or (nil? to) (<= (compare (:timestamp r) to) 0))))
                          records)
        ;; Group by functioning type
        by-type   (group-by :type in-window)
        stats-map (into {} (map (fn [[t rs]] [t (functioning-stats rs)]) by-type))

        ;; Summary metrics
        functioning-breadth (count stats-map)
        capability-breadth  (count (filter (fn [[_ s]] (:reproducible? s)) stats-map))
        capability-ratio    (if (pos? functioning-breadth)
                              (double (/ capability-breadth functioning-breadth))
                              0.0)
        columns-present     (->> in-window
                                 (map :column)
                                 (remove nil?)
                                 distinct
                                 set)]
    {:agent              agent
     :window             {:from from :to to}
     :functioning-types  stats-map
     :functioning-breadth functioning-breadth
     :capability-breadth  capability-breadth
     :capability-ratio    capability-ratio
     :column-coverage     columns-present
     :discipline          (discipline-profile agent from to in-window)
     :baldwin-phase       nil}))  ;; Set by trajectory module
