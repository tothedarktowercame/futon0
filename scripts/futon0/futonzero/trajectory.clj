(ns futon0.futonzero.trajectory
  "Compare two CapabilityProfiles to produce TrajectoryReports.

   Invariant: D-I1 (capability = reproducible functioning pattern).
   Pattern:   futon-theory/baldwin-cycle
   Theory:    Sen — freedom expansion is the core question"
  (:require [clojure.set :as set]))

;; ---------------------------------------------------------------------------
;; Baldwin phase detection (§D-5)
;; ---------------------------------------------------------------------------

(defn detect-baldwin-phase
  "Detect Baldwin cycle phase from a CapabilityProfile and its trajectory delta.
   Returns {:phase :exploring|:assimilating|:canalizing|:mixed
            :diversity-rate float
            :reproducibility-rate float
            :signals [string]}."
  [profile new-functioning-count invariant-trend]
  (let [breadth  (:functioning-breadth profile)
        cap-ratio (:capability-ratio profile)
        div-rate  (if (pos? breadth)
                    (double (/ new-functioning-count breadth))
                    0.0)
        signals   (atom [])]
    (cond
      ;; Exploring: high diversity, low reproducibility
      (and (> div-rate 0.3) (< cap-ratio 0.4))
      (do
        (swap! signals conj "High diversity rate, low reproducibility — trying many new things")
        {:phase :exploring
         :diversity-rate div-rate
         :reproducibility-rate cap-ratio
         :signals @signals})

      ;; Canalizing: high reproducibility, invariants stable or improving
      (and (>= cap-ratio 0.7) (>= (or invariant-trend 0) 0))
      (do
        (swap! signals conj "High reproducibility, invariants stable or improving")
        {:phase :canalizing
         :diversity-rate div-rate
         :reproducibility-rate cap-ratio
         :signals @signals})

      ;; Assimilating: moderate diversity, rising reproducibility
      (and (<= div-rate 0.3) (>= cap-ratio 0.4) (< cap-ratio 0.7))
      (do
        (swap! signals conj "Moderate diversity, rising reproducibility — consolidating gains")
        {:phase :assimilating
         :diversity-rate div-rate
         :reproducibility-rate cap-ratio
         :signals @signals})

      ;; Mixed
      :else
      {:phase :mixed
       :diversity-rate div-rate
       :reproducibility-rate cap-ratio
       :signals ["Ambiguous phase — mixed signals across indicators"]})))

;; ---------------------------------------------------------------------------
;; Phase-appropriate expectations (§D-5)
;; ---------------------------------------------------------------------------

(def phase-expectations
  "What's expected and concerning at each Baldwin phase."
  {:exploring
   {:expected    "High conversion cost, low reproducibility, new types appearing"
    :concerning  ["No new functioning types (stasis)"
                  "Very high correction rate (thrashing)"]}
   :assimilating
   {:expected    "Rising reproducibility, stabilising conversion cost"
    :concerning  ["Lost functionings (regression)"
                  "Weakened capabilities"]}
   :canalizing
   {:expected    "Low conversion cost, high reproducibility, invariant improvement"
    :concerning  ["New invariant violations"
                  "Rising conversion cost"
                  "Regression in reproducibility"]}
   :mixed
   {:expected    "Ambiguous — inspect components individually"
    :concerning  []}})

(defn- check-phase-concerns
  "Check for phase-inappropriate patterns. Returns list of warning strings."
  [phase delta]
  (let [warnings (atom [])]
    (case phase
      :exploring
      (do
        (when (empty? (:new-functionings delta))
          (swap! warnings conj "⚠ Stasis during exploration: no new functioning types appeared"))
        (when (> (count (:conversion-worsened delta)) 3)
          (swap! warnings conj "⚠ Thrashing during exploration: conversion worsened on multiple types")))

      :assimilating
      (do
        (when (seq (:lost-functionings delta))
          (swap! warnings conj (str "⚠ Regression during assimilation: lost "
                                    (count (:lost-functionings delta)) " functioning types")))
        (when (seq (:weakened delta))
          (swap! warnings conj (str "⚠ Weakening during assimilation: "
                                    (count (:weakened delta)) " types lost reproducibility"))))

      :canalizing
      (do
        (when (neg? (or (:invariant-net delta) 0))
          (swap! warnings conj "⚠ Invariant regression during canalization: new violations appeared"))
        (when (seq (:weakened delta))
          (swap! warnings conj (str "⚠ Regression during canalization: "
                                    (pr-str (:weakened delta))
                                    " weakened from reproducible to one-shot")))
        (when (seq (:conversion-worsened delta))
          (swap! warnings conj "⚠ Rising conversion cost during canalization")))

      ;; :mixed — no specific expectations
      nil)
    @warnings))

;; ---------------------------------------------------------------------------
;; Trajectory delta computation (§D-4)
;; ---------------------------------------------------------------------------

(defn compute-delta
  "Compute TrajectoryDelta between baseline and current CapabilityProfiles."
  [baseline current]
  (let [b-types (set (keys (:functioning-types baseline)))
        c-types (set (keys (:functioning-types current)))

        new-fns   (vec (set/difference c-types b-types))
        lost-fns  (vec (set/difference b-types c-types))
        common    (set/intersection b-types c-types)

        ;; Reproducibility transitions
        strengthened (vec (filter
                           (fn [t]
                             (and (not (get-in baseline [:functioning-types t :reproducible?]))
                                  (get-in current [:functioning-types t :reproducible?])))
                           common))
        weakened     (vec (filter
                           (fn [t]
                             (and (get-in baseline [:functioning-types t :reproducible?])
                                  (not (get-in current [:functioning-types t :reproducible?]))))
                           common))

        ;; Conversion changes (duration-ms comparison)
        conversion-improved (vec (filter
                                   (fn [t]
                                     (let [b-dur (get-in baseline [:functioning-types t :conversion-median :duration-ms])
                                           c-dur (get-in current [:functioning-types t :conversion-median :duration-ms])]
                                       (and b-dur c-dur (< c-dur b-dur))))
                                   common))
        conversion-worsened (vec (filter
                                   (fn [t]
                                     (let [b-dur (get-in baseline [:functioning-types t :conversion-median :duration-ms])
                                           c-dur (get-in current [:functioning-types t :conversion-median :duration-ms])]
                                       (and b-dur c-dur (> c-dur b-dur))))
                                   common))

        ;; Freedom metrics
        expansion  (+ (count new-fns) (count strengthened))
        regression (+ (count lost-fns) (count weakened))

        ;; Discipline delta
        b-disc (:discipline baseline)
        c-disc (:discipline current)
        disc-delta (when (and b-disc c-disc)
                     {:psr       (- (:psr-count c-disc 0) (:psr-count b-disc 0))
                      :pur       (- (:pur-count c-disc 0) (:pur-count b-disc 0))
                      :par       (- (:par-count c-disc 0) (:par-count b-disc 0))
                      :continuity (- (:continuity-ratio c-disc 0.0)
                                    (:continuity-ratio b-disc 0.0))})

        ;; Invariant delta placeholder (would need separate query)
        invariant-net 0]

    {:new-functionings     new-fns
     :lost-functionings    lost-fns
     :strengthened         strengthened
     :weakened             weakened
     :conversion-improved  conversion-improved
     :conversion-worsened  conversion-worsened
     :freedom-expansion    expansion
     :freedom-regression   regression
     :net-freedom-delta    (- expansion regression)
     :discipline-delta     disc-delta
     :invariant-net        invariant-net}))

;; ---------------------------------------------------------------------------
;; TrajectoryReport
;; ---------------------------------------------------------------------------

(defn trajectory-report
  "Build a TrajectoryReport comparing baseline and current CapabilityProfiles."
  [baseline current]
  (let [delta         (compute-delta baseline current)
        invariant-trend (:invariant-net delta)
        baldwin       (detect-baldwin-phase current
                                            (count (:new-functionings delta))
                                            invariant-trend)
        phase         (:phase baldwin)
        warnings      (check-phase-concerns phase delta)
        current'      (assoc current :baldwin-phase baldwin)]
    {:agent      (:agent current)
     :baseline   baseline
     :current    current'
     :delta      delta
     :baldwin    baldwin
     :warnings   warnings
     :phase-expectations (get phase-expectations phase)}))
