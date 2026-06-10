(ns futon0.futonzero.rollout-ledger
  "Read-only static rollout ledger for M-futonzero-generative §4.1/§4.4.

   This is a G-SIM pre-gate artifact: futon0 consumes the canonical
   futon3c.aif.calibration evidence seam and reshapes it into the charter
   ledger/report artifacts. It does not re-parse the source channels, update
   WM behavior, update policy, or treat operator approval as proof of fruit."
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def default-evidence-path "data/futonzero-calibration-evidence.edn")
(def default-ledger-path "data/futonzero-rollout-ledger.edn")
(def default-report-path "data/futonzero-calibration-report.edn")

(def regeneration-doc
  {:command-pair
   [{:step :canonical-reader
     :command
     "cd /home/joe/code/futon3c && clojure -M -m futon3c.aif.calibration --emit /home/joe/code/futon0/data/futonzero-calibration-evidence.edn"
     :writes "canonical evidence EDN"}
    {:step :futon0-post-process
     :command
     "cd /home/joe/code/futon0 && clojure -M -m futon0.futonzero.rollout-ledger"
     :writes "§4.1 ledger and §4.4 report artifacts"}]
   :staleness
   "The futon0/data/futonzero-*.edn artifacts are SNAPSHOTS regenerated at charter checkpoints, NOT live; the live calibration view is the canonical reader (futon3c.aif.calibration)."
   :principle
   "charter-home governs where the ARTIFACTS live (futon0), not where the AUDIT CODE runs (futon3c)."})

(defn- read-edn [path]
  (edn/read-string (slurp path)))

(defn- ledger-id [& parts]
  (->> parts
       (remove nil?)
       (map str)
       (str/join "/")))

(defn read-evidence-artifact
  ([] (read-evidence-artifact default-evidence-path))
  ([path]
   (let [{:keys [evidence report] :as artifact} (read-edn path)]
     (when-not (vector? evidence)
       (throw (ex-info "canonical evidence artifact must contain vector :evidence"
                       {:path path :keys (keys artifact)})))
     (when-not (map? report)
       (throw (ex-info "canonical evidence artifact must contain map :report"
                       {:path path :keys (keys artifact)})))
     artifact)))

(defn- canonical-action [record]
  {:type (:kind record)
   :target (:ref record)})

(defn- evidence-entry [idx record]
  (let [pred (:predicted record)
        realised (:realised record)]
    {:ledger/id (ledger-id "canonical" idx (:kind record) (:ref record))
     :playout/proposed {:action (canonical-action record)
                        :run-id (:ref record)
                        :at (:at record)}
     :predicted {:discharge-G pred
                 :effect-class (:kind record)
                 :top-shift? nil}
     :actual {:discharge-G realised
              :addressed? (:success record)
              :top-shift? nil
              :evidence-ref (:source record)}
     :witness/class (:witness-class record)
     :calibration {:G-error (when (and (number? pred) (number? realised))
                              (- pred realised))
                   :abs-G-error (:error record)
                   :top-shift-correct? nil}
     :canonical {:kind (:kind record)
                 :ref (:ref record)
                 :independent? (boolean (:independent? record))}
     :source (:kind record)}))

(defn build-ledger
  ([] (build-ledger {:evidence-path default-evidence-path}))
  ([{:keys [evidence-path evidence report]
     :or {evidence-path default-evidence-path}}]
   (let [{artifact-evidence :evidence artifact-report :report}
         (if evidence
           {:evidence evidence :report report}
           (read-evidence-artifact evidence-path))
         ledger (mapv evidence-entry (range) artifact-evidence)]
     (cond-> ledger
       artifact-report (with-meta {:canonical-report artifact-report})))))

(defn- mean [xs]
  (when (seq xs)
    (/ (reduce + 0.0 xs) (double (count xs)))))

(defn- witness-counts [ledger]
  (frequencies (map :witness/class ledger)))

(defn- canonical-report [ledger]
  (:canonical-report (meta ledger)))

(defn calibration-audit
  ([ledger] (calibration-audit ledger (canonical-report ledger)))
  ([ledger canonical]
   (let [g-errors (keep #(get-in % [:calibration :G-error]) ledger)
         abs-errors (keep #(get-in % [:calibration :abs-G-error]) ledger)
         counts-by-kind (frequencies (map :source ledger))
         counts-by-witness (witness-counts ledger)
         canonical-entry-count (reduce + 0 (vals (:n-by-kind canonical)))
         cross-check {:entry-count-matches? (= (count ledger) canonical-entry-count)
                      :source-counts-match? (= counts-by-kind (:n-by-kind canonical))
                      :degenerate? (:degenerate? canonical)
                      :verdict (:verdict canonical)
                      :witness-counts counts-by-witness}]
     {:entry-count (count ledger)
      :count-by-source counts-by-kind
      :count-by-witness counts-by-witness
      :paired-G-count (or (:paired-count canonical) (count g-errors))
      :mean-G-error (or (mean g-errors) 0.0)
      :mean-abs-G-error (or (mean abs-errors)
                            (mean (map #(Math/abs (double %)) g-errors))
                            0.0)
      :max-abs-G-error (or (get-in canonical [:error-stats :max])
                           (when (seq abs-errors) (apply max abs-errors))
                           0.0)
      :top-shift-paired-count 0
      :top-shift-accuracy nil
      :read-only? true
      :gate :G-SIM
      :degenerate? (:degenerate? canonical)
      :verdict (:verdict canonical)
      :regeneration regeneration-doc
      :canonical-report canonical
      :cross-check cross-check})))

(defn write-edn! [path value]
  (let [file (io/file path)]
    (when-let [parent (.getParentFile file)]
      (.mkdirs parent))
    (spit file (with-out-str (pprint/pprint value)))
    value))

(defn write-ledger-and-report!
  ([] (write-ledger-and-report! {}))
  ([{:keys [evidence-path ledger-path report-path]
     :or {evidence-path default-evidence-path
          ledger-path default-ledger-path
          report-path default-report-path}}]
   (let [{:keys [evidence report]} (read-evidence-artifact evidence-path)
         ledger (build-ledger {:evidence evidence :report report})
         audit (calibration-audit ledger report)]
     (write-edn! ledger-path ledger)
     (write-edn! report-path audit)
     {:evidence-path evidence-path
      :ledger-path ledger-path
      :report-path report-path
      :ledger-count (count ledger)
      :report audit})))

(defn -main [& _]
  (prn (write-ledger-and-report!)))
