(ns futon0.futonzero.rollout-ledger-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [futon0.futonzero.rollout-ledger :as ledger]))

(defn- tmp-dir []
  (doto (java.io.File/createTempFile "futonzero-ledger-" "")
    (.delete)
    (.mkdirs)))

(defn- write-edn [path x]
  (spit path (pr-str x))
  path)

(def fixture-evidence
  [{:kind :gamma-frame
    :at "2026-06-10"
    :predicted -5.0
    :realised -4.5
    :error 0.5
    :success nil
    :source "/canonical/traces/run.edn"
    :ref "run-1"
    :independent? true
    :witness-class :none}
   {:kind :pilots-log-turn
    :at "2026-06-10"
    :predicted -5.69
    :realised nil
    :error nil
    :success nil
    :source "/canonical/PILOTS.md"
    :ref "turn-1"
    :witness-class :operator-gate}
   {:kind :closure-fold
    :at nil
    :predicted nil
    :realised nil
    :error nil
    :success true
    :source "/canonical/closure-folds.edn"
    :ref "fold-ok"
    :witness-class :build-discharge}
   {:kind :ch2-discharge
    :at "t"
    :predicted nil
    :realised nil
    :error nil
    :success true
    :source "/canonical/ch2.edn"
    :ref "move-1"
    :witness-class :build-discharge}
   {:kind :discipline-event
    :at "t2"
    :predicted -2.0
    :realised nil
    :error nil
    :success false
    :source "/canonical/discipline.edn"
    :ref "run-d"
    :witness-class :discipline-event}])

(def fixture-report
  {:n-by-kind {:gamma-frame 1
               :pilots-log-turn 1
               :closure-fold 1
               :ch2-discharge 1
               :discipline-event 1}
   :paired-count 1
   :independent-paired-count 1
   :error-stats {:zero-count 0
                 :nonzero-count 1
                 :max 0.5
                 :mean 0.5}
   :degenerate? false
   :verdict :insufficient-evidence
   :warnings []})

(defn- fixture []
  (let [root (tmp-dir)
        evidence-path (io/file root "futonzero-calibration-evidence.edn")]
    (write-edn evidence-path {:evidence fixture-evidence
                              :report fixture-report})
    {:root root
     :evidence-path (.getPath evidence-path)}))

(deftest ledger-entry-invariants
  (let [{:keys [root evidence-path]} (fixture)]
    (try
      (let [entries (ledger/build-ledger {:evidence-path evidence-path})]
        (is (= 5 (count entries)))
        (is (every? :ledger/id entries))
        (is (every? :playout/proposed entries))
        (is (every? :predicted entries))
        (is (every? :actual entries))
        (is (every? :witness/class entries))
        (is (every? :calibration entries))
        (is (= #{:none :operator-gate :build-discharge :discipline-event}
               (set (map :witness/class entries))))
        (is (not-any? #(= :witness-discharge (:witness/class %)) entries)
            "static build evidence must not be laundered into witnessed fruit"))
      (finally
        (doseq [f (reverse (file-seq root))] (.delete f))))))

(deftest calibration-audit-cross-checks-canonical-report
  (let [{:keys [root evidence-path]} (fixture)]
    (try
      (let [entries (ledger/build-ledger {:evidence-path evidence-path})
            report (ledger/calibration-audit entries)]
        (is (= 5 (:entry-count report)))
        (is (= (:n-by-kind fixture-report) (:count-by-source report)))
        (is (= (:paired-count fixture-report) (:paired-G-count report)))
        (is (= (:degenerate? fixture-report) (:degenerate? report)))
        (is (= (:verdict fixture-report) (:verdict report)))
        (is (= {:none 1
                :operator-gate 1
                :build-discharge 2
                :discipline-event 1}
               (:count-by-witness report)))
        (is (true? (get-in report [:cross-check :entry-count-matches?])))
        (is (true? (get-in report [:cross-check :source-counts-match?])))
        (is (= "The futon0/data/futonzero-*.edn artifacts are SNAPSHOTS regenerated at charter checkpoints, NOT live; the live calibration view is the canonical reader (futon3c.aif.calibration)."
               (get-in report [:regeneration :staleness])))
        (is (= "charter-home governs where the ARTIFACTS live (futon0), not where the AUDIT CODE runs (futon3c)."
               (get-in report [:regeneration :principle])))
        (is (= ["cd /home/joe/code/futon3c && clojure -M -m futon3c.aif.calibration --emit /home/joe/code/futon0/data/futonzero-calibration-evidence.edn"
                "cd /home/joe/code/futon0 && clojure -M -m futon0.futonzero.rollout-ledger"]
               (mapv :command (get-in report [:regeneration :command-pair]))))
        (is (true? (:read-only? report)))
        (is (= :G-SIM (:gate report))))
      (finally
        (doseq [f (reverse (file-seq root))] (.delete f))))))

(deftest write-ledger-and-report-roundtrip
  (let [{:keys [root evidence-path]} (fixture)
        ledger-path (.getPath (io/file root "ledger.edn"))
        report-path (.getPath (io/file root "report.edn"))]
    (try
      (let [result (ledger/write-ledger-and-report! {:evidence-path evidence-path
                                                     :ledger-path ledger-path
                                                     :report-path report-path})]
        (is (= 5 (:ledger-count result)))
        (is (= 5 (count (edn/read-string (slurp ledger-path)))))
        (is (= (:report result) (edn/read-string (slurp report-path)))))
      (finally
        (doseq [f (reverse (file-seq root))] (.delete f))))))

(deftest real-evidence-audit-cross-checks-canonical-edn
  (let [{:keys [evidence report]} (ledger/read-evidence-artifact)
        entries (ledger/build-ledger)]
    (is (= (count evidence) (count entries)))
    (is (= (:n-by-kind report) (:count-by-source (ledger/calibration-audit entries))))
    (is (= (:degenerate? report) (:degenerate? (ledger/calibration-audit entries))))
    (is (= (frequencies (map :witness-class evidence))
           (:count-by-witness (ledger/calibration-audit entries))))))
