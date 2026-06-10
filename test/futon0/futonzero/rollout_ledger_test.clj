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

(defn- fixture []
  (let [root (tmp-dir)
        traces (doto (io/file root "traces") (.mkdirs))
        pilots (io/file root "PILOTS.md")
        folds (io/file root "closure.edn")
        ch2 (io/file root "ch2.edn")
        discipline (io/file root "discipline.edn")]
    (write-edn (io/file traces "run.edn")
               {:date "2026-06-10"
                :run-id "run-1"
                :trace [{:v {:type :open-mission :target "M-x"}
                         :prediction-error 0.5
                         :predicted-discharge -5.0
                         :realised-discharge -4.5
                         :delta-∇? true}]})
    (spit pilots (str "## Turn 1 — fixture\n"
                       "**READ.** Live WM recommended **`open-mission M-y`** (G=−5.69).\n"
                       "**PRINT / FOUND.** predicted G=-3.0; realised G=-2.75\n"))
    (write-edn folds [{:scope "fold-ok" :success true :used ["p"]}
                       {:scope "fold-no" :success false :used []}])
    (spit ch2 (str (pr-str {:move/id "move-1"
                            :discharged? true
                            :at "t"
                            :sorry-ref "sorry/ref"}) "\n"))
    (spit discipline (str (pr-str {:discipline/event :teleport-refused
                                   :run-id "run-1"
                                   :at "t"
                                   :action {:type :open-mission :target "M-y"}
                                   :predicted -2.0
                                   :note "fixture"}) "\n"))
    {:root root
     :inputs {:traces-dir (.getPath traces)
              :pilots-log (.getPath pilots)
              :closure-folds (.getPath folds)
              :ch2-events (.getPath ch2)
              :discipline-events (.getPath discipline)}}))

(deftest ledger-entry-invariants
  (let [{:keys [root inputs]} (fixture)]
    (try
      (let [entries (ledger/build-ledger inputs)]
        (is (= 7 (count entries)))
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

(deftest calibration-audit-computes-real-errors
  (let [{:keys [root inputs]} (fixture)]
    (try
      (let [entries (ledger/build-ledger inputs)
            report (ledger/calibration-audit entries)]
        (is (= 7 (:entry-count report)))
        (is (= 2 (:paired-G-count report)))
        (is (= -0.375 (:mean-G-error report)))
        (is (= 0.375 (:mean-abs-G-error report)))
        (is (= 1.0 (:top-shift-accuracy report)))
        (is (true? (:read-only? report)))
        (is (= :G-SIM (:gate report))))
      (finally
        (doseq [f (reverse (file-seq root))] (.delete f))))))

(deftest write-ledger-and-report-roundtrip
  (let [{:keys [root inputs]} (fixture)
        ledger-path (.getPath (io/file root "ledger.edn"))
        report-path (.getPath (io/file root "report.edn"))]
    (try
      (let [result (ledger/write-ledger-and-report! {:inputs inputs
                                                     :ledger-path ledger-path
                                                     :report-path report-path})]
        (is (= 7 (:ledger-count result)))
        (is (= 7 (count (edn/read-string (slurp ledger-path)))))
        (is (= (:report result) (edn/read-string (slurp report-path)))))
      (finally
        (doseq [f (reverse (file-seq root))] (.delete f))))))

(deftest real-evidence-audit-has-repl-traces
  (let [entries (ledger/build-ledger)]
    (is (pos? (count entries)))
    (is (pos? (get (frequencies (map :source entries)) :repl-trace 0)))
    (is (<= 2 (get (frequencies (map :source entries)) :pilots-log 0))
        "PILOTS-LOG Turn 1/2 READ recommendations should be present as operator-gate records")
    (is (pos? (:paired-G-count (ledger/calibration-audit entries))))))
