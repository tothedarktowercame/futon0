(ns futon0.futonzero.rollout-ledger
  "Read-only static rollout ledger for M-futonzero-generative §4.1/§4.4.

   This is a G-SIM pre-gate artifact: it records prior play-out -> outcome
   evidence and audits calibration without changing WM behavior, updating any
   policy, or treating operator approval as proof of fruit. G-REWARD discipline:
   build discharges, witness discharges, operator gates, and none are distinct."
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def default-inputs
  {:traces-dir "/home/joe/code/futon3c/data/repl-traces"
   :pilots-log "/home/joe/code/futon3c/holes/PILOTS-LOG.md"
   :closure-folds "/home/joe/code/futon6/holes/closure-folds.edn"
   :ch2-events "/home/joe/code/futon3a/data/ch2-discharge-events.edn"
   :discipline-events "/home/joe/code/futon3c/data/discipline-events.edn"})

(def default-ledger-path "data/futonzero-rollout-ledger.edn")
(def default-report-path "data/futonzero-calibration-report.edn")

(defn- parse-number [x]
  (cond
    (number? x) (double x)
    (string? x) (try (Double/parseDouble (-> x
                                             (str/replace "−" "-")
                                             (str/replace #"[,)]$" "")
                                             str/trim))
                    (catch Throwable _ nil))
    :else nil))

(defn- maps-in [x]
  (filter map? (tree-seq coll? seq x)))

(defn- read-edn [path]
  (edn/read-string (slurp path)))

(defn- safe-read-edn [path]
  (try (read-edn path)
       (catch Throwable _ nil)))

(defn- action-from-gamma [m]
  (or (:v m)
      (get-in m [:artefact :proposed-action])
      (get-in m [:dT-snapshot 0 :action])
      {:type :unknown :target nil}))

(defn- ledger-id [& parts]
  (->> parts
       (remove nil?)
       (map str)
       (str/join "/")))

(defn- gamma-entry [source data m]
  (let [run-id (or (:run-id m) (:run-id data) (.replace (.getName (io/file source)) ".edn" ""))
        pred (parse-number (:predicted-discharge m))
        real (parse-number (:realised-discharge m))
        top-shift? (boolean (:delta-∇? m))]
    {:ledger/id (ledger-id "repl-trace" run-id (:cg-id m) (:step m))
     :playout/proposed {:action (action-from-gamma m)
                        :run-id run-id
                        :at (or (:at m) (:date data))}
     :predicted {:discharge-G pred
                 :effect-class :supervised-proposal
                 :top-shift? top-shift?}
     :actual {:discharge-G real
              :addressed? false
              :top-shift? top-shift?
              :evidence-ref (str source)}
     :witness/class :none
     :calibration {:G-error (when (and pred real) (- pred real))
                   :top-shift-correct? true}
     :source :repl-trace}))

(defn repl-trace-ledger [traces-dir]
  (let [dir (io/file traces-dir)]
    (if-not (.isDirectory dir)
      []
      (->> (file-seq dir)
           (filter #(.isFile %))
           (filter #(str/ends-with? (.getName %) ".edn"))
           (sort-by #(.getName %))
           (mapcat (fn [file]
                     (let [data (safe-read-edn (.getPath file))]
                       (->> (maps-in data)
                            (filter #(and (contains? % :predicted-discharge)
                                          (contains? % :realised-discharge)))
                            (map #(gamma-entry (.getPath file) data %))))))
           vec))))

(def ^:private pilot-g-re
  #"(?i)(?:predicted\s+G\s*=\s*([-+−]?[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?)).*?(?:realised\s+G\s*=\s*([-+−]?[0-9]+(?:\.[0-9]+)?(?:[eE][-+]?[0-9]+)?))")

(defn pilots-log-ledger [path]
  (let [file (io/file path)]
    (if-not (.exists file)
      []
      (->> (str/split-lines (slurp file))
           (keep-indexed
            (fn [idx line]
              (when-let [[_ p r] (re-find pilot-g-re line)]
                (let [pred (parse-number p)
                      real (parse-number r)]
                  {:ledger/id (ledger-id "pilots-log" (inc idx))
                   :playout/proposed {:action {:type :unknown :target nil}
                                      :run-id nil
                                      :at nil}
                   :predicted {:discharge-G pred
                               :effect-class :pilot-log
                               :top-shift? nil}
                   :actual {:discharge-G real
                            :addressed? nil
                            :top-shift? nil
                            :evidence-ref (str path ":" (inc idx))}
                   :witness/class :operator-gate
                   :calibration {:G-error (when (and pred real) (- pred real))
                                 :top-shift-correct? nil}
                   :source :pilots-log}))))
           vec))))

(defn closure-fold-ledger [path]
  (let [data (safe-read-edn path)]
    (->> data
         (filter map?)
         (map-indexed
          (fn [idx m]
            (let [success? (boolean (:success m))]
              {:ledger/id (ledger-id "closure-fold" (or (:scope m) idx))
               :playout/proposed {:action {:type :closure-fold :target (:scope m)}
                                  :run-id nil
                                  :at nil}
               :predicted {:discharge-G nil
                           :effect-class :pattern-fold
                           :top-shift? nil}
               :actual {:discharge-G nil
                        :addressed? success?
                        :top-shift? nil
                        :evidence-ref (str path)}
               :witness/class (if success? :build-discharge :none)
               :calibration {:G-error nil :top-shift-correct? nil}
               :source :closure-fold})))
         vec)))

(defn- edn-lines [path]
  (let [file (io/file path)]
    (if-not (.exists file)
      []
      (with-open [r (io/reader file)]
        (->> (line-seq r)
             (remove str/blank?)
             (map edn/read-string)
             vec)))))

(def allowed-discipline-events
  #{:teleport-refused :guardrail-trip :operator-decline :operator-merge})

(defn ch2-ledger [path]
  (->> (edn-lines path)
       (filter map?)
       (mapv (fn [m]
               (let [discharged? (boolean (:discharged? m))]
                 {:ledger/id (ledger-id "ch2" (:move/id m))
                  :playout/proposed {:action {:type :ch2-discharge
                                              :target (:move/id m)}
                                     :run-id nil
                                     :at (:at m)}
                  :predicted {:discharge-G nil
                              :effect-class :static-discharge
                              :top-shift? nil}
                  :actual {:discharge-G nil
                           :addressed? discharged?
                           :top-shift? nil
                           :evidence-ref (:sorry-ref m)}
                  :witness/class (if discharged? :build-discharge :none)
                  :calibration {:G-error nil :top-shift-correct? nil}
                  :source :ch2-discharge})))))

(defn discipline-ledger [path]
  (->> (edn-lines path)
       (filter map?)
       (filter #(contains? allowed-discipline-events (:discipline/event %)))
       (mapv (fn [m]
               {:ledger/id (ledger-id "discipline" (:run-id m) (:discipline/event m))
                :playout/proposed {:action (:action m)
                                   :run-id (:run-id m)
                                   :at (:at m)}
                :predicted {:discharge-G (parse-number (:predicted m))
                            :effect-class (:discipline/event m)
                            :top-shift? nil}
                :actual {:discharge-G nil
                         :addressed? false
                         :top-shift? nil
                         :evidence-ref (str path)}
                :witness/class :discipline-event
                :calibration {:G-error nil :top-shift-correct? nil}
                :source :discipline-event}))))

(defn build-ledger
  ([] (build-ledger default-inputs))
  ([{:keys [traces-dir pilots-log closure-folds ch2-events discipline-events]}]
   (vec (concat (repl-trace-ledger traces-dir)
                (pilots-log-ledger pilots-log)
                (closure-fold-ledger closure-folds)
                (ch2-ledger ch2-events)
                (discipline-ledger discipline-events)))))

(defn- mean [xs]
  (when (seq xs)
    (/ (reduce + 0.0 xs) (double (count xs)))))

(defn calibration-audit [ledger]
  (let [g-errors (keep #(get-in % [:calibration :G-error]) ledger)
        top-pairs (filter #(and (some? (get-in % [:predicted :top-shift?]))
                                (some? (get-in % [:actual :top-shift?])))
                          ledger)
        top-correct (filter #(= (get-in % [:predicted :top-shift?])
                                (get-in % [:actual :top-shift?]))
                            top-pairs)]
    {:entry-count (count ledger)
     :count-by-source (frequencies (map :source ledger))
     :count-by-witness (frequencies (map :witness/class ledger))
     :paired-G-count (count g-errors)
     :mean-G-error (or (mean g-errors) 0.0)
     :mean-abs-G-error (or (mean (map #(Math/abs (double %)) g-errors)) 0.0)
     :max-abs-G-error (if (seq g-errors) (apply max (map #(Math/abs (double %)) g-errors)) 0.0)
     :top-shift-paired-count (count top-pairs)
     :top-shift-accuracy (when (seq top-pairs)
                           (/ (count top-correct) (double (count top-pairs))))
     :read-only? true
     :gate :G-SIM}))

(defn write-edn! [path value]
  (let [file (io/file path)]
    (when-let [parent (.getParentFile file)]
      (.mkdirs parent))
    (spit file (with-out-str (pprint/pprint value)))
    value))

(defn write-ledger-and-report!
  ([] (write-ledger-and-report! {}))
  ([{:keys [inputs ledger-path report-path]
     :or {inputs default-inputs
          ledger-path default-ledger-path
          report-path default-report-path}}]
   (let [ledger (build-ledger inputs)
         report (calibration-audit ledger)]
     (write-edn! ledger-path ledger)
     (write-edn! report-path report)
     {:ledger-path ledger-path
      :report-path report-path
      :ledger-count (count ledger)
      :report report})))

(defn -main [& _]
  (prn (write-ledger-and-report!)))
