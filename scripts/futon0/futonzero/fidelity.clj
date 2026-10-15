(ns futon0.futonzero.fidelity
  "Validation for the FutonZero MVP fidelity contract."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.math BigInteger]
           [java.security MessageDigest]))

(def contract-path "resources/futonzero/fidelity-contract-v1.edn")

(def required-capabilities
  #{:structured-evidence-observation
    :hyperedge-observation
    :agent-discovery
    :capability-profile
    :discipline-profile
    :trajectory-comparison
    :durable-reports
    :read-only-observer
    :cli-surface
    :scheduling-and-coaching
    :rich-interactive-ui
    :llm-generated-hints
    :mission-control-writeback})

(def dispositions #{:preserve :adapt :drop})

(defn- sha256 [path]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (with-open [input (io/input-stream path)]
      (let [buffer (byte-array 8192)]
        (loop []
          (let [n (.read input buffer)]
            (when (pos? n)
              (.update digest buffer 0 n)
              (recur))))))
    (format "%064x" (BigInteger. 1 (.digest digest)))))

(defn entry-errors [{:keys [capability/id disposition source concrete-behavior
                            rationale fidelity-conditions observable-check]}]
  (vec
   (concat
    (when-not (keyword? id) ["capability id must be a keyword"])
    (when-not (contains? dispositions disposition)
      [(str id " has invalid disposition")])
    (when-not (and (map? source) (string? (:path source))
                   (re-matches #"[0-9a-f]{64}" (or (:sha256 source) "")))
      [(str id " needs a pinned source")])
    (when-not (and (string? concrete-behavior) (not (str/blank? concrete-behavior)))
      [(str id " needs concrete FutonZero behavior")])
    (when-not (and (string? rationale) (not (str/blank? rationale)))
      [(str id " needs a disposition rationale")])
    (when-not (and (vector? fidelity-conditions) (seq fidelity-conditions)
                   (every? #(and (string? %) (not (str/blank? %))) fidelity-conditions))
      [(str id " needs fidelity conditions")])
    (when-not (and (map? observable-check) (keyword? (:kind observable-check))
                   (string? (:expect observable-check)))
      [(str id " needs an observable check")]))))

(defn validate-contract [{:keys [schema capabilities]}]
  (let [ids (map :capability/id capabilities)
        id-set (set ids)
        entry-results (mapv (fn [entry]
                              {:capability (:capability/id entry)
                               :errors (entry-errors entry)})
                            capabilities)
        source-errors
        (vec
         (mapcat
          (fn [{:keys [capability/id source]}]
            (let [{:keys [path] expected-sha :sha256} source]
              (cond
                (not (and (map? source)
                          (string? path)
                          (re-matches #"[0-9a-f]{64}" (or expected-sha ""))))
                []
                (not (.exists (io/file path))) [(str id " source is missing: " path)]
                (not= expected-sha (sha256 path))
                [(str id " source hash does not match: " path)]
                :else [])))
          capabilities))
        errors (vec
                (concat
                 (when-not (= :futonzero/fidelity-contract-v1 schema)
                   ["unexpected fidelity contract schema"])
                 (when-not (= (count ids) (count id-set))
                   ["capability ids are not unique"])
                 (when (seq (set/difference required-capabilities id-set))
                   [(str "missing baseline capabilities: "
                         (sort (set/difference required-capabilities id-set)))])
                 (when (seq (set/difference id-set required-capabilities))
                   [(str "undeclared baseline capabilities: "
                         (sort (set/difference id-set required-capabilities)))])
                 source-errors))]
    {:valid? (and (empty? errors)
                  (every? (comp empty? :errors) entry-results))
     :errors errors
     :entry-results entry-results
     :counts (frequencies (map :disposition capabilities))}))

(defn read-contract
  ([] (read-contract contract-path))
  ([path] (edn/read-string (slurp path))))

(defn -main [& [command path]]
  (case command
    "check" (let [result (validate-contract (read-contract (or path contract-path)))]
              (prn result)
              (when-not (:valid? result) (System/exit 1)))
    (do (binding [*out* *err*]
          (println "usage: clojure -M -m futon0.futonzero.fidelity check [contract]"))
        (System/exit 2))))
