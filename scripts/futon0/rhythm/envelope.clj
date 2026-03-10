(ns futon0.rhythm.envelope
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time DayOfWeek Instant LocalDate ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(defn- home []
  (System/getProperty "user.home"))

(defn- path [& parts]
  (str (io/file (apply str (interpose "/" parts)))))

(def ^:private default-input-dir
  (path (home) "code/storage/futon0/vitality/stack-hud"))

(def ^:private default-output-dir default-input-dir)

(def ^:private summary-suffix ".summary.json")

(def ^:private timestamp-formatter
  (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssZ"))

(defn- parse-ts [s]
  (when (string? s)
    (try
      (let [zdt (ZonedDateTime/parse s timestamp-formatter)]
        (.toInstant zdt))
      (catch Exception _ nil))))

(defn- read-json [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (json/read r :key-fn keyword)))))

(defn- list-summaries [dir]
  (let [root (io/file dir)]
    (when (.exists root)
      (->> (.listFiles root)
           (filter #(.isFile ^java.io.File %))
           (filter #(str/ends-with? (.getName ^java.io.File %) summary-suffix))
           (sort-by #(.getName ^java.io.File %))))))

(defn- summary->envelope [summary]
  (let [date (:date summary)
        first (:first summary)
        last (:last summary)
        first-ts (parse-ts (:timestamp first))
        last-ts (parse-ts (:timestamp last))
        duration-hours (when (and first-ts last-ts)
                         (/ (- (.toEpochMilli last-ts) (.toEpochMilli first-ts))
                            3600000.0))]
    (cond-> {:date date
             :first first
             :last last}
      duration-hours (assoc :duration_hours (double duration-hours)))))

(defn- sunday-anchor [^LocalDate date]
  (loop [d date]
    (if (= (.getDayOfWeek d) DayOfWeek/SUNDAY)
      d
      (recur (.minusDays d 1)))))

(defn- first-sunday [^LocalDate date]
  (loop [d (.withDayOfMonth date 1)]
    (if (= (.getDayOfWeek d) DayOfWeek/SUNDAY)
      d
      (recur (.plusDays d 1)))))

(defn- parse-date [s]
  (try
    (LocalDate/parse s)
    (catch Exception _ nil)))

(defn- group-by-anchor [envelopes anchor-fn key-name]
  (->> envelopes
       (group-by (fn [env]
                   (when-let [d (parse-date (:date env))]
                     (str (anchor-fn d)))))
       (map (fn [[anchor items]]
              (let [sorted (sort-by :date items)]
                {key-name anchor
                 :days (mapv :date sorted)
                 :first (first sorted)
                 :last (last sorted)})))
       (sort-by key-name)
       vec))

(defn- write-json [file payload]
  (with-open [w (io/writer file)]
    (json/write payload w)))

(defn- write-jsonl [file rows]
  (with-open [w (io/writer file)]
    (doseq [row rows]
      (json/write row w)
      (.write w "\n"))))

(defn- usage []
  (str/join
   \newline
   ["Usage:"
    "  clojure -M -m futon0.rhythm.envelope --write"
    ""
    "Options:"
    "  --input-dir <path>   Directory containing HUD summary.json files"
    "  --output-dir <path>  Output directory for envelope aggregates"
    "  --futon1-out <path>  Write EDN payload for FUTON1 ingestion"
    "  --write              Write envelopes.jsonl + weekly.json + monthly.json"
    "  --help               Show this help"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:input-dir default-input-dir
               :output-dir default-output-dir}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--input-dir" arg) (recur (nnext args) (assoc opts :input-dir (second args)))
          (= "--output-dir" arg) (recur (nnext args) (assoc opts :output-dir (second args)))
          (= "--futon1-out" arg) (recur (nnext args) (assoc opts :futon1-out (second args)))
          (= "--write" arg) (recur (rest args) (assoc opts :write? true))
          (= "--help" arg) (recur (rest args) (assoc opts :help? true))
          :else (throw (ex-info "Unknown argument" {:arg arg})))))))

(defn -main [& args]
  (let [{:keys [input-dir output-dir futon1-out write? help?]} (parse-args args)]
    (when help?
      (println (usage))
      (System/exit 0))
    (let [summaries (list-summaries input-dir)
          envelopes (->> summaries
                         (map read-json)
                         (remove nil?)
                         (map summary->envelope)
                         vec)
          weekly (group-by-anchor envelopes sunday-anchor :week_anchor)
          monthly (group-by-anchor envelopes first-sunday :month_anchor)
          payload {:generated_at (str (Instant/now))
                   :envelopes envelopes
                   :weekly weekly
                   :monthly monthly}]
      (when write?
        (let [out (io/file output-dir)]
          (.mkdirs out)
          (write-jsonl (io/file out "envelopes.jsonl") envelopes)
          (write-json (io/file out "weekly.json") weekly)
          (write-json (io/file out "monthly.json") monthly)
          (println "wrote:" (.getAbsolutePath out))))
      (when futon1-out
        (with-open [w (io/writer futon1-out)]
          (binding [*out* w]
            (pr payload)))
        (println "futon1 payload:" futon1-out))
      (when-not (or write? futon1-out)
        (println (json/write-str payload))))))
