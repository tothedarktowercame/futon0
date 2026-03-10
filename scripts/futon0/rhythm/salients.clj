(ns futon0.rhythm.salients
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time DayOfWeek Instant LocalDate ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(defn- home []
  (System/getProperty "user.home"))

(defn- default-output-root []
  (str (io/file (home) "code/backups")))

(def ^:private timestamp-formatter
  (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssZ"))

(defn- now-inst []
  (Instant/now))

(defn- now-iso []
  (.format timestamp-formatter (ZonedDateTime/ofInstant (now-inst) (ZoneId/systemDefault))))

(defn- read-edn [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (edn/read (java.io.PushbackReader. r))))))

(defn- read-json [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (json/read r :key-fn keyword)))))

(defn- write-json [file payload]
  (with-open [w (io/writer file)]
    (json/write payload w)))

(defn- sunday-anchor [^LocalDate date]
  (loop [d date]
    (if (= (.getDayOfWeek d) DayOfWeek/SUNDAY)
      d
      (recur (.minusDays d 1)))))

(defn- week-anchor []
  (let [date (.toLocalDate (ZonedDateTime/ofInstant (now-inst) (ZoneId/systemDefault)))]
    (str (sunday-anchor date))))

(def ^:private org-heading-re
  #"^(\*+)\s+(?:(TODO|NEXT|DONE|WAIT|HOLD|CANCELLED)\s+)?(.*)$")

(defn- parse-org-lines [lines]
  (->> lines
       (keep (fn [line]
               (when-let [[_ stars todo title] (re-matches org-heading-re line)]
                 {:level (count stars)
                  :todo (when (seq todo) (str/lower-case todo))
                  :title (str/trim (or title ""))})))
       vec))

(defn- read-org [path warnings]
  (let [f (io/file path)]
    (if (.exists f)
      (with-open [r (io/reader f)]
        (parse-org-lines (line-seq r)))
      (do
        (swap! warnings conj (str "missing org file: " path))
        []))))

(defn- summarize-org [entries]
  (let [todos (->> entries (keep :todo))
        counts (frequencies todos)
        todo-count (reduce + 0 (vals counts))]
    {:headlines (mapv #(select-keys % [:level :todo :title]) entries)
     :todo_counts counts
     :todo_total todo-count}))

(defn- summarize-futon1-tags [payload warnings]
  (cond
    (nil? payload)
    (do (swap! warnings conj "missing futon1 tags")
        {:tags []})

    (map? payload)
    (let [tags (or (:tags payload) (:tagged payload) (:sessions payload) [])]
      {:tags (vec tags)})

    (sequential? payload)
    {:tags (vec payload)}

    :else
    (do (swap! warnings conj "unrecognized futon1 tags format")
        {:tags []})))

(defn- snapshot-dir [root now]
  (let [date (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd")
                      (ZonedDateTime/ofInstant now (ZoneId/systemDefault)))
        time (.format (DateTimeFormatter/ofPattern "HHmmss")
                      (ZonedDateTime/ofInstant now (ZoneId/systemDefault)))]
    (io/file root date time)))

(defn- usage []
  (str/join
   \newline
   ["Usage:"
    "  clojure -M -m futon0.rhythm.salients --org <path> [--org <path> ...] --write"
    ""
    "Options:"
    "  --org <path>         Org files to scan for weekly salients"
    "  --futon1-tags <path> Optional EDN or JSON tags summary"
    "  --output-root <path> Root directory for snapshot outputs"
    "  --write              Write salients.json (and optional EDN handoff)"
    "  --futon1-out <path>  Write EDN payload for FUTON1 ingestion"
    "  --futon3-out <path>  Write EDN payload for FUTON3 ownership"
    "  --handoff-target <t> Record handoff target (e.g. futon3)"
    "  --handoff-notes <s>  Freeform handoff notes"
    "  --help               Show this help"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:org [] :output-root (default-output-root)}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--org" arg) (recur (nnext args) (update opts :org conj (second args)))
          (= "--futon1-tags" arg) (recur (nnext args) (assoc opts :futon1-tags (second args)))
          (= "--output-root" arg) (recur (nnext args) (assoc opts :output-root (second args)))
          (= "--futon1-out" arg) (recur (nnext args) (assoc opts :futon1-out (second args)))
          (= "--futon3-out" arg) (recur (nnext args) (assoc opts :futon3-out (second args)))
          (= "--handoff-target" arg) (recur (nnext args) (assoc opts :handoff-target (second args)))
          (= "--handoff-notes" arg) (recur (nnext args) (assoc opts :handoff-notes (second args)))
          (= "--write" arg) (recur (rest args) (assoc opts :write? true))
          (= "--help" arg) (recur (rest args) (assoc opts :help? true))
          :else (throw (ex-info "Unknown argument" {:arg arg})))))))

(defn -main [& args]
  (let [{:keys [org futon1-tags output-root write? futon1-out futon3-out
                handoff-target handoff-notes help?]} (parse-args args)]
    (when help?
      (println (usage))
      (System/exit 0))
    (let [warnings (atom [])
          entries (mapcat #(read-org % warnings) org)
          org-summary (summarize-org entries)
          tags-payload (or (read-edn futon1-tags) (read-json futon1-tags))
          tag-summary (summarize-futon1-tags tags-payload warnings)
          payload (cond-> {:generated_at (now-iso)
                           :week_anchor (week-anchor)
                           :org org-summary
                           :futon1 tag-summary}
                    (seq @warnings) (assoc :warnings (vec @warnings))
                    (or handoff-target handoff-notes)
                    (assoc :handoff (cond-> {}
                                      handoff-target (assoc :target handoff-target)
                                      handoff-notes (assoc :notes handoff-notes))))]
      (when write?
        (let [dir (snapshot-dir output-root (now-inst))]
          (.mkdirs dir)
          (write-json (io/file dir "salients.json") payload)
          (println "wrote:" (.getAbsolutePath dir))))
      (doseq [out [futon1-out futon3-out]
              :when out]
        (with-open [w (io/writer out)]
          (binding [*out* w]
            (pr payload)))
        (println "handoff:" out))
      (when-not (or write? futon1-out futon3-out)
        (println (json/write-str payload))))))
