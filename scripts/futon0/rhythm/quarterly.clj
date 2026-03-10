(ns futon0.rhythm.quarterly
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant LocalDate ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(defn- home []
  (System/getProperty "user.home"))

(defn- path [& parts]
  (str (io/file (apply str (interpose "/" parts)))))

(def ^:private default-envelopes
  (path (home) "code/storage/futon0/vitality/stack-hud/envelopes.jsonl"))

(def ^:private default-affect
  (path (home) "code/storage/futon0/vitality/affect.jsonl"))

(def ^:private default-markers
  (path (home) "code/futon0/data/affect_markers.json"))

(def ^:private default-output-root
  (path (home) "code/backups"))

(def ^:private iso-formatter
  (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss'Z'"))

(defn- now-iso []
  (.format iso-formatter (ZonedDateTime/ofInstant (Instant/now) (ZoneId/of "UTC"))))

(defn- read-json [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (json/read r :key-fn keyword)))))

(defn- read-jsonl [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (->> (line-seq r)
             (remove str/blank?)
             (map #(json/read-str % :key-fn keyword))
             vec)))))

(defn- write-json [file payload]
  (with-open [w (io/writer file)]
    (json/write payload w)))

(defn- snapshot-dir [root now]
  (let [date (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd")
                      (ZonedDateTime/ofInstant now (ZoneId/systemDefault)))
        time (.format (DateTimeFormatter/ofPattern "HHmmss")
                      (ZonedDateTime/ofInstant now (ZoneId/systemDefault)))]
    (io/file root date time)))

(defn- parse-date [s]
  (try
    (LocalDate/parse s)
    (catch Exception _ nil)))

(defn- parse-instant [s]
  (try
    (Instant/parse s)
    (catch Exception _ nil)))

(defn- quarter-key [^LocalDate date]
  (let [month (.getMonthValue date)
        q (inc (quot (dec month) 3))]
    (format "%d-Q%d" (.getYear date) q)))

(defn- env->quarter [env]
  (when-let [date (parse-date (:date env))]
    (quarter-key date)))

(defn- affect->quarter [entry]
  (when-let [ts (parse-instant (:timestamp entry))]
    (quarter-key (.toLocalDate (ZonedDateTime/ofInstant ts (ZoneId/of "UTC"))))))

(defn- avg [values]
  (let [vals (remove nil? values)
        cnt (count vals)]
    (if (pos? cnt)
      (/ (reduce + 0.0 vals) cnt)
      nil)))

(defn- round3 [value]
  (when (number? value)
    (/ (Math/round (* 1000.0 (double value))) 1000.0)))

(defn- summarize-envelope [env]
  {:date (:date env)
   :duration_hours (:duration_hours env)
   :warnings (get env :warnings)
   :reminder_soon (get env :reminder_soon)
   :tatami_gap (get env :tatami_gap)
   :recent_files (get env :recent_files)
   :needs_backup (get env :needs_backup)})

(defn- total-duration [envs]
  (reduce + 0.0 (map #(double (or (:duration_hours %) 0.0)) envs)))

(defn- derive-source [entry]
  (let [session-id (:session_id entry)
        source (:source entry)]
    (cond
      (and (string? session-id) (str/starts-with? session-id "git/"))
      (subs session-id (count "git/"))

      (string? session-id) session-id
      (string? source) source
      :else "unknown")))

(defn- compact-example [entry]
  {:timestamp (:timestamp entry)
   :source (derive-source entry)
   :value (:value entry)
   :trigger_text (:trigger_text entry)
   :capacity_terms (vec (take 6 (or (:capacity_terms entry) [])))})

(defn- top-terms [entries]
  (->> entries
       (mapcat #(or (:capacity_terms %) []))
       (remove str/blank?)
       frequencies
       (sort-by (fn [[term n]] [(- n) term]))
       (take 8)
       (mapv (fn [[term n]] {:term term :count n}))))

(defn- summarize-marker [entries marker-id]
  (let [matches (->> entries
                     (filter #(= marker-id (:marker %)))
                     (sort-by :timestamp)
                     vec)
        vals (->> matches (map :value) (remove nil?))
        latest (->> matches reverse (take 3) (mapv compact-example))]
    {:marker marker-id
     :samples (count vals)
     :avg (round3 (avg vals))
     :peak (when (seq vals) (round3 (apply max vals)))
     :sources (->> matches
                   (group-by derive-source)
                   (map (fn [[source rows]]
                          {:source source :count (count rows)}))
                   (sort-by (fn [{:keys [source count]}] [(- count) source]))
                   vec)
     :top_terms (top-terms matches)
     :examples latest}))

(defn- summarize-affect [entries markers-list]
  (let [marker-summaries (->> markers-list
                              (map #(summarize-marker entries (:id %)))
                              (filter #(pos? (:samples %)))
                              (sort-by (fn [{:keys [samples avg marker]}]
                                         [(- samples) (- (or avg 0.0)) marker]))
                              vec)
        sources (->> entries
                     (group-by derive-source)
                     (map (fn [[source rows]]
                            {:source source
                             :count (count rows)}))
                     (sort-by (fn [{:keys [source count]}] [(- count) source]))
                     vec)]
    {:events_count (count entries)
     :markers marker-summaries
     :top_markers (vec (take 5 marker-summaries))
     :source_breakdown sources}))

(defn- quarter-window [envs]
  {:start (:date (first envs))
   :end (:date (last envs))})

(defn- quarter-notes [envs affect-summary]
  (cond-> []
    (empty? envs)
    (conj "No envelope summaries for this quarter.")

    (zero? (:events_count affect-summary))
    (conj "No affect events for this quarter.")

    (and (seq envs) (zero? (total-duration envs)))
    (conj "Envelope summaries recorded no active duration.")))

(defn- format-marker-line [{:keys [marker samples avg peak]}]
  (str "- " marker ": " samples " events"
       (when avg (format ", avg %.3f" (double avg)))
       (when peak (format ", peak %.3f" (double peak)))))

(defn- format-source-line [{:keys [source count]}]
  (str "- " source ": " count))

(defn- format-example-line [{:keys [timestamp source value trigger_text]}]
  (str "  - " timestamp " [" source "] "
       (when value (format "(%.3f) " (double value)))
       (str/replace (or trigger_text "") #"\s+" " ")))

(defn- quarterly->markdown [payload]
  (str/join
   "\n"
   (concat
    [(str "# Quarterly Rhythm Report")
     ""
     (str "Generated: " (:generated_at payload))
     ""]
    (mapcat
     (fn [{:keys [quarter window envelopes_count active_days duration_hours_total
                  duration_hours_avg affect_summary notes]}]
       (concat
        [(str "## " quarter)
         ""
         (str "- Window: " (or (:start window) "?") " to " (or (:end window) "?"))
         (str "- Envelope days: " envelopes_count)
         (str "- Active days: " active_days)
         (format "- Total duration hours: %.3f" (double (or duration_hours_total 0.0)))
         (format "- Average duration hours: %.3f" (double (or duration_hours_avg 0.0)))
         (str "- Affect events: " (:events_count affect_summary))
         ""]
        (when (seq notes)
          (concat
           ["### Notes" ""]
           (map #(str "- " %) notes)
           [""]))
        (if (seq (:top_markers affect_summary))
          (concat
           ["### Top markers" ""]
           (map format-marker-line (:top_markers affect_summary))
           [""])
          ["### Top markers" "" "- none" ""])
        (if (seq (:source_breakdown affect_summary))
          (concat
           ["### Sources" ""]
           (map format-source-line (:source_breakdown affect_summary))
           [""])
          ["### Sources" "" "- none" ""])
        (if (seq (:markers affect_summary))
          (mapcat
           (fn [{:keys [marker samples avg peak top_terms examples]}]
             (concat
              [(str "### Marker: " marker)
               ""
               (str "- Samples: " samples)
               (when avg (format "- Average value: %.3f" avg))
               (when peak (format "- Peak value: %.3f" peak))
               (str "- Terms: "
                    (if (seq top_terms)
                      (str/join ", " (map #(str (:term %) " (" (:count %) ")") top_terms))
                      "none"))
               "- Examples:"]
              (if (seq examples)
                (map format-example-line examples)
                ["  - none"])
              [""]))
           (:markers affect_summary))
          [])))
     (:quarters payload)))))

(defn- usage []
  (str/join
   \newline
   ["Usage:"
    "  clojure -M -m futon0.rhythm.quarterly --write"
    ""
    "Options:"
    "  --envelopes <path>   Path to envelopes.jsonl"
    "  --affect <path>      Path to affect.jsonl (marker/value events)"
    "  --markers <path>     Path to affect_markers.json"
    "  --output-root <path> Root backup directory"
    "  --write              Write quarterly.json"
    "  --help               Show this help"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:envelopes default-envelopes
               :affect default-affect
               :markers default-markers
               :output-root default-output-root}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--envelopes" arg) (recur (nnext args) (assoc opts :envelopes (second args)))
          (= "--affect" arg) (recur (nnext args) (assoc opts :affect (second args)))
          (= "--markers" arg) (recur (nnext args) (assoc opts :markers (second args)))
          (= "--output-root" arg) (recur (nnext args) (assoc opts :output-root (second args)))
          (= "--write" arg) (recur (rest args) (assoc opts :write? true))
          (= "--help" arg) (recur (rest args) (assoc opts :help? true))
          :else (throw (ex-info "Unknown argument" {:arg arg})))))))

(defn -main [& args]
  (let [{:keys [envelopes affect markers output-root write? help?]} (parse-args args)]
    (when help?
      (println (usage))
      (System/exit 0))
    (let [envs (or (read-jsonl envelopes) [])
          aff (or (read-jsonl affect) [])
          marker-def (read-json markers)
          markers-list (or (:markers marker-def) [])
          grouped (group-by env->quarter envs)
          quarters (->> grouped
                        (map (fn [[quarter entries]]
       (let [summary (map summarize-envelope entries)
                                     quarter-affect (->> aff
                                                         (filter #(= quarter (affect->quarter %)))
                                                         (sort-by :timestamp)
                                                         vec)
                                     affect-summary (summarize-affect quarter-affect markers-list)]
                                 {:quarter quarter
                                  :window (quarter-window summary)
                                  :envelopes_count (count entries)
                                  :active_days (count (filter #(pos? (double (or (:duration_hours %) 0.0))) summary))
                                  :duration_hours_total (round3 (total-duration summary))
                                  :duration_hours_avg (round3 (avg (map :duration_hours summary)))
                                  :envelopes summary
                                  :affect_summary affect-summary
                                  :notes (quarter-notes summary affect-summary)})))
                        (sort-by :quarter)
                        vec)
          payload {:generated_at (now-iso)
                   :markers markers-list
                   :quarters quarters}]
      (if write?
        (let [dir (snapshot-dir output-root (Instant/now))]
          (.mkdirs dir)
          (write-json (io/file dir "quarterly.json") payload)
          (spit (io/file dir "quarterly.md") (quarterly->markdown payload))
          (println "wrote:" (.getAbsolutePath dir)))
        (println (json/write-str payload))))))
