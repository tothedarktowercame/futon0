(ns futon0.vitality.scanner
  "Emit a lightweight vitality snapshot for cron/systemd ingestion."
  (:require [clojure.data.json :as json]
            [clojure.string :as str])
  (:import (java.io File)
           (java.nio.file Files LinkOption Path Paths)
           (java.time Duration Instant LocalDateTime OffsetDateTime ZoneId ZoneOffset)
           (java.time.format DateTimeParseException)
           (java.util ArrayDeque)))

(def ^:private home-dir (System/getProperty "user.home"))
(def ^:private default-config
  (File. (str home-dir "/code/storage/futon0/vitality/vitality_scanner.json")))
(def ^:private default-output
  (File. (str home-dir "/code/storage/futon0/vitality/latest_scan.json")))
(def ^:private futon-liveness-window-hours 168)
(def ^:private futon-dirs (map #(format "futon%d" %) (range 8)))
(def ^:private futon-ignore-dirs
  #{".git" ".cache" ".clj-kondo" ".idea" ".lsp" ".pytest_cache" ".venv"
    "node_modules" "target" "dist" "build"})

(defn- parse-args [args]
  (loop [args args
         out {:config default-config
              :output nil
              :quiet false}]
    (if (empty? args)
      out
      (let [[flag value & rest] args]
        (cond
          (= flag "--config") (recur rest (assoc out :config (File. value)))
          (= flag "--output") (recur rest (assoc out :output (File. value)))
          (= flag "--quiet") (recur rest (assoc out :quiet true))
          :else (recur rest out))))))

(defn- read-json-file [^File path]
  (when (.exists path)
    (try
      (with-open [r (clojure.java.io/reader path)]
        (json/read r :key-fn keyword))
      (catch Exception _ nil))))

(defn- write-json-file [^File path data]
  (let [parent (.getParentFile path)]
    (when parent (.mkdirs parent)))
  (spit path (json/write-str data)))

(defn- expand-path [raw]
  (let [expanded (-> raw
                     (str/replace #"^\~" (System/getProperty "user.home"))
                     (str/replace #"\$([A-Za-z_][A-Za-z0-9_]*)"
                                  (fn [[_ name]]
                                    (or (System/getenv name) ""))))]
    (File. expanded)))

(defn- timestamp->iso [millis]
  (when millis
    (.toString (Instant/ofEpochMilli millis))))

(defn- safe-list-files [^File dir]
  (try
    (.listFiles dir)
    (catch Exception _ nil)))

(defn- dir-name [^File dir]
  (some-> dir .getName))

(defn- symlink? [^File file]
  (try
    (Files/isSymbolicLink (.toPath file))
    (catch Exception _ false)))

(defn- bucket-hours [hours]
  (cond
    (nil? hours) nil
    (< hours 24) "1"
    (< hours 48) "2"
    (< hours 72) "3"
    (< hours 96) "4"
    (< hours 120) "5"
    (< hours 144) "6"
    (< hours 168) "7"
    :else "7+"))

(defn- summarize-imports [entry]
  (when-let [index-path (:import_index entry)]
    (let [path (expand-path index-path)
          payload (read-json-file path)
          entries (or (:entries payload) [])]
      (when (seq entries)
        (let [limit (int (or (:import_limit entry) 5))
              sorted (sort-by #(or (:ingested_at %) (:recorded_date %) "") #(compare %2 %1) entries)
              recent (->> (take limit sorted)
                          (map (fn [item]
                                 (let [display (or (:title item)
                                                   (:base_name item)
                                                   (some-> (:copied_to item) File. .getName)
                                                   (some-> (:source item) File. .getName))
                                       info {:title display
                                             :recorded_date (:recorded_date item)
                                             :ingested_at (:ingested_at item)
                                             :mp3 (:mp3 item)
                                             :copied_to (:copied_to item)}]
                                   (into {}
                                         (filter (comp some? val) info))))))
              total (count entries)]
          {"total" total
           "recent" recent})))))

(defn- scan-filesystem [entry now default-lookback]
  (let [label (or (:label entry) (:path entry))
        root (expand-path (:path entry))
        lookback (int (or (:lookback_hours entry) default-lookback))
        cutoff (- (.toEpochMilli now) (* lookback 3600000))
        imports (summarize-imports entry)
        max-depth (int (or (:max_depth entry) 2))
        top-n (int (or (:top_n entry) 5))
        base-summary {"label" label
                      "path" (.getPath root)
                      "exists" (.exists root)
                      "lookback_hours" lookback}
        summary (cond-> base-summary
                  imports (assoc "imports" imports))]
    (if-not (.exists root)
      summary
      (let [start (System/nanoTime)
            queue (doto (ArrayDeque.) (.add [root 0]))
            children (atom {})
            recent-files (atom 0)
            latest-ts (atom nil)
            root-path (.toPath root)
            _ (while (not (.isEmpty queue))
                (let [[^File current depth] (.remove queue)]
                  (when-let [items (safe-list-files current)]
                    (doseq [child items]
                      (when-not (symlink? child)
                        (if (.isDirectory child)
                          (when (< depth max-depth)
                            (.add queue [child (inc depth)]))
                          (let [mtime (.lastModified child)]
                            (when (>= mtime cutoff)
                              (swap! recent-files inc)
                              (swap! latest-ts #(if % (max % mtime) mtime))
                              (let [child-path (.toPath child)
                                    rel (try
                                          (.relativize root-path child-path)
                                          (catch Exception _ nil))
                                    head (if (and rel (> (.getNameCount rel) 0))
                                           (str (.getName rel 0))
                                           (.getName child))]
                                (swap! children update head (fnil inc 0)))))))))))
            latest @latest-ts
            top-children (->> @children
                              (sort-by val >)
                              (take top-n)
                              (map (fn [[name count]]
                                     {"name" name
                                      "recent_files" count}))
                              vec)
            duration (/ (double (- (System/nanoTime) start)) 1000000000.0)
            stats {"recent_files" @recent-files
                   "latest_mtime" (timestamp->iso latest)
                   "top_children" top-children
                   "scan_duration_seconds"
                   (Double/parseDouble (format "%.3f" duration))}]
        (merge summary stats)))))

(defn- parse-timestamp [raw]
  (let [text (str/trim (or raw ""))]
    (when (seq text)
      (let [normalized (if (str/ends-with? text "Z")
                         (str (subs text 0 (dec (count text))) "+00:00")
                         text)]
        (or (try
              (Instant/parse normalized)
              (catch DateTimeParseException _ nil))
            (try
              (.toInstant (OffsetDateTime/parse normalized))
              (catch DateTimeParseException _ nil))
            (try
              (.toInstant (.atZone (LocalDateTime/parse normalized) ZoneOffset/UTC))
              (catch DateTimeParseException _ nil)))))))

(defn- scan-tatami [entry now default-lookback]
  (let [log-path (expand-path (or (:log_path entry) ""))
        summary {"log_path" (.getPath log-path)
                 "exists" (.exists log-path)
                 "lookback_hours" (int (or (:lookback_hours entry) default-lookback))}]
    (if-not (.exists log-path)
      summary
      (let [timestamp-field (or (:timestamp_field entry) "timestamp")
            fmt (or (:format entry) "auto")
            events (atom [])
            error (atom nil)]
        (try
          (with-open [r (clojure.java.io/reader log-path)]
            (doseq [line (line-seq r)]
              (let [trimmed (str/trim line)]
                (when (seq trimmed)
                  (let [ts (cond
                             (or (= fmt "jsonl")
                                 (and (= fmt "auto") (str/starts-with? trimmed "{")))
                             (try
                               (let [payload (json/read-str trimmed :key-fn keyword)]
                                 (get payload (keyword timestamp-field)))
                               (catch Exception _ trimmed))
                             :else trimmed)]
                    (when-let [parsed (parse-timestamp ts)]
                      (swap! events conj parsed)))))))
          (catch Exception exc
            (reset! error (.getMessage exc))))
        (let [sorted (sort @events)
              lookback (Duration/ofHours (long (get summary "lookback_hours")))
              threshold (.minus now lookback)
              events-in (count (filter (fn [event] (not (.isBefore event threshold))) sorted))
              last-event (last sorted)
              summary (cond-> summary
                        @error (assoc "error" @error)
                        true (assoc "event_count" (count sorted)
                                    "events_in_lookback" events-in))]
          (if last-event
            (let [hours-since (/ (double (.toMinutes (Duration/between last-event now))) 60.0)
                  gap-limit (:gap_warning_hours entry)
                  gap-duration (Duration/between last-event now)
                  gap-warning (when gap-limit
                                (>= (.compareTo gap-duration
                                                (Duration/ofHours (long gap-limit)))
                                    0))]
              (cond-> summary
                true (assoc "last_event" (.toString last-event)
                            "hours_since_last"
                            (Double/parseDouble (format "%.2f" hours-since)))
                gap-limit (assoc "gap_warning" gap-warning)))
            summary))))))

(defn- summarize-storage [entry]
  (when entry
    (let [paths (->> (:paths entry)
                     (map (fn [raw]
                            (cond
                              (string? raw) {:label raw :path raw}
                              (map? raw) raw
                              :else nil)))
                     (remove nil?)
                     (map (fn [{:keys [label path]}]
                            (let [resolved (expand-path path)]
                              {"label" (or label path)
                               "path" (.getPath resolved)
                               "exists" (.exists resolved)})))
                     vec)]
      (cond-> {"backed_up" (boolean (:backed_up entry))
               "note" (:note entry)
               "needs_backup" (not (boolean (:backed_up entry)))}
        (seq paths) (assoc "paths" paths)))))

(defn- scan-futon-activity [now]
  (let [base (File. (System/getProperty "user.home") "code")]
    (->> futon-dirs
         (map-indexed
          (fn [idx name]
            (let [root (File. base name)
                  entry {"id" (format "f%d" idx)
                         "path" (.getPath root)
                         "exists" (.exists root)}]
              (if-not (.exists root)
                entry
                (let [queue (doto (ArrayDeque.) (.add root))
                      latest-ts (atom nil)]
                  (while (not (.isEmpty queue))
                    (let [^File dir (.remove queue)
                          dirname (dir-name dir)]
                      (when (and dirname (not (contains? futon-ignore-dirs dirname))
                                 (not (str/starts-with? dirname ".")))
                        (when-let [items (safe-list-files dir)]
                          (doseq [child items]
                            (when-not (symlink? child)
                              (if (.isDirectory child)
                                (.add queue child)
                                (let [fname (.getName child)]
                                  (when-not (str/starts-with? fname ".")
                                    (let [mtime (.lastModified child)]
                                      (swap! latest-ts #(if % (max % mtime) mtime))))))))))))
                  (if-let [latest @latest-ts]
                    (let [last-event (Instant/ofEpochMilli latest)
                          hours-since (/ (double (.toMinutes (Duration/between last-event now))) 60.0)]
                      (assoc entry
                             "last_mtime" (.toString last-event)
                             "hours_since"
                             (Double/parseDouble (format "%.2f" hours-since))
                             "bucket" (bucket-hours hours-since)))
                    (assoc entry "last_mtime" nil
                           "hours_since" nil
                           "bucket" "7+")))))))
         vec)))

(defn -main [& args]
  (let [{:keys [config output quiet]} (parse-args args)
        cfg (or (read-json-file config) {"lookback_hours" 24
                                         "filesystem" []
                                         "tatami" nil
                                         "output" (.getPath default-output)})
        now (Instant/now)
        lookback (int (or (:lookback_hours cfg) 24))
        filesystem (mapv #(scan-filesystem % now lookback) (or (:filesystem cfg) []))
        tatami (when-let [tatami-cfg (:tatami cfg)]
                 (scan-tatami tatami-cfg now lookback))
        storage (summarize-storage (:storage_status cfg))
        futon-activity (scan-futon-activity now)
        summary (cond-> {"generated_at" (.toString now)
                         "lookback_hours" lookback
                         "filesystem" filesystem
                         "tatami" tatami
                         "futon_activity" futon-activity
                         "futon_activity_window_hours" futon-liveness-window-hours}
                  storage (assoc "storage_status" storage))
        output-path (or output
                        (some-> (:output cfg) expand-path)
                        default-output)]
    (when output-path
      (write-json-file output-path summary))
    (when-not quiet
      (println (json/write-str summary)))))
