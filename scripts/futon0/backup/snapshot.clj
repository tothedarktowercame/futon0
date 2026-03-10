(ns futon0.backup.snapshot
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant LocalDate ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(def ^:private default-lookback-hours 24)

(defn- home []
  (System/getProperty "user.home"))

(defn- path [& parts]
  (str (io/file (apply str (interpose "/" parts)))))

(def ^:private default-output-root
  (path (home) "code/backups"))

(def ^:private default-vitality-scan
  (path (home) "code/storage/futon0/vitality/latest_scan.json"))

(def ^:private default-zoom-index
  (path (home) "code/storage/zoomr4/meta/zoom_sync_index.json"))

(def ^:private default-git-summary
  (path (home) "code/futon3/resources/vitality/git_summary.edn"))

(def ^:private default-tatami-log
  (path (home) "code/futon3/resources/tatami/logs/sessions.jsonl"))

(def ^:private iso-formatter
  (.withZone DateTimeFormatter/ISO_OFFSET_DATE_TIME (ZoneId/systemDefault)))

(defn- now-inst []
  (Instant/now))

(defn- now-iso []
  (.format iso-formatter (now-inst)))

(defn- parse-instant [value]
  (when (string? value)
    (try
      (Instant/parse value)
      (catch Exception _ nil))))

(defn- read-json [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (json/read r :key-fn keyword)))))

(defn- read-edn [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (edn/read (java.io.PushbackReader. r))))))

(defn- weekday-anchor [^LocalDate date target]
  (loop [d date]
    (if (= (.getDayOfWeek d) target)
      d
      (recur (.minusDays d 1)))))

(defn- first-sunday [^LocalDate date]
  (loop [d (.withDayOfMonth date 1)]
    (if (= (.getDayOfWeek d) java.time.DayOfWeek/SUNDAY)
      d
      (recur (.plusDays d 1)))))

(defn- window-anchors [^Instant now]
  (let [date (.toLocalDate (ZonedDateTime/ofInstant now (ZoneId/systemDefault)))
        weekly (weekday-anchor date java.time.DayOfWeek/SUNDAY)
        monthly (first-sunday date)]
    {:weekly_anchor (str weekly)
     :monthly_anchor (str monthly)}))

(defn- vitality->report [scan warnings]
  (if scan
    (let [filesystem (->> (:filesystem scan)
                          (map (fn [entry]
                                 (let [imports (:imports entry)
                                       recent-imports (or (:recent_imports imports)
                                                          (when-let [recent (:recent imports)]
                                                            (count recent)))]
                                   (cond-> {:label (:label entry)
                                            :recent_files (:recent_files entry)
                                            :latest_mtime (:latest_mtime entry)}
                                     (some? recent-imports)
                                     (assoc :recent_imports recent-imports)))))
                          vec)]
      {:filesystem filesystem
       :tatami (:tatami scan)})
    (do
      (swap! warnings conj "missing vitality scan")
      nil)))

(defn- zoom-index->report [index now-inst lookback-hours warnings]
  (if index
    (let [entries (or (:entries index) (:items index) [])
          cutoff (- (.toEpochMilli now-inst) (* lookback-hours 60 60 1000))
          recent? (fn [entry]
                    (when-let [stamp (or (:ingested_at entry)
                                         (:ingested-at entry)
                                         (:recorded_at entry)
                                         (:recorded-at entry))]
                      (when-let [inst (parse-instant stamp)]
                        (<= cutoff (.toEpochMilli inst)))))
          recent (filter recent? entries)
          entry-mins (fn [entry]
                       (cond
                         (number? (:duration_minutes entry)) (:duration_minutes entry)
                         (number? (:duration_seconds entry)) (/ (:duration_seconds entry) 60.0)
                         (number? (:duration_ms entry)) (/ (:duration_ms entry) 60000.0)
                         :else nil))
          minutes (->> recent
                       (map entry-mins)
                       (remove nil?)
                       (reduce + 0.0))]
      {:recent_count (count recent)
       :recent_minutes (when (pos? minutes) (Math/round minutes))})
    (do
      (swap! warnings conj "missing zoom ingest index")
      {:recent_count nil
       :recent_minutes nil})))

(defn- git-summary->report [summary warnings]
  (if summary
    (let [commit-count (or (:commit_count summary)
                           (:commit-count summary)
                           (:commits summary))
          repos (or (:repos summary) (:repositories summary))
          touched (->> repos
                       (map (fn [repo]
                              (or (:label repo) (:name repo) (:repo repo))))
                       (remove nil?)
                       vec)]
      {:commit_count commit-count
       :repos_touched touched})
    (do
      (swap! warnings conj "missing git summary")
      {:commit_count nil
       :repos_touched []})))

(defn- storage-status->report [scan warnings]
  (if-let [status (:storage_status scan)]
    (select-keys status [:needs_backup :note :backed_up :paths])
    (do
      (swap! warnings conj "missing storage_status")
      {:needs_backup nil
       :note "storage_status missing"})))

(defn- build-report
  [{:keys [vitality-scan zoom-index git-summary tatami-log lookback-hours]}]
  (let [warnings (atom [])
        scan vitality-scan
        now (or (parse-instant (:generated_at scan)) (now-inst))
        lookback (or (:lookback_hours scan) lookback-hours default-lookback-hours)
        window (merge {:lookback_hours lookback} (window-anchors now))]
    (cond-> {:generated_at (or (:generated_at scan) (now-iso))
             :window window
             :vitality (vitality->report scan warnings)
             :recording_ingest (zoom-index->report zoom-index now lookback warnings)
             :git_activity (git-summary->report git-summary warnings)
             :storage_status (storage-status->report scan warnings)
             :derivatives_exchange {:summary "Optional computed indicators from other futons"
                                    :signals []}}
      (seq @warnings)
      (assoc :warnings (vec @warnings))

      (and (nil? scan) (io/file tatami-log))
      (update :warnings (fnil conj []) "tatami log path is set but not parsed"))))

(defn- snapshot-dir [root now]
  (let [date (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd")
                      (ZonedDateTime/ofInstant now (ZoneId/systemDefault)))
        time (.format (DateTimeFormatter/ofPattern "HHmmss")
                      (ZonedDateTime/ofInstant now (ZoneId/systemDefault)))]
    (io/file root date time)))

(defn- write-json [file payload]
  (with-open [w (io/writer file)]
    (json/write payload w)))

(defn- report->markdown [report]
  (let [vitality (:vitality report)
        tatami (:tatami vitality)
        filesystem (:filesystem vitality)
        ing (:recording_ingest report)
        git (:git_activity report)
        storage (:storage_status report)
        warnings (:warnings report)]
    (str "# Backup Summary\n\n"
         "- generated_at: " (:generated_at report) "\n"
         "- lookback_hours: " (get-in report [:window :lookback_hours]) "\n"
         "- weekly_anchor: " (get-in report [:window :weekly_anchor]) "\n"
         "- monthly_anchor: " (get-in report [:window :monthly_anchor]) "\n\n"
         "## Vitality\n"
         (if (seq filesystem)
           (str/join ""
                     (map (fn [entry]
                            (format "- %s: recent_files=%s latest=%s%s\n"
                                    (:label entry)
                                    (:recent_files entry)
                                    (:latest_mtime entry)
                                    (if (some? (:recent_imports entry))
                                      (format " recent_imports=%s" (:recent_imports entry))
                                      "")))
                          filesystem))
           "- (no filesystem data)\n")
         (if tatami
           (format "- tatami: events=%s hours_since=%s gap_warning=%s\n"
                   (:events_in_lookback tatami)
                   (:hours_since_last tatami)
                   (:gap_warning tatami))
           "- tatami: (missing)\n")
         "\n## Recording ingest\n"
         (format "- recent_count=%s recent_minutes=%s\n"
                 (:recent_count ing)
                 (:recent_minutes ing))
         "\n## Git activity\n"
         (format "- commit_count=%s repos_touched=%s\n"
                 (:commit_count git)
                 (or (:repos_touched git) []))
         "\n## Storage status\n"
         (format "- needs_backup=%s note=%s\n"
                 (:needs_backup storage)
                 (:note storage))
         (when (seq warnings)
           (str "\n## Warnings\n"
                (str/join "" (map #(str "- " % "\n") warnings)))))))

(defn- validate-report [report]
  (let [missing (atom [])]
    (doseq [path [[:generated_at]
                  [:window :lookback_hours]
                  [:window :weekly_anchor]
                  [:window :monthly_anchor]
                  [:storage_status]]]
      (when-not (get-in report path)
        (swap! missing conj (str "missing " (str/join "->" (map name path))))))
    @missing))

(defn- usage []
  (str/join
   \newline
   ["Usage:"
    "  clojure -M -m futon0.backup.snapshot --write"
    "  clojure -M -m futon0.backup.snapshot --validate <path>"
    ""
    "Options:"
    "  --output-root <path>  Root backup directory (default ~/code/backups)"
    "  --write               Write summary.json (+ summary.md) to a snapshot dir"
    "  --no-markdown         Do not write summary.md"
    "  --validate <path>     Validate an existing summary.json"
    "  --help                Show this help"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:output-root default-output-root
               :markdown? true}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--output-root" arg) (recur (nnext args) (assoc opts :output-root (second args)))
          (= "--write" arg) (recur (rest args) (assoc opts :write? true))
          (= "--no-markdown" arg) (recur (rest args) (assoc opts :markdown? false))
          (= "--validate" arg) (recur (nnext args) (assoc opts :validate (second args)))
          (= "--help" arg) (recur (rest args) (assoc opts :help? true))
          :else (throw (ex-info "Unknown argument" {:arg arg})))))))

(defn -main [& args]
  (let [{:keys [output-root write? markdown? validate help?]} (parse-args args)]
    (cond
      help?
      (do (println (usage)) (System/exit 0))

      validate
      (let [report (read-json validate)
            errors (if report (validate-report report) ["invalid json or missing file"])]
        (if (seq errors)
          (do
            (doseq [err errors] (println "error:" err))
            (System/exit 1))
          (do
            (println "ok:" validate)
            (System/exit 0))))

      :else
      (let [scan (read-json default-vitality-scan)
            zoom-index (read-json default-zoom-index)
            git-summary (read-edn default-git-summary)
            report (build-report {:vitality-scan scan
                                  :zoom-index zoom-index
                                  :git-summary git-summary
                                  :tatami-log default-tatami-log
                                  :lookback-hours default-lookback-hours})]
        (if write?
          (let [now (now-inst)
                dir (snapshot-dir output-root now)]
            (.mkdirs dir)
            (write-json (io/file dir "summary.json") report)
            (when markdown?
              (spit (io/file dir "summary.md") (report->markdown report)))
            (println "wrote:" (.getAbsolutePath dir)))
          (println (json/write-str report)))))))
