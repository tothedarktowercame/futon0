(ns futon0.rhythm.experiments
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

(def ^:private default-output-root
  (path (home) "code/backups"))

(defn- default-config []
  (let [cwd (System/getProperty "user.dir")
        local (io/file cwd "data/backup_experiments.json")
        fallback (io/file (home) "code/futon0/data/backup_experiments.json")]
    (if (.exists local) (.getAbsolutePath local) (.getAbsolutePath fallback))))

(def ^:private timestamp-formatter
  (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss'Z'"))

(defn- now-iso []
  (.format timestamp-formatter (ZonedDateTime/ofInstant (Instant/now) (ZoneId/of "UTC"))))

(defn- parse-date [s]
  (try
    (LocalDate/parse s)
    (catch Exception _ nil)))

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

(defn- parse-number [x]
  (cond
    (number? x) (double x)
    (string? x) (try (Double/parseDouble x) (catch Exception _ nil))
    :else nil))

(defn- truthy? [x]
  (contains? #{"t" "true" "yes" "1"} (str/lower-case (str x))))

(defn- stack-from [env]
  (or (get-in env [:last :stack])
      (get-in env [:first :stack])))

(defn- warnings-count [stack]
  (let [warnings (or (:warnings stack) [])]
    (count warnings)))

(defn- reminders-soon [stack]
  (let [reminders (or (:reminders stack) [])]
    (count (filter #(contains? #{"soon" "overdue"} (:status %)) reminders))))

(defn- tatami-gap? [stack]
  (let [gap (get-in stack [:vitality :tatami :gap-warning])]
    (boolean (truthy? gap))))

(defn- recent-files-total [stack]
  (let [entries (or (get-in stack [:vitality :filesystem]) [])]
    (->> entries
         (map #(parse-number (:recent-files %)))
         (remove nil?)
         (reduce + 0.0))))

(defn- needs-backup? [stack]
  (let [val (get-in stack [:vitality :storage :needs-backup])]
    (boolean (truthy? val))))

(defn- envelope-metrics [env]
  (let [stack (stack-from env)]
    {:date (:date env)
     :duration_hours (or (:duration_hours env) 0.0)
     :warnings (warnings-count stack)
     :reminder_soon (reminders-soon stack)
     :tatami_gap (if (tatami-gap? stack) 1.0 0.0)
     :recent_files (recent-files-total stack)
     :needs_backup (if (needs-backup? stack) 1.0 0.0)}))

(defn- within-window? [days-now window-days date-str]
  (when-let [date (parse-date date-str)]
    (let [cutoff (.minusDays days-now window-days)]
      (not (.isBefore date cutoff)))))

(defn- summarize-window [metrics window-days]
  (let [today (LocalDate/now)
        recent (filter #(within-window? today window-days (:date %)) metrics)
        cnt (max 1 (count recent))
        avg (fn [k]
              (/ (reduce + 0.0 (map #(double (or (get % k) 0.0)) recent)) cnt))]
    {:count (count recent)
     :duration_hours (avg :duration_hours)
     :warnings (avg :warnings)
     :reminder_soon (avg :reminder_soon)
     :tatami_gap (avg :tatami_gap)
     :recent_files (avg :recent_files)
     :needs_backup (avg :needs_backup)}))

(defn- score-experiment [summary weights]
  (reduce (fn [acc [k weight]]
            (+ acc (* (double weight)
                      (double (or (get summary (keyword k)) 0.0)))))
          0.0
          weights))

(defn- run-experiments [envelopes config]
  (let [metrics (mapv envelope-metrics envelopes)]
    (->> (:experiments config)
         (map (fn [exp]
                (let [window (or (:window_days exp) 7)
                      summary (summarize-window metrics window)
                      weights (:weights exp)
                      score (score-experiment summary weights)]
                  {:id (:id exp)
                   :label (:label exp)
                   :window_days window
                   :score score
                   :summary summary
                   :weights weights})))
         (sort-by (comp - :score))
         vec)))

(defn- leaderboard->markdown [rows]
  (let [header "| rank | id | label | score | window_days | warnings | reminder_soon | tatami_gap | recent_files | duration_hours | needs_backup |\n"
        sep "|---|---|---|---|---|---|---|---|---|---|---|\n"
        rows-text (map-indexed
                   (fn [idx row]
                     (let [s (:summary row)]
                       (format "| %d | %s | %s | %.3f | %d | %.2f | %.2f | %.2f | %.2f | %.2f | %.2f |\n"
                               (inc idx)
                               (:id row)
                               (:label row)
                               (:score row)
                               (:window_days row)
                               (:warnings s)
                               (:reminder_soon s)
                               (:tatami_gap s)
                               (:recent_files s)
                               (:duration_hours s)
                               (:needs_backup s))))
                   rows)]
    (str header sep (apply str rows-text))))

(defn- usage []
  (str/join
   \newline
   ["Usage:"
    "  clojure -M -m futon0.rhythm.experiments --write"
    ""
    "Options:"
    "  --envelopes <path>   Path to envelopes.jsonl"
    "  --config <path>      Experiments config JSON"
    "  --output-root <path> Root backup directory"
    "  --write              Write experiments.json + leaderboard.md"
    "  --help               Show this help"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:envelopes default-envelopes
               :config (default-config)
               :output-root default-output-root}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--envelopes" arg) (recur (nnext args) (assoc opts :envelopes (second args)))
          (= "--config" arg) (recur (nnext args) (assoc opts :config (second args)))
          (= "--output-root" arg) (recur (nnext args) (assoc opts :output-root (second args)))
          (= "--write" arg) (recur (rest args) (assoc opts :write? true))
          (= "--help" arg) (recur (rest args) (assoc opts :help? true))
          :else (throw (ex-info "Unknown argument" {:arg arg})))))))

(defn -main [& args]
  (let [{:keys [envelopes config output-root write? help?]} (parse-args args)]
    (when help?
      (println (usage))
      (System/exit 0))
    (let [envs (or (read-jsonl envelopes) [])
          cfg (read-json config)
          results (run-experiments envs cfg)
          payload {:generated_at (now-iso)
                   :config_path config
                   :envelopes_path envelopes
                   :results results}]
      (if write?
        (let [dir (snapshot-dir output-root (Instant/now))]
          (.mkdirs dir)
          (write-json (io/file dir "experiments.json") payload)
          (spit (io/file dir "leaderboard.md") (leaderboard->markdown results))
          (println "wrote:" (.getAbsolutePath dir)))
        (println (json/write-str payload))))))
