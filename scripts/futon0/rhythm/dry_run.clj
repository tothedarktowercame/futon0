(ns futon0.rhythm.dry-run
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [futon0.rhythm.envelope :as envelope]
            [futon0.rhythm.quarterly :as quarterly]
            [futon0.rhythm.salients :as salients]
            [clojure.string :as str])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)
           (java.time Instant ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(defn- temp-dir []
  (-> (Files/createTempDirectory "futon0-rhythm-dry-run" (into-array FileAttribute []))
      (.toFile)))

(defn- read-json [file]
  (let [f (io/file file)]
    (when (.exists f)
      (with-open [r (io/reader f)]
        (json/read r :key-fn keyword)))))

(defn- write-json [file payload]
  (with-open [w (io/writer file)]
    (json/write payload w)))

(defn- required-keys [label m ks]
  (doseq [k ks]
    (when-not (contains? m k)
      (throw (ex-info (str label " missing key") {:key k :payload m})))))

(defn- ensure-file [file label]
  (let [f (io/file file)]
    (when-not (.exists f)
      (throw (ex-info (str label " file not found") {:path (.getAbsolutePath f)})))
    (.getAbsolutePath f)))

(defn- validate-backup-cadence [path]
  (let [cadence (read-json (ensure-file path "cadence"))]
    (required-keys "cadence" cadence [:daily :weekly :monthly])
    (doseq [k [:daily :weekly :monthly]]
      (let [entry (get cadence k)]
        (required-keys (name k) entry [:keep])
        (when-not (number? (:keep entry))
          (throw (ex-info "cadence keep must be numeric" {:entry entry})))))))

(defn- validate-backup-report [path]
  (let [report (read-json (ensure-file path "backup report"))]
    (required-keys "backup report" report [:generated_at :window :vitality :storage_status])
    (required-keys "window" (:window report) [:lookback_hours :weekly_anchor :monthly_anchor])))

(defn- fake-summary [date]
  (let [stamp (.format (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssZ")
                       (ZonedDateTime/ofInstant (Instant/now) (ZoneId/systemDefault)))]
    {:date date
     :first {:timestamp stamp :stack {:generated-at stamp}}
     :last {:timestamp stamp :stack {:generated-at stamp}}}))

(defn- latest-subdir [root]
  (let [dirs (->> (.listFiles (io/file root))
                  (filter #(.isDirectory ^java.io.File %))
                  (sort-by #(.getName ^java.io.File %)))]
    (last dirs)))

(defn- latest-nested-subdir [root]
  (when-let [top (latest-subdir root)]
    (when-let [nested (latest-subdir top)]
      nested)))

(defn- validate-envelope-output [dir]
  (let [env (read-json (io/file dir "weekly.json"))
        mon (read-json (io/file dir "monthly.json"))
        lines (-> (io/file dir "envelopes.jsonl") slurp str/split-lines)]
    (when-not (seq lines)
      (throw (ex-info "envelopes.jsonl empty" {})))
    (when-not (vector? env)
      (throw (ex-info "weekly.json should be a vector" {:payload env})))
    (when-not (vector? mon)
      (throw (ex-info "monthly.json should be a vector" {:payload mon})))))

(defn- validate-experiments-config [path]
  (let [cfg (read-json path)]
    (required-keys "experiments config" cfg [:experiments])
    (when-not (sequential? (:experiments cfg))
      (throw (ex-info "experiments must be a list" {:payload cfg})))
    (doseq [exp (:experiments cfg)]
      (required-keys "experiment" exp [:id :weights]))))

(defn- validate-affect-markers [path]
  (let [cfg (read-json path)]
    (required-keys "affect markers" cfg [:markers])
    (when-not (sequential? (:markers cfg))
      (throw (ex-info "markers must be a list" {:payload cfg})))
    (doseq [marker (:markers cfg)]
      (required-keys "marker" marker [:id :label :scale]))))

(defn- write-jsonl [file rows]
  (with-open [w (io/writer file)]
    (doseq [row rows]
      (json/write row w)
      (.write w "\n"))))

(defn- validate-quarterly-output [dir]
  (let [report (read-json (io/file dir "quarterly.json"))]
    (required-keys "quarterly" report [:generated_at :markers :quarters])
    (when-not (sequential? (:quarters report))
      (throw (ex-info "quarters must be a list" {:payload report})))))

(defn- validate-salients-output [dir]
  (let [report (read-json (io/file dir "salients.json"))]
    (required-keys "salients" report [:generated_at :week_anchor :org :futon1])
    (required-keys "org" (:org report) [:headlines :todo_counts :todo_total])))

(defn -main [& _]
  (let [cwd (System/getProperty "user.dir")
        root-override (System/getenv "FUTON0_ROOT")
        repo-root (or (when root-override (io/file root-override))
                      (loop [dir (io/file cwd)]
                        (let [marker (io/file dir "futon0_rhythm.md")]
                          (cond
                            (.exists marker) dir
                            (nil? (.getParentFile dir)) nil
                            :else (recur (.getParentFile dir))))))
        root (temp-dir)
        summary-dir (io/file root "hud")
        out-dir (io/file root "out")
        org-file (io/file root "salients.org")
        tags-file (io/file root "tags.edn")
        affect-file (io/file root "affect.jsonl")]
    (.mkdirs summary-dir)
    (.mkdirs out-dir)
    (spit org-file "* TODO Sample headline\n** DONE Subtask\n")
    (spit tags-file (pr-str [{:tag "f0/p1" :count 2}]))
    (write-json (io/file summary-dir "2026-01-01.summary.json")
                (fake-summary "2026-01-01"))
    (write-jsonl affect-file
                 [{:timestamp "2026-01-01T10:00:00Z" :marker "energy" :value 4}
                  {:timestamp "2026-01-01T10:00:00Z" :marker "focus" :value 3}
                  {:timestamp "2026-01-01T10:00:00Z" :marker "lightness" :value 4}])
    (when-not repo-root
      (throw (ex-info "FUTON0 repo root not found; set FUTON0_ROOT" {:cwd cwd})))
    (validate-backup-cadence (io/file repo-root "data/backup_cadence.json"))
    (validate-backup-report (io/file repo-root "data/backup_report_example.json"))
    (envelope/-main "--input-dir" (.getAbsolutePath summary-dir)
                    "--output-dir" (.getAbsolutePath out-dir)
                    "--write")
    (validate-envelope-output out-dir)
    (validate-experiments-config (io/file repo-root "data/backup_experiments.json"))
    (validate-affect-markers (io/file repo-root "data/affect_markers.json"))
    (salients/-main "--org" (.getAbsolutePath org-file)
                    "--futon1-tags" (.getAbsolutePath tags-file)
                    "--output-root" (.getAbsolutePath out-dir)
                    "--write")
    (when-let [latest (latest-nested-subdir out-dir)]
      (validate-salients-output latest))
    (quarterly/-main "--envelopes" (.getAbsolutePath (io/file out-dir "envelopes.jsonl"))
                     "--affect" (.getAbsolutePath affect-file)
                     "--markers" (str (io/file repo-root "data/affect_markers.json"))
                     "--output-root" (.getAbsolutePath out-dir)
                     "--write")
    (when-let [latest (latest-nested-subdir out-dir)]
      (validate-quarterly-output latest))
    (println "dry-run ok:" (.getAbsolutePath root))))
