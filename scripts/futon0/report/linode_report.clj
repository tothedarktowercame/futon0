(ns futon0.report.linode-report
  "Linode-side status report: git activity, evidence landscape, repo freshness.

   Pulls from:
   - git repos in ~/code/ (commit history)
   - futon3c evidence API (localhost:7070)
   - futon1a hyperedge API (localhost:7071)

   Outputs markdown suitable for reading in Emacs or terminal.

   Invariant: D-I5 (read-only observer — no writes to stack).
   Pattern:   corps/working-where-others-can-see"
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.time Duration Instant LocalDate ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def ^:private home (System/getProperty "user.home"))

(def ^:private futon3c-url "http://localhost:7070")
(def ^:private futon1a-url "http://localhost:7071")

(def ^:private git-repos
  "Repos to scan, in order."
  [{:label "futon0"  :path (str home "/code/futon0")  :sphere "infra"}
   {:label "futon1"  :path (str home "/code/futon1")  :sphere "storage"}
   {:label "futon1a" :path (str home "/code/futon1a") :sphere "storage"}
   {:label "futon2"  :path (str home "/code/futon2")  :sphere "rhythm"}
   {:label "futon3"  :path (str home "/code/futon3")  :sphere "devmaps"}
   {:label "futon3a" :path (str home "/code/futon3a") :sphere "devmaps"}
   {:label "futon3b" :path (str home "/code/futon3b") :sphere "devmaps"}
   {:label "futon3c" :path (str home "/code/futon3c") :sphere "devmaps"}
   {:label "futon4"  :path (str home "/code/futon4")  :sphere "experiments"}
   {:label "futon5"  :path (str home "/code/futon5")  :sphere "proto"}
   {:label "futon5a" :path (str home "/code/futon5a") :sphere "personal"}
   {:label "futon6"  :path (str home "/code/futon6")  :sphere "proto"}
   {:label "futon7"  :path (str home "/code/futon7")  :sphere "proto"}
   {:label "futon7a" :path (str home "/code/futon7a") :sphere "shopfront"}])

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- http-get-json [url]
  (try
    (let [resp (http/get url {:headers {"Accept" "application/json"}
                              :timeout 5000
                              :throw false})]
      (when (= 200 (:status resp))
        (json/parse-string (:body resp) true)))
    (catch Exception _ nil)))

(defn- http-get-edn [url]
  (try
    (let [resp (http/get url {:timeout 5000 :throw false})]
      (when (= 200 (:status resp))
        (read-string (:body resp))))
    (catch Exception _ nil)))

(defn- git [repo-path & args]
  (let [{:keys [exit out]} (apply shell/sh "git" "-C" repo-path args)]
    (when (zero? exit) (str/trim out))))

(defn- bar [n max-width]
  (let [w (min n max-width)]
    (apply str (repeat w "#"))))

;; ---------------------------------------------------------------------------
;; Git activity
;; ---------------------------------------------------------------------------

(defn- git-commit-dates
  "Get commit dates (YYYY-MM-DD) for a repo in a window."
  [repo-path since-date]
  (when-let [out (git repo-path "log" "--since" since-date
                      "--date=short" "--format=%ad")]
    (when (seq out)
      (str/split-lines out))))

(defn- git-latest-commit-time
  "ISO timestamp of most recent commit."
  [repo-path]
  (git repo-path "log" "-1" "--format=%aI"))

(defn scan-git-activity
  "Scan all repos for commit activity in the given window."
  [days]
  (let [since (.toString (.minusDays (LocalDate/now) days))
        results (for [{:keys [label path sphere]} git-repos
                      :let [dates (git-commit-dates path since)
                            latest (git-latest-commit-time path)]
                      :when dates]
                  {:label label
                   :sphere sphere
                   :commits (count dates)
                   :days-active (count (distinct dates))
                   :by-day (frequencies dates)
                   :latest-commit latest})]
    {:since since
     :repos (vec (filter #(pos? (:commits %)) results))
     :total (reduce + 0 (map :commits results))
     :by-day (let [all-days (mapcat #(seq (:by-day %)) results)]
               (into (sorted-map)
                     (reduce (fn [acc [day n]]
                               (update acc day (fnil + 0) n))
                             {} all-days)))}))

;; ---------------------------------------------------------------------------
;; Evidence landscape
;; ---------------------------------------------------------------------------

(defn scan-evidence
  "Query futon3c evidence API for recent entries."
  [limit]
  (let [resp (http-get-json (str futon3c-url "/api/alpha/evidence?limit=" limit))]
    (when (:ok resp)
      (let [entries (:entries resp)
            by-author (group-by :evidence/author entries)
            by-type (group-by :evidence/type entries)
            sessions (->> entries
                          (map :evidence/session-id)
                          (remove nil?)
                          distinct
                          count)
            dates (sort (keep :evidence/at entries))]
        {:total (count entries)
         :by-author (into (sorted-map) (map (fn [[k v]] [k (count v)]) by-author))
         :by-type (into (sorted-map) (map (fn [[k v]] [k (count v)]) by-type))
         :sessions sessions
         :earliest (first dates)
         :latest (last dates)}))))

;; ---------------------------------------------------------------------------
;; Repo freshness
;; ---------------------------------------------------------------------------

(defn scan-freshness
  "Check how recently each repo was touched (last commit)."
  []
  (let [now (Instant/now)]
    (vec
     (for [{:keys [label path]} git-repos
           :let [ts-str (git-latest-commit-time path)]
           :when ts-str]
       (let [ts (try (Instant/from (ZonedDateTime/parse ts-str))
                     (catch Exception _ nil))
             hours (when ts
                     (/ (.toMinutes (Duration/between ts now)) 60.0))]
         {:label label
          :latest ts-str
          :hours-ago (when hours (Double/parseDouble (format "%.1f" hours)))})))))

;; ---------------------------------------------------------------------------
;; Service health
;; ---------------------------------------------------------------------------

(defn check-services []
  (let [check (fn [name url]
                (try
                  (let [resp (http/get url {:timeout 3000 :throw false})]
                    {:name name :status (:status resp) :up? (< (:status resp) 500)})
                  (catch Exception e
                    {:name name :status nil :up? false :error (.getMessage e)})))]
    [(check "futon3c" (str futon3c-url "/api/alpha/evidence?limit=1"))
     (check "futon1a" (str futon1a-url "/api/alpha/hyperedges?type=math/post&limit=1"))]))

;; ---------------------------------------------------------------------------
;; Report rendering
;; ---------------------------------------------------------------------------

(defn render-report
  "Render a complete Linode status report as markdown."
  [{:keys [git evidence freshness services now days]}]
  (let [sb (StringBuilder.)]
    (.append sb (str "# Linode Status Report\n\n"))
    (.append sb (str "**Generated:** " now "\n"))
    (.append sb (str "**Window:** " days " days\n\n"))

    ;; Services
    (.append sb "## Services\n\n")
    (doseq [{:keys [name status up?]} services]
      (.append sb (str "- " name ": " (if up? "UP" "DOWN")
                       (when status (str " (" status ")")) "\n")))
    (.append sb "\n")

    ;; Git activity summary
    (.append sb (str "## Git Activity (" (:total git) " commits, last " days " days)\n\n"))
    (when (seq (:by-day git))
      (doseq [[day n] (:by-day git)]
        (.append sb (str "  " day "  " (bar n 20) " " n "\n")))
      (.append sb "\n"))
    (when (seq (:repos git))
      (.append sb "| Repo | Commits | Sphere |\n")
      (.append sb "| --- | --- | --- |\n")
      (doseq [{:keys [label commits sphere]} (sort-by :commits > (:repos git))]
        (.append sb (str "| " label " | " commits " | " sphere " |\n")))
      (.append sb "\n"))

    ;; Evidence landscape
    (.append sb "## Evidence Landscape\n\n")
    (if evidence
      (do
        (.append sb (str "Entries (last " (:total evidence) " queried): "
                         (:earliest evidence) " → " (:latest evidence) "\n"))
        (.append sb (str "Sessions: " (:sessions evidence) "\n\n"))
        (.append sb "**By author:**\n")
        (doseq [[author n] (:by-author evidence)]
          (.append sb (str "- " author ": " n "\n")))
        (.append sb "\n")
        (.append sb "**By type:**\n")
        (doseq [[typ n] (:by-type evidence)]
          (.append sb (str "- " typ ": " n "\n")))
        (.append sb "\n"))
      (.append sb "Evidence API not reachable.\n\n"))

    ;; Repo freshness
    (.append sb "## Repo Freshness\n\n")
    (.append sb "| Repo | Hours Since Last Commit | Status |\n")
    (.append sb "| --- | --- | --- |\n")
    (doseq [{:keys [label hours-ago]} (sort-by :hours-ago freshness)]
      (let [status (cond
                     (nil? hours-ago) "?"
                     (< hours-ago 24) "active"
                     (< hours-ago 72) "recent"
                     (< hours-ago 168) "quiet"
                     :else "dormant")]
        (.append sb (str "| " label " | " (or hours-ago "?") " | " status " |\n"))))
    (.append sb "\n")

    ;; Languishing check
    (let [dormant (->> freshness
                       (filter #(and (:hours-ago %) (> (:hours-ago %) 168)))
                       (map :label))]
      (when (seq dormant)
        (.append sb "### Dormant repos (>7 days)\n\n")
        (doseq [r dormant]
          (.append sb (str "- " r "\n")))
        (.append sb "\n")))

    (str sb)))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn generate-report
  "Generate the full report data and render it."
  [days]
  (let [now (str (Instant/now))
        git (scan-git-activity days)
        evidence (scan-evidence 500)
        freshness (scan-freshness)
        services (check-services)]
    {:data {:git git :evidence evidence :freshness freshness
            :services services :now now :days days}
     :markdown (render-report {:git git :evidence evidence :freshness freshness
                               :services services :now now :days days})}))

(defn -main [& args]
  (let [days (if (seq args) (Integer/parseInt (first args)) 14)
        {:keys [markdown]} (generate-report days)]
    (println markdown)))
