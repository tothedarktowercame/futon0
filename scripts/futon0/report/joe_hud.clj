(ns futon0.report.joe-hud
  "Joe HUD — behavioral signals, not server health.

   Detects what Joe is and isn't doing, computes ratios and gaps,
   and surfaces actionable interpretations.

   Sources (Linode-observable):
   - git commit timestamps across ~/code/ repos
   - futon3c evidence API (localhost:7070)

   Future sources (laptop-synced):
   - 4-track recording metadata
   - Ultrahuman/sleep data

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
(def ^:private tz (ZoneId/of "America/Chicago"))

(def ^:private git-repos
  [{:label "futon0"  :path (str home "/code/futon0")}
   {:label "futon1"  :path (str home "/code/futon1")}
   {:label "futon1a" :path (str home "/code/futon1a")}
   {:label "futon2"  :path (str home "/code/futon2")}
   {:label "futon3"  :path (str home "/code/futon3")}
   {:label "futon3a" :path (str home "/code/futon3a")}
   {:label "futon3b" :path (str home "/code/futon3b")}
   {:label "futon3c" :path (str home "/code/futon3c")}
   {:label "futon4"  :path (str home "/code/futon4")}
   {:label "futon5"  :path (str home "/code/futon5")}
   {:label "futon5a" :path (str home "/code/futon5a")}
   {:label "futon6"  :path (str home "/code/futon6")}
   {:label "futon7"  :path (str home "/code/futon7")}
   {:label "futon7a" :path (str home "/code/futon7a")}])

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

(defn- git [repo-path & args]
  (let [{:keys [exit out]} (apply shell/sh "git" "-C" repo-path args)]
    (when (zero? exit) (str/trim out))))

(defn- bar [n max-width]
  (apply str (repeat (min n max-width) "#")))

;; ---------------------------------------------------------------------------
;; Signal 1: Work Schedule (commit time-of-day)
;; ---------------------------------------------------------------------------

(defn- git-commit-timestamps
  "Get ISO timestamps for all commits in window."
  [repo-path since]
  (when-let [out (git repo-path "log" "--since" since "--format=%aI")]
    (when (seq out)
      (str/split-lines out))))

(defn- parse-local-hour
  "Extract hour-of-day in Joe's timezone from an ISO timestamp string."
  [ts-str]
  (try
    (let [zdt (ZonedDateTime/parse ts-str)
          local (.withZoneSameInstant zdt tz)]
      (.getHour local))
    (catch Exception _ nil)))

(defn scan-work-schedule
  "Analyze commit times to detect work schedule patterns."
  [days]
  (let [since (.toString (.minusDays (LocalDate/now) days))
        all-hours (for [{:keys [label path]} git-repos
                        :let [timestamps (git-commit-timestamps path since)]
                        :when timestamps
                        ts timestamps
                        :let [h (parse-local-hour ts)]
                        :when h]
                    {:hour h :repo label})
        by-hour (frequencies (map :hour all-hours))
        total (count all-hours)
        ;; Classify periods
        early-morning (reduce + 0 (map #(get by-hour % 0) (range 5 8)))     ; 5-7
        morning       (reduce + 0 (map #(get by-hour % 0) (range 8 12)))    ; 8-11
        afternoon     (reduce + 0 (map #(get by-hour % 0) (range 12 17)))   ; 12-16
        evening       (reduce + 0 (map #(get by-hour % 0) (range 17 21)))   ; 17-20
        late-night    (reduce + 0 (map #(get by-hour % 0) (range 21 24)))   ; 21-23
        deep-night    (reduce + 0 (map #(get by-hour % 0) (range 0 5)))     ; 0-4

        ;; "Getting home on time" = what fraction of commits are after 18:00 CT
        after-6pm (+ evening late-night deep-night)
        after-6pm-pct (if (pos? total) (* 100.0 (/ after-6pm total)) 0.0)]
    {:total total
     :by-hour by-hour
     :periods {:early-morning early-morning
               :morning morning
               :afternoon afternoon
               :evening evening
               :late-night late-night
               :deep-night deep-night}
     :after-6pm-pct after-6pm-pct}))

;; ---------------------------------------------------------------------------
;; Signal 2: Evidence Discipline
;; ---------------------------------------------------------------------------

(defn scan-evidence-discipline
  "Analyze evidence entries for discipline gaps."
  [limit]
  (let [resp (http-get-json (str futon3c-url "/api/alpha/evidence?limit=" limit))]
    (when (:ok resp)
      (let [entries (:entries resp)

            ;; Separate turns from structured evidence
            turns (filter #(= "chat-turn" (get-in % [:evidence/body :event])) entries)
            joe-turns (filter #(= "joe" (:evidence/author %)) turns)
            agent-turns (remove #(= "joe" (:evidence/author %)) turns)

            ;; Evidence types
            forum-posts (filter #(= "forum-post" (:evidence/type %)) entries)
            coordinations (filter #(= "coordination" (:evidence/type %)) entries)

            ;; Discipline artifacts
            psrs (filter #(and (= "pattern-selection" (:evidence/type %))
                               (#{"goal" "question"} (:evidence/claim-type %)))
                         entries)
            purs (filter #(and (= "pattern-outcome" (:evidence/type %))
                               (= "evidence" (:evidence/claim-type %)))
                         entries)
            pars (filter #(and (= "reflection" (:evidence/type %))
                               (= "conclusion" (:evidence/claim-type %)))
                         entries)

            ;; Sessions
            sessions (->> entries
                          (map :evidence/session-id)
                          (remove nil?)
                          distinct)
            session-count (count sessions)

            ;; Sessions with PARs
            par-sessions (->> pars
                              (map :evidence/session-id)
                              (remove nil?)
                              distinct
                              count)

            ;; Agent turn ratio: how many agent turns per joe turn?
            joe-turn-count (count joe-turns)
            agent-turn-count (count agent-turns)
            delegation-ratio (if (pos? joe-turn-count)
                               (double (/ agent-turn-count joe-turn-count))
                               0.0)]
        {:total-entries (count entries)
         :joe-turns joe-turn-count
         :agent-turns agent-turn-count
         :delegation-ratio delegation-ratio
         :forum-posts (count forum-posts)
         :coordinations (count coordinations)
         :psrs (count psrs)
         :purs (count purs)
         :pars (count pars)
         :sessions session-count
         :par-coverage (if (pos? session-count)
                         (* 100.0 (/ par-sessions session-count))
                         0.0)
         :dates {:earliest (first (sort (keep :evidence/at entries)))
                 :latest (last (sort (keep :evidence/at entries)))}}))))

;; ---------------------------------------------------------------------------
;; Signal 3: Stack Breadth (are you only touching one repo?)
;; ---------------------------------------------------------------------------

(defn scan-stack-breadth
  "Which parts of the stack are being worked vs neglected."
  [days]
  (let [since (.toString (.minusDays (LocalDate/now) days))]
    (vec
     (for [{:keys [label path]} git-repos
           :let [count-str (git path "rev-list" "--count" (str "--since=" since) "HEAD")
                 n (when count-str (Integer/parseInt count-str))]
           :when (and n (pos? n))]
       {:label label :commits n}))))

;; ---------------------------------------------------------------------------
;; Signal 4: Creative Workflow (placeholder for laptop sync)
;; ---------------------------------------------------------------------------

(def ^:private recordings-index
  "Path to synced recording metadata (if available)."
  (str home "/code/storage/futon0/recordings/index.json"))

(defn scan-creative
  "Check for creative output signals. Currently a placeholder
   until laptop metadata sync is wired."
  []
  (let [index (java.io.File. recordings-index)]
    (if (.exists index)
      (try
        (let [data (json/parse-string (slurp index) true)
              entries (or (:entries data) (:recordings data) [])
              recent (take 5 (reverse (sort-by #(or (:date %) (:recorded_date %)) entries)))]
          {:available true
           :total (count entries)
           :recent (mapv #(select-keys % [:title :date :recorded_date :duration]) recent)})
        (catch Exception e
          {:available false :error (.getMessage e)}))
      {:available false
       :note "No recording index found. Sync laptop metadata to enable."})))

;; ---------------------------------------------------------------------------
;; Render
;; ---------------------------------------------------------------------------

(defn- pct-str [n] (format "%.0f%%" (double n)))

(defn render-hud
  "Render the Joe HUD as readable markdown."
  [{:keys [schedule evidence breadth creative now days]}]
  (let [sb (StringBuilder.)]
    (.append sb (str "# Joe HUD\n\n"))
    (.append sb (str "**" now "** | " days "-day window\n\n"))

    ;; --- Work Schedule ---
    (.append sb "## Work Schedule\n\n")
    (when schedule
      (let [{:keys [by-hour total periods after-6pm-pct]} schedule]
        (.append sb "```\n")
        (doseq [h (range 24)]
          (let [n (get by-hour h 0)]
            (when (pos? n)
              (.append sb (str (format "%02d:00 CT  " h) (bar n 15) " " n "\n")))))
        (.append sb "```\n\n")

        (.append sb (str "| Period | Commits | % |\n"))
        (.append sb (str "| --- | --- | --- |\n"))
        (doseq [[label k] [["Early morning (5-7)" :early-morning]
                            ["Morning (8-11)" :morning]
                            ["Afternoon (12-16)" :afternoon]
                            ["Evening (17-20)" :evening]
                            ["Late night (21-23)" :late-night]
                            ["Deep night (0-4)" :deep-night]]]
          (let [n (get periods k 0)]
            (when (pos? n)
              (.append sb (str "| " label " | " n " | "
                               (pct-str (* 100.0 (/ n (max total 1)))) " |\n")))))
        (.append sb "\n")

        ;; Interpretation
        (cond
          (> after-6pm-pct 50)
          (.append sb (str "**Schedule signal:** " (pct-str after-6pm-pct)
                          " of commits after 6pm CT. You're working evenings.\n\n"))
          (> after-6pm-pct 30)
          (.append sb (str "**Schedule signal:** " (pct-str after-6pm-pct)
                          " of commits after 6pm CT. Mixed schedule.\n\n"))
          :else
          (.append sb (str "**Schedule signal:** " (pct-str after-6pm-pct)
                          " of commits after 6pm CT. Mostly daytime work.\n\n")))))

    ;; --- Evidence Discipline ---
    (.append sb "## Evidence Discipline\n\n")
    (when evidence
      (let [{:keys [total-entries joe-turns agent-turns delegation-ratio
                    forum-posts coordinations psrs purs pars
                    sessions par-coverage]} evidence]
        (.append sb (str "| Metric | Value |\n"))
        (.append sb (str "| --- | --- |\n"))
        (.append sb (str "| Evidence entries | " total-entries " |\n"))
        (.append sb (str "| Joe turns | " joe-turns " |\n"))
        (.append sb (str "| Agent turns | " agent-turns " |\n"))
        (.append sb (str "| Delegation ratio | " (format "%.1f" delegation-ratio) " agent turns per joe turn |\n"))
        (.append sb (str "| Sessions | " sessions " |\n"))
        (.append sb (str "| PSRs | " psrs " |\n"))
        (.append sb (str "| PURs | " purs " |\n"))
        (.append sb (str "| PARs | " pars " |\n"))
        (.append sb (str "| PAR coverage | " (pct-str par-coverage) " of sessions |\n"))
        (.append sb "\n")

        ;; Interpretations
        (when (zero? psrs)
          (.append sb "**Discipline gap:** Zero PSRs. Pattern selections aren't being recorded.\n"))
        (when (zero? purs)
          (.append sb "**Discipline gap:** Zero PURs. Pattern outcomes aren't being recorded.\n"))
        (when (zero? pars)
          (.append sb "**Discipline gap:** Zero PARs. Sessions end without reflection.\n"))
        (when (and (pos? sessions) (< par-coverage 50))
          (.append sb (str "**Discipline gap:** Only " (pct-str par-coverage)
                          " of sessions have a PAR. Target: 100%.\n")))
        (when (pos? psrs)
          (.append sb (str "**Discipline signal:** " psrs " PSRs filed.\n")))
        (.append sb "\n")))

    ;; --- Stack Breadth ---
    (.append sb "## Stack Breadth\n\n")
    (when (seq breadth)
      (let [total-commits (reduce + 0 (map :commits breadth))
            top (first (sort-by :commits > breadth))
            concentration (if (pos? total-commits)
                            (* 100.0 (/ (:commits top) total-commits))
                            0.0)]
        (doseq [{:keys [label commits]} (sort-by :commits > breadth)]
          (.append sb (str "- " label ": " commits
                          " (" (pct-str (* 100.0 (/ commits (max total-commits 1)))) ")\n")))
        (.append sb "\n")
        (when (> concentration 70)
          (.append sb (str "**Breadth signal:** " (pct-str concentration)
                          " of commits in " (:label top)
                          ". The rest of the stack is languishing.\n\n")))))

    ;; --- Creative Workflow ---
    (.append sb "## Creative Workflow\n\n")
    (if (:available creative)
      (do
        (.append sb (str "Recordings on file: " (:total creative) "\n"))
        (when (seq (:recent creative))
          (.append sb "Recent:\n")
          (doseq [r (:recent creative)]
            (.append sb (str "- " (or (:title r) (:date r) "untitled") "\n"))))
        (.append sb "\n"))
      (.append sb (str (or (:note creative) "Not available.") "\n\n")))

    (str sb)))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn generate-hud
  "Collect all signals and render the HUD."
  [days]
  (let [now (.toString (.toLocalDate (.atZone (Instant/now) tz)))
        schedule (scan-work-schedule days)
        evidence (scan-evidence-discipline 1000)
        breadth (scan-stack-breadth days)
        creative (scan-creative)]
    {:data {:schedule schedule :evidence evidence :breadth breadth :creative creative}
     :markdown (render-hud {:schedule schedule :evidence evidence
                            :breadth breadth :creative creative
                            :now now :days days})}))

(defn -main [& args]
  (let [days (if (seq args) (Integer/parseInt (first args)) 14)
        {:keys [markdown]} (generate-hud days)]
    (println markdown)))
