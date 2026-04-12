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
(def ^:private futon3c-url
  (or (System/getenv "FUTON3C_EVIDENCE_BASE")
      (System/getenv "FUTON3C_SERVER")
      "http://localhost:47070"))
(def ^:private tz (ZoneId/of "Europe/London"))

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

(defn- tz-abbr
  "Current short zone name, e.g. \"GMT\" or \"BST\"."
  []
  (.format (ZonedDateTime/now tz) (DateTimeFormatter/ofPattern "z")))

(defn- diff-bar
  "Render a bar showing current vs previous.
   # = shared, + = growth, - = shrinkage."
  [current previous max-width]
  (let [shared (min current previous)
        growth (max 0 (- current previous))
        shrink (max 0 (- previous current))
        total  (+ shared growth shrink)
        scale  (if (> total max-width) (/ (double max-width) total) 1.0)
        s-n    (Math/round (* shared scale))
        g-n    (Math/round (* growth scale))
        k-n    (Math/round (* shrink scale))]
    (str (apply str (repeat s-n \#))
         (apply str (repeat g-n \+))
         (apply str (repeat k-n \-)))))

;; ---------------------------------------------------------------------------
;; Signal 1: Work Schedule (commit time-of-day)
;; ---------------------------------------------------------------------------

(defn- git-commit-timestamps
  "Get ISO timestamps for all commits in window."
  ([repo-path since]
   (git-commit-timestamps repo-path since nil))
  ([repo-path since until]
   (when-let [out (if until
                    (git repo-path "log" "--since" since "--until" until "--format=%aI")
                    (git repo-path "log" "--since" since "--format=%aI"))]
     (when (seq out)
       (str/split-lines out)))))

(defn- parse-local-hour
  "Extract hour-of-day in Joe's timezone from an ISO timestamp string."
  [ts-str]
  (try
    (let [zdt (ZonedDateTime/parse ts-str)
          local (.withZoneSameInstant zdt tz)]
      (.getHour local))
    (catch Exception _ nil)))

(defn- parse-local-datetime
  "Parse an ISO timestamp to a ZonedDateTime in Joe's timezone."
  [ts-str]
  (try
    (.withZoneSameInstant (ZonedDateTime/parse ts-str) tz)
    (catch Exception _ nil)))

(defn- collect-hours
  "Gather commit hours from repos in a [since, until) window."
  [since until]
  (for [{:keys [label path]} git-repos
        :let [timestamps (git-commit-timestamps path since until)]
        :when timestamps
        ts timestamps
        :let [h (parse-local-hour ts)]
        :when h]
    {:hour h :repo label}))

(defn- latest-commit-by-day
  "Return a map of date-string -> latest local hour for that day.
   Scans all repos in the given [since, until) window."
  [since until]
  (let [all-ts (for [{:keys [path]} git-repos
                     :let [timestamps (git-commit-timestamps path since until)]
                     :when timestamps
                     ts timestamps
                     :let [zdt (parse-local-datetime ts)]
                     :when zdt]
                 zdt)]
    (->> all-ts
         (group-by #(str (.toLocalDate %)))
         (map (fn [[day zdts]]
                [day (apply max (map #(.getHour %) zdts))]))
         (into (sorted-map)))))

(defn- summarise-hours
  "Compute by-hour frequencies and period buckets from hour records."
  [hour-records]
  (let [by-hour (frequencies (map :hour hour-records))
        total (count hour-records)
        early-morning (reduce + 0 (map #(get by-hour % 0) (range 5 8)))
        morning       (reduce + 0 (map #(get by-hour % 0) (range 8 12)))
        afternoon     (reduce + 0 (map #(get by-hour % 0) (range 12 17)))
        evening       (reduce + 0 (map #(get by-hour % 0) (range 17 21)))
        late-night    (reduce + 0 (map #(get by-hour % 0) (range 21 24)))
        deep-night    (reduce + 0 (map #(get by-hour % 0) (range 0 5)))
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

(defn scan-work-schedule
  "Analyze commit times for current and previous windows."
  [days]
  (let [now   (LocalDate/now)
        since (.toString (.minusDays now days))
        prev  (.toString (.minusDays now (* 2 days)))
        current  (summarise-hours (collect-hours since nil))
        previous (summarise-hours (collect-hours prev since))]
    (assoc current :prev-by-hour (:by-hour previous))))

;; ---------------------------------------------------------------------------
;; Signal 2: Evidence Discipline
;; ---------------------------------------------------------------------------

(defn scan-evidence-discipline
  "Analyze evidence entries for discipline gaps."
  ([limit] (scan-evidence-discipline limit 14))
  ([limit days]
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
                                0.0)
            ;; Turn counts per day (Joe HUD window)
             day-window (let [n (max 1 days)
                              today (LocalDate/now tz)]
                          (map #(str (.minusDays today %))
                               (reverse (range n))))
             day-set (set day-window)
             turn-counts (reduce (fn [acc entry]
                                   (if-let [zdt (parse-local-datetime (:evidence/at entry))]
                                     (let [date-str (str (.toLocalDate zdt))]
                                       (if (contains? day-set date-str)
                                         (update acc date-str (fnil inc 0))
                                         acc))
                                     acc))
                                 (zipmap day-window (repeat 0))
                                 turns)]
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
          :turns {:total (count turns)
                  :by-day (mapv (fn [date] {:date date
                                            :count (get turn-counts date 0)})
                                day-window)}
          :dates {:earliest (first (sort (keep :evidence/at entries)))
                  :latest (last (sort (keep :evidence/at entries)))}})))))

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
;; Signal 4: Creative Workflow
;; ---------------------------------------------------------------------------

(def ^:private recordings-index
  "Path to Zoom R4 sync index."
  (str home "/code/storage/zoomr4/meta/zoom_sync_index.json"))

(defn scan-creative
  "Analyze recording activity vs previous-night commit times.
   The hypothesis: late commits → oversleep → no morning recording."
  [days]
  (let [index (java.io.File. recordings-index)]
    (if (.exists index)
      (try
        (let [data (json/parse-string (slurp index) true)
              entries (:entries data)
              now (LocalDate/now)
              cutoff (.toString (.minusDays now days))
              ;; Recording dates in window (unique days)
              rec-dates (->> entries
                             (filter #(when-let [d (:recorded_at %)]
                                        (>= (compare d cutoff) 0)))
                             (map #(subs (:recorded_at %) 0 10))
                             distinct
                             set)
              ;; Latest commit hour per day (need one extra day before window)
              commit-by-day (latest-commit-by-day
                             (.toString (.minusDays now (inc days)))
                             nil)
              ;; Build daily log: for each day in window, did recording happen?
              ;; And what was the latest commit the night before?
              daily (vec
                     (for [offset (range days 0 -1)
                           :let [day (.toString (.minusDays now offset))
                                 prev-day (.toString (.minusDays now (inc offset)))
                                 recorded? (contains? rec-dates day)
                                 prev-latest-hour (get commit-by-day prev-day)]]
                       {:date day
                        :recorded recorded?
                        :prev-last-commit-hour prev-latest-hour}))]
          {:available true
           :total (count entries)
           :daily daily})
        (catch Exception e
          {:available false :error (.getMessage e)}))
      {:available false
       :note "No recording index found (expected zoomr4/meta/zoom_sync_index.json)."})))

;; ---------------------------------------------------------------------------
;; Operator: sorry topology + pocketwatch ticks (reads futon5a when present)
;; ---------------------------------------------------------------------------

(def ^:private futon5a-root (str home "/code/futon5a"))

(defn- read-edn-file [path]
  (try
    (when (.exists (java.io.File. path))
      (read-string (slurp path)))
    (catch Exception _ nil)))

(def ^:private workstream-repos
  "Repo paths classified by workstream. Used to compute commit ratios."
  {:stack       (mapv :path git-repos)
   :portfolio   [(str home "/vsat.wiki")
                 (str home "/code/futon7")
                 (str home "/code/futon7a")]
   :consulting  [(str home "/npt")]
   :mathematics [(str home "/code/futon6")]})

(defn- count-commits-since
  "Count commits in a repo since a date string."
  [repo-path since]
  (try
    (when (.isDirectory (java.io.File. repo-path))
      (when-let [out (git repo-path "log" "--oneline" "--since" since)]
        (count (str/split-lines out))))
    (catch Exception _ 0)))

(defn- workstream-commit-ratios
  "Compute commit counts per workstream in the given window."
  [days]
  (let [since (str (.minusDays (LocalDate/now tz) days))]
    (into {}
          (map (fn [[ws paths]]
                 [ws (reduce + 0 (map #(or (count-commits-since % since) 0) paths))])
               workstream-repos))))

(def ^:private turn-topic-patterns
  "Regex patterns for extracting topic signals from evidence turn text."
  {:bristol      #"(?i)bristol|may.?12"
   :ukrn         #"(?i)ukrn|nicola|elle|training"
   :prospectus   #"(?i)prospectus|invoice|revenue|paid|commercial"
   :working-paper #"(?i)working.paper|wp.?draft|annex"
   :math         #"(?i)prelim|proof|theorem|nlab|hyperreal"
   :stack        #"(?i)repl|agency|blackboard|emacs|futon3c|evidence.*browser"
   :jsdq         #"(?i)jsdq|hypergraph|sorry.*topology|sector"
   :portfolio    #"(?i)portfolio|consulting|depositing|cargo|free.solo"})

(defn- evidence-topic-counts
  "Count topic mentions in evidence turn text from the API."
  []
  (when-let [data (http-get-json (str futon3c-url "/api/alpha/evidence?limit=1000"))]
    (let [entries (or (:entries data) [])]
      (reduce
       (fn [acc entry]
         (let [body (:body entry (get entry :evidence/body {}))
               text (str (or (:text body) ""))]
           (if (str/blank? text)
             acc
             (reduce-kv
              (fn [a topic pat]
                (if (re-find pat text)
                  (update a topic (fnil inc 0))
                  a))
              acc
              turn-topic-patterns))))
       {}
       entries))))

(defn- sorry-signals
  "Compute sorry-filling signals from existing data."
  [days]
  (let [ratios (workstream-commit-ratios days)
        total (max 1 (reduce + (vals ratios)))
        stack-pct (/ (double (:stack ratios 0)) total)
        portfolio-pct (/ (double (:portfolio ratios 0)) total)
        consulting-pct (/ (double (:consulting ratios 0)) total)
        math-pct (/ (double (:mathematics ratios 0)) total)
        prospectus-exists? (.exists (java.io.File. (str home "/vsat.wiki/prospectus.md")))
        wp-exists? (.exists (java.io.File. (str home "/vsat.wiki/ukrn-demo/UKRN_WP_draft_v2.md")))
        topic-counts (or (evidence-topic-counts) {})]
    {:commit-ratios ratios
     :stack-pct stack-pct
     :depositing-pct (+ portfolio-pct consulting-pct)
     :foraging-signal (and (> stack-pct 0.7) (< consulting-pct 0.05))
     :cargo-signal (and wp-exists? (not prospectus-exists?))
     :prospectus-exists? prospectus-exists?
     :wp-exists? wp-exists?
     :topic-counts topic-counts}))

(defn scan-operator
  "Read sorry topology and logic model from futon5a. Gracefully absent."
  []
  (let [alignment (read-edn-file (str futon5a-root "/data/alignment.edn"))
        logic-model (read-edn-file (str futon5a-root "/data/stack-logic-model.edn"))
        snapshot (read-edn-file (str futon5a-root "/data/jsdq-terminal-vocabulary.edn"))
        signals (sorry-signals 14)]
    (when (or alignment logic-model)
      (let [sorrys (when alignment
                     (->> (:sorry-topology alignment)
                          (filter #(#{:critical :warning} (:severity %)))
                          (sort-by #(case (:severity %) :critical 0 :warning 1 2))))
            constraints (when snapshot
                          (->> (:a/constraints snapshot)
                               (filter :violated-by)))
            workstreams (when logic-model
                          (:workstreams logic-model))
            pocketwatch (when logic-model
                          (:pocketwatch logic-model))
            ticks (when pocketwatch
                    (:ticks pocketwatch))]
        {:available true
         :sorrys (vec sorrys)
         :constraints (vec (or constraints []))
         :workstreams (vec (or workstreams []))
         :pocketwatch-allocation (:target-allocation pocketwatch)
         :ticks (vec (or ticks []))
         :signals signals}))))

;; ---------------------------------------------------------------------------
;; Render
;; ---------------------------------------------------------------------------

(defn- pct-str [n] (format "%.0f%%" (double n)))

(defn- render-table
  "Render a justified markdown table.
   `headers` is a vec of header strings.
   `aligns` is a vec of :left or :right per column.
   `rows` is a vec of vecs of cell strings."
  [headers aligns rows]
  (let [ncols (count headers)
        widths (mapv (fn [i]
                       (apply max (count (nth headers i))
                              (map #(count (nth % i "")) rows)))
                     (range ncols))
        pad (fn [s w align]
              (let [s (or s "")
                    gap (- w (count s))
                    lpad (quot gap 2)
                    rpad (- gap lpad)]
                (case align
                  :right  (str (apply str (repeat gap \space)) s)
                  :center (str (apply str (repeat lpad \space)) s (apply str (repeat rpad \space)))
                  (str s (apply str (repeat gap \space))))))
        fmt-row (fn [cells]
                  (str "| "
                       (str/join " | "
                                 (map-indexed (fn [i cell]
                                                (pad cell (nth widths i) (nth aligns i)))
                                              cells))
                       " |\n"))
        sep (str "|"
                 (str/join "|"
                           (map-indexed (fn [i a]
                                          (let [w (nth widths i)]
                                            (case a
                                              :right  (str (apply str (repeat (inc w) \-)) ":")
                                              :center (str ":" (apply str (repeat w \-)) ":")
                                              (str (apply str (repeat (+ w 2) \-))))))
                                        aligns))
                 "|\n")]
    (str (fmt-row headers) sep (apply str (map fmt-row rows)))))

(defn render-hud
  "Render the Joe HUD as readable markdown."
  [{:keys [schedule evidence breadth creative operator now days] :as data}]
  (let [sb (StringBuilder.)]
    (.append sb (str "# Joe HUD\n\n"))
    (.append sb (str "**" now "** | " days "-day window\n\n"))

    ;; --- Work Schedule ---
    (.append sb "## Work Schedule\n\n")
    (when schedule
      (let [{:keys [by-hour prev-by-hour total periods after-6pm-pct]} schedule]
        (.append sb "```\n")
        (.append sb "# = both periods  + = growth  - = shrinkage (vs prior window)\n")
        (doseq [h (range 24)]
          (let [cur  (get by-hour h 0)
                prev (get prev-by-hour h 0)]
            (when (or (pos? cur) (pos? prev))
              (.append sb (str (format "%02d:00 %s  " h (tz-abbr))
                               (diff-bar cur prev 40)
                               " " cur
                               (when (not= cur prev)
                                 (str " (was " prev ")"))
                               "\n")))))
        (.append sb "```\n\n")

        (let [period-rows (vec (for [[label k] [["Early morning (5-7)" :early-morning]
                                                ["Morning (8-11)" :morning]
                                                ["Afternoon (12-16)" :afternoon]
                                                ["Evening (17-20)" :evening]
                                                ["Late night (21-23)" :late-night]
                                                ["Deep night (0-4)" :deep-night]]
                                     :let [n (get periods k 0)]
                                     :when (pos? n)]
                                 [(str label) (str n) (pct-str (* 100.0 (/ n (max total 1))))]))]
          (.append sb (render-table ["Period" "Commits" "%"]
                                    [:left :right :right]
                                    period-rows)))
        (.append sb "\n")

        ;; Interpretation
        (cond
          (> after-6pm-pct 50)
          (.append sb (str "**Schedule signal:** " (pct-str after-6pm-pct)
                           " of commits after 6pm. You're working evenings.\n\n"))
          (> after-6pm-pct 30)
          (.append sb (str "**Schedule signal:** " (pct-str after-6pm-pct)
                           " of commits after 6pm. Mixed schedule.\n\n"))
          :else
          (.append sb (str "**Schedule signal:** " (pct-str after-6pm-pct)
                           " of commits after 6pm. Mostly daytime work.\n\n")))))

    ;; --- Evidence Discipline ---
    (.append sb "## Evidence Discipline\n\n")
    (when evidence
      (let [{:keys [total-entries joe-turns agent-turns delegation-ratio
                    forum-posts coordinations psrs purs pars
                    sessions par-coverage]} evidence]
        (.append sb (render-table
                     ["Metric" "Value"]
                     [:left :left]
                     [["Evidence entries" (str total-entries)]
                      ["Joe turns" (str joe-turns)]
                      ["Agent turns" (str agent-turns)]
                      ["Delegation ratio" (str (format "%.1f" delegation-ratio) " agent turns per joe turn")]
                      ["Sessions" (str sessions)]
                      ["PSRs" (str psrs)]
                      ["PURs" (str purs)]
                      ["PARs" (str pars)]
                      ["PAR coverage" (str (pct-str par-coverage) " of sessions")]]))
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
        (.append sb "\n")
        (when-let [turns (:turns evidence)]
          (let [{:keys [total by-day]} turns]
            (.append sb (str "Turns logged (last " (count by-day) " days): "
                             total "\n\n"))
            (.append sb (render-table ["Date" "Turns"]
                                      [:left :right]
                                      (mapv (fn [{:keys [date count]}]
                                              [date (str count)])
                                            by-day)))
            (.append sb "\n")))))

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
      (let [{:keys [daily total]} creative
            rec-days (count (filter :recorded daily))
            total-days (count daily)]
        (.append sb (str "Recordings: " total " total, "
                         rec-days "/" total-days " days in window\n\n"))
        (let [creative-rows (mapv (fn [{:keys [date recorded prev-last-commit-hour]}]
                                    [date
                                     (if recorded "yes" "no")
                                     (if prev-last-commit-hour
                                       (format "%02d:00" prev-last-commit-hour)
                                       "-")])
                                  daily)]
          (.append sb (render-table ["Date" "Recorded" "Last commit night before"]
                                    [:left :right :right]
                                    creative-rows)))
        (.append sb "\n")
        ;; Correlation: only consider evenings (18:00+)
        (let [evening? #(and (:prev-last-commit-hour %) (>= (:prev-last-commit-hour %) 18))
              evening-days (filter evening? daily)
              rec-prev (map :prev-last-commit-hour (filter :recorded evening-days))
              miss-prev (map :prev-last-commit-hour (remove :recorded evening-days))]
          (when (and (seq rec-prev) (seq miss-prev))
            (let [avg-rec (/ (reduce + 0.0 rec-prev) (count rec-prev))
                  avg-miss (/ (reduce + 0.0 miss-prev) (count miss-prev))]
              (.append sb (str "**Pattern (evening commits only):** Before recording days: "
                               (format "%02.0f:00" avg-rec)
                               " / before missed days: "
                               (format "%02.0f:00" avg-miss) "\n\n"))))))
      (.append sb (str (or (:note creative) "Not available.") "\n\n")))

    ;; --- Operator (futon5a) ---
    (when-let [op (:operator data)]
      (when (:available op)
        (.append sb "## Operator\n\n")
        ;; Sorry topology
        (when (seq (:sorrys op))
          (.append sb "**Active sorrys:**\n\n")
          (.append sb (render-table
                       ["Severity" "Sorry" "Status" "Closes by"]
                       [:left :left :left :left]
                       (mapv (fn [s]
                               [(name (or (:severity s) :unknown))
                                (name (or (:id s) :unknown))
                                (name (or (:status s) :unknown))
                                (or (:closes-by s) "-")])
                             (:sorrys op))))
          (.append sb "\n"))
        ;; Pocketwatch allocation
        (when (:pocketwatch-allocation op)
          (.append sb "**Pocketwatch target:**\n\n")
          (.append sb (render-table
                       ["Workstream" "Target h" "Signal" "Note"]
                       [:left :right :left :left]
                       (mapv (fn [[k v]]
                               [(name k)
                                (if (:hours v) (format "%.1f" (double (:hours v))) "-")
                                (name (or (:signal v) :none))
                                (or (:note v) "-")])
                             (:pocketwatch-allocation op))))
          (.append sb "\n"))
        ;; Constraint ticks
        (when (seq (:ticks op))
          (.append sb "**Pocketwatch ticks:**\n\n")
          (doseq [t (:ticks op)]
            (.append sb (str "- **" (:id t) ":** " (:condition t) "\n")))
          (.append sb "\n"))
        ;; Workstreams
        (when (seq (:workstreams op))
          (.append sb "**Workstreams:**\n\n")
          (.append sb (render-table
                       ["Stream" "JSDQ mode" "Target h" "Constraint"]
                       [:left :left :right :left]
                       (mapv (fn [w]
                               [(:label w)
                                (name (or (:jsdq-mode w) :unknown))
                                (format "%.1f" (double (get-in w [:pocketwatch-hours :target] 0)))
                                (or (:constraint w) "-")])
                             (:workstreams op))))
          (.append sb "\n"))
        ;; Signals from existing data
        (when-let [signals (:signals op)]
          (.append sb "**Signals (14-day window, from git commits):**\n\n")
          (let [{:keys [commit-ratios stack-pct depositing-pct
                        foraging-signal cargo-signal
                        prospectus-exists? wp-exists?]} signals]
            (.append sb (render-table
                         ["Workstream" "Commits" "%"]
                         [:left :right :right]
                         (mapv (fn [[ws n]]
                                 [(name ws)
                                  (str n)
                                  (pct-str (* 100.0 (/ (double n)
                                                       (max 1 (reduce + (vals commit-ratios))))))])
                               (sort-by val > commit-ratios))))
            (.append sb "\n")
            ;; Artifact signals
            (.append sb (str "- Working Paper: " (if wp-exists? "exists" "**MISSING**") "\n"))
            (.append sb (str "- Prospectus: " (if prospectus-exists? "exists" "**MISSING**") "\n"))
            ;; Evidence turn topic counts
            (when (seq (:topic-counts signals))
              (.append sb "**Evidence turn topics (what Joe actually talks about):**\n\n")
              (.append sb (render-table
                           ["Topic" "Mentions"]
                           [:left :right]
                           (mapv (fn [[topic n]] [(name topic) (str n)])
                                 (sort-by val > (:topic-counts signals)))))
              (.append sb "\n"))
            ;; Firing ticks
            (when foraging-signal
              (.append sb "**TICK: foraging-warning** — >70% stack commits, <5% consulting. Mode appears stuck on :foraging.\n\n"))
            (when cargo-signal
              (.append sb "**TICK: cargo** — Working Paper exists but prospectus does not. Cargo undelivered.\n\n"))
            (.append sb "\n")))))

    (str sb)))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn generate-hud
  "Collect all signals and render the HUD."
  [days]
  (let [now (.toString (.toLocalDate (.atZone (Instant/now) tz)))
        schedule (scan-work-schedule days)
        evidence (scan-evidence-discipline 1000 days)
        breadth (scan-stack-breadth days)
        creative (scan-creative days)
        operator (scan-operator)]
    {:data {:schedule schedule :evidence evidence :breadth breadth
            :creative creative :operator operator}
     :markdown (render-hud {:schedule schedule :evidence evidence
                            :breadth breadth :creative creative
                            :operator operator
                            :now now :days days})}))

(defn -main [& args]
  (let [days (if (seq args) (Integer/parseInt (first args)) 14)
        {:keys [markdown]} (generate-hud days)]
    (println markdown)))
