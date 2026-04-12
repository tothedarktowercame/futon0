(ns futon0.report.war-machine
  "War Machine — strategic synthesis scans.

   Harmonizes terminal vocabularies across the futon stack into a
   unified observation of strategic state:

   | Source vocabulary          | What it contributes               |
   |----------------------------|-----------------------------------|
   | joe-terminal (futon5a)     | Personal life signals             |
   | jsdq-terminal (futon5a)    | Market interface signals          |
   | peripheral-aif (futon3c)   | Stack observation channels        |
   | logic model (futon5a)      | Dimensions, workstreams, ticks    |
   | sorry topology (futon5a)   | Constraint domain, typed holes    |
   | holistic argument (futon3) | Structural claims, loop arrows    |

   Each scan function follows the g-observe pattern from cyberants
   (futon2/src/ants/aif/observe.clj): read external state, normalize,
   return a data map. The scan functions ARE the war machine's
   observation layer — the AIF 'o' for the strategic domain.

   Scan → Observation → Tick pattern (cf. joe_hud.clj):
     scan-X [days] → {data}       (raw observation)
     generate-war-machine [days]   (compose all scans)
     render-war-machine [data]     (produce markdown)

   Invariant: WM-I1 (read-only observer — no writes to stack).
   Pattern:   war-machine/operational-not-decorative"
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.java.shell :as shell]
            [clojure.set]
            [clojure.string :as str])
  (:import (java.time Instant LocalDate ZoneId ZonedDateTime)
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
(def ^:private futon5a-root (str home "/code/futon5a"))

(def ^:private all-repos
  "All repos in the stack, classified by workstream."
  [{:label "futon0"  :path (str home "/code/futon0")  :workstream :stack}
   {:label "futon1"  :path (str home "/code/futon1")  :workstream :stack}
   {:label "futon1a" :path (str home "/code/futon1a") :workstream :stack}
   {:label "futon2"  :path (str home "/code/futon2")  :workstream :stack}
   {:label "futon3"  :path (str home "/code/futon3")  :workstream :stack}
   {:label "futon3a" :path (str home "/code/futon3a") :workstream :stack}
   {:label "futon3b" :path (str home "/code/futon3b") :workstream :stack}
   {:label "futon3c" :path (str home "/code/futon3c") :workstream :stack}
   {:label "futon4"  :path (str home "/code/futon4")  :workstream :stack}
   {:label "futon5"  :path (str home "/code/futon5")  :workstream :stack}
   {:label "futon5a" :path (str home "/code/futon5a") :workstream :stack}
   {:label "futon6"  :path (str home "/code/futon6")  :workstream :mathematics}
   {:label "futon7"  :path (str home "/code/futon7")  :workstream :portfolio}
   {:label "futon7a" :path (str home "/code/futon7a") :workstream :portfolio}
   {:label "vsat.wiki" :path (str home "/vsat.wiki")  :workstream :portfolio}
   {:label "npt"     :path (str home "/npt")          :workstream :consulting}])

;; ---------------------------------------------------------------------------
;; Helpers (same patterns as joe_hud.clj)
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

(defn- read-edn-file [path]
  (try
    (when (.exists (java.io.File. path))
      (read-string (slurp path)))
    (catch Exception _ nil)))

(defn- since-str
  "Date string for N days ago."
  [days]
  (.toString (.minusDays (LocalDate/now tz) days)))

(defn- parse-iso-date
  "Extract YYYY-MM-DD from an ISO timestamp string."
  [ts-str]
  (try
    (when (and ts-str (>= (count ts-str) 10))
      (subs ts-str 0 10))
    (catch Exception _ nil)))

(defn- count-commits-since
  "Count commits in a repo since a date string."
  [repo-path since]
  (try
    (when (.isDirectory (java.io.File. repo-path))
      (when-let [out (git repo-path "log" "--oneline" "--since" since)]
        (if (str/blank? out) 0 (count (str/split-lines out)))))
    (catch Exception _ 0)))

;; ---------------------------------------------------------------------------
;; Evidence API helpers
;; ---------------------------------------------------------------------------

(defn- fetch-evidence
  "Fetch evidence entries from the API. Returns vec or nil."
  [limit]
  (when-let [data (http-get-json (str futon3c-url "/api/alpha/evidence?limit=" limit))]
    (when (:ok data)
      (or (:entries data) []))))

(defn- fetch-missions
  "Fetch mission inventory from the API. Returns vec or nil."
  []
  (when-let [data (http-get-json (str futon3c-url "/api/alpha/missions"))]
    (when (:ok data)
      (or (:missions data) []))))

;; ---------------------------------------------------------------------------
;; Scan 1: Loop Health
;;
;; Maps the 6 arrows in the holistic argument self-improvement loop
;; to evidence freshness. Each arrow is a step in:
;;
;;   work → proof-paths → pattern-discovery → coordination
;;        → self-representation → portfolio-inference → better-work
;;
;; cf. holistic-argument-sketch.md lines 215-226
;; cf. cyberants AIF: g-observe normalizes sensory channels to [0,1]
;; ---------------------------------------------------------------------------

(def ^:private loop-arrows
  "The six arrows of the self-improvement loop.
   Each arrow has evidence-type filters for detecting when it fires."
  [{:id :work→proof
    :label "Work → Proof paths"
    :description "Work produces auditable evidence via gate pipeline"
    :detect-fn
    (fn [entries _] ;; Any chat-turn = work is happening
      (filter #(= "chat-turn" (get-in % [:evidence/body :event])) entries))}

   {:id :proof→patterns
    :label "Proof paths → Pattern discovery"
    :description "Evidence feeds pattern discovery (Baldwin cycle)"
    :detect-fn
    (fn [entries _] ;; PSR/PUR evidence OR goal-typed claims OR pattern mentions
      (filter #(or (#{"pattern-selection" "pattern-outcome"} (:evidence/type %))
                   (= "goal" (:evidence/claim-type %))
                   (when-let [text (get-in % [:evidence/body :text])]
                     (re-find #"(?i)pattern.*select|PSR|PUR|pattern.*record" text)))
              entries))}

   {:id :patterns→coordination
    :label "Patterns → Coordination"
    :description "Patterns inform coordination via ambient retrieval"
    :detect-fn
    (fn [entries _] ;; Context retrieval certificates or retrieval mentions
      (filter #(or (= "context-retrieval" (:evidence/type %))
                   (when-let [text (get-in % [:evidence/body :text])]
                     (re-find #"(?i)context.?retriev|pattern.?retriev|notions.*search" text)))
              entries))}

   {:id :coordination→self-rep
    :label "Coordination → Self-representation"
    :description "Coordination improvements update devmaps/missions"
    :detect-fn
    (fn [_ since] ;; Commits to mission docs, devmaps, or data files
      (let [mission-repos [(str home "/code/futon3c")
                           (str home "/code/futon3b")
                           (str home "/code/futon3")
                           (str home "/code/futon5a")]]
        (->> mission-repos
             (mapcat (fn [rp]
                       (when-let [out (git rp "log" "--format=%aI %s" "--since" since
                                          "--" "holes/missions/" "devmaps/" "data/")]
                         (when-not (str/blank? out)
                           (map (fn [line]
                                  (let [space-idx (.indexOf line " ")
                                        ts (when (pos? space-idx) (subs line 0 space-idx))]
                                    {:evidence/type "git-commit"
                                     :evidence/at ts
                                     :evidence/body {:text line}}))
                                (str/split-lines out))))))
             vec)))}

   {:id :self-rep→inference
    :label "Self-representation → Portfolio inference"
    :description "Self-images feed portfolio inference engine"
    :detect-fn
    (fn [entries _] ;; Portfolio evidence or review/inference mentions
      (filter #(or (str/starts-with? (or (:evidence/type %) "") "portfolio")
                   (when-let [text (get-in % [:evidence/body :text])]
                     (re-find #"(?i)portfolio|inference|review|triage" text)))
              entries))}

   {:id :inference→work
    :label "Portfolio inference → Better work"
    :description "Inference improves work selection"
    :detect-fn
    (fn [entries _] ;; Forum posts = collective coordination outputs
      (filter #(= "forum-post" (:evidence/type %)) entries))}])

(defn- arrow-health
  "Compute health [0,1] from evidence count and recency.
   Combines frequency (count in window) and freshness (days since last)."
  [evidence-count days-since-last window-days]
  (let [;; Frequency component: >10 entries/window = healthy
        freq (min 1.0 (/ (double evidence-count) 10.0))
        ;; Freshness component: seen today = 1.0, unseen in window = 0.0
        fresh (if days-since-last
                (max 0.0 (- 1.0 (/ (double days-since-last) (double window-days))))
                0.0)]
    ;; Geometric mean — both must be nonzero for health
    (Math/sqrt (* freq fresh))))

(defn scan-loop-health
  "Scan the 6 arrows of the holistic argument self-improvement loop.

   For each arrow, detects evidence of it firing in the window.
   Returns a map with :arrows (per-arrow health) and :overall.

   cf. cyberants observe.clj — each arrow is a sensory channel,
   health is the normalized [0,1] observation."
  [days]
  (let [since (since-str days)
        today (str (LocalDate/now tz))
        entries (or (fetch-evidence 2000) [])
        ;; Filter entries to window
        window-entries (filter (fn [e]
                                 (when-let [d (parse-iso-date (:evidence/at e))]
                                   (>= (compare d since) 0)))
                               entries)
        arrows (mapv (fn [{:keys [id label description detect-fn]}]
                       (let [matches (detect-fn window-entries since)
                             cnt (count matches)
                             last-date (when (seq matches)
                                         (->> matches
                                              (keep #(parse-iso-date (:evidence/at %)))
                                              sort
                                              last))
                             days-since (when last-date
                                          (let [last-ld (LocalDate/parse last-date)
                                                now-ld (LocalDate/now tz)]
                                            (.getDays (java.time.Period/between last-ld now-ld))))
                             health (arrow-health cnt days-since days)]
                         {:arrow-id id
                          :label label
                          :description description
                          :count cnt
                          :last-seen last-date
                          :days-since days-since
                          :health health}))
                     loop-arrows)
        healthy (filter #(> (:health %) 0.5) arrows)
        overall (if (seq arrows)
                  (/ (reduce + 0.0 (map :health arrows)) (count arrows))
                  0.0)]
    {:arrows arrows
     :overall overall
     :healthy-count (count healthy)
     :total-count (count arrows)
     :loop-complete? (= (count healthy) (count arrows))}))

;; ---------------------------------------------------------------------------
;; Scan 2: Support / Attack
;;
;; Maps the structural claims (S1-S5 support, A1-A4 attack) from the
;; holistic argument to evidence freshness.
;;
;; cf. holistic-argument-sketch.md lines 127-195
;; cf. JSDQ terminal vocabulary — same normalized observation pattern
;; ---------------------------------------------------------------------------

(def ^:private claim-patterns
  "Regex patterns for matching evidence text to structural claims.
   Follows the turn-topic-patterns approach from joe_hud.clj."
  {:S1 {:type :support :label "Evidence discipline works"
        :pattern #"(?i)pattern|evidence|traceab|PSR|PUR|gate|discipline|replicat"}
   :S2 {:type :support :label "AIF framing is generative"
        :pattern #"(?i)active.?inference|AIF|observe.*infer|free.?energy|generative.?model|predictive"}
   :S3 {:type :support :label "Pattern transfer is real"
        :pattern #"(?i)pattern.*transfer|compose.*domain|metaca.*ant|cross.?domain|flexiarg"}
   :S4 {:type :support :label "Commercial demand exists"
        :pattern #"(?i)consult|revenue|commercial|UKRN|Bristol|invoice|paid|prospect"}
   :S5 {:type :support :label "Reflexive architecture is rare"
        :pattern #"(?i)reflexiv|self.?improv|meta.?model|observe.?own|stack.*observ"}
   :A1 {:type :attack :label "Complexity cost"
        :pattern #"(?i)complex|too.?many|cognitive.?load|overwhelm|layers.*process|sprawl"}
   :A2 {:type :attack :label "Solo-developer bottleneck"
        :pattern #"(?i)bottleneck|solo|bus.?factor|single.?point|one.?person|joe.*only"}
   :A3 {:type :attack :label "Commercialisation gap"
        :pattern #"(?i)no.?revenue|unfunded|precarious|gap.*commercial|runway|burn.?rate"}
   :A4 {:type :attack :label "Explanation problem"
        :pattern #"(?i)hard.?to.?explain|jargon|opaque|communicate|legib|outsider"}})

(defn scan-support-attack
  "Scan evidence for structural claim coverage.

   For each of the 9 claims (S1-S5 support, A1-A4 attack), counts
   evidence entries whose text matches the claim's keywords.

   Returns {:claims [...] :support-coverage :attack-coverage}.
   Coverage = fraction of claims with at least one recent evidence entry."
  [days]
  (let [since (since-str days)
        entries (or (fetch-evidence 2000) [])
        window-entries (filter (fn [e]
                                 (when-let [d (parse-iso-date (:evidence/at e))]
                                   (>= (compare d since) 0)))
                               entries)
        claims (mapv
                (fn [[claim-id {:keys [type label pattern]}]]
                  (let [matches (filter
                                 (fn [e]
                                   (let [body (:evidence/body e {})
                                         text (str (or (:text body) "")
                                                   " " (or (:evidence/type e) ""))]
                                     (re-find pattern text)))
                                 window-entries)
                        cnt (count matches)
                        last-date (when (seq matches)
                                    (->> matches
                                         (keep #(parse-iso-date (:evidence/at %)))
                                         sort last))]
                    {:claim-id claim-id
                     :type type
                     :label label
                     :evidence-count cnt
                     :last-evidence last-date
                     :status (cond
                               (>= cnt 5) :strong
                               (>= cnt 1) :present
                               :else :absent)}))
                (sort-by key claim-patterns))
        support-claims (filter #(= :support (:type %)) claims)
        attack-claims (filter #(= :attack (:type %)) claims)
        covered? #(pos? (:evidence-count %))]
    {:claims claims
     :support-coverage (if (seq support-claims)
                         (/ (double (count (filter covered? support-claims)))
                            (count support-claims))
                         0.0)
     :attack-coverage (if (seq attack-claims)
                        (/ (double (count (filter covered? attack-claims)))
                           (count attack-claims))
                        0.0)}))

;; ---------------------------------------------------------------------------
;; Scan 3: Mission Triage
;;
;; Queries the missions API and classifies missions by health.
;; Detects abandoned-in-progress missions (the strategic debt).
;;
;; cf. peripheral-aif-vocabulary.sexp — mission-complete-ratio,
;; blocked-ratio, stall-count channels
;; ---------------------------------------------------------------------------

(defn scan-mission-triage
  "Scan mission inventory for strategic health.

   Queries GET /api/alpha/missions, cross-references with git activity
   to detect abandoned-in-progress missions.

   Returns {:total :by-status :by-repo :abandoned-missions :health}."
  [days]
  (let [since (since-str days)
        missions (or (fetch-missions) [])
        ;; Classify by status
        by-status (frequencies (map #(or (:mission/status %) "unknown") missions))
        ;; Classify by repo
        by-repo (frequencies (map #(or (:mission/repo %) "unknown") missions))
        ;; Detect abandoned: in-progress but no commits in window
        in-progress (filter #(#{"active" "in-progress" "in_progress"}
                               (:mission/status %))
                            missions)
        abandoned (when (seq in-progress)
                    (let [;; Build repo→commit-count cache
                          repo-commits
                          (into {}
                                (for [{:keys [label path]} all-repos]
                                  [label (or (count-commits-since path since) 0)]))]
                      (->> in-progress
                           (filter (fn [m]
                                     (let [repo (:mission/repo m)
                                           commits (get repo-commits repo 0)]
                                       (zero? commits))))
                           vec)))
        total (count missions)
        completed (get by-status "complete" 0)
        blocked (get by-status "blocked" 0)
        active (+ (get by-status "active" 0) (get by-status "in-progress" 0)
                  (get by-status "in_progress" 0))]
    {:total total
     :by-status by-status
     :by-repo by-repo
     :active active
     :completed completed
     :blocked blocked
     :abandoned-count (count (or abandoned []))
     :abandoned-missions (or abandoned [])
     ;; Health: high completion, low abandoned, low blocked
     :health (if (pos? total)
               (let [completion-ratio (/ (double completed) total)
                     abandon-penalty (/ (double (count (or abandoned []))) (max 1 active))
                     block-penalty (/ (double blocked) total)]
                 (max 0.0 (- completion-ratio (* 0.5 abandon-penalty) (* 0.3 block-penalty))))
               0.0)}))

;; ---------------------------------------------------------------------------
;; Scan 5: Sessions
;;
;; Builds a session index from the evidence store.
;; Each session becomes an "ant" that can be replayed across the hex grid.
;;
;; cf. cyberants: ant has position, mode, cargo, trail
;; cf. FuLab terminal vocabulary: session/turn terminals
;; ---------------------------------------------------------------------------

(def ^:private repo-detect-patterns
  "Patterns for detecting which repo an evidence entry relates to."
  {"futon0"  #"(?i)\bfuton0\b"
   "futon1"  #"(?i)\bfuton1\b(?!a)"
   "futon1a" #"(?i)\bfuton1a\b"
   "futon2"  #"(?i)\bfuton2\b"
   "futon3"  #"(?i)\bfuton3\b(?![abc])"
   "futon3a" #"(?i)\bfuton3a\b"
   "futon3b" #"(?i)\bfuton3b\b"
   "futon3c" #"(?i)\bfuton3c\b"
   "futon4"  #"(?i)\bfuton4\b"
   "futon5"  #"(?i)\bfuton5\b(?!a)"
   "futon5a" #"(?i)\bfuton5a\b"
   "futon6"  #"(?i)\bfuton6\b|prelim|proof|theorem|nlab"
   "futon7"  #"(?i)\bfuton7\b"
   "futon7a" #"(?i)\bfuton7a\b"
   "vsat.wiki" #"(?i)\bvsat\b|prospectus|working.paper"
   "npt"     #"(?i)\bnpt\b|consult|ukrn|bristol"})

(defn- detect-repos
  "Detect which repos an evidence entry relates to from its text."
  [entry]
  (let [text (str (get-in entry [:evidence/body :text] "")
                  " " (or (:evidence/type entry) ""))]
    (when-not (str/blank? text)
      (vec (for [[repo pat] repo-detect-patterns
                 :when (re-find pat text)]
             repo)))))

(defn- detect-missions
  "Detect which specific missions an evidence entry relates to.
   Looks for mission IDs (e.g. 'war-machine', 'portfolio-inference')
   and M- prefixed references (e.g. 'M-war-machine')."
  [entry mission-ids]
  (let [text (str (get-in entry [:evidence/body :text] ""))]
    (when-not (str/blank? text)
      (let [text-lower (str/lower-case text)]
        (vec (for [mid mission-ids
                   :when (or (str/includes? text-lower (str/lower-case mid))
                             (str/includes? text-lower (str "m-" (str/lower-case mid))))]
               mid))))))

(defn scan-sessions
  "Build session index from the evidence store.

   Each session has:
   - :session-id, :entry-count, :start, :end
   - :steps — chronological entries with detected repo AND mission positions
   - :repos-touched, :missions-touched — distinct repos/missions mentioned

   Returns {:sessions [...] :total-sessions N}."
  []
  (let [entries (or (fetch-evidence 2000) [])
        ;; Get mission IDs for mission detection
        mission-ids (when-let [missions (fetch-missions)]
                      (vec (keep :mission/id missions)))
        ;; Group by session-id
        by-session (group-by :evidence/session-id entries)
        sessions (->> by-session
                      (remove (fn [[sid _]] (nil? sid)))
                      (map (fn [[sid ses-entries]]
                             (let [sorted (sort-by :evidence/at ses-entries)
                                   start (:evidence/at (first sorted))
                                   end (:evidence/at (last sorted))
                                   steps (mapv (fn [e]
                                                 {:at (:evidence/at e)
                                                  :type (:evidence/type e)
                                                  :event (get-in e [:evidence/body :event])
                                                  :repos (detect-repos e)
                                                  :missions (if mission-ids
                                                              (detect-missions e mission-ids)
                                                              [])
                                                  :text (subs (str (get-in e [:evidence/body :text] ""))
                                                              0 (min 80 (count (str (get-in e [:evidence/body :text] "")))))})
                                               sorted)
                                   repos-touched (->> steps (mapcat :repos) distinct vec)
                                   missions-touched (->> steps (mapcat :missions) distinct vec)]
                               {:session-id sid
                                :entry-count (count ses-entries)
                                :start start
                                :end end
                                :steps steps
                                :repos-touched repos-touched
                                :missions-touched missions-touched})))
                      (sort-by :start #(compare %2 %1))
                      vec)]
    {:sessions sessions
     :total-sessions (count sessions)}))

;; ---------------------------------------------------------------------------
;; Scan 6b: Mission Detail (for mission hex view)
;;
;; Richer mission data for the zoomed-in mission view.
;; Groups missions by repo, includes dependency edges.
;; ---------------------------------------------------------------------------

(defn scan-mission-detail
  "Fetch detailed mission data for the mission hex view.
   Returns missions grouped by repo with dependency info."
  []
  (when-let [missions (fetch-missions)]
    (let [by-repo (group-by :mission/repo missions)
          ;; Extract blocked-by pairs from the portfolio structure
          step-data (try
                      (let [resp (http/post (str futon3c-url "/api/alpha/portfolio/step")
                                           {:headers {"Content-Type" "application/json"
                                                      "Accept" "application/json"}
                                            :body "{\"emit-evidence\":false}"
                                            :timeout 10000
                                            :throw false})]
                        (when (= 200 (:status resp))
                          (json/parse-string (:body resp) true)))
                      (catch Exception _ nil))
          blocked-pairs (get-in step-data [:structure :blocked-pairs] [])
          dep-edges (mapv (fn [[a b]] {:from a :to b :type :blocked-by}) blocked-pairs)]
      {:missions missions
       :by-repo by-repo
       :dependency-edges dep-edges
       :total (count missions)
       :repos (keys by-repo)})))

;; ---------------------------------------------------------------------------
;; Scan 7: Portfolio Inference
;;
;; Queries the live Portfolio Inference AIF loop.
;; Returns current belief state (μ), precision (τ), mode, urgency,
;; and the policy recommendation (ranked actions with EFE scores).
;;
;; This is the war machine reading the output of an existing AIF loop —
;; the portfolio inference engine is the "brain" and we're reading its
;; state, like reading an ant's mu/mode/hunger.
;; ---------------------------------------------------------------------------

(defn scan-portfolio
  "Query the Portfolio Inference engine for current state and recommendation.

   Calls two endpoints:
   - GET /api/alpha/portfolio/state — current beliefs without stepping
   - POST /api/alpha/portfolio/step — run one AIF step, get recommendation

   Returns {:state {...} :recommendation {...} :available? true/false}."
  ([] (scan-portfolio false))
  ([step?]
   (let [state-data (http-get-json (str futon3c-url "/api/alpha/portfolio/state"))
         step-data (when step?
                     (try
                       (let [resp (http/post (str futon3c-url "/api/alpha/portfolio/step")
                                            {:headers {"Content-Type" "application/json"
                                                       "Accept" "application/json"}
                                             :body "{\"emit-evidence\":false}"
                                             :timeout 10000
                                             :throw false})]
                         (when (= 200 (:status resp))
                           (json/parse-string (:body resp) true)))
                       (catch Exception _ nil)))]
     (if (or state-data step-data)
       {:available? true
        :state (when state-data
                 (let [s (:state state-data)]
                   {:mode (get-in s [:mu :mode])
                    :urgency (get-in s [:mu :urgency])
                    :tau (:tau s)
                    :free-energy nil ;; only available after step
                    :step-count (:step-count s)
                    :focus (get-in s [:mu :focus])
                    :channels (get-in s [:mu :sens])}))
        :recommendation (when step-data
                          {:action (:action step-data)
                           :abstain? (:abstain step-data)
                           :mode (get-in step-data [:diagnostics :mode])
                           :urgency (get-in step-data [:diagnostics :urgency])
                           :tau (get-in step-data [:diagnostics :tau])
                           :free-energy (get-in step-data [:diagnostics :free-energy])
                           :recommendation-text (:recommendation step-data)
                           :structure (:structure step-data)})}
       {:available? false}))))

;; ---------------------------------------------------------------------------
;; Scan 8: Graph (strategic state graph)
;;
;; Builds the unified graph from all sources. This is the core data
;; structure that the visualiser renders.
;;
;; Node types: repo, sorry, mission, workstream
;; Edge types: temporal-coupling, evidence-flow, sorry-affinity,
;;             workstream-dependency, logic-model-edge
;;
;; cf. war-machine-exotype.edn C-scan-graph
;; cf. cyberants: world grid with cells → here: stack with nodes/edges
;; ---------------------------------------------------------------------------

(defn- repo-nodes
  "Build repo nodes with commit activity."
  [days]
  (let [since (since-str days)]
    (vec
     (for [{:keys [label path workstream]} all-repos
           :let [commits (or (count-commits-since path since) 0)]
           :when (.isDirectory (java.io.File. path))]
       {:type :repo
        :id label
        :label label
        :workstream workstream
        :commits commits
        :active? (pos? commits)}))))

(defn- sorry-nodes
  "Build sorry nodes from the topology."
  []
  (when-let [alignment (read-edn-file (str futon5a-root "/data/alignment.edn"))]
    (vec
     (for [s (:sorry-topology alignment)]
       {:type :sorry
        :id (:id s)
        :label (name (:id s))
        :severity (:severity s)
        :status (:status s)
        :layer (:layer s)
        :closes-by (:closes-by s)}))))

(defn- workstream-nodes
  "Build workstream nodes from the logic model."
  []
  (when-let [model (read-edn-file (str futon5a-root "/data/stack-logic-model.edn"))]
    (vec
     (for [ws (:workstreams model)]
       {:type :workstream
        :id (:id ws)
        :label (:label ws)
        :jsdq-mode (:jsdq-mode ws)
        :target-hours (get-in ws [:pocketwatch-hours :target])
        :constraint (:constraint ws)}))))

(defn- mission-nodes
  "Build mission nodes from the API. Limits to active/blocked/ready
   missions to keep the graph legible."
  []
  (when-let [missions (fetch-missions)]
    (vec
     (for [m missions
           :let [status (or (:mission/status m) "unknown")]
           :when (#{"active" "in-progress" "in_progress" "blocked" "ready" "testing"} status)]
       {:type :mission
        :id (or (:mission/id m) (:id m))
        :label (or (:mission/id m) (:id m) "?")
        :status status
        :repo (or (:mission/repo m) "unknown")}))))

(defn- temporal-coupling-edges
  "Detect co-change patterns: repos changed on the same days.
   Returns edges with coupling strength [0,1]."
  [days]
  (let [since (since-str days)
        ;; Collect per-repo per-day commit dates
        repo-days (into {}
                        (for [{:keys [label path]} all-repos
                              :when (.isDirectory (java.io.File. path))
                              :let [out (git path "log" "--since" since "--format=%aI")
                                    dates (when (and out (not (str/blank? out)))
                                            (->> (str/split-lines out)
                                                 (keep parse-iso-date)
                                                 distinct
                                                 set))]]
                          [label (or dates #{})]))
        ;; Compute Jaccard similarity between repo day-sets
        repos (keys repo-days)]
    (->> (for [a repos, b repos
               :when (pos? (compare (str b) (str a)))
               :let [da (get repo-days a)
                     db (get repo-days b)
                     inter (count (clojure.set/intersection da db))
                     union (count (clojure.set/union da db))
                     jaccard (if (pos? union) (/ (double inter) union) 0.0)]
               :when (> jaccard 0.1)]  ;; threshold: at least 10% co-change
           {:type :temporal-coupling
            :from a :to b
            :strength jaccard
            :co-change-days inter})
         (sort-by :strength >)
         vec)))

(defn- workstream-dependency-edges
  "Logic model edges between workstreams."
  []
  (when-let [model (read-edn-file (str futon5a-root "/data/stack-logic-model.edn"))]
    (vec
     (for [edge (:edges model)
           :when (:from edge)]
       {:type :logic-model-edge
        :id (:id edge)
        :from (:from edge)
        :to (:to edge)
        :edge-type (:type edge)
        :status (:status edge)
        :reinforcing? (:reinforcing edge)
        :constraint (:constraint edge)}))))

(defn- evidence-flow-edges
  "Count evidence entries per workstream (approximated by topic)."
  [entries]
  (let [ws-patterns {:mathematics #"(?i)prelim|proof|theorem|nlab|math"
                     :portfolio   #"(?i)portfolio|prospectus|working.paper|placemat|vsat"
                     :consulting  #"(?i)consult|ukrn|bristol|invoice|paid|eric"
                     :stack       #"(?i)repl|evidence|mission|futon|agency|pattern"}]
    (->> (for [[ws pat] ws-patterns]
           (let [cnt (count (filter (fn [e]
                                      (let [text (str (get-in e [:evidence/body :text] ""))]
                                        (and (not (str/blank? text))
                                             (re-find pat text))))
                                    entries))]
             {:type :evidence-flow
              :from ws
              :to :evidence-store
              :count cnt}))
         (filter #(pos? (:count %)))
         vec)))

(defn scan-graph
  "Build the strategic state graph.

   Nodes: repos (with commit activity), sorrys (with severity/status),
          workstreams (with JSDQ mode and pocketwatch targets).
   Edges: temporal coupling (Jaccard co-change), workstream dependencies
          (logic model), evidence flow (topic counts).
   Dynamics: pocketwatch ticks, sorry signals.

   This is the core data structure consumed by the visualiser.
   cf. cyberants world grid — here the 'world' is the stack itself."
  [days]
  (let [since (since-str days)
        entries (or (fetch-evidence 2000) [])
        window-entries (filter (fn [e]
                                 (when-let [d (parse-iso-date (:evidence/at e))]
                                   (>= (compare d since) 0)))
                               entries)
        ;; Nodes
        repos (repo-nodes days)
        sorrys (or (sorry-nodes) [])
        workstreams (or (workstream-nodes) [])
        missions (or (mission-nodes) [])
        ;; Edges
        coupling (temporal-coupling-edges days)
        dependencies (or (workstream-dependency-edges) [])
        evidence-flows (evidence-flow-edges window-entries)
        ;; Dynamics: pocketwatch ticks
        model (read-edn-file (str futon5a-root "/data/stack-logic-model.edn"))
        ticks (when model (get-in model [:pocketwatch :ticks]))
        ;; Compute actual workstream commit ratios for tick evaluation
        ws-commits (reduce (fn [acc {:keys [workstream commits]}]
                             (update acc workstream (fnil + 0) commits))
                           {}
                           repos)
        total-commits (max 1 (reduce + (vals ws-commits)))
        stack-pct (/ (double (:stack ws-commits 0)) total-commits)
        consulting-pct (/ (double (:consulting ws-commits 0)) total-commits)
        portfolio-pct (/ (double (:portfolio ws-commits 0)) total-commits)
        ;; Evaluate ticks
        tick-results (vec
                      (for [t (or ticks [])]
                        (let [fired? (case (:id t)
                                      :hermit-warning (and (> stack-pct 0.7) (< consulting-pct 0.05))
                                      :hobby-warning (and (pos? (:portfolio ws-commits 0))
                                                          (< portfolio-pct 0.05))
                                      :foraging-warning (> stack-pct 0.7)
                                      :cargo-warning (and (.exists (java.io.File.
                                                                     (str home "/vsat.wiki/ukrn-demo/UKRN_WP_draft_v2.md")))
                                                          (not (.exists (java.io.File.
                                                                         (str home "/vsat.wiki/prospectus.md")))))
                                      false)]
                          (assoc t :fired? fired?))))]
    {:nodes {:repos repos
             :sorrys sorrys
             :workstreams workstreams
             :missions missions}
     :edges {:temporal-coupling coupling
             :dependencies dependencies
             :evidence-flows evidence-flows}
     :dynamics {:ticks tick-results
                :commit-ratios ws-commits
                :commit-percentages {:stack stack-pct
                                     :consulting consulting-pct
                                     :portfolio portfolio-pct
                                     :mathematics (/ (double (:mathematics ws-commits 0)) total-commits)}}
     :summary {:total-repos (count repos)
               :active-repos (count (filter :active? repos))
               :total-sorrys (count sorrys)
               :total-workstreams (count workstreams)
               :coupling-edges (count coupling)
               :ticks-firing (count (filter :fired? tick-results))}}))

;; ---------------------------------------------------------------------------
;; Renderer: markdown fallback
;;
;; The primary renderer is the Swing visualiser (war_machine_visual.clj).
;; This markdown renderer is the fallback for M-x war-machine in Emacs.
;; ---------------------------------------------------------------------------

(defn- pct-str [n] (format "%.0f%%" (* 100.0 (double n))))

(defn- render-table
  "Render a justified markdown table."
  [headers aligns rows]
  (let [ncols (count headers)
        widths (mapv (fn [i]
                       (apply max (count (nth headers i))
                              (map #(count (nth % i "")) rows)))
                     (range ncols))
        pad (fn [s w align]
              (let [s (or s "")
                    gap (- w (count s))]
                (case align
                  :right (str (apply str (repeat gap \space)) s)
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
                                              :right (str (apply str (repeat (inc w) \-)) ":")
                                              (str (apply str (repeat (+ w 2) \-))))))
                                        aligns))
                 "|\n")]
    (str (fmt-row headers) sep (apply str (map fmt-row rows)))))

(defn- health-indicator [h]
  (cond (>= h 0.7) "●"  ;; healthy
        (>= h 0.3) "◐"  ;; partial
        :else       "○")) ;; absent

(defn render-war-machine
  "Render the War Machine strategic synthesis as markdown."
  [{:keys [loop-health support-attack mission-triage graph portfolio now days] :as data}]
  (let [sb (StringBuilder.)]
    (.append sb "# War Machine — Strategic Synthesis\n\n")
    (.append sb (str "**" now "** | " days "-day window\n\n"))

    ;; --- Loop Health ---
    (.append sb "## Loop Health\n\n")
    (when loop-health
      (let [{:keys [arrows overall loop-complete?]} loop-health]
        (.append sb (str "Overall: " (pct-str overall)
                         (if loop-complete? " — **loop complete**" " — gaps detected")
                         "\n\n"))
        (.append sb (render-table
                     ["Arrow" "Health" "Count" "Last seen" ""]
                     [:left :right :right :left :left]
                     (mapv (fn [{:keys [label health count last-seen]}]
                             [label
                              (pct-str health)
                              (str count)
                              (or last-seen "never")
                              (health-indicator health)])
                           arrows)))
        (.append sb "\n")))

    ;; --- Support / Attack ---
    (.append sb "## Holistic Argument\n\n")
    (when support-attack
      (let [{:keys [claims support-coverage attack-coverage]} support-attack]
        (.append sb (str "Support coverage: " (pct-str support-coverage)
                         " | Attack coverage: " (pct-str attack-coverage) "\n\n"))
        (.append sb (render-table
                     ["Claim" "Type" "Evidence" "Last" "Status"]
                     [:left :left :right :left :left]
                     (mapv (fn [{:keys [claim-id type label evidence-count last-evidence status]}]
                             [(str (name claim-id) ": " label)
                              (name type)
                              (str evidence-count)
                              (or last-evidence "-")
                              (name status)])
                           claims)))
        (.append sb "\n")))

    ;; --- Mission Triage ---
    (.append sb "## Mission Triage\n\n")
    (when mission-triage
      (let [{:keys [total active completed blocked abandoned-count
                    abandoned-missions by-repo health]} mission-triage]
        (.append sb (render-table
                     ["Metric" "Value"]
                     [:left :right]
                     [["Total missions" (str total)]
                      ["Active" (str active)]
                      ["Completed" (str completed)]
                      ["Blocked" (str blocked)]
                      ["Abandoned (no commits in window)" (str abandoned-count)]
                      ["Health" (pct-str health)]]))
        (.append sb "\n")
        ;; By repo
        (when (seq by-repo)
          (.append sb "**By repo:**\n\n")
          (doseq [[repo cnt] (sort-by val > by-repo)]
            (.append sb (str "- " repo ": " cnt "\n")))
          (.append sb "\n"))
        ;; Abandoned missions
        (when (seq abandoned-missions)
          (.append sb "**Abandoned missions (in-progress, no recent commits):**\n\n")
          (doseq [m (take 10 abandoned-missions)]
            (.append sb (str "- " (:mission/id m) " (" (:mission/repo m) ")\n")))
          (.append sb "\n"))))

    ;; --- Portfolio Inference ---
    (.append sb "## Portfolio Inference\n\n")
    (when-let [pf (:portfolio data)]
      (if (:available? pf)
        (do
          (when-let [s (:state pf)]
            (.append sb (render-table
                         ["Metric" "Value"]
                         [:left :left]
                         [["Mode" (str (:mode s))]
                          ["Urgency" (format "%.2f" (double (or (:urgency s) 0)))]
                          ["Temperature (τ)" (format "%.2f" (double (or (:tau s) 0)))]
                          ["Step count" (str (:step-count s))]
                          ["Focus" (str (or (:focus s) "(none)"))]]))
            (.append sb "\n")
            (when-let [channels (:channels s)]
              (.append sb "**Sensory channels (μ predictions):**\n\n")
              (doseq [[ch val] (sort-by key channels)]
                (.append sb (str "- " (name ch) ": " (format "%.2f" (double val)) "\n")))
              (.append sb "\n")))
          (when-let [r (:recommendation pf)]
            (.append sb (str "**Recommendation:** " (:action r) "\n"))
            (.append sb (str "Free energy: " (when (:free-energy r)
                                               (format "%.4f" (double (:free-energy r))))
                             " | τ: " (when (:tau r) (format "%.2f" (double (:tau r))))
                             "\n\n"))))
        (.append sb "Portfolio inference not available.\n\n")))

    ;; --- Graph Summary ---
    (.append sb "## Strategic Graph\n\n")
    (when graph
      (let [{:keys [summary dynamics]} graph
            {:keys [total-repos active-repos total-sorrys coupling-edges ticks-firing]} summary
            {:keys [commit-percentages ticks]} dynamics]
        (.append sb (render-table
                     ["Metric" "Value"]
                     [:left :right]
                     [["Active repos" (str active-repos "/" total-repos)]
                      ["Open sorrys" (str total-sorrys)]
                      ["Temporal coupling edges" (str coupling-edges)]
                      ["Ticks firing" (str ticks-firing)]]))
        (.append sb "\n")
        ;; Workstream balance
        (.append sb "**Workstream balance (commit %):**\n\n")
        (doseq [[ws pct] (sort-by val > commit-percentages)]
          (.append sb (str "- " (name ws) ": " (pct-str pct) "\n")))
        (.append sb "\n")
        ;; Firing ticks
        (when (seq (filter :fired? ticks))
          (.append sb "**Ticks firing:**\n\n")
          (doseq [t (filter :fired? ticks)]
            (.append sb (str "- **" (name (:id t)) ":** " (:fires t) "\n")))
          (.append sb "\n"))
        ;; Temporal coupling
        (when-let [coupling (seq (get-in graph [:edges :temporal-coupling]))]
          (.append sb "**Strongest temporal coupling:**\n\n")
          (.append sb (render-table
                       ["Repo A" "Repo B" "Jaccard" "Co-change days"]
                       [:left :left :right :right]
                       (mapv (fn [{:keys [from to strength co-change-days]}]
                               [from to (format "%.2f" strength) (str co-change-days)])
                             (take 10 coupling))))
          (.append sb "\n"))))

    (str sb)))

;; ---------------------------------------------------------------------------
;; Orchestrator
;; ---------------------------------------------------------------------------

(defn generate-war-machine
  "Collect all strategic scans and render.
   Returns {:data ... :markdown ...}."
  [days]
  (let [now (.toString (.toLocalDate (.atZone (Instant/now) tz)))
        loop-health (scan-loop-health days)
        support-attack (scan-support-attack days)
        mission-triage (scan-mission-triage days)
        graph (scan-graph days)
        sessions (scan-sessions)
        portfolio (scan-portfolio)
        mission-detail (scan-mission-detail)]
    {:data {:loop-health loop-health
            :support-attack support-attack
            :mission-triage mission-triage
            :graph graph
            :sessions sessions
            :portfolio portfolio
            :mission-detail mission-detail}
     :markdown (render-war-machine {:loop-health loop-health
                                    :support-attack support-attack
                                    :mission-triage mission-triage
                                    :graph graph
                                    :portfolio portfolio
                                    :now now :days days})}))

;; ---------------------------------------------------------------------------
;; Normalized observation vector (AIF terminal vocabulary)
;;
;; Converts the raw scan data into a normalized [0,1] observation vector
;; following the core-terminal-vocabulary.md schema.
;;
;; cf. cyberants observe.clj/sense->vector — same pattern, strategic domain
;; ---------------------------------------------------------------------------

(def ^:private observation-channels
  "The war machine's observation channels, harmonized from all vocabularies.
   Each channel is a named terminal with a source vocabulary and normalization."
  [:loop-health           ;; overall loop health [0,1] — from holistic argument
   :support-coverage      ;; S1-S5 evidence coverage [0,1] — from holistic argument
   :attack-coverage       ;; A1-A4 evidence coverage [0,1] — from holistic argument
   :mission-health        ;; mission triage health [0,1] — from peripheral-aif
   :stack-pct             ;; stack commit % [0,1] — from logic model / joe-hud
   :consulting-pct        ;; consulting commit % [0,1] — from JSDQ
   :portfolio-pct         ;; portfolio commit % [0,1] — from JSDQ
   :mathematics-pct       ;; mathematics commit % [0,1] — from JSDQ
   :active-repo-ratio     ;; active repos / total repos [0,1] — from logic model
   :sorry-count-norm      ;; open sorrys / 10 (capped at 1) — from sorry topology
   :coupling-density      ;; coupling edges / max edges [0,1] — from temporal analysis
   :ticks-firing-ratio    ;; firing ticks / total ticks [0,1] — from logic model
   ])

(defn observe
  "Produce normalized observation vector from raw scan data.
   Returns a map of channel-id → [0,1] value.

   This is the war machine's g-observe: the bridge between
   raw scan data and the AIF loop."
  [data]
  (let [{:keys [loop-health support-attack mission-triage graph]} data
        {:keys [commit-percentages ticks]} (:dynamics graph {})
        {:keys [summary]} graph]
    {:loop-health (:overall loop-health 0.0)
     :support-coverage (:support-coverage support-attack 0.0)
     :attack-coverage (:attack-coverage support-attack 0.0)
     :mission-health (:health mission-triage 0.0)
     :stack-pct (:stack commit-percentages 0.0)
     :consulting-pct (:consulting commit-percentages 0.0)
     :portfolio-pct (:portfolio commit-percentages 0.0)
     :mathematics-pct (:mathematics commit-percentages 0.0)
     :active-repo-ratio (if (and summary (pos? (:total-repos summary)))
                          (/ (double (:active-repos summary 0))
                             (:total-repos summary))
                          0.0)
     :sorry-count-norm (min 1.0 (/ (double (:total-sorrys summary 0)) 10.0))
     :coupling-density (let [n (:total-repos summary 0)
                              max-edges (/ (* n (dec n)) 2)]
                          (if (pos? max-edges)
                            (min 1.0 (/ (double (:coupling-edges summary 0)) max-edges))
                            0.0))
     :ticks-firing-ratio (let [total (count (or ticks []))
                                firing (:ticks-firing summary 0)]
                            (if (pos? total)
                              (/ (double firing) total)
                              0.0))}))

(defn sense->vector
  "Convert observation map to ordered vector (for ML/AIF consumption).
   cf. cyberants observe.clj/sense->vector."
  [obs]
  (mapv #(get obs % 0.0) observation-channels))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [days (if (seq args) (Integer/parseInt (first args)) 14)
        {:keys [markdown data]} (generate-war-machine days)]
    (println markdown)
    ;; Print observation vector summary
    (println "\n## Observation Vector\n")
    (let [obs (observe data)]
      (doseq [[k v] (sort-by key obs)]
        (println (str "  " (name k) ": " (format "%.2f" (double v))))))))
