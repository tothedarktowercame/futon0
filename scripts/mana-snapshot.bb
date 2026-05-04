#!/usr/bin/env bb
;; mana-snapshot — produce a small JSON snapshot of the
;; metabolic-balance/working-tree drain across the futon stack.
;;
;; Usage:
;;   bb scripts/mana-snapshot.bb [--out PATH]
;;
;; Default output: /home/joe/code/storage/futon0/mana-snapshot.json
;; (per the "data out of source repo" discipline; the snapshot is
;; regenerable, so it lives at storage, not in tracked tree.)
;;
;; The snapshot mirrors the JVM check-fn's formula
;; (futon3c.logic.metabolic-balance/compute-channel-pressure):
;;   P = max(count_eligible/N, age_eligible_max/D, bytes_eligible/B)
;; with N=20 paths, D=7 days, B=10 MB.
;;
;; Eligibility filtering and per-channel modulation (Process C) are
;; NOT applied here — this is a lightweight HUD-feed approximation.
;; The authoritative reading lives in the JVM check-fn (probe-tap +
;; on-load!). When they disagree, trust the JVM.

(require '[babashka.fs :as fs]
         '[babashka.process :as proc]
         '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def manifest-path
  (str (fs/path (fs/parent (fs/parent (fs/real-path *file*)))
                "data" "git_sources.json")))

(def default-out
  "/home/joe/code/storage/futon0/mana-snapshot.json")

(def nominals
  {:N-count 20
   :D-age-days 7.0
   :B-bytes (* 10 1024 1024)})

(defn load-repos []
  (let [base (fs/parent manifest-path)
        data (json/parse-string (slurp manifest-path) true)]
    (->> (:repos data)
         (map (fn [r]
                (let [abs (str (fs/normalize (fs/path base (:path r))))]
                  (assoc r :abs-path abs))))
         (filter #(fs/exists? (str (fs/path (:abs-path %) ".git"))))
         vec)))

(defn run-git [repo & args]
  (try
    (let [{:keys [exit out]} (apply proc/shell
                                    {:dir repo :out :string :err :string :continue true}
                                    "git" args)]
      (if (zero? exit) out ""))
    (catch Throwable _ "")))

(defn now-ms [] (System/currentTimeMillis))

(defn file-age-days [^java.io.File f]
  (if (and f (.exists f))
    (let [t (.lastModified f)]
      (if (zero? t) 0.0
          (double (/ (- (now-ms) t) (* 1000.0 60 60 24)))))
    0.0))

(defn file-bytes [^java.io.File f]
  (if (and f (.exists f)) (.length f) 0))

(defn list-uncommitted-paths [repo]
  (->> (run-git repo "status" "--porcelain")
       str/split-lines
       (remove str/blank?)
       (mapv (fn [line]
               (let [rel (str/triml (subs line (min 3 (count line))))
                     f (java.io.File. (str (fs/path repo rel)))]
                 {:path rel
                  :age-days (file-age-days f)
                  :bytes (file-bytes f)})))))

(defn pressure->tier [p]
  (cond
    (< p 1.0) "silent"
    (< p 2.0) "advisory"
    (< p 4.0) "high"
    :else "stop-the-line"))

(defn compute-pressure [paths {:keys [N-count D-age-days B-bytes]}]
  (if (empty? paths)
    {:P 0.0 :count 0 :max-age-days 0.0 :total-bytes 0 :tier "silent"}
    (let [n (count paths)
          max-age (apply max 0.0 (map #(or (:age-days %) 0.0) paths))
          total-bytes (reduce + 0 (map #(or (:bytes %) 0) paths))
          P (max (/ n (double N-count))
                 (/ max-age (double D-age-days))
                 (/ total-bytes (double B-bytes)))]
      {:P P
       :count n
       :max-age-days max-age
       :total-bytes total-bytes
       :tier (pressure->tier P)})))

(def futon3c-base (or (System/getenv "FUTON3C_BASE_URL") "http://localhost:7070"))
(def nonstarter-base (or (System/getenv "NONSTARTER_BASE_URL") "http://localhost:7072"))
(def http-timeout-ms 1500)

(defn fetch-json
  "GET URL with the standard short timeout. Returns parsed JSON or nil
   on any failure — never throws. Both futon3c and nonstarter must be
   optional dependencies."
  [url]
  (try
    (let [resp (http/get url {:throw false :timeout http-timeout-ms})]
      (when (= 200 (:status resp))
        (try (json/parse-string (:body resp) true) (catch Throwable _ nil))))
    (catch Throwable _ nil)))

(defn fetch-agents
  "GET /api/alpha/agents → list of agent maps. Returns [] when futon3c
   is unreachable or the endpoint fails (per amendment-III ergonomic:
   the snapshot must work even when nonstarter / agency aren't up)."
  []
  (let [resp (fetch-json (str futon3c-base "/api/alpha/agents"))]
    (cond
      (nil? resp) []
      ;; Two known shapes: {:agents {...}} (current) or {:agents [...]}.
      (map? (:agents resp))
      (mapv (fn [[id m]] (assoc m :agent-id (or (get m :id-key) (str id))))
            (:agents resp))
      (sequential? (:agents resp)) (vec (:agents resp))
      :else [])))

(defn fetch-session-mana
  "GET /api/mana?session-id=... on nonstarter. Returns nil on failure."
  [session-id]
  (when (and session-id (not (str/blank? (str session-id))))
    (fetch-json (str nonstarter-base "/api/mana?session-id=" session-id))))

(defn fetch-pool
  "GET /api/pool on nonstarter. Returns nil on failure."
  []
  (fetch-json (str nonstarter-base "/api/pool")))

(defn ^:private session-record
  "Build the per-session snapshot record for one agent.

  Per V-6 amendment (II): drain measures per-repo; balance aggregates
  per-session. Per Joe's D-01 reframe: each session is its own AIF
  head; the War Machine integrates these heads.

  Schema (first-pass; richer AIF-head fields land when M-aif-head's
  apparatus extends to sessions):
    {:agent-id <string>
     :session-id <uuid>
     :status <string>          ; agent's current status (idle / busy / ...)
     :last-active <iso>         ; agent.last-active
     :balance <number-or-nil>   ; from /api/mana?session-id=...
     :earned <number-or-nil>
     :spent  <number-or-nil>
     :aif-head/phase <kw>       ; for now, derived from status
                                ; (idle → :rest; busy → :active)
     :aif-head/source :session  ; tag so War Machine knows the
                                ; head is session-shaped (vs
                                ; mission-shaped, peripheral-shaped, etc.)}"
  [agent]
  (let [session-id (or (:session-id agent)
                       (get-in agent [:id :id/value])
                       (:agent-id agent))
        mana (fetch-session-mana session-id)
        status (:status agent)]
    {:agent-id (or (:agent-id agent) (get-in agent [:id :id/value]))
     :session-id session-id
     :status status
     :last-active (:last-active agent)
     :balance (:balance mana)
     :earned (:earned mana)
     :spent (:spent mana)
     :aif-head/phase (case status
                       "busy" :active
                       "idle" :rest
                       :unknown)
     :aif-head/source :session}))

(defn snapshot []
  (let [repos (load-repos)
        per-repo (vec
                  (for [{:keys [name abs-path]} repos]
                    (let [paths (list-uncommitted-paths abs-path)
                          p (compute-pressure paths nominals)]
                      (assoc p :repo name :abs-path abs-path))))
        max-tier (->> per-repo (map :tier)
                      (reduce (fn [acc t]
                                (let [rank {"silent" 0 "advisory" 1
                                            "high" 2 "stop-the-line" 3}]
                                  (if (> (get rank t 0) (get rank acc 0)) t acc)))
                              "silent"))
        max-pressure (apply max 0.0 (map :P per-repo))
        agents (fetch-agents)
        sessions (mapv session-record agents)
        pool (fetch-pool)]
    {:generated-at (str (java.time.Instant/now))
     :nominals nominals
     :max-tier max-tier
     :max-pressure max-pressure
     :per-repo per-repo
     ;; Per D-01: each session is its own AIF head; War Machine consumes.
     :sessions sessions
     :pool pool}))

(defn parse-args [args]
  (loop [opts {:out default-out} remaining args]
    (case (first remaining)
      "--out" (recur (assoc opts :out (second remaining)) (drop 2 remaining))
      nil opts
      (recur opts (next remaining)))))

(defn -main [& args]
  (let [{:keys [out]} (parse-args args)
        snap (snapshot)
        out-file (java.io.File. out)]
    (fs/create-dirs (fs/parent out))
    (spit out (json/generate-string snap))
    (println (format "wrote %s — max-tier=%s P=%.2f across %d repos"
                     out (:max-tier snap) (:max-pressure snap)
                     (count (:per-repo snap))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
