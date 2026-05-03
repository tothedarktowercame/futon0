#!/usr/bin/env bb
;; current-usage-report.bb — per-session token burndown for Claude + Codex.
;;
;; Reads ~/.claude/projects/-home-joe-code/*.jsonl and ~/.codex/sessions/...
;; Prints one row per session active in the rolling window. With --push, also
;; appends each row to the Arxana Browser → Sessions section via futon3c's
;; mission-control + evidence endpoints.
;;
;; No agent invocation. No JVM execution. Pure local file parse + HTTP POST.
;;
;; Companion algorithm: ~/code/algorithms/current-usage-report.md

(require '[cheshire.core :as json]
         '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[babashka.fs :as fs]
         '[babashka.http-client :as http])

(import '(java.time Instant Duration ZoneOffset)
        '(java.time.format DateTimeFormatter))

(def home (System/getProperty "user.home"))
(def claude-project-dir (str home "/.claude/projects/-home-joe-code"))
(def codex-sessions-root (str home "/.codex/sessions"))
(def mc-base "http://localhost:7070")

(defn usage []
  (println "Usage: current-usage-report.bb [--window MIN] [--json] [--push] [--verbose]")
  (println "")
  (println "  --window MIN   Rolling window in minutes (default 300 = 5h Codex window).")
  (println "  --json         Emit a single JSON object on stdout (for elisp/HUD consumers).")
  (println "  --push         POST per-session reports to Arxana via futon3c HTTP.")
  (println "  --verbose      Include all fields in stdout output.")
  (println "  --help         Show this message."))

(defn parse-args [args]
  (loop [opts {:window 300} remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--window"  (recur (assoc opts :window (Long/parseLong (second remaining))) (nnext remaining))
        "--json"    (recur (assoc opts :json true) (rest remaining))
        "--push"    (recur (assoc opts :push true) (rest remaining))
        "--verbose" (recur (assoc opts :verbose true) (rest remaining))
        "--help"    (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn iso-now [] (.toString (Instant/now)))

(defn since-instant [window-min]
  (.minus (Instant/now) (Duration/ofMinutes window-min)))

(defn parse-iso [s]
  (when (and (string? s) (seq s)) (try (Instant/parse s) (catch Exception _ nil))))

(defn after? [t cutoff] (and t (.isAfter t cutoff)))

(defn read-jsonl-lines [f]
  (with-open [r (io/reader (str f))]
    (doall (keep (fn [line]
                   (when (seq (str/trim line))
                     (try (json/parse-string line true) (catch Exception _ nil))))
                 (line-seq r)))))

;; --- Claude --------------------------------------------------------------

(defn claude-session-id-from-name [path]
  (let [n (fs/file-name path)] (str/replace n #"\.jsonl$" "")))

(defn claude-session-summary
  "Sum usage blocks for assistant messages whose timestamp is after `since`.
   Returns nil if no in-window assistant messages were seen."
  [path since]
  (let [lines    (read-jsonl-lines path)
        in-win   (filter (fn [m]
                           (and (= "assistant" (:type m))
                                (after? (parse-iso (:timestamp m)) since)
                                (some? (get-in m [:message :usage]))))
                         lines)
        first-ts (some-> (first in-win) :timestamp parse-iso)
        last-ts  (some-> (last in-win)  :timestamp parse-iso)
        sums (reduce (fn [acc m]
                       (let [u (get-in m [:message :usage])]
                         (-> acc
                             (update :input         + (or (:input_tokens u) 0))
                             (update :cache-create  + (or (:cache_creation_input_tokens u) 0))
                             (update :cache-read    + (or (:cache_read_input_tokens u) 0))
                             (update :output        + (or (:output_tokens u) 0))
                             (update :messages inc))))
                     {:input 0 :cache-create 0 :cache-read 0 :output 0 :messages 0}
                     in-win)
        weighted (long (+ (* (:cache-create sums) 1.25)
                          (* (:cache-read sums) 0.1)
                          (:input sums)))]
    (when (pos? (:messages sums))
      {:source :claude
       :session-id (claude-session-id-from-name path)
       :file (str path)
       :first-at (some-> first-ts .toString)
       :last-at  (some-> last-ts  .toString)
       :usage (assoc sums :weighted-input-equiv weighted)})))

(defn claude-active-sessions [since]
  (let [files (when (fs/exists? claude-project-dir)
                (filter #(str/ends-with? (str %) ".jsonl")
                        (fs/list-dir claude-project-dir)))
        recent (filter (fn [p]
                         (let [m (fs/last-modified-time p)]
                           (.isAfter (.toInstant m) since)))
                       files)]
    (keep #(claude-session-summary % since) recent)))

;; --- Codex ---------------------------------------------------------------

(defn codex-session-summary
  "Extract the latest token_count event whose timestamp is in window.
   Returns nil if none seen."
  [path since]
  (let [lines (read-jsonl-lines path)
        meta  (first (filter #(= "session_meta" (:type %)) lines))
        sid   (or (get-in meta [:payload :id])
                  (-> path fs/file-name (str/replace #"\.jsonl$" "")))
        tcs   (filter (fn [m]
                        (and (= "event_msg" (:type m))
                             (= "token_count" (get-in m [:payload :type]))
                             (after? (parse-iso (:timestamp m)) since)))
                      lines)
        latest (last tcs)]
    (when latest
      (let [rl (get-in latest [:payload :rate_limits :primary])
            ttu (get-in latest [:payload :info :total_token_usage])
            resets-at (when-let [e (:resets_at rl)]
                        (.toString (Instant/ofEpochSecond (long e))))]
        {:source :codex
         :session-id sid
         :file (str path)
         :first-at (some-> tcs first :timestamp)
         :last-at  (:timestamp latest)
         :rate-limit {:used-pct (:used_percent rl)
                      :window-min (:window_minutes rl)
                      :resets-at resets-at}
         :usage (when ttu
                  {:input  (:input_tokens ttu)
                   :cached (:cached_input_tokens ttu)
                   :output (:output_tokens ttu)
                   :reasoning (:reasoning_output_tokens ttu)
                   :total (:total_tokens ttu)})}))))

(defn codex-active-sessions [since]
  (let [today (.format (java.time.LocalDate/now) (DateTimeFormatter/ofPattern "yyyy/MM/dd"))
        yday  (.format (.minusDays (java.time.LocalDate/now) 1)
                       (DateTimeFormatter/ofPattern "yyyy/MM/dd"))
        dirs  (filter fs/exists? [(str codex-sessions-root "/" today)
                                  (str codex-sessions-root "/" yday)])
        files (mapcat #(filter (fn [p] (str/ends-with? (str p) ".jsonl"))
                               (fs/list-dir %))
                      dirs)
        recent (filter (fn [p]
                         (.isAfter (.toInstant (fs/last-modified-time p)) since))
                       files)]
    (keep #(codex-session-summary % since) recent)))

;; --- Render --------------------------------------------------------------

(defn short-id [s] (subs (str s) 0 (min 8 (count (str s)))))

(defn fmt-row [r]
  (let [src (name (:source r))
        sid (short-id (:session-id r))
        u (:usage r)
        rl (:rate-limit r)]
    (case (:source r)
      :claude (format "  claude  %s  msgs=%d  in=%d  cw=%d  cr=%d  out=%d  weighted=%d  (%s)"
                      sid (:messages u) (:input u) (:cache-create u) (:cache-read u)
                      (:output u) (:weighted-input-equiv u)
                      (or (:last-at r) "?"))
      :codex  (format "  codex   %s  used=%.1f%%  resets=%s  total=%s  (%s)"
                      sid
                      (double (or (:used-pct rl) 0))
                      (or (:resets-at rl) "?")
                      (str (or (:total u) "?"))
                      (or (:last-at r) "?")))))

(defn render-report [{:keys [window-min since claude codex]}]
  (str/join
   "\n"
   (concat
    [(format "=== Current Usage Report — %s ===" (iso-now))
     (format "Window: last %d min  (since %s)" window-min (.toString since))
     ""
     (format "Codex sessions in window (%d):" (count codex))]
    (if (seq codex) (map fmt-row codex) ["  (none)"])
    [""
     (format "Claude sessions in window (%d):" (count claude))]
    (if (seq claude) (map fmt-row claude) ["  (none)"]))))

;; --- Push to Arxana ------------------------------------------------------

(defn http-post-json [path body]
  (try
    (let [resp (http/post (str mc-base path)
                          {:headers {"Content-Type" "application/json"}
                           :body (json/generate-string body)
                           :throw false})]
      {:status (:status resp)
       :body   (try (json/parse-string (:body resp) true)
                    (catch Exception _ (:body resp)))})
    (catch Exception e
      {:status :error :error (.getMessage e)})))

(defn mc-start! [session-id author]
  (http-post-json "/api/alpha/mission-control"
                  {:action "start" :session-id session-id :author author}))

(defn evidence-create! [entry]
  (http-post-json "/api/alpha/evidence" entry))

(defn record-id [r]
  (str "usage:" (name (:source r)) ":" (short-id (:session-id r))))

(defn push-record! [r]
  (let [sid (record-id r)
        start-resp (mc-start! sid "usage-monitor")
        ev-entry {:author "usage-monitor"
                  :type "observation"
                  :claim-type "usage-snapshot"
                  :subject-type "session"
                  :subject-id (:session-id r)
                  :session-id sid
                  :tags ["usage" "burndown" (name (:source r))]
                  :body (assoc r :recorded-at (iso-now))}
        ev-resp (evidence-create! ev-entry)]
    {:session-id sid
     :start-status (:status start-resp)
     :evidence-status (:status ev-resp)
     :evidence-id (get-in ev-resp [:body :evidence/id])}))

;; --- Main ----------------------------------------------------------------

(defn aggregate
  "Collapse per-session records into top-line totals.
   Codex used-pct: max across active sessions (single shared window).
   Claude: sums."
  [{:keys [claude codex]}]
  {:codex
   (let [used (->> codex (keep #(get-in % [:rate-limit :used-pct])) seq)]
     {:active-sessions (count codex)
      :used-pct-max (when used (apply max used))
      :resets-at (some-> codex first :rate-limit :resets-at)
      :total-tokens (->> codex (keep #(get-in % [:usage :total])) (apply + 0))})
   :claude
   (let [us (map :usage claude)]
     {:active-sessions (count claude)
      :messages (apply + 0 (keep :messages us))
      :input    (apply + 0 (keep :input us))
      :cache-create (apply + 0 (keep :cache-create us))
      :cache-read   (apply + 0 (keep :cache-read us))
      :output       (apply + 0 (keep :output us))
      :weighted-input-equiv (apply + 0 (keep :weighted-input-equiv us))})})

(defn -main [& args]
  (let [opts (parse-args args)]
    (cond
      (:help opts) (do (usage) (System/exit 0))
      (:unknown opts) (do (println "Unknown arg:" (:unknown opts)) (usage) (System/exit 2))
      :else
      (let [window-min (:window opts)
            since (since-instant window-min)
            claude (claude-active-sessions since)
            codex  (codex-active-sessions  since)
            report {:window-min window-min
                    :at (iso-now)
                    :since (.toString since)
                    :claude claude
                    :codex codex}
            report (assoc report :totals (aggregate report))]
        (if (:json opts)
          (println (json/generate-string report))
          (println (render-report (update report :since #(Instant/parse %)))))
        (when (:push opts)
          (println "")
          (println "=== Pushing to Arxana → Sessions ===")
          (doseq [r (concat codex claude)]
            (let [res (push-record! r)]
              (println (format "  %s  start=%s  evidence=%s  id=%s"
                               (:session-id res)
                               (:start-status res)
                               (:evidence-status res)
                               (or (:evidence-id res) "-"))))))))))

(apply -main *command-line-args*)
