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

(import '(java.time Instant Duration))

(def home (System/getProperty "user.home"))
(def claude-project-dir (str home "/.claude/projects/-home-joe-code"))
(def codex-sessions-root
  (str (or (System/getenv "CODEX_HOME")
           (str home "/.codex"))
       "/sessions"))
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

(defn rate-limit-summary [limit]
  (when (map? limit)
    {:used-pct (:used_percent limit)
     :window-min (:window_minutes limit)
     :resets-at (when-let [epoch (:resets_at limit)]
                  (.toString (Instant/ofEpochSecond (long epoch))))}))

(defn rate-limit-for-window [rate-limits window-min]
  (->> [(:primary rate-limits)
        (:secondary rate-limits)
        (:individual_limit rate-limits)]
       (filter map?)
       (filter #(= window-min (:window_minutes %)))
       first
       rate-limit-summary))

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
      (let [rate-limits (get-in latest [:payload :rate_limits])
            five-hour (rate-limit-for-window rate-limits 300)
            weekly (rate-limit-for-window rate-limits 10080)
            ;; Compatibility for consumers predating the explicit two-window
            ;; shape. Prefer 5h, but never relabel a weekly-only limit as 5h.
            legacy-limit (or five-hour weekly)
            ttu (get-in latest [:payload :info :total_token_usage])]
        {:source :codex
         :session-id sid
         :file (str path)
         :first-at (some-> tcs first :timestamp)
         :last-at  (:timestamp latest)
         :rate-limit legacy-limit
         :rate-limits {:five-hour five-hour
                       :weekly weekly}
         :usage (when ttu
                  {:input  (:input_tokens ttu)
                   :cached (:cached_input_tokens ttu)
                   :output (:output_tokens ttu)
                   :reasoning (:reasoning_output_tokens ttu)
                   :total (:total_tokens ttu)})}))))

(defn codex-active-sessions [since]
  ;; A resumed session remains in the directory for its original start date,
  ;; so calendar-based pruning loses live sessions. Enumerate the tree and use
  ;; the transcript mtime/event timestamps as the activity authority.
  (let [files (when (fs/exists? codex-sessions-root)
                (->> (file-seq (io/file codex-sessions-root))
                     (filter #(.isFile ^java.io.File %))
                     (filter #(str/ends-with? (str %) ".jsonl"))))
        recent (filter (fn [p]
                         (.isAfter (.toInstant (fs/last-modified-time p)) since))
                       files)]
    (keep #(codex-session-summary % since) recent)))

;; --- Render --------------------------------------------------------------

(defn short-id [s] (subs (str s) 0 (min 8 (count (str s)))))

(defn fmt-row [r]
  (let [sid (short-id (:session-id r))
        u (:usage r)
        rl (:rate-limit r)]
    (case (:source r)
      :claude (format "  claude  %s  msgs=%d  in=%d  cw=%d  cr=%d  out=%d  weighted=%d  (%s)"
                      sid (:messages u) (:input u) (:cache-create u) (:cache-read u)
                      (:output u) (:weighted-input-equiv u)
                      (or (:last-at r) "?"))
      :codex  (let [window-label (case (:window-min rl)
                                   300 "5h"
                                   10080 "wk"
                                   (str (or (:window-min rl) "?") "m"))]
                (format "  codex   %s  used=%.1f%% %s  resets=%s  total=%s  (%s)"
                        sid
                        (double (or (:used-pct rl) 0))
                        window-label
                        (or (:resets-at rl) "?")
                        (str (or (:total u) "?"))
                        (or (:last-at r) "?"))))))

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
   (let [five-used (->> codex (keep #(get-in % [:rate-limits :five-hour :used-pct])) seq)
         week-used (->> codex (keep #(get-in % [:rate-limits :weekly :used-pct])) seq)
         five-reset (some #(get-in % [:rate-limits :five-hour :resets-at]) codex)
         week-reset (some #(get-in % [:rate-limits :weekly :resets-at]) codex)
         legacy-used (or five-used week-used)]
     {:active-sessions (count codex)
      :five-hour-used-pct-max (when five-used (apply max five-used))
      :weekly-used-pct-max (when week-used (apply max week-used))
      :five-hour-resets-at five-reset
      :weekly-resets-at week-reset
      :used-pct-max (when legacy-used (apply max legacy-used))
      :resets-at (or five-reset week-reset)
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

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
