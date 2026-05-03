(ns futon0.report.pattern-density
  "Pattern-retrieval density over time — first INSTANTIATE demo for
   M-war-machine's visualizer slot.

   Reads context-retrieval evidence from futon3c's evidence log
   (each such event carries a `results` vector of retrieved pattern
   ids + scores) and renders a markdown block with:

     - Overall retrieval density sparkline (per day)
     - Top-N patterns × daily counts, each with a sparkline
     - Summary stats

   Uses only Unicode sparkline chars — no HTML, no browser, renders
   inside the *War Machine* Emacs buffer or any markdown viewer.

   Standalone for now: `bb -m futon0.report.pattern-density [days] [top-n]`.
   Defaults: 14 days, top 15 patterns. Promote into war_machine.clj
   once the shape is confirmed useful.

   Data source: GET /api/alpha/evidence?limit=N on futon3c-url.
   Env: FUTON3C_PORT (default 7070), FUTON3C_SERVER, FUTON3C_EVIDENCE_BASE."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import (java.time LocalDate ZoneId)))

(def ^:private tz (ZoneId/of "Europe/London"))

(def ^:private futon3c-url
  (or (System/getenv "FUTON3C_EVIDENCE_BASE")
      (System/getenv "FUTON3C_SERVER")
      (str "http://localhost:" (or (System/getenv "FUTON3C_PORT") "7070"))))

(def ^:private spark-chars "▁▂▃▄▅▆▇█")

;; ---------- HTTP ----------

(defn- fetch-evidence [limit]
  (let [url (str futon3c-url "/api/alpha/evidence?limit=" limit)
        resp (http/get url {:headers {"accept" "application/json"}
                            :throw false
                            :timeout 10000})]
    (when (= 200 (:status resp))
      (let [parsed (json/parse-string (:body resp) true)]
        (when (:ok parsed)
          (or (:entries parsed) []))))))

;; ---------- Aggregation ----------

(defn- entry->day [entry]
  (some-> (:evidence/at entry) (subs 0 10)))

(defn- context-retrieval?
  [entry]
  (= "context-retrieval" (get-in entry [:evidence/body :event])))

(defn- retrieval-results
  "Yields {:pattern-id <id> :score <score>} per retrieved item."
  [entry]
  (for [r (get-in entry [:evidence/body :results])
        :let [pid (or (:id r) (get r "id"))]
        :when pid]
    {:pattern-id pid
     :score (or (:score r) (get r "score"))}))

(defn- day-window
  "Seq of YYYY-MM-DD strings from (now - days) back, inclusive, ending today."
  [days]
  (let [today (LocalDate/now tz)
        start (.minusDays today (dec days))]
    (mapv (fn [i] (.toString (.plusDays start i))) (range days))))

(defn- aggregate
  "Returns a map with per-day totals and per-pattern per-day counts,
   filtered to the requested window."
  [entries days]
  (let [window (day-window days)
        window-set (set window)
        retrievals (filter context-retrieval? entries)
        ;; Per-day totals (all retrieved items, not just unique)
        per-day-totals (reduce (fn [acc e]
                                 (let [day (entry->day e)]
                                   (if (window-set day)
                                     (update acc day
                                             (fnil + 0)
                                             (count (retrieval-results e)))
                                     acc)))
                               (zipmap window (repeat 0))
                               retrievals)
        ;; Per-pattern per-day counts
        per-pat (reduce
                 (fn [acc e]
                   (let [day (entry->day e)]
                     (if (window-set day)
                       (reduce (fn [a {:keys [pattern-id]}]
                                 (update-in a [pattern-id day] (fnil inc 0)))
                               acc
                               (retrieval-results e))
                       acc)))
                 {}
                 retrievals)
        pat-totals (into {} (for [[p days->n] per-pat]
                              [p (reduce + (vals days->n))]))]
    {:window window
     :per-day-totals per-day-totals
     :per-pattern per-pat
     :pattern-totals pat-totals
     :retrieval-event-count (count (filter #(window-set (entry->day %)) retrievals))
     :unique-patterns (count per-pat)}))

;; ---------- Sparkline ----------

(defn- sparkline
  "Unicode sparkline for a seq of non-negative counts. Scales to local max.
   Zero days render as `·` so they're visible (distinguishable from any
   bar) without being noisy."
  [counts]
  (if (or (empty? counts) (zero? (apply max 0 counts)))
    (apply str (repeat (count counts) \·))
    (let [mx (apply max counts)
          n (dec (count spark-chars))]
      (apply str
             (for [c counts]
               (if (zero? c)
                 \·
                 (nth spark-chars (int (Math/round (* n (/ (double c) mx)))))))))))

;; ---------- Rendering ----------

(defn- pad-right [s n]
  (let [s (str s)]
    (str s (apply str (repeat (max 0 (- n (count s))) \space)))))

(defn- render-md [{:keys [window per-day-totals per-pattern pattern-totals
                          retrieval-event-count unique-patterns]}
                  top-n]
  (let [sb (StringBuilder.)
        daily-seq (mapv #(get per-day-totals % 0) window)
        top-patterns (->> pattern-totals
                          (sort-by (comp - val))
                          (take top-n))
        name-col (max 30 (+ 2 (apply max 0 (map (comp count first) top-patterns))))]
    (.append sb (format "## Pattern Retrieval Density  _(%s → %s, %d days)_\n\n"
                        (first window) (last window) (count window)))
    (.append sb (format "**Overall:** %d context-retrieval events · %d unique patterns retrieved\n\n"
                        retrieval-event-count unique-patterns))
    (.append sb (format "**Daily density:** `%s`  max=%d\n\n"
                        (sparkline daily-seq) (apply max 0 daily-seq)))
    (if (zero? retrieval-event-count)
      (.append sb "_No retrieval activity in window._\n")
      (do
        (.append sb (format "### Top %d patterns by retrieval count\n\n" (count top-patterns)))
        (.append sb (format "| %s | Total | Daily sparkline (%s → %s) |\n"
                            (pad-right "Pattern" (- name-col 2))
                            (first window) (last window)))
        (.append sb (format "|%s|------:|%s|\n"
                            (apply str (repeat name-col \-))
                            (apply str (repeat (+ 4 (count window)) \-))))
        (doseq [[pid total] top-patterns]
          (let [per-day (get per-pattern pid {})
                series (mapv #(get per-day % 0) window)]
            (.append sb (format "| %s | %5d | `%s` |\n"
                                (pad-right pid (- name-col 2))
                                total
                                (sparkline series)))))))
    (.append sb (format "\n_Source: GET %s/api/alpha/evidence (context-retrieval events)_\n"
                        futon3c-url))
    (str sb)))

;; ---------- Entry point ----------

(defn- parse-int [s default]
  (try (Integer/parseInt (str s)) (catch Exception _ default)))

(defn -main
  ([] (-main "14" "15"))
  ([days] (-main days "15"))
  ([days top-n]
   (let [days (parse-int days 14)
         top-n (parse-int top-n 15)
         ;; Fetch enough to cover the window comfortably.
         limit (max 500 (* days 500))
         entries (or (fetch-evidence limit) [])]
     (if (empty? entries)
       (do (binding [*out* *err*]
             (println (str "No evidence entries fetched from " futon3c-url)))
           (System/exit 1))
       (println (render-md (aggregate entries days) top-n))))))

(apply -main *command-line-args*)
