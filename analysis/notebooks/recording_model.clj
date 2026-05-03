(ns recording-model
  "Click 1: Evening commit time predicts morning recording.

   Loads git commit history and Zoom R4 recording index, pairs each day
   with (a) whether a recording happened and (b) the latest commit hour
   the night before.  Computes a Beta-binomial posterior for
   P(recording | commit-time bucket).

   Invariant: D-I5 (read-only observer).
   Pattern:   corps/working-where-others-can-see"
  (:require [clojure.edn :as edn]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [cheshire.core :as json])
  (:import (java.time LocalDate ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def assumptions (-> "assumptions.edn" slurp edn/read-string))
(def home (System/getProperty "user.home"))
(def tz (ZoneId/of (get-in assumptions [:data :timezone])))

(defn expand-home [path]
  (str/replace path "~" home))

;; ---------------------------------------------------------------------------
;; Git data: latest commit hour per day
;; ---------------------------------------------------------------------------

(defn- git [repo-path & args]
  (let [{:keys [exit out]} (apply shell/sh "git" "-C" repo-path args)]
    (when (zero? exit) (str/trim out))))

(defn- find-repos []
  (let [root (expand-home (get-in assumptions [:data :git-repos-root]))
        prefixes (get-in assumptions [:data :git-repo-prefixes])]
    (->> (java.io.File. root)
         .listFiles
         (filter #(.isDirectory %))
         (filter (fn [d]
                   (and (.exists (java.io.File. d ".git"))
                        (some #(str/starts-with? (.getName d) %) prefixes))))
         (mapv #(.getAbsolutePath %)))))

(defn- parse-local-zdt [ts-str]
  (try
    (.withZoneSameInstant (ZonedDateTime/parse ts-str) tz)
    (catch Exception _ nil)))

(defn latest-commit-hour-by-day
  "Return {\"2026-03-15\" 22, ...} mapping each date to the latest
   commit hour across all repos."
  []
  (let [repos (find-repos)
        all-zdts (for [repo repos
                       :let [out (git repo "log" "--format=%aI")]
                       :when out
                       line (str/split-lines out)
                       :let [zdt (parse-local-zdt line)]
                       :when zdt]
                   zdt)]
    (->> all-zdts
         (group-by #(str (.toLocalDate %)))
         (map (fn [[day zdts]]
                [day (apply max (map #(.getHour %) zdts))]))
         (into (sorted-map)))))

;; ---------------------------------------------------------------------------
;; Recording data: which days had a recording
;; ---------------------------------------------------------------------------

(defn recording-dates
  "Return a set of date strings (\"2026-03-15\") that had a recording."
  []
  (let [path (expand-home (get-in assumptions [:data :recordings-index]))
        data (json/parse-string (slurp path) true)
        entries (:entries data)]
    (->> entries
         (keep :recorded_at)
         (map #(subs % 0 10))
         set)))

;; ---------------------------------------------------------------------------
;; Pair days: for each day, did recording happen? what was last commit
;; hour the night before?
;; ---------------------------------------------------------------------------

(defn build-daily-log
  "Return a seq of {:date, :recorded, :prev-last-commit-hour}."
  []
  (let [commits (latest-commit-hour-by-day)
        recs (recording-dates)
        all-dates (sort (distinct (concat (keys commits)
                                          (seq recs))))
        ;; Build a vector of all dates from earliest to latest
        first-date (LocalDate/parse (first all-dates))
        last-date  (LocalDate/parse (last all-dates))
        days (loop [d first-date acc []]
               (if (.isAfter d last-date)
                 acc
                 (recur (.plusDays d 1) (conj acc (str d)))))]
    (vec
     (for [day (rest days) ;; skip first day (no "night before")
           :let [prev-day (str (.minusDays (LocalDate/parse day) 1))
                 prev-hour (get commits prev-day)
                 recorded? (contains? recs day)]]
       {:date day
        :recorded recorded?
        :prev-last-commit-hour prev-hour}))))

;; ---------------------------------------------------------------------------
;; Bucketing
;; ---------------------------------------------------------------------------

(defn assign-bucket
  "Given a commit hour (or nil), return the bucket :id."
  [hour buckets]
  (when hour
    (some (fn [{:keys [id min max]}]
            (when (<= min hour max) id))
          buckets)))

(defn bucket-days
  "Group daily-log entries by commit-time bucket."
  [daily-log]
  (let [buckets (get-in assumptions [:click-1 :buckets])]
    (->> daily-log
         (filter :prev-last-commit-hour) ;; exclude days with no commits
         (group-by #(assign-bucket (:prev-last-commit-hour %) buckets)))))

;; ---------------------------------------------------------------------------
;; Beta-binomial posterior
;; ---------------------------------------------------------------------------

(defn posterior
  "Compute Beta posterior parameters given observed data and prior."
  [days {:keys [alpha beta]}]
  (let [n (count days)
        k (count (filter :recorded days))]
    {:n n
     :k k
     :alpha-post (+ alpha k)
     :beta-post  (+ beta (- n k))
     :mean       (/ (double (+ alpha k))
                    (+ alpha k beta (- n k)))
     :recording-rate (if (pos? n) (/ (double k) n) 0.0)}))

(defn credible-interval
  "Compute symmetric credible interval from Beta(a,b).
   Uses the quantile function via inverse regularized beta."
  [alpha beta level]
  (let [lo-p (/ (- 1.0 level) 2.0)
        hi-p (+ lo-p level)
        ;; Apache commons math via fastmath, or manual approx
        ;; For now use a simple normal approximation of Beta quantiles
        mu (/ (double alpha) (+ alpha beta))
        var (/ (* (double alpha) beta)
               (* (+ alpha beta) (+ alpha beta) (+ alpha beta 1.0)))
        sd (Math/sqrt var)
        ;; Normal quantile approximation (Beasley-Springer-Moro)
        z (cond
            (= level 0.80) 1.282
            (= level 0.95) 1.960
            :else           1.645)]
    [(max 0.0 (- mu (* z sd)))
     (min 1.0 (+ mu (* z sd)))]))

;; ---------------------------------------------------------------------------
;; Main analysis
;; ---------------------------------------------------------------------------

(defn run-click-1
  "Run the full Click 1 analysis. Returns a map of results."
  []
  (let [daily-log (build-daily-log)
        buckets (get-in assumptions [:click-1 :buckets])
        prior (get-in assumptions [:click-1 :prior])
        bucketed (bucket-days daily-log)
        results (mapv (fn [{:keys [id label] :as bucket}]
                        (let [days (get bucketed id [])
                              post (posterior days prior)
                              [lo80 hi80] (credible-interval
                                            (:alpha-post post) (:beta-post post) 0.80)
                              [lo95 hi95] (credible-interval
                                            (:alpha-post post) (:beta-post post) 0.95)]
                          (assoc post
                                 :bucket-id id
                                 :label label
                                 :ci-80 [lo80 hi80]
                                 :ci-95 [lo95 hi95])))
                      buckets)]
    {:daily-log daily-log
     :total-days (count daily-log)
     :days-with-commits (count (filter :prev-last-commit-hour daily-log))
     :days-with-recordings (count (filter :recorded daily-log))
     :buckets results}))

;; ---------------------------------------------------------------------------
;; Markdown rendering
;; ---------------------------------------------------------------------------

(defn- pad-right [s w] (str s (apply str (repeat (- w (count s)) \space))))
(defn- pad-left  [s w] (str (apply str (repeat (- w (count s)) \space)) s))

(defn- render-table [headers aligns rows]
  (let [ncols (count headers)
        widths (mapv (fn [i]
                       (apply max (count (nth headers i))
                              (map #(count (nth % i "")) rows)))
                     (range ncols))
        pad (fn [s w align]
              (case align
                :right (pad-left (or s "") w)
                (pad-right (or s "") w)))
        fmt-row (fn [cells]
                  (str "| "
                       (str/join " | "
                                 (map-indexed #(pad %2 (nth widths %1) (nth aligns %1)) cells))
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

(defn render-click-1
  "Render Click 1 results as markdown."
  [{:keys [total-days days-with-commits days-with-recordings buckets]}]
  (let [sb (StringBuilder.)]
    (.append sb "# Click 1: Evening Commits Predict Morning Recording\n\n")
    (.append sb (str "**Data:** " total-days " days, "
                     days-with-commits " with commits, "
                     days-with-recordings " with recordings\n\n"))

    ;; Summary table
    (.append sb "## Posterior: P(recording | last commit bucket)\n\n")
    (.append sb (render-table
                  ["Bucket" "Days" "Recorded" "P(rec)" "80% CI" "95% CI"]
                  [:left :right :right :right :right :right]
                  (mapv (fn [{:keys [label n k mean ci-80 ci-95]}]
                          [label
                           (str n)
                           (str k)
                           (format "%.2f" mean)
                           (format "[%.2f, %.2f]" (first ci-80) (second ci-80))
                           (format "[%.2f, %.2f]" (first ci-95) (second ci-95))])
                        buckets)))
    (.append sb "\n")

    ;; Interpretation
    (let [sorted (sort-by :mean > buckets)
          best (first sorted)
          worst (last sorted)]
      (.append sb (str "**Best window:** " (:label best)
                       " — P(recording) = " (format "%.2f" (:mean best)) "\n"))
      (.append sb (str "**Worst window:** " (:label worst)
                       " — P(recording) = " (format "%.2f" (:mean worst)) "\n\n")))

    ;; Visual: ASCII posterior bars
    (.append sb "## Visual\n\n")
    (.append sb "```\n")
    (doseq [{:keys [label mean ci-80]} buckets]
      (let [bar-width 30
            filled (Math/round (* mean bar-width))
            lo-mark (Math/round (* (first ci-80) bar-width))
            hi-mark (Math/round (* (second ci-80) bar-width))
            bar (apply str
                       (for [i (range bar-width)]
                         (cond
                           (= i filled) "|"
                           (and (>= i lo-mark) (<= i hi-mark)) "-"
                           :else " ")))]
        (.append sb (str (pad-right label 14)
                         " " bar " " (format "%.0f%%" (* 100 mean)) "\n"))))
    (.append sb "```\n\n")

    (str sb)))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(defn -main [& _args]
  (let [results (run-click-1)
        md (render-click-1 results)]
    (println md)))
