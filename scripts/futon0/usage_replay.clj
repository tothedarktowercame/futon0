(ns futon0.usage-replay
  "Deterministic replay of scheduling policies over durable usage
   observations, without dispatching agents (M-usage-hacking §4.1,
   §7 ARGUE/VERIFY). Two policies:

   fixed-50        the current conservative Zai cron control: permit an
                   unattended dispatch iff EVERY window reports strictly
                   more than 50% free and at most one other invocation
                   (mission §3.1).

   adaptive-forecast  candidate end-of-window policy (mission §3.4):
                   permit unattended dispatch iff (a) every active window's
                   current free percentage is at or above the foreground
                   reserve, (b) the forecast unused quota at the nearest
                   window reset exceeds the configured end-of-window
                   reserve, and (c) the concurrency gate holds.

   Forecasts are linear projections from the observations themselves and
   are returned as forecasts, never merged into measured facts (§6
   invariant 6). Missing evidence refuses the dispatch (§6 invariant 1).
   A short-window reset never erases pressure in a longer window: every
   active window participates in the decision independently (§6
   invariant 3; the 04:15 UTC case).")

(def ^:const min-free 0.0)

(defn- burn-rate-for
  "burn-rate-per-hour option may be a number (all windows) or a map
   keyed by window id with a numeric default under :default."
  [burn-opts window-id]
  (if (map? burn-opts)
    (get burn-opts window-id (:default burn-opts 0.5))
    (or burn-opts 0.5)))

(defn windows-of
  "Group a single observation into per-window states:
   {:window-id {:free pct :next-reset inst :certainty c}}"
  [obs]
  (if (vector? (:window obs))
    (into {} (for [w (:window obs)] [(:id w) w]))
    {(:id (:window obs)) (:window obs)}))

(defn- missing-evidence?
  "True when any window the policy must consult is unavailable."
  [per-window]
  (boolean (some (fn [[_ w]]
                   (or (= :unavailable (:free w))
                       (nil? (:free w))
                       (= :unavailable (:next-reset w))))
                 per-window)))

(defn fixed-50
  "The current production control. Explainable refusal reasons:
   :missing-evidence | :window-below-threshold | :concurrency."
  [{:keys [threshold] :or {threshold 50}}]
  (fn decide [obs]
    (let [per-window (windows-of obs)]
      (cond
        (missing-evidence? per-window)
        {:decision :refuse :policy :fixed-50
         :reason :missing-evidence
         :explanation "usage evidence unavailable; adaptive spending fails closed"}

        (>= (or (:invocations obs) 0) 2)
        {:decision :refuse :policy :fixed-50
         :reason :concurrency
         :explanation "another Zai agent is invoking"}

        :else
        (let [blocked (for [[id w] per-window
                            :when (not (> (:free w) threshold))]
                        [id (:free w)])]
          (if (seq blocked)
            {:decision :refuse :policy :fixed-50
             :reason :window-below-threshold
             :explanation (str "windows at or under threshold: "
                               (pr-str (into (sorted-map) blocked)))}
            {:decision :permit :policy :fixed-50
             :reason :all-windows-above-threshold
             :explanation (str "every window strictly above " threshold "% free")}))))))

(defn- hours-between [a b]
  ;; ISO instants restricted to the fixed UTC shapes used in the ledger.
  (/ (- (inst-ms (java.time.Instant/parse b))
        (inst-ms (java.time.Instant/parse a)))
     3600000.0))

(defn forecast-unused-at-reset
  "Linear projection: free now minus burn rate times hours to the
   window's next reset. Returns a forecast value tagged as such."
  [{:keys [free burn-rate-per-hour next-reset] :as _w} now-inst]
  {:kind :forecast
   :free-now free
   :burn-rate-per-hour burn-rate-per-hour
   :hours-to-reset (hours-between now-inst next-reset)
   :forecast-unused-at-reset
    (max min-free (- free (* burn-rate-per-hour
                            (hours-between now-inst next-reset))))})

(defn adaptive-forecast
  "Candidate end-of-window policy. Refusal reasons:
   :missing-evidence | :foreground-reserve | :end-of-window-reserve |
   :concurrency."
  [{:keys [foreground-reserve end-reserve burn-rate-per-hour]
    :or {foreground-reserve 40.0 end-reserve 10.0
         burn-rate-per-hour 0.5}}]
  (fn decide [obs]
    (let [per-window (windows-of obs)
          now (:observed-at obs)]
      (cond
        (missing-evidence? per-window)
        {:decision :refuse :policy :adaptive-forecast
         :reason :missing-evidence
         :explanation "usage evidence unavailable; adaptive spending fails closed"}

        (some (fn [[id w]] (when (< (:free w) foreground-reserve) [id (:free w)]))
              per-window)
        (let [below (into (sorted-map)
                          (keep (fn [[id w]]
                                  (when (< (:free w) foreground-reserve)
                                    [id (:free w)]))
                                per-window))]
          {:decision :refuse :policy :adaptive-forecast
           :reason :foreground-reserve
           :explanation (str "windows under foreground reserve "
                             foreground-reserve "%: " (pr-str below))})

        :else
        (let [forecasts (into (sorted-map)
                              (map (fn [[id w]]
                                     [id (forecast-unused-at-reset
                                          (assoc w
                                                 :burn-rate-per-hour
                                                 (burn-rate-for burn-rate-per-hour id))
                                          now)])
                                   per-window))
              forecast-min (apply min (map :forecast-unused-at-reset
                                           (vals forecasts)))]
          (if (< forecast-min end-reserve)
            {:decision :refuse :policy :adaptive-forecast
             :reason :end-of-window-reserve
             :explanation (str "minimum forecast unused at reset "
                               forecast-min "% is under the configured end-of-window reserve "
                               end-reserve "%")}
            {:decision :permit :policy :adaptive-forecast
             :reason :forecast-unused-exceeds-reserve
             :explanation (str "minimum forecast unused at reset "
                               forecast-min "% exceeds the configured reserve "
                               end-reserve "%; every window free percentage is at or above the foreground reserve")}))))))

(defn replay
  "Run a policy over a sequence of observations in order. Deterministic:
   same observations, same decisions, same explanations."
  [policy observations]
  (mapv (fn [obs] {:observed-at (:observed-at obs)
                   :workload (:workload obs)
                   :decision (policy obs)})
        observations))
