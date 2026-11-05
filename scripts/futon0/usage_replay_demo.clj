(ns futon0.usage-replay-demo
  "Replay the durable Zai 13–18 July 2026 observations through the fixed
   50% control and the adaptive-forecast candidate, printing every
   decision with its explanation. Deterministic: same inputs, same
   output, no wall-clock reads."
  (:require [futon0.usage-ledger :as ledger]
            [futon0.usage-replay :as replay]))

(def ^:const observations-path
  "resources/usage-observations/zai-2026-07-13-to-14.edn")

(defn -main [& _]
  (let [obs (ledger/read-all observations-path)
        policies {:fixed-50 (replay/fixed-50 {})
                  :adaptive-forecast
                    (replay/adaptive-forecast
                     {:foreground-reserve 40.0
                      :end-reserve 10.0
                      :burn-rate-per-hour
                      ;; observed drifts from the durable Zai window:
                      ;; five-hour 58→49 over ~2.5h, weekly 51→49.
                      {:zai-5h 3.6 :zai-weekly 0.3 :default 0.5}})}]
    (println "replaying" (count obs) "durable observations of"
             "resources/usage-observations/zai-2026-07-13-to-14.edn")
    (doseq [[name policy] policies]
      (println)
      (println "== policy" name "==")
      (doseq [{:keys [observed-at decision]} (replay/replay policy obs)]
        (println observed-at (:decision decision) (:reason decision)
                 "-" (:explanation decision))))
    (shutdown-agents)))
