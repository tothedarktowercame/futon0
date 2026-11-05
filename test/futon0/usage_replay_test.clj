(ns futon0.usage-replay-test
  "Replay tests for M-usage-hacking: the fixed 50% control and the
   adaptive-forecast candidate produce explainable decisions over the
   durable Zai 13–18 July observations; missing evidence fails closed;
   every active window and the foreground reserve stay enforced."
  (:require [clojure.test :refer [deftest is]]
            [futon0.usage-ledger :as ledger]
            [futon0.usage-replay :as replay]))

(def ^:const obs-path "resources/usage-observations/zai-2026-07-13-to-14.edn")

(def observations (ledger/read-all obs-path))

(def fixed (replay/fixed-50 {}))
(def adaptive (replay/adaptive-forecast
               {:foreground-reserve 40.0 :end-reserve 10.0
                :burn-rate-per-hour 0.5}))

(deftest ledger-observations-are-durable-and-valid
  (is (= 4 (count observations)))
  (is (every? #(not= :malformed-ledger-record (:kind %)) observations))
  (is (every? #(= :zai (:provider %)) observations)))

(deftest fixed-50-reproduces-the-documented-zai-decisions
  (let [decisions (replay/replay fixed observations)]
    (is (= [[:permit] [:refuse] [:refuse] [:refuse]]
           (mapv (comp vector :decision :decision) decisions)))
    (is (= [:all-windows-above-threshold
            :window-below-threshold
            :window-below-threshold
            :window-below-threshold]
           (mapv (comp :reason :decision) decisions)))))

(deftest short-window-reset-does-not-erase-weekly-pressure
  ;; 04:15 UTC: the five-hour window reset to 99% free, but the weekly
  ;; window stood at 45%; the fixed gate stayed closed (§3.2, §6 inv 3).
  (let [d ((replay/fixed-50 {}) (nth observations 2))]
    (is (= :refuse (:decision d)))
    (is (.contains ^String (:explanation d) "zai-weekly"))))

(deftest adaptive-forecast-explains-every-decision
  (let [decisions (replay/replay adaptive observations)]
    (is (= 4 (count decisions)))
    (is (every? #(string? (:explanation %))
                (map :decision decisions)))
    ;; forecast reasons are distinct from measured facts (§6 inv 6)
    (is (every? #(contains? #{:missing-evidence :foreground-reserve
                              :end-of-window-reserve
                              :forecast-unused-exceeds-reserve}
                            (:reason %))
                (map :decision decisions)))))

(deftest missing-evidence-refuses-both-policies
  (let [gap {:schema ledger/schema-version :provider :zai
             :observed-at "2026-07-14T10:00:00Z" :source "test"
             :window [{:id :zai-5h :free :unavailable
                       :next-reset "2026-07-14T15:00:00Z"}
                      {:id :zai-weekly :free 42
                       :next-reset "2026-07-18T08:48:00Z"}]
             :certainty :unavailable :invocations 1 :workload :unattended}]
    (is (= :refuse (:decision (fixed gap))))
    (is (= :missing-evidence (:reason (fixed gap))))
    (is (= :refuse (:decision (adaptive gap))))
    (is (= :missing-evidence (:reason (adaptive gap))))))

(deftest adaptive-policy-honors-foreground-reserve
  ;; both windows forecast to end far above the end reserve, but the
  ;; weekly free percentage sits under the foreground reserve: refuse.
  (let [d (adaptive {:schema ledger/schema-version
                     :provider :zai
                     :observed-at "2026-07-14T10:00:00Z"
                     :source "test"
                     :window [{:id :zai-5h :free 95
                               :next-reset "2026-07-14T15:00:00Z"}
                              {:id :zai-weekly :free 39
                               :next-reset "2026-07-18T08:48:00Z"}]
                     :certainty :provider-reported
                     :invocations 1 :workload :unattended})]
    (is (= :refuse (:decision d)))
    (is (= :foreground-reserve (:reason d)))))

(deftest adaptive-policy-permits-when-forecast-unused-exceeds-reserve
  (let [d (adaptive {:schema ledger/schema-version
                     :provider :zai
                     :observed-at "2026-07-18T04:00:00Z"
                     :source "test"
                     :window [{:id :zai-5h :free 95
                               :next-reset "2026-07-18T09:00:00Z"}
                              {:id :zai-weekly :free 60
                               :next-reset "2026-07-18T08:48:00Z"}]
                     :certainty :provider-reported
                     :invocations 1 :workload :unattended})]
    (is (= :permit (:decision d)))
    (is (= :forecast-unused-exceeds-reserve (:reason d)))))

(deftest replay-is-deterministic
  (is (= (replay/replay fixed observations)
         (replay/replay fixed observations)))
  (is (= (replay/replay adaptive observations)
         (replay/replay adaptive observations))))

(deftest ledger-append-rejects-invalid-observations
  (let [tmp (java.io.File/createTempFile "usage-ledger" ".edn")]
    (.deleteOnExit tmp)
    (is (thrown? Exception
                 (ledger/append! (.getPath tmp)
                                 [{:schema "wrong" :provider :zai}])))
    (is (= "" (slurp tmp)) "nothing written when any record is invalid")))
