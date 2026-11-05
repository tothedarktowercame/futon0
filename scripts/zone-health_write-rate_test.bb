#!/usr/bin/env bb
;; zone-health_write-rate_test.bb — bad-case tests for the write-rate check
;; (scripts/zone-health.bb). Loads the script, which is inert under load (the
;; main block runs only when bb runs the file directly).
;; Run: bb scripts/zone-health_write-rate_test.bb   (from futon0 root)
;;
;; The bad case this check is named for (zai-7 audit, 2026-11-04): disk-watch
;; stopped sampling, so the TB delta reads 0 and the check used to PASS.

(require '[clojure.test :refer [deftest is testing run-tests]])

(load-file "scripts/zone-health.bb")

(def now 1793836773) ; fixed epoch so the fixtures cannot drift with wall time

(defn- sample
  "One disk-watch CSV row at epoch T with cumulative TB written."
  [t tb]
  (format "%d,45,%s,669,0,0,0,0" t tb))

(defn- fresh-rows
  "Samples spanning the last hour plus an older one, moderate writes."
  []
  [(sample (- now 7200) "10.840") (sample (- now 3600) "10.850")
   (sample (- now 300) "10.853")])

(deftest fresh-moderate-writes-pass
  (let [{:keys [problems detail]} (write-rate-verdict (fresh-rows) now)]
    (is (empty? problems))
    (is (re-find #"3\.0 GB last hour" detail))))

(deftest over-the-limit-fails
  (let [rows [(sample (- now 7200) "10.000") (sample (- now 3600) "10.000")
              (sample (- now 300) "10.020")] ; 20 GB in the last hour
        {:keys [problems fix]} (write-rate-verdict rows now)]
    (is (some #(re-find #"20\.0 GB written in the last hour \(limit 11 GB\)" %) problems))
    (is (re-find #"writes.txt" fix))))

(deftest stale-newest-sample-fails-not-zero-delta-pass
  (testing "disk-watch stopped: newest sample is 2 h old, so the hour delta would be 0"
    (let [rows [(sample (- now 14400) "10.800") (sample (- now 7200) "10.801")]
          {:keys [problems fix]} (write-rate-verdict rows now)]
      (is (some #(re-find #"newest sample 120 min old" %) problems))
      (is (re-find #"disk-watch.timer" fix)))))

(deftest no-rows-fail
  (let [{:keys [problems]} (write-rate-verdict [] now)]
    (is (some #(re-find #"no disk-watch samples" %) problems))))

(deftest no-sample-an-hour-old-fails
  (let [rows [(sample (- now 120) "10.850")]
        {:keys [problems]} (write-rate-verdict rows now)]
    (is (some #(re-find #"no sample at least 1 h old" %) problems))))

(defn- run-tests* [] (run-tests))
(when (= *file* (System/getProperty "babashka.file"))
  (let [summary (run-tests*)]
    (System/exit (if (pos? (+ (:fail summary) (:error summary))) 1 0))))
