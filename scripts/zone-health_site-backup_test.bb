#!/usr/bin/env bb
;; zone-health_site-backup_test.bb — bad-case tests for the site-backup check
;; (scripts/zone-health.bb). Run: bb scripts/zone-health_site-backup_test.bb
;; (from futon0 root). The bad cases: a hub with no verified copy, and a copy
;; that stopped refreshing (the nightly timer died).

(require '[clojure.test :refer [deftest is run-tests]])

(load-file "scripts/zone-health.bb")

(def now 1793836773)

(deftest both-fresh-pass
  (is (= :pass (:status (site-backup-verdict {"metameso" (- now 3600) "lucy" (- now 7200)} now)))))

(deftest a-hub-without-a-copy-fails
  (let [r (site-backup-verdict {"metameso" (- now 3600) "lucy" nil} now)]
    (is (= :fail (:status r)))
    (is (= ["lucy"] (:items r)))))

(deftest a-copy-that-stopped-refreshing-fails
  (let [r (site-backup-verdict {"metameso" (- now 3600) "lucy" (- now (* 3 86400))} now)]
    (is (= :fail (:status r)))
    (is (= ["lucy: 72 h old"] (:items r)))))

(let [{:keys [fail error]} (run-tests)] (System/exit (if (zero? (+ fail error)) 0 1)))
