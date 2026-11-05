#!/usr/bin/env bb
;; zone-health_backup-paths_test.bb — bad-case tests for the site-backup and reverse-tunnel checks
;; (scripts/zone-health.bb). Run: bb scripts/zone-health_backup-paths_test.bb
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

(deftest tunnel-in-use-passes
  (is (= :pass (:status (tunnel-verdict true (- now 600) now)))))

(deftest tunnel-unit-down-fails
  (is (= :fail (:status (tunnel-verdict false (- now 600) now)))))

(deftest tunnel-not-used-recently-fails
  (let [r (tunnel-verdict true (- now 3600) now)]
    (is (= :fail (:status r)))
    (is (re-find #"60 min ago" (:summary r)))))

(deftest tunnel-never-used-fails
  (is (= :fail (:status (tunnel-verdict true nil now)))))

(deftest pass-sync-current-passes
  (is (= :pass (:status (pass-sync-verdict (- now 600) {"phone1" (- now 3600) "phone2" (- now 86400)} now)))))

(deftest pass-sync-stopped-fails
  (is (= :fail (:status (pass-sync-verdict (- now 7200) {"phone1" (- now 600) "phone2" (- now 600)} now)))))

(deftest pass-sync-never-ran-fails
  (is (= :fail (:status (pass-sync-verdict nil {"phone1" (- now 600) "phone2" (- now 600)} now)))))

(deftest a-phone-lagging-a-week-fails
  (let [r (pass-sync-verdict (- now 600) {"phone1" (- now 600) "phone2" (- now (* 8 86400))} now)]
    (is (= :fail (:status r)))
    (is (= ["phone2: 8 days since last sync"] (:items r)))))

(deftest chrome-import-this-month-passes
  (is (= :pass (:status (chrome-import-verdict (- now (* 10 86400)) now)))))

(deftest chrome-import-overdue-fails
  (is (= :fail (:status (chrome-import-verdict (- now (* 40 86400)) now)))))

(deftest git-backup-fresh-receipt-passes
  (is (= :pass (:status (git-backup-verdict {"recovery_exit" 0} (- now 600) now)))))

(deftest git-backup-stale-receipt-fails
  (is (= :fail (:status (git-backup-verdict {"recovery_exit" 0} (- now 7200) now)))))

(deftest git-backup-failed-recovery-fails
  (is (= :fail (:status (git-backup-verdict {"recovery_exit" 1} (- now 600) now)))))

(deftest git-backup-no-receipt-fails
  (is (= :fail (:status (git-backup-verdict nil nil now)))))

(let [{:keys [fail error]} (run-tests)] (System/exit (if (zero? (+ fail error)) 0 1)))
