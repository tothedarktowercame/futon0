#!/usr/bin/env bb
;; Linode Status Report — git activity, evidence landscape, repo freshness.
;;
;; Usage: cd futon0/scripts && bb -cp . linode-report.clj [days]
;; Default window: 14 days.

(require '[futon0.report.linode-report :as report])

(apply report/-main *command-line-args*)
