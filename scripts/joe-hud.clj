#!/usr/bin/env bb
;; Joe HUD — behavioral signals: work schedule, evidence discipline,
;; stack breadth, creative workflow.
;;
;; Usage: cd futon0/scripts && bb -cp . joe-hud.clj [days]
;; Default window: 14 days.

(require '[futon0.report.joe-hud :as hud])

(apply hud/-main *command-line-args*)
