#!/usr/bin/env bb
;; Run Click 1 analysis: evening commits predict morning recording.
;;
;; Usage: cd futon0/analysis && bb -cp notebooks:.. run-click-1.clj

(require '[recording-model :as m])

(apply m/-main *command-line-args*)
