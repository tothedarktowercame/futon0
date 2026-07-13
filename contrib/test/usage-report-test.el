;;; usage-report-test.el --- ERT tests for usage-report -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))
(require 'usage-report)

(defun usage-report-test--snap (claude codex)
  (list :totals (list :claude claude :codex codex)
        :claude nil
        :codex nil))

(ert-deftest usage-report-median ()
  (should (= (usage-report--median '(4 1 9)) 4))
  (should (= (usage-report--median '(4 1 9 11)) 6.5)))

(ert-deftest usage-report-claude-estimates-weighted ()
  (let* ((current-totals (list :output 2400
                               :messages 12
                               :weighted-input-equiv 24000))
         (week-totals (list :output 20000
                            :messages 120
                            :weighted-input-equiv 200000))
         (anchor (list :current-window-minutes 300
                       :week-window-minutes 10080
                       :current-used-pct 24.0
                       :week-used-pct 20.0
                       :current-totals current-totals
                       :week-totals week-totals))
         (current-snap (usage-report-test--snap current-totals
                                                (list :used-pct-max 8.0
                                                      :resets-at "2026-05-01T15:56:03Z")))
         (week-snap (usage-report-test--snap week-totals nil))
         (est (let ((usage-report-claude-estimate-metric :weighted-input-equiv))
                (usage-report--claude-estimates current-snap week-snap (list anchor)))))
    (should (= (plist-get est :current-cap) 100000.0))
    (should (= (plist-get est :week-cap) 1000000.0))
    (should (= (plist-get est :current-used-pct) 24.0))
    (should (= (plist-get est :week-used-pct) 20.0))))

(ert-deftest usage-report-claude-headline-fragment-prefers-estimate ()
  (let* ((current-totals (list :output 2400
                               :messages 12
                               :weighted-input-equiv 24000))
         (week-totals (list :output 20000
                            :messages 120
                            :weighted-input-equiv 200000))
         (anchor (list :current-window-minutes 300
                       :week-window-minutes 10080
                       :current-used-pct 24.0
                       :week-used-pct 20.0
                       :current-totals current-totals
                       :week-totals week-totals))
         (current-snap (usage-report-test--snap current-totals
                                                (list :used-pct-max 8.0
                                                      :resets-at "2026-05-01T15:56:03Z")))
         (week-snap (usage-report-test--snap week-totals nil))
         (usage-report-claude-estimate-metric :weighted-input-equiv))
    (cl-letf (((symbol-function 'usage-report-claude-live) (lambda (&rest _) nil)))
      (should (equal (usage-report--claude-headline-fragment
                      current-snap week-snap (list anchor))
                     "Claude est 24% 5h / 20% wk")))))

(ert-deftest usage-report-read-write-anchors-roundtrip ()
  (let* ((path (make-temp-file "usage-report-anchors-" nil ".eld"))
         (usage-report-claude-anchor-file path)
         (anchors (list (list :captured-at "2026-05-01T12:00:00Z"
                              :current-used-pct 24.0
                              :week-used-pct 20.0))))
    (unwind-protect
        (progn
          (usage-report--write-claude-anchors anchors)
          (should (equal (usage-report-claude-anchors) anchors)))
      (delete-file path))))

(ert-deftest usage-report-empty-anchor-file-is-nil ()
  (let* ((path (make-temp-file "usage-report-empty-anchors-" nil ".eld"))
         (usage-report-claude-anchor-file path))
    (unwind-protect
        (should-not (usage-report-claude-anchors))
      (delete-file path))))

(ert-deftest usage-report-zai-live-parse-token-windows ()
  (let* ((response
          '(:success t
            :data (:level "max"
                   :limits ((:type "TOKENS_LIMIT" :unit 3 :number 5
                             :percentage 10 :nextResetTime 1783976431687)
                            (:type "TOKENS_LIMIT" :unit 6 :number 1
                             :percentage 34 :nextResetTime 1784364508997)
                            (:type "TIME_LIMIT" :unit 1 :number 100
                             :percentage 7 :nextResetTime 1784000000000)))))
         (live (usage-report--zai-live-parse response)))
    (should (equal (plist-get live :level) "max"))
    (should (= (plist-get live :five-hour-used-pct) 10))
    (should (= (plist-get live :five-hour-free-pct) 90.0))
    (should (= (plist-get live :weekly-used-pct) 34))
    (should (= (plist-get live :weekly-free-pct) 66.0))
    (should (= (plist-get live :weekly-resets-ms) 1784364508997))))

(ert-deftest usage-report-zai-live-parse-rejects-unsuccessful-response ()
  (should-not
   (usage-report--zai-live-parse
    '(:success nil :data (:limits ((:type "TOKENS_LIMIT" :unit 3 :number 5
                                    :percentage 10)))))))

(ert-deftest usage-report-zai-headline-reports-free-quota ()
  (should
   (equal (usage-report--zai-headline-fragment
           '(:five-hour-free-pct 90.0 :weekly-free-pct 66.0))
          "ZAI 90% 5h / 66% wk free")))

(ert-deftest usage-report-stack-hud-renders-zai-without-local-snapshot ()
  (cl-letf (((symbol-function 'usage-report-snapshot) (lambda (&rest _) nil))
            ((symbol-function 'usage-report-claude-anchors) (lambda () nil))
            ((symbol-function 'usage-report-zai-live)
             (lambda (&rest _)
               '(:level "max"
                 :five-hour-used-pct 10 :five-hour-free-pct 90.0
                 :five-hour-resets-ms 1783976431687
                 :weekly-used-pct 34 :weekly-free-pct 66.0
                 :weekly-resets-ms 1784364508997))))
    (with-temp-buffer
      (usage-report-insert-stack-hud-block)
      (let ((rendered (buffer-string)))
        (should (string-match-p "(script unavailable)" rendered))
        (should (string-match-p "ZAI: 10\\.0% 5h / 34\\.0% wk used" rendered))
        (should (string-match-p "90\\.0% / 66\\.0% free; Max plan" rendered))
        (should (string-match-p "source: ZAI quota api (live)" rendered))))))
