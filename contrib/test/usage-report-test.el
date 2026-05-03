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
    (should (equal (usage-report--claude-headline-fragment
                    current-snap week-snap (list anchor))
                   "Claude est 24% 5h / 20% wk"))))

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
