;;; stack-hud-test.el --- ERT tests for stack-hud -*- lexical-binding: t; -*-

(require 'ert)
(require 'subr-x)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'stack-hud)

(ert-deftest stack-hud-status-symbol ()
  (should (eq (my-chatgpt-shell--stack-status-symbol 'soon) 'soon))
  (should (eq (my-chatgpt-shell--stack-status-symbol "Soon") 'soon)))

(ert-deftest stack-hud-format-hours ()
  (should (equal (my-chatgpt-shell--stack-format-hours nil) "n/a"))
  (should (equal (my-chatgpt-shell--stack-format-hours 0.5) "30m"))
  (should (equal (my-chatgpt-shell--stack-format-hours 1.25) "1.2h"))
  (should (equal (my-chatgpt-shell--stack-format-hours 49) "2d 1h")))

(ert-deftest stack-hud-top-children ()
  (let ((text (my-chatgpt-shell--stack-top-children
               (list (list :name "alpha" :recent 2)
                     (list :name "beta" :recent 1)))))
    (should (string-match-p "alpha" text))
    (should (string-match-p "2" text))
    (should (string-match-p "beta" text))))

(ert-deftest stack-hud-join-names ()
  (should (equal (my-chatgpt-shell--stack-join-names '("a" :b 3 "d" "e"))
                 "a, :b, 3, d")))

(ert-deftest stack-hud-read-edn-file ()
  (let ((path (make-temp-file "stack-hud-" nil ".edn")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "{:ok true, :nested {:items [1 2], :missing false}}"))
          (let ((data (stack-hud--read-edn-file path)))
            (should (eq (plist-get data :ok) t))
            (should (equal (plist-get (plist-get data :nested) :items) '(1 2)))
            (should-not (plist-get (plist-get data :nested) :missing))))
      (delete-file path))))

(ert-deftest stack-hud-briefing-finds-claude-in-extra-exec-path ()
  (let* ((dir (make-temp-file "stack-hud-bin-" t))
         (program (expand-file-name "claude" dir)))
    (unwind-protect
        (progn
          (with-temp-file program
            (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes program #o755)
          (let ((stack-hud-briefing-claude-command "claude")
                (stack-hud-briefing-extra-exec-path (list dir))
                (exec-path nil)
                (process-environment '("PATH=/stack-hud/no-such-dir")))
            (should (equal (stack-hud--briefing-claude-program) program))))
      (delete-directory dir t))))

(ert-deftest stack-hud-briefing-generate-missing-command-does-not-signal ()
  (let ((stack-hud-briefing-claude-command "stack-hud-missing-claude")
        (stack-hud-briefing-extra-exec-path nil)
        (stack-hud--briefing-cache nil)
        (stack-hud--briefing-generating nil)
        (stack-hud--briefing-last-error nil)
        (exec-path nil)
        (process-environment '("PATH=/stack-hud/no-such-dir")))
    (should-not (stack-hud--briefing-generate))
    (should-not stack-hud--briefing-generating)
    (should (string-match-p "Cannot find Claude CLI"
                            stack-hud--briefing-last-error))))

(ert-deftest stack-hud-briefing-insert-renders-command-error ()
  (let ((stack-hud--briefing-cache nil)
        (stack-hud--briefing-generating nil)
        (stack-hud--briefing-last-error "Cannot find Claude CLI"))
    (with-temp-buffer
      (my-chatgpt-shell--insert-stack-briefing)
      (let ((text (buffer-string)))
        (should (string-match-p "Briefing unavailable" text))
        (should (string-match-p "Cannot find Claude CLI" text))))))

(ert-deftest stack-hud-apm-scan-uses-canonical-problem-bundles ()
  (let* ((root (make-temp-file "stack-hud-apm-" t))
         (source-dir (expand-file-name "apm" root))
         (problems-dir (expand-file-name "problems" root)))
    (unwind-protect
        (progn
          (make-directory source-dir t)
          (make-directory problems-dir t)
          (dolist (id '("a01" "a02" "a03"))
            (with-temp-file (expand-file-name (concat id ".tex") source-dir)
              (insert "problem")))
          (make-directory (expand-file-name "a01/lean" problems-dir) t)
          (with-temp-file (expand-file-name "a01/informal-solution.md" problems-dir)
            (insert (make-string 120 ?i)))
          (with-temp-file (expand-file-name "a01/lean/Main.lean" problems-dir)
            (insert "by\n  sorry\n"))
          (make-directory (expand-file-name "a02/candidates/frame/lean" problems-dir) t)
          (with-temp-file (expand-file-name "a02/candidates/frame/lean/Main.lean" problems-dir)
            (insert "by\n  trivial\n"))
          (make-directory (expand-file-name "a03" problems-dir) t)
          (with-temp-file (expand-file-name "a03/informal-solution.md" problems-dir)
            (insert "too short"))
          (let ((stack-hud-apm-source-dir source-dir)
                (stack-hud-apm-problems-dir problems-dir))
            (should (equal (stack-hud--apm-scan)
                           '(:total 3 :informal 1
                             :lean-total 2 :lean-with-sorry 1
                             :lean-clean 1 :sorries 1)))))
      (delete-directory root t))))

(ert-deftest stack-hud-apm-burndown-uses-daily-snapshots-and-live-status ()
  (let ((log-dir (make-temp-file "stack-hud-apm-log-" t)))
    (unwind-protect
        (let ((stack-hud-log-dir log-dir)
              (stack-hud-apm-progress-path (expand-file-name "missing.jsonl" log-dir))
              (stack-hud-apm-burndown-days 14)
              (stack-hud-apm-cron-interval-minutes 15))
          (with-temp-file (expand-file-name "2026-07-12.jsonl" log-dir)
            (insert "{\"timestamp\":\"2026-07-12T23:00:00+0000\",\"stack\":{\"apm\":{\"total\":10,\"lean-clean\":2}}}\n"))
          (cl-letf (((symbol-function 'stack-hud--today-string)
                     (lambda () "2026-07-13"))
                    ((symbol-function 'current-time)
                     (lambda () (date-to-time "2026-07-13T12:00:00Z"))))
            (let* ((status '(:total 10 :informal 8 :lean-total 6
                                      :lean-with-sorry 1 :lean-clean 5 :sorries 2))
                   (history (stack-hud--apm-history status)))
              (should (equal (mapcar (lambda (sample)
                                       (plist-get sample :remaining))
                                     history)
                             '(8 5)))
              (should (equal (stack-hud--apm-sparkline '(8 5)) "█▁"))
              (with-temp-buffer
                (my-chatgpt-shell--insert-stack-apm status)
                (let ((text (buffer-string)))
                  (should (string-match-p "burn down  █▁  5 remaining" text))
                  (should (string-match-p "+3 clean in 1d (3.00/day)" text))
                  (should (string-match-p "gated start ceiling 4/hour" text)))))))
      (delete-directory log-dir t))))

(ert-deftest stack-hud-apm-burndown-does-not-invent-prehistory ()
  (let* ((stack-hud-log-dir (make-temp-file "stack-hud-apm-empty-" t))
         (stack-hud-apm-progress-path
          (expand-file-name "missing.jsonl" stack-hud-log-dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'stack-hud--today-string)
                   (lambda () "2026-07-13"))
                  ((symbol-function 'current-time)
                   (lambda () (date-to-time "2026-07-13T12:00:00Z"))))
          (with-temp-buffer
            (stack-hud--apm-insert-burndown '(:total 10 :lean-clean 4))
            (should (string-match-p "6 remaining | tracking starts today"
                                    (buffer-string)))))
      (delete-directory stack-hud-log-dir t))))

(ert-deftest stack-hud-apm-burndown-prefers-cron-observations ()
  (let* ((root (make-temp-file "stack-hud-apm-cron-" t))
         (stack-hud-log-dir root)
         (stack-hud-apm-progress-path (expand-file-name "progress.jsonl" root)))
    (unwind-protect
        (progn
          (with-temp-file stack-hud-apm-progress-path
            (insert "{\"schema\":\"apm-formal-progress.v1\",\"timestamp\":\"2026-07-12T22:00:00Z\",\"total\":10,\"lean_clean\":3}\n")
            (insert "{\"schema\":\"apm-formal-progress.v1\",\"timestamp\":\"2026-07-12T23:00:00Z\",\"total\":10,\"lean_clean\":4}\n"))
          (cl-letf (((symbol-function 'stack-hud--today-string)
                     (lambda () "2026-07-13"))
                    ((symbol-function 'current-time)
                     (lambda () (date-to-time "2026-07-13T12:00:00Z"))))
            (let ((history (stack-hud--apm-history
                            '(:total 10 :lean-clean 5))))
              (should (equal (mapcar (lambda (sample)
                                       (plist-get sample :lean-clean))
                                     history)
                             '(4 5))))))
      (delete-directory root t))))
