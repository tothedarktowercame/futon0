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
