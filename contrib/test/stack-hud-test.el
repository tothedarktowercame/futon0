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
