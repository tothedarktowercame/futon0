;;; stack-doc-test.el --- ERT tests for stack-doc -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'stack-doc)

(ert-deftest stack-doc-for-line ()
  (should (string-match-p "Stack Context HUD"
                          (stack-doc--stack-doc-for-line "Stack HUD")))
  (should (string-match-p "Hot reload"
                          (stack-doc--stack-doc-for-line "  Hot reload:")))
  (should (string-match-p "Voice typing"
                          (stack-doc--stack-doc-for-line "  Voice typing:")))
  (should (string-match-p "Boundary"
                          (stack-doc--stack-doc-for-line "  Boundary gaps:"))))
