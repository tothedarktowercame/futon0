;;; voice-typing-test.el --- ERT tests for voice-typing -*- lexical-binding: t; -*-

(require 'ert)

(add-to-list 'load-path (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(require 'voice-typing)

(ert-deftest voice-typing-command-list-string ()
  (let ((my-chatgpt-shell-voice-command "echo hi"))
    (should (equal (my-chatgpt-shell--voice-command-list)
                   (list shell-file-name shell-command-switch "echo hi")))))

(ert-deftest voice-typing-command-list-vector ()
  (let ((my-chatgpt-shell-voice-command '("/bin/echo" "hi")))
    (should (equal (my-chatgpt-shell--voice-command-list)
                   '("/bin/echo" "hi")))))

(ert-deftest voice-typing-command-configured-p ()
  (let ((my-chatgpt-shell-voice-command "echo hi"))
    (should (my-chatgpt-shell--voice-command-configured-p)))
  (let ((my-chatgpt-shell-voice-command ""))
    (should-not (my-chatgpt-shell--voice-command-configured-p))))

