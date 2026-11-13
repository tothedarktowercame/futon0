;;; code-admit.el --- Joe's sign-off on additions to ~/code, from Emacs  -*- lexical-binding: t; -*-

;; M-x code-admit, then type what you would type after `code-admit' in a shell:
;;   futon8                      clone tothedarktowercame/futon8 and admit it
;;   clone <git-url> [name]      clone any repo and admit it
;;   add <name> [note]           admit an entry made by hand
;;   remove <name> [dest]        move an entry out
;;   check | lock | unlock
;; Output goes to the *code-admit* buffer.  The script and allowlist live beside this file
;; (futon0/system/zone/code-allowlist/; README.md there).

(defvar code-admit-script
  (expand-file-name "code-admit"
                    (file-name-directory (or load-file-name buffer-file-name
                                             "~/code/futon0/system/zone/code-allowlist/")))
  "Path to the code-admit script.")

(defun code-admit (args)
  "Run code-admit with ARGS (a string, split on spaces), showing output in *code-admit*."
  (interactive (list (read-string "code-admit " nil 'code-admit-history)))
  (let ((buf (get-buffer-create "*code-admit*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n$ code-admit %s\n" args)))
    (let ((proc (apply #'start-process "code-admit" buf code-admit-script
                       (split-string args " " t))))
      (set-process-sentinel
       proc (lambda (p _e)
              (when (memq (process-status p) '(exit signal))
                (message "code-admit: %s (exit %d); see *code-admit*"
                         args (process-exit-status p))))))
    (display-buffer buf)))

(provide 'code-admit)
;;; code-admit.el ends here
