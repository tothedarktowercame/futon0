;;; war-machine.el --- Strategic synthesis display -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: On-demand war machine strategic synthesis.
;;   Runs bb script, renders markdown in a read-only buffer.
;;   Four views available in the Swing visualiser (clojure -M:war-machine).
;;
;; Usage:
;;   (load "/home/joe/code/futon0/emacs/war-machine.el")
;;   M-x war-machine

;;; Configuration

(defgroup war-machine nil
  "War Machine strategic synthesis display."
  :group 'tools)

(defcustom war-machine-script-dir
  (expand-file-name "~/code/futon2")
  "Directory the war machine babashka scripts run from.

Must be the futon2 project root (NOT futon2/scripts) so the relative
classpath in `war-machine' (`-cp src:scripts:resources:.') resolves to
the right paths. The futon2.report.war-machine namespace requires
futon2.aif.* namespaces which live under src/; if cwd is futon2/scripts/
those won't be on bb's classpath and the load fails with `Could not
locate futon2/aif/action_proposer.clj' (operator-surfaced 2026-05-24)."
  :type 'directory
  :group 'war-machine)

(defcustom war-machine-days 14
  "Default lookback window in days."
  :type 'integer
  :group 'war-machine)

(defcustom war-machine-buffer-name "*War Machine*"
  "Name of the display buffer."
  :type 'string
  :group 'war-machine)

;;; Internal

(defvar war-machine--process nil
  "Current war-machine background process.")

(defun war-machine--render-buffer (output)
  "Display OUTPUT in the war machine buffer with markdown fontification."
  (let ((buf (get-buffer-create war-machine-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        (when (fboundp 'markdown-mode)
          (markdown-mode))
        (if (fboundp 'markdown-view-mode)
            (markdown-view-mode)
          (setq buffer-read-only t))))
    (display-buffer buf '(display-buffer-reuse-window))))

(defun war-machine--sentinel (proc event)
  "Handle process completion."
  (when (memq (process-status proc) '(exit signal))
    (let ((output (with-current-buffer (process-buffer proc)
                    (buffer-string)))
          (exit-code (process-exit-status proc)))
      (if (zerop exit-code)
          (war-machine--render-buffer output)
        (war-machine--render-buffer
         (format "# War Machine — Error\n\nExit code: %d\n\n```\n%s\n```"
                 exit-code output)))
      (kill-buffer (process-buffer proc))
      (setq war-machine--process nil))))

;;;###autoload
(defun war-machine (&optional days)
  "Generate and display the War Machine strategic synthesis.
With prefix arg DAYS, override the lookback window."
  (interactive "P")
  (let* ((d (or days war-machine-days))
         (proc-buf (generate-new-buffer " *war-machine-proc*"))
         (default-directory war-machine-script-dir))
    (when (and war-machine--process (process-live-p war-machine--process))
      (kill-process war-machine--process))
    (message "War Machine: scanning (%d-day window)..." d)
    (setq war-machine--process
          (start-process "war-machine" proc-buf
                         "bb" "-cp" "src:scripts:resources:." "-m" "futon2.report.war-machine"
                         (number-to-string d)))
    (set-process-sentinel war-machine--process #'war-machine--sentinel)))

;;;###autoload
(defun war-machine-refresh ()
  "Regenerate the war machine with the same window."
  (interactive)
  (war-machine war-machine-days))

;;; NAGs view — the operator's actionable queue (web UI stays "for info")

(defcustom war-machine-nags-script
  (expand-file-name "~/code/futon3c/scripts/wm-nags.bb")
  "Babashka helper that lists/dismisses WM needs-you NAGs."
  :type 'file
  :group 'war-machine)

(defcustom war-machine-nags-buffer-name "*WM NAGs*"
  "Name of the NAGs display buffer."
  :type 'string
  :group 'war-machine)

(defvar war-machine-nags-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "d") #'war-machine-nags-dismiss)
    (define-key m (kbd "g") #'war-machine-nags)
    (define-key m (kbd "q") #'quit-window)
    m)
  "Keymap for `war-machine-nags-mode'.")

(define-derived-mode war-machine-nags-mode special-mode "WM-NAGs"
  "Major mode for viewing and clearing War Machine pattern-warranted NAGs.
\\{war-machine-nags-mode-map}")

(defun war-machine--nags-fetch ()
  "Run the bb list helper and return the parsed list of NAG plists."
  (with-temp-buffer
    (ignore-errors
      (call-process "bb" nil t nil war-machine-nags-script "list"))
    (goto-char (point-min))
    (condition-case nil (read (current-buffer)) (error nil))))

;;;###autoload
(defun war-machine-nags ()
  "View the War Machine's pattern-warranted NAGs — the operator's queue.
Each NAG cites the design pattern (your own rule) that makes it
operator-required, plus the one gap to fill.  Press \\<war-machine-nags-mode-map>\\[war-machine-nags-dismiss] to
clear the NAG at point, \\[war-machine-nags] to refresh."
  (interactive)
  (let ((nags (war-machine--nags-fetch))
        (buf (get-buffer-create war-machine-nags-buffer-name)))
    (with-current-buffer buf
      (war-machine-nags-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "War Machine — NAGs (%d)\n" (length nags))
                            'face 'bold))
        (insert "d: dismiss at point   g: refresh   q: quit\n\n")
        (if (null nags)
            (insert "  ✓ queue clear — no NAGs.\n")
          (let ((i 0))
            (dolist (n nags)
              (setq i (1+ i))
              (let ((start (point)))
                (insert (format "[%d] %s\n" i (plist-get n :title)))
                (insert (format "     ⊢ %s — %s\n"
                                (plist-get n :pattern) (plist-get n :warrant)))
                (insert (format "     → %s\n\n" (plist-get n :gap)))
                (put-text-property start (point) 'wm-nag-id (plist-get n :id))))))
        (goto-char (point-min))))
    (display-buffer buf '(display-buffer-reuse-window))))

(defun war-machine-nags-dismiss ()
  "Dismiss (clear) the NAG at point from the needs-you queue.
Transient: a later WM run re-emits any NAG whose underlying gap is
still unresolved (e.g. a 0-hole mission still has no articulated hole)."
  (interactive)
  (let ((id (get-text-property (point) 'wm-nag-id)))
    (if (not id)
        (message "No NAG at point.")
      (ignore-errors
        (call-process "bb" nil nil nil war-machine-nags-script "dismiss" id))
      (message "Dismissed NAG: %s" id)
      (war-machine-nags))))

(provide 'war-machine)
;;; war-machine.el ends here
