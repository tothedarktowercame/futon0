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
  (expand-file-name "~/code/futon0/scripts")
  "Directory containing the war machine babashka scripts."
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
                         "bb" "-cp" "." "-m" "futon0.report.war-machine"
                         (number-to-string d)))
    (set-process-sentinel war-machine--process #'war-machine--sentinel)))

;;;###autoload
(defun war-machine-refresh ()
  "Regenerate the war machine with the same window."
  (interactive)
  (war-machine war-machine-days))

(provide 'war-machine)
;;; war-machine.el ends here
