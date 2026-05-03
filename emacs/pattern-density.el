;;; pattern-density.el --- Pattern retrieval density sparklines -*- lexical-binding: t; -*-

;; Description: Pattern-retrieval density over time — first INSTANTIATE
;;   demo for M-war-machine's visualizer slot. Renders sparklines of
;;   context-retrieval events + top patterns in a markdown buffer.
;;
;; Usage:
;;   (load "/home/joe/code/futon0/emacs/pattern-density.el")
;;   M-x pattern-density           ;; defaults: 14 days, top 15 patterns
;;   C-u M-x pattern-density       ;; prompt for days
;;
;; Data source: futon3c evidence API (default http://localhost:7070).

(defgroup pattern-density nil
  "Pattern retrieval density visualisation."
  :group 'tools)

(defcustom pattern-density-script-dir
  (expand-file-name "~/code/futon0/scripts")
  "Directory containing the pattern-density babashka script."
  :type 'directory
  :group 'pattern-density)

(defcustom pattern-density-days 14
  "Default lookback window in days."
  :type 'integer
  :group 'pattern-density)

(defcustom pattern-density-top-n 15
  "Default number of top patterns to show."
  :type 'integer
  :group 'pattern-density)

(defcustom pattern-density-buffer-name "*Pattern Density*"
  "Name of the display buffer."
  :type 'string
  :group 'pattern-density)

(defvar pattern-density--process nil)

(defun pattern-density--render (output)
  (let ((buf (get-buffer-create pattern-density-buffer-name)))
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

(defun pattern-density--sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (let ((output (with-current-buffer (process-buffer proc)
                    (buffer-string)))
          (exit-code (process-exit-status proc)))
      (if (zerop exit-code)
          (pattern-density--render output)
        (pattern-density--render
         (format "# Pattern Density — Error\n\nExit code: %d\n\n```\n%s\n```"
                 exit-code output)))
      (kill-buffer (process-buffer proc))
      (setq pattern-density--process nil))))

;;;###autoload
(defun pattern-density (&optional days top-n)
  "Render pattern retrieval density sparklines for the last DAYS days.
With prefix arg, prompt for DAYS. TOP-N controls the pattern count."
  (interactive
   (list (if current-prefix-arg
             (read-number "Lookback days: " pattern-density-days)
           pattern-density-days)
         pattern-density-top-n))
  (let* ((d (or days pattern-density-days))
         (n (or top-n pattern-density-top-n))
         (proc-buf (generate-new-buffer " *pattern-density-proc*"))
         (default-directory pattern-density-script-dir))
    (when (and pattern-density--process (process-live-p pattern-density--process))
      (kill-process pattern-density--process))
    (message "Pattern density: scanning (%d-day window, top %d)..." d n)
    (setq pattern-density--process
          (start-process "pattern-density" proc-buf
                         "bb" "-cp" "." "-m" "futon0.report.pattern-density"
                         (number-to-string d) (number-to-string n)))
    (set-process-sentinel pattern-density--process #'pattern-density--sentinel)))

(provide 'pattern-density)
;;; pattern-density.el ends here
