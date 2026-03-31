;;; joe-hud.el --- Behavioral HUD for Joe -*- lexical-binding: t; -*-

;; Author: Claude + Joe
;; Description: On-demand personal HUD showing work schedule, evidence
;;   discipline, stack breadth, and creative workflow signals.
;;   Runs bb script on Linode, renders markdown in a read-only buffer.
;;
;; Usage:
;;   (load "/home/joe/code/futon0/emacs/joe-hud.el")
;;   M-x joe-hud

;;; Configuration

(defgroup joe-hud nil
  "Joe's behavioral HUD."
  :group 'tools)

(defcustom joe-hud-script-dir
  (expand-file-name "~/code/futon0/scripts")
  "Directory containing the HUD babashka scripts."
  :type 'directory
  :group 'joe-hud)

(defcustom joe-hud-days 14
  "Default lookback window in days."
  :type 'integer
  :group 'joe-hud)

(defcustom joe-hud-buffer-name "*Joe HUD*"
  "Name of the HUD display buffer."
  :type 'string
  :group 'joe-hud)

;;; Core

(defvar joe-hud--process nil
  "Running HUD process, if any.")

(defun joe-hud--render-buffer (output)
  "Display OUTPUT in the HUD buffer with markdown fontification."
  (let ((buf (get-buffer-create joe-hud-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert output)
        (goto-char (point-min))
        ;; Use markdown-mode if available, otherwise fundamental
        (if (fboundp 'markdown-view-mode)
            (markdown-view-mode)
          (when (fboundp 'markdown-mode)
            (markdown-mode))
          (setq buffer-read-only t))
        (setq buffer-read-only t)))
    (display-buffer buf '(display-buffer-reuse-window))))

(defun joe-hud--sentinel (proc event)
  "Handle process completion."
  (when (memq (process-status proc) '(exit signal))
    (let ((output (with-current-buffer (process-buffer proc)
                    (buffer-string)))
          (exit-code (process-exit-status proc)))
      (if (zerop exit-code)
          (joe-hud--render-buffer output)
        (joe-hud--render-buffer
         (format "# Joe HUD — Error\n\nExit code: %d\n\n```\n%s\n```" exit-code output)))
      (kill-buffer (process-buffer proc))
      (setq joe-hud--process nil))))

;;;###autoload
(defun joe-hud (&optional days)
  "Generate and display the Joe HUD.
With prefix arg DAYS, override the lookback window."
  (interactive "P")
  (let* ((d (or days joe-hud-days))
         (proc-buf (generate-new-buffer " *joe-hud-proc*"))
         (default-directory joe-hud-script-dir))
    (when (and joe-hud--process (process-live-p joe-hud--process))
      (kill-process joe-hud--process))
    (message "Generating Joe HUD (%d-day window)..." d)
    (setq joe-hud--process
          (start-process "joe-hud" proc-buf
                         "bb" "-cp" "." "joe-hud.clj"
                         (number-to-string d)))
    (set-process-sentinel joe-hud--process #'joe-hud--sentinel)))

;;;###autoload
(defun joe-hud-refresh ()
  "Regenerate the HUD with the same window."
  (interactive)
  (joe-hud joe-hud-days))

(provide 'joe-hud)
;;; joe-hud.el ends here
