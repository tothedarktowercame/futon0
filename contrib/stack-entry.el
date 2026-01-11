;;; stack-entry.el --- Stack HUD entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone entry point for rendering the Stack HUD outside chatgpt-shell.

;;; Code:

(require 'stack-render)
(require 'stack-hud)

(defconst futon0--futon3-bridge-path
  "/home/joe/code/futon3/contrib/futon3-bridge.el"
  "Path to the Futon3 bridge helpers used to fetch Stack HUD status.")
(defconst stack-hud--diagnostic-buffer-name "*Stack HUD Diagnostics*")
(defcustom stack-hud-diagnostics-log-path
  (expand-file-name "stack-hud-diagnostics.log" stack-hud-log-dir)
  "Path where non-interactive Stack HUD diagnostics are written."
  :type 'file
  :group 'tatami-integration)
(defcustom stack-hud-diagnostic-timeout-seconds 10
  "Seconds before a Stack HUD diagnostic step is considered stalled."
  :type 'integer
  :group 'tatami-integration)
(defcustom stack-hud-diagnostics-fast nil
  "When non-nil, skip slower diagnostics (like pattern sync)."
  :type 'boolean
  :group 'tatami-integration)
(defcustom stack-hud-diagnostics-trace-path
  (expand-file-name "stack-hud-diagnostics.trace" stack-hud-log-dir)
  "Path where Stack HUD diagnostics traces are appended."
  :type 'file
  :group 'tatami-integration)

(defvar stack-hud--diagnostic-buffer nil)

(defun stack-hud--diagnostic-log (line)
  (when (and stack-hud-diagnostics-trace-path
             (stringp stack-hud-diagnostics-trace-path))
    (make-directory (file-name-directory stack-hud-diagnostics-trace-path) t)
    (with-temp-buffer
      (insert (format "[%s] %s\n" (format-time-string "%Y-%m-%dT%H:%M:%S%z") line))
      (append-to-file (point-min) (point-max) stack-hud-diagnostics-trace-path))))

(defun stack-hud--diagnostic-render-component (fn stack)
  (with-temp-buffer
    (unless (fboundp fn)
      (user-error "Function %s unavailable" fn))
    (pcase fn
      ('my-chatgpt-shell--insert-stack-futon-liveness
       (funcall fn (plist-get stack :vitality)))
      ('my-chatgpt-shell--insert-stack-focus-profile
       (funcall fn (plist-get stack :focus-profile)))
      ('my-chatgpt-shell--insert-stack-vitality
       (funcall fn (plist-get stack :vitality)))
      ('my-chatgpt-shell--insert-stack-git
       (funcall fn (plist-get stack :git)))
      ('my-chatgpt-shell--insert-stack-boundary
       (funcall fn (plist-get stack :boundary)))
      ('my-chatgpt-shell--insert-stack-reminders
       (funcall fn (plist-get stack :reminders)))
      (_
       (funcall fn)))
    (buffer-string)))

(defun stack-hud--diagnostic-time (label thunk &optional timeout)
  (let ((start (float-time))
        (value nil)
        (err nil))
    (stack-hud--diagnostic-log (format "START %s" label))
    (condition-case e
        (if (numberp timeout)
            (with-timeout (timeout (error "Timeout after %.1fs" timeout))
              (setq value (funcall thunk)))
          (setq value (funcall thunk)))
      (error (setq err e))
      (quit (setq err e)))
    (stack-hud--diagnostic-log
     (format "END %s (%.2fs)%s"
             label
             (- (float-time) start)
             (if err (format " ERROR=%s" (error-message-string err)) "")))
    (list :label label
          :elapsed (- (float-time) start)
          :value value
          :error err)))

(defun stack-hud--diagnostic-insert (entry)
  (let ((label (plist-get entry :label))
        (elapsed (plist-get entry :elapsed))
        (err (plist-get entry :error)))
    (with-current-buffer (or stack-hud--diagnostic-buffer (current-buffer))
      (insert (format "%-28s %7.2fs" label (or elapsed 0.0)))
      (when err
        (insert (format "  ERROR: %s" (error-message-string err))))
      (insert "\n"))))

(defun stack-hud--diagnostic-report ()
  (let ((status nil))
    (with-temp-buffer
      (let ((stack-hud--diagnostic-buffer (current-buffer)))
        (insert "Stack HUD diagnostics\n\n")
        (stack-hud--diagnostic-insert
         (stack-hud--diagnostic-time
          "Load futon3 bridge"
          (lambda ()
            (unless (boundp 'my-tatami--clojure)
              (defvar my-tatami--clojure (or (executable-find "clojure") "clojure")))
            (unless (featurep 'futon3-bridge)
              (if (file-readable-p futon0--futon3-bridge-path)
                  (load-file futon0--futon3-bridge-path)
                (user-error "Futon3 bridge not found at %s" futon0--futon3-bridge-path))))
          stack-hud-diagnostic-timeout-seconds))
        (stack-hud--diagnostic-insert
         (stack-hud--diagnostic-time
          "Ensure futon3 running"
          (lambda ()
            (if (fboundp 'my-futon3-ensure-running)
                (my-futon3-ensure-running)
              (user-error "my-futon3-ensure-running is unavailable")))
          stack-hud-diagnostic-timeout-seconds))
        (stack-hud--diagnostic-insert
         (stack-hud--diagnostic-time
          "Boundary scan"
          #'stack-hud--maybe-refresh-boundary-scan
          stack-hud-diagnostic-timeout-seconds))
        (stack-hud--diagnostic-insert
         (stack-hud--diagnostic-time
          "Vitality scan"
          #'stack-hud--maybe-refresh-vitality-scan
          stack-hud-diagnostic-timeout-seconds))
        (let ((entry (stack-hud--diagnostic-time
                      "Refresh futon3 status"
                      (lambda ()
                        (if (fboundp 'my-futon3-refresh-status)
                            (my-futon3-refresh-status)
                          (user-error "my-futon3-refresh-status is unavailable")))
                      stack-hud-diagnostic-timeout-seconds)))
          (stack-hud--diagnostic-insert entry)
          (setq status (plist-get entry :value)))
        (let* ((stack (and status (plist-get status :stack)))
               (entry (stack-hud--diagnostic-time
                       "Render HUD string"
                       (lambda ()
                         (when stack
                           (let ((stack-hud-disable-pattern-sync stack-hud-diagnostics-fast))
                             (my-chatgpt-shell--stack-hud-string stack))))
                       stack-hud-diagnostic-timeout-seconds)))
          (stack-hud--diagnostic-insert entry)
          (unless stack
            (insert "Render HUD string         skipped (no stack data)\n")))
        (when (and status (plist-get status :stack))
          (insert "\nRender components\n")
          (let* ((stack (plist-get status :stack))
                 (specs '(("Futon liveness" . my-chatgpt-shell--insert-stack-futon-liveness)
                          ("Hot reload" . my-chatgpt-shell--insert-stack-hot-reload)
                          ("Voice typing" . my-chatgpt-shell--insert-stack-voice)
                          ("Pattern sync" . my-chatgpt-shell--insert-stack-pattern-sync)
                          ("Focus/profile" . my-chatgpt-shell--insert-stack-focus-profile)
                          ("Vitality" . my-chatgpt-shell--insert-stack-vitality)
                          ("Git" . my-chatgpt-shell--insert-stack-git)
                          ("Boundary" . my-chatgpt-shell--insert-stack-boundary)
                          ("Reminders" . my-chatgpt-shell--insert-stack-reminders)))
                 (specs (if stack-hud-diagnostics-fast
                            (seq-remove (lambda (spec)
                                          (string= (car spec) "Pattern sync"))
                                        specs)
                          specs)))
            (dolist (spec specs)
              (let* ((label (car spec))
                     (fn (cdr spec))
                     (entry (stack-hud--diagnostic-time
                             label
                             (lambda ()
                               (stack-hud--diagnostic-render-component fn stack))
                             stack-hud-diagnostic-timeout-seconds)))
                (stack-hud--diagnostic-insert entry)))))
        (if stack-hud-diagnostics-fast
            (insert "Pattern sync status        skipped (fast diagnostics)\n")
          (let ((entry (stack-hud--diagnostic-time
                        "Pattern sync status"
                        #'stack-hud--pattern-sync-status
                        stack-hud-diagnostic-timeout-seconds)))
            (stack-hud--diagnostic-insert entry)
            (let* ((value (plist-get entry :value))
                   (reachable (plist-get value :reachable))
                   (diff (plist-get value :diff))
                   (total (plist-get diff :total)))
              (when value
                (insert (format "Pattern sync detail      reachable=%s diff=%s\n"
                                reachable
                                (if (numberp total) total "n/a")))))))
        (insert "\nVoice typing\n")
        (let ((configured (my-chatgpt-shell--voice-command-configured-p))
              (running (my-chatgpt-shell-voice-running-p))
              (ydotoold (and my-chatgpt-shell--ydotoold-process
                             (process-live-p my-chatgpt-shell--ydotoold-process))))
          (insert (format "  Configured: %s\n" (if configured "yes" "no")))
          (insert (format "  Running: %s\n" (if running "yes" "no")))
          (insert (format "  ydotoold: %s\n" (if ydotoold "yes" "no")))
          (when configured
            (insert (format "  Command: %s\n" my-chatgpt-shell-voice-command))))
        (when-let ((buf (get-buffer my-chatgpt-shell-voice-buffer-name)))
          (insert (format "  Buffer: %s\n" my-chatgpt-shell-voice-buffer-name))
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-max))
              (forward-line -1)
              (let ((line (string-trim (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))))
                (when (not (string-empty-p line))
                  (insert (format "  Last line: %s\n" line)))))))
        (buffer-string)))))

(defun stack-hud-diagnostics ()
  "Run Stack HUD components independently and report timings."
  (interactive)
  (let ((buf (get-buffer-create stack-hud--diagnostic-buffer-name))
        (report (stack-hud--diagnostic-report)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert report)
        (special-mode)))
    (display-buffer buf)))

(defun stack-hud-diagnostics-to-file ()
  "Run Stack HUD diagnostics and write the report to disk."
  (interactive)
  (let ((report (stack-hud--diagnostic-report)))
    (make-directory (file-name-directory stack-hud-diagnostics-log-path) t)
    (with-temp-file stack-hud-diagnostics-log-path
      (insert report))
    (message "Stack HUD diagnostics written to %s" stack-hud-diagnostics-log-path)))

(defun stack-hud-diagnostics-batch ()
  "Batch entry point for Stack HUD diagnostics."
  (let ((report (stack-hud--diagnostic-report)))
    (make-directory (file-name-directory stack-hud-diagnostics-log-path) t)
    (with-temp-file stack-hud-diagnostics-log-path
      (insert report))
    (princ report)))

(defun stack-hud ()
  "Fetch Stack HUD state from Futon3 and render the Stack HUD."
  (interactive)
  (unless (featurep 'futon3-bridge)
    (if (file-readable-p futon0--futon3-bridge-path)
        (load-file futon0--futon3-bridge-path)
      (user-error "Futon3 bridge not found at %s" futon0--futon3-bridge-path)))
  (my-futon3-ensure-running)
  (let* ((boundary-updated (stack-hud--maybe-refresh-boundary-scan))
         (status (or (my-futon3-refresh-status) my-futon3-last-status))
         (status (if boundary-updated
                     (or (my-futon3-refresh-status) status)
                   status))
         (stack (and status (plist-get status :stack))))
    (if stack
        (progn
          (stack-hud--render-context stack)
          (stack-hud-log-snapshot stack)
          (when-let ((win (get-buffer-window my-chatgpt-shell-stack-buffer-name t)))
            (select-window win)
            (raise-frame (window-frame win))))
      (message "No Stack HUD data returned from Futon3."))))

(provide 'stack-entry)

;;; stack-entry.el ends here
