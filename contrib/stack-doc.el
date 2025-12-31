;;; stack-doc.el --- Stack HUD line docs -*- lexical-binding: t; -*-

;;; Commentary:
;; Stack HUD line documentation and popup behavior.

;;; Code:

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'futon-hud-windows)
(require 'stack-hud)

(defcustom my-chatgpt-shell-stack-doc-popup-delay 0.35
  "Seconds to wait before showing Stack Context line docs."
  :type 'number
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-stack-doc-popup-mode 'tooltip
  "How to show Stack Context line docs."
  :type '(choice (const tooltip) (const buffer) (const echo) (const none))
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-stack-doc-window-side 'right
  "Side used for the Stack Doc window when popup mode is `buffer`."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'tatami-integration)
(defconst my-chatgpt-shell-stack-doc-buffer-name "*Stack Doc*")
(defvar stack-doc--stack-doc-window nil)
(defvar-local stack-doc--stack-doc-last nil)
(defvar-local stack-doc--stack-doc-timer nil)
(defvar-local stack-doc--stack-last-state nil)
(defvar stack-doc--stack-vitality-config-cache nil)
(defvar stack-doc--stack-vitality-config-mtime nil)
(defvar stack-doc--stack-vitality-timer-cache nil)
(defvar stack-doc--stack-vitality-timer-mtime nil)

(defun stack-doc--stack-buffer (&optional ensure)
  (let ((buf (if ensure
                 (get-buffer-create my-chatgpt-shell-stack-buffer-name)
               (get-buffer my-chatgpt-shell-stack-buffer-name))))
    (when (and ensure buf)
      (with-current-buffer buf
        (visual-line-mode 1)
        (my-chatgpt-shell-stack-doc-mode 1)
        (setq-local truncate-lines nil)
        (local-set-key (kbd "q") #'stack-doc--stack-quit)
        (add-hook 'kill-buffer-hook #'stack-doc--stack-doc-close nil t)))
    buf))

(defun stack-doc--stack-doc-buffer ()
  (let ((buf (get-buffer-create my-chatgpt-shell-stack-doc-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (unless (derived-mode-p 'special-mode)
          (special-mode))
        (visual-line-mode 1)
        (setq-local truncate-lines nil)
        (setq-local cursor-type nil)))
    buf))

(defun stack-doc--stack-doc-for-line (line)
  (cond
   ((string-prefix-p "Stack HUD" line)
    "Overview of the Stack Context HUD and its current status.")
   ((string-prefix-p "  Hot reload:" line)
    "Hot reload status for Stack tooling. Toggle to enable/disable watchers.")
   ((string-prefix-p "  Voice typing:" line)
    "Voice typing status and controls for the Stack HUD.")
   ((string-prefix-p "  Pattern sync:" line)
    "Futon3→Futon1 pattern sync status, diff count, and sync action.")
   ((string-prefix-p "  Focus/Profile:" line)
    "Current focus anchors and profile summary from Tatami.")
   ((string-prefix-p "    Anchors:" line)
    "Anchors describe the current focus cluster.")
   ((string-prefix-p "    Neighbors:" line)
    "Neighbors are related focus nodes near the active anchors.")
   ((string-prefix-p "    Relations:" line)
    "Profile relations summarize connected topics and counts.")
   ((string-prefix-p "    Recent:" line)
    "Recent focus/profile events (last few entries).")
   ((string-prefix-p "  Vitality:" line)
    "Filesystem vitality plus Tatami activity recency.")
   ((string-prefix-p "    Tatami:" line)
    "Tatami log recency and gap warnings.")
   ((string-prefix-p "  Git:" line)
    "Git activity summary across tracked repos.")
   ((string-prefix-p "    Dominant sphere:" line)
    "Most active work sphere from recent commits.")
   ((string-prefix-p "    Streak:" line)
    "Current and longest activity streak.")
   ((string-prefix-p "    Quiet:" line)
    "Days since last git activity.")
   ((string-prefix-p "    Last commit:" line)
    "Most recent commit date and total count.")
   ((string-prefix-p "  Boundary gaps:" line)
    "Boundary evidence missing across futon repos.")
   ((string-prefix-p "  Boundary details:" line)
    "Lab/media counts for repos with boundary gaps.")
   ((string-prefix-p "  Reminders:" line)
    "Upcoming reminders and their urgency.")
   ((string-prefix-p "  Futon liveness:" line)
    "Per-futon freshness buckets (1=last 24h, 2=48h, …, 7=168h, 7+=older).")
   ((string-prefix-p "  ⚠️" line)
    "Warning emitted by Stack HUD checks.")
   (t nil)))

(defun stack-doc--stack-warning-doc (msg vitality)
  (cond
   ((string-prefix-p "No filesystem activity recorded in the last scan window" msg)
    (let ((lookback (stack-doc--stack-vitality-lookback vitality)))
      (if lookback
          (format "Scan window is the last %s hours. Configure lookback_hours in ~/code/storage/futon0/vitality/vitality_scanner.json." lookback)
        "Scan window duration is set by lookback_hours in ~/code/storage/futon0/vitality/vitality_scanner.json.")))
   ((string-prefix-p "No futon activity recorded in the last" msg)
    "No recent mtimes across futon0–futon7 within the 7-day window. The window is fixed at 168h in futon0.vitality.scanner.")
   ((string= msg "Tatami activity gap exceeds configured window.")
    (let* ((tatami (plist-get vitality :tatami))
           (gap (or (plist-get tatami :gap-warning)
                    (plist-get tatami :gap_warning)))
           (lookback (or (plist-get tatami :lookback-hours)
                         (plist-get tatami :lookback_hours)))
           (hours (or (plist-get tatami :hours-since)
                      (plist-get tatami :hours_since)
                      (plist-get tatami :hours_since_last))))
      (cond
       ((and hours lookback)
        (format "Tatami gap exceeds %s hours; last event %s hours ago. To clear, open M-x chatgpt-shell and send a turn so Tatami logs a session. Configure tatami.gap_warning_hours in ~/code/storage/futon0/vitality/vitality_scanner.json."
                lookback hours))
       (lookback
        (format "Tatami gap warning threshold is %s hours. To clear, open M-x chatgpt-shell and send a turn so Tatami logs a session. Configure tatami.gap_warning_hours in ~/code/storage/futon0/vitality/vitality_scanner.json." lookback))
       (t "Tatami gap warning threshold is set in ~/code/storage/futon0/vitality/vitality_scanner.json. To clear, open M-x chatgpt-shell and send a turn so Tatami logs a session."))))
   (t nil)))

(defun stack-doc--stack-vitality-lookback (vitality)
  (or (plist-get vitality :lookback-hours)
      (plist-get vitality :lookback_hours)
      (plist-get (plist-get vitality :tatami) :lookback-hours)
      (plist-get (plist-get vitality :tatami) :lookback_hours)
      (stack-doc--stack-vitality-lookback-from-file)))

(defun stack-doc--stack-vitality-lookback-from-file ()
  (let ((path "/home/joe/code/storage/futon0/vitality/vitality_scanner.json"))
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (condition-case nil
            (let* ((json-object-type 'plist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (data (if (fboundp 'json-parse-buffer)
                             (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil :false-object nil)
                           (json-read))))
              (plist-get data :lookback_hours))
          (error nil))))))

(defun stack-doc--stack-vitality-config ()
  (let* ((path "/home/joe/code/storage/futon0/vitality/vitality_scanner.json")
         (mtime (nth 5 (file-attributes path))))
    (if (and stack-doc--stack-vitality-config-cache
             (equal mtime stack-doc--stack-vitality-config-mtime))
        stack-doc--stack-vitality-config-cache
      (when (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (condition-case nil
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (json-key-type 'symbol)
                     (data (if (fboundp 'json-parse-buffer)
                               (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil :false-object nil)
                             (json-read))))
                (setq stack-doc--stack-vitality-config-cache data)
                (setq stack-doc--stack-vitality-config-mtime mtime)
                data)
            (error nil)))))))

(defun stack-doc--stack-vitality-filesystem-doc (label)
  (when-let* ((config (stack-doc--stack-vitality-config))
              (entries (plist-get config :filesystem))
              (entry (seq-find (lambda (item)
                                 (string= (plist-get item :label) label))
                               entries)))
    (let* ((path (plist-get entry :path))
           (max-depth (plist-get entry :max_depth))
           (top-n (plist-get entry :top_n))
           (import-index (plist-get entry :import_index))
           (import-limit (plist-get entry :import_limit))
           (lookback (plist-get config :lookback_hours))
           (bits (delq nil
                       (list
                        (when path (format "Filesystem label '%s' scans %s." label path))
                        (when max-depth (format "Max depth: %s." max-depth))
                        (when top-n (format "Top active child folders: %s (by recent file mtimes)." top-n))
                        (when import-index (format "Import index: %s." import-index))
                        (when import-limit (format "Import limit: %s." import-limit))
                        (when lookback (format "Scan window: %s hours." lookback))))))
      (when bits
        (string-join bits " ")))))

(defun stack-doc--stack-vitality-header-doc ()
  (let* ((config (stack-doc--stack-vitality-config))
         (lookback (or (plist-get config :lookback_hours)
                       (stack-doc--stack-vitality-lookback
                        (plist-get stack-doc--stack-last-state :vitality))))
         (entries (length (plist-get config :filesystem)))
         (generated-at (plist-get (plist-get stack-doc--stack-last-state :vitality) :generated-at))
         (schedule (stack-doc--stack-vitality-timer-schedule)))
    (cond
     ((and lookback entries)
      (format "Filesystem + Tatami vitality summary. %s filesystem entries; scan window: %s hours. Last scan: %s. %s. Config: ~/code/storage/futon0/vitality/vitality_scanner.json."
              entries lookback (or generated-at "unknown") schedule))
     (lookback
      (format "Filesystem + Tatami vitality summary. Scan window: %s hours. Last scan: %s. %s. Config: ~/code/storage/futon0/vitality/vitality_scanner.json."
              lookback (or generated-at "unknown") schedule))
     (t (format "Filesystem + Tatami vitality summary from the latest scan. Last scan: %s. %s. Config: ~/code/storage/futon0/vitality/vitality_scanner.json."
                (or generated-at "unknown") schedule)))))

(defun stack-doc--stack-vitality-timer-schedule ()
  (let ((path "/home/joe/code/futon0/systemd/user-vitality-scanner.timer")
        (path-unit "/home/joe/code/futon0/systemd/user-vitality-scanner.path"))
    (if (file-readable-p path)
        (let* ((mtime (nth 5 (file-attributes path)))
               (cached (and stack-doc--stack-vitality-timer-cache
                            (equal mtime stack-doc--stack-vitality-timer-mtime)
                            stack-doc--stack-vitality-timer-cache)))
          (or cached
              (with-temp-buffer
                (insert-file-contents path)
                (goto-char (point-min))
                (if (re-search-forward "^OnCalendar=\\(.+\\)$" nil t)
                    (let ((value (match-string 1)))
                      (setq stack-doc--stack-vitality-timer-cache
                            (if (file-exists-p path-unit)
                                (format "Schedule: %s (timer) + path trigger (optional)" value)
                              (format "Schedule: %s (timer)" value)))
                      (setq stack-doc--stack-vitality-timer-mtime mtime)
                      stack-doc--stack-vitality-timer-cache)
                  "Schedule: unknown (timer)"))))
      "Schedule: unknown (timer)")))

(defun stack-doc--stack-vitality-tatami-doc ()
  (when-let* ((config (stack-doc--stack-vitality-config))
              (tatami (plist-get config :tatami)))
    (let* ((path (plist-get tatami :log_path))
           (format (plist-get tatami :format))
           (gap (plist-get tatami :gap_warning_hours))
           (bits (delq nil
                       (list
                        (when path (format "Tatami log path: %s." path))
                        (when format (format "Format: %s." format))
                        (when gap (format "Gap warning: %s hours." gap))))))
      (when bits
        (string-join bits " ")))))

(defun stack-doc--stack-apply-doc-properties ()
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol))
               (doc (stack-doc--stack-doc-for-line line))
               (warn-msg (when (string-prefix-p "  ⚠" line)
                           (string-trim (replace-regexp-in-string "^\\s-*⚠\\(?:️\\)?\\s-+" "" line))))
               (warn-doc (when warn-msg
                           (stack-doc--stack-warning-doc
                            warn-msg
                            (plist-get stack-doc--stack-last-state :vitality))))
               (vitality-doc
                (cond
                 ((string-prefix-p "  Vitality:" line)
                  (stack-doc--stack-vitality-header-doc))
                 ((string-prefix-p "    Tatami:" line)
                  (stack-doc--stack-vitality-tatami-doc))
                 ((string-match "^\\s-+\\([^:]+\\):" line)
                  (stack-doc--stack-vitality-filesystem-doc (match-string 1 line)))))
               (existing (get-text-property bol 'stack-doc)))
          (cond
           (warn-doc
            (add-text-properties bol eol `(stack-doc ,warn-doc)))
           (vitality-doc
            (add-text-properties bol eol `(stack-doc ,vitality-doc)))
           ((and doc (not existing))
            (add-text-properties bol eol `(stack-doc ,doc)))))
        (forward-line 1)))))

(defun stack-doc--stack-doc-clear ()
  (when (and (eq my-chatgpt-shell-stack-doc-popup-mode 'tooltip)
             (fboundp 'tooltip-hide))
    (tooltip-hide))
  (when (and (eq my-chatgpt-shell-stack-doc-popup-mode 'buffer)
             (buffer-live-p (get-buffer my-chatgpt-shell-stack-doc-buffer-name)))
    (with-current-buffer my-chatgpt-shell-stack-doc-buffer-name
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun stack-doc--stack-doc-close ()
  "Hide any Stack Doc windows and clear their contents."
  (stack-doc--stack-doc-clear)
  (dolist (win (get-buffer-window-list my-chatgpt-shell-stack-doc-buffer-name nil t))
    (when (window-live-p win)
      (delete-window win)))
  (setq stack-doc--stack-doc-window nil))

(defun stack-doc--stack-quit ()
  "Quit the Stack Context buffer and close the Stack Doc mini-HUD."
  (interactive)
  (stack-doc--stack-doc-close)
  (quit-window t))

(defun stack-doc--stack-doc-show-buffer (doc)
  (let* ((buf (stack-doc--stack-doc-buffer))
         (stack-win (get-buffer-window my-chatgpt-shell-stack-buffer-name t))
         (frame (when (window-live-p stack-win) (window-frame stack-win)))
         (win (display-buffer-in-side-window
               buf
               `((side . ,my-chatgpt-shell-stack-doc-window-side)
                 (window-width . 0.33)
                 ,@(when frame `((frame . ,frame)))))))
    (unless (window-live-p win)
      (setq win (display-buffer
                 buf
                 (append '(display-buffer-pop-up-window)
                         (when frame `((frame . ,frame)))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (replace-regexp-in-string "\\. " ".\n" doc))
        (goto-char (point-min))))
    (when (window-live-p win)
      (set-window-parameter win 'stack-doc-owner t)
      (setq stack-doc--stack-doc-window win))))

(defun stack-doc--stack-doc-show (doc)
  (pcase my-chatgpt-shell-stack-doc-popup-mode
    ('tooltip (if (display-graphic-p)
                  (tooltip-show doc)
                (message "%s" doc)))
    ('buffer (stack-doc--stack-doc-show-buffer doc))
    ('echo (message "%s" doc))
    (_ nil)))

(defun stack-doc--stack-doc-post-command ()
  (let ((doc (get-text-property (point) 'stack-doc)))
    (unless (equal doc stack-doc--stack-doc-last)
      (setq stack-doc--stack-doc-last doc)
      (when stack-doc--stack-doc-timer
        (cancel-timer stack-doc--stack-doc-timer)
        (setq stack-doc--stack-doc-timer nil))
      (if doc
          (if (eq my-chatgpt-shell-stack-doc-popup-mode 'buffer)
              (stack-doc--stack-doc-show doc)
            (setq stack-doc--stack-doc-timer
                  (run-with-idle-timer
                   my-chatgpt-shell-stack-doc-popup-delay
                   nil
                   #'stack-doc--stack-doc-show
                   doc)))
        (stack-doc--stack-doc-clear)))))

(define-minor-mode my-chatgpt-shell-stack-doc-mode
  "Show line docs for Stack Context buffer."
  :lighter " StackDoc"
  (if my-chatgpt-shell-stack-doc-mode
      (add-hook 'post-command-hook #'stack-doc--stack-doc-post-command nil t)
    (remove-hook 'post-command-hook #'stack-doc--stack-doc-post-command t)
    (stack-doc--stack-doc-clear)))

(defun my-chatgpt-shell-stack-doc-at-point ()
  "Return a (LINE . DOC) pair for the current Stack Context line, or nil."
  (when-let ((buf (get-buffer my-chatgpt-shell-stack-buffer-name)))
    (with-current-buffer buf
      (save-excursion
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol))
               (doc (get-text-property (point) 'stack-doc)))
          (when doc
            (cons line doc)))))))

(defun my-chatgpt-shell-stack-doc-dump ()
  "Return a list of (LINE . DOC) pairs for Stack Context."
  (when-let ((buf (get-buffer my-chatgpt-shell-stack-buffer-name)))
    (with-current-buffer buf
      (stack-doc--stack-apply-doc-properties)
      (save-excursion
        (goto-char (point-min))
        (let (rows)
          (while (not (eobp))
            (let* ((bol (line-beginning-position))
                   (eol (line-end-position))
                   (line (buffer-substring-no-properties bol eol))
                   (doc (get-text-property bol 'stack-doc)))
              (when doc
                (push (cons line doc) rows)))
            (forward-line 1))
          (nreverse rows))))))

(provide 'stack-doc)

;;; stack-doc.el ends here
