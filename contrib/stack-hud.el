;;; stack-hud.el --- Stack HUD definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; Stack HUD and Stack Context configuration for Futon/ChatGPT integration.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'seq)
(require 'subr-x)
(require 'voice-typing)

(defconst my-chatgpt-shell-stack-buffer-name "*Stack Context*")
(defcustom my-chatgpt-shell-stack-window-side 'left
  "Preferred side for the Stack Context window.
The value is passed to `display-buffer-in-side-window'."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-stack-frame-name "Stack HUD"
  "Name of the dedicated Stack HUD frame."
  :type 'string
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-stack-frame-fullscreen t
  "When non-nil, make the Stack HUD frame fullscreen."
  :type 'boolean
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-stack-frame-close-on-exit nil
  "When non-nil, close the Stack HUD frame when the HUD is dismissed."
  :type 'boolean
  :group 'tatami-integration)

(defcustom stack-hud-devmap-root "/home/joe/code/futon3"
  "Root directory for devmap files referenced by boundary entries."
  :type 'directory
  :group 'tatami-integration)

(defcustom stack-hud-log-dir "/home/joe/code/storage/futon0/vitality/stack-hud"
  "Directory where Stack HUD snapshots are logged."
  :type 'directory
  :group 'tatami-integration)

(defcustom stack-hud-pattern-sync-root "/home/joe/code/futon3"
  "Root directory for Futon3 when running pattern sync."
  :type 'directory
  :group 'tatami-integration)

(defcustom stack-hud-futon1-api-base (or (getenv "FUTON1_API_BASE")
                                         "http://localhost:8080/api/alpha")
  "Base URL for Futon1 API (used for reachability + sync)."
  :type 'string
  :group 'tatami-integration)

(defcustom stack-hud-futon1-profile (getenv "FUTON1_PROFILE")
  "Futon1 profile header used for API calls."
  :type 'string
  :group 'tatami-integration)

(defcustom stack-hud-pattern-sync-cache-seconds 60
  "Seconds to cache pattern sync status in the Stack HUD."
  :type 'integer
  :group 'tatami-integration)
(defcustom stack-hud-pattern-sync-timeout-seconds 5
  "Seconds to allow pattern sync status checks before timing out."
  :type 'integer
  :group 'tatami-integration)
(defcustom stack-hud-pattern-sync-async t
  "When non-nil, refresh pattern sync status asynchronously."
  :type 'boolean
  :group 'tatami-integration)
(defcustom stack-hud-disable-pattern-sync nil
  "When non-nil, skip pattern sync status checks in the Stack HUD."
  :type 'boolean
  :group 'tatami-integration)

(defcustom stack-hud-vitality-scan-path "/home/joe/code/futon3/resources/vitality/latest_scan.json"
  "Path to the futon vitality snapshot consumed by the Stack HUD."
  :type 'file
  :group 'tatami-integration)

(defcustom stack-hud-vitality-scan-stale-minutes 30
  "Refresh vitality snapshot when the file is older than this many minutes."
  :type 'integer
  :group 'tatami-integration)

(defcustom stack-hud-vitality-scan-min-interval-seconds 30
  "Minimum seconds between vitality scan invocations."
  :type 'integer
  :group 'tatami-integration)

(defcustom stack-hud-vitality-scan-command
  '("bb" "/home/joe/code/futon0/scripts/futon0/vitality/scanner.bb" "--"
    "--quiet"
    "--config" "/home/joe/code/storage/futon0/vitality/vitality_scanner.json"
    "--output" "/home/joe/code/futon3/resources/vitality/latest_scan.json")
  "Command used to refresh the vitality snapshot."
  :type '(repeat string)
  :group 'tatami-integration)

(defcustom stack-hud-boundary-scan-path "/home/joe/code/futon3/boundary.edn"
  "Path to the boundary snapshot consumed by the Stack HUD."
  :type 'file
  :group 'tatami-integration)

(defcustom stack-hud-boundary-scan-stale-minutes 120
  "Refresh boundary snapshot when the file is older than this many minutes."
  :type 'integer
  :group 'tatami-integration)

(defcustom stack-hud-boundary-scan-min-interval-seconds 120
  "Minimum seconds between boundary scan invocations."
  :type 'integer
  :group 'tatami-integration)

(defcustom stack-hud-boundary-scan-command
  '("python3"
    "/home/joe/code/futon3/scripts/devmap_readiness.py"
    "--output" "/home/joe/code/futon3/boundary.edn")
  "Command used to refresh the boundary snapshot."
  :type '(repeat string)
  :group 'tatami-integration)

(defcustom stack-hud-command-timeout-seconds 15
  "Seconds to allow blocking Stack HUD commands before timing out."
  :type 'integer
  :group 'tatami-integration)

(defvar stack-hud--last-log-day nil)
(defvar stack-hud--pattern-sync-cache nil)
(defvar stack-hud--pattern-sync-cache-time 0)
(defvar stack-hud--pattern-sync-last-error nil)
(defvar stack-hud--pattern-sync-last-output nil)
(defvar stack-hud--pattern-sync-refresh-in-flight nil)
(defvar stack-hud--last-vitality-scan-time 0)
(defvar stack-hud--last-boundary-scan-time 0)
(defvar stack-hud--vitality-config-cache nil)
(defvar stack-hud--vitality-config-mtime nil)

(defun stack-hud--futon1-root-url ()
  (when (and stack-hud-futon1-api-base
             (not (string-empty-p stack-hud-futon1-api-base)))
    (let ((base (string-trim stack-hud-futon1-api-base)))
      (replace-regexp-in-string
       "/api/\\(alpha\\|%CE%B1\\|%ce%b1\\)/?\\'" "" base))))

(defun stack-hud--futon1-health-url ()
  (when-let ((root (stack-hud--futon1-root-url)))
    (concat root "/healthz")))

(defun stack-hud--futon1-api-url (path)
  (when (and stack-hud-futon1-api-base
             (not (string-empty-p stack-hud-futon1-api-base)))
    (concat (string-trim-right stack-hud-futon1-api-base "/") path)))

(defun stack-hud--fetch-json (url &optional headers)
  (condition-case nil
      (let ((url-request-extra-headers headers))
        (when-let ((buf (url-retrieve-synchronously url t t 2.0)))
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "\n\n" nil t)
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (json-key-type 'symbol)
                     (data (if (fboundp 'json-parse-buffer)
                               (json-parse-buffer :object-type 'plist
                                                  :array-type 'list
                                                  :null-object nil
                                                  :false-object nil)
                             (json-read))))
                (kill-buffer buf)
                data)))))
    (error nil)))

(defun stack-hud--futon1-reachable-p ()
  (condition-case nil
      (when-let ((url (stack-hud--futon1-health-url)))
        (let ((buf (url-retrieve-synchronously url t t 2.0)))
          (when buf
            (with-current-buffer buf
              (goto-char (point-min))
              (let ((status (when (re-search-forward "HTTP/[^ ]+ \\([0-9]+\\)" nil t)
                              (string-to-number (match-string 1)))))
                (kill-buffer buf)
                (and status (<= 200 status 299)))))))
    (error nil)))

(defun stack-hud--pattern-sync-verify-summary (payload)
  (when payload
    (let* ((ok (plist-get payload :ok?))
           (results (plist-get payload :results))
           (patterns (make-hash-table :test 'equal))
           (issues (make-hash-table :test 'equal)))
      (dolist (result results)
        (unless (plist-get result :ok?)
          (dolist (failure (plist-get result :failures))
            (let ((name (or (plist-get failure :pattern-name)
                            (plist-get failure :pattern-id)))
                  (issue (plist-get failure :issue)))
              (when name
                (puthash name t patterns))
              (when issue
                (puthash issue (1+ (gethash issue issues 0)) issues))))))
      (let* ((pattern-count (hash-table-count patterns))
             (issue-list (let (items)
                           (maphash (lambda (k v)
                                      (push (cons k v) items))
                                    issues)
                           (sort items (lambda (a b) (> (cdr a) (cdr b))))))
             (names (let (items)
                      (maphash (lambda (k _v) (push k items)) patterns)
                      (sort items #'string<)))
             (sample (seq-take names 3)))
        (list :ok? (and ok (zerop pattern-count))
              :pattern-count pattern-count
              :issues issue-list
              :sample sample)))))

(defun stack-hud--pattern-sync-command (&optional diff)
  (let ((cmd (list "clj" "-M" "-m" "scripts.pattern-sync"
                   "--root" stack-hud-pattern-sync-root)))
    (when diff
      (setq cmd (append cmd '("--diff" "--registry"))))
    (when (and stack-hud-futon1-api-base
               (not (string-empty-p stack-hud-futon1-api-base)))
      (setq cmd (append cmd (list "--api-base" stack-hud-futon1-api-base))))
    (when (and stack-hud-futon1-profile
               (not (string-empty-p stack-hud-futon1-profile)))
      (setq cmd (append cmd (list "--profile" stack-hud-futon1-profile))))
    cmd))

(defun stack-hud--vitality-scan-stale-p (path now)
  (let* ((attrs (and path (file-attributes path)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (age-stale (if (and mtime (numberp stack-hud-vitality-scan-stale-minutes))
                        (> (- now (float-time mtime))
                           (* 60 stack-hud-vitality-scan-stale-minutes))
                      t)))
    (or age-stale
        (stack-hud--vitality-log-newer-p mtime))))

(defun stack-hud--vitality-config ()
  (let* ((path "/home/joe/code/storage/futon0/vitality/vitality_scanner.json")
         (mtime (and (file-exists-p path) (nth 5 (file-attributes path)))))
    (if (and stack-hud--vitality-config-cache
             (equal mtime stack-hud--vitality-config-mtime))
        stack-hud--vitality-config-cache
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
                (setq stack-hud--vitality-config-cache data
                      stack-hud--vitality-config-mtime mtime)
                data)
            (error nil)))))))

(defun stack-hud--vitality-log-newer-p (scan-mtime)
  (when scan-mtime
    (let* ((config (stack-hud--vitality-config))
           (tatami (plist-get config :tatami))
           (log-path (and tatami (plist-get tatami :log_path))))
      (when (and log-path (stringp log-path))
        (let* ((expanded (expand-file-name (substitute-in-file-name log-path)))
               (attrs (and (file-exists-p expanded)
                           (file-attributes expanded)))
               (log-mtime (and attrs (file-attribute-modification-time attrs))))
          (and log-mtime
               (time-less-p scan-mtime log-mtime)))))))

(defun stack-hud--maybe-refresh-vitality-scan ()
  (when (and stack-hud-vitality-scan-command
             (listp stack-hud-vitality-scan-command))
    (let ((now (float-time)))
      (when (and (> (- now stack-hud--last-vitality-scan-time)
                    stack-hud-vitality-scan-min-interval-seconds)
                 (stack-hud--vitality-scan-stale-p stack-hud-vitality-scan-path now))
        (setq stack-hud--last-vitality-scan-time now)
        (condition-case nil
            (with-timeout (stack-hud-command-timeout-seconds
                           (error "Stack HUD vitality scan timed out"))
              (apply #'call-process
                     (car stack-hud-vitality-scan-command)
                     nil nil nil
                     (cdr stack-hud-vitality-scan-command)))
          (error nil))))))

(defun stack-hud--boundary-scan-stale-p (path now)
  (let* ((attrs (and path (file-attributes path)))
         (mtime (and attrs (file-attribute-modification-time attrs))))
    (if (and mtime (numberp stack-hud-boundary-scan-stale-minutes))
        (> (- now (float-time mtime))
           (* 60 stack-hud-boundary-scan-stale-minutes))
      t)))

(defun stack-hud--maybe-refresh-boundary-scan ()
  (when (and stack-hud-boundary-scan-command
             (listp stack-hud-boundary-scan-command))
    (let ((now (float-time)))
      (when (and (> (- now stack-hud--last-boundary-scan-time)
                    stack-hud-boundary-scan-min-interval-seconds)
                 (stack-hud--boundary-scan-stale-p stack-hud-boundary-scan-path now))
        (setq stack-hud--last-boundary-scan-time now)
        (condition-case nil
            (let ((status (with-timeout (stack-hud-command-timeout-seconds
                                         (error "Stack HUD boundary scan timed out"))
                            (apply #'call-process
                                   (car stack-hud-boundary-scan-command)
                                   nil nil nil
                                   (cdr stack-hud-boundary-scan-command)))))
              (eq status 0))
          (error nil))))))

(defun stack-hud--call-command (cmd)
  (condition-case err
      (when (and cmd (file-directory-p stack-hud-pattern-sync-root))
        (with-temp-buffer
          (let* ((default-directory stack-hud-pattern-sync-root)
                 (status (with-timeout (stack-hud-command-timeout-seconds
                                        (error "Stack HUD pattern sync timed out"))
                           (apply #'call-process (car cmd) nil (list t t) nil (cdr cmd))))
                 (output (string-trim (buffer-string)))
                 (line (car (split-string output "\n"))))
            (setq stack-hud--pattern-sync-last-output output)
            (if (and (numberp status) (zerop status))
                (progn
                  (setq stack-hud--pattern-sync-last-error nil)
                  output)
              (setq stack-hud--pattern-sync-last-error
                    (format "pattern-sync failed (status %s): %s"
                            status (or line "no output")))
              nil))))
    (error
     (setq stack-hud--pattern-sync-last-error (error-message-string err))
     nil)))

(defun stack-hud--pattern-sync-parse-summary (output)
  (when output
    (let* ((line (car (seq-filter (lambda (row)
                                    (string-prefix-p "Plan summary:" row))
                                  (split-string output "\n"))))
           (summary (and line (string-trim (string-remove-prefix "Plan summary:" line))))
           (entity (when (and summary (string-match ":ensure-entity \\([0-9]+\\)" summary))
                     (string-to-number (match-string 1 summary))))
           (relation (when (and summary (string-match ":ensure-relation \\([0-9]+\\)" summary))
                       (string-to-number (match-string 1 summary)))))
      (when (or entity relation)
        (list :ensure-entity (or entity 0)
              :ensure-relation (or relation 0)
              :total (+ (or entity 0) (or relation 0)))))))

(defun stack-hud--pattern-sync-diff ()
  (when (and stack-hud-futon1-api-base
             (not (string-empty-p stack-hud-futon1-api-base)))
    (let* ((cmd (stack-hud--pattern-sync-command t))
           (output (stack-hud--call-command cmd)))
      (stack-hud--pattern-sync-parse-summary output))))

(defun stack-hud--pattern-sync-refresh-async ()
  (when (and (not stack-hud--pattern-sync-refresh-in-flight)
             (file-directory-p stack-hud-pattern-sync-root))
    (setq stack-hud--pattern-sync-refresh-in-flight t)
    (let ((state (list :reachable nil
                       :diff nil
                       :diff-error nil
                       :verify nil))
          (pending 0))
      (cl-labels
          ((finalize ()
             (setq stack-hud--pattern-sync-cache state
                   stack-hud--pattern-sync-cache-time (float-time)
                   stack-hud--pattern-sync-refresh-in-flight nil))
           (mark-done ()
             (setq pending (1- pending))
             (when (<= pending 0)
               (finalize)))
           (parse-json-buffer ()
             (let ((json-object-type 'plist)
                   (json-array-type 'list)
                   (json-key-type 'symbol))
               (when (re-search-forward "\n\n" nil t)
                 (if (fboundp 'json-parse-buffer)
                     (json-parse-buffer :object-type 'plist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)
                   (json-read)))))
           (start-verify ()
             (let ((url (stack-hud--futon1-api-url "/meta/model/verify")))
               (if (not url)
                   (mark-done)
                 (let ((timeout stack-hud-pattern-sync-timeout-seconds)
                       (timer nil))
                   (setq pending (1+ pending))
                   (setq timer
                         (run-at-time
                          timeout nil
                          (lambda ()
                            (setq timer nil)
                            (setq stack-hud--pattern-sync-last-error
                                  "pattern sync verify timed out")
                            (setq state (plist-put state :diff-error
                                                   stack-hud--pattern-sync-last-error))
                            (mark-done))))
                   (url-retrieve
                    url
                    (lambda (status)
                      (when (timerp timer)
                        (cancel-timer timer))
                      (let ((err (plist-get status :error)))
                        (if err
                            (setq stack-hud--pattern-sync-last-error
                                  (format "verify error: %s" err))
                          (setq stack-hud--pattern-sync-last-error nil)
                          (let ((payload (save-excursion
                                           (goto-char (point-min))
                                           (ignore-errors (parse-json-buffer)))))
                            (setq state (plist-put state :verify
                                                   (stack-hud--pattern-sync-verify-summary payload))))))
                      (setq state (plist-put state :diff-error
                                             stack-hud--pattern-sync-last-error))
                      (kill-buffer (current-buffer))
                      (mark-done))
                    nil t t)))))
           (start-diff ()
             (let ((cmd (stack-hud--pattern-sync-command t)))
               (if (not cmd)
                   (mark-done)
                 (let* ((default-directory stack-hud-pattern-sync-root)
                        (buffer (generate-new-buffer " *stack-hud-pattern-sync*"))
                        (proc (make-process :name "stack-hud-pattern-sync-diff"
                                            :buffer buffer
                                            :command cmd
                                            :noquery t))
                        (timeout stack-hud-pattern-sync-timeout-seconds)
                        (done nil)
                        (timer nil))
                   (setq pending (1+ pending))
                   (setq timer
                         (run-at-time
                          timeout nil
                          (lambda ()
                            (when (and (not done) (process-live-p proc))
                              (setq done t)
                              (kill-process proc)
                              (setq stack-hud--pattern-sync-last-error
                                    "pattern sync diff timed out")
                              (setq state (plist-put state :diff nil))
                              (setq state (plist-put state :diff-error
                                                     stack-hud--pattern-sync-last-error))
                              (mark-done)))))
                   (set-process-sentinel
                    proc
                    (lambda (process _event)
                      (when (and (not done)
                                 (memq (process-status process) '(exit signal)))
                        (setq done t)
                        (when (timerp timer)
                          (cancel-timer timer))
                        (let* ((status (process-exit-status process))
                               (output (with-current-buffer (process-buffer process)
                                         (buffer-string)))
                               (line (car (split-string output "\n"))))
                          (setq stack-hud--pattern-sync-last-output output)
                          (if (and (numberp status) (zerop status))
                              (setq stack-hud--pattern-sync-last-error nil)
                            (setq stack-hud--pattern-sync-last-error
                                  (format "pattern-sync failed (status %s): %s"
                                          status (or line "no output"))))
                          (setq state (plist-put state :diff
                                                 (stack-hud--pattern-sync-parse-summary output)))
                          (setq state (plist-put state :diff-error
                                                 stack-hud--pattern-sync-last-error)))
                        (kill-buffer (process-buffer process))
                        (mark-done))))))))
           (start-reachable ()
             (let ((url (stack-hud--futon1-health-url)))
               (if (not url)
                   (finalize)
                 (let ((timeout stack-hud-pattern-sync-timeout-seconds)
                       (timer nil))
                   (setq timer
                         (run-at-time
                          timeout nil
                          (lambda ()
                            (setq timer nil)
                            (setq stack-hud--pattern-sync-last-error
                                  "pattern sync reachability timed out")
                            (setq state (plist-put state :reachable nil))
                            (setq state (plist-put state :diff-error
                                                   stack-hud--pattern-sync-last-error))
                            (finalize))))
                   (url-retrieve
                    url
                    (lambda (status)
                      (when (timerp timer)
                        (cancel-timer timer))
                      (let ((ok nil)
                            (err (plist-get status :error)))
                        (if err
                            (setq stack-hud--pattern-sync-last-error
                                  (format "reachability error: %s" err))
                          (goto-char (point-min))
                          (when (re-search-forward "HTTP/[^ ]+ \\([0-9]+\\)" nil t)
                            (let ((code (string-to-number (match-string 1))))
                              (setq ok (<= 200 code 299))))
                          (setq stack-hud--pattern-sync-last-error nil))
                        (setq state (plist-put state :reachable ok))
                        (kill-buffer (current-buffer))
                        (if ok
                            (progn
                              (start-diff)
                              (start-verify)
                              (when (<= pending 0)
                                (finalize)))
                          (finalize))))
                    nil t t)))))
        (start-reachable))))))

(defun stack-hud--pattern-sync-status ()
  (if stack-hud-pattern-sync-async
      (let* ((now (float-time))
             (stale (or (null stack-hud--pattern-sync-cache)
                        (>= (- now stack-hud--pattern-sync-cache-time)
                            stack-hud-pattern-sync-cache-seconds))))
        (when stale
          (stack-hud--pattern-sync-refresh-async))
        (let ((status (or stack-hud--pattern-sync-cache
                          (list :reachable nil
                                :diff nil
                                :diff-error "pattern sync pending"
                                :verify nil
                                :checked-at nil))))
          (if stack-hud--pattern-sync-refresh-in-flight
              (plist-put (copy-sequence status) :diff-pending t)
            status)))
    (with-timeout (stack-hud-pattern-sync-timeout-seconds
                   (list :reachable nil
                         :diff nil
                         :diff-error "pattern sync status timed out"
                         :verify nil
                         :checked-at (current-time-string)))
      (let ((now (float-time)))
        (if (and stack-hud--pattern-sync-cache
                 (< (- now stack-hud--pattern-sync-cache-time)
                    stack-hud-pattern-sync-cache-seconds))
            stack-hud--pattern-sync-cache
          (let* ((reachable (stack-hud--futon1-reachable-p))
                 (diff (when reachable
                         (stack-hud--pattern-sync-diff)))
                 (verify (when reachable
                           (let* ((headers (when (and stack-hud-futon1-profile
                                                      (not (string-empty-p stack-hud-futon1-profile)))
                                             `(("X-Profile" . ,stack-hud-futon1-profile))))
                                  (payload (stack-hud--fetch-json
                                            (stack-hud--futon1-api-url "/meta/model/verify")
                                            headers)))
                             (stack-hud--pattern-sync-verify-summary payload))))
                 (status (list :reachable reachable
                               :diff diff
                               :diff-error stack-hud--pattern-sync-last-error
                               :verify verify
                               :checked-at (current-time-string))))
            (setq stack-hud--pattern-sync-cache status
                  stack-hud--pattern-sync-cache-time now)
            status))))))

(defun stack-hud--pattern-sync-reset-cache ()
  (setq stack-hud--pattern-sync-cache nil)
  (setq stack-hud--pattern-sync-cache-time 0)
  (setq stack-hud--pattern-sync-refresh-in-flight nil))

(defun stack-hud--pattern-sync-run ()
  (interactive)
  (let ((cmd (stack-hud--pattern-sync-command nil))
        (buffer (get-buffer-create "*Pattern Sync*")))
    (unless (and cmd (file-directory-p stack-hud-pattern-sync-root))
      (user-error "Pattern sync root is unavailable"))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let* ((default-directory stack-hud-pattern-sync-root)
           (proc (apply #'start-process "pattern-sync" buffer (car cmd) (cdr cmd))))
      (set-process-sentinel
       proc
       (lambda (process event)
         (when (memq (process-status process) '(exit signal))
           (stack-hud--pattern-sync-reset-cache)
           (message "Pattern sync finished: %s" (string-trim event))
           (when (fboundp 'my-chatgpt-shell--maybe-render-context)
             (my-chatgpt-shell--maybe-render-context))))))))

(defun my-chatgpt-shell--stack-status-symbol (value)
  (cond
   ((symbolp value) value)
   ((stringp value) (intern (downcase value)))
   (t value)))

(defun my-chatgpt-shell--stack-format-hours (hours)
  (cond
   ((not (numberp hours)) "n/a")
   ((>= hours 48)
    (let* ((days (floor (/ hours 24)))
           (rem (- hours (* days 24)))
           (hrs (floor rem)))
      (format "%dd %dh" days hrs)))
   ((>= hours 1) (format "%.1fh" hours))
   (t (format "%.0fm" (* hours 60)))))

(defun my-chatgpt-shell--stack-top-children (children)
  (when (and (listp children) children)
    (let ((items (seq-take children 3)))
      (string-join
       (delq nil
             (mapcar (lambda (child)
                       (let ((name (plist-get child :name))
                             (count (plist-get child :recent)))
                         (when (and name count)
                           (format "%s×%s" name count))))
                     items))
       ", "))))

(defun my-chatgpt-shell--stack-join-names (items)
  (when (and (listp items) items)
    (let ((clean (delq nil
                       (mapcar (lambda (item)
                                 (cond
                                  ((stringp item) item)
                                  ((keywordp item) (symbol-name item))
                                  (t (when item (format "%s" item)))))
                               (seq-take items 4)))))
      (when clean
        (string-join clean ", ")))))

(defun stack-hud--parse-iso-time (text)
  (when (and text (stringp text))
    (ignore-errors (date-to-time text))))

(defun stack-hud--file-time-newer-p (a b)
  (let ((ta (stack-hud--parse-iso-time a))
        (tb (stack-hud--parse-iso-time b)))
    (cond
     ((and ta tb) (time-less-p tb ta))
     (ta t)
     (t nil))))

(defun my-chatgpt-shell--stack-vitality-from-file ()
  (stack-hud--maybe-refresh-vitality-scan)
  (let ((path stack-hud-vitality-scan-path))
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
              data)
          (error nil))))))

(defun my-chatgpt-shell--stack-zoomr4-status-counts ()
  (let ((path (and (boundp 'arxana-media-index-path) arxana-media-index-path)))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path)
        (condition-case nil
            (let* ((json-object-type 'plist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (data (if (fboundp 'json-parse-buffer)
                             (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil :false-object nil)
                           (json-read)))
                   (entries (plist-get data :entries))
                   (counts (list (cons "hold" 0)
                                 (cons "archive" 0)
                                 (cons "trash" 0))))
              (dolist (entry entries)
                (let ((status (or (plist-get entry :status) "hold")))
                  (when (assoc status counts)
                    (setcdr (assoc status counts) (1+ (cdr (assoc status counts)))))))
              counts)
          (error nil))))))

(defun my-chatgpt-shell--stack-format-timestamp (ms)
  (when (numberp ms)
    (format-time-string "%H:%M" (seconds-to-time (/ ms 1000)))))

(defun my-chatgpt-shell--stack-history-line (entry)
  (let* ((type (plist-get entry :type))
         (ts (my-chatgpt-shell--stack-format-timestamp (plist-get entry :timestamp)))
         (anchors (my-chatgpt-shell--stack-join-names (plist-get entry :anchors)))
         (topics (my-chatgpt-shell--stack-join-names (plist-get entry :topics)))
         (relations (plist-get entry :relation-count)))
    (pcase type
      (:focus (format "%s focus → %s" (or ts "?") (or anchors "n/a")))
      (:profile (format "%s profile → %s rel (%s)"
                        (or ts "?")
                        (or relations 0)
                        (or topics "-")))
      (_ (format "%s %s" (or ts "?") (or type "event"))))))

(defun my-chatgpt-shell--insert-stack-focus-profile (focus-profile)
  (when focus-profile
    (let ((focus (plist-get focus-profile :focus))
          (profile (plist-get focus-profile :profile))
          (history (plist-get focus-profile :history)))
      (insert "  Focus/Profile:\n")
      (when focus
        (let ((anchors (my-chatgpt-shell--stack-join-names (plist-get focus :anchors)))
              (neighbors (my-chatgpt-shell--stack-join-names (plist-get focus :neighbors))))
          (insert (format "    Anchors: %s\n" (or anchors "n/a")))
          (insert (format "    Neighbors: %s\n" (or neighbors "n/a")))))
      (when profile
        (let ((relations (plist-get profile :relation-count))
              (topics (my-chatgpt-shell--stack-join-names (plist-get profile :topics))))
          (insert (format "    Relations: %s" (or relations 0)))
          (insert (format " | Topics: %s\n" (or topics "-")))))
      (when (and (listp history) history)
        (insert "    Recent:\n")
        (dolist (entry (seq-take (reverse history) 3))
          (insert (format "      • %s\n"
                          (my-chatgpt-shell--stack-history-line entry)))))
      (insert "\n"))))

(defun my-chatgpt-shell--insert-stack-vitality (vitality)
  (let ((filesystem (plist-get vitality :filesystem))
        (tatami (plist-get vitality :tatami))
        (zoomr4-status (my-chatgpt-shell--stack-zoomr4-status-counts)))
    (when (or filesystem tatami)
      (insert "  Vitality:\n")
      (dolist (entry filesystem)
        (let* ((label (or (plist-get entry :label) "?"))
               (imports (plist-get entry :imports))
               (import-recent (plist-get imports :recent-imports))
               (recent (if (and (string= label "zoomr4") import-recent)
                           import-recent
                         (or (plist-get entry :recent-files) 0)))
               (top (my-chatgpt-shell--stack-top-children (plist-get entry :top-children)))
               (import-total (and imports (plist-get imports :total)))
               (hold-text (if (and (string= label "zoomr4") zoomr4-status)
                              (let ((hold (cdr (assoc "hold" zoomr4-status)))
                                    (trash (cdr (assoc "trash" zoomr4-status))))
                                (concat
                                 (if hold (format " | hold %s" hold) "")
                                 (if trash (format " | trash %s" trash) "")))
                            ""))
               (import-text (if (and import-total (> import-total 0))
                                (format " | %s processed" import-total)
                              "")))
          (unless (string= label "code")
            (insert (format "    %s: %d recent%s%s\n"
                            label
                            recent
                            (if top (format " (%s)" top) "")
                            (concat hold-text import-text))))
          (when-let* ((recent-imports (and imports (plist-get imports :recent))))
            (dolist (item (seq-take recent-imports 3))
              (let ((title (or (plist-get item :title) "untitled"))
                    (recorded (plist-get item :recorded_date)))
                (insert (format "      • %s%s\n"
                                title
                                (if recorded (format " (%s)" recorded) ""))))))))
      (when tatami
        (if (plist-get tatami :exists)
            (let ((last-event (or (plist-get tatami :last-event)
                                  (plist-get tatami :last_event)))
                  (hours-since (or (plist-get tatami :hours-since)
                                   (plist-get tatami :hours_since)
                                   (plist-get tatami :hours_since_last)))
                  (gap-warning (or (plist-get tatami :gap-warning)
                                   (plist-get tatami :gap_warning))))
              (insert (format "    Tatami: last %s (%s)%s\n"
                              (or last-event "n/a")
                              (my-chatgpt-shell--stack-format-hours hours-since)
                              (if gap-warning " ⚠️" ""))))
          (insert "    Tatami: log missing\n")))
      (insert "\n"))))

(defun my-chatgpt-shell--insert-stack-git (git)
  (when git
    (insert "  Git:\n")
    (when-let* ((sphere (plist-get git :dominant-sphere)))
      (insert (format "    Dominant sphere: %s\n" sphere)))
    (when-let* ((streak (plist-get git :streak)))
      (insert (format "    Streak: current %s / longest %s\n"
                      (or (plist-get streak :current) 0)
                      (or (plist-get streak :longest) 0))))
    (when-let* ((quiet (plist-get git :quiet-days)))
      (insert (format "    Quiet: %s days\n" quiet)))
    (when-let* ((last-active (plist-get git :last-active)))
      (insert (format "    Last commit: %s (%s commits)\n"
                      (or (plist-get last-active :date) "n/a")
                      (or (plist-get last-active :total) 0))))
    (insert "\n")))

(defun my-chatgpt-shell--stack-open-lab-files (kind)
  (if (fboundp 'arxana-patterns-browse-lab-files-other-frame)
      (arxana-patterns-browse-lab-files-other-frame kind)
    (if (fboundp 'arxana-patterns-browse-lab-files-other-window)
        (arxana-patterns-browse-lab-files-other-window kind)
      (if (fboundp 'arxana-patterns-browse-lab-files)
          (arxana-patterns-browse-lab-files kind)
        (message "Arxana lab browser is unavailable; load futon4 dev/bootstrap.el")))))

(defun my-chatgpt-shell--stack-insert-lab-count (label kind count)
  (insert (format "%s " label))
  (let ((text (format "%s" (or count 0))))
    (insert-text-button text
                        'follow-link t
                        'help-echo (format "Open lab %s files" label)
                        'action (lambda (_btn)
                                  (my-chatgpt-shell--stack-open-lab-files kind)))))

(defun stack-hud--open-devmap (path title)
  (let* ((full (if (file-name-absolute-p path)
                   path
                 (expand-file-name path stack-hud-devmap-root))))
    (if (file-readable-p full)
        (progn
          (find-file-other-window full)
          (goto-char (point-min))
          (when (and title (search-forward title nil t))
            (beginning-of-line)))
      (message "Devmap not found: %s" full))))

(defun my-chatgpt-shell--insert-stack-boundary (boundary)
  (let* ((critical (plist-get boundary :critical))
         (futons (plist-get boundary :futons)))
    (unless (seq-empty-p critical)
      (let* ((milestone (plist-get boundary :milestone-prototype))
             (label (if milestone
                        (format "  Boundary gaps (<= Prototype %s):\n" milestone)
                      "  Boundary gaps:\n")))
        (insert label))
      (dolist (entry (seq-take critical 4))
        (let ((fid (or (plist-get entry :id) "f?"))
              (missing (or (plist-get entry :missing_evidence) 0))
              (last-modified (or (plist-get entry :last_modified) "n/a"))
              (path (plist-get entry :path)))
          (insert "    ")
          (if path
              (insert-text-button fid
                                  'follow-link t
                                  'help-echo "Open devmap"
                                  'action (lambda (_btn)
                                            (stack-hud--open-devmap path nil)))
            (insert fid))
          (insert (format " missing %s (last %s)\n"
                          (format "%s evidence blocks" missing)
                          last-modified)))
        (when-let ((titles (plist-get entry :missing_evidence_titles)))
          (let ((path (plist-get entry :path)))
            (dolist (title titles)
              (insert "      • ")
              (if path
                  (insert-text-button title
                                      'follow-link t
                                      'help-echo "Open missing evidence block"
                                      'action (lambda (_btn)
                                                (stack-hud--open-devmap path title)))
                (insert title))
              (insert "\n")))))
      (insert "\n"))
    (when (seqp futons)
      (let ((rows (seq-filter (lambda (entry)
                                (or (plist-get entry :lab_stub_count)
                                    (plist-get entry :lab_raw_count)
                                    (plist-get entry :lab_doc_draft_count)
                                    (plist-get entry :media_unpersisted)))
                              futons)))
        (unless (seq-empty-p rows)
          (insert "  Boundary details:\n")
          (dolist (entry rows)
            (insert (format "    %s lab " (or (plist-get entry :id) "f?")))
            (my-chatgpt-shell--stack-insert-lab-count "raw" 'raw (plist-get entry :lab_raw_count))
            (insert " | ")
            (my-chatgpt-shell--stack-insert-lab-count "stubs" 'stubs (plist-get entry :lab_stub_count))
            (insert " | ")
            (my-chatgpt-shell--stack-insert-lab-count "drafts" 'drafts (plist-get entry :lab_doc_draft_count))
            (insert (format " | media %s\n" (or (plist-get entry :media_unpersisted) 0))))
          (insert "\n"))))))

(defun my-chatgpt-shell--insert-stack-reminders (reminders)
  (unless (seq-empty-p reminders)
    (insert "  Reminders:\n")
    (dolist (rem (seq-take reminders 5))
      (let* ((label (or (plist-get rem :label) "unnamed"))
             (display (or (plist-get rem :display) "?"))
             (hours (plist-get rem :hours))
             (status (my-chatgpt-shell--stack-status-symbol (plist-get rem :status))))
        (insert (format "    %s – %s (%s, %s)\n"
                        display
                        label
                        (pcase status
                          ('now "now")
                          ('imminent "<6h")
                          ('soon "<24h")
                          ('upcoming "<3d")
                          ('scheduled "scheduled")
                          ('overdue "overdue")
                          (_ (format "%s" status)))
                        (my-chatgpt-shell--stack-format-hours hours)))))
    (insert "\n")))

(defun my-chatgpt-shell--insert-stack-hot-reload ()
  (insert "  Hot reload: ")
  (cond
   ((not (my-chatgpt-shell--hot-reload-feature-available-p))
    (insert "unsupported on this Emacs build.\n\n"))
   (t
    (let* ((enabled my-chatgpt-shell--hot-reload-enabled)
           (count (my-chatgpt-shell--hot-reload-watching-count))
           (delta (my-chatgpt-shell--hot-reload-last-delta-hours))
           (status (if enabled "ON" "OFF")))
      (insert (format "%s (watching %d) " status count))
      (insert-text-button (if enabled "Disable" "Enable")
                          'help-echo "Toggle Stack hot reload watchers"
                          'action (lambda (_event)
                                    (my-chatgpt-shell-hot-reload-toggle)
                                    (my-chatgpt-shell--maybe-render-context))
                          'follow-link t)
      (when delta
        (insert (format " | last reload %s"
                        (my-chatgpt-shell--stack-format-hours delta))))
      (insert "\n\n")))))

(defun my-chatgpt-shell--insert-stack-voice ()
  (insert "  Voice typing: ")
  (cond
   ((not (my-chatgpt-shell--voice-command-configured-p))
    (insert "unconfigured (set `my-chatgpt-shell-voice-command`).\n\n"))
   (t
    (let ((running (my-chatgpt-shell-voice-running-p))
          (proc my-chatgpt-shell--voice-process))
      (insert (if running "ON " "OFF "))
      (insert-text-button (if running "Stop" "Start")
                          'help-echo "Toggle the Futon voice typing process"
                          'action (lambda (_event)
                                    (my-chatgpt-shell-voice-toggle))
                          'follow-link t)
      (when (and running (process-live-p proc))
        (insert (format " | pid %s" (process-id proc))))
      (insert "\n\n")))))

(defun my-chatgpt-shell--insert-stack-pattern-sync ()
  (if stack-hud-disable-pattern-sync
      (insert "  Pattern sync: skipped\n\n")
    (let* ((status (stack-hud--pattern-sync-status))
           (reachable (plist-get status :reachable))
         (diff (plist-get status :diff))
         (diff-pending (plist-get status :diff-pending))
         (diff-error (plist-get status :diff-error))
         (verify (plist-get status :verify))
           (total (plist-get diff :total))
           (entity (plist-get diff :ensure-entity))
           (relation (plist-get diff :ensure-relation))
           (lines '())
           (extras '()))
      (insert "  Pattern sync: ")
      (cond
       ((or (null stack-hud-futon1-api-base)
            (string-empty-p stack-hud-futon1-api-base))
        (push "unconfigured" lines))
       ((eq reachable nil)
        (push "futon1 unknown" lines))
       (reachable
        (push "futon1 ok" lines))
       (t
        (push "futon1 down" lines)))
    (cond
     (diff
      (push (format "diff %s" (or total 0)) lines)
      (when (and entity relation)
        (push (format "diff detail: entity %s, rel %s" entity relation) lines)))
     (diff-pending
      (push "diff pending" lines))
     ((and reachable diff-error)
      (push (format "diff err: %s" diff-error) lines))
     (reachable
      (push "diff n/a" lines)))
      (when verify
        (if (plist-get verify :ok?)
            (push "inv ok" lines)
          (let* ((count (plist-get verify :pattern-count))
                 (issues (plist-get verify :issues))
                 (sample (plist-get verify :sample))
                 (issue-text (when issues
                               (string-join
                                (mapcar (lambda (pair)
                                          (format "%s=%s" (car pair) (cdr pair)))
                                        issues)
                                ", "))))
            (push (format "inv fail: %s" (or count 0)) lines)
            (when issue-text
              (push (format "inv detail: %s" issue-text) lines))
            (when (and (listp sample) sample)
              (push (format "Patterns failing: %s"
                            (string-join sample ", "))
                    extras)))))
      (when lines
        (insert (string-join (nreverse lines) "\n    ")))
      (insert " ")
      (cond
       ((and diff (numberp total) (> total 0))
        (insert-text-button "Sync now"
                            'help-echo "Push filesystem patterns into Futon1"
                            'action (lambda (_event)
                                      (stack-hud--pattern-sync-run))
                            'follow-link t))
       ((and reachable diff-error)
        (insert "check diff"))
       (t
        (insert "clean")))
      (insert "\n")
      (dolist (line (nreverse extras))
        (insert (format "    %s\n" line)))
      (insert "\n"))))

(defun my-chatgpt-shell--insert-stack-futon-liveness (vitality)
  (let* ((activity (plist-get vitality :futon-activity))
         (vitality-generated (or (plist-get vitality :generated-at)
                                 (plist-get vitality :generated_at)))
         (file-vitality (my-chatgpt-shell--stack-vitality-from-file))
         (file-activity (plist-get file-vitality :futon_activity))
         (file-generated (plist-get file-vitality :generated_at))
         (use-file (and (listp file-activity)
                        file-activity
                        (or (not (listp activity))
                            (not activity)
                            (stack-hud--file-time-newer-p file-generated vitality-generated))))
         (fallback (and (not use-file) (null activity) file-activity))
         (selected (if use-file file-activity activity)))
    (cond
     ((and (listp selected) selected)
      (insert "  Futon liveness: ")
      (let ((parts (mapcar (lambda (entry)
                             (let ((fid (plist-get entry :id))
                                   (bucket (or (plist-get entry :bucket) "n/a")))
                               (format "%s(%s)" fid bucket)))
                           selected)))
        (insert (string-join parts " "))
        (insert "\n\n")))
     ((and (listp fallback) fallback)
      (insert "  Futon liveness: ")
      (let ((parts (mapcar (lambda (entry)
                             (let ((fid (plist-get entry :id))
                                   (bucket (or (plist-get entry :bucket) "n/a")))
                               (format "%s(%s)" fid bucket)))
                           fallback)))
        (insert (string-join parts " "))
        (insert "\n"))
      (insert "  ⚠️ Futon3 server reload needed (using file fallback for liveness).\n\n"))
     (t
      (insert "  Futon liveness: unavailable (run futon0.vitality.scanner)\n\n")))))

(defun my-chatgpt-shell--insert-stack-hud (stack)
  (let ((vitality (plist-get stack :vitality))
        (git (plist-get stack :git))
        (boundary (plist-get stack :boundary))
        (reminders (plist-get stack :reminders))
        (focus-profile (plist-get stack :focus-profile))
        (warnings (plist-get stack :warnings)))
    (insert (propertize "Stack HUD" 'face 'bold) "\n")
    (unless (seq-empty-p warnings)
      (dolist (msg warnings)
        (let ((start (point)))
          (insert (format "  ⚠️ %s\n" msg))
          (when-let ((doc (stack-doc--stack-warning-doc msg vitality)))
            (add-text-properties start (1- (point)) `(stack-doc ,doc))))))
      (insert "\n")
    (my-chatgpt-shell--insert-stack-futon-liveness vitality)
    (my-chatgpt-shell--insert-stack-hot-reload)
    (my-chatgpt-shell--insert-stack-voice)
    (my-chatgpt-shell--insert-stack-pattern-sync)
    (my-chatgpt-shell--insert-stack-focus-profile focus-profile)
    (my-chatgpt-shell--insert-stack-vitality vitality)
    (my-chatgpt-shell--insert-stack-git git)
    (my-chatgpt-shell--insert-stack-boundary boundary)
    (my-chatgpt-shell--insert-stack-reminders reminders)))

(defun my-chatgpt-shell--stack-hud-string (stack)
  (with-temp-buffer
    (my-chatgpt-shell--insert-stack-hud stack)
    (buffer-string)))

(defun my-chatgpt-shell--stack-columnize (text window)
  (let* ((text (or text ""))
         (width (window-body-width window))
         (height (window-body-height window))
         (lines (split-string text "\n"))
         (line-count (length lines))
         (max-line (if lines (apply #'max (mapcar #'string-width lines)) 0)))
    (if (or (<= width 0) (<= height 0) (<= line-count height) (<= width max-line))
        text
      (let* ((gap 2)
             (max-columns (max 1 (floor (/ (+ width gap) (+ max-line gap)))))
             (columns (min (ceiling (/ (float line-count) height))
                           max-columns))
             (col-width (floor (/ (- width (* gap (1- columns))) columns))))
        (if (or (< col-width 20) (<= columns 1))
            text
          (let ((rows (make-vector height nil)))
            (dotimes (row height)
              (let ((row-parts '()))
                (dotimes (col columns)
                  (let* ((idx (+ (* col height) row))
                         (cell (if (< idx line-count) (nth idx lines) ""))
                         (trim (truncate-string-to-width cell col-width 0 ?\s))
                         (padded (format (format "%%-%ds" col-width) trim)))
                    (push padded row-parts)
                    (when (< col (1- columns))
                      (push (make-string gap ?\s) row-parts))))
                (aset rows row (apply #'concat (nreverse row-parts)))))
            (concat (string-join (append rows nil) "\n") "\n")))))))

(defun stack-hud--today-string ()
  (format-time-string "%Y-%m-%d"))

(defun stack-hud--log-path (day)
  (expand-file-name (format "%s.jsonl" day) stack-hud-log-dir))

(defun stack-hud--summary-path (day)
  (expand-file-name (format "%s.summary.json" day) stack-hud-log-dir))

(defun stack-hud--jsonify (value)
  (cond
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((hash-table-p value)
    (let (items)
      (maphash (lambda (k v)
                 (push (cons (stack-hud--jsonify k) (stack-hud--jsonify v)) items))
               value)
      (nreverse items)))
   ((and (listp value) (car value) (keywordp (car value)))
    (let (items)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (push (cons (stack-hud--jsonify key) (stack-hud--jsonify val)) items)))
      (nreverse items)))
   ((listp value)
    (mapcar #'stack-hud--jsonify value))
   (t value)))

(defun stack-hud--decode-json (line)
  (if (fboundp 'json-parse-string)
      (json-parse-string line :object-type 'alist :array-type 'list :null-object nil :false-object nil)
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read-from-string line))))

(defun stack-hud--compact-day (day)
  (let* ((raw (stack-hud--log-path day))
         (summary (stack-hud--summary-path day)))
    (when (file-exists-p raw)
      (with-temp-buffer
        (insert-file-contents raw)
        (let* ((lines (seq-filter (lambda (line) (not (string-empty-p line)))
                                  (split-string (buffer-string) "\n")))
               (first-line (car lines))
               (last-line (car (last lines))))
          (when (and first-line last-line)
            (let* ((first (stack-hud--decode-json first-line))
                   (last (stack-hud--decode-json last-line))
                   (payload `(("date" . ,day)
                              ("first" . ,first)
                              ("last" . ,last))))
              (make-directory stack-hud-log-dir t)
              (with-temp-file summary
                (insert (json-encode payload)))))
          (delete-file raw))))))

(defun stack-hud--maybe-compact (today)
  (when (file-directory-p stack-hud-log-dir)
    (dolist (file (directory-files stack-hud-log-dir t "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.jsonl\\'"))
      (let ((day (file-name-base file)))
        (unless (string= day today)
          (let ((summary (stack-hud--summary-path day)))
            (if (file-exists-p summary)
                (ignore-errors (delete-file file))
              (stack-hud--compact-day day))))))))

(defun stack-hud-log-snapshot (stack)
  "Append a Stack HUD snapshot and compact the previous day on date rollover."
  (condition-case err
      (let* ((today (stack-hud--today-string))
             (payload `(("timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                        ("stack" . ,(stack-hud--jsonify stack))))
             (log-path (stack-hud--log-path today)))
        (stack-hud--maybe-compact today)
        (make-directory stack-hud-log-dir t)
        (with-temp-buffer
          (insert (json-encode payload) "\n")
          (append-to-file (point-min) (point-max) log-path))
        (setq stack-hud--last-log-day today))
    (error
     (message "Stack HUD log failed: %s" (error-message-string err)))))

(provide 'stack-hud)

;;; stack-hud.el ends here
