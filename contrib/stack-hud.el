;;; stack-hud.el --- Stack HUD definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; Stack HUD and Stack Context configuration for Futon/ChatGPT integration.

;;; Code:

(require 'json)
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

(defvar stack-hud--last-log-day nil)

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

(defun my-chatgpt-shell--stack-futon-activity-from-file ()
  (let ((path "/home/joe/code/futon3/resources/vitality/latest_scan.json"))
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
              (plist-get data :futon_activity))
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
               (recent (or (plist-get entry :recent-files) 0))
               (top (my-chatgpt-shell--stack-top-children (plist-get entry :top-children)))
               (imports (plist-get entry :imports))
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
          (insert (format "    %s: %d recent%s%s\n"
                          label
                          recent
                          (if top (format " (%s)" top) "")
                          (concat hold-text import-text)))
          (when-let* ((recent-imports (and imports (plist-get imports :recent))))
            (dolist (item (seq-take recent-imports 3))
              (let ((title (or (plist-get item :title) "untitled"))
                    (recorded (plist-get item :recorded_date)))
                (insert (format "      • %s%s\n"
                                title
                                (if recorded (format " (%s)" recorded) ""))))))))
      (when tatami
        (if (plist-get tatami :exists)
            (insert (format "    Tatami: last %s (%s)%s\n"
                            (or (plist-get tatami :last-event) "n/a")
                            (my-chatgpt-shell--stack-format-hours
                             (plist-get tatami :hours-since))
                            (if (plist-get tatami :gap-warning) " ⚠️" "")))
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
          (when (search-forward title nil t)
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
        (insert (format "    %s missing %s (last %s)\n"
                        (or (plist-get entry :id) "f?")
                        (format "%s evidence blocks" (or (plist-get entry :missing_evidence) 0))
                        (or (plist-get entry :last_modified) "n/a")))
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

(defun my-chatgpt-shell--insert-stack-futon-liveness (vitality)
  (let* ((activity (plist-get vitality :futon-activity))
         (fallback (and (null activity)
                        (my-chatgpt-shell--stack-futon-activity-from-file))))
    (cond
     ((and (listp activity) activity)
      (insert "  Futon liveness: ")
      (let ((parts (mapcar (lambda (entry)
                             (let ((fid (plist-get entry :id))
                                   (bucket (or (plist-get entry :bucket) "n/a")))
                               (format "%s(%s)" fid bucket)))
                           activity)))
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
  (let* ((width (window-body-width window))
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
