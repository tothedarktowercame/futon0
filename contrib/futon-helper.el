;;; futon-helper.el --- Convenience helpers for Futon tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Small helpers to speed up Codex workflows in eat buffers.
;; Session ID discovery:
;; - Uses `futon-codex-session-selector` (default: contrib/futon-lab-select-session.bb).
;; - The selector scans ~/.codex/sessions/**/rollout-*.jsonl, reads the first
;;   session_meta line, and matches on `cwd` + `--since` timestamp.
;; - If multiple sessions match, it prompts with a preview of recent message text.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'seq)

(defgroup futon-helper nil
  "Convenience helpers for Futon tooling."
  :group 'tools)

(defcustom futon-codex-scan-lines 200
  "Number of lines to scan backward for Codex M-: requests."
  :type 'integer)

(defcustom futon-codex-request-regexp "^\\s-*M-:\\s-*"
  "Regexp used to find Codex eval requests in output text."
  :type 'regexp)

(defcustom futon-codex-message-boundary-regexp nil
  "Regexp that marks the start of the most recent Codex message.
When non-nil, scanning starts after the last match of this regexp.
When nil, scan the last `futon-codex-scan-lines` lines."
  :type '(choice (const nil) regexp))

(defcustom futon-codex-report-source 'session
  "Where to read Codex requests for reports.
Use `session` to read from Codex session JSONL, or `buffer` to scan eat output."
  :type '(choice (const buffer) (const session)))

(defcustom futon-codex-session-selector
  "/home/joe/code/futon0/contrib/futon-lab-select-session.bb"
  "Path to a session selector script for locating Codex session files."
  :type 'file)

(defcustom futon-codex-session-since-hours 24
  "How many hours back to search for matching Codex sessions."
  :type 'integer)

(defcustom futon-codex-bb-command "bb"
  "Babashka command used to run lab-select-session."
  :type 'string)

(defcustom futon-codex-session-preview-chars 25
  "Maximum number of characters to show at the start of session previews."
  :type 'integer)

(defcustom futon-codex-session-select-mode 'latest
  "How to choose a Codex session when multiple candidates exist."
  :type '(choice (const latest) (const prompt)))

(defcustom futon-codex-session-message-limit nil
  "How many recent messages to scan for Codex requests.
When nil, scan all messages in the session."
  :type '(choice (const nil) integer))

(defcustom futon-codex-report-header "\u2014\u2014REPORT-FOR-CODEX\u2014\u2014"
  "Header line for generated Codex reports."
  :type 'string)

(defcustom futon-codex-report-footer "\u2014\u2014END-REPORT-FOR-CODEX\u2014\u2014"
  "Footer line for generated Codex reports."
  :type 'string)

(defvar futon-helper-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") #'futon-eval-expression-to-kill-ring)
    (define-key map (kbd "r") #'futon-codex-report-to-kill-ring)
    (define-key map (kbd "p") #'futon-codex-report-to-kill-ring-and-insert)
    (define-key map (kbd "c") #'futon-helper-clear-session-cache)
    map)
  "Keymap for Futon helper commands.")

(define-key global-map (kbd "C-c f") futon-helper-map)
(define-key global-map (kbd "<f10>") #'futon-helper-copy-latest-message-to-kill-ring)

(with-eval-after-load 'eat
  (define-key eat-mode-map (kbd "<f8>") #'futon-codex-report-to-kill-ring-and-insert)
  (define-key eat-mode-map (kbd "<f9>") #'futon-codex-eval-requests-to-messages))

(defvar-local futon-codex--session-cache nil
  "Buffer-local cache of the selected Codex session (SESSION-ID . FILE).")

(defun futon-helper--iso-utc (time)
  "Format TIME as an ISO 8601 UTC timestamp with milliseconds."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" time t))

(defun futon-helper--session-since-iso ()
  "Return ISO timestamp for session search cutoff."
  (if (and futon-codex-session-since-hours
           (> futon-codex-session-since-hours 0))
      (futon-helper--iso-utc
       (time-subtract (current-time)
                      (seconds-to-time (* futon-codex-session-since-hours 3600))))
    "1970-01-01T00:00:00.000Z"))

(defun futon-helper--parse-session-line (line)
  "Return (SESSION-ID FILE PREVIEW) parsed from LINE."
  (let* ((parts (split-string line "\t"))
         (session-id (nth 0 parts))
         (file (nth 1 parts))
         (preview (nth 2 parts)))
    (list session-id file preview)))

(defun futon-helper--session-mtime (path)
  (let ((attrs (and path (file-attributes path))))
    (or (and attrs (file-attribute-modification-time attrs))
        (current-time))))

(defun futon-helper--select-session ()
  "Return (SESSION-ID . FILE) for the chosen Codex session."
  (if (and futon-codex--session-cache
           (file-readable-p (cdr futon-codex--session-cache)))
      futon-codex--session-cache
    (let* ((bb (or (and futon-codex-bb-command
                        (executable-find futon-codex-bb-command))
                   (executable-find "bb")))
           (selector futon-codex-session-selector))
      (unless bb
        (user-error "Babashka not found; set futon-codex-bb-command"))
      (unless (and selector (file-readable-p selector))
        (user-error "Session selector not found: %s" selector))
      (let* ((lines (process-lines bb selector
                                   "--since" (futon-helper--session-since-iso)
                                   "--cwd" (directory-file-name
                                            (expand-file-name default-directory)))))
        (unless lines
          (user-error "No Codex sessions found for %s" default-directory))
        (let* ((candidates
                (mapcar (lambda (line)
                          (let* ((parsed (futon-helper--parse-session-line line))
                                 (session-id (nth 0 parsed))
                                 (session-file (nth 1 parsed))
                                 (preview (or (nth 2 parsed)
                                              (futon-helper--session-preview session-file)
                                              "no preview"))
                                 (display (format "%s | %s | %s"
                                                  preview
                                                  session-id
                                                  (file-name-nondirectory session-file))))
                            (cons display parsed)))
                        lines))
               (choice (if (= (length candidates) 1)
                           (car candidates)
                         (pcase futon-codex-session-select-mode
                           ('latest
                            (car (sort (copy-sequence candidates)
                                       (lambda (a b)
                                         (let ((file-a (nth 1 (cdr a)))
                                               (file-b (nth 1 (cdr b))))
                                           (time-less-p (futon-helper--session-mtime file-b)
                                                        (futon-helper--session-mtime file-a)))))))
                           (_
                            (assoc (completing-read "Codex session: "
                                                    (mapcar #'car candidates)
                                                    nil
                                                    t)
                                   candidates)))))
               (parsed (cdr choice))
               (selected (cons (nth 0 parsed) (nth 1 parsed))))
          (unless (and (nth 0 parsed) (nth 1 parsed))
            (user-error "Failed to parse session selection"))
          (setq futon-codex--session-cache selected)
          selected)))))

(defun futon-helper-clear-session-cache ()
  "Clear the buffer-local Codex session cache."
  (interactive)
  (setq futon-codex--session-cache nil)
  (message "Cleared Codex session cache"))

(defun futon-helper-current-session-uuid ()
  "Return the selected Codex session UUID as a string."
  (interactive)
  (let ((session-id (car (futon-helper--select-session))))
    (if (called-interactively-p 'interactive)
        (message "%s" session-id)
      session-id)))

(defun futon-helper--json-parse-line (line)
  "Parse LINE as JSON, returning an object or nil."
  (condition-case nil
      (json-parse-string line
                         :object-type 'alist
                         :object-key-type 'symbol
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    (error
     (condition-case nil
         (let ((json-object-type 'alist)
               (json-array-type 'list)
               (json-false nil)
               (json-null nil))
           (json-read-from-string line))
       (error nil)))))

(defun futon-helper--get (obj key)
  "Return KEY from OBJ, handling alists and hash tables."
  (cond
   ((hash-table-p obj) (or (gethash key obj)
                           (and (symbolp key) (gethash (symbol-name key) obj))
                           (and (stringp key) (gethash (intern key) obj))))
   ((listp obj) (or (alist-get key obj)
                    (and (symbolp key) (alist-get (symbol-name key) obj))
                    (and (stringp key) (alist-get (intern key) obj))))
   (t nil)))

(defun futon-helper--extract-message-text (message)
  "Extract text from a JSON MESSAGE payload."
  (let* ((content (futon-helper--get message 'content))
         (parts (cond
                 ((vectorp content) (append content nil))
                 ((listp content) content)
                 (t nil)))
         (text (when parts
                 (string-trim
                  (mapconcat (lambda (part)
                               (or (futon-helper--get part 'text) ""))
                             parts
                             "")))))
    (or text
        (futon-helper--get message 'message)
        (futon-helper--get message 'text))))

(defun futon-helper--event-message-text (payload role)
  "Return text from an event message PAYLOAD for ROLE."
  (let ((event-type (futon-helper--get payload 'type)))
    (cond
     ((and (string= role "assistant") (string= event-type "agent_message"))
      (or (futon-helper--get payload 'message) (futon-helper--get payload 'text)))
     ((and (string= role "user") (string= event-type "user_message"))
      (or (futon-helper--get payload 'message) (futon-helper--get payload 'text)))
     (t nil))))

(defun futon-helper--last-message-text (session-file role)
  "Return the last message text for ROLE in SESSION-FILE."
  (with-temp-buffer
    (insert-file-contents session-file)
    (let ((last-text nil))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (obj (futon-helper--json-parse-line line))
               (type (futon-helper--get obj 'type))
               (payload (futon-helper--get obj 'payload)))
          (cond
           ((and (string= type "response_item")
                 (string= (futon-helper--get payload 'type) "message")
                 (string= (futon-helper--get payload 'role) role))
            (let ((text (futon-helper--extract-message-text payload)))
              (when (and text (not (string-empty-p text)))
                (setq last-text text))))
           ((string= type "event_msg")
            (let ((text (futon-helper--event-message-text payload role)))
              (when (and text (not (string-empty-p text)))
                (setq last-text text))))))
        (forward-line 1))
      last-text)))

(defun futon-helper--recent-message-texts (session-file &optional limit)
  "Return a list of recent message texts from SESSION-FILE."
  (let ((limit (or limit futon-codex-session-message-limit))
        (texts '()))
    (with-temp-buffer
      (insert-file-contents session-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (obj (futon-helper--json-parse-line line))
               (type (futon-helper--get obj 'type))
               (payload (futon-helper--get obj 'payload))
               (role (futon-helper--get payload 'role))
               (text nil))
          (cond
           ((and (string= type "response_item")
                 (string= (futon-helper--get payload 'type) "message")
                 (member role '("assistant" "user")))
            (setq text (futon-helper--extract-message-text payload)))
           ((string= type "event_msg")
            (setq text (or (futon-helper--event-message-text payload "assistant")
                           (futon-helper--event-message-text payload "user")))))
          (when (and text (not (string-empty-p text)))
            (push text texts)))
        (forward-line 1)))
    (let* ((ordered (nreverse texts))
           (count (length ordered)))
      (if (and (integerp limit) (> count limit))
          (seq-subseq ordered (- count limit))
        ordered))))

(defun futon-helper--last-assistant-message (session-file)
  "Return the last assistant message text from SESSION-FILE."
  (futon-helper--last-message-text session-file "assistant"))

(defun futon-helper--last-user-message (session-file)
  "Return the last user message text from SESSION-FILE."
  (futon-helper--last-message-text session-file "user"))

(defun futon-helper-copy-latest-message-to-kill-ring ()
  "Copy the latest Codex message to the kill ring."
  (interactive)
  (let* ((session (futon-helper--select-session))
         (session-file (cdr session))
         (text (or (futon-helper--last-assistant-message session-file)
                   (futon-helper--last-user-message session-file))))
    (unless (and text (not (string-empty-p text)))
      (user-error "No message text found in the selected session"))
    (kill-new text)
    (message "Copied latest Codex message (%d chars)" (length text))))

(defun futon-helper--session-preview (session-file)
  "Return a short preview string from the last session message."
  (let ((text (or (futon-helper--last-assistant-message session-file)
                  (futon-helper--last-user-message session-file))))
    (when text
      (let ((preview (string-trim (replace-regexp-in-string "[\n\r]+" " " text))))
        (truncate-string-to-width preview futon-codex-session-preview-chars nil nil t)))))
(defun futon-helper--scan-region ()
  "Return cons of (START . END) for the most recent Codex scan region."
  (save-excursion
    (let ((end (point-max)))
      (goto-char end)
      (if (and futon-codex-message-boundary-regexp
               (re-search-backward futon-codex-message-boundary-regexp nil t))
          (cons (match-end 0) end)
        (forward-line (- futon-codex-scan-lines))
        (cons (point) end)))))

(defun futon-helper--extract-request (line)
  "Return (EXPR-TEXT . EXPR) from LINE or nil if it can't be read."
  (let* ((trimmed (string-trim line "[`[:space:]]+" "[`[:space:]]+"))
         (trimmed (string-trim (replace-regexp-in-string
                                futon-codex-request-regexp "" trimmed)))
         (read-result (condition-case nil
                          (read-from-string trimmed)
                        (error nil))))
    (when read-result
      (let* ((expr (car read-result))
             (idx (cdr read-result))
             (rest (string-trim (substring trimmed idx))))
        (when (string-empty-p rest)
          (cons trimmed expr))))))

(defun futon-helper--skip-report-line-p (line)
  (or (string= line futon-codex-report-header)
      (string= line futon-codex-report-footer)))

(defun futon-helper--collect-codex-requests-in-lines (lines)
  "Collect Codex M-: requests from LINES."
  (let ((entries '())
        (in-report nil)
        (i 0)
        (n (length lines)))
    (while (< i n)
      (let ((line (nth i lines)))
        (cond
         ((futon-helper--skip-report-line-p line)
          (setq in-report (string= line futon-codex-report-header))
          (setq i (1+ i)))
         (in-report
          (setq i (1+ i)))
         ((string-match futon-codex-request-regexp line)
          (let ((acc line)
                (j i)
                (request nil))
            (while (and (null request) (< j n))
              (setq request (futon-helper--extract-request acc))
              (when (null request)
                (setq j (1+ j))
                (when (< j n)
                  (setq acc (concat acc "\n" (nth j lines))))))
            (setq i (1+ j))
            (when request
              (let* ((expr-text (car request))
                     (expr (cdr request))
                     (errorp nil)
                     (result (condition-case err
                                 (eval expr t)
                               (error
                                (setq errorp t)
                                err)))
                     (entry (list :expr-text expr-text
                                  :result result
                                  :error errorp)))
                (push entry entries)))))
         (t (setq i (1+ i))))))
    (let ((seen (make-hash-table :test 'equal))
          (uniq '()))
      (dolist (entry (nreverse entries))
        (let ((key (plist-get entry :expr-text)))
          (unless (gethash key seen)
            (puthash key t seen)
            (push entry uniq))))
      (nreverse uniq))))

(defun futon-helper--collect-codex-requests (start end)
  "Collect Codex M-: requests in region between START and END."
  (let ((lines (split-string (buffer-substring-no-properties start end) "\n")))
    (futon-helper--collect-codex-requests-in-lines lines)))

(defun futon-helper--format-codex-report (entries)
  "Format ENTRIES as a Codex report block."
  (let ((body (mapconcat
               (lambda (entry)
                 (let ((expr-text (plist-get entry :expr-text))
                       (result (plist-get entry :result))
                       (errorp (plist-get entry :error)))
                   (if errorp
                       (format "M-: %s\n!! %s"
                               expr-text
                               (error-message-string result))
                     (format "M-: %s\n=> %s"
                             expr-text
                             (prin1-to-string result)))))
               entries
               "\n")))
    (string-join (list futon-codex-report-header body futon-codex-report-footer) "\n")))

(defun futon-helper--collect-codex-entries ()
  "Return entries for the current Codex requests."
  (let ((entries
         (pcase futon-codex-report-source
           ('buffer
            (unless (derived-mode-p 'eat-mode)
              (user-error "Not in an eat buffer"))
            (let ((region (futon-helper--scan-region)))
              (futon-helper--collect-codex-requests (car region) (cdr region))))
           ('session
            (let* ((session (futon-helper--select-session))
                   (session-file (cdr session))
                   (texts (futon-helper--recent-message-texts session-file))
                   (lines (and texts (split-string (string-join texts "\n") "\n")))
                   (entries (and lines
                                 (futon-helper--collect-codex-requests-in-lines lines))))
              (or entries
                  (user-error "No M-: requests found in %s" session-file))))
           (_ (user-error "Unknown report source: %s" futon-codex-report-source)))))
    (unless entries
      (user-error "No M-: requests found in recent output"))
    entries))

(defun futon-helper--build-codex-report ()
  "Return cons of (REPORT . COUNT) for the current Codex requests."
  (let ((entries (futon-helper--collect-codex-entries)))
    (cons (futon-helper--format-codex-report entries) (length entries))))

(defun futon-helper--insert-report (report)
  "Insert REPORT into the current eat buffer."
  (unless (derived-mode-p 'eat-mode)
    (user-error "Not in an eat buffer"))
  (kill-new report)
  (if (fboundp 'eat-yank)
      (eat-yank)
    (yank)))

;; Eval helper: eval last sexp (or prompt with prefix) and copy the result.
(defun futon-eval-expression-to-kill-ring (&optional prompt)
  "Eval last sexp (or PROMPT for one) and push the result to the kill ring."
  (interactive "P")
  (let* ((expr (if prompt
                   (read--expression "Eval and copy: ")
                 (preceding-sexp)))
         (result (eval expr t))
         (printed (prin1-to-string result)))
    (kill-new printed)
    (message "Copied eval result: %s"
             (truncate-string-to-width printed 120 nil nil t))))

(defun futon-codex-report-to-kill-ring ()
  "Collect Codex eval requests and copy a report to the kill ring."
  (interactive)
  (let* ((result (futon-helper--build-codex-report))
         (report (car result))
         (count (cdr result)))
    (kill-new report)
    (message "Copied Codex report with %d entr%s"
             count
             (if (= count 1) "y" "ies"))))

(defun futon-codex-eval-requests-to-messages ()
  "Eval Codex requests and echo results to *Messages*."
  (interactive)
  (let ((entries (futon-helper--collect-codex-entries)))
    (dolist (entry entries)
      (let ((expr-text (plist-get entry :expr-text))
            (result (plist-get entry :result))
            (errorp (plist-get entry :error)))
        (if errorp
            (message "M-: %s => !! %s"
                     expr-text
                     (error-message-string result))
          (message "M-: %s => %s"
                   expr-text
                   (prin1-to-string result)))))))

(defun futon-codex-report-to-kill-ring-and-insert ()
  "Collect Codex eval requests, copy a report, and insert into the eat buffer."
  (interactive)
  (let* ((result (futon-helper--build-codex-report))
         (report (car result))
         (count (cdr result)))
    (futon-helper--insert-report report)
    (message "Inserted Codex report with %d entr%s"
             count
             (if (= count 1) "y" "ies"))))

(global-set-key (kbd "M-;") #'futon-eval-expression-to-kill-ring)

(provide 'futon-helper)

;;; futon-helper.el ends here
