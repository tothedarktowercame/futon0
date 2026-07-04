;;; repl-reaper.el --- Reap stale Agency agents from Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; As-needed cleanup for the Agency roster.  A registered agent has a footprint
;; in two places:
;;
;;   - the Agency registry (JVM, port 7070) — cleared with DELETE
;;     /api/alpha/agents/:id
;;   - this Emacs (its `*<type>-repl:<id>*' buffer)
;;
;; `repl-reaper-list' shows agents idle for >= `repl-reaper-idle-threshold-hours'
;; (dry-run).  `repl-reaper-reap' cleans up both places after a confirm.
;;
;; Agents currently `invoking', and any id in `repl-reaper-protected-agent-ids',
;; are never reaped.  Reaping is reversible: a reaped agent re-registers/restores
;; on its next allocation, and the underlying session transcripts under
;; ~/.claude/projects are untouched.
;;
;; This is the Emacs port of futon3c/scripts/reap_idle_agents.py.  It runs inside
;; the operator Emacs, so it kills buffers directly rather than over emacsclient.
;;
;; Paired invoke buffers: each REPL buffer `*<runtime>-repl:<id>*' has a
;; companion `*invoke: <id>*' (claude) or `*invoke: <runtime>:<id>*' (codex lane)
;; buffer holding the raw invoke transcript.  This module keeps the pair together
;; on teardown: `C-x C-k' in a REPL buffer kills both, a buffer-local
;; `kill-buffer-hook' cascades from any other kill path, and the reaper clears
;; both when it deregisters an agent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'iso8601)
(require 'url)  ; makes `url-request-method' a special var so our let binds it dynamically

(defgroup repl-reaper nil
  "Reap stale Agency agents from Emacs."
  :group 'futon-hot)

(defcustom repl-reaper-api-url "http://localhost:7070"
  "Base URL of the Agency HTTP API."
  :type 'string
  :group 'repl-reaper)

(defcustom repl-reaper-idle-threshold-hours 15.0
  "Minimum idle hours (since `last-active') before an agent is a reap candidate."
  :type 'number
  :group 'repl-reaper)

(defcustom repl-reaper-protected-agent-ids nil
  "Agent IDs that are never reaped regardless of idle time.
Currently-`invoking' agents are always protected too, independent of
this list."
  :type '(repeat string)
  :group 'repl-reaper)

(defconst repl-reaper--buffer "*repl-reaper*"
  "Name of the buffer showing reaper candidates and results.")

;;; --- HTTP ------------------------------------------------------------------

(defun repl-reaper--url (path)
  (concat (string-trim-right repl-reaper-api-url "/") path))

(defun repl-reaper--fetch-json (url &optional method)
  "GET (or METHOD) URL and parse the JSON body as an alist, or nil on failure."
  (condition-case nil
      (let ((url-request-method (or method "GET")))
        (when-let ((buf (url-retrieve-synchronously url t t 5.0)))
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (re-search-forward "\n\n" nil t)
                  (json-parse-buffer :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)))
            (kill-buffer buf))))
    (error nil)))

(defun repl-reaper--agents ()
  "Return the roster as a list of (ID . ALIST) pairs."
  (let ((agents (alist-get 'agents (repl-reaper--fetch-json
                                    (repl-reaper--url "/api/alpha/agents")))))
    (mapcar (lambda (pair) (cons (symbol-name (car pair)) (cdr pair))) agents)))

;;; --- Candidate selection ---------------------------------------------------

(defun repl-reaper--idle-hours (agent)
  "Hours since AGENT's `last-active', or nil if unknown."
  (when-let ((la (alist-get 'last-active agent)))
    ;; Drop sub-second precision — the API emits 9-digit fractions that some
    ;; iso8601 parsers reject, and the threshold is hours-scale anyway.
    (let ((clean (replace-regexp-in-string "\\.[0-9]+" "" la)))
      (condition-case nil
          (/ (float-time (time-subtract (current-time)
                                        (encode-time (iso8601-parse clean))))
             3600.0)
        (error nil)))))

(defun repl-reaper--emacs-socket (agent)
  (alist-get 'emacs-socket (alist-get 'metadata agent)))

(defun repl-reaper--type (agent)
  (or (alist-get 'type agent) "claude"))

(defun repl-reaper--candidates ()
  "Return reap candidates as (ID HOURS AGENT) triples, oldest-idle first."
  (let (out)
    (dolist (pair (repl-reaper--agents))
      (let* ((id (car pair)) (agent (cdr pair))
             (hrs (repl-reaper--idle-hours agent)))
        (unless (or (member id repl-reaper-protected-agent-ids)
                    (equal (alist-get 'status agent) "invoking")
                    (null hrs)
                    (< hrs repl-reaper-idle-threshold-hours))
          (push (list id hrs agent) out))))
    (sort out (lambda (a b) (> (nth 1 a) (nth 1 b))))))

;;; --- Reaping ---------------------------------------------------------------

(defun repl-reaper--kill-buffer (id type)
  "Kill the `*TYPE-repl:ID*' buffer in this Emacs. Return a status keyword."
  (let ((name (format "*%s-repl:%s*" type id)))
    (if-let ((buf (get-buffer name)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf)
          :buffer-killed)
      :buffer-absent)))

(defun repl-reaper--deregister (id)
  "DELETE ID from the Agency registry. Return a status keyword or string."
  (let ((resp (repl-reaper--fetch-json
               (repl-reaper--url (format "/api/alpha/agents/%s" id))
               "DELETE")))
    (if (eq (alist-get 'ok resp) t) :deregistered
      (format "jvm-fail:%s" (or (alist-get 'error resp) "no-response")))))

(defun repl-reaper--invoke-buffer-names (repl-buffer-name)
  "Return candidate `*invoke: ...*' buffer names paired with REPL-BUFFER-NAME.
REPL-BUFFER-NAME is like `*claude-repl:claude-3*' or `*codex-repl:fable-1*'.
Both the id-keyed form (`*invoke: claude-3*') and the runtime-lane form
\(`*invoke: codex-repl:fable-1*') are returned so either convention is covered."
  (when (and (stringp repl-buffer-name)
             (string-match "\\`\\*\\([^:]+\\):\\(.+\\)\\*\\'" repl-buffer-name))
    (let ((runtime (match-string 1 repl-buffer-name))   ; e.g. "claude-repl"
          (agent   (match-string 2 repl-buffer-name)))  ; e.g. "claude-3"
      (delete-dups
       (list (format "*invoke: %s*" agent)
             (format "*invoke: %s:%s*" runtime agent))))))

(defun repl-reaper--kill-paired-invoke-buffers (repl-buffer-name)
  "Kill live invoke buffers paired with REPL-BUFFER-NAME. Return killed names."
  (let (killed)
    (dolist (name (repl-reaper--invoke-buffer-names repl-buffer-name))
      (when-let ((buf (get-buffer name)))
        (when (buffer-live-p buf)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf))
          (push name killed))))
    (nreverse killed)))

(defun repl-reaper--reap-one (id agent)
  "Reap one AGENT: kill its REPL + paired invoke buffer(s) and deregister it.
Return the list of status values."
  (let ((type (repl-reaper--type agent)) (results '()))
    (when (repl-reaper--emacs-socket agent)
      (let* ((repl-name (format "*%s-repl:%s*" type id))
             ;; Kill invoke buffers first, before the REPL buffer's own
             ;; kill-buffer-hook would race to do it — so the count is accurate.
             (invoke-killed (repl-reaper--kill-paired-invoke-buffers repl-name)))
        (push (repl-reaper--kill-buffer id type) results)
        (when invoke-killed
          (push (format "invoke-killed(%d)" (length invoke-killed)) results))))
    (push (repl-reaper--deregister id) results)
    (nreverse results)))

;;; --- Pair killing: C-x C-k and kill-buffer-hook ----------------------------

(defun repl-reaper--repl-kill-hook ()
  "Buffer-local `kill-buffer-hook': also kill this REPL's paired invoke buffer(s).
Cascades from any kill path (\\[kill-buffer], the reaper, buffer-cleaner)."
  (ignore-errors
    (repl-reaper--kill-paired-invoke-buffers (buffer-name))))

(defun repl-reaper--install-repl-kill-hook ()
  "Arm the paired-invoke kill hook in the current REPL buffer."
  (add-hook 'kill-buffer-hook #'repl-reaper--repl-kill-hook nil t))

;;;###autoload
(defun repl-reaper-kill-repl ()
  "Kill the current REPL buffer and its paired `*invoke: ...*' buffer(s).
Bound to \\`C-x C-k' in `claude-repl-mode' and `codex-repl-mode'."
  (interactive)
  ;; Kill the invoke buffer(s) explicitly so this works even where the
  ;; kill-buffer-hook was never armed; then kill the REPL buffer itself.
  (repl-reaper--kill-paired-invoke-buffers (buffer-name))
  (kill-buffer (current-buffer)))

;;; --- Display ---------------------------------------------------------------

(defun repl-reaper--render (title lines)
  (with-current-buffer (get-buffer-create repl-reaper--buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert title "\n\n")
      (dolist (l lines) (insert l "\n")))
    (goto-char (point-min))
    (special-mode)
    (display-buffer (current-buffer))))

(defun repl-reaper--fmt (id hrs agent &optional suffix)
  (format "  %-14s idle %5.1fh  type=%-6s emacs=%s%s"
          id hrs (repl-reaper--type agent)
          (or (repl-reaper--emacs-socket agent) "-")
          (or suffix "")))

;;;###autoload
(defun repl-reaper-list ()
  "Show Agency agents idle >= `repl-reaper-idle-threshold-hours' (dry-run)."
  (interactive)
  (let ((cands (repl-reaper--candidates)))
    (if (null cands)
        (message "repl-reaper: no agents idle >= %.1fh"
                 repl-reaper-idle-threshold-hours)
      (repl-reaper--render
       (format "DRY-RUN — %d agent(s) idle >= %.1fh (M-x repl-reaper-reap to clean up)"
               (length cands) repl-reaper-idle-threshold-hours)
       (mapcar (lambda (c) (apply #'repl-reaper--fmt c)) cands)))))

;;;###autoload
(defun repl-reaper-reap (&optional hours)
  "Reap Agency agents idle >= `repl-reaper-idle-threshold-hours', after a confirm.
Kills each agent's Emacs REPL buffer and deregisters it from the JVM.

With \\[universal-argument] prefix, prompt for the idle threshold in
hours for this invocation only. A plain numeric prefix (e.g. M-8) is
used directly as the hours. The `repl-reaper-idle-threshold-hours'
custom itself is never changed."
  (interactive
   (list (cond ((consp current-prefix-arg)
                (read-number "Reap agents idle >= hours: "
                             repl-reaper-idle-threshold-hours))
               (current-prefix-arg
                (prefix-numeric-value current-prefix-arg)))))
  (let* ((repl-reaper-idle-threshold-hours
          (or hours repl-reaper-idle-threshold-hours))
         (cands (repl-reaper--candidates)))
    (cond
     ((null cands)
      (message "repl-reaper: no agents idle >= %.1fh"
               repl-reaper-idle-threshold-hours))
     ((yes-or-no-p (format "Reap %d agent(s) idle >= %.1fh (%s)? "
                           (length cands) repl-reaper-idle-threshold-hours
                           (mapconcat #'car cands ", ")))
      (repl-reaper--render
       (format "REAPED %d agent(s) idle >= %.1fh" (length cands)
               repl-reaper-idle-threshold-hours)
       (mapcar
        (lambda (c)
          (cl-destructuring-bind (id hrs agent) c
            (let ((res (repl-reaper--reap-one id agent)))
              (repl-reaper--fmt
               id hrs agent
               (format "  ->  %s"
                       (mapconcat (lambda (r) (if (keywordp r)
                                                  (substring (symbol-name r) 1)
                                                r))
                                  res ", "))))))
        cands)))
     (t (message "repl-reaper: cancelled")))))

;;; --- Wiring ----------------------------------------------------------------

;; `C-x C-k' in a REPL buffer kills the REPL and its paired invoke buffer.
;; (It was the kmacro prefix there before — REPLs don't record macros.)
(with-eval-after-load 'claude-repl
  (when (boundp 'claude-repl-mode-map)
    (define-key claude-repl-mode-map (kbd "C-x C-k") #'repl-reaper-kill-repl)))
(with-eval-after-load 'codex-repl
  (when (boundp 'codex-repl-mode-map)
    (define-key codex-repl-mode-map (kbd "C-x C-k") #'repl-reaper-kill-repl)))

;; Arm the cascade hook on new REPL buffers, and retroactively on any that
;; already exist when this file loads.
(add-hook 'claude-repl-mode-hook #'repl-reaper--install-repl-kill-hook)
(add-hook 'codex-repl-mode-hook #'repl-reaper--install-repl-kill-hook)
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (memq major-mode '(claude-repl-mode codex-repl-mode))
      (repl-reaper--install-repl-kill-hook))))

(provide 'repl-reaper)

;;; repl-reaper.el ends here
