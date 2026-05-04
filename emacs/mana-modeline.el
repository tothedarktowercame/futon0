;;; mana-modeline.el --- Per-session mana balance lighter for the modeline -*- lexical-binding: t; -*-

;;; Commentary:
;; A small modeline lighter that shows the current operator session's
;; mana balance (per V-6 amendment II of M-bounded-in-flight-state:
;; "drain measures per-repo; balance aggregates per-session").
;;
;; The lighter polls the nonstarter HTTP API at
;; `mana-modeline-base-url' (default http://localhost:7072) for
;; `GET /api/mana?session-id=<id>' and renders a tiny tier-coloured
;; glyph + balance number in the modeline.
;;
;; Session-id formation is currently delegated to the operator: set
;; `mana-modeline-session-id' (a buffer-local or global var) to the
;; live session-id, or supply a function via
;; `mana-modeline-session-id-fn' that returns one on demand.  When no
;; session-id is set the lighter renders a polite '?'.
;;
;; Enable with `(mana-modeline-mode 1)' (global minor mode).

;;; Code:

(require 'json)
(require 'url)
(require 'subr-x)

(defgroup mana-modeline nil
  "Per-session mana balance lighter for the modeline."
  :group 'tatami-integration)

(defcustom mana-modeline-base-url "http://localhost:7072"
  "Base URL of the nonstarter mana API.
Per ./scripts/dev-laptop-env, nonstarter listens on :7072 by default."
  :type 'string
  :group 'mana-modeline)

(defcustom mana-modeline-poll-interval 30
  "How often (seconds) to refresh the mana balance via HTTP.
Polling is asynchronous; a stale value renders until the next refresh
returns."
  :type 'integer
  :group 'mana-modeline)

(defcustom mana-modeline-request-timeout 1.5
  "HTTP request timeout in seconds."
  :type 'number
  :group 'mana-modeline)

(defcustom mana-modeline-session-id nil
  "Override session-id for buffers that don't carry their own.
Buffer-local variables take precedence (see
`mana-modeline-session-id-buffer-locals').  When neither this nor a
buffer-local var is set, the lighter renders \"?\" politely."
  :type '(choice (const :tag "Read from buffer-local vars only" nil)
                 string)
  :group 'mana-modeline)

(defcustom mana-modeline-session-id-fn nil
  "Optional zero-arg function returning the current session-id.
Called in the buffer whose modeline is being rendered.  Takes
precedence over `mana-modeline-session-id-buffer-locals' and
`mana-modeline-session-id'."
  :type '(choice (const :tag "Use buffer-local vars" nil)
                 function)
  :group 'mana-modeline)

(defcustom mana-modeline-session-id-buffer-locals
  '(mana-session-id
    claude-repl--evidence-session-id
    codex-repl--evidence-session-id)
  "Buffer-local variable names to consult, in order, for a session-id.
The first that is bound and non-nil wins.  Per Joe's D-04 resolution:
buffers like *claude-repl:claude-3* correspond to sessions; the
session-id is buffer-local and survives renames."
  :type '(repeat symbol)
  :group 'mana-modeline)

(defcustom mana-modeline-agent-id-buffer-locals
  '(mana-agent-id
    claude-repl-agent-id
    codex-repl-agency-agent-id)
  "Buffer-local variable names to consult for an agent-id.
Used for the lighter's tooltip; the API call is keyed by session-id
alone (one agent → one session, by I-1 of futon3c)."
  :type '(repeat symbol)
  :group 'mana-modeline)

(defface mana-modeline-positive
  '((t :foreground "#86efac"))
  "Face for non-negative balance."
  :group 'mana-modeline)

(defface mana-modeline-negative
  '((t :foreground "#f87171" :weight bold))
  "Face for negative balance (allostatic load exceeds awards)."
  :group 'mana-modeline)

(defface mana-modeline-no-session
  '((t :foreground "#9ca3af" :slant italic))
  "Face for the no-session state (lighter renders '?')."
  :group 'mana-modeline)

(defface mana-modeline-error
  '((t :foreground "#9ca3af" :slant italic))
  "Face for transient fetch errors / nonstarter unavailable."
  :group 'mana-modeline)

(defvar mana-modeline--balance-cache (make-hash-table :test 'equal)
  "Map session-id → plist (:balance N :fetched-at TIME :error? bool).
Populated by `mana-modeline-refresh' (a periodic timer or interactive
call) for all session-ids seen across visible buffers.  The lighter
reads this cache on each modeline render — never blocks.")

(defvar mana-modeline--timer nil
  "The polling timer, or nil when the mode is disabled.")

(defun mana-modeline--read-buffer-local (vars)
  "Return the first bound, non-nil value of VARS in the current buffer."
  (catch 'found
    (dolist (v vars)
      (when (and (boundp v)
                 (not (null (symbol-value v)))
                 (not (and (stringp (symbol-value v))
                           (string-empty-p (symbol-value v)))))
        (throw 'found (symbol-value v))))
    nil))

(defun mana-modeline--current-session-id ()
  "Return the session-id for the current buffer, or nil.
Order: `mana-modeline-session-id-fn' (if set) →
buffer-local vars from `mana-modeline-session-id-buffer-locals' →
`mana-modeline-session-id' (global override)."
  (or (when (functionp mana-modeline-session-id-fn)
        (condition-case _err
            (funcall mana-modeline-session-id-fn)
          (error nil)))
      (mana-modeline--read-buffer-local mana-modeline-session-id-buffer-locals)
      (when (and (stringp mana-modeline-session-id)
                 (not (string-empty-p mana-modeline-session-id)))
        mana-modeline-session-id)))

(defun mana-modeline--current-agent-id ()
  "Return the agent-id for the current buffer, or nil.  Used for
tooltip text only."
  (mana-modeline--read-buffer-local mana-modeline-agent-id-buffer-locals))

(defun mana-modeline--known-session-ids ()
  "Walk visible windows + cached entries and return the set of
session-ids worth polling."
  (let ((sids (make-hash-table :test 'equal)))
    (dolist (win (window-list nil 'no-mini))
      (with-current-buffer (window-buffer win)
        (when-let* ((sid (mana-modeline--current-session-id)))
          (puthash sid t sids))))
    (maphash (lambda (k _v) (puthash k t sids)) mana-modeline--balance-cache)
    (hash-table-keys sids)))

(defun mana-modeline--format-balance (balance)
  "Format BALANCE (a number) for the modeline."
  (cond
   ((null balance) "?")
   ((and (numberp balance) (integerp balance)) (format "%d" balance))
   ((numberp balance) (format "%.1f" balance))
   (t (format "%s" balance))))

(defun mana-modeline--render-from-cache ()
  "Compute the modeline string for the *current buffer*.
Reads the buffer-local session-id, then looks up the cached balance
(if any).  Never blocks — never makes an HTTP call.  Refresh happens
out-of-band via the timer or a manual `mana-modeline-refresh' call."
  (let* ((sid (mana-modeline--current-session-id)))
    (cond
     ((null sid)
      "")    ; render nothing in non-session buffers (cleaner than '?')

     (t
      (let* ((entry (gethash sid mana-modeline--balance-cache))
             (balance (plist-get entry :balance))
             (error? (plist-get entry :error?))
             (agent (mana-modeline--current-agent-id))
             (tip (format "session=%s%s%s"
                          sid
                          (if agent (format " agent=%s" agent) "")
                          (cond
                           (error? " (nonstarter unreachable)")
                           ((null entry) " (refreshing…)")
                           (t (format " balance=%s"
                                      (mana-modeline--format-balance balance)))))))
        (cond
         (error?
          (propertize " 🜋·" 'face 'mana-modeline-error 'help-echo tip))
         ((null entry)
          (propertize " 🜋…" 'face 'mana-modeline-no-session 'help-echo tip))
         (t
          (let ((face (if (and (numberp balance) (< balance 0))
                          'mana-modeline-negative
                        'mana-modeline-positive)))
            (propertize (format " 🜋%s" (mana-modeline--format-balance balance))
                        'face face 'help-echo tip)))))))))

(defun mana-modeline--fetch-async (session-id callback)
  "Asynchronously GET nonstarter's /api/mana for SESSION-ID, then CALLBACK with
the parsed alist (or nil on failure)."
  (let ((url-request-method "GET")
        (url-request-extra-headers nil)
        (url (format "%s/api/mana?session-id=%s"
                     mana-modeline-base-url
                     (url-hexify-string session-id))))
    (url-retrieve
     url
     (lambda (status &rest _)
       (let ((parsed
              (condition-case _err
                  (when (and (not (plist-get status :error))
                             (search-forward "\n\n" nil t))
                    (let ((json-object-type 'alist)
                          (json-array-type 'list)
                          (json-key-type 'symbol))
                      (json-read)))
                (error nil))))
         (kill-buffer (current-buffer))
         (funcall callback parsed)))
     nil
     'silent
     'inhibit-cookies)))

(defun mana-modeline--refresh-one (sid)
  "Refresh the cache entry for SID asynchronously."
  (mana-modeline--fetch-async
   sid
   (lambda (parsed)
     (puthash sid
              (list :balance (and parsed (cdr (assq 'balance parsed)))
                    :fetched-at (current-time)
                    :error? (null parsed))
              mana-modeline--balance-cache)
     (force-mode-line-update t))))

(defun mana-modeline-refresh ()
  "Refresh balance cache for every session-id seen in any visible
buffer (or already cached).  Idempotent; safe from timer or manual."
  (interactive)
  (dolist (sid (mana-modeline--known-session-ids))
    (mana-modeline--refresh-one sid)))

(defun mana-modeline--start-timer ()
  (when mana-modeline--timer (cancel-timer mana-modeline--timer))
  (setq mana-modeline--timer
        (run-with-timer 0 mana-modeline-poll-interval
                        #'mana-modeline-refresh)))

(defun mana-modeline--stop-timer ()
  (when mana-modeline--timer
    (cancel-timer mana-modeline--timer)
    (setq mana-modeline--timer nil)))

;;;###autoload
(define-minor-mode mana-modeline-mode
  "Show a per-session mana balance lighter in the modeline.
The lighter renders only in buffers that carry a session-id (per
`mana-modeline-session-id-buffer-locals'); other buffers show
nothing.  Balance values are read from a cache refreshed out-of-band
every `mana-modeline-poll-interval' seconds — modeline render
itself never blocks on HTTP."
  :init-value nil
  :global t
  :lighter (:eval (mana-modeline--render-from-cache))
  (if mana-modeline-mode
      (mana-modeline--start-timer)
    (mana-modeline--stop-timer)))

(provide 'mana-modeline)
;;; mana-modeline.el ends here
