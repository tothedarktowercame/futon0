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
  "Session-id for the current operator session.
Set this directly, or supply `mana-modeline-session-id-fn' for dynamic
lookup."
  :type '(choice (const :tag "Use session-id-fn" nil) string)
  :group 'mana-modeline)

(defcustom mana-modeline-session-id-fn nil
  "Optional zero-arg function returning the current session-id.
Takes precedence over `mana-modeline-session-id' when non-nil."
  :type '(choice (const :tag "Use mana-modeline-session-id" nil)
                 function)
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

(defvar mana-modeline--current-text " 🜋?"
  "The string currently rendered in the modeline.
Refreshed periodically; do not mutate by hand.")

(defvar mana-modeline--timer nil
  "The polling timer, or nil when the mode is disabled.")

(defun mana-modeline--current-session-id ()
  "Return the session-id to query, or nil."
  (cond
   ((functionp mana-modeline-session-id-fn)
    (condition-case _err
        (funcall mana-modeline-session-id-fn)
      (error nil)))
   ((and (stringp mana-modeline-session-id)
         (not (string-empty-p mana-modeline-session-id)))
    mana-modeline-session-id)
   (t nil)))

(defun mana-modeline--format-balance (balance)
  "Format BALANCE (a number) for the modeline."
  (cond
   ((null balance) "?")
   ((and (numberp balance) (integerp balance)) (format "%d" balance))
   ((numberp balance) (format "%.1f" balance))
   (t (format "%s" balance))))

(defun mana-modeline--render (balance &optional error?)
  "Build the modeline string for BALANCE.  ERROR? when fetch failed."
  (cond
   (error?
    (propertize " 🜋·" 'face 'mana-modeline-error
                'help-echo "mana-modeline: nonstarter unreachable"))
   ((null balance)
    (propertize " 🜋?" 'face 'mana-modeline-no-session
                'help-echo "mana-modeline: no session-id; set mana-modeline-session-id"))
   (t
    (let* ((face (if (and (numberp balance) (< balance 0))
                     'mana-modeline-negative
                   'mana-modeline-positive))
           (text (format " 🜋%s" (mana-modeline--format-balance balance))))
      (propertize text 'face face
                  'help-echo (format "Session mana balance: %s" balance))))))

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
                          (json-key-type 'symbol)))
                    (json-read))
                (error nil))))
         (kill-buffer (current-buffer))
         (funcall callback parsed)))
     nil
     'silent
     'inhibit-cookies)))

(defun mana-modeline-refresh ()
  "Fetch the current session balance and update the modeline string.
Idempotent; safe to call from a timer or interactively."
  (interactive)
  (let ((sid (mana-modeline--current-session-id)))
    (cond
     ((null sid)
      (setq mana-modeline--current-text (mana-modeline--render nil))
      (force-mode-line-update t))
     (t
      (mana-modeline--fetch-async
       sid
       (lambda (parsed)
         (let ((balance (and parsed (cdr (assq 'balance parsed)))))
           (setq mana-modeline--current-text
                 (mana-modeline--render balance (null parsed)))
           (force-mode-line-update t))))))))

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
  "Show a per-session mana balance lighter in the modeline."
  :init-value nil
  :global t
  :lighter (:eval mana-modeline--current-text)
  (if mana-modeline-mode
      (mana-modeline--start-timer)
    (mana-modeline--stop-timer)))

(provide 'mana-modeline)
;;; mana-modeline.el ends here
