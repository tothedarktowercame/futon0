;;; futon-buffer-cleaner.el --- Periodic stale buffer cleanup -*- lexical-binding: t; -*-

;;; Commentary:
;; Conservative cleanup for long-lived Futon Emacs sessions.  The cleaner
;; removes recreateable internal buffers while preserving visible buffers,
;; file-visiting buffers, live process buffers, and the configured active
;; Claude/Codex lanes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup futon-buffer-cleaner nil
  "Periodic stale buffer cleanup for Futon Emacs sessions."
  :group 'futon-hot)

(defcustom futon-buffer-cleaner-interval (* 60 60)
  "Seconds between automatic stale-buffer cleanup passes."
  :type 'integer
  :group 'futon-buffer-cleaner)

(defcustom futon-buffer-cleaner-active-agent-ids
  '("claude-3" "fable-1" "fable-2" "codex-1")
  "Agent IDs whose REPL and invoke buffers should not be cleaned.
This list drifts as the roster changes — when cleanup leaves stale
*invoke:* buffers behind (or eats a live lane), update it to the
current agency roster."
  :type '(repeat string)
  :group 'futon-buffer-cleaner)

(defcustom futon-buffer-cleaner-stream-buffer-regexp
  "^ \\*codex-repl-stream\\*"
  "Regexp matching hidden per-invocation stream buffers.
These leak one buffer per codex-repl call (census 2026-06-11: 101 of
295 live buffers) and are killable as soon as their process is gone."
  :type 'regexp
  :group 'futon-buffer-cleaner)

(defcustom futon-buffer-cleaner-render-buffer-regexps
  '("^\\*Arxana Essay \\|^\\*Arxana Essay (compiled)\\|^\\*Arxana Essay Pair\\|^\\*Arxana Essays Audit\\*\\|^\\*Arxana Essay Notes\\*\\|^\\*Arxana Essay Compiled Notes\\*")
  "Regexps matching regenerable render buffers.
Renders mark themselves modified by inserting their content, so the
ordinary unmodified-only guard never catches them; they are killed
even when modified — nothing user-authored lives in a render.
\(*Arxana Browser* is NOT here: it holds live navigation state.)"
  :type '(repeat regexp)
  :group 'futon-buffer-cleaner)

(defcustom futon-buffer-cleaner-file-stale-age (* 6 60 60)
  "Seconds since last display before an unmodified file buffer is stale.
Stale file buffers are killed — they reload from disk on next visit.
Buffers never displayed at all count as stale. Modified buffers and
buffers with pending emacsclient clients are always preserved.
Set to nil to disable file-buffer cleanup."
  :type '(choice (const :tag "Never clean file buffers" nil)
                 integer)
  :group 'futon-buffer-cleaner)

(defcustom futon-buffer-cleaner-http-buffer-regexp
  "^ \\*http \\(127\\.0\\.0\\.1\\|localhost\\):707[01]\\*"
  "Regexp matching stale hidden HTTP request buffers."
  :type 'regexp
  :group 'futon-buffer-cleaner)

(defcustom futon-buffer-cleaner-temp-modes
  '(help-mode
    Buffer-menu-mode
    calendar-mode
    special-mode
    fundamental-mode
    org-mode
    shell-command-mode)
  "Major modes considered safe to clean when unmodified and not visible.
\(`arxana-browser-mode' was removed 2026-06-11: the only buffer in that
mode is *Arxana Browser*, whose navigation stack is live state — it was
one frame-switch away from being eaten.)"
  :type '(repeat symbol)
  :group 'futon-buffer-cleaner)

(defcustom futon-buffer-cleaner-message-threshold 1
  "Minimum number of killed buffers before logging a cleanup message.
Set to nil to disable automatic cleanup messages."
  :type '(choice (const :tag "Never message" nil)
                 integer)
  :group 'futon-buffer-cleaner)

(defvar futon-buffer-cleaner--timer nil
  "Timer object for periodic stale-buffer cleanup.")

(defvar futon-buffer-cleaner--last-report nil
  "Most recent plist returned by `futon-buffer-cleaner-clean-now'.")

(defun futon-buffer-cleaner--visible-buffers ()
  "Return a hash table of buffers currently visible in any frame."
  (let ((visible (make-hash-table :test 'eq)))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (puthash (window-buffer window) t visible)))
    visible))

(defun futon-buffer-cleaner--active-agent-buffer-p (name)
  "Return non-nil when buffer NAME belongs to a configured active agent."
  (and (stringp name)
       (cl-some
        (lambda (agent-id)
          ;; lane prefix is the RUNTIME, not the agent family: fable
          ;; agents run in claude repls (*claude-repl:fable-1*), so try
          ;; the id-derived prefix AND both runtime prefixes
          (let ((prefixes (delete-dups
                           (list (car (split-string agent-id "-"))
                                 "claude" "codex"))))
            (or (cl-some (lambda (p)
                           (or (string= name (format "*%s-repl:%s*" p agent-id))
                               (string= name (format "*invoke: %s-repl:%s*"
                                                     p agent-id))))
                         prefixes)
                (string= name (format "*invoke: %s*" agent-id)))))
        futon-buffer-cleaner-active-agent-ids)))

(defun futon-buffer-cleaner--kill-buffer (buffer)
  "Kill BUFFER without prompting about internal modified state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless buffer-file-name
        (set-buffer-modified-p nil)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer))))

(defun futon-buffer-cleaner--candidate-kind (buffer visible)
  "Return cleanup kind for BUFFER, or nil when it should be preserved.
VISIBLE is the hash table returned by `futon-buffer-cleaner--visible-buffers'."
  (when (and (buffer-live-p buffer)
             (not (gethash buffer visible))
             (not (get-buffer-process buffer)))
    (with-current-buffer buffer
      (let ((name (buffer-name buffer)))
        (cond
         ((or (futon-buffer-cleaner--active-agent-buffer-p name)
              (string-prefix-p " *Minibuf-" name)
              (string-prefix-p " *Echo Area " name)
              (string= name "*scratch*")
              ;; live navigation state, not a render — never cleaned
              (string= name "*Arxana Browser*"))
          nil)
         ;; unmodified file buffers reload from disk; kill when not
         ;; displayed for a while (and no emacsclient is waiting on them)
         (buffer-file-name
          (when (and futon-buffer-cleaner-file-stale-age
                     (not (buffer-modified-p))
                     (not (bound-and-true-p server-buffer-clients))
                     (or (null buffer-display-time)
                         (> (float-time (time-subtract (current-time)
                                                       buffer-display-time))
                            futon-buffer-cleaner-file-stale-age)))
            'file-stale))
         ((string-match-p futon-buffer-cleaner-stream-buffer-regexp name)
          'stream)
         ((and (cl-some (lambda (re) (string-match-p re name))
                        futon-buffer-cleaner-render-buffer-regexps)
               ;; *Arxana Essay* is the EDITABLE render (C-c C-c saves back
               ;; to the source md) — modified means unsaved operator edits,
               ;; never killable (near-miss with Joe's EOI edits, 2026-06-11)
               (or (not (string= name "*Arxana Essay*"))
                   (not (buffer-modified-p))))
          'render)
         ((and (string-prefix-p "*invoke: " name)
               ;; active-agent lanes already returned nil above
               t)
          'invoke)
         ((string-match-p futon-buffer-cleaner-http-buffer-regexp name)
          'http)
         ((and (eq major-mode 'dired-mode)
               (not (buffer-modified-p)))
          'dired)
         ((and (memq major-mode futon-buffer-cleaner-temp-modes)
               (not (buffer-modified-p)))
          'temp)
         (t nil))))))

;;;###autoload
(defun futon-buffer-cleaner-clean-now ()
  "Remove stale buffers and return a report plist.
The report has :before, :after, :total, and per-kind counts."
  (interactive)
  (let ((before (length (buffer-list)))
        (visible (futon-buffer-cleaner--visible-buffers))
        (counts '((http . 0) (dired . 0) (temp . 0) (stream . 0)
                  (render . 0) (invoke . 0) (file-stale . 0))))
    (dolist (buffer (buffer-list))
      (when-let ((kind (futon-buffer-cleaner--candidate-kind buffer visible)))
        (futon-buffer-cleaner--kill-buffer buffer)
        (cl-incf (alist-get kind counts))))
    (let* ((after (length (buffer-list)))
           (total (- before after))
           (report (list :before before
                         :after after
                         :total total
                         :http (alist-get 'http counts)
                         :dired (alist-get 'dired counts)
                         :temp (alist-get 'temp counts)
                         :stream (alist-get 'stream counts)
                         :render (alist-get 'render counts)
                         :invoke (alist-get 'invoke counts)
                         :file-stale (alist-get 'file-stale counts)
                         :at (current-time))))
      (setq futon-buffer-cleaner--last-report report)
      (when (called-interactively-p 'interactive)
        (message "Futon buffer cleaner: killed %d buffer(s); %d remain."
                 total after))
      report)))

(defun futon-buffer-cleaner--run ()
  "Timer entry point for stale-buffer cleanup."
  (condition-case err
      (let* ((report (futon-buffer-cleaner-clean-now))
             (total (plist-get report :total)))
        (when (and futon-buffer-cleaner-message-threshold
                   (>= total futon-buffer-cleaner-message-threshold))
          (message "Futon buffer cleaner: killed %d stale buffer(s) (%d stream, %d file, %d render, %d invoke, %d HTTP, %d Dired, %d temp); %d remain."
                   total
                   (plist-get report :stream)
                   (plist-get report :file-stale)
                   (plist-get report :render)
                   (plist-get report :invoke)
                   (plist-get report :http)
                   (plist-get report :dired)
                   (plist-get report :temp)
                   (plist-get report :after))))
    (error
     (message "Futon buffer cleaner failed: %s" (error-message-string err)))))

;;;###autoload
(defun futon-buffer-cleaner-enable ()
  "Enable periodic stale-buffer cleanup."
  (interactive)
  (futon-buffer-cleaner-disable)
  (setq futon-buffer-cleaner--timer
        (run-at-time futon-buffer-cleaner-interval
                     futon-buffer-cleaner-interval
                     #'futon-buffer-cleaner--run))
  (message "Futon buffer cleaner enabled (%ss interval)."
           futon-buffer-cleaner-interval)
  futon-buffer-cleaner--timer)

;;;###autoload
(defun futon-buffer-cleaner-disable ()
  "Disable periodic stale-buffer cleanup."
  (interactive)
  (when (timerp futon-buffer-cleaner--timer)
    (cancel-timer futon-buffer-cleaner--timer))
  (setq futon-buffer-cleaner--timer nil)
  (when (called-interactively-p 'interactive)
    (message "Futon buffer cleaner disabled.")))

;;;###autoload
(defun futon-buffer-cleaner-status ()
  "Return a plist describing the current buffer cleaner state."
  (interactive)
  (let ((status (list :enabled (timerp futon-buffer-cleaner--timer)
                      :interval futon-buffer-cleaner-interval
                      :last-report futon-buffer-cleaner--last-report)))
    (when (called-interactively-p 'interactive)
      (message "Futon buffer cleaner: %s, interval=%ss, last=%S"
               (if (plist-get status :enabled) "enabled" "disabled")
               futon-buffer-cleaner-interval
               futon-buffer-cleaner--last-report))
    status))

(provide 'futon-buffer-cleaner)

;;; futon-buffer-cleaner.el ends here
