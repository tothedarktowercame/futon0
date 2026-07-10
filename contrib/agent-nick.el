;;; agent-nick.el --- Human nicknames for agent REPL buffers -*- lexical-binding: t; -*-

;;; Commentary:
;; Agent REPL buffers have canonical names the system depends on for routing and
;; identity (`*claude-repl:claude-12*').  That name can't change.  This module
;; adds a *display alias* — a nickname — on top of it:
;;
;;   - stored buffer-local in `agent-nick' (and mirrored in a session registry
;;     keyed by agent-id, so a reaped/re-invoked buffer keeps its nick);
;;   - shown in the mode line in parens after the buffer name;
;;   - shown in Agent Mission Control (via advice — no core edits);
;;   - jumpable with `C-c b' (`agent-switch-to-buffer-by-nick'), a nick-aware
;;     analogue of `C-x b' that uses `completing-read' (Vertico enhances it).
;;
;; Set a nick with `M-x agent-nick-set' inside a REPL buffer; empty input clears
;; it.  The canonical buffer name is never touched.

;;; Code:

(require 'subr-x)
(require 'seq)

(declare-function agent-mission-control-refresh "agent-mission-control")

(defgroup agent-nick nil
  "Human nicknames for agent REPL buffers."
  :group 'futon-hot)

(defcustom agent-nick-repl-modes '(claude-repl-mode codex-repl-mode)
  "Major modes considered agent REPL buffers for nick display and jumping."
  :type '(repeat symbol)
  :group 'agent-nick)

(defvar-local agent-nick nil
  "Human display nickname for this agent REPL buffer, or nil.")

(defvar agent-nick--by-agent (make-hash-table :test 'equal)
  "Session registry mapping agent-id -> nick, so nicks survive buffer recreation.")

(defvar agent-nick--mode-line-segment
  '(:eval (when agent-nick
            (propertize (format " (%s)" agent-nick) 'face 'italic)))
  "Mode-line construct appended after the buffer name to show the nick.")

;;; --- Identity --------------------------------------------------------------

(defun agent-nick--agent-id (&optional buf)
  "Return the agent-id for BUF (default current), or nil."
  (with-current-buffer (or buf (current-buffer))
    (or (and (boundp 'agent-chat--agent-id) agent-chat--agent-id)
        (and (local-variable-p 'claude-repl-agent-id)
             (boundp 'claude-repl-agent-id) claude-repl-agent-id)
        (and (string-match "\\`\\*[^:]+:\\(.+\\)\\*\\'" (buffer-name))
             (match-string 1 (buffer-name))))))

(defun agent-nick--repl-buffer-p (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (apply #'derived-mode-p agent-nick-repl-modes)))

(defun agent-nick-of-buffer (buf)
  "Return the nick for BUF: the buffer-local value, else the registry by agent-id."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (or agent-nick
          (when-let ((id (agent-nick--agent-id buf)))
            (gethash id agent-nick--by-agent))))))

;;; --- Mode line -------------------------------------------------------------

(defun agent-nick--install-mode-line ()
  "Ensure the nick segment is present in this buffer's mode-line buffer id."
  (let ((current (if (local-variable-p 'mode-line-buffer-identification)
                     mode-line-buffer-identification
                   (default-value 'mode-line-buffer-identification))))
    (unless (and (listp current) (memq agent-nick--mode-line-segment current))
      (setq-local mode-line-buffer-identification
                  (append (if (listp current) current (list current))
                          (list agent-nick--mode-line-segment))))))

(defun agent-nick--setup ()
  "Restore any saved nick and install the mode-line segment in this REPL buffer."
  (when-let ((id (agent-nick--agent-id)))
    (when-let ((saved (gethash id agent-nick--by-agent)))
      (setq-local agent-nick saved)))
  (agent-nick--install-mode-line))

;;; --- Setter ----------------------------------------------------------------

;;;###autoload
(defun agent-nick-set (nick)
  "Set this REPL buffer's display NICK (empty input clears it)."
  (interactive
   (progn
     (unless (agent-nick--repl-buffer-p)
       (user-error "Not in an agent REPL buffer"))
     (list (read-string (format "Agent nick for %s (empty clears): " (buffer-name))
                        agent-nick))))
  (let ((id (agent-nick--agent-id))
        (clean (string-trim (or nick ""))))
    (if (string-empty-p clean)
        (progn
          (kill-local-variable 'agent-nick)
          (when id (remhash id agent-nick--by-agent))
          (message "agent-nick: cleared for %s" (or id (buffer-name))))
      (setq-local agent-nick clean)
      (when id (puthash id clean agent-nick--by-agent))
      (message "agent-nick: %s -> %s" (or id (buffer-name)) clean))
    (agent-nick--install-mode-line)
    (force-mode-line-update)
    (when (get-buffer "*agent-mission-control*")
      (ignore-errors (agent-mission-control-refresh)))))

;;; --- Jump (C-c b) ----------------------------------------------------------

(defun agent-nick--agent-buffers ()
  "Return the list of live agent REPL buffers."
  (seq-filter #'agent-nick--repl-buffer-p (buffer-list)))

;;;###autoload
(defun agent-switch-to-buffer-by-nick ()
  "Switch to an agent REPL buffer, choosing by nick or buffer name.
A nick-aware analogue of `switch-to-buffer' (\\[switch-to-buffer])."
  (interactive)
  (let* ((bufs (agent-nick--agent-buffers))
         (cands (mapcar
                 (lambda (b)
                   (let ((nick (agent-nick-of-buffer b)))
                     (cons (if nick
                               (format "%s — %s" nick (buffer-name b))
                             (buffer-name b))
                           b)))
                 bufs)))
    (unless cands (user-error "No agent REPL buffers"))
    (let* ((choice (completing-read "Agent buffer: " (mapcar #'car cands) nil t))
           (buf (cdr (assoc choice cands))))
      (when (buffer-live-p buf)
        (switch-to-buffer buf)))))

;;; --- Mission Control integration (advice, no core edits) -------------------

(defun agent-nick--mc-label-advice (orig buf)
  "Append the nick to Agent Mission Control's per-buffer label."
  (let ((base (funcall orig buf))
        (nick (agent-nick-of-buffer buf)))
    (if nick (format "%s (%s)" base nick) base)))

;;; --- Wiring ----------------------------------------------------------------

(global-set-key (kbd "C-c b") #'agent-switch-to-buffer-by-nick)
;; C-c r sets the current REPL buffer's nick (replaces eglot-rename globally).
(global-set-key (kbd "C-c r") #'agent-nick-set)

(add-hook 'claude-repl-mode-hook #'agent-nick--setup)
(add-hook 'codex-repl-mode-hook #'agent-nick--setup)

(with-eval-after-load 'agent-mission-control
  (advice-add 'agent-mission-control--buffer-agent-label
              :around #'agent-nick--mc-label-advice))

;; Arm any REPL buffers that already exist when this file loads.
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (agent-nick--repl-buffer-p)
      (agent-nick--setup))))

(provide 'agent-nick)

;;; agent-nick.el ends here
