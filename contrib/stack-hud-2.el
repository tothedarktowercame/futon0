;;; stack-hud-2.el --- Posframe-native Stack HUD in a dedicated full-screen frame -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Replacement for stack-hud.el's side-window UI.  The legacy HUD
;; remains in stack-hud.el (conceptually "stack-hud-1") so blocks can
;; be ported across without breaking the existing M-x stack-hud flow.
;;
;; This file owns the new HUD surface.  Differences from stack-hud-1:
;;
;;   - Renders into a dedicated Emacs frame (via `make-frame'), not
;;     a side-window inside the current Emacs frame.
;;   - That frame is full-screen and intended to float under Sway,
;;     so the HUD does not disrupt Sway's window numbering or tiling
;;     layout.
;;   - Per-block docs come from stack-hud-widgets.el (a posframe popup
;;     near point), replacing the *Stack Doc* sidewindow from
;;     stack-doc.el.  See the Per-widget docs section in
;;     stack-hud-widgets.el.
;;   - Block bodies are widget render functions from
;;     stack-hud-widgets.el — single source of truth.  Iterate widgets
;;     standalone with M-x stack-hud-widget-render-...; promote into
;;     this HUD by adding the widget id to `stack-hud-2-blocks'.
;;
;; ENTRY POINT
;;
;;   M-x stack-hud            (primary entry point)
;;   M-x stack-hud-2-toggle   (same toggle, direct command)
;;   M-x stack-hud-1          (legacy side-window HUD)
;;
;;     - If no HUD frame exists, create it.
;;     - If a HUD frame exists but is not focused, raise/focus it.
;;     - If the HUD frame is focused, close it.
;;
;; SWAY CONFIG (one-time)
;;
;; Add to ~/.config/sway/config:
;;
;;     # Stack HUD 2: float and fullscreen the dedicated Emacs frame
;;     for_window [title="^stack-hud-2$"] floating enable, fullscreen enable
;;
;; Reload sway config (Mod+Shift+c by convention).  After that, every
;; new stack-hud-2 frame appears as a floating fullscreen window.
;;
;; The matching is by frame title.  Emacs sets the X11 title from the
;; frame's `name' parameter, which we set explicitly in `make-frame'.

;;; Code:

(require 'cl-lib)
(require 'stack-hud-widgets)

;;; --------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------

(defgroup stack-hud-2 nil
  "Posframe-native Stack HUD, rendered in a dedicated floating frame."
  :group 'tools)

(defcustom stack-hud-2-frame-name "stack-hud-2"
  "Frame title for the dedicated HUD frame.
This is the string Sway should match in its `for_window' rule.
Changing this requires updating the Sway window-rule to match."
  :type 'string
  :group 'stack-hud-2)

(defcustom stack-hud-2-frame-parameters
  '((menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (fullscreen . fullboth))
  "Frame parameters applied when opening the HUD frame.
`name', `title', and the marker parameter `stack-hud-2' are added
automatically by `stack-hud-2-open'."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'stack-hud-2)

(defcustom stack-hud-2-blocks
  '(invariant-queue
    briefing-summary
    sessions-in-flight
    evidence-per-pattern
    evidence-per-session)
  "Ordered list of widget ids to render in the HUD.
Each id must have a registered render function in stack-hud-widgets.el."
  :type '(repeat symbol)
  :group 'stack-hud-2)

(defcustom stack-hud-2-toggle-key "⁂"
  "Global key sequence bound to `stack-hud-2-toggle'.
Set to nil to skip the auto-binding and bind from your init."
  :type '(choice (const :tag "do not auto-bind" nil)
                 string)
  :group 'stack-hud-2)

(defcustom stack-hud-2-buffer-name "*stack-hud-2*"
  "Buffer name used inside the HUD frame."
  :type 'string
  :group 'stack-hud-2)

;;; --------------------------------------------------------------------
;;; Rendering
;;; --------------------------------------------------------------------

(defvar stack-hud-2--block-renderers
  '((evidence-per-pattern  . stack-hud-widget--render-evidence-per-pattern)
    (evidence-per-session  . stack-hud-widget--render-evidence-per-session)
    (sessions-in-flight    . stack-hud-widget--render-sessions-in-flight)
    (briefing-summary      . stack-hud-widget--render-briefing-summary)
    (invariant-queue       . stack-hud-widget--render-invariant-queue))
  "Mapping from block id (symbol) to render function.
Render functions take no arguments and write to the current buffer.")

(defun stack-hud-2--separator ()
  "Insert a separator line between blocks."
  (insert (propertize (make-string 78 ?─) 'face 'shadow) "\n\n"))

(defun stack-hud-2--render ()
  "Render the HUD into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Stack HUD 2 — ant's-eye view" 'face 'bold))
    (insert (format "  · %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
    (insert (propertize
             "  ⁂ toggle  ·  RET open target at point  ·  g refresh  ·  ? doc  ·  q close\n\n"
             'face 'shadow))
    (stack-hud-2--separator)
    (dolist (id stack-hud-2-blocks)
      (let ((fn (cdr (assq id stack-hud-2--block-renderers))))
        (cond
         ((not fn)
          (insert (propertize
                   (format "  ⚠ no renderer registered for block %s\n\n" id)
                   'face 'error)))
         (t
          (condition-case err
              (funcall fn)
            (error
             (insert (propertize
                      (format "  ⚠ %s render error: %S\n\n" id err)
                      'face 'error))))
          (stack-hud-2--separator)))))
    (goto-char (point-min))))

;;; --------------------------------------------------------------------
;;; Major mode
;;; --------------------------------------------------------------------

(defvar stack-hud-2-mode-map
  (make-sparse-keymap)
  "Keymap for `stack-hud-2-mode'.")

;; Mutate the existing keymap so `load-file' updates bindings in a live Emacs.
(define-key stack-hud-2-mode-map (kbd "g") #'stack-hud-2-refresh)
(define-key stack-hud-2-mode-map (kbd "RET") #'stack-hud-2-visit-at-point)
(define-key stack-hud-2-mode-map (kbd "<return>") #'stack-hud-2-visit-at-point)
(define-key stack-hud-2-mode-map [mouse-1] #'stack-hud-2-mouse-visit)
(define-key stack-hud-2-mode-map (kbd "q") #'stack-hud-2-close)
(define-key stack-hud-2-mode-map (kbd "?") #'stack-hud-widget-show-doc)
(define-key stack-hud-2-mode-map (kbd "C-c C-d") #'stack-hud-widget-show-doc)
(define-key stack-hud-2-mode-map (kbd "⁂") #'stack-hud-2-toggle)

(define-derived-mode stack-hud-2-mode special-mode "StackHUD-2"
  "Major mode for the dedicated Stack HUD 2 buffer."
  (setq-local truncate-lines t))

(defun stack-hud-2--buffer-at-point ()
  "Return the HUD target buffer at point, or nil."
  (get-text-property (point) 'stack-hud-target-buffer))

(defun stack-hud-2--window-showing-buffer (buffer)
  "Return a non-HUD window already showing BUFFER, or nil."
  (cl-find-if (lambda (win)
                (not (frame-parameter (window-frame win) 'stack-hud-2)))
              (get-buffer-window-list buffer nil t)))

(defun stack-hud-2--first-non-hud-window ()
  "Return the first live non-HUD window across all frames, or nil."
  (let (found)
    (walk-windows (lambda (win)
                    (when (and (not found)
                               (window-live-p win)
                               (not (frame-parameter (window-frame win) 'stack-hud-2)))
                      (setq found win)))
                  'no-minibuf t)
    found))

(defun stack-hud-2-visit-at-point ()
  "Jump to the buffer targeted by the HUD block at point."
  (interactive)
  (let ((buffer (stack-hud-2--buffer-at-point)))
    (unless (buffer-live-p buffer)
      (user-error "No live buffer target at point"))
    (let ((win (or (stack-hud-2--window-showing-buffer buffer)
                   (stack-hud-2--first-non-hud-window))))
      (if (window-live-p win)
          (progn
            (select-frame-set-input-focus (window-frame win))
            (select-window win)
            (unless (eq (window-buffer win) buffer)
              (switch-to-buffer buffer)))
        (switch-to-buffer buffer)))))

(defun stack-hud-2-mouse-visit (event)
  "Visit the HUD target under mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (stack-hud-2-visit-at-point))

;;; --------------------------------------------------------------------
;;; Frame management
;;; --------------------------------------------------------------------

(defun stack-hud-2--frame ()
  "Return the existing HUD frame, or nil."
  (cl-find-if (lambda (f) (frame-parameter f 'stack-hud-2))
              (frame-list)))

(defun stack-hud-2-open ()
  "Open the HUD in a dedicated full-screen frame.
If a HUD frame already exists, raise and focus it."
  (interactive)
  (let ((existing (stack-hud-2--frame)))
    (cond
     (existing
      (select-frame-set-input-focus existing))
     (t
      (let* ((params (append `((name  . ,stack-hud-2-frame-name)
                                (title . ,stack-hud-2-frame-name)
                                (stack-hud-2 . t))
                              stack-hud-2-frame-parameters))
             (frame (make-frame params))
             (buf   (get-buffer-create stack-hud-2-buffer-name)))
        (with-current-buffer buf
          (unless (derived-mode-p 'stack-hud-2-mode)
            (stack-hud-2-mode))
          (stack-hud-2--render))
        (select-frame-set-input-focus frame)
        (switch-to-buffer buf))))))

(defun stack-hud-2-refresh ()
  "Re-render the HUD buffer in place."
  (interactive)
  (when-let ((buf (get-buffer stack-hud-2-buffer-name)))
    (with-current-buffer buf
      (stack-hud-2--render))))

(defun stack-hud-2-close ()
  "Close the HUD frame (kill its buffer too)."
  (interactive)
  (when-let ((frame (stack-hud-2--frame)))
    (when (frame-live-p frame)
      (delete-frame frame)))
  (when-let ((buf (get-buffer stack-hud-2-buffer-name)))
    (kill-buffer buf)))

(defun stack-hud-2-toggle ()
  "Toggle the HUD: open if absent, raise if hidden, close if focused."
  (interactive)
  (let ((existing (stack-hud-2--frame)))
    (cond
     ((and existing (eq existing (selected-frame)))
      (stack-hud-2-close))
     (existing
      (select-frame-set-input-focus existing))
     (t
      (stack-hud-2-open)))))

;;; --------------------------------------------------------------------
;;; Auto-bind ⁂ globally (override via init or by setting toggle-key nil)
;;; --------------------------------------------------------------------

(when (and stack-hud-2-toggle-key
           (stringp stack-hud-2-toggle-key)
           (not (string-empty-p stack-hud-2-toggle-key)))
  (global-set-key (kbd stack-hud-2-toggle-key) #'stack-hud-2-toggle))

(provide 'stack-hud-2)

;;; stack-hud-2.el ends here
