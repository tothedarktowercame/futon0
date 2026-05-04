;;; mana-hud.el --- Metabolic-balance HUD widget for stack-hud -*- lexical-binding: t; -*-

;;; Commentary:
;; A small HUD widget for the metabolic-balance/working-tree drain
;; channel of M-bounded-in-flight-state.  Reads the JSON snapshot at
;; `mana-hud-snapshot-path' (produced by
;; `~/code/futon0/scripts/mana-snapshot.bb') and renders a single
;; line describing current pressure across the stack.
;;
;; Run the snapshot script periodically (cron, after-save-hook, or
;; manually) to keep the HUD fresh.  When the snapshot is stale or
;; missing the widget renders a polite '?' rather than failing.
;;
;; To enable from `stack-hud.el', call `(mana-hud-render)' at the
;; appropriate place in the hud composition.

;;; Code:

(require 'json)
(require 'subr-x)

(defgroup mana-hud nil
  "Metabolic-balance HUD widget."
  :group 'tatami-integration)

(defcustom mana-hud-snapshot-path
  "/home/joe/code/storage/futon0/mana-snapshot.json"
  "Path to the JSON snapshot produced by `scripts/mana-snapshot.bb'."
  :type 'file
  :group 'mana-hud)

(defcustom mana-hud-stale-after-seconds 600
  "If the snapshot is older than this many seconds, render it as stale."
  :type 'integer
  :group 'mana-hud)

(defface mana-hud-tier-silent
  '((t :foreground "#86efac"))   ; calm green
  "Face for the silent tier."
  :group 'mana-hud)

(defface mana-hud-tier-advisory
  '((t :foreground "#fde047" :weight bold))   ; soft yellow
  "Face for the advisory tier."
  :group 'mana-hud)

(defface mana-hud-tier-high
  '((t :foreground "#fb923c" :weight bold))   ; warm orange
  "Face for the high tier."
  :group 'mana-hud)

(defface mana-hud-tier-stop-the-line
  '((t :foreground "#f87171" :weight bold))   ; red
  "Face for the stop-the-line tier."
  :group 'mana-hud)

(defface mana-hud-tier-stale
  '((t :foreground "#9ca3af" :slant italic))   ; muted grey
  "Face for stale or missing snapshots."
  :group 'mana-hud)

(defconst mana-hud--tier-face
  '(("silent"        . mana-hud-tier-silent)
    ("advisory"      . mana-hud-tier-advisory)
    ("high"          . mana-hud-tier-high)
    ("stop-the-line" . mana-hud-tier-stop-the-line)))

(defconst mana-hud--tier-glyph
  '(("silent"        . "○")
    ("advisory"      . "◔")
    ("high"          . "◑")
    ("stop-the-line" . "●")))

(defun mana-hud--read-snapshot ()
  "Read the snapshot JSON.  Returns the parsed alist or nil."
  (when (file-readable-p mana-hud-snapshot-path)
    (condition-case _err
        (with-temp-buffer
          (insert-file-contents mana-hud-snapshot-path)
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol)))
          (goto-char (point-min))
          (json-read))
      (error nil))))

(defun mana-hud--snapshot-stale-p (snap)
  "Return non-nil if SNAP's :generated-at is older than `mana-hud-stale-after-seconds'."
  (let* ((ts (cdr (assq 'generated-at snap)))
         (gen (and ts (parse-iso8601-time-string ts))))
    (when gen
      (> (- (float-time) (float-time gen))
         mana-hud-stale-after-seconds))))

(defun mana-hud--violators (per-repo)
  "Return a string listing repos in :high or :stop-the-line tier from PER-REPO."
  (when-let ((violating
              (seq-filter
               (lambda (r)
                 (let ((tier (cdr (assq 'tier r))))
                   (member tier '("high" "stop-the-line"))))
               per-repo)))
    (mapconcat
     (lambda (r)
       (let* ((repo (cdr (assq 'repo r)))
              (p (cdr (assq 'P r)))
              (tier (cdr (assq 'tier r)))
              (face (cdr (assoc tier mana-hud--tier-face))))
         (propertize (format "%s(%.1f)" repo (or p 0.0))
                     'face face)))
     violating
     " ")))

(defun mana-hud-render ()
  "Render a single-line HUD summary of metabolic-balance state.
Suitable for embedding in stack-hud or any other widget host."
  (let ((snap (mana-hud--read-snapshot)))
    (cond
     ((null snap)
      (propertize "mana-hud: snapshot missing"
                  'face 'mana-hud-tier-stale))

     ((mana-hud--snapshot-stale-p snap)
      (propertize (format "mana-hud: stale (>%ds)"
                          mana-hud-stale-after-seconds)
                  'face 'mana-hud-tier-stale))

     (t
      (let* ((max-tier (cdr (assq 'max-tier snap)))
             (max-p (cdr (assq 'max-pressure snap)))
             (n-repos (length (cdr (assq 'per-repo snap))))
             (face (cdr (assoc max-tier mana-hud--tier-face)))
             (glyph (cdr (assoc max-tier mana-hud--tier-glyph)))
             (header (propertize
                      (format "%s drain %s P=%.2f / %d repos"
                              glyph max-tier (or max-p 0.0) n-repos)
                      'face face))
             (violators (mana-hud--violators (cdr (assq 'per-repo snap)))))
        (if violators
            (concat header "  " violators)
          header))))))

(defun mana-hud-show ()
  "Echo a one-line metabolic-balance snapshot in the minibuffer."
  (interactive)
  (message "%s" (mana-hud-render)))

(provide 'mana-hud)
;;; mana-hud.el ends here
