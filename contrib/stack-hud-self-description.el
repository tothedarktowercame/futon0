;;; stack-hud-self-description.el --- AIF+ self-description block for Stack HUD -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Wires the futon stack's AIF+ self-description (THE-STACK.md and the
;; AIF+ EDN substrate at futon5a/holes/stories/) into the Stack HUD as
;; an interoceptive readout.  Shows the cached level-0 reading's age
;; and the drift detector's verdict; nominates a refresh when the
;; self-description is older than `stack-hud-self-description-stale-days'
;; OR when the drift detector reports staleness on any layer.
;;
;; The HUD only NOMINATES.  Actual re-zip (Layer 2) and meta-reassembly
;; (Layer 3) are LLM jobs and are not automated here.  The interactive
;; command `stack-hud-refresh-self-description' runs the cheap mechanical
;; re-render (prose generators) but flags Layer 2 / Layer 3 work for
;; manual attention.
;;
;; Load order: load this file AFTER stack-hud.el; the dispatch advice
;; attaches at load-time.  Then add `(:key self-description :enabled t)'
;; to your `stack-hud-blocks' configuration (or M-x customize).
;;
;; Usage:
;;   (load "~/code/futon0/contrib/stack-hud-self-description.el")
;;   (add-to-list 'stack-hud-blocks '(:key self-description :enabled t) t)

;;; Code:

(require 'stack-hud)
(require 'json)

(defcustom stack-hud-self-description-path
  "/home/joe/code/futon5a/holes/THE-STACK.md"
  "Path to the cached level-0 self-description (THE-STACK.md)."
  :type 'file
  :group 'tatami-integration)

(defcustom stack-hud-self-description-stale-days 7
  "Nominate refresh when the self-description file is older than N days."
  :type 'integer
  :group 'tatami-integration)

(defcustom stack-hud-self-description-cache-seconds 600
  "Re-run the drift detector at most every N seconds during HUD renders.
The detector itself is fast (~1s) but caching avoids running it on
every HUD redraw."
  :type 'integer
  :group 'tatami-integration)

(defcustom stack-hud-detect-drift-script
  "/home/joe/code/futon5a/scripts/detect_drift.clj"
  "Path to the AIF+ drift detector babashka script."
  :type 'file
  :group 'tatami-integration)

(defcustom stack-hud-render-aif2-prose-script
  "/home/joe/code/futon5a/scripts/render_aif2_prose.clj"
  "Path to the holistic-argument-aif2.md prose generator."
  :type 'file
  :group 'tatami-integration)

(defcustom stack-hud-render-leaf-prose-script
  "/home/joe/code/futon5a/scripts/render_leaf_prose.clj"
  "Path to the per-leaf prose generator."
  :type 'file
  :group 'tatami-integration)

(defvar stack-hud--self-description-cache nil
  "Cached parsed result of `detect_drift.clj --json'.")

(defvar stack-hud--self-description-cache-time 0
  "Float-time of the last drift-detector cache refresh.")

;; ---------- helpers ----------

(defun stack-hud--self-description-cache-stale-p ()
  "Return non-nil iff the cached drift result is older than the cache TTL."
  (> (- (float-time) stack-hud--self-description-cache-time)
     stack-hud-self-description-cache-seconds))

(defun stack-hud--self-description-fetch ()
  "Run the drift detector with --json and return the parsed alist.
Returns nil on parse error or if the script does not exist."
  (when (file-exists-p stack-hud-detect-drift-script)
    (let* ((cmd (format "bb %s --json"
                        (shell-quote-argument stack-hud-detect-drift-script)))
           (output (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process-shell-command cmd nil t nil)))))
      (condition-case _
          (let ((json-object-type 'alist)
                (json-array-type 'list)
                (json-key-type 'symbol))
            (json-read-from-string output))
        (error nil)))))

(defun stack-hud--self-description-get ()
  "Return the drift detector result, refreshing the cache if stale."
  (when (or (null stack-hud--self-description-cache)
            (stack-hud--self-description-cache-stale-p))
    (setq stack-hud--self-description-cache (stack-hud--self-description-fetch)
          stack-hud--self-description-cache-time (float-time)))
  stack-hud--self-description-cache)

(defun stack-hud--self-description-age-days ()
  "Return number of days since the self-description file was modified.
Nil if the file does not exist."
  (let ((path stack-hud-self-description-path))
    (when (file-exists-p path)
      (/ (- (float-time)
            (float-time (nth 5 (file-attributes path))))
         86400.0))))

(defun stack-hud--self-description-nested-int (data layer key)
  "Pull integer at DATA -> LAYER -> KEY, defaulting to 0."
  (or (and data (alist-get key (alist-get layer data))) 0))

(defun stack-hud--self-description-nested-list (data layer key)
  "Pull list at DATA -> LAYER -> KEY, defaulting to nil."
  (and data (alist-get key (alist-get layer data))))

;; ---------- render ----------

(defun my-chatgpt-shell--insert-stack-self-description ()
  "Insert the AIF+ self-description block into the Stack HUD."
  (insert (propertize "Self-description (THE-STACK)" 'face 'bold) "\n")
  (let* ((data (stack-hud--self-description-get))
         (age-days (stack-hud--self-description-age-days))
         (age-stale? (and age-days
                          (> age-days stack-hud-self-description-stale-days)))
         (l2-stale (stack-hud--self-description-nested-int data 'layer_2 'stale))
         (l3-stale (stack-hud--self-description-nested-int data 'layer_3 'stale))
         (l4-outdated (stack-hud--self-description-nested-int data 'layer_4 'outdated))
         (drift-needs-refresh? (and data (eq (alist-get 'needs_refresh data) t)))
         (nominated? (or age-stale? drift-needs-refresh?)))
    ;; Path
    (insert (format "  cached at: %s\n" stack-hud-self-description-path))
    ;; Age
    (cond
     ((null age-days)
      (insert "  ⚠ self-description file not found\n"))
     (age-stale?
      (insert (format "  age: %.1f days  ⚠ STALE (>%d d threshold)\n"
                      age-days stack-hud-self-description-stale-days)))
     (t
      (insert (format "  age: %.1f days  ✓\n" age-days))))
    ;; Drift summary
    (cond
     ((null data)
      (insert "  drift: detector unavailable\n"))
     ((and (= 0 l2-stale) (= 0 l3-stale) (= 0 l4-outdated))
      (insert "  drift: 16 leaves in sync ✓\n"))
     (t
      (insert (format "  drift: L2=%d stale, L3=%d stale, L4=%d outdated\n"
                      l2-stale l3-stale l4-outdated))
      (let ((reason (and data (alist-get 'refresh_reason data))))
        (when (and reason (not (eq reason :null)))
          (insert (format "         %s\n" reason))))))
    ;; Nomination
    (when nominated?
      (insert (propertize
               "  ⚠ M-x stack-hud-refresh-self-description  (re-render + nominate re-zip)\n"
               'face 'warning)))
    (insert "\n")))

;; ---------- interactive refresh ----------

(defun stack-hud-refresh-self-description (&optional force)
  "Re-run the prose generators (cheap, mechanical), then refresh the HUD.

Layer 2 (re-zip) and Layer 3 (meta-reassembly) are NOT automated here —
they need LLM judgment.  If the drift detector reports such staleness,
this command logs the affected leaves and leaves their resolution to a
manual Phase 2 / Phase 3 process.

With prefix arg FORCE, run unconditionally; otherwise skip when the
drift detector reports nothing needs refresh."
  (interactive "P")
  (let* ((data (stack-hud--self-description-fetch))
         (needs-refresh (and data (eq (alist-get 'needs_refresh data) t))))
    (cond
     ((and (not force) (not needs-refresh))
      (message "Self-description in sync; nothing to refresh."))
     (t
      (message "Re-running prose generators...")
      (call-process-shell-command
       (format "bb %s" (shell-quote-argument stack-hud-render-aif2-prose-script))
       nil "*stack-hud-self-description*" nil)
      (call-process-shell-command
       (format "bb %s" (shell-quote-argument stack-hud-render-leaf-prose-script))
       nil "*stack-hud-self-description*" nil)
      ;; Bust cache so next render picks up new state
      (setq stack-hud--self-description-cache nil
            stack-hud--self-description-cache-time 0)
      (let* ((data2 (stack-hud--self-description-fetch))
             (l2 (stack-hud--self-description-nested-int data2 'layer_2 'stale))
             (l3 (stack-hud--self-description-nested-int data2 'layer_3 'stale))
             (l2-leaves (stack-hud--self-description-nested-list data2 'layer_2 'stale-leaves))
             (l3-leaves (stack-hud--self-description-nested-list data2 'layer_3 'stale-leaves)))
        (cond
         ((and (= 0 l2) (= 0 l3))
          (message "Self-description refreshed; all layers in sync."))
         (t
          (message "Prose regenerated. Manual work still needed:%s%s"
                   (if (> l2 0)
                       (format " Layer-2 re-zip: %s."
                               (mapconcat #'identity l2-leaves ", "))
                     "")
                   (if (> l3 0)
                       (format " Layer-3 reassembly: %s."
                               (mapconcat #'identity l3-leaves ", "))
                     "")))))
      (when (fboundp 'stack-hud--refresh-buffer)
        (stack-hud--refresh-buffer))))))

(defun stack-hud-open-self-description ()
  "Open the cached level-0 self-description (THE-STACK.md)."
  (interactive)
  (find-file stack-hud-self-description-path))

;; ---------- dispatch advice ----------

(defun stack-hud--block-render-fn--self-description-advice (orig-fn key)
  "Add `self-description' dispatch to `stack-hud--block-render-fn'."
  (or (and (eq key 'self-description)
           #'my-chatgpt-shell--insert-stack-self-description)
      (funcall orig-fn key)))

(advice-add 'stack-hud--block-render-fn :around
            #'stack-hud--block-render-fn--self-description-advice)

(provide 'stack-hud-self-description)

;;; stack-hud-self-description.el ends here
