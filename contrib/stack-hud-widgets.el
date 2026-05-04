;;; stack-hud-widgets.el --- Standalone widget demos for the Stack HUD audit -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Widget demos for E-stack-hud-cleanup
;; (futon0/analysis/excursions/E-stack-hud-cleanup.md).
;;
;; Each widget is built and inspected in isolation BEFORE being promoted
;; into the live HUD.  Methodology Joe wants:
;;
;;   1. Build the widget here as a standalone interactive command.
;;   2. M-x stack-hud-widget-render-NAME to see it live, hit `g' to
;;      refresh, `q' to quit.
;;   3. Joe iterates on the highest-priority widgets.
;;   4. Once a widget is good, lift its render-fn into stack-hud.el and
;;      add the block key to `stack-hud-blocks'.
;;
;; All widgets pull from the running futon1a HTTP API at FUTON1A_URL
;; (default http://localhost:7071).  No widget triggers an LLM call.
;;
;; Widgets in priority order:
;;
;;   1. evidence-per-pattern        — top-N patterns by activation
;;   2. evidence-per-session        — delta-since-N decomposed by session
;;   3. sessions-in-flight          — Arxana cache pull (no haiku)
;;   4. briefing-summary-of-summaries (architecture only, not impl yet)
;;   5. alarm-aggregator             (Layer 2 reazon, deferred)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)

;;; --------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------

(defgroup stack-hud-widgets nil
  "Standalone widget demos for the Stack HUD audit."
  :group 'tools)

(defcustom stack-hud-widget-evidence-base-url
  (or (getenv "FUTON1A_URL") "http://localhost:7071")
  "Base URL for the futon1a evidence API."
  :type 'string
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-default-window-minutes 60
  "Default lookback window (minutes) for delta-style widgets."
  :type 'integer
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-pattern-top-n 8
  "How many top patterns to show in evidence-per-pattern."
  :type 'integer
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-fetch-limit 500
  "Max entries to fetch per probe; capped server-side."
  :type 'integer
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-invariant-queue-fetch-limit 10000
  "Max entries fetched for the invariant-queue widget.
Larger than `stack-hud-widget-fetch-limit' because pipeline-tracer
opens are emitted at boot and may be days old; without tag-filtering on
the futon1a HTTP endpoint, the recent-N-entries window must reach back
far enough to include them. Drop once the endpoint accepts `tags='."
  :type 'integer
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-fetch-timeout 5
  "HTTP timeout (seconds) for a probe call."
  :type 'integer
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-sessions-top-k 3
  "Maximum number of sessions to render in sessions-in-flight.
The data layer still returns all cached sessions for downstream
consumers (e.g. briefing summary-of-summaries) — only the render
truncates to this count."
  :type 'integer
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-popup-max-width 84
  "Max char width for widget doc popup."
  :type 'integer :group 'stack-hud-widgets)

(defcustom stack-hud-widget-popup-max-height 22
  "Max line height for widget doc popup."
  :type 'integer :group 'stack-hud-widgets)

(defcustom stack-hud-widget-popup-background-color "#0b1a33"
  "Background color for the widget doc posframe.
Default is a dark navy that distinguishes the popup from the parent
HUD frame."
  :type 'color :group 'stack-hud-widgets)

(defcustom stack-hud-widget-popup-foreground-color nil
  "Foreground color for the widget doc posframe, or nil to inherit."
  :type '(choice (const :tag "inherit" nil) color)
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-popup-border-color "#1f3a66"
  "Border color for the widget doc posframe.
Slightly lighter than the background by default for a soft outline."
  :type 'color :group 'stack-hud-widgets)

;;; --------------------------------------------------------------------
;;; Shared HTTP + parse
;;; --------------------------------------------------------------------

(defun stack-hud-widget--iso-since (minutes-ago)
  "Return an ISO-8601 timestamp for MINUTES-AGO minutes before now."
  (let* ((seconds-ago (* 60 minutes-ago))
         (then (time-subtract (current-time)
                              (seconds-to-time seconds-ago))))
    (format-time-string "%Y-%m-%dT%H:%M:%SZ" then t)))

(defun stack-hud-widget--fetch-evidence (since-iso &optional limit)
  "Fetch evidence entries SINCE-ISO with optional LIMIT.
Returns a plist list of entries, or nil on error.  Errors are
surfaced as a `:error' key on a single returned entry."
  (let* ((lim (or limit stack-hud-widget-fetch-limit))
         (qs (format "?limit=%d&since=%s"
                     lim
                     (url-hexify-string since-iso)))
         (base (string-remove-suffix "/" stack-hud-widget-evidence-base-url))
         (url (concat base "/api/alpha/evidence" qs))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buf (ignore-errors
                (url-retrieve-synchronously url t t
                                            stack-hud-widget-fetch-timeout))))
    (if (not buf)
        (list (list :error (format "fetch failed: %s" url)))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (condition-case err
            (let* ((data (if (fboundp 'json-parse-buffer)
                             (json-parse-buffer
                              :object-type 'plist
                              :array-type 'list
                              :null-object nil
                              :false-object nil)
                           (let ((json-object-type 'plist)
                                 (json-array-type 'list)
                                 (json-key-type 'symbol))
                             (json-read))))
                   (entries (plist-get data :entries)))
              (kill-buffer buf)
              entries)
          (error
           (kill-buffer buf)
           (list (list :error (format "parse error: %S" err)))))))))

(defun stack-hud-widget--err? (entries)
  "Return error string if ENTRIES is a single :error sentinel, else nil."
  (when (and (= (length entries) 1)
             (plist-get (car entries) :error))
    (plist-get (car entries) :error)))

;;; --------------------------------------------------------------------
;;; Generic widget buffer mode
;;; --------------------------------------------------------------------

(defvar-local stack-hud-widget--render-fn nil
  "Render function used by `stack-hud-widget-refresh' for this buffer.")

(defun stack-hud-widget-refresh ()
  "Re-render the current widget buffer."
  (interactive)
  (when (functionp stack-hud-widget--render-fn)
    (let ((render-fn stack-hud-widget--render-fn)
          (inhibit-read-only t))
      (erase-buffer)
      (funcall render-fn)
      (goto-char (point-min)))))

(defun stack-hud-widget-quit ()
  "Quit the widget buffer."
  (interactive)
  (quit-window t))

(defvar stack-hud-widget-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "g") #'stack-hud-widget-refresh)
    (define-key m (kbd "q") #'stack-hud-widget-quit)
    (define-key m (kbd "?") #'stack-hud-widget-show-doc)
    (define-key m (kbd "C-c C-d") #'stack-hud-widget-show-doc)
    m))

(define-derived-mode stack-hud-widget-mode special-mode "HUD-Widget"
  "Mode for inspecting a single Stack HUD widget."
  (setq-local truncate-lines t))

(defun stack-hud-widget--popup (buf-name render-fn)
  "Pop up a widget buffer named BUF-NAME and render it via RENDER-FN."
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (stack-hud-widget-mode)
      (setq-local stack-hud-widget--render-fn render-fn)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall render-fn))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;; --------------------------------------------------------------------
;;; Per-widget docs (replaces stack-doc sidewindow UI for widgets)
;;; --------------------------------------------------------------------
;;
;; Each widget owns its own doc string.  In the widget buffer (or in
;; the demo-all buffer), press `?' to toggle a floating posframe that
;; shows the doc for the widget at point.  Posframe on GUI; bottom
;; side-window fallback in terminal Emacs.
;;
;; Architecture:
;;
;; This supersedes `stack-doc.el`'s centralised sidewindow look-up of
;; warning-line documentation (`stack-doc--stack-warning-doc' →
;; `*Stack Doc*' buffer).  Per-widget ownership keeps the "what does
;; this show?" text with the code that shows it: when a widget changes,
;; its doc lives next door, not in a separate registry that drifts.
;;
;; The posframe pattern is lifted from
;; `futon3c/emacs/agent-chat.el:1184-1223' (agent-chat-popup-show) but
;; reimplemented standalone so widgets do not require futon3c on the
;; load-path.

(defvar stack-hud-widget-docs
  '((evidence-per-pattern .
     "Top-N most-activated patterns from recent context-retrieval evidence.

Decision served (E-stack-hud-cleanup §1 question 3): \"what patterns are
firing?\" — surfaces emergent themes from the futon3a semantic-search
results embedded in :coordination/context-retrieval evidence bodies.

Data source: GET /api/alpha/evidence?since=… on futon1a.  Filters to
entries with body.event=\"context-retrieval\", aggregates body.results
[].id counts, sorts desc, takes top stack-hud-widget-pattern-top-n.

Refresh: `g'.  Cost: one HTTP probe; no LLM call.

Tunables:
  stack-hud-widget-pattern-top-n            (default 8)
  stack-hud-widget-default-window-minutes   (default 60)
  stack-hud-widget-evidence-base-url        (env FUTON1A_URL)")

    (evidence-per-session .
     "Evidence delta decomposed by session-id over the lookback window.

Decision served: §1 question 1 (basics happen?) and 2 (where is recent
work landing?).  Tells you which conversations are alive at a glance.

Data source: same /api/alpha/evidence probe as evidence-per-pattern,
group-by :evidence/session-id.  No filter on event type — every entry
counts.

Refresh: `g'.  Cost: one HTTP probe; no LLM call.

Tunables:
  stack-hud-widget-default-window-minutes   (default 60)")

    (sessions-in-flight .
     "Top-K most recently active sessions, with cached summaries.

Decision served: §1 question 2 (where is recent work landing?) at the
texture level — uses Arxana's per-session summaries when populated.

Data source: arxana-evidence--open-session-item-cache
(futon4/dev/arxana-browser-evidence.el:106).  Read-only access; never
invokes the LLM-summary path.  Enumerates open REPL buffers to decide
which sessions are actually in flight, then uses the cache only to
enrich those live sessions with :about / :missions content.

Sort: by a live per-session probe of latest evidence activity, then
truncate to stack-hud-widget-sessions-top-k.

Refresh: `g'.  Cost: hash-table read + buffer enumeration + one live
probe per open session.  No LLM.
To force-recompute the underlying summaries, open Arxana
Browser → Sessions in the usual way; this widget never triggers haiku.

Tunables:
  stack-hud-widget-sessions-top-k           (default 3)")

    (briefing-summary .
     "Summary-of-summaries: top sessions weighted by recent evidence × age decay.

Decision served: §1 question 5 (what's queued?) plus a quiet-window
check for question 4.  Adds value over sessions-in-flight by
SYNTHESISING across cached summaries — does not duplicate them.

Algorithm: join sessions-in-flight items with evidence-per-session
deltas on session-id.  Weight = delta × 1/(1 + days-old).  Sort desc,
take top-K.  Aggregate :missions across the top-K, weighted.

Sessions with zero recent delta drop out of \"Hot now\" automatically;
quiet windows render explicitly as quiet so you can tell the difference
between \"nothing happened\" and \"data missing\".

Data source: futon1a evidence (W2) + Arxana cache (W3).  Cost: zero
new LLM calls per refresh.

Tunables:
  stack-hud-widget-briefing-top-k           (default 3)
  stack-hud-widget-default-window-minutes   (default 60)"))
  "Alist of widget-id → doc string.  Shown via `?' in a widget buffer.")

(defvar stack-hud-widget--popup-buffer " *stack-hud-widget-doc*"
  "Buffer name for widget doc popup.")

(defcustom stack-hud-widget-posframe-search-dirs
  '("~/.emacs-graph/straight/build/posframe"
    "~/.emacs.d/straight/build/posframe"
    "~/.config/emacs/straight/build/posframe")
  "Directories searched for posframe when (require 'posframe) fails.
First match is added to `load-path'.  Glob-style ELPA dirs are also
searched if these fail (see `stack-hud-widget-posframe-elpa-globs')."
  :type '(repeat string)
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-posframe-elpa-globs
  '("~/.emacs.d/elpa/posframe-*"
    "~/.config/emacs/elpa/posframe-*")
  "Glob patterns searched for posframe when no straight.el build is found."
  :type '(repeat string)
  :group 'stack-hud-widgets)

(defun stack-hud-widget--ensure-posframe ()
  "Try (require 'posframe), looking in known install dirs if necessary.
Returns t if posframe is loaded after the call, nil otherwise."
  (or (require 'posframe nil t)
      (let ((found
             (or (seq-find (lambda (d)
                             (let ((expanded (expand-file-name d)))
                               (file-exists-p
                                (expand-file-name "posframe.el" expanded))))
                           stack-hud-widget-posframe-search-dirs)
                 (seq-some (lambda (g)
                             (car (file-expand-wildcards
                                   (expand-file-name g))))
                           stack-hud-widget-posframe-elpa-globs))))
        (when found
          (add-to-list 'load-path (expand-file-name found))
          (require 'posframe nil t)))))

(defun stack-hud-widget-popup-show (text &optional title)
  "Show TEXT in a floating posframe child frame near point.
Posframe-only.  If posframe is not loadable or the call fails or the
display is non-graphic, this function does NOT fall back to a
same-frame side window — it surfaces a loud error in the echo area
so the underlying problem is fixable.  Run
`stack-hud-widget-popup-diagnose' for a structured report.
TITLE renders as a header line."
  (let ((buf (get-buffer-create stack-hud-widget--popup-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (setq-local mode-line-format nil)
        (when title
          (setq-local header-line-format
                      (propertize (format " %s" title) 'face 'bold)))
        (special-mode)))
    (cond
     ((not (display-graphic-p))
      (message "[stack-hud-widget] popup needs a GUI display (display-graphic-p is nil)."))
     ((not (stack-hud-widget--ensure-posframe))
      (message "[stack-hud-widget] posframe not loadable. Install with M-x package-install RET posframe."))
     (t
      (condition-case err
          (or (apply #'posframe-show buf
                     `(:position ,(point)
                       :max-width ,stack-hud-widget-popup-max-width
                       :max-height ,stack-hud-widget-popup-max-height
                       :background-color ,stack-hud-widget-popup-background-color
                       ,@(when stack-hud-widget-popup-foreground-color
                           `(:foreground-color ,stack-hud-widget-popup-foreground-color))
                       :border-width 1
                       :border-color ,stack-hud-widget-popup-border-color
                       :internal-border-width 10))
              (message "[stack-hud-widget] posframe-show returned nil; run M-x stack-hud-widget-popup-diagnose"))
        (error
         (message "[stack-hud-widget] posframe-show error: %S — run M-x stack-hud-widget-popup-diagnose" err)))))))

;;;###autoload
(defun stack-hud-widget-popup-diagnose ()
  "Report the state of the popup display path.
Use when `?' on a widget produces no visible posframe."
  (interactive)
  (let* ((parent (selected-frame))
         (lines
          (list
           (format "display-graphic-p:        %s" (display-graphic-p))
           (format "window-system:            %s" (window-system))
           (format "posframe library:         %s" (or (locate-library "posframe")
                                                       "NOT FOUND on load-path"))
           (format "posframe loaded:          %s" (featurep 'posframe))
           (format "selected frame name:      %S" (frame-parameter parent 'name))
           (format "selected frame fullscreen:%S" (frame-parameter parent 'fullscreen))
           (format "selected frame parent:    %S" (frame-parameter parent 'parent-frame))
           (format "selected frame stack-hud-2: %S" (frame-parameter parent 'stack-hud-2))
           (format "child-frame border-width: %S" (frame-parameter parent 'child-frame-border-width)))))
    (message "%s" (mapconcat #'identity lines " | "))
    (with-help-window "*stack-hud-widget-popup-diagnose*"
      (princ "stack-hud-widget popup diagnostic\n")
      (princ "=================================\n\n")
      (dolist (l lines) (princ l) (princ "\n"))
      (princ "\n")
      (princ "If posframe is NOT FOUND, install via:\n")
      (princ "  M-x package-install RET posframe RET\n\n")
      (princ "If posframe is loaded but no child frame appears under Sway:\n")
      (princ "  - Confirm Emacs build supports child frames\n")
      (princ "      (frame-parent (make-frame '((parent-frame . " )
      (princ "(selected-frame))))) should not error.\n")
      (princ "  - Check whether Sway is intercepting child frames; child frames\n")
      (princ "    are typically X11 windows whose title differs from the parent.\n")
      (princ "  - The widget popup buffer name is `%s'\n" )
      (princ (format "    (\"%s\")\n" stack-hud-widget--popup-buffer)))))

(defun stack-hud-widget-popup-hide ()
  "Dismiss the posframe doc popup."
  (interactive)
  (when (stack-hud-widget--ensure-posframe)
    (posframe-hide stack-hud-widget--popup-buffer)))

(defun stack-hud-widget--popup-visible-p ()
  "Return non-nil if our posframe doc popup is currently visible.
Walks `frame-list' rather than relying on posframe internals so the
check is stable across posframe versions."
  (when-let ((buf (get-buffer stack-hud-widget--popup-buffer)))
    (cl-some (lambda (f)
               (and (frame-live-p f)
                    (frame-visible-p f)
                    (eq (window-buffer (frame-root-window f)) buf)))
             (frame-list))))

(defun stack-hud-widget--current-id ()
  "Determine widget-id from buffer name or `stack-hud-widget-id' text property.
The headline text itself is no longer machine-readable — the id is
attached as a text property so the visible title can be terse."
  (let ((bn (buffer-name)))
    (or (and (string-match "\\*stack-hud-widget:\\([a-z-]+\\)\\*" bn)
             (intern (match-string 1 bn)))
        (get-text-property (point) 'stack-hud-widget-id)
        (save-excursion
          (let ((m (text-property-search-backward 'stack-hud-widget-id)))
            (when m (prop-match-value m))))
        (save-excursion
          (let ((m (text-property-search-forward 'stack-hud-widget-id)))
            (when m (prop-match-value m)))))))

(defun stack-hud-widget-show-doc ()
  "Toggle the doc popup for the widget at point.

The widget-id is derived from the buffer name.  In the demo-all buffer,
it's derived from the nearest preceding \"Widget: <name>\" heading."
  (interactive)
  (if (stack-hud-widget--popup-visible-p)
      (stack-hud-widget-popup-hide)
    (let* ((id (stack-hud-widget--current-id))
           (doc (and id (cdr (assq id stack-hud-widget-docs)))))
      (cond
       ((null id)
        (message "No widget at point."))
       ((null doc)
        (message "No doc registered for widget %s." id))
       (t
        (stack-hud-widget-popup-show
         doc (format "Widget doc: %s" id)))))))

;;; --------------------------------------------------------------------
;;; Widget 1: evidence-per-pattern
;;; --------------------------------------------------------------------
;;
;; Data source: GET /api/alpha/evidence?since=...&limit=...
;;   Filter to entries whose body has event="context-retrieval".
;;   For each, body.results is [{id, title, score}, ...].
;;   Aggregate count of (id, title) across all entries.
;;
;; Display: top-N rows, sorted by count desc.

(defun stack-hud-widget--patterns-from-entries (entries)
  "Return alist ((id title) . count) sorted by count desc."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let* ((body (plist-get e :evidence/body))
             (event (and body (plist-get body :event)))
             (results (and body (plist-get body :results))))
        (when (and (equal event "context-retrieval") results)
          (dolist (r results)
            (when-let* ((id (plist-get r :id)))
              (let* ((title (or (plist-get r :title) ""))
                     (key (cons id title))
                     (cur (gethash key tbl 0)))
                (puthash key (1+ cur) tbl)))))))
    (let (acc)
      (maphash (lambda (k v) (push (cons k v) acc)) tbl)
      (sort acc (lambda (a b) (> (cdr a) (cdr b)))))))

(defun stack-hud-widget-data-evidence-per-pattern (&optional minutes)
  "Compute evidence-per-pattern data for the last MINUTES minutes.
Returns plist (:since :total-entries :patterns ((id title count) ...) :error)."
  (let* ((m (or minutes stack-hud-widget-default-window-minutes))
         (since (stack-hud-widget--iso-since m))
         (entries (stack-hud-widget--fetch-evidence since))
         (err (stack-hud-widget--err? entries)))
    (if err
        (list :since since :error err)
      (let ((sorted (stack-hud-widget--patterns-from-entries entries)))
        (list :since since
              :total-entries (length entries)
              :patterns sorted)))))

(defun stack-hud-widget--render-evidence-per-pattern ()
  "Render evidence-per-pattern into the current buffer."
  (let* ((m stack-hud-widget-default-window-minutes)
         (data (stack-hud-widget-data-evidence-per-pattern m))
         (since (plist-get data :since))
         (err (plist-get data :error))
         (patterns (plist-get data :patterns))
         (total (plist-get data :total-entries))
         (top (cl-subseq patterns 0
                         (min (length patterns)
                              stack-hud-widget-pattern-top-n))))
    (insert (propertize "Patterns"
                        'face 'bold
                        'stack-hud-widget-id 'evidence-per-pattern)
            "\n")
    (insert (format "since %s (last %dm) — %s\n\n"
                    since m
                    (or stack-hud-widget-evidence-base-url "?")))
    (cond
     (err
      (insert (propertize (format "  ⚠ %s\n" err) 'face 'error)))
     ((null patterns)
      (insert (format "  no context-retrieval events in last %dm "
                      m))
      (insert (format "(checked %d entries)\n" (or total 0))))
     (t
      (insert (format "  %d entries scanned, %d distinct patterns\n\n"
                      total (length patterns)))
      (let ((max-count (cdar top)))
        (dolist (row top)
          (let* ((id (car (car row)))
                 (title (cdr (car row)))
                 (count (cdr row))
                 (bars (make-string
                        (max 1 (round (* 12.0 (/ (float count)
                                                 max-count))))
                        ?█))
                 (id-str (truncate-string-to-width id 30 nil ?\s "…"))
                 (title-str (truncate-string-to-width title 36 nil ?\s "…")))
            (insert (format "  %2d  %s  %s  %s\n"
                            count
                            (propertize bars 'face 'shadow)
                            (propertize id-str 'face 'font-lock-keyword-face)
                            title-str)))))))
))

;;;###autoload
(defun stack-hud-widget-render-evidence-per-pattern ()
  "Open the evidence-per-pattern widget in its own buffer."
  (interactive)
  (stack-hud-widget--popup "*stack-hud-widget:evidence-per-pattern*"
                           #'stack-hud-widget--render-evidence-per-pattern))

;;; --------------------------------------------------------------------
;;; Widget 2: evidence-per-session
;;; --------------------------------------------------------------------
;;
;; Data source: same fetch as widget 1.  Group entries by
;; :evidence/session-id, count per group.  Show top sessions by count
;; with last activity timestamp.

(defun stack-hud-widget--sessions-from-entries (entries)
  "Return alist ((session-id last-author last-at) . count) sorted by count desc."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let* ((sid (or (plist-get e :evidence/session-id) "(no-session)"))
             (author (or (plist-get e :evidence/author) ""))
             (at (or (plist-get e :evidence/at) ""))
             (cur (gethash sid tbl)))
        (puthash sid
                 (list :count (1+ (or (and cur (plist-get cur :count)) 0))
                       :last-author (if cur (plist-get cur :last-author) author)
                       :last-at (if cur (plist-get cur :last-at) at)
                       :first-at at)
                 tbl)))
    ;; Convert to sorted alist
    (let (acc)
      (maphash (lambda (k v)
                 (push (cons k v) acc))
               tbl)
      (sort acc (lambda (a b)
                  (> (plist-get (cdr a) :count)
                     (plist-get (cdr b) :count)))))))

(defun stack-hud-widget-data-evidence-per-session (&optional minutes)
  "Compute evidence-per-session data for the last MINUTES minutes."
  (let* ((m (or minutes stack-hud-widget-default-window-minutes))
         (since (stack-hud-widget--iso-since m))
         (entries (stack-hud-widget--fetch-evidence since))
         (err (stack-hud-widget--err? entries)))
    (if err
        (list :since since :error err)
      (list :since since
            :total-entries (length entries)
            :sessions (stack-hud-widget--sessions-from-entries entries)))))

(defun stack-hud-widget--short-iso (iso)
  "Shorten ISO timestamp to HH:MM:SS for display."
  (when (and iso (stringp iso) (>= (length iso) 19))
    (substring iso 11 19)))

(defun stack-hud-widget--short-sid (sid)
  "Show first 8 chars of session-id."
  (if (and (stringp sid) (>= (length sid) 8))
      (substring sid 0 8)
    (or sid "?")))

(defun stack-hud-widget--render-evidence-per-session ()
  "Render evidence-per-session into the current buffer."
  (let* ((m stack-hud-widget-default-window-minutes)
         (data (stack-hud-widget-data-evidence-per-session m))
         (since (plist-get data :since))
         (err (plist-get data :error))
         (sessions (plist-get data :sessions))
         (total (plist-get data :total-entries)))
    (insert (propertize "Session Activity"
                        'face 'bold
                        'stack-hud-widget-id 'evidence-per-session)
            "\n")
    (insert (format "since %s (last %dm)\n\n" since m))
    (cond
     (err
      (insert (propertize (format "  ⚠ %s\n" err) 'face 'error)))
     ((null sessions)
      (insert (format "  no evidence in last %dm\n" m)))
     (t
      (insert (format "  %d entries across %d sessions\n\n"
                      total (length sessions)))
      (insert "      Δ  session   author       last       first\n")
      (insert "  ─────────────────────────────────────────────────\n")
      (dolist (row sessions)
        (let* ((sid (car row))
               (info (cdr row))
               (count (plist-get info :count))
               (author (or (plist-get info :last-author) ""))
               (last-at (stack-hud-widget--short-iso
                         (plist-get info :last-at)))
               (first-at (stack-hud-widget--short-iso
                          (plist-get info :first-at))))
          (insert (format "   %4d  %s  %-12s  %s   %s\n"
                          count
                          (propertize (stack-hud-widget--short-sid sid)
                                      'face 'font-lock-keyword-face)
                          (truncate-string-to-width author 12 nil ?\s "…")
                          (or last-at "—")
                          (or first-at "—")))))))
))

;;;###autoload
(defun stack-hud-widget-render-evidence-per-session ()
  "Open the evidence-per-session widget in its own buffer."
  (interactive)
  (stack-hud-widget--popup "*stack-hud-widget:evidence-per-session*"
                           #'stack-hud-widget--render-evidence-per-session))

;;; --------------------------------------------------------------------
;;; Widget 3: sessions-in-flight (Arxana cache pull, no LLM)
;;; --------------------------------------------------------------------
;;
;; Reads cached session items from
;;   `arxana-evidence--open-session-item-cache'
;;   (defined in futon4/dev/arxana-browser-evidence.el:106)
;; WITHOUT triggering `arxana-evidence--apply-open-session-llm-summaries'.
;;
;; Each cached value is a plist with keys from
;; `arxana-evidence--open-session-summary' (line ~726):
;;   :type :buffer :agent :session-id :evidence-server :state
;;   :count :latest :latest-id :about :outcome :missions :artifacts :entries
;;
;; The cache is populated by Arxana Browser → Sessions view.  If Joe
;; hasn't opened that view since Emacs started, the cache may be empty;
;; in that case we fall back to enumerating open REPL buffers via
;; `arxana-evidence--open-repl-sessions' (which is also a no-LLM path)
;; so we still surface live sessions, just without :about / :missions.

(defun stack-hud-widget--cache-available-p ()
  "Non-nil if the Arxana session-item cache is loaded and non-empty."
  (and (boundp 'arxana-evidence--open-session-item-cache)
       (hash-table-p arxana-evidence--open-session-item-cache)
       (> (hash-table-count arxana-evidence--open-session-item-cache) 0)))

(defun stack-hud-widget--cache-items ()
  "Return list of cached session-item plists, deduplicated by session-id."
  (when (stack-hud-widget--cache-available-p)
    (let ((seen (make-hash-table :test 'equal))
          acc)
      (maphash
       (lambda (_k v)
         (let ((sid (or (plist-get v :session-id) "")))
           (unless (gethash sid seen)
             (puthash sid t seen)
             (push v acc))))
       arxana-evidence--open-session-item-cache)
      acc)))

(defun stack-hud-widget--cache-index ()
  "Return cached session items keyed by session-id."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (item (stack-hud-widget--cache-items))
      (let ((sid (plist-get item :session-id)))
        (when (and (stringp sid)
                   (not (string-empty-p sid))
                   (not (gethash sid index)))
          (puthash sid item index))))
    index))

(defun stack-hud-widget--merge-live-session-item (session cache-index)
  "Return SESSION enriched with cached summary fields from CACHE-INDEX."
  (let* ((sid (plist-get session :session-id))
         (cached (and sid (gethash sid cache-index)))
         (item (if cached
                   (copy-sequence cached)
                 (list :type 'evidence-open-session
                       :count nil
                       :latest nil
                       :latest-id nil
                       :about "(no cached summary — open Arxana Sessions to populate)"
                       :outcome ""
                       :missions nil
                       :artifacts nil
                       :entries nil))))
    (dolist (key '(:buffer :agent :session-id :evidence-server :state :latest-id-hint))
      (setq item (plist-put item key (plist-get session key))))
    item))

(defun stack-hud-widget--live-session-items ()
  "Return open REPL sessions enriched with cached Arxana summaries."
  (when (fboundp 'arxana-evidence--open-repl-sessions)
    (let ((cache-index (stack-hud-widget--cache-index)))
      (mapcar (lambda (session)
                (stack-hud-widget--merge-live-session-item session cache-index))
              (condition-case nil
                  (arxana-evidence--open-repl-sessions)
                (error nil))))))

(defun stack-hud-widget--repl-fallback-items ()
  "Return basic session items from open REPL buffers (no cached summary)."
  (when (fboundp 'arxana-evidence--open-repl-sessions)
    (mapcar
     (lambda (s)
       (list :type 'evidence-open-session
             :buffer (plist-get s :buffer)
             :agent (plist-get s :agent)
             :session-id (plist-get s :session-id)
             :state (plist-get s :state)
             :latest-id-hint (plist-get s :latest-id-hint)
             :about "(no cached summary — open Arxana Sessions to populate)"
             :missions nil :artifacts nil
             :count nil :latest nil))
     (condition-case nil
         (arxana-evidence--open-repl-sessions)
       (error nil)))))

(defun stack-hud-widget--age-days (iso)
  "Return age in days for ISO timestamp, or nil if unparseable."
  (when (and iso (stringp iso) (>= (length iso) 19))
    (condition-case nil
        (let* ((then (date-to-time iso))
               (diff (float-time (time-subtract (current-time) then))))
          (/ diff 86400.0))
      (error nil))))

(defun stack-hud-widget--age-tag (iso)
  "Return short age tag for ISO timestamp.
Granularity: <1m → seconds, <1h → minutes, <1d → hours, <14d → days, else weeks."
  (let ((days (stack-hud-widget--age-days iso)))
    (cond
     ((null days) "—")
     ((< days (/ 1.0 1440.0)) (format "%ds" (max 0 (round (* days 86400)))))
     ((< days (/ 1.0 24.0))   (format "%dm" (max 0 (round (* days 1440)))))
     ((< days 1)              (format "%dh" (max 0 (round (* days 24)))))
     ((< days 14)             (format "%dd" (round days)))
     (t                       (format "%dw" (round (/ days 7)))))))

(defcustom stack-hud-widget-session-probe-limit 200
  "Per-session live probe limit for Recent Sessions.
Fetches up to this many entries to compute live count, first-at, and
latest-at.  Sessions with more entries get a `+' suffix on the count."
  :type 'integer :group 'stack-hud-widgets)

(defun stack-hud-widget--session-stats (sid)
  "Probe futon1a for live stats on session SID.
Returns plist (:count :truncated? :latest-at :first-at) or nil on
fetch failure.  Cost: one HTTP probe; no LLM."
  (when (and sid (stringp sid) (not (string-empty-p sid)))
    (let* ((base (string-remove-suffix
                  "/" stack-hud-widget-evidence-base-url))
           (lim stack-hud-widget-session-probe-limit)
           (url (format "%s/api/alpha/evidence?session-id=%s&limit=%d"
                        base (url-hexify-string sid) lim))
           (url-request-method "GET")
           (url-request-extra-headers '(("Accept" . "application/json")))
           (buf (ignore-errors
                  (url-retrieve-synchronously
                   url t t stack-hud-widget-fetch-timeout))))
      (when buf
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\n\n" nil 'move)
          (let* ((data (condition-case nil
                           (json-parse-buffer
                            :object-type 'plist
                            :array-type 'list
                            :null-object nil
                            :false-object nil)
                         (error nil)))
                 (entries (or (plist-get data :entries) '()))
                 (count (length entries)))
            (kill-buffer buf)
            (when entries
              (list :count count
                    :truncated? (= count lim)
                    :latest-at (plist-get (car entries) :evidence/at)
                    :first-at  (plist-get (car (last entries))
                                          :evidence/at)))))))))

(defun stack-hud-widget--decorate-session-item (item)
  "Return ITEM with live stats attached for ordering and display."
  (let* ((sid (plist-get item :session-id))
         (live (stack-hud-widget--session-stats sid)))
    (if (not live)
        item
      (let ((copy (copy-sequence item)))
        (setq copy (plist-put copy :live-stats live))
        (when-let ((count (plist-get live :count)))
          (setq copy (plist-put copy :count count)))
        (when-let ((latest-at (plist-get live :latest-at)))
          (setq copy (plist-put copy :latest latest-at)))
        copy))))

(defun stack-hud-widget--annotate-session-block (start end item)
  "Annotate rendered session block from START to END for ITEM."
  (when-let* ((buffer-name (plist-get item :buffer))
              (buffer (get-buffer buffer-name)))
    (add-text-properties
     start end
     (list 'stack-hud-target-buffer buffer
           'mouse-face 'highlight
           'help-echo (format "RET: jump to %s" buffer-name)))))

(defun stack-hud-widget-data-sessions-in-flight ()
  "Return live session items with cache-backed summaries when available.
Returns plist (:items :source :error)."
  (cond
   ((fboundp 'arxana-evidence--open-repl-sessions)
    (list :source (if (stack-hud-widget--cache-available-p)
                      :live+cache
                    :repl-fallback)
          :items (or (stack-hud-widget--live-session-items) '())))
   ((stack-hud-widget--cache-available-p)
    (let ((items (stack-hud-widget--cache-items)))
      (list :source :cache-only
            :items (sort items
                         (lambda (a b)
                           (string> (or (plist-get a :latest) "")
                                    (or (plist-get b :latest) "")))))))
   (t
    (list :source :unavailable
          :error "Arxana not loaded (cannot read session cache or REPL list)."
          :items nil))))

(defun stack-hud-widget--render-sessions-in-flight ()
  "Render sessions-in-flight into the current buffer.
Truncates to `stack-hud-widget-sessions-top-k' most recently active."
  (let* ((data (stack-hud-widget-data-sessions-in-flight))
         (source (plist-get data :source))
         (all-items
          (sort (mapcar #'stack-hud-widget--decorate-session-item
                        (or (plist-get data :items) '()))
                (lambda (a b)
                  (string> (or (plist-get a :latest) "")
                           (or (plist-get b :latest) "")))))
         (items (seq-take (or all-items '())
                          stack-hud-widget-sessions-top-k))
         (truncated (> (length (or all-items '())) (length items)))
         (err (plist-get data :error)))
    (insert (propertize "Recent Sessions"
                        'face 'bold
                        'stack-hud-widget-id 'sessions-in-flight)
            "\n")
    (pcase source
      (:live+cache    (insert "(Live REPLs + Arxana cache, no LLM)"))
      (:repl-fallback (insert "(REPL buffers — cache empty, no summaries)"))
      (:cache-only    (insert "(Arxana cache only — REPL index unavailable)"))
      (:unavailable   (insert "(Arxana not loaded)")))
    (when truncated
      (insert (format "  · top %d of %d"
                      (length items) (length all-items))))
    (insert "\n\n")
    (cond
     (err
      (insert (propertize (format "  ⚠ %s\n" err) 'face 'error)))
     ((null items)
      (insert "  no sessions in flight.\n"))
     (t
      (dolist (it items)
        (let ((start (point)))
          (let* ((agent (or (plist-get it :agent) "?"))
               (sid (or (plist-get it :session-id) ""))
               (state (or (plist-get it :state) ""))
               (about (or (plist-get it :about) ""))
               (outcome (or (plist-get it :outcome) ""))
               (missions (plist-get it :missions))
               (live (plist-get it :live-stats))
               (count (plist-get it :count))
               (count-str
                (cond
                 ((and live (plist-get live :truncated?))
                  (format "%d+ turns" count))
                 (count (format "%d turns" count))
                 (t "—")))
               (since-first (and live
                                 (stack-hud-widget--age-tag
                                  (plist-get live :first-at))))
               (since-latest (and live
                                  (stack-hud-widget--age-tag
                                   (plist-get live :latest-at))))
               (age-str
                (cond
                 ((and since-first since-latest
                       (not (equal since-first since-latest)))
                  (format "[%s, %s]" since-first since-latest))
                 (since-latest since-latest)
                 (t (or (stack-hud-widget--age-tag
                         (plist-get it :latest)) "—")))))
          (insert (format "  %s %s · %s · %s · %s\n"
                          (propertize (stack-hud-widget--short-sid sid)
                                      'face 'font-lock-keyword-face)
                          (propertize agent 'face 'font-lock-function-name-face)
                          (or state "?")
                          count-str
                          age-str))
          (when (and missions (consp missions))
            (insert (format "    missions: %s\n"
                            (mapconcat #'identity (seq-take missions 3) ", "))))
          (when (and about (not (string-empty-p about)))
            (insert (format "    about:    %s\n"
                            (truncate-string-to-width about 90 nil nil "…"))))
          (when (and outcome (not (string-empty-p outcome)))
            (insert (format "    latest:   %s\n"
                            (truncate-string-to-width outcome 90 nil nil "…"))))
          (insert "\n")
          (stack-hud-widget--annotate-session-block start (point) it))))))
))

;;;###autoload
(defun stack-hud-widget-render-sessions-in-flight ()
  "Open the sessions-in-flight widget."
  (interactive)
  (stack-hud-widget--popup "*stack-hud-widget:sessions-in-flight*"
                           #'stack-hud-widget--render-sessions-in-flight))

;;; --------------------------------------------------------------------
;;; Widget 4: briefing-summary-of-summaries
;;; --------------------------------------------------------------------
;;
;; Joe-named design (2026-04-25): the briefing should NOT duplicate
;; per-session info (Widget 3 shows that directly).  It should be a
;; SUMMARY OF the cached session summaries, weighted by accumulated
;; evidence delta from Widget 2.  Zero LLM calls per refresh.
;;
;; Inputs:
;;   - Cached session items (Widget 3).
;;   - Per-session evidence delta over lookback (Widget 2).
;;
;; Algorithm:
;;   1. Join cached items with session deltas on session-id.
;;   2. Weight = delta * age-decay where age-decay = 1/(1 + days-old).
;;      Sessions with zero recent evidence get zero weight.
;;   3. Sort by weight desc, take top-K (default 3).
;;   4. Aggregate :missions across the top-K — count occurrences.
;;   5. Render: top sessions, themes (top missions), and verbatim
;;      :about lines for the top-1 session.

(defcustom stack-hud-widget-briefing-top-k 3
  "How many top-weighted sessions to feature in the briefing summary."
  :type 'integer
  :group 'stack-hud-widgets)

(defun stack-hud-widget--session-deltas (minutes)
  "Return alist (session-id . delta) for the last MINUTES."
  (let ((data (stack-hud-widget-data-evidence-per-session minutes))
        acc)
    (dolist (row (plist-get data :sessions))
      (push (cons (car row)
                  (plist-get (cdr row) :count))
            acc))
    acc))

(defun stack-hud-widget--weight-item (item delta-alist)
  "Compute weight for ITEM given DELTA-ALIST."
  (let* ((sid (or (plist-get item :session-id) ""))
         (delta (or (cdr (assoc sid delta-alist)) 0))
         (days (or (stack-hud-widget--age-days
                    (plist-get item :latest))
                   30.0))
         (decay (/ 1.0 (+ 1.0 days))))
    (* delta decay)))

(defun stack-hud-widget-data-briefing-summary (&optional minutes)
  "Compute briefing summary-of-summaries data for last MINUTES.
Returns plist (:top :themes :session-count :total-delta :error)."
  (let* ((m (or minutes stack-hud-widget-default-window-minutes))
         (sessions (plist-get (stack-hud-widget-data-sessions-in-flight)
                              :items))
         (deltas (stack-hud-widget--session-deltas m))
         (total-delta (apply #'+ (mapcar (lambda (d) (or (cdr d) 0))
                                         deltas)))
         (weighted (mapcar (lambda (it)
                             (cons it
                                   (stack-hud-widget--weight-item it deltas)))
                           sessions))
         (sorted (sort weighted (lambda (a b) (> (cdr a) (cdr b)))))
         (top (cl-subseq sorted 0
                         (min stack-hud-widget-briefing-top-k
                              (length sorted))))
         (theme-tbl (make-hash-table :test 'equal)))
    ;; Aggregate missions across top items.
    (dolist (entry top)
      (let* ((it (car entry))
             (w (cdr entry))
             (missions (plist-get it :missions)))
        (when (> w 0)
          (dolist (m missions)
            (puthash m
                     (+ (gethash m theme-tbl 0) (or w 0))
                     theme-tbl)))))
    (let (themes)
      (maphash (lambda (k v) (push (cons k v) themes)) theme-tbl)
      (setq themes (sort themes (lambda (a b) (> (cdr a) (cdr b)))))
      (list :window-minutes m
            :session-count (length sessions)
            :total-delta total-delta
            :top top                    ; ((item . weight) ...)
            :themes themes))))          ; ((mission . weight) ...)

(defun stack-hud-widget--render-briefing-summary ()
  "Render the briefing summary-of-summaries into the current buffer."
  (let* ((m stack-hud-widget-default-window-minutes)
         (data (stack-hud-widget-data-briefing-summary m))
         (top (plist-get data :top))
         (themes (plist-get data :themes))
         (sc (plist-get data :session-count))
         (td (plist-get data :total-delta))
         (active-top (cl-remove-if (lambda (e) (zerop (cdr e))) top)))
    (insert (propertize "Briefing Summary"
                        'face 'bold
                        'stack-hud-widget-id 'briefing-summary)
            "\n")
    (insert (format "last %dm — %d sessions, %d new evidence entries\n\n"
                    m sc td))
    (cond
     ((null active-top)
      (insert "  no sessions with recent evidence — quiet window.\n")
      (insert "  (sessions exist in cache but none accumulated evidence ")
      (insert (format "in last %dm)\n" m)))
     (t
      (insert (propertize "Hot now (weighted by recent evidence × recency)\n"
                          'face 'bold))
      (dolist (entry active-top)
        (let* ((it (car entry))
               (w (cdr entry))
               (sid (or (plist-get it :session-id) ""))
               (agent (or (plist-get it :agent) "?"))
               (about (or (plist-get it :about) ""))
               (state (or (plist-get it :state) "")))
          (insert (format "  %s %s · %s · weight %.1f\n"
                          (propertize (stack-hud-widget--short-sid sid)
                                      'face 'font-lock-keyword-face)
                          (propertize agent 'face 'font-lock-function-name-face)
                          state
                          w))
          (when (and about (not (string-empty-p about)))
            (insert (format "    %s\n"
                            (truncate-string-to-width about 90 nil nil "…"))))))
      (when themes
        (insert "\n")
        (insert (propertize "Themes (across top sessions)\n" 'face 'bold))
        (dolist (theme (cl-subseq themes 0 (min 5 (length themes))))
          (insert (format "  • %s  (weight %.1f)\n"
                          (car theme) (cdr theme)))))))
))

;;;###autoload
(defun stack-hud-widget-render-briefing-summary ()
  "Open the briefing summary-of-summaries widget."
  (interactive)
  (stack-hud-widget--popup "*stack-hud-widget:briefing-summary*"
                           #'stack-hud-widget--render-briefing-summary))

;;; --------------------------------------------------------------------
;;; Widget 5: invariant-queue motion (M-invariant-queue-extend tracer board)
;;; --------------------------------------------------------------------
;;
;; Surfaces the live "stuckness or motion" of the structural-law invariant
;; queue. Two data sources, both via the futon1a evidence API:
;;
;;   1. :event :pipeline-tracer-item entries (open) and
;;      :event :pipeline-tracer-closed entries (closed) — work-item
;;      tracers from `futon3c.logic.tracer/default-tracers`. Each open
;;      tracer has a :track-id, :title, :target-date.
;;   2. :event :family-fired entries (probe canary outcomes) — outcome is
;;      one of :ok | :violation | :inactive per family per fire.
;;
;; The widget renders three sections:
;;
;;   - Tracer leaderboard: open count, closed count in window, ratio.
;;   - Per-tracer status with days-until-target.
;;   - Recent family-canary outcomes (last 5 fires, color-coded).
;;
;; Mission: M-invariant-queue-extend (futon3c/holes/missions/).
;; Reframe (Joe 2026-04-29): the apparatus's first real customer is the
;; work that built it; this widget is the live read of whether the queue
;; is flowing.

(defcustom stack-hud-widget-invariant-queue-window-days 7
  "Default lookback window (days) for invariant-queue motion.
The pipeline-tracer reframe targets ≈ 1 week; default matches."
  :type 'integer
  :group 'stack-hud-widgets)

(defcustom stack-hud-widget-invariant-queue-canary-tail 5
  "Number of most-recent family-canary outcomes to render."
  :type 'integer
  :group 'stack-hud-widgets)

(defun stack-hud-widget--days-iso-since (days-ago)
  "ISO-8601 timestamp DAYS-AGO days before now."
  (let* ((seconds-ago (* 60 60 24 days-ago))
         (then (time-subtract (current-time)
                              (seconds-to-time seconds-ago))))
    (format-time-string "%Y-%m-%dT%H:%M:%SZ" then t)))

(defun stack-hud-widget--entry-event (entry)
  "Return the :body :event of an evidence ENTRY (string or symbol-coerced)."
  (let ((body (plist-get entry :evidence/body)))
    (or (plist-get body :event)
        (and (listp body)
             (cl-loop for (k v) on body by #'cddr
                      when (or (eq k 'event) (eq k :event)
                               (and (stringp k) (string= k "event")))
                      return v)))))

(defun stack-hud-widget--entry-tags (entry)
  "Return tags of an evidence ENTRY as a list of strings (lower-cased)."
  (mapcar (lambda (tag)
            (cond
             ((stringp tag) (downcase tag))
             ((symbolp tag) (downcase (symbol-name tag)))
             ((keywordp tag) (downcase (substring (symbol-name tag) 1)))
             (t (format "%s" tag))))
          (or (plist-get entry :evidence/tags) '())))

(defun stack-hud-widget--has-tag? (entry tag)
  "True iff ENTRY's :evidence/tags contains TAG (string match, case-insensitive)."
  (let ((needle (downcase tag)))
    (cl-some (lambda (t1) (string= t1 needle))
             (stack-hud-widget--entry-tags entry))))

(defun stack-hud-widget--body-get (entry key)
  "Get KEY (keyword) from ENTRY's :evidence/body, tolerant of string/symbol keys."
  (let ((body (plist-get entry :evidence/body))
        (kname (substring (symbol-name key) 1)))
    (cond
     ((null body) nil)
     ((hash-table-p body)
      (or (gethash key body)
          (gethash kname body)
          (gethash (intern kname) body)))
     ((listp body)
      (or (plist-get body key)
          (cl-loop for (k v) on body by #'cddr
                   when (or (and (stringp k) (string= k kname))
                            (and (symbolp k) (string= (symbol-name k) kname)))
                   return v))))))

(defun stack-hud-widget--days-until (target-iso)
  "Return integer days from now until TARGET-ISO (ISO date string).
Negative values = past target. Returns nil if TARGET-ISO unparseable."
  (when (and (stringp target-iso) (>= (length target-iso) 10))
    (condition-case _
        (let* ((target (date-to-time (concat (substring target-iso 0 10)
                                             "T00:00:00Z")))
               (now (current-time))
               (delta-secs (float-time (time-subtract target now))))
          (truncate (/ delta-secs (* 60 60 24))))
      (error nil))))

(defun stack-hud-widget--fetch-coordination (since-iso &optional limit)
  "Fetch up to LIMIT coordination entries since SINCE-ISO.
LIMIT defaults to `stack-hud-widget-fetch-limit'."
  (let* ((base (string-remove-suffix "/" stack-hud-widget-evidence-base-url))
         (qs (format "?type=coordination&limit=%d&since=%s"
                     (or limit stack-hud-widget-fetch-limit)
                     (url-hexify-string since-iso)))
         (url (concat base "/api/alpha/evidence" qs))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buf (ignore-errors
                (url-retrieve-synchronously url t t
                                            stack-hud-widget-fetch-timeout))))
    (if (not buf)
        (list (list :error (format "fetch failed: %s" url)))
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (condition-case err
            (let* ((data (if (fboundp 'json-parse-buffer)
                             (json-parse-buffer
                              :object-type 'plist
                              :array-type 'list
                              :null-object nil
                              :false-object nil)
                           (let ((json-object-type 'plist)
                                 (json-array-type 'list)
                                 (json-key-type 'symbol))
                             (json-read))))
                   (entries (plist-get data :entries)))
              (kill-buffer buf)
              entries)
          (error
           (kill-buffer buf)
           (list (list :error (format "parse error: %S" err)))))))))

(defun stack-hud-widget--invariant-queue-data ()
  "Aggregate invariant-queue motion data for the widget."
  (let* ((days stack-hud-widget-invariant-queue-window-days)
         (since (stack-hud-widget--days-iso-since days))
         (entries (stack-hud-widget--fetch-coordination
                   since
                   stack-hud-widget-invariant-queue-fetch-limit))
         (err (stack-hud-widget--err? entries)))
    (if err
        (list :since since :error err)
      (let* ((tracer-entries
              (cl-remove-if-not
               (lambda (e) (stack-hud-widget--has-tag? e "pipeline-tracer"))
               entries))
             (open-tracers
              (cl-remove-if-not
               (lambda (e) (stack-hud-widget--has-tag? e "open"))
               tracer-entries))
             (closed-tracers
              (cl-remove-if-not
               (lambda (e) (stack-hud-widget--has-tag? e "closed"))
               tracer-entries))
             ;; Dedupe open tracers by track-id (latest wins)
             (open-by-track
              (let ((tbl (make-hash-table :test 'equal)))
                (dolist (e open-tracers)
                  (let* ((tid (or (stack-hud-widget--body-get e :track-id) "?"))
                         (key (format "%s" tid))
                         (prior (gethash key tbl))
                         (at (plist-get e :evidence/at)))
                    (when (or (null prior)
                              (and at (string> at (plist-get prior :evidence/at))))
                      (puthash key e tbl))))
                tbl))
             (closed-track-ids
              (let ((s '()))
                (dolist (e closed-tracers)
                  (let ((tid (or (stack-hud-widget--body-get e :track-id) "?")))
                    (push (format "%s" tid) s)))
                s))
             ;; Open set = open tracers whose track-id is NOT in closed-track-ids
             (still-open
              (cl-loop for k being the hash-keys of open-by-track
                       using (hash-values v)
                       unless (member k closed-track-ids)
                       collect (cons k v)))
             (canary-entries
              (cl-remove-if-not
               (lambda (e)
                 (let ((ev (stack-hud-widget--entry-event e)))
                   (or (and (stringp ev) (string= ev "family-fired"))
                       (and (symbolp ev) (string= (symbol-name ev) "family-fired"))
                       (eq ev :family-fired))))
               entries))
             (canary-recent (cl-subseq canary-entries 0
                                       (min stack-hud-widget-invariant-queue-canary-tail
                                            (length canary-entries)))))
        (list :since since
              :window-days days
              :open still-open
              :closed-count (length closed-tracers)
              :total-tracer-entries (length tracer-entries)
              :canary-recent canary-recent
              :canary-total (length canary-entries))))))

(defun stack-hud-widget--motion-flag (open-count closed-count &optional canary-total)
  "Return a propertized motion indicator for the leaderboard header.
OPEN-COUNT and CLOSED-COUNT are pipeline-tracer counts; optional
CANARY-TOTAL is the family-canary fire count in the same window.
STUCK is reserved for genuine no-motion (closed=0 AND canary=0);
PULSING means canary is firing without tracer-flow."
  (cond
   ((and (zerop closed-count) (zerop (or canary-total 0)))
    (propertize "STUCK" 'face '(:foreground "red" :weight bold)))
   ((zerop closed-count)
    ;; Canary firing but no pipeline-tracer flow — system is alive but
    ;; the queue isn't churning. Caught 2026-05-04 by arxana-dramaturge
    ;; test invariant-queue-flag-honesty: STUCK was firing alongside
    ;; non-zero canary-total, which lied about whether the system was
    ;; moving.
    (propertize "PULSING" 'face '(:foreground "yellow" :weight bold)))
   ((>= closed-count open-count)
    (propertize "FLOWING" 'face '(:foreground "green" :weight bold)))
   (t
    (propertize "MOVING" 'face '(:foreground "yellow" :weight bold)))))

(defun stack-hud-widget--canary-face (outcome)
  "Face for a canary OUTCOME string."
  (cond
   ((string= outcome "ok") '(:foreground "green"))
   ((string= outcome "violation") '(:foreground "red" :weight bold))
   ((string= outcome "inactive") '(:foreground "gray"))
   (t 'shadow)))

(defun stack-hud-widget--render-invariant-queue ()
  "Render invariant-queue motion into the current buffer."
  (let* ((data (stack-hud-widget--invariant-queue-data))
         (err (plist-get data :error))
         (since (plist-get data :since))
         (days (plist-get data :window-days))
         (open (plist-get data :open))
         (closed-count (plist-get data :closed-count))
         (canary-recent (plist-get data :canary-recent))
         (canary-total (plist-get data :canary-total))
         (open-count (length open)))
    (insert (propertize "Invariant Queue — motion"
                        'face 'bold
                        'stack-hud-widget-id 'invariant-queue)
            "\n")
    (insert (format "window %dd · since %s\n\n" days since))
    (cond
     (err
      (insert (propertize (format "  ⚠ %s\n" err) 'face 'error)))
     (t
      (insert (format "  %s   open: %d   closed: %d   canary fires: %d\n\n"
                      (stack-hud-widget--motion-flag open-count closed-count canary-total)
                      open-count closed-count canary-total))
      (cond
       ((null open)
        (insert "  no open tracers\n"))
       (t
        (insert "  Open tracers (by track-id, days-to-target):\n")
        (insert "  ─────────────────────────────────────────────────\n")
        (dolist (cell open)
          (let* ((track-id (car cell))
                 (entry (cdr cell))
                 (title (or (stack-hud-widget--body-get entry :title) ""))
                 (target (stack-hud-widget--body-get entry :target-date))
                 (days-until (stack-hud-widget--days-until target))
                 (days-str (cond
                            ((null days-until) "? ")
                            ((< days-until 0)
                             (propertize (format "%+dd" days-until)
                                         'face '(:foreground "red")))
                            ((<= days-until 1)
                             (propertize (format "%+dd" days-until)
                                         'face '(:foreground "yellow")))
                            (t (format "%+dd" days-until)))))
            (insert (format "   %s  %-32s  %s\n"
                            days-str
                            (truncate-string-to-width
                             (format "%s" track-id) 32 nil ?\s "…")
                            (truncate-string-to-width title 38 nil ?\s "…")))))))
      (insert "\n")
      (cond
       ((null canary-recent)
        (insert "  no recent canary fires\n"))
       (t
        (insert (format "  Recent canary fires (last %d of %d):\n"
                        (length canary-recent) canary-total))
        (insert "  ─────────────────────────────────────────────────\n")
        (dolist (entry canary-recent)
          (let* ((fid (or (stack-hud-widget--body-get entry :family-id) "?"))
                 (outcome-raw (stack-hud-widget--body-get entry :outcome))
                 (outcome (cond
                           ((stringp outcome-raw) outcome-raw)
                           ((symbolp outcome-raw)
                            (let ((s (symbol-name outcome-raw)))
                              (if (string-prefix-p ":" s) (substring s 1) s)))
                           (t (format "%s" outcome-raw))))
                 (at (stack-hud-widget--short-iso (plist-get entry :evidence/at))))
            (insert (format "   %s  %-44s  %s\n"
                            (or at "—")
                            (truncate-string-to-width
                             (format "%s" fid) 44 nil ?\s "…")
                            (propertize (or outcome "?")
                                        'face (stack-hud-widget--canary-face outcome))))))))))))

;;;###autoload
(defun stack-hud-widget-render-invariant-queue ()
  "Open the invariant-queue motion widget in its own buffer."
  (interactive)
  (stack-hud-widget--popup "*stack-hud-widget:invariant-queue*"
                           #'stack-hud-widget--render-invariant-queue))

;; Register the widget's doc string.
(setf (alist-get 'invariant-queue stack-hud-widget-docs)
      "Live motion of the structural-law invariant queue.

Shows the apparatus from M-invariant-queue-unstuck +
M-invariant-queue-extend doing its self-test: are the open pipeline-tracers
moving to closed? Are the family-canary fires firing? Three sections:

  · Header line — flow indicator (STUCK/MOVING/FLOWING) + counts.
  · Open tracers — one row per open `:pipeline-tracer-item`, sorted by
    track-id, with days-until-target color-coded (red past, yellow ≤1d).
  · Recent canary fires — last N `:family-fired` entries with outcome
    color-coded (ok=green, violation=red, inactive=gray).

Data source: GET /api/alpha/evidence?type=coordination on futon1a, with
client-side filtering by tag (:pipeline-tracer + :open/:closed) and event
(:family-fired). Window:
  stack-hud-widget-invariant-queue-window-days   (default 7)
  stack-hud-widget-invariant-queue-canary-tail   (default 5)

Refresh: `g'. Cost: one HTTP probe; no LLM call.

Mission: M-invariant-queue-extend (futon3c/holes/missions/).")

;;; --------------------------------------------------------------------
;;; Aggregator
;;; --------------------------------------------------------------------

;;;###autoload
(defun stack-hud-widget-demo-all ()
  "Render all widget demos in a single inspection buffer."
  (interactive)
  (stack-hud-widget--popup
   "*stack-hud-widgets:demo-all*"
   (lambda ()
     (insert (propertize "Stack HUD widget demos" 'face 'bold) "\n")
     (insert "(see futon0/analysis/excursions/E-stack-hud-cleanup.md §3)\n\n")
     (stack-hud-widget--render-evidence-per-pattern)
     (insert "\n")
     (stack-hud-widget--render-evidence-per-session)
     (insert "\n")
     (stack-hud-widget--render-sessions-in-flight)
     (insert "\n")
     (stack-hud-widget--render-briefing-summary)
     (insert "\n")
     (stack-hud-widget--render-invariant-queue))))

(provide 'stack-hud-widgets)

;;; stack-hud-widgets.el ends here
