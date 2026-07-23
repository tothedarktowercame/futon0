;;; usage-report.el --- Usage quotas for Claude, Codex, and ZAI  -*- lexical-binding: t; -*-

;; Wraps ~/code/futon0/contrib/current-usage-report.bb and exposes its
;; snapshot to two surfaces:
;;
;;   - M-x stack-hud          via the `usage' block (auto-registered).
;;   - Arxana Browser headline via `usage-report-arxana-headline-suffix'.
;;
;; The bb script is a pure local read of ~/.claude and ~/.codex JSONL
;; transcripts plus per-session token totals. No model calls, no JVM
;; execution, but the read is ~1 second so we cache snapshots per window.

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)

(defgroup usage-report nil
  "Claude, Codex, and ZAI usage report."
  :group 'tools)

(defcustom usage-report-bb-script
  (expand-file-name "~/code/futon0/contrib/current-usage-report.bb")
  "Path to the bb script that emits the JSON snapshot."
  :type 'file
  :group 'usage-report)

(defcustom usage-report-cache-seconds 30
  "Seconds to reuse a cached snapshot before invoking the bb script again."
  :type 'integer
  :group 'usage-report)

(defcustom usage-report-window-minutes 300
  "Rolling window passed to the bb script for current-window views."
  :type 'integer
  :group 'usage-report)

(defcustom usage-report-claude-week-window-minutes 10080
  "Window passed to the bb script for Claude weekly estimates."
  :type 'integer
  :group 'usage-report)

(defcustom usage-report-claude-output-soft-cap 200000
  "Legacy heuristic Claude output-token budget for the rolling window.
Retained for compatibility, but anchor-derived estimates are preferred."
  :type 'integer
  :group 'usage-report)

(defcustom usage-report-claude-estimate-metric :weighted-input-equiv
  "Claude metric used when deriving estimated usage percentages from anchors.

The metric is a *proxy* for Anthropic's billing basis, not the basis itself.
The bb script's `weighted-input-equiv' formula (input + 1.25*cache-create +
0.1*cache-read; see current-usage-report.bb:92-94) does not include output and
does not match Anthropic's published billing weights exactly. Anchors absorb
that proportionality constant: each anchor records (proxy-value, page-%) at
capture time, so the implied cap is `proxy-value / (page-% / 100)' regardless
of the proxy's absolute scale.

Consequence: if the bb script's formula changes, previously captured anchors
become miscalibrated and must be discarded. Re-capture after any formula edit."
  :type '(choice (const :tag "Weighted input equivalent" :weighted-input-equiv)
                 (const :tag "Output tokens" :output))
  :group 'usage-report)

(defcustom usage-report-claude-anchor-min-samples 1
  "Minimum number of anchors required before showing Claude estimated percentages."
  :type 'integer
  :group 'usage-report)

(defcustom usage-report-claude-anchor-file
  (locate-user-emacs-file "usage-report-claude-anchors.eld")
  "File storing manual Claude usage anchors as printed Lisp data."
  :type 'file
  :group 'usage-report)

(defvar usage-report--cache (make-hash-table :test 'equal)
  "Hash table from WINDOW-MINUTES to (UNIX-TIME . SNAPSHOT-PLIST).
Earlier versions of this file used a single cons cell here; on reload the
defvar does not rebind, so accessors must tolerate a non-hash-table value.")

(defun usage-report--ensure-cache ()
  "Ensure `usage-report--cache' is a hash table.
Recovers transparently when reloading over an older single-cons-cell
binding from a prior version of this file."
  (unless (hash-table-p usage-report--cache)
    (setq usage-report--cache (make-hash-table :test 'equal))))

(defun usage-report--run (&optional window-minutes)
  "Invoke the bb script for WINDOW-MINUTES and return the parsed snapshot, or nil."
  (when (file-executable-p usage-report-bb-script)
    (with-temp-buffer
      (let ((rc (call-process usage-report-bb-script nil t nil
                              "--json"
                              "--window" (number-to-string
                                          (or window-minutes
                                              usage-report-window-minutes)))))
        (when (zerop rc)
          (goto-char (point-min))
          (condition-case _err
              (let ((json-object-type 'plist)
                    (json-array-type 'list)
                    (json-key-type 'keyword)
                    (json-false nil))
                (json-read))
            (error nil)))))))

(defun usage-report-snapshot-for-window (window-minutes &optional force)
  "Return the latest snapshot for WINDOW-MINUTES, using the cache when fresh.
With FORCE non-nil, bypass the cache."
  (usage-report--ensure-cache)
  (let* ((window (or window-minutes usage-report-window-minutes))
         (entry (gethash window usage-report--cache))
         (now (float-time)))
    (when (or force
              (null entry)
              (> (- now (car entry))
                 usage-report-cache-seconds))
      (when-let ((snap (usage-report--run window)))
        (setq entry (cons now snap))
        (puthash window entry usage-report--cache)))
    (cdr entry)))

(defun usage-report-snapshot (&optional force window-minutes)
  "Return the latest snapshot as a plist.
With FORCE non-nil, bypass the cache. WINDOW-MINUTES defaults to
`usage-report-window-minutes'."
  (usage-report-snapshot-for-window (or window-minutes
                                        usage-report-window-minutes)
                                    force))

(defun usage-report-clear-cache ()
  "Drop local and live snapshots so the next call refreshes all usage."
  (interactive)
  (usage-report--ensure-cache)
  (clrhash usage-report--cache)
  (setq usage-report--claude-live-cache nil
        usage-report--claude-live-cache-time 0
        usage-report--zai-live-cache nil
        usage-report--zai-live-cache-time 0)
  (message "[usage-report] cache cleared"))

(defun usage-report--totals (snap source)
  "Return totals block for SOURCE from SNAP."
  (plist-get (plist-get snap :totals) source))

(defun usage-report--read-claude-anchors ()
  "Read and return the stored Claude anchors."
  (if (not (file-exists-p usage-report-claude-anchor-file))
      nil
    (with-temp-buffer
      (insert-file-contents usage-report-claude-anchor-file)
      (goto-char (point-min))
      (if (string-blank-p (buffer-string))
          nil
        (condition-case err
            (read (current-buffer))
          (error
           (error "Failed to read Claude anchor file %s: %s"
                  usage-report-claude-anchor-file
                  (error-message-string err))))))))

(defun usage-report--write-claude-anchors (anchors)
  "Write ANCHORS to `usage-report-claude-anchor-file'."
  (make-directory (file-name-directory usage-report-claude-anchor-file) t)
  (with-temp-file usage-report-claude-anchor-file
    (let ((print-length nil)
          (print-level nil))
      (prin1 anchors (current-buffer))
      (insert "\n"))))

(defun usage-report-claude-anchors ()
  "Return all stored Claude anchors."
  (let ((anchors (usage-report--read-claude-anchors)))
    (if (listp anchors) anchors nil)))

(defun usage-report--iso-now ()
  "Return current UTC time in ISO-8601 format."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))

(defun usage-report--claude-metric-label (&optional metric)
  "Return a readable label for Claude estimate METRIC."
  (pcase (or metric usage-report-claude-estimate-metric)
    (:weighted-input-equiv "weighted")
    (:output "output")
    (_ "metric")))

(defun usage-report--claude-window-metric-value (totals &optional metric)
  "Return Claude METRIC value from TOTALS."
  (let ((value (plist-get totals (or metric usage-report-claude-estimate-metric))))
    (and (numberp value) value)))

(defun usage-report--median (numbers)
  "Return the median of NUMBERS, or nil when NUMBERS is empty."
  (when numbers
    (let* ((sorted (sort (copy-sequence numbers) #'<))
           (len (length sorted))
           (mid (/ len 2)))
      (if (= (% len 2) 1)
          (nth mid sorted)
        (/ (+ (nth (1- mid) sorted)
              (nth mid sorted))
           2.0)))))

(defun usage-report--claude-anchor-window-data (anchor window)
  "Return (USED-PCT TOTALS WINDOW-MINUTES) for ANCHOR WINDOW."
  (pcase window
    (:current (list (plist-get anchor :current-used-pct)
                    (plist-get anchor :current-totals)
                    (plist-get anchor :current-window-minutes)))
    (:week (list (plist-get anchor :week-used-pct)
                 (plist-get anchor :week-totals)
                 (plist-get anchor :week-window-minutes)))
    (_ (error "Unknown Claude anchor window: %s" window))))

(defun usage-report--claude-anchor-implied-cap (anchor window &optional metric)
  "Return the implied full-scale cap for ANCHOR WINDOW under METRIC.

The cap is derived as `metric-value / (used-pct / 100)', i.e. if the page
reported 24% used and the snapshot's metric value at that moment was 24000,
the implied 100%-cap is 100000. Returns nil when used-pct or value is missing
or non-positive (a 0%-used anchor carries no calibration signal)."
  (pcase-let ((`(,used-pct ,totals ,_window-minutes)
               (usage-report--claude-anchor-window-data anchor window)))
    (let ((value (and totals
                      (usage-report--claude-window-metric-value totals metric))))
      (when (and (numberp used-pct)
                 (> used-pct 0.0)
                 (numberp value)
                 (> value 0.0))
        (/ value (/ used-pct 100.0))))))

(defun usage-report--claude-anchor-caps (anchors window &optional metric)
  "Return implied caps from ANCHORS for WINDOW under METRIC."
  (delq nil
        (mapcar (lambda (anchor)
                  (usage-report--claude-anchor-implied-cap anchor window metric))
                anchors)))

(defun usage-report--claude-estimated-cap (anchors window &optional metric)
  "Return the median implied cap from ANCHORS for WINDOW under METRIC."
  (let ((caps (usage-report--claude-anchor-caps anchors window metric)))
    (when (>= (length caps) usage-report-claude-anchor-min-samples)
      (usage-report--median caps))))

(defun usage-report--pct-used (value cap)
  "Return percent used as a float, clamped to [0, 100]."
  (cond ((or (null cap) (<= cap 0.0)) nil)
        ((null value) nil)
        (t (max 0.0 (min 100.0 (* 100.0 (/ (float value) cap)))))))

(defun usage-report--claude-estimates (current-snap week-snap &optional anchors metric)
  "Return Claude estimate plist derived from CURRENT-SNAP and WEEK-SNAP."
  (let* ((anchors (or anchors (usage-report-claude-anchors)))
         (metric (or metric usage-report-claude-estimate-metric))
         (current-totals (usage-report--totals current-snap :claude))
         (week-totals (usage-report--totals week-snap :claude))
         (current-value (and current-totals
                             (usage-report--claude-window-metric-value current-totals metric)))
         (week-value (and week-totals
                          (usage-report--claude-window-metric-value week-totals metric)))
         (current-caps (usage-report--claude-anchor-caps anchors :current metric))
         (week-caps (usage-report--claude-anchor-caps anchors :week metric))
         (current-cap (usage-report--claude-estimated-cap anchors :current metric))
         (week-cap (usage-report--claude-estimated-cap anchors :week metric)))
    (list :metric metric
          :current-cap current-cap
          :current-samples (length current-caps)
          :current-used-pct (usage-report--pct-used current-value current-cap)
          :week-cap week-cap
          :week-samples (length week-caps)
          :week-used-pct (usage-report--pct-used week-value week-cap))))

(defun usage-report-capture-claude-anchor (current-used-pct week-used-pct)
  "Capture a manual Claude settings anchor from the usage page."
  (interactive
   (list (read-number "Claude 5-hour used % from claude.ai/settings/usage: ")
         (read-number "Claude weekly used % from claude.ai/settings/usage: ")))
  (let* ((current-snap (usage-report-snapshot-for-window usage-report-window-minutes t))
         (week-snap (usage-report-snapshot-for-window
                     usage-report-claude-week-window-minutes t))
         (current-totals (usage-report--totals current-snap :claude))
         (week-totals (usage-report--totals week-snap :claude)))
    (unless current-totals
      (user-error "No Claude totals available for current window"))
    (unless week-totals
      (user-error "No Claude totals available for weekly window"))
    (let* ((anchor (list :captured-at (usage-report--iso-now)
                         :current-window-minutes usage-report-window-minutes
                         :week-window-minutes usage-report-claude-week-window-minutes
                         :current-used-pct current-used-pct
                         :week-used-pct week-used-pct
                         :current-totals (copy-tree current-totals)
                         :week-totals (copy-tree week-totals)))
           (anchors (append (usage-report-claude-anchors) (list anchor)))
           (metric usage-report-claude-estimate-metric)
           (current-cap (usage-report--claude-anchor-implied-cap anchor :current metric))
           (week-cap (usage-report--claude-anchor-implied-cap anchor :week metric)))
      (usage-report--write-claude-anchors anchors)
      (message
       "Saved Claude anchor (%s metric): current %.1f%% -> cap %s, week %.1f%% -> cap %s"
       (usage-report--claude-metric-label metric)
       current-used-pct
       (if current-cap (format "%.0f" current-cap) "?")
       week-used-pct
       (if week-cap (format "%.0f" week-cap) "?")))))

;; --- Live usage (Claude OAuth endpoint) ------------------------------------
;;
;; The same endpoint the interactive /usage command hits. Exact utilization
;; percentages — no anchor calibration needed. Undocumented, so every failure
;; degrades to nil and the anchor-based estimate path takes over.

(defcustom usage-report-claude-credentials-file "~/.claude/.credentials.json"
  "Claude Code OAuth credentials file (provides the usage-API bearer token)."
  :type 'file
  :group 'usage-report)

(defcustom usage-report-claude-live-url "https://api.anthropic.com/api/oauth/usage"
  "Claude usage endpoint; what the interactive /usage command queries."
  :type 'string
  :group 'usage-report)

(defcustom usage-report-claude-live-timeout 4
  "Seconds to wait for the Claude usage endpoint."
  :type 'integer
  :group 'usage-report)

(defcustom usage-report-claude-live-cache-seconds 60
  "Seconds to reuse a fetched live usage snapshot."
  :type 'integer
  :group 'usage-report)

(defvar usage-report--claude-live-cache nil)
(defvar usage-report--claude-live-cache-time 0)

(defun usage-report--claude-oauth-token ()
  "Return a non-expired Claude OAuth access token, or nil."
  (condition-case nil
      (let* ((file (expand-file-name usage-report-claude-credentials-file))
             (creds (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (let ((json-object-type 'plist)
                            (json-key-type 'keyword)
                            (json-false nil))
                        (json-read))))
             (oauth (plist-get creds :claudeAiOauth))
             (expires-ms (plist-get oauth :expiresAt)))
        (when (and oauth
                   (numberp expires-ms)
                   (> (/ expires-ms 1000.0) (float-time)))
          (plist-get oauth :accessToken)))
    (error nil)))

(defun usage-report--claude-live-parse (obj)
  "Reduce the usage-endpoint response OBJ to the fields we render."
  (let* ((five (plist-get obj :five_hour))
         (limits (plist-get obj :limits))
         (weekly (seq-find (lambda (l) (equal (plist-get l :kind) "weekly_all"))
                           limits))
         (scoped (seq-filter (lambda (l)
                               (equal (plist-get l :kind) "weekly_scoped"))
                             limits)))
    (list :five-hour-pct (plist-get five :utilization)
          :five-hour-resets (plist-get five :resets_at)
          :weekly-pct (and weekly (plist-get weekly :percent))
          :weekly-resets (and weekly (plist-get weekly :resets_at))
          :scoped (mapcar (lambda (l)
                            (list :model (plist-get
                                          (plist-get (plist-get l :scope) :model)
                                          :display_name)
                                  :pct (plist-get l :percent)
                                  :severity (plist-get l :severity)
                                  :active (plist-get l :is_active)))
                          scoped))))

(defun usage-report--claude-live-fetch ()
  "Fetch live Claude usage; nil on any failure (expired token, timeout, 4xx)."
  (when-let ((token (usage-report--claude-oauth-token)))
    (condition-case nil
        (let ((url-request-extra-headers
               `(("Authorization" . ,(concat "Bearer " token))
                 ("anthropic-beta" . "oauth-2025-04-20"))))
          (when-let ((buf (url-retrieve-synchronously
                           usage-report-claude-live-url
                           t t usage-report-claude-live-timeout)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (when (re-search-forward "HTTP/[^ ]+ 200" nil t)
                    (re-search-forward "^\r?\n" nil t)
                    (let ((json-object-type 'plist)
                          (json-array-type 'list)
                          (json-key-type 'keyword)
                          (json-false nil))
                      (usage-report--claude-live-parse (json-read)))))
              (kill-buffer buf))))
      (error nil))))

(defun usage-report-claude-live (&optional force)
  "Return the cached live Claude usage plist, or nil when unavailable.
With FORCE non-nil, bypass the cache."
  (let ((now (float-time)))
    (when (or force
              (> (- now usage-report--claude-live-cache-time)
                 usage-report-claude-live-cache-seconds))
      (setq usage-report--claude-live-cache (usage-report--claude-live-fetch)
            usage-report--claude-live-cache-time now))
    usage-report--claude-live-cache))

;; --- Live usage (ZAI coding plan) ----------------------------------------

(defcustom usage-report-zai-key-files '("~/.zaikey" "~/.zai-key")
  "Files checked for a ZAI API key after `ZAI_API_KEY'."
  :type '(repeat file)
  :group 'usage-report)

(defcustom usage-report-zai-live-url
  "https://api.z.ai/api/monitor/usage/quota/limit"
  "ZAI coding-plan quota endpoint."
  :type 'string
  :group 'usage-report)

(defcustom usage-report-zai-live-timeout 4
  "Seconds to wait for the ZAI quota endpoint."
  :type 'integer
  :group 'usage-report)

(defcustom usage-report-zai-live-cache-seconds 60
  "Seconds to reuse a fetched ZAI quota snapshot."
  :type 'integer
  :group 'usage-report)

(defvar usage-report--zai-live-cache nil)
(defvar usage-report--zai-live-cache-time 0)

(defun usage-report--zai-api-key ()
  "Return the configured ZAI API key, or nil."
  (or (let ((key (getenv "ZAI_API_KEY")))
        (and key (not (string-empty-p (string-trim key)))
             (string-trim key)))
      (seq-some
       (lambda (path)
         (condition-case nil
             (let ((file (expand-file-name path)))
               (when (file-readable-p file)
                 (with-temp-buffer
                   (insert-file-contents file)
                   (let ((key (string-trim (buffer-string))))
                     (and (not (string-empty-p key)) key)))))
           (error nil)))
       usage-report-zai-key-files)))

(defun usage-report--zai-token-limit (limits unit number)
  "Find the token limit in LIMITS identified by UNIT and NUMBER."
  (seq-find (lambda (limit)
              (and (equal (plist-get limit :type) "TOKENS_LIMIT")
                   (equal (plist-get limit :unit) unit)
                   (equal (plist-get limit :number) number)))
            limits))

(defun usage-report--zai-limit-fields (limit prefix)
  "Return normalized fields from LIMIT, using PREFIX in their keys."
  (when limit
    (let* ((used (plist-get limit :percentage))
           (free (and (numberp used) (max 0.0 (- 100.0 used)))))
      (list (intern (format ":%s-used-pct" prefix)) used
            (intern (format ":%s-free-pct" prefix)) free
            (intern (format ":%s-resets-ms" prefix))
            (plist-get limit :nextResetTime)))))

(defun usage-report--zai-live-parse (obj)
  "Reduce ZAI quota response OBJ to the coding-plan fields we render."
  (when (plist-get obj :success)
    (let* ((data (plist-get obj :data))
           (limits (plist-get data :limits))
           ;; The API identifies the two token windows as 5 × unit 3 and
           ;; 1 × unit 6. TIME_LIMIT is a separate MCP/search allowance.
           (five (usage-report--zai-token-limit limits 3 5))
           (week (usage-report--zai-token-limit limits 6 1)))
      (when (or five week)
        (append (list :level (plist-get data :level))
                (usage-report--zai-limit-fields five "five-hour")
                (usage-report--zai-limit-fields week "weekly"))))))

(defun usage-report--zai-live-fetch ()
  "Fetch live ZAI coding-plan quota; return nil on any failure."
  (when-let ((key (usage-report--zai-api-key)))
    (condition-case nil
        (let ((url-request-extra-headers
               `(("Authorization" . ,key)
                 ("Accept" . "application/json"))))
          (when-let ((buf (url-retrieve-synchronously
                           usage-report-zai-live-url
                           t t usage-report-zai-live-timeout)))
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (when (re-search-forward "HTTP/[^ ]+ 200" nil t)
                    (re-search-forward "^\r?\n" nil t)
                    (let ((json-object-type 'plist)
                          (json-array-type 'list)
                          (json-key-type 'keyword)
                          (json-false nil))
                      (usage-report--zai-live-parse (json-read)))))
              (kill-buffer buf))))
      (error nil))))

(defun usage-report-zai-live (&optional force)
  "Return cached live ZAI quota, or nil when unavailable.
With FORCE non-nil, bypass the cache."
  (let ((now (float-time)))
    (when (or force
              (> (- now usage-report--zai-live-cache-time)
                 usage-report-zai-live-cache-seconds))
      (setq usage-report--zai-live-cache (usage-report--zai-live-fetch)
            usage-report--zai-live-cache-time now))
    usage-report--zai-live-cache))

(defun usage-report--claude-live-implied-cap (live-pct window-value)
  "Cap implied by LIVE-PCT covering WINDOW-VALUE of the local proxy metric.
Assumes the window's spend is all local (headless/phone spend inflates
per-session shares); still fresher than a manually captured anchor."
  (when (and (numberp live-pct) (> live-pct 0.0)
             (numberp window-value) (> window-value 0.0))
    (/ (float window-value) (/ live-pct 100.0))))

(defun usage-report--resets-clock (resets)
  "Format an ISO RESETS timestamp as HH:MMZ."
  (if (and (stringp resets) (>= (length resets) 16))
      (concat (substring resets 11 16) "Z")
    "?"))

(defun usage-report--resets-day-clock (resets)
  "Format an ISO RESETS timestamp as MM-DD HH:MMZ."
  (if (and (stringp resets) (>= (length resets) 16))
      (concat (substring resets 5 10) " " (substring resets 11 16) "Z")
    "?"))

(defun usage-report--epoch-ms-clock (epoch-ms &optional include-day)
  "Format EPOCH-MS in UTC, including the month/day when INCLUDE-DAY is non-nil."
  (if (numberp epoch-ms)
      (format-time-string (if include-day "%m-%d %H:%MZ" "%H:%MZ")
                          (seconds-to-time (/ epoch-ms 1000.0)) t)
    "?"))

(defun usage-report--claude-scoped-warnings (live)
  "Return scoped-limit entries from LIVE worth surfacing (non-normal or active)."
  (seq-filter (lambda (s)
                (and (numberp (plist-get s :pct))
                     (or (plist-get s :active)
                         (not (equal (plist-get s :severity) "normal")))))
              (and live (plist-get live :scoped))))

(defun usage-report-capture-claude-anchor-live ()
  "Capture a Claude anchor from the live usage endpoint (no page visit).
Keeps the anchor fallback calibrated for when the endpoint is unreachable."
  (interactive)
  (let ((live (usage-report-claude-live t)))
    (unless live
      (user-error "Live Claude usage unavailable (token expired or endpoint down)"))
    (let ((five (plist-get live :five-hour-pct))
          (week (plist-get live :weekly-pct)))
      (unless (and (numberp five) (> five 0.0)
                   (numberp week) (> week 0.0))
        (user-error "Live percentages missing or zero (no calibration signal): 5h=%s wk=%s"
                    five week))
      (usage-report-capture-claude-anchor five week))))

;; --- Formatting helpers ---------------------------------------------------

(defun usage-report--fmt-pct (pct)
  (cond ((null pct) "?")
        ((numberp pct) (format "%.1f%%" pct))
        (t (format "%s" pct))))

(defun usage-report--fmt-int (n)
  (cond ((null n) "?")
        ((numberp n)
         (cond ((>= n 1000000) (format "%.1fM" (/ n 1e6)))
               ((>= n 1000)    (format "%.1fk" (/ n 1e3)))
               (t (format "%d" n))))
        (t (format "%s" n))))

(defun usage-report--short-id (id)
  (let ((s (format "%s" (or id ""))))
    (if (> (length s) 8) (substring s 0 8) s)))

(defun usage-report--codex-reset-clock (resets &optional include-day)
  "Format Codex ISO RESET time, including month/day when INCLUDE-DAY is non-nil."
  (if (and (stringp resets) (>= (length resets) 16))
      ;; 2026-04-25T15:56:03Z -> 15:56Z or 04-25 15:56Z.
      (concat (if include-day (concat (substring resets 5 10) " ") "")
              (substring resets 11 16) "Z")
    "?"))

(defun usage-report--codex-resets-fragment (snap)
  "Return labelled Codex reset clocks from SNAP."
  (let* ((totals (usage-report--totals snap :codex))
         (five (plist-get totals :five-hour-resets-at))
         (week (plist-get totals :weekly-resets-at))
         (legacy (plist-get totals :resets-at)))
    (cond
     ((and five week)
      (format "5h %s · wk %s"
              (usage-report--codex-reset-clock five)
              (usage-report--codex-reset-clock week t)))
     (five (format "5h %s" (usage-report--codex-reset-clock five)))
     (week (format "wk %s" (usage-report--codex-reset-clock week t)))
     (legacy (usage-report--codex-reset-clock legacy))
     (t "?"))))

(defun usage-report--codex-usage-fragment (snap freep)
  "Return labelled Codex usage from SNAP.
When FREEP is non-nil, report remaining quota; otherwise report used quota."
  (let* ((totals (usage-report--totals snap :codex))
         (five (plist-get totals :five-hour-used-pct-max))
         (week (plist-get totals :weekly-used-pct-max))
         (legacy (plist-get totals :used-pct-max))
         (convert (lambda (pct) (if freep (max 0.0 (- 100.0 pct)) pct))))
    (cond
     ((and (numberp five) (numberp week))
      (format "%.0f%% 5h / %.0f%% wk"
              (funcall convert five) (funcall convert week)))
     ((numberp five) (format "%.0f%% 5h" (funcall convert five)))
     ((numberp week) (format "%.0f%% wk" (funcall convert week)))
     ((numberp legacy) (format "%.0f%%" (funcall convert legacy)))
     (t "?"))))

(defun usage-report--claude-headline-fragment (current-snap week-snap anchors)
  "Return Claude fragment for a headline using CURRENT-SNAP, WEEK-SNAP, and ANCHORS.
Prefers exact live percentages from the usage endpoint; falls back to
anchor-based estimates (labelled est), then raw magnitudes."
  (let* ((live (usage-report-claude-live))
         (live-5h (and live (plist-get live :five-hour-pct)))
         (live-wk (and live (plist-get live :weekly-pct)))
         (current-totals (usage-report--totals current-snap :claude))
         (week-totals (and week-snap (usage-report--totals week-snap :claude)))
         (estimates (and current-totals
                         week-totals
                         anchors
                         (usage-report--claude-estimates current-snap week-snap anchors))))
    (cond
     ((and (numberp live-5h) (numberp live-wk))
      (let ((scoped (usage-report--claude-scoped-warnings live)))
        (concat (format "Claude %.0f%% 5h / %.0f%% wk" live-5h live-wk)
                (mapconcat (lambda (s)
                             (format " · %s wk %.0f%%⚠"
                                     (or (plist-get s :model) "scoped")
                                     (plist-get s :pct)))
                           scoped ""))))
     ((numberp live-5h)
      (format "Claude %.0f%% 5h" live-5h))
     ((and estimates
           (numberp (plist-get estimates :current-used-pct))
           (numberp (plist-get estimates :week-used-pct)))
      (format "Claude est %.0f%% 5h / %.0f%% wk"
              (plist-get estimates :current-used-pct)
              (plist-get estimates :week-used-pct)))
     ((and estimates
           (numberp (plist-get estimates :current-used-pct)))
      (format "Claude est %.0f%% 5h · %s out / %s msgs"
              (plist-get estimates :current-used-pct)
              (usage-report--fmt-int (plist-get current-totals :output))
              (usage-report--fmt-int (plist-get current-totals :messages))))
     (t
      (format "Claude %s out / %s msgs"
              (usage-report--fmt-int (plist-get current-totals :output))
              (usage-report--fmt-int (plist-get current-totals :messages)))))))

(defun usage-report--zai-headline-fragment (&optional live)
  "Return a compact free-quota summary from ZAI LIVE usage."
  (let* ((live (or live (usage-report-zai-live)))
         (five (and live (plist-get live :five-hour-free-pct)))
         (week (and live (plist-get live :weekly-free-pct))))
    (cond
     ((and (numberp five) (numberp week))
      (format "ZAI %.0f%% 5h / %.0f%% wk free" five week))
     ((numberp five) (format "ZAI %.0f%% 5h free" five))
     ((numberp week) (format "ZAI %.0f%% wk free" week)))))

(defun usage-report-headline-string (&optional snap)
  "Return a one-line summary suitable for a header-line."
  (let* ((snap (or snap (usage-report-snapshot)))
         (anchors (usage-report-claude-anchors))
         (zai-fragment (usage-report--zai-headline-fragment)))
    (if (null snap)
        (if zai-fragment
            (format "Usage: (script unavailable) · %s" zai-fragment)
          "Usage: (script unavailable)")
      (let* ((week-snap (and anchors
                             (usage-report-snapshot-for-window
                              usage-report-claude-week-window-minutes)))
             (claude-fragment (usage-report--claude-headline-fragment
                               snap week-snap anchors)))
        (concat
         (format "Codex %s free · %s"
                 (usage-report--codex-usage-fragment snap t)
                 claude-fragment)
         (if zai-fragment (concat " · " zai-fragment) "")
         (format " · resets %s" (usage-report--codex-resets-fragment snap)))))))

(defun usage-report-arxana-headline-suffix (&optional snap)
  "Return ` · <usage>' to append to an Arxana Browser headline, or empty.
Doubles `%' so the result is safe to embed in `header-line-format' /
`mode-line-format' through `format-mode-line'."
  (let ((line (usage-report-headline-string snap)))
    (if (and line (not (string-empty-p line)))
        (concat " · " (replace-regexp-in-string "%" "%%" line))
      "")))

(defun usage-report-headline-prefix (&optional snap)
  "Return `<usage> · ' to prepend to an Arxana Browser headline, or empty.
Like `usage-report-arxana-headline-suffix' but punctuated for the front of
a headline so the usage info stays visible regardless of window width.
Doubles `%' for `format-mode-line' safety."
  (let ((line (usage-report-headline-string snap)))
    (if (and line (not (string-empty-p line)))
        (concat (replace-regexp-in-string "%" "%%" line) " · ")
      "")))

;; --- Stack HUD block ------------------------------------------------------

(defun usage-report--insert-codex (snap)
  (let* ((totals (usage-report--totals snap :codex))
         (sessions (plist-get snap :codex)))
    (insert (format "    Codex: %s used  (max across %d sess)\n"
                    (usage-report--codex-usage-fragment snap nil)
                    (or (plist-get totals :active-sessions) 0)))
    (insert (format "      resets: %s\n"
                    (usage-report--codex-resets-fragment snap)))
    (dolist (s sessions)
      (let* ((limits (plist-get s :rate-limits))
             (five (plist-get (plist-get limits :five-hour) :used-pct))
             (week (plist-get (plist-get limits :weekly) :used-pct))
             (legacy (plist-get (plist-get s :rate-limit) :used-pct))
             (quota (cond
                     ((and (numberp five) (numberp week))
                      (format "%.1f%% 5h / %.1f%% wk" five week))
                     ((numberp five) (format "%.1f%% 5h" five))
                     ((numberp week) (format "%.1f%% wk" week))
                     (t (usage-report--fmt-pct legacy))))
             (u (plist-get s :usage)))
        (insert (format "      %s  used %s  total %s\n"
                        (usage-report--short-id (plist-get s :session-id))
                        quota
                        (usage-report--fmt-int (plist-get u :total))))))))

(defun usage-report--insert-claude (current-snap week-snap anchors)
  (let* ((live (usage-report-claude-live))
         (live-5h (and live (plist-get live :five-hour-pct)))
         (live-wk (and live (plist-get live :weekly-pct)))
         (totals (usage-report--totals current-snap :claude))
         (sessions (plist-get current-snap :claude))
         (estimates (and anchors
                         week-snap
                         (usage-report--claude-estimates current-snap week-snap anchors)))
         (metric (or (plist-get estimates :metric)
                     usage-report-claude-estimate-metric))
         (metric-label (usage-report--claude-metric-label metric))
         (current-value (and totals
                             (usage-report--claude-window-metric-value totals metric)))
         ;; Live-implied cap beats a manually captured anchor: it recalibrates
         ;; every fetch. Anchor cap remains the offline fallback.
         (current-cap (or (usage-report--claude-live-implied-cap live-5h current-value)
                          (and estimates (plist-get estimates :current-cap))))
         (current-pct (or live-5h
                          (and estimates (plist-get estimates :current-used-pct))))
         (week-pct (or live-wk
                       (and estimates (plist-get estimates :week-used-pct))))
         (week-cap (and estimates (plist-get estimates :week-cap)))
         (n-sess (or (plist-get totals :active-sessions) 0))
         (n-msgs (or (plist-get totals :messages) 0)))
    ;; Top line: exact live percentages when the endpoint answers; anchor
    ;; estimates (labelled est) when not; raw magnitude when uncalibrated.
    (cond
     ((and current-pct week-pct)
      (insert (format "    Claude: %.1f%% 5h / %.1f%% wk used%s  (%d sess, %d msgs)\n"
                      current-pct week-pct
                      (if live-5h "" " (est)")
                      n-sess n-msgs)))
     (current-pct
      (insert (format "    Claude: %.1f%% 5h used%s  (%d sess, %d msgs)\n"
                      current-pct
                      (if live-5h "" " (est)")
                      n-sess n-msgs)))
     (t
      (insert (format "    Claude: %s weighted  (%d sess, %d msgs)\n"
                      (usage-report--fmt-int
                       (plist-get totals :weighted-input-equiv))
                      n-sess n-msgs))))
    ;; Reset clocks and scoped model limits only exist on the live path.
    (when live
      (insert (format "      resets: 5h %s · wk %s\n"
                      (usage-report--resets-clock (plist-get live :five-hour-resets))
                      (usage-report--resets-day-clock (plist-get live :weekly-resets))))
      (dolist (s (usage-report--claude-scoped-warnings live))
        (insert (propertize
                 (format "      %s: %.0f%% wk (model-scoped limit, %s)\n"
                         (or (plist-get s :model) "scoped")
                         (plist-get s :pct)
                         (or (plist-get s :severity) "active"))
                 'face 'warning))))
    ;; Per-session: scale each session's metric by the current-window cap so
    ;; rows are directly comparable to Codex's per-session %.
    (dolist (s sessions)
      (let* ((u (plist-get s :usage))
             (sid (usage-report--short-id (plist-get s :session-id)))
             (msgs (or (plist-get u :messages) 0))
             (value (usage-report--claude-window-metric-value u metric))
             (pct (and current-cap value (> current-cap 0)
                       (* 100.0 (/ (float value) current-cap)))))
        (if pct
            (insert (format "      %s  used %4.1f%%  msgs %d\n" sid pct msgs))
          (insert (format "      %s  out %s  msgs %d\n"
                          sid
                          (usage-report--fmt-int (plist-get u :output))
                          msgs)))))
    ;; Provenance footer: where the numbers came from, in one line.
    (cond
     (live-5h
      (insert (propertize
               (format "      source: usage api (live)%s\n"
                       (if current-cap
                           (format " · 5h cap≈%s implied (per-session %% assumes local spend)"
                                   (usage-report--fmt-int current-cap))
                         ""))
               'face 'shadow)))
     (estimates
      (insert (propertize
               (format "      caps: 5h=%s / wk=%s  (%d %s anchor%s in %s)\n"
                       (usage-report--fmt-int current-cap)
                       (usage-report--fmt-int week-cap)
                       (plist-get estimates :current-samples)
                       metric-label
                       (if (= (plist-get estimates :current-samples) 1) "" "s")
                       usage-report-claude-anchor-file)
               'face 'shadow)))
     (t
      (insert (propertize
               "      est unavailable; live api down and no anchors — M-x usage-report-capture-claude-anchor-live\n"
               'face 'shadow))))))

(defun usage-report--insert-zai (&optional live)
  "Insert ZAI coding-plan quota details from LIVE usage."
  (let* ((live (or live (usage-report-zai-live)))
         (five-used (and live (plist-get live :five-hour-used-pct)))
         (five-free (and live (plist-get live :five-hour-free-pct)))
         (week-used (and live (plist-get live :weekly-used-pct)))
         (week-free (and live (plist-get live :weekly-free-pct)))
         (level (and live (plist-get live :level))))
    (if (not live)
        (insert (propertize "    ZAI: live quota unavailable\n" 'face 'shadow))
      (insert (format "    ZAI: %s 5h / %s wk used  (%s / %s free%s)\n"
                      (usage-report--fmt-pct five-used)
                      (usage-report--fmt-pct week-used)
                      (usage-report--fmt-pct five-free)
                      (usage-report--fmt-pct week-free)
                      (if (and (stringp level) (not (string-empty-p level)))
                          (format "; %s plan" (capitalize level))
                        "")))
      (insert (format "      resets: 5h %s · wk %s\n"
                      (usage-report--epoch-ms-clock
                       (plist-get live :five-hour-resets-ms))
                      (usage-report--epoch-ms-clock
                       (plist-get live :weekly-resets-ms) t)))
      (insert (propertize "      source: ZAI quota api (live)\n" 'face 'shadow)))))

(defun usage-report-insert-stack-hud-block ()
  "Render the Token Usage block into the current buffer.
Bound into stack-hud via the `usage' key in `stack-hud-blocks'."
  (insert "  Usage:\n")
  (let ((snap (usage-report-snapshot))
        (anchors (usage-report-claude-anchors))
        (zai-live (usage-report-zai-live)))
    (cond
     ((null snap)
      (insert (propertize "    (script unavailable)\n" 'face 'shadow)))
     ((and (null (plist-get snap :codex))
           (null (plist-get snap :claude)))
      (insert "    (no sessions in window)\n"))
     (t
      (let ((week-snap (and anchors
                            (usage-report-snapshot-for-window
                             usage-report-claude-week-window-minutes))))
        (insert (format "    %s\n" (usage-report-headline-string snap)))
        (usage-report--insert-codex snap)
        (usage-report--insert-claude snap week-snap anchors))))
    ;; ZAI is independent of the local transcript snapshot, so keep it visible
    ;; when there are no sessions or the bb script is unavailable.
    (usage-report--insert-zai zai-live)))

;; Stack-hud auto-registration: only takes effect once stack-hud.el is loaded
;; and its dispatch is taught about `usage'. The dispatch edit lives in
;; stack-hud.el; this file provides the renderer it dispatches to under the
;; conventional `my-chatgpt-shell--insert-stack-X' name.
(defalias 'my-chatgpt-shell--insert-stack-usage
  #'usage-report-insert-stack-hud-block
  "Stack HUD block renderer for token usage. See `usage-report-insert-stack-hud-block'.")

;; --- Interactive entry point ---------------------------------------------

(defun usage-report-show ()
  "Pop a buffer with the human-readable usage report.
Equivalent to running the bb script directly, but goes through the cache."
  (interactive)
  (let ((buf (get-buffer-create "*usage-report*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "=== Token Usage Report ===\n\n")
        (insert (usage-report-headline-string) "\n\n")
        (usage-report-insert-stack-hud-block))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf)))

(provide 'usage-report)
;;; usage-report.el ends here
