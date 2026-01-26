;;; futon0-rhythm.el --- Ensemble-based life rhythm tracking -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; PSR/PUR framework for life rhythm without life-logging.
;;
;; PSR (Pattern Selection Record): Clock-in with frame + weighted pattern stack
;; PUR (Pattern Use Record): Clock-out with actual weights + telemetry delta
;;
;; Usage:
;;   M-x futon0-rhythm-clock-in    ; morning: set frame + stack
;;   M-x futon0-rhythm-clock-out   ; evening: record actuals, get verdict
;;   M-x futon0-rhythm-status      ; check current ensemble
;;
;; Ensemble model:
;;   - Frame: day-level mode (e.g., 坤/recovery, 乾/push, 泰/balanced)
;;   - Stack: weighted patterns running in parallel (q1, q2, q3, q4, etc.)
;;   - Weights should sum to 1.0 (or close)
;;
;; Integrates with:
;;   - Ultrahuman ring (when ultrahuman_daily.json available)
;;   - Vitality scanner (git, files)
;;   - Weekly bid/clear system (track.clj)

;;; Code:

(require 'json)
(require 'subr-x)

(defgroup futon0-rhythm nil
  "Ensemble-based life rhythm tracking."
  :group 'futon0)

(defcustom futon0-rhythm-storage-dir
  (expand-file-name "~/code/storage/futon0/rhythm/ensembles")
  "Directory for storing daily ensemble PSR/PUR records."
  :type 'directory
  :group 'futon0-rhythm)

(defcustom futon0-rhythm-ultrahuman-file
  (expand-file-name "~/code/storage/futon0/vitality/ultrahuman/ultrahuman_daily.json")
  "Path to Ultrahuman daily metrics JSON."
  :type 'file
  :group 'futon0-rhythm)

(defcustom futon0-rhythm-frames
  '("泰" "乾" "坤" "transitional")
  "Available day-level frames (hexagram modes)."
  :type '(repeat string)
  :group 'futon0-rhythm)

(defcustom futon0-rhythm-patterns
  '("q1" "q2" "q3" "q4" "sleep" "recovery" "maintenance" "slack" "job-search")
  "Available patterns for stack composition."
  :type '(repeat string)
  :group 'futon0-rhythm)

(defcustom futon0-rhythm-default-weights
  '(("q4" . 0.4) ("q2" . 0.3) ("q3" . 0.2) ("slack" . 0.1))
  "Default pattern weights for quick clock-in."
  :type '(alist :key-type string :value-type number)
  :group 'futon0-rhythm)

;; Internal state
(defvar futon0-rhythm--current-psr nil
  "Current PSR (nil if not clocked in).")

(defvar futon0-rhythm--history nil
  "Recent PSR/PUR pairs for quick reference.")

;;; Utilities

(defun futon0-rhythm--today ()
  "Return today's date as YYYY-MM-DD string."
  (format-time-string "%Y-%m-%d"))

(defun futon0-rhythm--now ()
  "Return current time as HH:MM string."
  (format-time-string "%H:%M"))

(defun futon0-rhythm--ensure-storage-dir ()
  "Create storage directory if it doesn't exist."
  (unless (file-directory-p futon0-rhythm-storage-dir)
    (make-directory futon0-rhythm-storage-dir t)))

(defun futon0-rhythm--daily-file (&optional date)
  "Return path to daily ensemble file for DATE (default today)."
  (expand-file-name (format "%s.edn" (or date (futon0-rhythm--today)))
                    futon0-rhythm-storage-dir))

(defun futon0-rhythm--read-ultrahuman ()
  "Read current Ultrahuman metrics, or nil if unavailable."
  (when (file-readable-p futon0-rhythm-ultrahuman-file)
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'list))
          (json-read-file futon0-rhythm-ultrahuman-file))
      (error nil))))

(defun futon0-rhythm--ultrahuman-snapshot ()
  "Extract key metrics from Ultrahuman for PSR/PUR."
  (when-let ((data (futon0-rhythm--read-ultrahuman)))
    (list :recovery (alist-get 'recovery data)
          :avg-sleep-hrv (alist-get 'avg_sleep_hrv data)
          :total-sleep (alist-get 'total_sleep_minutes data)
          :active-minutes (alist-get 'active_minutes data))))

(defun futon0-rhythm--git-activity-since (start-time)
  "Count git commits since START-TIME (HH:MM string)."
  (let ((default-directory (expand-file-name "~/code")))
    (condition-case nil
        (let* ((today (futon0-rhythm--today))
               (since (format "%sT%s:00" today start-time))
               (output (shell-command-to-string
                        (format "find . -maxdepth 2 -name .git -type d 2>/dev/null | while read d; do git -C \"$(dirname \"$d\")\" log --oneline --since='%s' 2>/dev/null; done | wc -l"
                                since))))
          (string-to-number (string-trim output)))
      (error 0))))

(defun futon0-rhythm--format-stack (stack)
  "Format STACK for display."
  (mapconcat (lambda (item)
               (format "%s: %.0f%%"
                       (plist-get item :pattern)
                       (* 100 (plist-get item :weight))))
             stack
             ", "))

(defun futon0-rhythm--compute-verdict (psr pur)
  "Compute verdict string comparing PSR to PUR."
  (let* ((psr-stack (plist-get psr :stack))
         (pur-stack (plist-get pur :stack))
         (frame-held (equal (plist-get psr :frame) (plist-get pur :frame)))
         (expansions nil)
         (contractions nil))
    ;; Find patterns that expanded or contracted
    (dolist (pur-item pur-stack)
      (let* ((pattern (plist-get pur-item :pattern))
             (delta (plist-get pur-item :delta)))
        (when delta
          (cond
           ((> delta 0.15) (push pattern expansions))
           ((< delta -0.15) (push pattern contractions))))))
    ;; Build verdict
    (concat
     (if frame-held "frame-held" "frame-shifted")
     (when expansions
       (format "; %s-expansion" (string-join (reverse expansions) "/")))
     (when contractions
       (format "; %s-squeezed" (string-join (reverse contractions) "/"))))))

;;; PSR/PUR persistence

(defun futon0-rhythm--save-record (record)
  "Save RECORD to daily file."
  (futon0-rhythm--ensure-storage-dir)
  (let ((file (futon0-rhythm--daily-file)))
    (with-temp-file file
      (insert ";; futon0-rhythm ensemble record\n")
      (insert ";; Generated: " (format-time-string "%Y-%m-%dT%H:%M:%S") "\n\n")
      (pp record (current-buffer)))))

(defun futon0-rhythm--load-record (&optional date)
  "Load record for DATE (default today), or nil."
  (let ((file (futon0-rhythm--daily-file date)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            ;; Skip comments
            (while (looking-at "^;")
              (forward-line 1))
            (read (current-buffer)))
        (error nil)))))

;;; Interactive commands

(defun futon0-rhythm--read-weight (pattern current-weight)
  "Read actual weight for PATTERN, showing CURRENT-WEIGHT as reference."
  (let* ((prompt (format "%s (bid: %.0f%%): " pattern (* 100 current-weight)))
         (input (read-string prompt)))
    (if (string-empty-p input)
        current-weight
      (/ (string-to-number input) 100.0))))

;;;###autoload
(defun futon0-rhythm-clock-in (frame)
  "Clock in with FRAME and build pattern stack interactively."
  (interactive
   (list (completing-read "Frame: " futon0-rhythm-frames nil t)))
  (when futon0-rhythm--current-psr
    (if (yes-or-no-p "Already clocked in. Clock out first? ")
        (futon0-rhythm-clock-out)
      (user-error "Aborted")))
  (let ((stack nil)
        (total-weight 0.0)
        (continue t))
    ;; Build stack interactively
    (while continue
      (let* ((remaining (- 1.0 total-weight))
             (pattern (completing-read
                       (format "Pattern (%.0f%% remaining, RET to finish): "
                               (* 100 remaining))
                       futon0-rhythm-patterns nil nil))
             (default-weight (min remaining
                                   (or (cdr (assoc pattern futon0-rhythm-default-weights))
                                       0.25))))
        (if (string-empty-p pattern)
            (setq continue nil)
          (let* ((weight-str (read-string
                              (format "Weight for %s (default %.0f%%): "
                                      pattern (* 100 default-weight))))
                 (weight (if (string-empty-p weight-str)
                             default-weight
                           (/ (string-to-number weight-str) 100.0))))
            (push (list :pattern pattern :weight weight) stack)
            (setq total-weight (+ total-weight weight))))))
    ;; Build PSR
    (let ((psr (list :frame frame
                     :stack (nreverse stack)
                     :time (futon0-rhythm--now)
                     :ultrahuman (futon0-rhythm--ultrahuman-snapshot))))
      (setq futon0-rhythm--current-psr psr)
      ;; Save partial record
      (futon0-rhythm--save-record (list :date (futon0-rhythm--today)
                                         :psr psr))
      (message "Clocked in: %s | %s" frame (futon0-rhythm--format-stack (plist-get psr :stack))))))

;;;###autoload
(defun futon0-rhythm-clock-in-quick ()
  "Clock in quickly with default weights."
  (interactive)
  (let* ((frame (completing-read "Frame: " futon0-rhythm-frames nil t))
         (stack (mapcar (lambda (pair)
                          (list :pattern (car pair) :weight (cdr pair)))
                        futon0-rhythm-default-weights))
         (psr (list :frame frame
                    :stack stack
                    :time (futon0-rhythm--now)
                    :ultrahuman (futon0-rhythm--ultrahuman-snapshot))))
    (setq futon0-rhythm--current-psr psr)
    (futon0-rhythm--save-record (list :date (futon0-rhythm--today)
                                       :psr psr))
    (message "Quick clock-in: %s | %s" frame (futon0-rhythm--format-stack stack))))

;;;###autoload
(defun futon0-rhythm-clock-out ()
  "Clock out, record actual weights, compute verdict."
  (interactive)
  (unless futon0-rhythm--current-psr
    ;; Try to load from file
    (let ((record (futon0-rhythm--load-record)))
      (when record
        (setq futon0-rhythm--current-psr (plist-get record :psr)))))
  (unless futon0-rhythm--current-psr
    (user-error "Not clocked in"))
  (let* ((psr futon0-rhythm--current-psr)
         (psr-stack (plist-get psr :stack))
         (pur-stack nil)
         (total-actual 0.0))
    ;; Collect actual weights
    (dolist (item psr-stack)
      (let* ((pattern (plist-get item :pattern))
             (bid-weight (plist-get item :weight))
             (actual (futon0-rhythm--read-weight pattern bid-weight))
             (delta (- actual bid-weight)))
        (push (list :pattern pattern
                    :weight actual
                    :delta delta)
              pur-stack)
        (setq total-actual (+ total-actual actual))))
    ;; Build PUR
    (let* ((pur (list :frame (plist-get psr :frame)
                      :stack (nreverse pur-stack)
                      :time (futon0-rhythm--now)
                      :ultrahuman (futon0-rhythm--ultrahuman-snapshot)
                      :git-commits (futon0-rhythm--git-activity-since
                                    (plist-get psr :time))))
           (verdict (futon0-rhythm--compute-verdict psr pur))
           (record (list :date (futon0-rhythm--today)
                         :psr psr
                         :pur pur
                         :verdict verdict)))
      ;; Save complete record
      (futon0-rhythm--save-record record)
      ;; Update history
      (push record futon0-rhythm--history)
      ;; Clear current PSR
      (setq futon0-rhythm--current-psr nil)
      ;; Display verdict
      (futon0-rhythm--display-verdict psr pur verdict))))

(defun futon0-rhythm--display-verdict (psr pur verdict)
  "Display PSR vs PUR comparison with VERDICT."
  (let ((buf (get-buffer-create "*Futon0 Rhythm Verdict*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Futon0 Rhythm - Daily Verdict\n")
      (insert "═══════════════════════════════════════════════════════\n\n")
      (insert (format "Date: %s\n" (futon0-rhythm--today)))
      (insert (format "Frame: %s\n\n" (plist-get psr :frame)))
      (insert "Pattern          Bid      Actual    Delta\n")
      (insert "───────────────────────────────────────────────────────\n")
      (let ((psr-stack (plist-get psr :stack))
            (pur-stack (plist-get pur :stack)))
        (cl-loop for pur-item in pur-stack
                 for psr-item in psr-stack
                 do (let* ((pattern (plist-get pur-item :pattern))
                           (bid (plist-get psr-item :weight))
                           (actual (plist-get pur-item :weight))
                           (delta (plist-get pur-item :delta))
                           (flag (cond
                                  ((> delta 0.15) " ← expanded")
                                  ((< delta -0.15) " ← squeezed")
                                  (t ""))))
                      (insert (format "%-16s %5.0f%%   %5.0f%%   %+5.0f%%%s\n"
                                      pattern
                                      (* 100 bid)
                                      (* 100 actual)
                                      (* 100 delta)
                                      flag)))))
      (insert "───────────────────────────────────────────────────────\n\n")
      ;; Ultrahuman deltas if available
      (let ((psr-uh (plist-get psr :ultrahuman))
            (pur-uh (plist-get pur :ultrahuman)))
        (when (and psr-uh pur-uh)
          (insert "Ultrahuman:\n")
          (when-let ((r1 (plist-get psr-uh :recovery))
                     (r2 (plist-get pur-uh :recovery)))
            (insert (format "  Recovery: %d → %d (Δ %+d)\n" r1 r2 (- r2 r1))))
          (when-let ((h1 (plist-get psr-uh :avg-sleep-hrv))
                     (h2 (plist-get pur-uh :avg-sleep-hrv)))
            (insert (format "  HRV: %d → %d (Δ %+d)\n" h1 h2 (- h2 h1))))
          (insert "\n")))
      ;; Git activity
      (when-let ((commits (plist-get pur :git-commits)))
        (insert (format "Git commits: %d\n\n" commits)))
      ;; Verdict
      (insert "───────────────────────────────────────────────────────\n")
      (insert (format "VERDICT: %s\n" verdict))
      (insert "───────────────────────────────────────────────────────\n"))
    (display-buffer buf)
    (message "Verdict: %s" verdict)))

;;;###autoload
(defun futon0-rhythm-status ()
  "Show current clock-in status."
  (interactive)
  (if futon0-rhythm--current-psr
      (let ((psr futon0-rhythm--current-psr))
        (message "Clocked in since %s: %s | %s"
                 (plist-get psr :time)
                 (plist-get psr :frame)
                 (futon0-rhythm--format-stack (plist-get psr :stack))))
    ;; Check file
    (if-let ((record (futon0-rhythm--load-record)))
        (if (plist-get record :pur)
            (message "Today complete. Verdict: %s" (plist-get record :verdict))
          (progn
            (setq futon0-rhythm--current-psr (plist-get record :psr))
            (message "Resumed from file: %s | %s"
                     (plist-get (plist-get record :psr) :frame)
                     (futon0-rhythm--format-stack
                      (plist-get (plist-get record :psr) :stack)))))
      (message "Not clocked in"))))

;;;###autoload
(defun futon0-rhythm-weekly-summary ()
  "Show summary of the past week's ensembles."
  (interactive)
  (let ((buf (get-buffer-create "*Futon0 Weekly Rhythm*"))
        (days nil))
    ;; Collect last 7 days
    (dotimes (i 7)
      (let* ((date (format-time-string "%Y-%m-%d"
                                       (time-subtract (current-time)
                                                      (days-to-time i))))
             (record (futon0-rhythm--load-record date)))
        (when record
          (push (cons date record) days))))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Futon0 Rhythm - Weekly Summary\n")
      (insert "═══════════════════════════════════════════════════════\n\n")
      (if (null days)
          (insert "No records found for the past week.\n")
        (dolist (day-record (reverse days))
          (let* ((date (car day-record))
                 (record (cdr day-record))
                 (psr (plist-get record :psr))
                 (pur (plist-get record :pur))
                 (verdict (plist-get record :verdict)))
            (insert (format "%s  %s  " date (plist-get psr :frame)))
            (if verdict
                (insert (format "%s\n" verdict))
              (insert "(incomplete)\n")))))
      (insert "\n")
      ;; Pattern drift analysis
      (let ((pattern-deltas (make-hash-table :test 'equal)))
        (dolist (day-record days)
          (when-let ((pur (plist-get (cdr day-record) :pur)))
            (dolist (item (plist-get pur :stack))
              (let* ((pattern (plist-get item :pattern))
                     (delta (or (plist-get item :delta) 0))
                     (current (gethash pattern pattern-deltas 0)))
                (puthash pattern (+ current delta) pattern-deltas)))))
        (insert "Pattern drift (cumulative):\n")
        (insert "───────────────────────────────────────────────────────\n")
        (maphash (lambda (pattern total-delta)
                   (let ((avg-delta (/ total-delta (float (length days)))))
                     (when (> (abs avg-delta) 0.05)
                       (insert (format "  %s: %+.0f%% avg drift%s\n"
                                       pattern
                                       (* 100 avg-delta)
                                       (if (> avg-delta 0) " (tends to expand)"
                                         " (tends to squeeze)"))))))
                 pattern-deltas)))
    (display-buffer buf)))

(provide 'futon0-rhythm)
;;; futon0-rhythm.el ends here
