;;; loop-lag.el --- Event-loop lag indicator for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs runs Lisp on a single thread, so WHILE the main thread is blocked it
;; cannot run a timer to observe itself.  `loop-lag-mode' installs a repeating
;; heartbeat timer that, on each fire, measures the gap since its *last actual*
;; fire: when the loop was blocked the timer fired late, and (gap - interval) is
;; how long the main thread stalled.
;;
;; This is therefore a RETROSPECTIVE indicator — it sees a stall only once the
;; block ends.  For real-time "frozen right now" detection you need an external
;; watchdog process pinging Emacs (that is deliberately out of scope here; this
;; is mechanism #1 of two — see the discussion around E-agency-ws-cutover).
;;
;; Provides: a mode-line light (green / amber / red over a trailing window), a
;; rolling sample history, a stall log with best-effort attribution to the
;; command that ran just before the stall, and `loop-lag-report'.

;;; Code:

(require 'cl-lib)

(defgroup loop-lag nil
  "Event-loop lag indicator."
  :group 'convenience)

(defcustom loop-lag-interval 0.25
  "Heartbeat period, in seconds."
  :type 'number)

(defcustom loop-lag-threshold-ms 200
  "Lag at or above this many ms counts as a stall."
  :type 'number)

(defcustom loop-lag-window 8.0
  "The mode-line light reflects peak lag over this many trailing seconds."
  :type 'number)

(defcustom loop-lag-history-max 4000
  "Maximum number of heartbeat samples retained."
  :type 'integer)

(defcustom loop-lag-stalls-max 500
  "Maximum number of stall records retained."
  :type 'integer)

(defcustom loop-lag-instrument-syncio t
  "When non-nil, `loop-lag-mode' instruments known synchronous entry points
so a stall can be attributed to the exact blocking call rather than the generic
\"non-command\" bucket.  See `loop-lag--install-instrumentation'."
  :type 'boolean)

(defcustom loop-lag-blocker-min-ms 150
  "An instrumented call is recorded as a candidate blocker only if it ran at
least this many ms (keeps fast calls out of the attribution slot)."
  :type 'number)

(defface loop-lag-ok-face '((t :foreground "green3"))
  "Face for the calm (no recent stall) indicator.")
(defface loop-lag-warn-face '((t :foreground "orange"))
  "Face for mild recent lag.")
(defface loop-lag-stall-face '((t :foreground "red" :weight bold))
  "Face for a recent hard stall.")

(defvar loop-lag--timer nil)
(defvar loop-lag--last nil "Time of the last heartbeat tick.")
(defvar loop-lag--history nil "List of (TIME . LAG-MS), newest first.")
(defvar loop-lag--stalls nil "List of stall plists, newest first.")
(defvar loop-lag--last-command nil "Plist describing the most recent command.")
(defvar loop-lag--cmd-start nil "Start time of the in-flight command.")
(defvar loop-lag--last-blocker nil
  "Plist describing the most recent instrumented synchronous call that ran long:
\(:what LABEL :detail STRING :ended TIME :dur-ms N).")
(defvar loop-lag--instrumented nil "List of (SYMBOL . ADVICE-NAME) currently advised.")
(defvar loop-lag--last-gc-elapsed 0.0 "`gc-elapsed' at the previous tick.")
(defvar loop-lag--last-gcs-done 0 "`gcs-done' at the previous tick.")
(defvar loop-lag--blocker-sum 0.0 "Summed instrumented-blocker ms since last tick.")
(defvar loop-lag--blocker-count 0 "Count of instrumented-blocker calls since last tick.")
(defvar loop-lag--blocker-labels nil "Alist label -> summed ms since last tick.")

(defvar loop-lag--indicator '(:eval (loop-lag--mode-line))
  "The `global-mode-string' construct this mode installs.")

(defun loop-lag--trim (list n)
  "Destructively cap LIST at N elements; return LIST."
  (when (> (length list) n)
    (setcdr (nthcdr (1- n) list) nil))
  list)

(defun loop-lag--pre-command ()
  (setq loop-lag--cmd-start (current-time)))

(defun loop-lag--post-command ()
  ;; Only record real commands; a nil `this-command' bracket spans idle/read
  ;; periods and yields bogus multi-minute "command nil" durations.
  (when (and loop-lag--cmd-start this-command)
    (let ((dur (* 1000 (float-time (time-subtract (current-time)
                                                  loop-lag--cmd-start)))))
      (setq loop-lag--last-command
            (list :command this-command :ended (current-time) :dur-ms dur)))))

(defun loop-lag--fmt-blocker (b)
  (let ((detail (plist-get b :detail)))
    (format "%s%s (%.0fms)"
            (plist-get b :what)
            (if (and detail (not (string-empty-p detail))) (concat " " detail) "")
            (plist-get b :dur-ms))))

(defun loop-lag--in-window-p (plist stall-start)
  "Non-nil if PLIST's :ended falls at/after STALL-START.
That means the recorded call was still running during the stall."
  (and plist (time-less-p stall-start (plist-get plist :ended))))

(defun loop-lag--attribution (stall-start gc-ms gc-n bsum bcount blabels)
  "Best-effort attribution for a stall that began around STALL-START.
GC-MS/GC-N are GC time+count in the stall; BSUM/BCOUNT/BLABELS are the summed
ms, count, and per-label breakdown of instrumented sync calls in the stall.
Attributes to the largest known contributor: stacked sync calls (BSUM), GC, or a
real command; else the generic non-command bucket (uninstrumented tight Lisp
loop / process-filter / non-yielding op)."
  (let* ((lc (and (loop-lag--in-window-p loop-lag--last-command stall-start)
                  (plist-get loop-lag--last-command :command)
                  (>= (plist-get loop-lag--last-command :dur-ms) loop-lag-threshold-ms)
                  loop-lag--last-command))
         (lc-ms (if lc (plist-get lc :dur-ms) 0))
         (best (max bsum gc-ms lc-ms)))
    (cond
     ((< best loop-lag-threshold-ms)
      (if (> gc-ms 0)
          (format "non-command (+%.0fms GC in %d) — uninstrumented" gc-ms gc-n)
        "non-command — uninstrumented (tight Lisp loop / process-filter / non-yielding op)"))
     ((and (= best gc-ms) (> gc-ms 0))
      (format "GC (%.0fms in %d collection%s)" gc-ms gc-n (if (= gc-n 1) "" "s")))
     ((= best bsum)
      (let ((brk (mapconcat (lambda (c) (format "%s×%.0fms" (car c) (cdr c)))
                            (seq-take (cl-sort (copy-sequence blabels) #'> :key #'cdr) 3)
                            "  ")))
        (format "%d sync call%s (sum %.0fms): %s"
                bcount (if (= bcount 1) "" "s") bsum brk)))
     (t (format "command %s (%.0fms)" (plist-get lc :command) lc-ms)))))

;;; Instrumentation of synchronous entry points -------------------------------

(defun loop-lag-instrument (fn &optional label detail-fn)
  "Advise FN (a symbol) to record itself as the last blocker when a call runs
>= `loop-lag-blocker-min-ms'.  LABEL defaults to FN's name; DETAIL-FN, if given,
is applied to the call's arg list to produce a short detail string (guarded)."
  (when (and (fboundp fn) (not (assq fn loop-lag--instrumented)))
    (let* ((label (or label (symbol-name fn)))
           (advice-name (intern (concat "loop-lag-instr:" label)))
           (advice
            (lambda (orig &rest args)
              (let ((start (current-time)))
                (unwind-protect (apply orig args)
                  (let ((dur (* 1000 (float-time (time-subtract (current-time) start)))))
                    (when (>= dur loop-lag-blocker-min-ms)
                      (setq loop-lag--last-blocker
                            (list :what label
                                  :detail (and detail-fn
                                               (ignore-errors (funcall detail-fn args)))
                                  :ended (current-time) :dur-ms dur))
                      ;; Accumulate for per-stall blocker-sum: distinguishes one big
                      ;; sync call from many stacked ones summing to the stall.
                      (setq loop-lag--blocker-sum (+ loop-lag--blocker-sum dur)
                            loop-lag--blocker-count (1+ loop-lag--blocker-count))
                      (let ((cell (assoc label loop-lag--blocker-labels)))
                        (if cell (setcdr cell (+ (cdr cell) dur))
                          (push (cons label dur) loop-lag--blocker-labels))))))))))
      (advice-add fn :around advice (list (cons 'name advice-name)))
      (push (cons fn advice-name) loop-lag--instrumented))))

(defun loop-lag--remove-instrumentation ()
  (dolist (pair loop-lag--instrumented)
    (when (fboundp (car pair))
      (advice-remove (car pair) (cdr pair))))
  (setq loop-lag--instrumented nil))

(defun loop-lag--truncate (s &optional n)
  (let ((s (format "%s" s)) (n (or n 90)))
    (if (> (length s) n) (concat (substring s 0 n) "…") s)))

(defun loop-lag--url-detail (args)
  "Detail for `url-retrieve-synchronously': the target URL (first arg)."
  (let ((u (car args)))
    (loop-lag--truncate (if (stringp u) u (ignore-errors (url-recreate-url u))))))

(defun loop-lag--call-process-detail (args)
  "Detail for `call-process': PROGRAM plus its argv (URLs surface for curl)."
  ;; (PROGRAM &optional INFILE DESTINATION DISPLAY &rest PROGRAM-ARGS)
  (let ((program (car args))
        (pargs (nthcdr 4 args)))
    (loop-lag--truncate (string-join (cons (format "%s" program)
                                           (mapcar (lambda (a) (format "%s" a)) pargs))
                                     " "))))

(defun loop-lag--install-instrumentation ()
  "Instrument the known synchronous entry points (hard blockers first)."
  ;; call-process is a HARD block (no timer yielding) — the likeliest source of
  ;; multi-second stalls via synchronous curl.  url-retrieve-synchronously runs
  ;; a nested loop that DOES yield to timers, so it stalls less, but instrument
  ;; it too for completeness.
  (loop-lag-instrument 'call-process "call-process" #'loop-lag--call-process-detail)
  (loop-lag-instrument 'url-retrieve-synchronously
                       "url-retrieve-synchronously" #'loop-lag--url-detail))

(defun loop-lag--tick ()
  (let* ((now (current-time))
         (gap (float-time (time-subtract now (or loop-lag--last now))))
         (lag-ms (max 0.0 (* 1000 (- gap loop-lag-interval))))
         (gc-ms (max 0.0 (* 1000 (- gc-elapsed loop-lag--last-gc-elapsed))))
         (gc-n (max 0 (- gcs-done loop-lag--last-gcs-done))))
    (let ((bsum loop-lag--blocker-sum)
          (bcount loop-lag--blocker-count)
          (blabels loop-lag--blocker-labels))
      (setq loop-lag--last now
            loop-lag--last-gc-elapsed gc-elapsed
            loop-lag--last-gcs-done gcs-done
            loop-lag--blocker-sum 0.0
            loop-lag--blocker-count 0
            loop-lag--blocker-labels nil)
      (push (cons now lag-ms) loop-lag--history)
      (loop-lag--trim loop-lag--history loop-lag-history-max)
      (when (>= lag-ms loop-lag-threshold-ms)
        (let ((stall-start (time-subtract now (seconds-to-time (/ lag-ms 1000.0)))))
          (push (list :at now :lag-ms lag-ms :gc-ms gc-ms :gc-count gc-n
                      :blocker-ms bsum :blocker-count bcount
                      :blocker-labels blabels
                      :attribution (loop-lag--attribution stall-start gc-ms gc-n
                                                          bsum bcount blabels))
                loop-lag--stalls)
          (loop-lag--trim loop-lag--stalls loop-lag-stalls-max))))
    (force-mode-line-update t)))

(defun loop-lag--recent-peak ()
  "Peak lag (ms) over the trailing `loop-lag-window' seconds."
  (let ((cutoff (time-subtract (current-time) (seconds-to-time loop-lag-window)))
        (peak 0.0))
    (cl-loop for (tm . lag) in loop-lag--history
             while (time-less-p cutoff tm)
             do (when (> lag peak) (setq peak lag)))
    peak))

(defun loop-lag--mode-line ()
  "Mode-line construct: a coloured light + recent peak lag when notable."
  (let* ((peak (loop-lag--recent-peak))
         (face (cond ((>= peak (* 3 loop-lag-threshold-ms)) 'loop-lag-stall-face)
                     ((>= peak loop-lag-threshold-ms) 'loop-lag-warn-face)
                     (t 'loop-lag-ok-face)))
         (txt (if (>= peak loop-lag-threshold-ms)
                  (format "⏱%.0fms" peak)
                "⏱")))
    (propertize
     txt 'face face
     'help-echo (format "loop-lag: peak %.0fms over %ss (stall threshold %sms).  M-x loop-lag-report"
                        peak loop-lag-window loop-lag-threshold-ms))))

;;;###autoload
(define-minor-mode loop-lag-mode
  "Global minor mode indicating Emacs event-loop lag over time.
Installs a heartbeat timer and a mode-line light; `loop-lag-report' shows the
history and attributed stalls."
  :global t :lighter nil
  (if loop-lag-mode
      (progn
        (setq loop-lag--last (current-time)
              loop-lag--last-gc-elapsed gc-elapsed
              loop-lag--last-gcs-done gcs-done)
        (add-hook 'pre-command-hook #'loop-lag--pre-command)
        (add-hook 'post-command-hook #'loop-lag--post-command)
        (unless (member loop-lag--indicator global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '("")) (list loop-lag--indicator))))
        (when loop-lag-instrument-syncio (loop-lag--install-instrumentation))
        (when (timerp loop-lag--timer) (cancel-timer loop-lag--timer))
        (setq loop-lag--timer
              (run-with-timer loop-lag-interval loop-lag-interval #'loop-lag--tick)))
    (when (timerp loop-lag--timer) (cancel-timer loop-lag--timer))
    (setq loop-lag--timer nil)
    (remove-hook 'pre-command-hook #'loop-lag--pre-command)
    (remove-hook 'post-command-hook #'loop-lag--post-command)
    (loop-lag--remove-instrumentation)
    (setq global-mode-string (delete loop-lag--indicator global-mode-string))
    (force-mode-line-update t)))

(defun loop-lag--bucket-counts (lags)
  "Return an alist of (LABEL . COUNT) over LAGS (ms)."
  (let ((edges '((0 . 50) (50 . 100) (100 . 200) (200 . 500)
                 (500 . 1000) (1000 . nil))))
    (mapcar
     (lambda (e)
       (let* ((lo (car e)) (hi (cdr e))
              (label (if hi (format "%4d-%-4d ms" lo hi) (format ">= %d ms" lo))))
         (cons label
               (cl-count-if (lambda (l) (and (>= l lo) (or (null hi) (< l hi))))
                            lags))))
     edges)))

(defun loop-lag-report ()
  "Show a report of recent event-loop lag and attributed stalls."
  (interactive)
  (let ((buf (get-buffer-create "*loop-lag*"))
        (lags (mapcar #'cdr loop-lag--history))
        (stalls loop-lag--stalls))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "loop-lag report   interval=%ss  threshold=%sms  window=%ss\n"
                        loop-lag-interval loop-lag-threshold-ms loop-lag-window))
        (insert (format "samples=%d   stalls(>=%sms)=%d   peak=%.0fms   monitor=%s\n"
                        (length lags) loop-lag-threshold-ms (length stalls)
                        (if lags (apply #'max lags) 0)
                        (if (timerp loop-lag--timer) "on" "off")))
        (insert (format "GC: %d collections, %.0fs total, avg %.0fms, threshold %.0fMB\n\n"
                        gcs-done gc-elapsed
                        (if (> gcs-done 0) (/ (* 1000.0 gc-elapsed) gcs-done) 0)
                        (/ gc-cons-threshold 1048576.0)))
        (insert "lag distribution:\n")
        (dolist (b (loop-lag--bucket-counts lags))
          (insert (format "  %-14s : %d\n" (car b) (cdr b))))
        (insert (format "\nworst %d stalls (attributed):\n"
                        (min 20 (length stalls))))
        (if (null stalls)
            (insert "  (none yet)\n")
          (cl-loop for s in (cl-sort (copy-sequence stalls) #'> :key
                                     (lambda (s) (plist-get s :lag-ms)))
                   for i from 1 to 20
                   do (insert (format "  %s  %7.0fms  %s\n"
                                      (format-time-string "%H:%M:%S" (plist-get s :at))
                                      (plist-get s :lag-ms)
                                      (plist-get s :attribution)))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(provide 'loop-lag)
;;; loop-lag.el ends here
