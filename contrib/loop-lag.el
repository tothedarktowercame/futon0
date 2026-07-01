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
  (when loop-lag--cmd-start
    (let ((dur (* 1000 (float-time (time-subtract (current-time)
                                                  loop-lag--cmd-start)))))
      (setq loop-lag--last-command
            (list :command this-command :ended (current-time) :dur-ms dur)))))

(defun loop-lag--attribution (stall-start)
  "Best-effort attribution for a stall that began around STALL-START.
If a command ran long and ended within the stall window, name it; otherwise
the block came from a timer / process-filter / sentinel / synchronous I/O,
none of which run through the command loop."
  (let ((lc loop-lag--last-command))
    (if (and lc
             (>= (plist-get lc :dur-ms) loop-lag-threshold-ms)
             (time-less-p stall-start (plist-get lc :ended)))
        (format "command %s (%.0fms)"
                (plist-get lc :command) (plist-get lc :dur-ms))
      "non-command (timer / process-filter / sentinel / sync I/O)")))

(defun loop-lag--tick ()
  (let* ((now (current-time))
         (gap (float-time (time-subtract now (or loop-lag--last now))))
         (lag-ms (max 0.0 (* 1000 (- gap loop-lag-interval)))))
    (setq loop-lag--last now)
    (push (cons now lag-ms) loop-lag--history)
    (loop-lag--trim loop-lag--history loop-lag-history-max)
    (when (>= lag-ms loop-lag-threshold-ms)
      (let ((stall-start (time-subtract now (seconds-to-time (/ lag-ms 1000.0)))))
        (push (list :at now :lag-ms lag-ms
                    :attribution (loop-lag--attribution stall-start))
              loop-lag--stalls)
        (loop-lag--trim loop-lag--stalls loop-lag-stalls-max)))
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
        (setq loop-lag--last (current-time))
        (add-hook 'pre-command-hook #'loop-lag--pre-command)
        (add-hook 'post-command-hook #'loop-lag--post-command)
        (unless (member loop-lag--indicator global-mode-string)
          (setq global-mode-string
                (append (or global-mode-string '("")) (list loop-lag--indicator))))
        (when (timerp loop-lag--timer) (cancel-timer loop-lag--timer))
        (setq loop-lag--timer
              (run-with-timer loop-lag-interval loop-lag-interval #'loop-lag--tick)))
    (when (timerp loop-lag--timer) (cancel-timer loop-lag--timer))
    (setq loop-lag--timer nil)
    (remove-hook 'pre-command-hook #'loop-lag--pre-command)
    (remove-hook 'post-command-hook #'loop-lag--post-command)
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
        (insert (format "samples=%d   stalls(>=%sms)=%d   peak=%.0fms   monitor=%s\n\n"
                        (length lags) loop-lag-threshold-ms (length stalls)
                        (if lags (apply #'max lags) 0)
                        (if (timerp loop-lag--timer) "on" "off")))
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
