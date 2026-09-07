;;; stack-hud-apm-report.el --- Minimal APM visibility, no HUD render -*- lexical-binding: t; -*-

;;; Commentary:
;; `stack-hud-1' builds state AND renders it into a HUD window.  When the
;; render half misbehaves (window/frame handling differs between GUI, TTY and
;; daemon Emacs), the numbers become unreachable even though collecting them
;; works fine.  This command is the collector half on its own: no HUD buffer,
;; no window selection, no frame raising, no network.
;;
;; Measured 2026-09-06: the scan is ~4s cold over 491 problems and is cached
;; for `stack-hud-apm-cache-seconds'.  Pass a prefix argument to force a
;; refresh.

;;; Code:

(require 'stack-hud)

;;;###autoload
(defun stack-hud-apm-report (&optional refresh)
  "Show APM proof-recovery counts in a buffer.
With prefix arg REFRESH, bypass the cache and rescan."
  (interactive "P")
  (when refresh
    (setq stack-hud--apm-cache nil
          stack-hud--apm-cache-time 0))
  (let* ((s (stack-hud--apm-scan))
         (total (plist-get s :total))
         (informal (plist-get s :informal))
         (lean-total (plist-get s :lean-total))
         (with-sorry (plist-get s :lean-with-sorry))
         (clean (plist-get s :lean-clean))
         (sorries (plist-get s :sorries))
         (buf (get-buffer-create "*APM report*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "APM report  %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
        (insert (format "  problems (source .tex)      %4d\n" total))
        (insert (format "  with informal solution      %4d   (missing %d)\n"
                        informal (- total informal)))
        (insert (format "  with Lean material          %4d\n" lean-total))
        (insert (format "  SOLVED (Lean, zero sorry)   %4d\n" clean))
        (insert (format "  open   (Lean, has sorry)    %4d\n" with-sorry))
        (insert (format "  total executable sorries    %4d\n" sorries))
        (insert (format "\n  bar %s %d/%d\n"
                        (stack-hud--apm-bar clean lean-total 24) clean lean-total))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)
    (message "APM: %d solved / %d with Lean; %d sorries outstanding"
             clean lean-total sorries)))

(provide 'stack-hud-apm-report)

;;; stack-hud-apm-report.el ends here
