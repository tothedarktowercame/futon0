;;; futon-hud-windows.el --- HUD window/frame helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared window/frame management for Tatami and Stack HUDs.

;;; Code:

(require 'seq)

(defvar my-chatgpt-shell--context-window nil
  "Live window showing the Tatami Context HUD (side window). Only one exists.")
(defvar my-chatgpt-shell--stack-window nil
  "Live window showing the Stack Context HUD next to the main Tatami HUD.")
(defvar my-chatgpt-shell--stack-frame nil
  "Frame hosting the Stack HUD when rendered in its own frame.")

(defcustom stack-hud-context-font-scale 1.0
  "Relative font scale for the Stack/Tatami HUD buffers.
Set to nil or 1.0 to leave the default face size unchanged."
  :type 'number
  :group 'tatami-integration)

(defvar-local stack-hud--context-face-cookie nil)
(defvar-local stack-hud--context-face-scale nil)

(defun my-chatgpt-shell--stack-window-owned-p (&optional win)
  (let ((candidate (or win my-chatgpt-shell--stack-window)))
    (and (window-live-p candidate)
         (window-parameter candidate 'tatami-stack-owner))))

(defun my-chatgpt-shell--delete-tatami-window ()
  (dolist (win (get-buffer-window-list my-chatgpt-shell-context-buffer-name nil t))
    (when (window-live-p win)
      (set-window-parameter win 'tatami-context-owner nil)
      (delete-window win)))
  (setq my-chatgpt-shell--context-window nil))

(defun my-chatgpt-shell--delete-stack-window ()
  (when (my-chatgpt-shell--stack-window-owned-p)
    (set-window-parameter my-chatgpt-shell--stack-window 'tatami-context-owner nil)
    (set-window-parameter my-chatgpt-shell--stack-window 'tatami-stack-owner nil)
    (set-window-dedicated-p my-chatgpt-shell--stack-window nil)
    (if (one-window-p t)
        (set-window-buffer my-chatgpt-shell--stack-window (get-buffer-create "*scratch*"))
      (delete-window my-chatgpt-shell--stack-window)))
  (setq my-chatgpt-shell--stack-window nil))

(defun my-chatgpt-shell--stack-frame ()
  (let ((existing (or (and (frame-live-p my-chatgpt-shell--stack-frame)
                           my-chatgpt-shell--stack-frame)
                      (seq-find (lambda (frame)
                                  (and (frame-live-p frame)
                                       (equal (frame-parameter frame 'name)
                                              my-chatgpt-shell-stack-frame-name)))
                                (frame-list)))))
    (or existing
        (let ((frame (make-frame `((name . ,my-chatgpt-shell-stack-frame-name)))))
          (set-frame-parameter frame 'tatami-stack-frame t)
          (when my-chatgpt-shell-stack-frame-fullscreen
            (set-frame-parameter frame 'fullscreen 'fullboth))
          (setq my-chatgpt-shell--stack-frame frame)
          frame))))

(defun my-chatgpt-shell--delete-context-windows ()
  "Close every live Tatami/Stack HUD window and reset state."
  (my-chatgpt-shell--delete-tatami-window)
  (my-chatgpt-shell--delete-stack-window))

(defun my-chatgpt-shell--prepare-context-buffer (buf)
  (with-current-buffer buf
    (when stack-hud--context-face-cookie
      (face-remap-remove-relative stack-hud--context-face-cookie)
      (setq stack-hud--context-face-cookie nil
            stack-hud--context-face-scale nil))
    (when (and stack-hud-context-font-scale
               (not (equal stack-hud-context-font-scale 1.0)))
      (setq stack-hud--context-face-cookie
            (face-remap-add-relative 'default `(:height ,stack-hud-context-font-scale)))
      (setq stack-hud--context-face-scale stack-hud-context-font-scale))
    (visual-line-mode 1)
    (setq-local truncate-lines nil)))

(defun my-chatgpt-shell--ensure-context-window (buf)
  "Display BUF in the dedicated Tatami HUD side window near the active frame."
  (my-chatgpt-shell--prepare-context-buffer buf)
  (if (and (window-live-p my-chatgpt-shell--context-window)
           (eq (window-buffer my-chatgpt-shell--context-window) buf))
      my-chatgpt-shell--context-window
    (let ((frame (window-frame (selected-window))))
      (my-chatgpt-shell--delete-tatami-window)
      (let ((win (with-selected-frame frame
                   (display-buffer-in-side-window buf '((side . right)
                                                        (slot . 0)
                                                        (window-width . 0.4))))))
        (when (window-live-p win)
          (set-window-dedicated-p win t)
          (set-window-parameter win 'tatami-context-owner (current-buffer))
          (setq my-chatgpt-shell--context-window win))))))

(defun my-chatgpt-shell--ensure-stack-window (buf)
  "Display BUF in the dedicated Stack HUD frame."
  (my-chatgpt-shell--prepare-context-buffer buf)
  (let* ((frame (my-chatgpt-shell--stack-frame))
         (win (frame-selected-window frame)))
    (when (window-live-p win)
      (set-window-buffer win buf)
      (set-window-dedicated-p win t)
      (set-window-parameter win 'tatami-context-owner (current-buffer))
      (set-window-parameter win 'tatami-stack-owner t)
      (setq my-chatgpt-shell--stack-window win))
    win))

(provide 'futon-hud-windows)

;;; futon-hud-windows.el ends here
