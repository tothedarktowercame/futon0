;;; hud-service.el --- Shared HUD rendering helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared buffer/window helpers for Stack-style HUDs.

;;; Code:

(require 'seq)
(require 'futon-hud-windows)

(defgroup futon-hud-service nil
  "Shared helpers for Stack-style HUD buffers."
  :group 'tatami-integration)

(defcustom futon-hud-service-default-truncate-lines t
  "Default truncation setting for HUD buffers."
  :type 'boolean
  :group 'futon-hud-service)

(defcustom futon-hud-service-default-frame-fullscreen t
  "When non-nil, HUD frames created by the service are fullscreen."
  :type 'boolean
  :group 'futon-hud-service)

(defvar futon-hud-service--frames nil)

(defun futon-hud-service--frame (name &optional fullscreen)
  (let* ((existing (seq-find (lambda (frame)
                               (and (frame-live-p frame)
                                    (equal (frame-parameter frame 'name) name)))
                             (frame-list)))
         (frame (or existing (make-frame `((name . ,name))))))
    (set-frame-parameter frame 'futon-hud-frame t)
    (when (and (or fullscreen futon-hud-service-default-frame-fullscreen)
               (not (frame-parameter frame 'fullscreen)))
      (set-frame-parameter frame 'fullscreen 'fullboth))
    frame))

(defun futon-hud-service-render (buffer render-fn &optional options)
  "Render BUFFER using RENDER-FN in a Stack-style HUD window.

BUFFER may be a buffer or buffer name. RENDER-FN is called with (buffer window)
and should insert the desired contents.

OPTIONS is a plist supporting:
  :mode           symbol or function to enable (use 'keep to preserve mode)
  :truncate-lines override `futon-hud-service-default-truncate-lines`
  :use-stack-window when non-nil, reuse the Stack HUD window
  :frame-name     name for a dedicated HUD frame
  :fullscreen     when non-nil, fullscreen the HUD frame
  :side           display buffer in side window (e.g. 'right)
  :window-width   width for side window."
  (let* ((buf (if (bufferp buffer)
                  buffer
                (get-buffer-create buffer)))
         (win (cond
               ((plist-get options :use-stack-window)
                (my-chatgpt-shell--ensure-stack-window buf))
               ((plist-get options :side)
                (let* ((side (plist-get options :side))
                       (width (plist-get options :window-width))
                       (win (display-buffer-in-side-window
                             buf
                             (append `((side . ,side) (slot . 0))
                                     (when width `((window-width . ,width)))))))
                  (when (window-live-p win)
                    (set-window-dedicated-p win t)
                    (set-window-parameter win 'futon-hud-owner t))
                  win))
               (t
                (let* ((frame (futon-hud-service--frame
                               (or (plist-get options :frame-name) "HUD")
                               (plist-get options :fullscreen)))
                       (win (frame-selected-window frame)))
                  (when (window-live-p win)
                    (set-window-buffer win buf)
                    (set-window-dedicated-p win t)
                    (set-window-parameter win 'futon-hud-owner t))
                  win))))
         (mode (plist-get options :mode))
         (truncate (if (plist-member options :truncate-lines)
                       (plist-get options :truncate-lines)
                     futon-hud-service-default-truncate-lines)))
    (when (window-live-p win)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (cond
           ((eq mode 'keep) nil)
           (mode (funcall mode))
           ((not (derived-mode-p 'special-mode)) (special-mode)))
          (setq-local truncate-lines truncate)
          (funcall render-fn buf win)
          (goto-char (point-min)))))
    buf))

(provide 'hud-service)

;;; hud-service.el ends here
