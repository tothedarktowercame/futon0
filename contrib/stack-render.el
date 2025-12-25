;;; stack-render.el --- Stack HUD rendering helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone rendering logic for the Stack HUD, split out of stack-doc.

;;; Code:

(require 'futon-hud-windows)
(require 'stack-doc)
(require 'stack-hud)

(defun stack-hud--render-context (stack)
  "Render STACK into the Stack HUD buffer and window."
  (if stack
      (let ((buf (stack-doc--stack-buffer t)))
        (let ((win (my-chatgpt-shell--ensure-stack-window buf))
              (text (my-chatgpt-shell--stack-hud-string stack)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (unless (derived-mode-p 'special-mode)
                (special-mode))
              (setq-local truncate-lines t)
              (setq-local stack-doc--stack-last-state stack)
              (insert (my-chatgpt-shell--stack-columnize text win))
              (stack-doc--stack-apply-doc-properties)
              (goto-char (point-min))))))
    (my-chatgpt-shell--delete-stack-window)))

(provide 'stack-render)

;;; stack-render.el ends here
