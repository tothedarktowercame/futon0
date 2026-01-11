;;; stack-render.el --- Stack HUD rendering helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone rendering logic for the Stack HUD, split out of stack-doc.

;;; Code:

(require 'futon-hud-windows)
(require 'hud-service)
(require 'stack-doc)
(require 'stack-hud)

(defun stack-hud--render-context (stack)
  "Render STACK into the Stack HUD buffer and window."
  (if stack
      (let ((buf (stack-doc--stack-buffer t)))
        (futon-hud-service-render
         buf
         (lambda (_buf win)
           (let ((text (my-chatgpt-shell--stack-hud-string stack)))
             (insert (my-chatgpt-shell--stack-columnize text win))
             (setq-local stack-doc--stack-last-state stack)
             (stack-doc--stack-apply-doc-properties)))
         '(:mode keep :truncate-lines t :use-stack-window t)))
    (my-chatgpt-shell--delete-stack-window)))

(provide 'stack-render)

;;; stack-render.el ends here
