;;; stack-entry.el --- Stack HUD entry point -*- lexical-binding: t; -*-

;;; Commentary:
;; Standalone entry point for rendering the Stack HUD outside chatgpt-shell.

;;; Code:

(require 'stack-render)

(defconst futon0--futon3-bridge-path
  "/home/joe/code/futon3/contrib/futon3-bridge.el"
  "Path to the Futon3 bridge helpers used to fetch Stack HUD status.")

(defun stack-hud ()
  "Fetch Stack HUD state from Futon3 and render the Stack HUD."
  (interactive)
  (unless (featurep 'futon3-bridge)
    (if (file-readable-p futon0--futon3-bridge-path)
        (load-file futon0--futon3-bridge-path)
      (user-error "Futon3 bridge not found at %s" futon0--futon3-bridge-path)))
  (my-futon3-ensure-running)
  (let* ((status (or (my-futon3-refresh-status) my-futon3-last-status))
         (stack (and status (plist-get status :stack))))
    (if stack
        (progn
          (stack-hud--render-context stack)
          (when-let ((win (get-buffer-window my-chatgpt-shell-stack-buffer-name t)))
            (select-window win)
            (raise-frame (window-frame win))))
      (message "No Stack HUD data returned from Futon3."))))

(provide 'stack-entry)

;;; stack-entry.el ends here
