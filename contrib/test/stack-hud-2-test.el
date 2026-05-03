;;; stack-hud-2-test.el --- ERT tests for stack-hud-2 -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'stack-hud-2)

(ert-deftest stack-hud-2-visit-at-point-selects-existing-window ()
  (let ((target (get-buffer-create "*stack-hud-2-target*"))
        (hud (generate-new-buffer " *stack-hud-2-test*"))
        (selected-frame nil)
        (selected-window nil)
        (switched-buffer nil)
        (win 'fake-window))
    (unwind-protect
        (cl-letf (((symbol-function 'stack-hud-2--window-showing-buffer)
                   (lambda (buffer)
                     (and (eq buffer target) win)))
                  ((symbol-function 'stack-hud-2--first-non-hud-window)
                   (lambda () nil))
                  ((symbol-function 'window-live-p)
                   (lambda (window) (eq window win)))
                  ((symbol-function 'window-frame)
                   (lambda (_window) 'fake-frame))
                  ((symbol-function 'window-buffer)
                   (lambda (_window) (get-buffer-create "*other*")))
                  ((symbol-function 'select-frame-set-input-focus)
                   (lambda (frame) (setq selected-frame frame)))
                  ((symbol-function 'select-window)
                   (lambda (window) (setq selected-window window)))
                  ((symbol-function 'switch-to-buffer)
                   (lambda (buffer &rest _args) (setq switched-buffer buffer))))
          (with-current-buffer hud
            (insert "target")
            (add-text-properties (point-min) (point-max)
                                 (list 'stack-hud-target-buffer target))
            (goto-char (point-min))
            (stack-hud-2-visit-at-point)
            (should (eq selected-frame 'fake-frame))
            (should (eq selected-window win))
            (should (eq switched-buffer target)))))
      (when (buffer-live-p hud)
        (kill-buffer hud))
      (when (buffer-live-p target)
        (kill-buffer target))))

(ert-deftest stack-hud-2-visit-at-point-errors-without-target ()
  (with-temp-buffer
    (should-error (stack-hud-2-visit-at-point) :type 'user-error)))
