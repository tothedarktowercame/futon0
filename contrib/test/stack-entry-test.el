;;; stack-entry-test.el --- ERT tests for stack-entry -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'stack-entry)

(ert-deftest stack-entry-stack-hud-logs-before-opening-hud-2 ()
  (let ((calls '())
        (stack '(:ok t)))
    (cl-letf (((symbol-function 'stack-hud--build-state)
               (lambda ()
                 (push 'build calls)
                 stack))
              ((symbol-function 'stack-hud-log-snapshot)
               (lambda (arg)
                 (push (list 'log arg) calls)))
              ((symbol-function 'stack-hud-2-toggle)
               (lambda ()
                 (push 'toggle calls))))
      (stack-hud)
      (should (equal (nreverse calls)
                     (list 'build
                           (list 'log stack)
                           'toggle))))))

(ert-deftest stack-entry-stack-hud-1-renders-legacy-hud ()
  (let ((calls '())
        (stack '(:legacy t)))
    (cl-letf (((symbol-function 'stack-hud--build-state)
               (lambda ()
                 (push 'build calls)
                 stack))
              ((symbol-function 'stack-hud--render-context)
               (lambda (arg)
                 (push (list 'render arg) calls)))
              ((symbol-function 'stack-hud-log-snapshot)
               (lambda (arg)
                 (push (list 'log arg) calls)))
              ((symbol-function 'get-buffer-window)
               (lambda (&rest _args) nil)))
      (stack-hud-1)
      (should (equal (nreverse calls)
                     (list 'build
                           (list 'render stack)
                           (list 'log stack)))))))
