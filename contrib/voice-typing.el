;;; voice-typing.el --- Stack voice typing controls -*- lexical-binding: t; -*-

;;; Commentary:
;; This file was split out of aob-chatgpt.el to isolate Stack voice typing
;; controls and ydotoold helpers from the chat/HUD core.

;;; Code:

(require 'subr-x)

(defcustom my-chatgpt-shell-voice-command "/home/joe/opt/voice-typing-linux/voice --layout dvorak --enter-keyword rocket"
  "Command used to start the Futon voice typing interface.
Value can be a string (executed via the user shell) or a list of program
and arguments passed directly to `start-process'."
  :type '(choice string (repeat string))
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-voice-buffer-name "*Stack Voice Typing*"
  "Buffer used to capture stdout/stderr from the voice typing process."
  :type 'string
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-voice-socket-path
  (format "/run/user/%d/ydotoold/socket" (user-uid))
  "Socket path shared with ydotool/ydotoold."
  :type 'string
  :group 'tatami-integration)
(defcustom my-chatgpt-shell-ydotoold-command '("/home/joe/.local/bin/ydotoold")
  "Command used to launch ydotoold when Stack voice typing starts.
Value can be a string (shell command) or a list passed to `start-process'."
  :type '(choice string (repeat string))
  :group 'tatami-integration)

(defvar my-chatgpt-shell--voice-process nil)
(defvar my-chatgpt-shell--ydotoold-process nil)

(defun my-chatgpt-shell--voice-command-list ()
  (let ((cmd my-chatgpt-shell-voice-command))
    (cond
     ((and (stringp cmd) (not (string-empty-p cmd)))
      (list shell-file-name shell-command-switch cmd))
     ((and (consp cmd) (stringp (car cmd)))
      cmd)
     (t
      (user-error "Stack voice typing command is not configured.")))))

(defun my-chatgpt-shell--voice-command-configured-p ()
  (ignore-errors
    (let ((cmd (my-chatgpt-shell--voice-command-list)))
      (and (consp cmd) (stringp (car cmd))))))

(defun my-chatgpt-shell-voice-running-p ()
  (and my-chatgpt-shell--voice-process
       (process-live-p my-chatgpt-shell--voice-process)))

(defun my-chatgpt-shell--voice-sentinel (proc event)
  (when (eq proc my-chatgpt-shell--voice-process)
    (setq my-chatgpt-shell--voice-process nil)
    (message "Stack voice typing stopped (%s)" (string-trim event))
    (my-chatgpt-shell--maybe-render-context)))

(defun my-chatgpt-shell-voice-start ()
  "Start the Futon voice typing process."
  (interactive)
  (if (my-chatgpt-shell-voice-running-p)
      (message "Stack voice typing already running.")
    (my-chatgpt-shell--voice-ensure-ydotoold)
    (let* ((cmd (my-chatgpt-shell--voice-command-list))
           (program (car cmd))
           (args (cdr cmd))
           (buffer (get-buffer-create my-chatgpt-shell-voice-buffer-name))
           (process-environment
            (cons (format "YDOTOOL_SOCKET_PATH=%s" my-chatgpt-shell-voice-socket-path)
                  process-environment))
           (proc (apply #'start-process "stack-voice"
                        buffer program args)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel proc #'my-chatgpt-shell--voice-sentinel)
      (setq my-chatgpt-shell--voice-process proc)
      (message "Stack voice typing started (buffer %s)."
               my-chatgpt-shell-voice-buffer-name)
      (my-chatgpt-shell--maybe-render-context)
      proc)))

(defun my-chatgpt-shell-voice-stop ()
  "Stop the Futon voice typing process."
  (interactive)
  (if (my-chatgpt-shell-voice-running-p)
      (progn
        (kill-process my-chatgpt-shell--voice-process)
        (message "Stopping Stack voice typing...")
        (my-chatgpt-shell--voice-stop-ydotoold))
    (message "Stack voice typing is not running.")))

(defun my-chatgpt-shell-voice-toggle ()
  "Toggle the Futon voice typing process."
  (interactive)
  (if (my-chatgpt-shell-voice-running-p)
      (my-chatgpt-shell-voice-stop)
    (my-chatgpt-shell-voice-start)))

(defun my-chatgpt-shell--voice-ydotoold-command-list ()
  (let ((cmd my-chatgpt-shell-ydotoold-command))
    (cond
     ((and (stringp cmd) (not (string-empty-p cmd)))
      (list shell-file-name shell-command-switch cmd))
     ((and (consp cmd) (stringp (car cmd)))
      cmd)
     (t
      (user-error "Stack ydotoold command not configured.")))))

(defun my-chatgpt-shell--voice-ensure-ydotoold ()
  (if (and my-chatgpt-shell--ydotoold-process
           (process-live-p my-chatgpt-shell--ydotoold-process))
      my-chatgpt-shell--ydotoold-process
    (let* ((cmd (my-chatgpt-shell--voice-ydotoold-command-list))
           (program (car cmd))
           (args (append (cdr cmd)
                         (list "--socket-path" my-chatgpt-shell-voice-socket-path))))
      (make-directory (file-name-directory my-chatgpt-shell-voice-socket-path) t)
      (let ((proc (apply #'start-process "stack-ydotoold" nil program args)))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc (lambda (_proc event)
                                     (when (string-match-p "finished" event)
                                       (message "Stack ydotoold stopped (%s)" (string-trim event)))
                                     (setq my-chatgpt-shell--ydotoold-process nil)
                                     (my-chatgpt-shell--maybe-render-context)))
        (setq my-chatgpt-shell--ydotoold-process proc)
        (message "Stack ydotoold started (socket %s)." my-chatgpt-shell-voice-socket-path)
        (my-chatgpt-shell--maybe-render-context)
        proc))))

(defun my-chatgpt-shell--voice-stop-ydotoold ()
  (when (and my-chatgpt-shell--ydotoold-process
             (process-live-p my-chatgpt-shell--ydotoold-process))
    (kill-process my-chatgpt-shell--ydotoold-process)
    (setq my-chatgpt-shell--ydotoold-process nil)
    (message "Stack ydotoold stopping...")))

(provide 'voice-typing)

;;; voice-typing.el ends here
