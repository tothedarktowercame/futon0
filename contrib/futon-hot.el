;;; futon-hot.el --- Hot reload helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared hot reload support for Futon Emacs integrations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup futon-hot nil
  "Hot reload helpers for Futon Emacs tooling."
  :group 'tools)

(defcustom futon-hot-root nil
  "Root directory used to resolve relative hot reload paths."
  :type '(choice (const :tag "Auto-detect" nil)
                 directory)
  :group 'futon-hot)

(defun futon-hot--base-root ()
  (or futon-hot-root
      (and (boundp 'my-futon3--repo-root) my-futon3--repo-root)
      (let ((base (or load-file-name (buffer-file-name))))
        (when base
          (expand-file-name ".." (file-name-directory base))))
      default-directory))

(defconst my-chatgpt-shell--hot-reload-default-files
  '("./futon-config.el")
  "Default file list for Stack hot reload.")

(defcustom my-chatgpt-shell-hot-reload-files my-chatgpt-shell--hot-reload-default-files
  "Files that auto-revert and re-evaluate when Stack hot reload is enabled.
Each entry can be absolute or relative to `futon-hot-root'."
  :type '(repeat file)
  :group 'futon-hot)

(defcustom my-chatgpt-shell-hot-reload-include-defaults t
  "When non-nil, always include default hot reload files."
  :type 'boolean
  :group 'futon-hot)

(defvar my-chatgpt-shell--hot-reload-enabled nil)
(defvar my-chatgpt-shell--hot-reload-watches nil)
(defvar my-chatgpt-shell--hot-reload-pending nil)
(defvar my-chatgpt-shell--hot-reload-timer nil)
(defvar my-chatgpt-shell--last-hot-reload nil)

(defun my-chatgpt-shell--hot-reload-feature-available-p ()
  (require 'filenotify nil t))

(defun my-chatgpt-shell--hot-reload-expand-path (path)
  (let ((expanded (if (file-name-absolute-p path)
                      path
                    (expand-file-name path (futon-hot--base-root)))))
    (when expanded
      (file-truename expanded))))

(defun my-chatgpt-shell--hot-reload-resolve-files ()
  (let ((targets nil)
        (missing nil))
    (let ((paths (if my-chatgpt-shell-hot-reload-include-defaults
                     (seq-uniq (append my-chatgpt-shell-hot-reload-files
                                       my-chatgpt-shell--hot-reload-default-files)
                               #'string=)
                   my-chatgpt-shell-hot-reload-files)))
      (dolist (path paths)
        (let ((abs (my-chatgpt-shell--hot-reload-expand-path path)))
          (cond
           ((and abs (file-exists-p abs))
            (push abs targets))
           (abs
            (push abs missing))
           (t nil)))))
    (list :targets (nreverse targets)
          :missing (nreverse missing))))

(defun my-chatgpt-shell--hot-reload-remove-watch (descriptor)
  (setq my-chatgpt-shell--hot-reload-watches
        (assq-delete-all descriptor my-chatgpt-shell--hot-reload-watches)))

(defun my-chatgpt-shell--hot-reload-watch (file)
  (when (and file (my-chatgpt-shell--hot-reload-feature-available-p))
    (condition-case err
        (let ((desc (file-notify-add-watch file '(change attribute-change)
                                           #'my-chatgpt-shell--hot-reload-dispatch)))
          (push (cons desc file) my-chatgpt-shell--hot-reload-watches)
          desc)
      (error
       (message "Stack hot reload: could not watch %s (%s)"
                (file-name-nondirectory file)
                (error-message-string err))
       nil))))

(defun my-chatgpt-shell--hot-reload-schedule (file)
  (let ((abs (and file (file-truename file))))
    (when abs
      (push abs my-chatgpt-shell--hot-reload-pending)
      (unless my-chatgpt-shell--hot-reload-timer
        (setq my-chatgpt-shell--hot-reload-timer
              (run-with-idle-timer 0.5 nil #'my-chatgpt-shell--hot-reload-process))))))

(defun my-chatgpt-shell--hot-reload-process ()
  (let ((files (seq-uniq my-chatgpt-shell--hot-reload-pending #'string=)))
    (setq my-chatgpt-shell--hot-reload-pending nil)
    (setq my-chatgpt-shell--hot-reload-timer nil)
    (dolist (file files)
      (my-chatgpt-shell--hot-reload-apply file))))

(defun hot-reload-do-reload ()
  "Force a hot-reload pass for all configured files."
  (interactive)
  (dolist (file (plist-get (my-chatgpt-shell--hot-reload-resolve-files) :targets))
    (my-chatgpt-shell--hot-reload-apply file))
  (message "Hot reload: forced reload completed."))

(defun my-chatgpt-shell--hot-reload-apply (file)
  (when (file-readable-p file)
    (let ((buffer (or (find-buffer-visiting file)
                      (find-file-noselect file t))))
      (when buffer
        (with-current-buffer buffer
          (if (buffer-modified-p)
              (message "Stack hot reload: skipped %s (buffer has unsaved edits)."
                       (file-name-nondirectory file))
            (revert-buffer :ignore-auto :noconfirm)
            (if (derived-mode-p 'emacs-lisp-mode)
                (condition-case err
                    (progn
                      (eval-buffer)
                      (setq my-chatgpt-shell--last-hot-reload (current-time))
                      (message "Stack hot reload: evaluated %s"
                               (file-name-nondirectory file)))
                  (error
                   (message "Stack hot reload: error in %s (%s)"
                            (file-name-nondirectory file)
                            (error-message-string err))))
              (message "Stack hot reload: %s is not an Emacs Lisp buffer."
                       (file-name-nondirectory file)))))))))

(defun my-chatgpt-shell--hot-reload-dispatch (event)
  (pcase event
    (`(,descriptor ,action ,file . ,_)
     (cond
      ((eq action 'stopped)
       (my-chatgpt-shell--hot-reload-remove-watch descriptor))
      ((and (memq action '(change attribute-change changed))
            my-chatgpt-shell--hot-reload-enabled)
       (let ((target (or file (cdr (assoc descriptor my-chatgpt-shell--hot-reload-watches)))))
         (my-chatgpt-shell--hot-reload-schedule target)))))))

(defun my-chatgpt-shell-hot-reload-enable ()
  "Enable Stack hot reload watchers."
  (interactive)
  (unless (my-chatgpt-shell--hot-reload-feature-available-p)
    (user-error "File notification support is unavailable in this Emacs."))
  (unless my-chatgpt-shell--hot-reload-enabled
    (setq my-chatgpt-shell--hot-reload-watches nil)
    (let* ((resolved (my-chatgpt-shell--hot-reload-resolve-files))
           (files (plist-get resolved :targets))
           (missing (plist-get resolved :missing)))
      (if (null files)
          (message "Stack hot reload: no readable targets configured.")
        (dolist (file files)
          (my-chatgpt-shell--hot-reload-watch file))
        (if (null my-chatgpt-shell--hot-reload-watches)
            (message "Stack hot reload: failed to activate any watchers.")
          (setq my-chatgpt-shell--hot-reload-enabled t)
          (message "Stack hot reload enabled (watching %d file%s)."
                   (length my-chatgpt-shell--hot-reload-watches)
                   (if (= (length my-chatgpt-shell--hot-reload-watches) 1) "" "s"))
          (when missing
            (message "Stack hot reload skipped: %s"
                     (string-join
                      (mapcar (lambda (path)
                                (file-relative-name path (futon-hot--base-root)))
                              missing)
                      ", ")))))))
  my-chatgpt-shell--hot-reload-enabled)

(defun my-chatgpt-shell-hot-reload-disable ()
  "Disable Stack hot reload watchers."
  (interactive)
  (dolist (entry my-chatgpt-shell--hot-reload-watches)
    (ignore-errors (file-notify-rm-watch (car entry))))
  (setq my-chatgpt-shell--hot-reload-watches nil)
  (setq my-chatgpt-shell--hot-reload-enabled nil)
  (when my-chatgpt-shell--hot-reload-timer
    (cancel-timer my-chatgpt-shell--hot-reload-timer)
    (setq my-chatgpt-shell--hot-reload-timer nil))
  (setq my-chatgpt-shell--hot-reload-pending nil)
  (message "Stack hot reload disabled."))

(defun my-chatgpt-shell-hot-reload-toggle (&optional arg)
  "Toggle Stack hot reload.
With prefix ARG enable when positive, disable otherwise."
  (interactive "P")
  (let ((enable (if arg
                    (> (prefix-numeric-value arg) 0)
                  (not my-chatgpt-shell--hot-reload-enabled))))
    (if enable
        (my-chatgpt-shell-hot-reload-enable)
      (my-chatgpt-shell-hot-reload-disable)))
  my-chatgpt-shell--hot-reload-enabled)

(defun my-chatgpt-shell--hot-reload-status-string ()
  (cond
   ((not (my-chatgpt-shell--hot-reload-feature-available-p))
    "unsupported")
   (my-chatgpt-shell--hot-reload-enabled "on")
   (t "off")))

(defun my-chatgpt-shell--hot-reload-last-delta-hours ()
  (when my-chatgpt-shell--last-hot-reload
    (/ (float-time (time-subtract (current-time) my-chatgpt-shell--last-hot-reload))
       3600.0)))

(defun my-chatgpt-shell--hot-reload-watching-count ()
  (length my-chatgpt-shell--hot-reload-watches))

(provide 'futon-hot)

;;; futon-hot.el ends here
