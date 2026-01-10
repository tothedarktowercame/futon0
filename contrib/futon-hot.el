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

(defcustom futon-hot-keymap-reload-enabled t
  "When non-nil, reset keymaps defined in hot-reloaded files before eval."
  :type 'boolean
  :group 'futon-hot)

(defcustom my-chatgpt-shell-hot-reload-after-eval-hook nil
  "Hook run after a hot-reloaded file is evaluated.
Each function is called with the reloaded FILE."
  :type 'hook
  :group 'futon-hot)

(defcustom my-chatgpt-shell-hot-reload-after-batch-hook nil
  "Hook run after a batch hot reload completes."
  :type 'hook
  :group 'futon-hot)

(defcustom my-chatgpt-shell-hot-reload-debug nil
  "When non-nil, capture a backtrace buffer on hot reload errors."
  :type 'boolean
  :group 'futon-hot)

(defvar my-chatgpt-shell--hot-reload-enabled nil)
(defvar my-chatgpt-shell--hot-reload-watches nil)
(defvar my-chatgpt-shell--hot-reload-pending nil)
(defvar my-chatgpt-shell--hot-reload-timer nil)
(defvar my-chatgpt-shell--last-hot-reload nil)
(defvar my-chatgpt-shell--hot-reload-batch-p nil)
(defvar my-chatgpt-shell--hot-reload-last-error nil)
(defvar my-chatgpt-shell--hot-reload-last-backtrace nil)

(defvar futon-hot--keymap-index (make-hash-table :test 'equal)
  "Map of filename -> list of mode map symbols seen in that file.")

(defun futon-hot--buffer-keymap-symbols (&optional buffer)
  "Return a list of mode map symbols defined in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (let ((symbols '())
          (seen (make-hash-table :test 'equal)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "(def\\(var\\|const\\|var-keymap\\)\\_>\\s-*\\([[:alnum:]-]+-mode-map\\)\\_>"
                nil t)
          (let* ((name (match-string 2))
                 (sym (intern name)))
            (unless (gethash sym seen)
              (puthash sym t seen)
              (push sym symbols)))))
      (nreverse symbols))))

(defun futon-hot--remember-keymaps (file buffer)
  (when (and file futon-hot-keymap-reload-enabled)
    (puthash file (futon-hot--buffer-keymap-symbols buffer) futon-hot--keymap-index)))

(defun futon-hot--reset-keymaps (file &optional buffer)
  "Reset keymaps defined in FILE or BUFFER before re-eval.

Return an alist of (SYMBOL . VALUE) pairs for keymaps that were unbound."
  (when (and file futon-hot-keymap-reload-enabled)
    (let ((symbols (or (and buffer (futon-hot--buffer-keymap-symbols buffer))
                       (gethash file futon-hot--keymap-index)))
          (saved nil))
      (dolist (sym symbols)
        (when (and (boundp sym) (keymapp (symbol-value sym)))
          (push (cons sym (symbol-value sym)) saved)
          (ignore-errors (makunbound sym))))
      saved)))

(defun futon-hot--restore-keymaps (saved)
  "Restore keymaps from SAVED as returned by `futon-hot--reset-keymaps`."
  (dolist (entry saved)
    (when (consp entry)
      (set (car entry) (cdr entry)))))

(defun futon-hot--safe-error-message (err)
  "Return a safe error string for ERR."
  (condition-case nil
      (or (and err (error-message-string err))
          (and err (prin1-to-string err))
          "unknown error")
    (error "unknown error")))

(defun futon-hot--safe-message (text)
  "Emit TEXT via `message` safely."
  (ignore-errors
    (message "%s" (or text "unknown error"))))

(defun futon-hot--record-error (file err)
  (setq my-chatgpt-shell--hot-reload-last-error (list :file file :error err))
  (setq my-chatgpt-shell--hot-reload-last-backtrace
        (condition-case nil
            (with-temp-buffer
              (let ((standard-output (current-buffer)))
                (backtrace))
              (buffer-string))
          (error nil)))
  (when my-chatgpt-shell-hot-reload-debug
    (condition-case _err
        (let ((buf (get-buffer-create "*Stack Hot Reload Debug*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (format "Hot reload error in %s\n\n"
                              (if (stringp file)
                                  (file-name-nondirectory file)
                                "unknown")))
              (insert (format "Error: %s\n\n" (futon-hot--safe-error-message err)))
              (insert "Backtrace:\n")
              (let ((standard-output (current-buffer)))
                (backtrace))))
          (display-buffer buf))
      (error
       (futon-hot--safe-message "Stack hot reload: failed to render debug buffer.")))))

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
       (futon-hot--safe-message
        (format "Stack hot reload: could not watch %s (%s)"
                (file-name-nondirectory file)
                (futon-hot--safe-error-message err)))
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
    (let ((my-chatgpt-shell--hot-reload-batch-p t))
      (dolist (file files)
        (condition-case _err
            (my-chatgpt-shell--hot-reload-apply file)
          (error
           (futon-hot--safe-message "Stack hot reload: process error (unknown error)"))))
      (run-hook-with-args 'my-chatgpt-shell-hot-reload-after-batch-hook))))

(defun hot-reload-do-reload ()
  "Force a hot-reload pass for pending files."
  (interactive)
  (let ((files (seq-uniq my-chatgpt-shell--hot-reload-pending #'string=)))
    (setq my-chatgpt-shell--hot-reload-pending nil)
    (let ((my-chatgpt-shell--hot-reload-batch-p t))
      (if files
          (progn
            (dolist (file files)
              (my-chatgpt-shell--hot-reload-apply file))
            (run-hook-with-args 'my-chatgpt-shell-hot-reload-after-batch-hook)
            (message "Hot reload: reloaded %d pending file(s)." (length files)))
        (message "Hot reload: no pending changes (use hot-reload-do-reload-all).")))))

(defun hot-reload-do-reload-all ()
  "Force a hot-reload pass for all configured files."
  (interactive)
  (let ((my-chatgpt-shell--hot-reload-batch-p t))
    (dolist (file (plist-get (my-chatgpt-shell--hot-reload-resolve-files) :targets))
      (my-chatgpt-shell--hot-reload-apply file))
    (run-hook-with-args 'my-chatgpt-shell-hot-reload-after-batch-hook))
  (message "Hot reload: forced reload completed."))

(defun my-chatgpt-shell--hot-reload-apply (file)
  (let* ((buffer (and (file-readable-p file)
                      (or (find-buffer-visiting file)
                          (find-file-noselect file t))))
         (err nil))
    (when buffer
      (with-current-buffer buffer
        (if (buffer-modified-p)
            (futon-hot--safe-message
             (format "Stack hot reload: skipped %s (buffer has unsaved edits)."
                     (file-name-nondirectory file)))
          (revert-buffer :ignore-auto :noconfirm)
          (if (derived-mode-p 'emacs-lisp-mode)
              (let ((saved (futon-hot--reset-keymaps file (current-buffer))))
                (setq err (condition-case e
                              (progn
                                (eval-buffer)
                                (futon-hot--remember-keymaps file (current-buffer))
                                (setq my-chatgpt-shell--last-hot-reload (current-time))
                                (futon-hot--safe-message
                                 (format "Stack hot reload: evaluated %s"
                                         (file-name-nondirectory file)))
                                (condition-case hook-err
                                    (run-hook-with-args 'my-chatgpt-shell-hot-reload-after-eval-hook file)
                                  (error
                                   (let ((msg (futon-hot--safe-error-message hook-err)))
                                     (futon-hot--safe-message
                                      (format "Stack hot reload: after-eval hook failed (%s)"
                                              msg)))))
                                nil)
                            (error e)))
                (when err
                  (futon-hot--record-error file err)
                  (futon-hot--restore-keymaps saved)
                  (futon-hot--safe-message
                   (format "Stack hot reload: error in %s (%s)"
                           (file-name-nondirectory file)
                           (futon-hot--safe-error-message err)))))
            (futon-hot--safe-message
             (format "Stack hot reload: %s is not an Emacs Lisp buffer."
                     (file-name-nondirectory file)))))))
    (when (and (not buffer) file)
      (setq err (list 'error "buffer not available")))
    (when err
      (futon-hot--record-error file err)
      (futon-hot--safe-message
       (format "Stack hot reload: failed for %s (%s)"
               (if (and file (stringp file))
                   (file-name-nondirectory file)
                 "unknown")
               (futon-hot--safe-error-message err))))))
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

(defun my-chatgpt-shell-hot-reload-refresh (&optional enable)
  "Rebuild hot reload watchers.
With ENABLE non-nil, enable hot reload if it is currently disabled."
  (interactive)
  (let ((was my-chatgpt-shell--hot-reload-enabled))
    (when was
      (my-chatgpt-shell-hot-reload-disable))
    (when (or was enable)
      (my-chatgpt-shell-hot-reload-enable))))

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
