;;; futon-crdt-par.el --- Collaborative PAR sessions via CRDT -*- lexical-binding: t; -*-

;;; Commentary:
;; Real-time collaborative Post-Action Review (PAR) sessions using crdt.el.
;;
;; Server setup (add to init.el on server):
;;   (require 'futon-crdt-par)
;;   (add-hook 'emacs-startup-hook #'futon-crdt-ensure-server)
;;
;; Laptop setup (add to init.el on laptop):
;;   (require 'futon-crdt-par)
;;   (setq futon-crdt-server-host "YOUR_LINODE_IP")
;;   ;; Optionally auto-connect:
;;   ;; (add-hook 'emacs-startup-hook #'futon-crdt-connect-to-server)
;;
;; Usage:
;;   M-x futon-start-par - Create and share a PAR buffer

;;; Code:

(require 'crdt)

(defgroup futon-crdt nil
  "Futon CRDT collaboration settings."
  :group 'futon)

(defcustom futon-crdt-port 6530
  "Port for Futon CRDT server."
  :type 'integer
  :group 'futon-crdt)

(defcustom futon-crdt-server-host "localhost"
  "Host for CRDT server. Set to Linode IP on laptop."
  :type 'string
  :group 'futon-crdt)

(defcustom futon-crdt-display-name nil
  "Display name for CRDT session. Defaults to user-login-name."
  :type '(choice (const nil) string)
  :group 'futon-crdt)

(defcustom futon-musn-endpoint "http://localhost:6065"
  "MUSN HTTP endpoint for PAR submission."
  :type 'string
  :group 'futon-crdt)

(defvar futon-par-session-id nil
  "Current MUSN session ID for PAR submission.
Set this to associate PAR with an active Lab session.")

(defun futon-crdt--display-name ()
  "Get display name for CRDT session."
  (or futon-crdt-display-name user-login-name "futon-user"))

(defun futon-crdt-ensure-server ()
  "Ensure CRDT server is running. Call this on the server Emacs."
  (interactive)
  (require 'crdt)
  (let ((url (format "localhost:%d" futon-crdt-port)))
    (unless (seq-find (lambda (s) (string= (crdt--session-urlstr s) url))
                      crdt--session-list)
      (crdt-new-session futon-crdt-port nil nil
                        (futon-crdt--display-name)
                        crdt-default-session-permissions))
    (message "CRDT server ready on port %d" futon-crdt-port)))

(defun futon-crdt-connect-to-server ()
  "Connect to Futon CRDT server. Call this on laptop Emacs."
  (interactive)
  (require 'crdt)
  (let ((url (format "%s:%d" futon-crdt-server-host futon-crdt-port)))
    (crdt-connect url)
    (message "Connected to CRDT server at %s" url)))

(defun futon-crdt-session ()
  "Get the active Futon CRDT session, or nil."
  (car crdt--session-list))

(defun insert-par-template ()
  "Insert the 5-question PAR template at point."
  (interactive)
  (let ((pos (point)))
    (insert (mapconcat #'identity
              '("**1. Review the intention: what do we expect to learn or make together?**"
                ""
                "**2. Establish what is happening: what and how are we learning?**"
                ""
                "**3. What are some different perspectives on what is happening?**"
                ""
                "**4. What did we learn or change?**"
                ""
                "**5. What else should we change going forward?**"
                "")
              "\n"))
    (goto-char pos)))

(defun futon-start-par (&optional title)
  "Start a collaborative PAR session.
Creates a buffer with the PAR template and shares it via CRDT."
  (interactive "sSession title (optional): ")
  (let* ((ts (format-time-string "%Y-%m-%d-%H%M"))
         (buf-name (if (and title (not (string-empty-p title)))
                       (format "*PAR: %s*" title)
                     (format "*PAR-%s*" ts)))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "# PAR: %s\n" (or title ts)))
      (insert (format "# Session: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert-par-template)
      (goto-char (point-min))
      (when (fboundp 'markdown-mode) (markdown-mode)))
    (switch-to-buffer buf)
    ;; Share in CRDT session if one exists
    (when-let ((session (futon-crdt-session)))
      (crdt-share-buffer (crdt--session-urlstr session)))
    (message "PAR buffer ready: %s" buf-name)
    buf))

(defun futon-par-submit (&optional session-id)
  "Submit the current PAR buffer to MUSN.
Extracts the 5 answers and posts to /musn/par endpoint.
SESSION-ID defaults to `futon-par-session-id' or prompts if nil."
  (interactive
   (list (or futon-par-session-id
             (read-string "MUSN Session ID: "))))
  (unless session-id
    (user-error "No session ID provided. Set `futon-par-session-id' or provide one"))
  (let* ((content (buffer-string))
         (sections (futon-par--parse-sections content))
         (url (concat futon-musn-endpoint "/musn/par"))
         (payload (json-encode
                   `(("session/id" . ,session-id)
                     ("par/questions" . ,sections)
                     ("par/tags" . ["checkpoint"])))))
    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-data payload))
      (url-retrieve
       url
       (lambda (status)
         (if (plist-get status :error)
             (message "PAR submit failed: %S" (plist-get status :error))
           (goto-char url-http-end-of-headers)
           (let ((resp (json-read)))
             (if (eq (alist-get 'ok resp) t)
                 (message "PAR submitted successfully to session %s" session-id)
               (message "PAR submit error: %s" (alist-get 'err resp))))))
       nil t t)))
  (message "Submitting PAR to session %s..." session-id))

(defun futon-par-set-session (session-id)
  "Set the current MUSN session ID for PAR submission."
  (interactive "sSession ID: ")
  (setq futon-par-session-id session-id)
  (message "PAR session set to: %s" session-id))

(defun futon-par-list-sessions ()
  "List active MUSN Lab sessions and allow selection."
  (interactive)
  (let* ((url (concat (replace-regexp-in-string "/6065$" "/5050" futon-musn-endpoint)
                      "/fulab/lab/sessions/active"))
         (buf (url-retrieve-synchronously url t t 5)))
    (when buf
      (with-current-buffer buf
        (goto-char url-http-end-of-headers)
        (let* ((resp (json-read))
               (sessions (alist-get 'sessions resp))
               (choices (mapcar (lambda (s)
                                  (cons (format "%s (%s, %sKB)"
                                                (alist-get 'id s)
                                                (alist-get 'project s)
                                                (alist-get 'size-kb s))
                                        (alist-get 'id s)))
                                sessions)))
          (kill-buffer buf)
          (when choices
            (let* ((choice (completing-read "Select session: " choices nil t))
                   (session-id (cdr (assoc choice choices))))
              (futon-par-set-session session-id))))))))

(defun futon-par--parse-sections (content)
  "Parse PAR content into alist of question answers."
  (let ((result '()))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      ;; Find each question and its answer
      (dolist (q '(("intention" . "\\*\\*1\\.[^*]+\\*\\*")
                   ("happening" . "\\*\\*2\\.[^*]+\\*\\*")
                   ("perspectives" . "\\*\\*3\\.[^*]+\\*\\*")
                   ("learned" . "\\*\\*4\\.[^*]+\\*\\*")
                   ("forward" . "\\*\\*5\\.[^*]+\\*\\*")))
        (goto-char (point-min))
        (when (re-search-forward (cdr q) nil t)
          (let* ((start (point))
                 (end (or (and (re-search-forward "\\*\\*[0-9]\\." nil t)
                               (match-beginning 0))
                          (point-max)))
                 (answer (string-trim (buffer-substring-no-properties start end))))
            (push (cons (car q) answer) result)))))
    (nreverse result)))

(provide 'futon-crdt-par)

;;; futon-crdt-par.el ends here
