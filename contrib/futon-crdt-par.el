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

(defvar futon-par-session-ids nil
  "List of session IDs for multi-agent PAR.
When set, PAR is written to all sessions as a nexus point.")

(defvar futon-par-participants nil
  "List of participant names for multi-agent PAR.
E.g., (:fucodex :fuclaude :joe)")

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

(defun futon-par-submit-multi ()
  "Submit PAR to multiple sessions as a shared nexus point.
Uses `futon-par-session-ids' for target sessions.
The same PAR appears in all session timelines with cross-references."
  (interactive)
  (unless futon-par-session-ids
    (user-error "No sessions set. Use `futon-par-add-session' first"))
  (let* ((content (buffer-string))
         (sections (futon-par--parse-sections content))
         (par-id (format "par-joint-%s" (format-time-string "%Y%m%d-%H%M%S")))
         (timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
         (url (concat futon-musn-endpoint "/musn/par/multi"))
         (payload (json-encode
                   `(("par/id" . ,par-id)
                     ("par/timestamp" . ,timestamp)
                     ("par/sessions" . ,futon-par-session-ids)
                     ("par/participants" . ,futon-par-participants)
                     ("par/questions" . ,sections)
                     ("par/tags" . ["joint" "nexus"])))))
    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          (url-request-data payload))
      (url-retrieve
       url
       (lambda (status)
         (if (plist-get status :error)
             (message "Multi-PAR submit failed: %S" (plist-get status :error))
           (goto-char url-http-end-of-headers)
           (let ((resp (json-read)))
             (if (eq (alist-get 'ok resp) t)
                 (message "PAR submitted to %d sessions: %s"
                          (length futon-par-session-ids)
                          (mapconcat #'identity futon-par-session-ids ", "))
               (message "Multi-PAR error: %s" (alist-get 'err resp))))))
       nil t t)))
  (message "Submitting joint PAR to %d sessions..." (length futon-par-session-ids)))

(defun futon-par-add-session (session-id)
  "Add SESSION-ID to the multi-agent PAR session list."
  (interactive "sSession ID to add: ")
  (add-to-list 'futon-par-session-ids session-id)
  (message "Sessions: %s" (mapconcat #'identity futon-par-session-ids ", ")))

(defun futon-par-add-participant (name)
  "Add NAME to the PAR participants list."
  (interactive "sParticipant name: ")
  (add-to-list 'futon-par-participants (intern (concat ":" name)))
  (message "Participants: %s" futon-par-participants))

(defun futon-par-clear-multi ()
  "Clear multi-session PAR state."
  (interactive)
  (setq futon-par-session-ids nil
        futon-par-participants nil)
  (message "Multi-PAR state cleared"))

(defun futon-start-joint-par (title &rest participants)
  "Start a collaborative PAR for multiple PARTICIPANTS.
TITLE is the PAR session title.
PARTICIPANTS are symbols like :fucodex :fuclaude :joe.

Example: (futon-start-joint-par \"Lab Upload Standup\" :fucodex :fuclaude :joe)"
  (interactive "sJoint PAR title: ")
  (setq futon-par-session-ids nil
        futon-par-participants (or participants
                                   (mapcar #'intern
                                           (split-string
                                            (read-string "Participants (space-separated): ")))))
  (let ((buf (futon-start-par title)))
    (with-current-buffer buf
      (goto-char (point-min))
      (end-of-line)
      (insert (format "\n# Participants: %s"
                      (mapconcat (lambda (p) (substring (symbol-name p) 1))
                                 futon-par-participants ", ")))
      (insert "\n# Type: Joint/Nexus PAR (appears in all participant timelines)"))
    (message "Joint PAR started. Add sessions with `futon-par-add-session', submit with `futon-par-submit-multi'")
    buf))

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
