;;; futon-par-peripheral.el --- PAR peripheral for AI agents -*- lexical-binding: t; -*-

;;; Commentary:
;; Allows AI agents to participate in collaborative CRDT PAR sessions.
;; Each agent runs their own Emacs instance that connects to the CRDT server
;; and contributes their perspective to the shared PAR buffer.
;;
;; Usage (from shell):
;;   CRDT_HOST=server-ip AGENT_ID=fucodex PAR_TITLE="Lab Upload" \
;;     emacs --script futon-par-peripheral.el
;;
;; Or interactively:
;;   (par-peripheral-start "fucodex" "Lab Upload Debrief")

;;; Code:

(require 'crdt)
(require 'json)
(require 'url)

(defgroup futon-par-peripheral nil
  "PAR peripheral for AI agents."
  :group 'futon)

(defcustom par-peripheral-crdt-host "localhost"
  "CRDT server host."
  :type 'string
  :group 'futon-par-peripheral)

(defcustom par-peripheral-crdt-port 6530
  "CRDT server port."
  :type 'integer
  :group 'futon-par-peripheral)

(defcustom par-peripheral-agency-url "http://localhost:7070"
  "Agency server URL for dispatching to agents."
  :type 'string
  :group 'futon-par-peripheral)

(defcustom par-peripheral-timeout 300
  "Timeout in seconds for agent responses."
  :type 'integer
  :group 'futon-par-peripheral)

(defvar par-peripheral-agent-id nil
  "This peripheral's agent ID (e.g., fucodex, fuclaude).")

(defvar par-peripheral-par-buffer nil
  "The PAR buffer this peripheral is participating in.")

(defvar par-peripheral-state 'idle
  "Current state: idle, waiting, contributing, done.")

;; PAR section markers
(defconst par-peripheral-sections
  '(("intention" . "\\*\\*1\\.[^*]+\\*\\*")
    ("happening" . "\\*\\*2\\.[^*]+\\*\\*")
    ("perspectives" . "\\*\\*3\\.[^*]+\\*\\*")
    ("learned" . "\\*\\*4\\.[^*]+\\*\\*")
    ("forward" . "\\*\\*5\\.[^*]+\\*\\*"))
  "Regex patterns for each PAR section.")

(defun par-peripheral--find-section-end (section-name)
  "Find the end position of SECTION-NAME in current buffer.
Returns point after the section header, before the next section."
  (save-excursion
    (goto-char (point-min))
    (let ((pattern (cdr (assoc section-name par-peripheral-sections))))
      (when (re-search-forward pattern nil t)
        (let ((start (point)))
          ;; Find next section or end of buffer
          (if (re-search-forward "\\*\\*[0-9]\\." nil t)
              (match-beginning 0)
            (point-max)))))))

(defun par-peripheral--insert-contribution (section-name text)
  "Insert TEXT into SECTION-NAME of the PAR buffer."
  (with-current-buffer par-peripheral-par-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((pattern (cdr (assoc section-name par-peripheral-sections))))
        (when (re-search-forward pattern nil t)
          (end-of-line)
          (insert (format "\n\n**%s:** %s" par-peripheral-agent-id text)))))))

(defun par-peripheral--call-agent (prompt)
  "Call the agent via Agency /agency/page and return the response text.
PROMPT is the instruction for what to contribute.
Uses /agency/page to send synchronously to a connected agent."
  (let* ((url (format "%s/agency/page" par-peripheral-agency-url))
         (timeout-ms (* par-peripheral-timeout 1000)) ; Convert to milliseconds
         (payload (json-encode
                   `(("agent-id" . ,par-peripheral-agent-id)
                     ("prompt" . ,prompt)
                     ("timeout-ms" . ,timeout-ms))))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data payload)
         response)
    (with-current-buffer (url-retrieve-synchronously url t nil par-peripheral-timeout)
      (goto-char url-http-end-of-headers)
      (setq response (json-read))
      (kill-buffer))
    ;; Response structure: {:ok true :response {:result "..." ...}}
    (let ((inner-response (alist-get 'response response)))
      (or (alist-get 'result inner-response)
          (alist-get 'error inner-response)))))

(defun par-peripheral--get-par-context ()
  "Get the current PAR buffer content for context."
  (with-current-buffer par-peripheral-par-buffer
    (buffer-string)))

(defun par-peripheral--contribute-to-section (section-name question)
  "Have the agent contribute to SECTION-NAME by answering QUESTION."
  (message "[par-peripheral] %s contributing to %s..."
           par-peripheral-agent-id section-name)
  (let* ((context (par-peripheral--get-par-context))
         (prompt (format "You are participating in a Post-Action Review (PAR).

Current PAR state:
%s

Your task: Answer this PAR question from your perspective as %s.
Question: %s

Keep your response concise (2-4 sentences). Focus on your unique perspective.
Do not repeat what others have said. Just provide your answer, no preamble."
                         context par-peripheral-agent-id question))
         (response (par-peripheral--call-agent prompt)))
    (when response
      (par-peripheral--insert-contribution section-name response)
      (message "[par-peripheral] %s contributed to %s"
               par-peripheral-agent-id section-name))))

(defun par-peripheral--wait-for-buffer (buffer-pattern &optional timeout)
  "Wait for a buffer matching BUFFER-PATTERN to appear.
TIMEOUT defaults to 60 seconds."
  (let ((timeout (or timeout 60))
        (start-time (current-time)))
    (while (and (not (seq-find (lambda (b)
                                 (string-match-p buffer-pattern (buffer-name b)))
                               (buffer-list)))
                (< (float-time (time-subtract (current-time) start-time)) timeout))
      (sleep-for 1))
    (seq-find (lambda (b)
                (string-match-p buffer-pattern (buffer-name b)))
              (buffer-list))))

(defun par-peripheral-connect ()
  "Connect to the CRDT server."
  (interactive)
  (let ((url (format "%s:%d" par-peripheral-crdt-host par-peripheral-crdt-port)))
    (message "[par-peripheral] Connecting to CRDT at %s..." url)
    (crdt-connect url par-peripheral-agent-id)
    (message "[par-peripheral] Connected to CRDT")))

(defun par-peripheral-join-par (par-title)
  "Join the PAR buffer with PAR-TITLE."
  (let ((buffer-pattern (format "\\*PAR.*%s" (regexp-quote par-title))))
    (message "[par-peripheral] Waiting for PAR buffer: %s" par-title)
    (let ((buf (par-peripheral--wait-for-buffer buffer-pattern 120)))
      (if buf
          (progn
            (setq par-peripheral-par-buffer buf)
            (message "[par-peripheral] Joined PAR buffer: %s" (buffer-name buf)))
        (error "[par-peripheral] Timeout waiting for PAR buffer")))))

(defun par-peripheral-contribute ()
  "Contribute to all relevant PAR sections."
  (interactive)
  (setq par-peripheral-state 'contributing)

  ;; Agents typically contribute to sections 2, 3, 4
  ;; Section 1 (intention) is usually pre-filled
  ;; Section 5 (forward) is often collaborative at the end

  (par-peripheral--contribute-to-section
   "happening"
   "What actually happened? What approaches did you take and what worked?")

  (sleep-for 2) ; Brief pause between sections

  (par-peripheral--contribute-to-section
   "perspectives"
   "What's your unique perspective on what occurred? What did you notice?")

  (sleep-for 2)

  (par-peripheral--contribute-to-section
   "learned"
   "What explicit takeaways emerged from your work? What did you learn?")

  (setq par-peripheral-state 'done)
  (message "[par-peripheral] %s finished contributing" par-peripheral-agent-id))

(defun par-peripheral-start (agent-id par-title)
  "Start the PAR peripheral for AGENT-ID, joining PAR with PAR-TITLE."
  (interactive "sAgent ID: \nsPAR Title: ")
  (setq par-peripheral-agent-id agent-id)
  (setq par-peripheral-state 'waiting)

  (message "[par-peripheral] Starting peripheral for %s" agent-id)

  ;; Connect to CRDT
  (par-peripheral-connect)

  ;; Wait a moment for CRDT to sync
  (sleep-for 3)

  ;; Join the PAR buffer
  (par-peripheral-join-par par-title)

  ;; Contribute
  (par-peripheral-contribute)

  (message "[par-peripheral] %s complete" agent-id))

(defun par-peripheral-run ()
  "Entry point for batch mode.
Reads config from environment variables."
  (let ((host (or (getenv "CRDT_HOST") "localhost"))
        (port (string-to-number (or (getenv "CRDT_PORT") "6530")))
        (agent-id (or (getenv "AGENT_ID") "agent"))
        (par-title (or (getenv "PAR_TITLE") "Untitled PAR"))
        (agency-url (or (getenv "AGENCY_URL") "http://localhost:7070")))

    (setq par-peripheral-crdt-host host)
    (setq par-peripheral-crdt-port port)
    (setq par-peripheral-agency-url agency-url)

    (par-peripheral-start agent-id par-title)))

;; If running in batch mode, auto-start
(when noninteractive
  (par-peripheral-run))

(provide 'futon-par-peripheral)

;;; futon-par-peripheral.el ends here
