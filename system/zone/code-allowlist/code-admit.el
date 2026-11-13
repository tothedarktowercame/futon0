;;; code-admit.el --- Joe's sign-off on additions to ~/code, from Emacs  -*- lexical-binding: t; -*-

;; M-x code-admit, then type what you would type after `code-admit' in a shell:
;;   futon8                      clone tothedarktowercame/futon8 and admit it
;;   clone <git-url> [name]      clone any repo and admit it
;;   add <name> [note]           admit an entry made by hand
;;   remove <name> [dest]        move an entry out
;;   check | lock | unlock
;; Output goes to the *code-admit* buffer.  The script and allowlist live beside this file
;; (futon0/system/zone/code-allowlist/; README.md there).

(defvar code-admit-script
  (expand-file-name "code-admit"
                    (file-name-directory (or load-file-name buffer-file-name
                                             "~/code/futon0/system/zone/code-allowlist/")))
  "Path to the code-admit script.")

(defun code-admit (args)
  "Run code-admit with ARGS (a string, split on spaces), showing output in *code-admit*."
  (interactive (list (read-string "code-admit " nil 'code-admit-history)))
  (let ((buf (get-buffer-create "*code-admit*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "\n$ code-admit %s\n" args)))
    (let ((proc (apply #'start-process "code-admit" buf code-admit-script
                       (split-string args " " t))))
      (set-process-sentinel
       proc (lambda (p _e)
              (when (memq (process-status p) '(exit signal))
                (message "code-admit: %s (exit %d); see *code-admit*"
                         args (process-exit-status p))))))
    (display-buffer buf)))

;; ---------------------------------------------------------------------------
;; Requests: agents ask, Joe signs off.
;;
;; An agent asks with:
;;   emacsclient --eval '(code-admit-request "futon8" "why it is needed" "claude-9")'
;; where the first string is what Joe would type after `code-admit'.  The request is appended
;; to `code-admit-requests-file' and announced in the echo area.  Joe runs
;; M-x code-admit-requests: `a' approves (runs code-admit with those arguments), `r' rejects
;; with a reason, `g' refreshes.  Every decision stays in the file as a record.

(require 'tabulated-list)

(defvar code-admit-requests-file (expand-file-name "~/storage/code-admit/requests.tsv")
  "Requests for additions to or removals from ~/code: id, time, requester, args, reason,
status (pending/approved/rejected), decision note.")

(defun code-admit--read-requests ()
  (when (file-exists-p code-admit-requests-file)
    (with-temp-buffer
      (insert-file-contents code-admit-requests-file)
      (delq nil (mapcar (lambda (line) (and (> (length line) 0) (split-string line "\t")))
                        (split-string (buffer-string) "\n"))))))

(defun code-admit--write-requests (rows)
  (make-directory (file-name-directory code-admit-requests-file) t)
  (with-temp-file code-admit-requests-file
    (dolist (r rows) (insert (mapconcat #'identity r "\t") "\n"))))

(defun code-admit--clean (s) (replace-regexp-in-string "[\t\n]+" " " (or s "")))

(defun code-admit-request (args reason requester)
  "Record a request to run `code-admit ARGS', with REASON, from REQUESTER; return its id."
  (let* ((rows (code-admit--read-requests))
         (id (format "r%d" (1+ (length rows))))
         (row (list id (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                    (code-admit--clean requester) (code-admit--clean args)
                    (code-admit--clean reason) "pending" "")))
    (code-admit--write-requests (append rows (list row)))
    (message "code-admit request %s from %s: %s (%s). M-x code-admit-requests"
             id requester args reason)
    id))

(defun code-admit--set-status (id status note)
  (code-admit--write-requests
   (mapcar (lambda (r) (if (equal (car r) id)
                           (append (seq-take r 5) (list status (code-admit--clean note)))
                         r))
           (code-admit--read-requests))))

(define-derived-mode code-admit-requests-mode tabulated-list-mode "code-admit-requests"
  "Pending and decided requests for ~/code."
  (setq tabulated-list-format [("id" 4 t) ("time" 20 t) ("from" 10 t) ("status" 9 t)
                               ("args" 30 nil) ("reason" 0 nil)])
  (tabulated-list-init-header))

(defun code-admit-requests--refresh ()
  (setq tabulated-list-entries
        (mapcar (lambda (r) (list (nth 0 r)
                                  (vector (nth 0 r) (nth 1 r) (nth 2 r) (nth 5 r)
                                          (nth 3 r) (nth 4 r))))
                (reverse (code-admit--read-requests))))
  (tabulated-list-print t))

(defun code-admit-requests ()
  "List requests for ~/code; a = approve, r = reject, g = refresh."
  (interactive)
  (let ((buf (get-buffer-create "*code-admit-requests*")))
    (with-current-buffer buf
      (code-admit-requests-mode)
      (local-set-key (kbd "a") #'code-admit-requests-approve)
      (local-set-key (kbd "r") #'code-admit-requests-reject)
      (local-set-key (kbd "g") (lambda () (interactive) (code-admit-requests--refresh)))
      (code-admit-requests--refresh))
    (pop-to-buffer buf)))

(defun code-admit-requests--current ()
  (let ((id (tabulated-list-get-id)))
    (or (seq-find (lambda (r) (equal (car r) id)) (code-admit--read-requests))
        (user-error "No request on this line"))))

(defun code-admit-requests-approve ()
  "Approve the request on this line: run code-admit with its arguments."
  (interactive)
  (let ((r (code-admit-requests--current)))
    (unless (equal (nth 5 r) "pending") (user-error "Request %s is %s" (car r) (nth 5 r)))
    (when (y-or-n-p (format "Run code-admit %s? " (nth 3 r)))
      (code-admit (nth 3 r))
      (code-admit--set-status (car r) "approved" "")
      (code-admit-requests--refresh))))

(defun code-admit-requests-reject (note)
  "Reject the request on this line, with NOTE."
  (interactive "sReason for rejecting: ")
  (let ((r (code-admit-requests--current)))
    (code-admit--set-status (car r) "rejected" note)
    (code-admit-requests--refresh)))

(provide 'code-admit)
;;; code-admit.el ends here
