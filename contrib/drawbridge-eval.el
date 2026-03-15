;;; drawbridge-eval.el --- Evaluate Clojure via Drawbridge -*- lexical-binding: t; -*-

;;; Commentary:
;; Small Emacs wrapper for futon3c Drawbridge (nREPL-over-HTTP).
;; Useful for administrative or diagnostic forms that should be evaluated
;; in the running JVM from within Emacs.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)

(defgroup drawbridge-eval nil
  "Evaluate Clojure forms in the running futon3c JVM."
  :group 'tools)

(defcustom drawbridge-eval-url "http://localhost:6768"
  "Base URL for the Drawbridge eval endpoint."
  :type 'string
  :group 'drawbridge-eval)

(defcustom drawbridge-eval-token nil
  "Admin token for Drawbridge eval.
If nil, read from `.admintoken` in the nearest enclosing project root."
  :type '(choice (const nil) string)
  :group 'drawbridge-eval)

(defcustom drawbridge-eval-timeout 5
  "Default timeout in seconds for Drawbridge eval requests."
  :type 'number
  :group 'drawbridge-eval)

(defun drawbridge-eval--project-roots ()
  "Return candidate roots that may contain `.admintoken`."
  (delete-dups
   (delq nil
         (list (locate-dominating-file default-directory ".admintoken")
               (locate-dominating-file default-directory "AGENTS.md")
               (and load-file-name
                    (locate-dominating-file load-file-name ".admintoken"))
               "/home/joe/code/futon3c/"))))

(defun drawbridge-eval--token ()
  "Return the Drawbridge token, reading `.admintoken` on first use."
  (or drawbridge-eval-token
      (let* ((token-file
              (cl-find-if
               #'file-exists-p
               (mapcar (lambda (root)
                         (expand-file-name ".admintoken" root))
                       (drawbridge-eval--project-roots)))))
        (when token-file
          (setq drawbridge-eval-token
                (string-trim (with-temp-buffer
                               (insert-file-contents token-file)
                               (buffer-string))))))))

(defun drawbridge-eval--parse-response-body (body)
  "Parse Drawbridge response BODY into an alist.
Accept both JSON and the EDN-like map strings returned by some futon3c
Drawbridge paths."
  (or
   (condition-case nil
       (json-parse-string body :object-type 'alist :array-type 'list :null-object nil)
     (error nil))
   (let ((ok (cond
              ((string-match ":ok[[:space:]]+true" body) t)
              ((string-match ":ok[[:space:]]+false" body) nil)
              (t :missing)))
         (value (cond
                 ((string-match ":value[[:space:]]+\"\\([^\"]*\\)\"" body)
                  (match-string 1 body))
                 ((string-match ":value[[:space:]]+\\([^,}\n]+\\)" body)
                  (let ((raw (string-trim (match-string 1 body))))
                    (cond
                     ((string= raw "nil") nil)
                     ((string= raw "true") t)
                     ((string= raw "false") nil)
                     ((string-match-p "\\`[-+]?[0-9]+\\'" raw) (string-to-number raw))
                     (t raw))))
                 (t nil)))
         (error-text (when (string-match ":error[[:space:]]+\"\\([^\"]*\\)\"" body)
                       (match-string 1 body)))
         (type-text (when (string-match ":type[[:space:]]+\"\\([^\"]*\\)\"" body)
                      (match-string 1 body))))
     (when (not (eq ok :missing))
       `((ok . ,ok)
         (value . ,value)
         (error . ,error-text)
         (type . ,type-text))))))

(defun drawbridge-eval-request (clj-code &optional timeout-seconds)
  "Evaluate CLJ-CODE via Drawbridge and return the parsed response alist.
Signals an error if the request fails or the response is not parseable."
  (let* ((url (format "%s/eval" (string-remove-suffix "/" drawbridge-eval-url)))
         (token (drawbridge-eval--token))
         (payload (string-as-unibyte (encode-coding-string clj-code 'utf-8)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("x-admin-token" . ,token)
            ("Content-Type" . "text/plain; charset=utf-8")))
         (url-request-data payload)
         (buffer (condition-case err
                     (url-retrieve-synchronously url t t (or timeout-seconds drawbridge-eval-timeout))
                   (error
                    (signal (car err) (cdr err))))))
    (unless (buffer-live-p buffer)
      (error "Drawbridge request did not return a buffer"))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (re-search-forward "\n\n" nil 'move)
          (let* ((body (buffer-substring-no-properties (point) (point-max)))
                 (parsed (drawbridge-eval--parse-response-body body)))
            (unless parsed
              (error "Drawbridge returned unparseable response: %s" body))
            parsed))
      (kill-buffer buffer))))

(defun drawbridge-eval (clj-code &optional timeout-seconds)
  "Evaluate CLJ-CODE via Drawbridge and return its `value` field.
Signals an error when Drawbridge reports failure."
  (let* ((response (drawbridge-eval-request clj-code timeout-seconds))
         (ok (alist-get 'ok response))
         (value (alist-get 'value response))
         (error-text (or (alist-get 'err response)
                         (alist-get 'error response))))
    (unless ok
      (error "Drawbridge eval failed%s"
             (if error-text
                 (format ": %s" error-text)
               "")))
    value))

(defun drawbridge-eval-read (clj-code)
  "Prompt for CLJ-CODE, evaluate it via Drawbridge, and echo the result."
  (interactive "sDrawbridge eval: ")
  (let ((value (drawbridge-eval clj-code)))
    (message "Drawbridge => %S" value)
    value))

(defun drawbridge-eval-last-sexp ()
  "Evaluate the preceding s-expression as Clojure via Drawbridge."
  (interactive)
  (drawbridge-eval-read
   (buffer-substring-no-properties
    (save-excursion (backward-sexp) (point))
    (point))))

(defun drawbridge-eval-region (beg end)
  "Evaluate region from BEG to END as Clojure via Drawbridge."
  (interactive "r")
  (drawbridge-eval-read
   (buffer-substring-no-properties beg end)))

(provide 'drawbridge-eval)

;;; drawbridge-eval.el ends here
