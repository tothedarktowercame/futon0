;;; stack-hud-widgets-test.el --- ERT tests for stack-hud widgets -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)

(defvar arxana-evidence--open-session-item-cache)

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'stack-hud-widgets)

(ert-deftest stack-hud-widgets-sessions-in-flight-filters-stale-cache ()
  (let ((arxana-evidence--open-session-item-cache
         (make-hash-table :test 'equal)))
    (puthash "old"
             (list :type 'evidence-open-session
                   :session-id "sid-stale"
                   :about "Stale cached summary"
                   :latest "2026-04-26T09:00:00Z")
             arxana-evidence--open-session-item-cache)
    (puthash "live"
             (list :type 'evidence-open-session
                   :session-id "sid-live"
                   :about "Live cached summary"
                   :latest "2026-04-26T08:00:00Z")
             arxana-evidence--open-session-item-cache)
    (cl-letf (((symbol-function 'arxana-evidence--open-repl-sessions)
               (lambda ()
                 (list (list :buffer "*codex-repl:live*"
                             :agent "codex"
                             :session-id "sid-live"
                             :evidence-server "http://127.0.0.1:7071/api/alpha/evidence"
                             :state "running"
                             :latest-id-hint "e-live")))))
      (let* ((data (stack-hud-widget-data-sessions-in-flight))
             (items (plist-get data :items)))
        (should (eq (plist-get data :source) :live+cache))
        (should (= (length items) 1))
        (should (equal (plist-get (car items) :session-id) "sid-live"))
        (should (equal (plist-get (car items) :about) "Live cached summary"))))))

(ert-deftest stack-hud-widgets-sessions-in-flight-ignores-cache-without-live-membership ()
  (let ((arxana-evidence--open-session-item-cache
         (make-hash-table :test 'equal)))
    (puthash "old"
             (list :type 'evidence-open-session
                   :session-id "sid-stale"
                   :about "Stale cached summary"
                   :latest "2026-04-26T09:00:00Z")
             arxana-evidence--open-session-item-cache)
    (cl-letf (((symbol-function 'arxana-evidence--open-repl-sessions)
               (lambda () nil)))
      (let ((data (stack-hud-widget-data-sessions-in-flight)))
        (should (eq (plist-get data :source) :live+cache))
        (should-not (plist-get data :items))))))

(ert-deftest stack-hud-widgets-render-sessions-in-flight-sorts-by-live-latest ()
  (cl-letf (((symbol-function 'stack-hud-widget-data-sessions-in-flight)
             (lambda ()
               (list :source :live+cache
                     :items
                     (list
                      (list :type 'evidence-open-session
                            :session-id "aaaaaaaa-old"
                            :agent "codex"
                            :state "running"
                            :count 3
                            :latest "2026-04-26T09:00:00Z")
                      (list :type 'evidence-open-session
                            :session-id "bbbbbbbb-new"
                            :agent "claude"
                            :state "idle"
                            :count 4
                            :latest "2026-04-25T09:00:00Z")))))
            ((symbol-function 'stack-hud-widget--session-stats)
             (lambda (sid)
               (if (equal sid "aaaaaaaa-old")
                   (list :count 3
                         :latest-at "2026-04-25T10:00:00Z"
                         :first-at "2026-04-25T09:00:00Z")
                 (list :count 4
                       :latest-at "2026-04-26T10:00:00Z"
                       :first-at "2026-04-26T09:00:00Z")))))
    (with-temp-buffer
      (let ((stack-hud-widget-sessions-top-k 1))
        (stack-hud-widget--render-sessions-in-flight)
        (let ((text (buffer-string)))
          (should (string-match-p "bbbbbbbb claude" text))
          (should-not (string-match-p "aaaaaaaa codex" text)))))))

(ert-deftest stack-hud-widgets-render-sessions-in-flight-annotates-session-block ()
  (let ((target (get-buffer-create "*stack-hud-widget-target*")))
    (unwind-protect
        (cl-letf (((symbol-function 'stack-hud-widget-data-sessions-in-flight)
                   (lambda ()
                     (list :source :live+cache
                           :items
                           (list
                            (list :type 'evidence-open-session
                                  :buffer (buffer-name target)
                                  :session-id "cccccccc-live"
                                  :agent "codex"
                                  :state "idle"
                                  :count 2
                                  :about "Annotated block"
                                  :outcome "Latest outcome"
                                  :latest "2026-04-26T09:00:00Z")))))
                  ((symbol-function 'stack-hud-widget--session-stats)
                   (lambda (_sid) nil)))
          (with-temp-buffer
            (stack-hud-widget--render-sessions-in-flight)
            (goto-char (point-min))
            (search-forward "Annotated block")
            (should (eq (get-text-property (point) 'stack-hud-target-buffer)
                        target))
            (search-forward "Latest outcome")
            (should (eq (get-text-property (point) 'stack-hud-target-buffer)
                        target))))
      (when (buffer-live-p target)
        (kill-buffer target)))))
