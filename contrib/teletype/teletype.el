;;; teletype.el --- type on one terminal frame, act in another  -*- lexical-binding: t; -*-

;; One keyboard, two screens.  Phone A has the keyboard, phone B drives a second
;; monitor; both are `emacsclient -t' frames on the same Emacs daemon.  In A, run
;; `teletype-connect': from then on every key typed on A is executed in B's
;; selected window, and A's screen shows a running printout of what was sent.
;; C-] (telnet's escape) ends the session.
;;
;; No network layer: the shared daemon already is the switchboard.

;;; Code:

(require 'subr-x)

(defgroup teletype nil
  "Relay keystrokes from one terminal frame to another."
  :group 'frames)

(defcustom teletype-escape-key (kbd "C-]")
  "Key that ends a teletype session.  It is never relayed."
  :type 'key-sequence)

(defvar teletype--target nil
  "Frame that receives relayed keys, or nil when not connected.")

(defvar teletype--source nil
  "Frame whose keyboard is being relayed.")

(defconst teletype--buffer-name "*teletype*")

(defun teletype--frame-label (frame)
  "A short human label for FRAME: its name and terminal device."
  (let ((term (frame-terminal frame)))
    (format "%s on %s"
            (frame-parameter frame 'name)
            (or (and (terminal-live-p term) (terminal-name term)) "?"))))

(defun teletype--other-frames ()
  "Live frames other than the selected one, on other terminals first."
  (let ((here (frame-terminal (selected-frame))))
    (sort (seq-filter
           (lambda (f)
             (and (frame-live-p f)
                  (not (eq f (selected-frame)))
                  (frame-visible-p f)
                  ;; A daemon's placeholder frame lives on "initial_terminal".
                  (not (equal (terminal-name (frame-terminal f)) "initial_terminal"))))
           (frame-list))
          (lambda (a _b) (not (eq (frame-terminal a) here))))))

(defun teletype--choose-target ()
  (let ((frames (teletype--other-frames)))
    (pcase (length frames)
      (0 (user-error "No other frame to relay to; open emacsclient -t on the other phone"))
      (1 (car frames))
      (_ (let* ((alist (mapcar (lambda (f) (cons (teletype--frame-label f) f)) frames))
                (pick (completing-read "Relay to frame: " alist nil t)))
           (cdr (assoc pick alist)))))))

(defun teletype--print (fmt &rest args)
  "Append a line to the printout in the source frame."
  (with-current-buffer (get-buffer-create teletype--buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (apply #'format fmt args))
      (dolist (w (get-buffer-window-list (current-buffer) nil t))
        (set-window-point w (point-max))))))

(defun teletype--describe (keys)
  "Printable form of KEYS for the printout: plain text inline, chords bracketed."
  (let ((s (key-description keys)))
    (cond ((equal s "SPC") " ")
          ((equal s "RET") "\n")
          ((= (length s) 1) s)
          (t (format "‹%s›" s)))))

(defvar teletype--relay-map
  (let ((map (make-sparse-keymap))
        (esc (make-sparse-keymap)))
    (define-key esc [t] #'teletype-relay)
    ;; ESC O and ESC [ start the SS3/CSI codes terminals send for arrows and
    ;; function keys.  Keep them open as prefixes too, so the default binding
    ;; does not end the sequence before `input-decode-map' has seen all of it.
    (dolist (intro '("O" "["))
      (let ((sub (make-sparse-keymap)))
        (define-key sub [t] #'teletype-relay)
        (define-key esc intro sub)))
    (define-key map [t] #'teletype-relay)
    ;; ESC must stay a prefix: on a terminal, arrow and function keys arrive
    ;; as ESC sequences, and binding ESC itself would stop `input-decode-map'
    ;; from turning them into <up>, <f1> ... before the relay sees them.
    (define-key map (kbd "ESC") esc)
    map)
  "Installed as the source terminal's `overriding-terminal-local-map'.
Only default bindings: every key sequence lands in `teletype-relay' after
one key (or ESC plus one key), and the relay reads any further keys of the
sequence against the target's keymaps.")

(defun teletype--install (on)
  "Install or remove the relay map on the source terminal.
Must run while a frame of the source terminal is selected: the variable
is terminal-local."
  (setq overriding-terminal-local-map (and on teletype--relay-map)))

(defconst teletype--readers
  '(read-from-minibuffer read-event read-char read-char-exclusive
    read-key-sequence read-key-sequence-vector)
  "Primitives that wait for keyboard input.
During a relayed command each is made to wait on the source frame.")

(defun teletype--input-on-source (orig &rest args)
  "Around advice: do keyboard reads on the source frame.
Waiting for input while the target frame is selected waits on the
target's terminal, which has no keyboard (\"Terminal N is locked\").
So every read -- minibuffer prompts, y/n questions, query-replace
answers -- happens on the source frame, where the typing is, and the
command then carries on in the target window."
  (if (and (frame-live-p teletype--source)
           (not (eq (frame-terminal (selected-frame))
                    (frame-terminal teletype--source))))
      (with-selected-window (frame-selected-window teletype--source)
        (apply orig args))
    (apply orig args)))

(defun teletype--advise (on)
  (dolist (f teletype--readers)
    (if on
        (advice-add f :around #'teletype--input-on-source)
      (advice-remove f #'teletype--input-on-source))))

(defun teletype--read-sequence (start win)
  "Complete the key sequence beginning with the keys START, using WIN's keymaps.
Keys are read on the source frame with `read-key', which decodes
function keys; whether more keys are needed is decided in WIN, so
prefixes like C-x and ESC (Meta on a terminal) follow the target's
bindings."
  (let ((keys start))
    (while (keymapp (with-selected-window win (key-binding keys t)))
      (setq keys (vconcat keys (vector (read-key)))))
    keys))

(defun teletype--run-in-target (start)
  "Complete the key sequence beginning with the keys START; run it in the target."
  (let ((win (frame-selected-window teletype--target))
        keys cmd)
    (teletype--install nil)
    (unwind-protect
        (progn
          (setq keys (teletype--read-sequence start win))
          (if (equal keys (vconcat teletype-escape-key))
              (setq cmd 'teletype--escape)
            (setq cmd (with-selected-window win (key-binding keys t)))
            (teletype--print "%s" (teletype--describe keys))
            (if (not (commandp cmd))
                (teletype--print "‹undefined›")
              (teletype--advise t)
              (unwind-protect
                  (with-selected-window win
                    (setq last-command-event (aref keys (1- (length keys)))
                          this-command cmd)
                    (condition-case err
                        (call-interactively cmd nil keys)
                      ((quit error)
                       (teletype--print "‹%s›" (error-message-string err)))))
                (teletype--advise nil)))))
      ;; Back on the source frame: re-arm the relay unless we were told to stop.
      (if (eq cmd 'teletype--escape)
          (teletype-disconnect)
        (when teletype--target (teletype--install t)))))
  (redisplay t))

(defun teletype-relay ()
  "Relay the key sequence that starts with the current event to the target frame."
  (interactive)
  (if (not (frame-live-p teletype--target))
      (progn (teletype-disconnect)
             (message "teletype: target frame is gone"))
    (teletype--run-in-target (this-command-keys-vector))))


;; Derived from fundamental mode, not special-mode: a parent's explicit
;; bindings (q, g, SPC ...) would win over the [t] default and not be relayed.
(define-derived-mode teletype-mode nil "Teletype"
  "Printout buffer of a teletype session.  Every key is relayed to the target frame."
  (setq buffer-read-only t
        truncate-lines nil))

;;;###autoload
(defun teletype-connect (&optional frame)
  "Relay this frame's keyboard to FRAME (chosen interactively)."
  (interactive)
  (setq teletype--target (or frame (teletype--choose-target))
        teletype--source (selected-frame))
  (let ((buf (get-buffer-create teletype--buffer-name)))
    (with-current-buffer buf
      (teletype-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "TELETYPE → %s\nEverything you type runs there.  %s to stop.\n%s\n"
                        (teletype--frame-label teletype--target)
                        (key-description teletype-escape-key)
                        (make-string 40 ?─)))))
    (switch-to-buffer buf)
    (delete-other-windows)
    (teletype--install t)
    (message "teletype: relaying to %s" (teletype--frame-label teletype--target))))

(defun teletype-disconnect ()
  "End the teletype session."
  (interactive)
  (when teletype--target
    (teletype--print "\n%s\n[disconnected]\n" (make-string 40 ?─)))
  (when (and teletype--source (frame-live-p teletype--source))
    (with-selected-frame teletype--source (teletype--install nil)))
  (setq teletype--target nil)
  (message "teletype: disconnected"))

(provide 'teletype)
;;; teletype.el ends here
