;;; teletype.el --- type on one terminal frame, act in another  -*- lexical-binding: t; -*-

;; One keyboard, two screens.  Phone A has the keyboard, phone B drives a second
;; monitor; both are `emacsclient -t' frames on the same Emacs daemon.
;;
;; Window-manager style (`teletype-wm-mode'): frames get positions, left and
;; right.  In the keyboard's frame, C-c <left> sends the keyboard to the left
;; monitor, C-c <right> brings it home, and C-] toggles.  The focused monitor's
;; mode line is highlighted.  While focus is home nothing is relayed at all.
;;
;; One-shot style: `teletype-connect' relays to another frame and turns this
;; screen into a running printout of what was sent; C-] ends it.
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

(defvar teletype-wm-mode)               ; defined by `define-minor-mode' below
(defvar teletype-wm-mode-map)

(defvar teletype--home nil
  "In `teletype-wm-mode', the frame on the terminal that has the keyboard.")

(defface teletype-focus
  '((t :background "green4" :foreground "white" :weight bold :inverse-video nil))
  "Mode line of the frame that currently receives the keyboard.")

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
          (let ((wm (and teletype-wm-mode (lookup-key teletype-wm-mode-map keys))))
            (cond ((commandp wm) (setq cmd (cons 'teletype--wm wm)))
                  ((equal keys (vconcat teletype-escape-key))
                   (setq cmd 'teletype--escape))))
          (unless cmd
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
      (cond ((eq cmd 'teletype--escape) (teletype-disconnect))
            ((eq (car-safe cmd) 'teletype--wm) (call-interactively (cdr cmd)))
            (teletype--target (teletype--install t)))))
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
  (teletype--update-indicators)
  (message "teletype: disconnected"))

;;;; Window-manager layer

(defun teletype--frame-at (position)
  "The live frame whose teletype position is POSITION, or nil."
  (seq-find (lambda (f) (and (frame-live-p f)
                             (eq (frame-parameter f 'teletype-position) position)))
            (frame-list)))

(defun teletype--focused-frame ()
  (if (frame-live-p teletype--target) teletype--target teletype--home))

(defun teletype--update-indicators ()
  "Highlight the mode line of the frame that has the keyboard's focus."
  (dolist (f (frame-list))
    (when (frame-live-p f)
      (face-spec-recalc 'mode-line f)
      (face-spec-recalc 'mode-line-active f)))
  (let ((f (teletype--focused-frame)))
    (when (and teletype-wm-mode (frame-live-p f))
      (dolist (face '(mode-line mode-line-active))
        (set-face-attribute face f :inherit 'teletype-focus
                            :background 'unspecified :foreground 'unspecified
                            :inverse-video 'unspecified))))
  (force-mode-line-update t)
  (redisplay t))

(defun teletype--lighter ()
  "Mode-line text for the frame being drawn."
  (when teletype-wm-mode
    (let ((f (selected-frame)))
      (cond ((eq f (teletype--focused-frame)) " ◆typing here◆")
            ((and (eq f teletype--home) (frame-live-p teletype--target))
             (format " [keys → %s]"
                     (or (frame-parameter teletype--target 'teletype-position) "other")))))))

(defun teletype-set-position (position)
  "Name this frame's place on the desk: `left' or `right'."
  (interactive (list (intern (completing-read "Position: " '("left" "right") nil t))))
  (set-frame-parameter nil 'teletype-position position)
  (teletype--update-indicators))

(defun teletype-focus (position)
  "Send the keyboard to the frame at POSITION.
The keyboard's own frame means home: stop relaying."
  (unless (frame-live-p teletype--home)
    (user-error "teletype: no keyboard frame; run `teletype-wm-start' there"))
  (let ((frame (teletype--frame-at position)))
    (cond
     ((null frame) (message "teletype: no %s monitor" position))
     ((eq frame teletype--home)
      (with-selected-frame teletype--home (teletype--install nil))
      (setq teletype--target nil)
      (message "teletype: keyboard on %s (home)" position))
     (t
      (setq teletype--target frame
            teletype--source teletype--home)
      (with-selected-frame teletype--home (teletype--install t))
      (teletype--print "\n[focus → %s]\n" position)
      (message "teletype: keyboard on %s" position))))
  (teletype--update-indicators))

(defun teletype-focus-left ()  (interactive) (teletype-focus 'left))
(defun teletype-focus-right () (interactive) (teletype-focus 'right))

(defun teletype-focus-toggle ()
  "Toggle the keyboard between home and the other monitor."
  (interactive)
  (if (frame-live-p teletype--target)
      (teletype-focus (frame-parameter teletype--home 'teletype-position))
    (let ((other (seq-find (lambda (f) (and (frame-parameter f 'teletype-position)
                                            (not (eq f teletype--home))))
                           (frame-list))))
      (if other
          (teletype-focus (frame-parameter other 'teletype-position))
        (message "teletype: no other monitor")))))

(defvar teletype-wm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c <left>")  #'teletype-focus-left)
    (define-key map (kbd "C-c <right>") #'teletype-focus-right)
    (define-key map teletype-escape-key #'teletype-focus-toggle)
    map)
  "Focus keys.  Also honoured while relaying: they are never sent across.")

(define-minor-mode teletype-wm-mode
  "Two monitors, one keyboard: move the keyboard's focus between frames."
  :global t
  :keymap teletype-wm-mode-map
  (if teletype-wm-mode
      (add-to-list 'global-mode-string '(:eval (teletype--lighter)) t)
    (when (frame-live-p teletype--home)
      (with-selected-frame teletype--home (teletype--install nil)))
    (setq teletype--target nil)
    (setq global-mode-string (delete '(:eval (teletype--lighter)) global-mode-string)))
  (teletype--update-indicators))

;;;###autoload
(defun teletype-wm-start (&optional position)
  "Make this frame the keyboard's home at POSITION (default `right')."
  (interactive)
  (setq teletype--home (selected-frame))
  (set-frame-parameter nil 'teletype-position (or position 'right))
  (teletype-wm-mode 1)
  (message "teletype: home is %s.  C-c <left>/<right> or C-] to move the keyboard"
           (or position 'right)))

(provide 'teletype)
;;; teletype.el ends here
