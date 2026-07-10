;;; futon-config.el --- User config for Futon tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralize Futon-related customizations here to avoid scattered overrides.

;;; Code:

;;; Futon 0:

(add-to-list 'load-path "/home/joe/code/futon0/contrib/")
(add-to-list 'load-path "/home/joe/code/futon0/emacs/")

(add-to-list 'load-path "/home/joe/code/futon3/contrib/")
(add-to-list 'load-path "/home/joe/code/futon3c/emacs/")
(add-to-list 'load-path "/home/joe/code/futon7/holes/")

;; Stack HUD entry point.
(require 'stack-entry)
(require 'stack-hud)
(setq stack-hud-2-toggle-key "⁂")
(require 'stack-hud-2)

(setq stack-hud-services-detail 'names)

;; Per-session Claude + Codex token burndown (Stack HUD `usage' block + Arxana
;; Browser Sessions headline). See ~/code/algorithms/current-usage-report.md.
(require 'usage-report)
(require 'war-machine)
(require 'pudding-prover)

;; As-needed Agency cleanup: M-x repl-reaper-list / repl-reaper-reap.
(require 'repl-reaper)

;; Human nicknames for agent REPL buffers: M-x agent-nick-set, jump with C-c b.
(require 'agent-nick)

;; `stack-hud-blocks' is a defcustom whose default already includes `usage'.
;; If a previously-saved value pre-dates that addition, splice the block in
;; after `services' so the HUD picks it up without a customize roundtrip.
(unless (cl-find 'usage stack-hud-blocks
                 :key (lambda (b) (plist-get b :key)))
  (let* ((tail (cdr (cl-member 'services stack-hud-blocks
                               :key (lambda (b) (plist-get b :key)))))
         (head (cl-subseq stack-hud-blocks 0
                          (- (length stack-hud-blocks) (length tail)))))
    (setq stack-hud-blocks
          (append head '((:key usage :enabled t)) tail))))

;; Zoom R4 ingest metadata lives in storage (override here for local tools).
(setenv "ZOOM_SYNC_DEST" "/home/joe/code/storage/zoomr4")
(setenv "ZOOM_SYNC_META_DIR" "/home/joe/code/storage/zoomr4/meta")
(setq arxana-media-index-path "/home/joe/code/storage/zoomr4/meta/zoom_sync_index.json")

;; Stack mini-HUD documentation

(setq my-chatgpt-shell-stack-doc-popup-mode 'buffer)

(require 'futon-hot)
(require 'subr-x)
(require 'futon-buffer-cleaner)
(unless (boundp 'futon-buffer-cleaner-stream-buffer-regexp)
  (load "futon-buffer-cleaner" nil nil))

;; Hot reload defaults (override as needed).
(setq my-chatgpt-shell-hot-reload-files
      '("../futon0/contrib/futon-buffer-cleaner.el"
        "../futon3/contrib/flexiarg.el"
	"../futon3/contrib/aob-chatgpt.el"
        "../futon3c/emacs/agent-chat.el"
        "../futon3c/emacs/agent-follow-mode.el"
        "../futon3c/emacs/agent-mission-control.el"
        "../futon3c/emacs/claude-repl.el"
        "../futon3c/emacs/codex-repl.el"
        "../futon3c/emacs/futon3c-code-blocks.el"
        "../futon3c/emacs/smart-cursor.el"
        "../futon3c/emacs/futon-agency-ws.el"
        "../futon4/dev/bootstrap.el"
        "../futon4/dev/arxana-article.el"
        "../futon4/dev/arxana-browser-docbook.el"
        "../futon4/dev/arxana-patterns.el"
        "../futon4/dev/arxana-browser-trace.el"
        "../futon4/dev/arxana-browser-core.el"
        "../futon4/dev/arxana-browser-code.el"
        "../futon4/dev/arxana-browser-patterns.el"
        "../futon4/dev/arxana-browser-patterns-hud.el"
        "../futon4/dev/arxana-browser-forum.el"
        "../futon4/dev/arxana-browser.el"
        "../futon4/dev/arxana-docbook-core.el"
        "../futon4/dev/arxana-media.el"
        "../futon4/dev/arxana-store.el"
        "../futon4/dev/arxana-docbook.el"
        "../futon4/dev/arxana-docbook-ui.el"
        "../futon4/dev/arxana-docbook-checkout.el"
        "../futon4/dev/arxana-docbook-remote.el"
        "../futon4/dev/arxana-docbook-toc.el"
        "../futon4/dev/arxana-links.el"
        "../futon4/dev/arxana-org-links.el"
        "../futon4/dev/arxana-relations.el"
        "../futon4/dev/arxana-scholium.el"
        "../futon4/dev/arxana-derivation.el"
        "../futon4/dev/arxana-saving.el"
        "../futon4/dev/arxana-inclusion.el"
        "../futon4/dev/arxana-import.el"
        "../futon4/dev/arxana-articles-export.el"
        "../futon4/dev/arxana-compat.el"
        "../futon4/dev/arxana-ui.el"
        "../futon4/dev/arxana-xtdb-browse.el"
        "../futon4/dev/arxana-lab.el"
        "../futon0/contrib/futon-config.el"
        "../futon0/contrib/loop-lag.el"
        "../futon0/contrib/futon-helper.el"
        "../futon0/contrib/futon-hot.el"
        "../futon0/contrib/hud-service.el"
        "../futon0/contrib/stack-entry.el"
        "../futon0/contrib/stack-hud.el"
        "../futon0/contrib/stack-hud-2.el"
        "../futon0/contrib/stack-render.el"
        "../futon0/contrib/usage-report.el"
        "../futon0/emacs/joe-hud.el"
        "../futon0/emacs/war-machine.el"
        "../futon7/holes/pudding-prover.el"
	"../futon4/dev/arxana-browser-songs.el"
	"../futon4/dev/arxana-browser-chorus.el"
	"../futon4/dev/arxana-browser-essays.el"
	"../futon4/dev/arxana-browser-essays-compiled.el"
	"../futon4/dev/arxana-browser-essays-wikibooks.el"
	"../futon3c/emacs/mission-mode.el"
	"../futon3c/emacs/session-overview.el"
	"../futon3c/emacs/session-mode.el"))

(setq my-chatgpt-shell-hot-reload-include-defaults t)

(my-chatgpt-shell-hot-reload-refresh t)

(add-hook 'my-chatgpt-shell-hot-reload-after-eval-hook
          #'arxana-reload-after-hot-reload)

(add-hook 'my-chatgpt-shell-hot-reload-after-batch-hook
          #'arxana-reload-after-hot-reload-batch)

(futon-buffer-cleaner-enable)

(require 'futon-helper)
(require 'drawbridge-eval)

;; Event-loop lag indicator: mode-line light + `loop-lag-report'.  Retrospective
;; heartbeat that measures how long the Emacs main thread was blocked (e.g. by
;; synchronous Drawbridge/emacsclient calls during turn finalisation).
(require 'loop-lag)
(loop-lag-mode 1)

;; Memory pressure: gcmh raises gc-cons-threshold to its *high* value during
;; activity, so on a shared 30GB box (Firefox ~15GB + JVM ~5GB) the Emacs heap
;; balloons toward ~2GB precisely during long agent turns and gets swapped —
;; then faulting those pages back in (GC, process filters) is the multi-second
;; stall loop-lag reports.  Cap the active-burst threshold so bursts stay
;; bounded while GC still mostly stays out of the typing path.
(with-eval-after-load 'gcmh
  (setq gcmh-high-cons-threshold (* 256 1024 1024)))
(when (boundp 'gcmh-high-cons-threshold)
  (setq gcmh-high-cons-threshold (* 256 1024 1024)))

;;; Futon 3:

(require 'flexiarg)

;;; Futon 3c:

(require 'agent-chat)
(require 'agent-follow-mode)
(require 'agent-mission-control)
(require 'claude-repl)
(require 'codex-repl)
(require 'futon3c-code-blocks)
(require 'smart-cursor)
(require 'futon-branches)
(require 'mission-mode)   ; live mission scope view (M-x mission-mode) — donor for session-mode
(require 'session-overview)   ; live session weak-scope view (M-x session-overview)
(require 'session-mode)       ; deterministic NNexus markup of the live buffer (M-x session-mode)

;; Shared Agency WS observer (single socket, one ordered reader): HUD +
;; completion bubbles + park-ready fast-path.  Connects as the `emacs-hud'
;; observer (broadcast-only, not an invocable agent — see
;; futon3c/holes/excursions/E-agency-ws-cutover.md).  Guarded so a missing
;; server or websocket package at init never aborts the rest of this config;
;; the connector's own reconnect timer handles a not-yet-up :7070.
(require 'futon-agency-ws)
(with-demoted-errors "futon-agency-ws enable: %S"
  (futon-agency-hud-enable))

;;; Futon 4:

;; Stack/Arxana frame naming (used by sway rules).
(setq my-chatgpt-shell-stack-frame-name "Stack HUD")
(setq my-chatgpt-shell-stack-frame-fullscreen nil)

(setq arxana-patterns-frame-name "Arxana")
(setq arxana-patterns-frame-fullscreen nil)

;; Lab browser root (keeps lab views working across frames).
(setq arxana-lab-root "/home/joe/code/storage/lab")

;; Piper TTS defaults for transcript_commentary.py.
(setenv "PIPER_MODEL" "/home/joe/code/tts/voices/en_GB-semaine-medium.onnx")
(setenv "PIPER_CONFIG" "/home/joe/code/tts/voices/en_GB-semaine-medium.onnx.json")

;;; Set up Futon4

(let* ((raw-base (or (getenv "FUTON4_BASE_URL")
                     "http://localhost:7071/api/alpha"))
       (base (replace-regexp-in-string "/+$" "" raw-base)))
  (setq futon4-base-url
        (cond
         ((string-match-p "/api/alpha\\'" base) base)
         ((string-match-p "/api\\'" base) (concat base "/alpha"))
         (t (concat base "/api/alpha")))))
(setq futon4-enable-sync t)
(setq arxana-store-default-penholder
      (or (getenv "FUTON4_PENHOLDER")
          (getenv "FUTON1A_COMPAT_PENHOLDER")
          "api"))

(load-file "~/code/futon4/dev/bootstrap.el")
(arxana-load)

;; Reazon-backed window/data invariants are OPT-IN (M-x arxana-window-constraints-mode),
;; like loop-lag-mode.  Left OFF by default (2026-07-02): forcing them on ran
;; arxana-ui-refresh's Reazon validation on every *global*
;; window-configuration-change — a logic query per window change — causing
;; multi-second redisplay freezes even when Arxana wasn't in use.  Keep the
;; strict failure-action preference for when the mode IS turned on.
(setq arxana-window-constraints-failure-action 'error
      arxana-data-constraints-failure-action 'error)

(setq arxana-lab-futon1-server "http://localhost:7071/api/alpha")
(setq arxana-evidence-server   "http://localhost:7071/api/alpha")

(global-set-key (kbd "※") #'arxana-browser-home)

(provide 'futon-config)

;;; futon-config.el ends here
