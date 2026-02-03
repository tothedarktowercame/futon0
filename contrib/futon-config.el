;;; futon-config.el --- User config for Futon tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralize Futon-related customizations here to avoid scattered overrides.

;;; Code:

;;; Futon 0:

(add-to-list 'load-path "/home/joe/code/futon0/contrib/")

(add-to-list 'load-path "/home/joe/code/futon3/contrib/")

;; Stack HUD entry point.
(require 'stack-entry)

(require 'stack-hud)

(setq stack-hud-services-detail 'names)

;; Zoom R4 ingest metadata lives in storage (override here for local tools).
(setenv "ZOOM_SYNC_DEST" "/home/joe/code/storage/zoomr4")
(setenv "ZOOM_SYNC_META_DIR" "/home/joe/code/storage/zoomr4/meta")
(setq arxana-media-index-path "/home/joe/code/storage/zoomr4/meta/zoom_sync_index.json")

;; Stack mini-HUD documentation

(setq my-chatgpt-shell-stack-doc-popup-mode 'buffer)

(require 'futon-hot)
(require 'subr-x)

;; Hot reload defaults (override as needed).
(setq my-chatgpt-shell-hot-reload-files
      '("../futon3/contrib/flexiarg.el"
	"../futon3/contrib/aob-chatgpt.el"
        "../futon4/dev/bootstrap.el"
        "../futon4/dev/arxana-article.el"
        "../futon4/dev/arxana-browser-docbook.el"
        "../futon4/dev/arxana-patterns.el"
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
        "../futon0/contrib/futon-helper.el"
        "../futon0/contrib/futon-hot.el"
        "../futon0/contrib/hud-service.el"
        "../futon0/contrib/stack-entry.el"
        "../futon0/contrib/stack-hud.el"
        "../futon0/contrib/stack-render.el"))

(setq my-chatgpt-shell-hot-reload-include-defaults t)

(my-chatgpt-shell-hot-reload-refresh t)

(add-hook 'my-chatgpt-shell-hot-reload-after-eval-hook
          #'arxana-reload-after-hot-reload)

(add-hook 'my-chatgpt-shell-hot-reload-after-batch-hook
          #'arxana-reload-after-hot-reload-batch)

(require 'futon-helper)

;;; Futon 3:

(setq tatami-actor "Joe Corneli")
(require 'flexiarg)

;;; Futon 4:

;; Stack/Arxana frame naming (used by sway rules).
(setq my-chatgpt-shell-stack-frame-name "Stack HUD")
(setq my-chatgpt-shell-stack-frame-fullscreen nil)

(setq arxana-patterns-frame-name "Arxana")
(setq arxana-patterns-frame-fullscreen nil)

;; Lab browser root (keeps lab views working across frames).
(setq arxana-lab-root "/home/joe/code/storage/lab")

;;; Set up tatami — my custom integration layer between local futon stack and ChatGPT

(require 'url)
(load-file "~/code/futon1/contrib/tatami.el")
(require 'tatami)


(setq tatami-profile "default")
(setq tatami-data-directory "/home/joe/code/futon1/data/")

(setq tatami-start-directory "/home/joe/code/futon1")
(setq tatami-start-command '("./scripts/run_api.sh"))

(setq tatami-base-url "http://localhost:8080")
(defvar my-tatami--clojure (or (executable-find "clojure") "clojure"))
(unless (and my-tatami--clojure (file-executable-p my-tatami--clojure))
  (message "Warning: could not locate a runnable clojure executable; tatami auto-start may fail."))

(setq tatami-verbose nil)
(setq tatami-startup-wait 20)

;; Piper TTS defaults for transcript_commentary.py.
(setenv "PIPER_MODEL" "/home/joe/code/tts/voices/en_GB-semaine-medium.onnx")
(setenv "PIPER_CONFIG" "/home/joe/code/tts/voices/en_GB-semaine-medium.onnx.json")

;;; Set up Futon4

(setq futon4-base-url "http://localhost:8080/api/alpha")
(setq futon4-enable-sync t)

(load-file "~/code/futon4/dev/bootstrap.el")
(arxana-load)

(provide 'futon-config)

;;; futon-config.el ends here
