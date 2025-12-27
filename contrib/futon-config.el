;;; futon-config.el --- User config for Futon tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralize Futon-related customizations here to avoid scattered overrides.

;;; Code:
(add-to-list 'load-path "/home/joe/code/futon0/contrib/")

;; Stack HUD entry point.
(require 'stack-entry)

;; Zoom R4 ingest metadata lives in storage (override here for local tools).
(setenv "ZOOM_SYNC_DEST" "/home/joe/code/storage/zoomr4")
(setenv "ZOOM_SYNC_META_DIR" "/home/joe/code/storage/zoomr4/meta")
(setq arxana-media-index-path "/home/joe/code/storage/zoomr4/meta/zoom_sync_index.json")

(require 'futon-hot)

;; Hot reload defaults (override as needed).
(setq my-chatgpt-shell-hot-reload-files
      '("../futon3/contrib/aob-chatgpt.el"
        "../futon4/dev/arxana-patterns.el"
        "../futon4/dev/arxana-media.el"
        "../futon4/dev/arxana-store.el"
        "../futon4/dev/arxana-docbook.el"
        "../futon4/dev/arxana-lab.el"
        "../futon0/contrib/futon-config.el"
        "../futon0/contrib/stack-entry.el"
        "../futon0/contrib/stack-hud.el"
        "../futon0/contrib/stack-render.el"))

(setq my-chatgpt-shell-hot-reload-include-defaults t)

(my-chatgpt-shell-hot-reload-enable)

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
(setq tatami-start-directory "/home/joe/code/futon1/")
(setq tatami-base-url "http://localhost:8080")
(defvar my-tatami--clojure (or (executable-find "clojure") "clojure"))
(unless (and my-tatami--clojure (file-executable-p my-tatami--clojure))
  (message "Warning: could not locate a runnable clojure executable; tatami auto-start may fail."))
(setq tatami-verbose nil)
(setq tatami-start-command (list my-tatami--clojure "-M:server"))
(setq tatami-startup-wait 20)

;;; Set up Futon4

(setq futon4-base-url "http://localhost:8080/api/alpha")
(setq futon4-enable-sync t)

(load-file "~/code/futon4/dev/bootstrap.el")
(arxana-load)

(provide 'futon-config)

;;; futon-config.el ends here
