#!/usr/bin/env bb
;; FutonZero — Capability Monitor for the Self-Representing Stack
;;
;; Reads futon1a hyperedges and futon3c evidence to track capability
;; trajectories per agent. Grounded in Sen's capability approach.
;;
;; Usage: bb futonzero.clj <command> [args]
;; See:   bb futonzero.clj --help

(require '[futon0.futonzero.cli :as cli])

(apply cli/-main *command-line-args*)
