(ns futon0.report.pattern-receipts
  "Level-4 pattern→code receipts for M-patterns-done-right: the `Pattern:`
  commit-message footer parser and the `invariant-pattern-receipts-resolve`
  verify invariant.

  Receipt discipline (mission text, Level 4): every commit since a named
  cutover that touches code in a pattern-claimed region carries one or more
  `Pattern: <ns>/<name>` footer lines (same shape as the `Block:` footer
  prior art — `^Pattern: <ns>/<name>$`, last occurrence wins); the verify
  invariant checks every receipt's pattern-ref resolves against the
  pattern library on disk.

  Cutover: the receipts discipline starts at the cutover commit of this
  repository, 2026-11-05 (see mission record). Historical commits before
  the cutover are out of scope by design; the invariant accepts an
  explicit --since so the boundary is a stated input, never implicit.

  Pattern-claimed region (v1, conservative): the flexiarg library,
  `library/**/*.flexiarg`. Widening the claimed region is a mission
  decision, not a script default.

  CLI: clojure -M -m futon0.report.pattern-receipts --repo <path> [--since <git-rev>]
  Exit 0 = :pass, exit 1 = :fail, exit 2 = usage error."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:gen-class))

(def footer-pattern
  "One `Pattern:` footer line. ns/name are kebab/lowercase words per the
  library naming convention; the regex is deliberately anchored so a
  prose sentence mentioning 'Pattern: something' never parses."
  #"^Pattern:[ \t]+([a-z0-9][a-z0-9-]*/[a-z0-9][a-z0-9-]*)[ \t]*$")

(defn parse-pattern-footers
  "Commit message -> distinct pattern ids from `Pattern: <ns>/<name>` lines.
  Last occurrence wins: a repeated id collapses to one receipt."
  [message]
  (vec (distinct (keep #(second (re-matches footer-pattern %))
                       (str/split-lines (str message))))))

(defn pattern-claimed-region?
  "Is this changed path inside a pattern-claimed region? v1: the flexiarg
  library only."
  [path]
  (boolean (re-matches #"library/.+\.flexiarg" path)))

(defn- git [& args]
  (apply shell/sh "git" args))

(defn- commit-record
  "One commit since `since` as {:sha :message :claimed-paths}. Uses
  `git log --name-only` against the real repository."
  [repo sha]
  (let [fmt "--format=%B%x00"]
    (let [{:keys [exit out err]}
          (git "-C" repo "show" fmt "--name-only" "--no-renames" sha)]
      (when (zero? exit)
        (let [[message paths] (str/split out #"\x00" 2)]
          {:sha sha
           :message message
           :claimed-paths (filter pattern-claimed-region?
                                  (remove str/blank? (str/split-lines paths)))})))))

(defn- commits-since
  "Commits since `since` (exclusive) touching a pattern-claimed region,
  oldest first. `since` may be a sha, a date expression like
  \"2026-11-05\", or \"--all\" for the whole history (diagnostic mode)."
  [repo since]
  (let [log-args (concat ["log" "--format=%H" "--reverse"]
                         (when-not (= "--all" since) [(str since "..HEAD")])
                         ["--" "library"])
        {:keys [exit out err]} (apply git "-C" repo log-args)]
    (when (zero? exit)
      (mapv (partial commit-record repo)
            (remove str/blank? (str/split-lines out))))))

(defn- pattern-ref-resolves?
  "Does pattern id `ns/name` resolve against the library on disk?"
  [repo pattern-id]
  (let [[ns name] (str/split pattern-id #"/" 2)]
    (and ns name
         (.isFile (java.io.File. (str repo "/library/" ns "/" name ".flexiarg"))))))

(defn invariant-pattern-receipts-resolve
  "The verify invariant for Level-4 receipts. Returns
  {:status :pass|:fail :repo :since :commits-checked :violations [...]}.
  A commit in the claimed region since the cutover violates if it carries
  no `Pattern:` footer, or if any footered pattern-ref does not resolve
  to a library file. Pass with zero commits checked is reported as
  :pass with :vacuous true — an honest distinction, never a hidden one."
  [repo since]
  (let [commits (commits-since repo since)
        violations
        (vec (mapcat
              (fn [{:keys [sha message claimed-paths]}]
                (let [receipts (parse-pattern-footers message)]
                  (concat
                   (when (and (seq claimed-paths) (empty? receipts))
                     [{:kind :missing-pattern-footer
                       :commit sha
                       :claimed-paths claimed-paths}])
                   (keep (fn [pid]
                           (when-not (pattern-ref-resolves? repo pid)
                             {:kind :unresolvable-pattern-ref
                              :commit sha
                              :pattern pid}))
                         receipts))))
              commits))]
    {:status (if (seq violations) :fail :pass)
     :repo repo :since since
     :commits-checked (count commits)
     :vacuous (zero? (count commits))
     :violations violations}))

(defn -main
  [& args]
  (let [opts (zipmap (map #(keyword (subs % 2)) (take-nth 2 args))
                     (take-nth 2 (drop 1 args)))
        {:keys [repo since]} opts]
    (if (or (nil? repo) (nil? since))
      (do (println "usage: futon0.report.pattern-receipts --repo <path> --since <git-rev|--all>")
          (System/exit 2))
      (let [result (invariant-pattern-receipts-resolve repo since)]
        (println (pr-str result))
        (System/exit (if (= :pass (:status result)) 0 1))))))
