#!/usr/bin/env bb
;; futon-sync — Multi-repo git hygiene for the futon stack.
;;
;; Reads the repo manifest from futon0/data/git_sources.json and provides:
;;   futon-sync          — status dashboard (default)
;;   futon-sync status   — same as above
;;   futon-sync review   — show dirty/untracked file details for cleanup triage
;;   futon-sync pull     — bulk pull --rebase --autostash
;;   futon-sync push     — push all repos ahead of origin (with confirmation)
;;   futon-sync park     — stash dirty/untracked changes across repos
;;     --dry-run         — preview what would be stashed
;;     --yes             — skip confirmation prompt
;;     --message TEXT    — explicit stash message
;;   futon-sync hygiene  — detect noisy untracked files, suggest .gitignore
;;     --fix             — apply suggestions automatically

(require '[babashka.fs :as fs]
         '[babashka.process :as proc]
         '[cheshire.core :as json]
         '[clojure.string :as str])

;; ── Manifest ─────────────────────────────────────────────────────────────────

(def manifest-path
  (str (fs/path (fs/parent (fs/parent (fs/real-path *file*)))
                "data" "git_sources.json")))

(defn load-repos []
  (let [base (fs/parent manifest-path)
        data (json/parse-string (slurp manifest-path) true)]
    (->> (:repos data)
         (map (fn [r]
                (let [abs (str (fs/normalize (fs/path base (:path r))))]
                  (assoc r :abs-path abs))))
         (filter #(fs/exists? (str (fs/path (:abs-path %) ".git"))))
         vec)))

;; ── Noise patterns ───────────────────────────────────────────────────────────

(def noise-patterns
  [{:match #"\.elc$"          :suggest "*.elc"}
   {:match #"^\.shadow-cljs/" :suggest ".shadow-cljs/"}
   {:match #"^\.venv/"        :suggest ".venv/"}
   {:match #"^\.env"          :suggest ".env*"}
   {:match #"\.tar\.gz$"      :suggest "*.tar.gz"}
   {:match #"^mo-processed/"  :suggest "mo-processed/"}
   {:match #"^test.*\.pdf$"   :suggest "test*.pdf"}
   {:match #"\.ppm$"          :suggest "*.ppm"}
   {:match #"^out/"           :suggest "out/"}
   {:match #"^lab/"           :suggest "lab/"}
   {:match #"^__pycache__/"   :suggest "__pycache__/"}
   {:match #"^\.entangled/"   :suggest ".entangled/"}
   {:match #"^error$"         :suggest "error"}
   {:match #"^\.lsp/"         :suggest ".lsp/"}
   {:match #"^\.clj-kondo/"   :suggest ".clj-kondo/"}])

(defn noisy? [filename]
  (some (fn [{:keys [match suggest]}]
          (when (re-find match filename) suggest))
        noise-patterns))

;; ── ANSI helpers ─────────────────────────────────────────────────────────────

(def ^:private ansi-reset  "\033[0m")
(def ^:private ansi-bold   "\033[1m")
(def ^:private ansi-dim    "\033[2m")
(def ^:private ansi-green  "\033[32m")
(def ^:private ansi-yellow "\033[33m")
(def ^:private ansi-red    "\033[31m")
(def ^:private ansi-cyan   "\033[36m")

(defn- c [color s] (str color s ansi-reset))

;; ── Git helpers ──────────────────────────────────────────────────────────────

(defn- git [repo-path & args]
  (let [r (apply proc/shell {:dir repo-path :out :string :err :string
                              :continue true}
                  "git" args)]
    {:exit (:exit r) :out (str/trim (:out r)) :err (str/trim (:err r))}))

(defn- parse-branch [porcelain-header]
  ;; "## main...origin/main [ahead 3]" or "## master" (no tracking)
  (let [line (or porcelain-header "")]
    (second (re-find #"^## ([^\s.]+)" line))))

(defn- parse-status-entry [line]
  (let [code (subs line 0 2)
        path (subs line 3)]
    {:code code :path path :raw line}))

(defn- repo-status [repo]
  (let [path (:abs-path repo)
        st (git path "status" "--porcelain=v1" "-b")
        lines (str/split-lines (:out st))
        header (first lines)
        entries (rest lines)
        branch (parse-branch header)
        ;; ahead/behind from rev-list
        ab (git path "rev-list" "--left-right" "--count" "HEAD...@{u}")
        [ahead behind] (if (zero? (:exit ab))
                         (mapv #(Integer/parseInt %) (str/split (str/trim (:out ab)) #"\t"))
                         [0 0])
        no-remote? (not (zero? (:exit ab)))
        ;; classify entries
        dirty (mapv parse-status-entry
                    (filterv #(not (str/starts-with? % "??")) entries))
        dirty-files (mapv :path dirty)
        untracked (mapv #(subs % 3) (filterv #(str/starts-with? % "??") entries))
        noisy-files (filterv noisy? untracked)]
    (assoc repo
           :branch (or branch "?")
           :ahead ahead :behind behind :no-remote no-remote?
           :dirty-count (count dirty)
           :dirty-files dirty-files
           :dirty-entries dirty
           :untracked untracked
           :untracked-count (count untracked)
           :noisy-files noisy-files
           :noisy-suggestions (into #{} (keep noisy?) noisy-files)
           :clean? (and (zero? (count dirty))
                        (zero? (count untracked))
                        (zero? ahead) (zero? behind)))))

;; ── Status command ───────────────────────────────────────────────────────────

(defn- sync-str [{:keys [ahead behind no-remote]}]
  (cond
    no-remote                       (c ansi-dim "??")
    (and (pos? ahead) (pos? behind)) (c ansi-red (str "↑" ahead "↓" behind))
    (pos? ahead)                     (c ansi-yellow (str "↑" ahead))
    (pos? behind)                    (c ansi-cyan (str "↓" behind))
    :else                            (c ansi-green "=")))

(defn- dirty-str [{:keys [dirty-count]}]
  (if (zero? dirty-count)
    (c ansi-dim "0")
    (c ansi-yellow (str dirty-count))))

(defn- untracked-str [{:keys [untracked-count]}]
  (if (zero? untracked-count)
    (c ansi-dim "0")
    (c ansi-yellow (str untracked-count))))

(defn- noisy-str [{:keys [noisy-files noisy-suggestions]}]
  (if (empty? noisy-files)
    (c ansi-dim "0")
    (str (c ansi-red (str (count noisy-files)))
         (c ansi-dim (str " (" (str/join "," noisy-suggestions) ")")))))

(defn cmd-status [repos]
  (let [now (java.time.LocalDateTime/now)
        fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")
        statuses (mapv repo-status repos)
        sep "───────────────────────────────────────────────────────────────────"]
    (println (str ansi-bold "futon stack status" ansi-reset
                  (c ansi-dim (str " — " (.format now fmt)))))
    (println sep)
    (printf " %-14s %-10s %-10s %-7s %-11s %s%n"
            "Repo" "Branch" "Sync" "Dirty" "Untracked" "Noisy")
    (println sep)
    (doseq [s statuses]
      (printf " %-14s %-10s %-10s %-7s %-11s %s%n"
              (:label s) (:branch s) (sync-str s)
              (dirty-str s) (untracked-str s) (noisy-str s)))
    (println sep)
    (let [n-repos (count statuses)
          n-ahead (count (filter #(pos? (:ahead %)) statuses))
          n-behind (count (filter #(pos? (:behind %)) statuses))
          n-dirty (count (filter #(pos? (:dirty-count %)) statuses))
          n-noisy (count (filter #(seq (:noisy-files %)) statuses))]
      (printf " %d repos" n-repos)
      (when (pos? n-ahead)  (printf "  |  %s ahead" (c ansi-yellow (str n-ahead))))
      (when (pos? n-behind) (printf "  |  %s behind" (c ansi-cyan (str n-behind))))
      (when (pos? n-dirty)  (printf "  |  %s dirty" (c ansi-yellow (str n-dirty))))
      (when (pos? n-noisy)  (printf "  |  %s noisy" (c ansi-red (str n-noisy))))
      (println))))

;; ── Review command ───────────────────────────────────────────────────────────

(defn- print-file-list [title files color]
  (when (seq files)
    (println (str "  " (c ansi-bold title)))
    (doseq [f files]
      (println (str "    " (c color f))))))

(defn cmd-review [repos]
  (let [statuses (->> repos
                      (map repo-status)
                      (filter #(or (pos? (:dirty-count %))
                                   (pos? (:untracked-count %))
                                   (seq (:noisy-files %))))
                      vec)]
    (if (empty? statuses)
      (println (c ansi-green "All repos clean — nothing to review."))
      (do
        (println (str ansi-bold "futon-sync review" ansi-reset))
        (println)
        (doseq [s statuses]
          (println (format "%s  %s  %s"
                           (c ansi-bold (:label s))
                           (c ansi-dim (str "[" (:branch s) "]"))
                           (c ansi-dim (str (fs/file-name (:abs-path s))))))
          (println (format "  dirty=%s untracked=%s noisy=%s sync=%s"
                           (dirty-str s)
                           (untracked-str s)
                           (noisy-str s)
                           (sync-str s)))
          (print-file-list "Modified / staged" (:dirty-files s) ansi-yellow)
          (print-file-list "Untracked" (:untracked s) ansi-cyan)
          (when (seq (:noisy-files s))
            (println (str "  " (c ansi-bold "Noisy suggestions")))
            (doseq [f (:noisy-files s)]
              (println (format "    %s → %s"
                               (c ansi-red f)
                               (c ansi-cyan (noisy? f))))))
          (println))
        (println (c ansi-dim "Use `park` to stash work in progress or `hygiene --fix` for ignoreable noise."))))))

;; ── Park command ─────────────────────────────────────────────────────────────

(defn- default-park-message []
  (str "futon-sync park "
       (.format (java.time.LocalDateTime/now)
                (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))))

(defn- parse-park-message [args]
  (loop [remaining args]
    (when-let [x (first remaining)]
      (cond
        (= x "--message") (second remaining)
        :else (recur (rest remaining))))))

(defn cmd-park [repos {:keys [dry-run? yes? message]}]
  (let [statuses (mapv repo-status repos)
        parkable (filterv #(or (pos? (:dirty-count %))
                               (pos? (:untracked-count %)))
                          statuses)
        stash-message (or message (default-park-message))]
    (if (empty? parkable)
      (println (c ansi-green "All repos already clean — nothing to stash."))
      (do
        (println (str ansi-bold "futon-sync park" ansi-reset))
        (println (str " message: " (c ansi-dim stash-message)))
        (println)
        (doseq [s parkable]
          (printf " %-14s dirty=%-3d untracked=%-3d %s%n"
                  (:label s)
                  (:dirty-count s)
                  (:untracked-count s)
                  (c ansi-dim (:branch s))))
        (println)
        (if dry-run?
          (println (c ansi-dim "Dry run only — no stashes created."))
          (let [proceed? (if yes?
                           true
                           (do
                             (printf "Stash %d repo%s? [y/N] "
                                     (count parkable)
                                     (if (= 1 (count parkable)) "" "s"))
                             (flush)
                             (= "y" (str/lower-case (str/trim (or (read-line) ""))))))]
            (if-not proceed?
              (println "Aborted.")
              (doseq [s parkable]
                (let [r (git (:abs-path s) "stash" "push" "--include-untracked" "-m" stash-message)]
                  (if (zero? (:exit r))
                    (printf " %-14s %s %s%n"
                            (:label s)
                            (c ansi-green "✓ Stashed")
                            (c ansi-dim (or (first (str/split-lines (:out r))) "")))
                    (printf " %-14s %s %s%n"
                            (:label s)
                            (c ansi-red "✗ Failed:")
                            (c ansi-dim (first (str/split-lines (:err r)))))))))))))))

;; ── Pull command ─────────────────────────────────────────────────────────────

(defn cmd-pull [repos]
  (println (str ansi-bold "futon-sync pull" ansi-reset))
  (doseq [repo repos]
    (let [path (:abs-path repo)
          label (:label repo)
          ;; Check if there's a remote tracking branch
          check (git path "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")]
      (if (not (zero? (:exit check)))
        (printf " %-14s %s%n" label (c ansi-dim "skipped (no upstream)"))
        (let [r (git path "pull" "--rebase" "--autostash")]
          (if (zero? (:exit r))
            (let [msg (cond
                        (str/includes? (:out r) "Already up to date")
                        (c ansi-green "✓ Already up to date")

                        (str/includes? (:out r) "autostash")
                        (c ansi-green "✓ Pulled (autostash applied)")

                        :else
                        (c ansi-green (str "✓ " (first (str/split-lines (:out r))))))]
              (printf " %-14s %s%n" label msg))
            (printf " %-14s %s %s%n" label
                    (c ansi-red "✗ Failed:")
                    (c ansi-dim (first (str/split-lines (:err r)))))))))))

;; ── Push command ─────────────────────────────────────────────────────────────

(defn cmd-push [repos]
  (let [statuses (mapv repo-status repos)
        pushable (filterv #(and (pos? (:ahead %)) (not (:no-remote %))) statuses)]
    (if (empty? pushable)
      (println (c ansi-green "All repos up to date with origin."))
      (do
        (println (str ansi-bold "futon-sync push" ansi-reset))
        (doseq [s pushable]
          (let [path (:abs-path s)
                upstream (git path "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")
                target (if (zero? (:exit upstream)) (:out upstream) "origin/?")]
            (printf " %-14s %d commit%s → %s%n"
                    (:label s) (:ahead s)
                    (if (= 1 (:ahead s)) "" "s")
                    target)))
        (printf "%nPush %d repo%s? [y/N] " (count pushable)
                (if (= 1 (count pushable)) "" "s"))
        (flush)
        (let [answer (str/trim (or (read-line) ""))]
          (if (= (str/lower-case answer) "y")
            (doseq [s pushable]
              (let [r (git (:abs-path s) "push")]
                (if (zero? (:exit r))
                  (printf " %-14s %s%n" (:label s) (c ansi-green "✓ Pushed"))
                  (printf " %-14s %s %s%n" (:label s)
                          (c ansi-red "✗ Failed:")
                          (c ansi-dim (first (str/split-lines (:err r))))))))
            (println "Aborted.")))))))

;; ── Hygiene command ──────────────────────────────────────────────────────────

(defn cmd-hygiene [repos {:keys [fix?]}]
  (let [statuses (mapv repo-status repos)
        ;; Collect {repo-path #{suggestions}}
        suggestions (into {}
                      (comp
                        (filter #(seq (:noisy-suggestions %)))
                        (map (fn [s] [s (:noisy-suggestions s)])))
                      statuses)]
    (if (empty? suggestions)
      (println (c ansi-green "All repos clean — no noisy untracked files."))
      (do
        (println (str ansi-bold "futon-sync hygiene" ansi-reset))
        (println)
        (doseq [[s suggs] suggestions]
          (doseq [f (:noisy-files s)]
            (let [sug (noisy? f)]
              (printf " %-14s %-40s → suggest: %s%n"
                      (str (:label s) ":")
                      f (c ansi-cyan sug)))))
        (println)
        (if fix?
          ;; Auto-apply
          (doseq [[s suggs] suggestions]
            (let [gi-path (str (fs/path (:abs-path s) ".gitignore"))
                  existing (if (fs/exists? gi-path) (slurp gi-path) "")
                  existing-lines (into #{} (str/split-lines existing))
                  new-lines (remove existing-lines suggs)]
              (if (empty? new-lines)
                (printf " %-14s %s%n" (:label s) (c ansi-dim "already in .gitignore"))
                (do
                  (spit gi-path (str existing
                                     (when-not (str/ends-with? existing "\n") "\n")
                                     (str/join "\n" (sort new-lines))
                                     "\n"))
                  (printf " %-14s %s %s%n" (:label s)
                          (c ansi-green "✓ Added to .gitignore:")
                          (str/join ", " (sort new-lines)))))))
          ;; Just report
          (println (c ansi-dim "Run with --fix to apply suggestions.")))))))

;; ── Main ─────────────────────────────────────────────────────────────────────

(let [args *command-line-args*
      cmd (or (first args) "status")
      repos (load-repos)]
  (case cmd
    ("status" "st") (cmd-status repos)
    ("review" "rv") (cmd-review repos)
    "pull"          (cmd-pull repos)
    "push"          (cmd-push repos)
    "park"          (cmd-park repos {:dry-run? (some #{"--dry-run"} args)
                                     :yes? (some #{"--yes"} args)
                                     :message (parse-park-message args)})
    "hygiene"       (cmd-hygiene repos {:fix? (some #{"--fix"} args)})
    (do (println (str "Unknown command: " cmd))
        (println "Usage: futon-sync [status|review|pull|push|park [--dry-run] [--yes] [--message TEXT]|hygiene [--fix]]")
        (System/exit 1))))
