#!/usr/bin/env bb
;; zone-health — the whole-box health scan for zone, beyond the XTDB endpoint.
;;
;; Usage:
;;   bb scripts/zone-health.bb            # table; exit 1 if any check fails
;;   bb scripts/zone-health.bb --json     # one JSON object per check
;;   bb scripts/zone-health.bb --only repos-pushed,pass-hubs-agree
;;
;; Every check is a test: it PASSes or FAILs, and a FAIL says what is wrong and
;; what would fix it. Known-open work from the 2026-10-06 drive failure (see
;; ~/README-ZONE-1.1.md) is deliberately left FAILING here rather than skipped,
;; so the scan stays red until each item is actually dealt with. A check whose
;; fix is Joe's call says so in :fix ("Joe: ...").
;;
;; Read-only: the scan never writes to a repo, the pass store, or /mnt/old, and
;; it never pages anyone. Remote checks go over the same ssh route as
;; ~/bin/zone-git-backup and time out after a few seconds.

(require '[babashka.fs :as fs]
         '[babashka.process :as proc]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def home (System/getProperty "user.home"))
(def code-dir (str home "/code"))
(def hubs {"metameso" "ssh://joe@172.236.108.82:2222"
           "lucy"     "ssh://joe@172.236.28.208:2222"})
(def federation-peers {"metameso" "http://172.236.108.82:7070/health"
                       "lucy"     "http://172.236.28.208:7070/health"})
(def repo-exclude #{"storage" "lean4"})           ; same exclusions as zone-git-backup
(def parked-dir (str home "/.config/systemd/enabled-before-outage"))
;; ufw.service is a oneshot that is inactive until a boot runs it; the firewall
;; check reads the loaded rules instead, plus whether the unit is enabled.
(def system-units ["caddy.service" "ssh.service" "smartd.service" "disk-watch.timer"])
(def disk-pct-max 80)
(def disk-watch-max-age-s (* 30 60))
(def disk-temp-max-c 75)
(def backup-max-age-s (* 26 3600))
;; Joe 2026-10-09: a copy made the same day is not overdue. There is no recurring
;; job yet, so this check goes red when the newest copy passes a week.
(def evidence-copy-max-age-s (* 7 86400))
(def recovery-image "/recovery/old-root.img")

;; ---------------------------------------------------------------- plumbing

(defn sh
  "Run cmd; return {:exit :out :err}. Never throws, never prompts."
  [& cmd]
  (try
    (let [r (apply proc/shell {:out :string :err :string :continue true
                               :in "" :timeout 15000}
                   cmd)]
      {:exit (:exit r) :out (str/trim (:out r)) :err (str/trim (:err r))})
    (catch Exception e {:exit -1 :out "" :err (ex-message e)})))

(defn lines [s] (remove str/blank? (str/split-lines (or s ""))))

(defn http-status [url]
  (let [{:keys [exit out]} (sh "curl" "-s" "-o" "/dev/null" "-m" "4" "-w" "%{http_code}" url)]
    (if (zero? exit) (parse-long out) 0)))

(defn now-s [] (quot (System/currentTimeMillis) 1000))

(defn result [status summary & {:as more}]
  (merge {:status status :summary summary} more))

(def pass-r (partial result :pass))
(def fail-r (partial result :fail))

;; ------------------------------------------------------------ pure helpers

(defn disk-watch-verdict
  "Evaluate the latest /var/log/disk-watch.csv row against the alert limits."
  [header row now]
  (let [m (zipmap (str/split header #",") (str/split row #","))
        age (- now (parse-long (m "time")))
        temp (parse-long (m "temp_c"))
        media (parse-long (m "media_errors"))
        crit (parse-long (m "crit_warning"))
        problems (cond-> []
                   (> age disk-watch-max-age-s) (conj (str "last sample " (quot age 60) " min old"))
                   (>= temp disk-temp-max-c) (conj (str temp " °C"))
                   (pos? media) (conj (str media " media errors"))
                   (pos? crit) (conj (str "critical warning " crit)))]
    {:problems problems
     :detail (format "%d °C, %s TB written, %s%% used" temp (m "written_tb") (m "pct_used"))}))

(defn repo-liability
  "Classify one repo's push state. remotes: remote names; zone-only: local
  branches no remote-tracking ref contains; dirty: count of porcelain lines."
  [{:keys [remotes zone-only dirty]}]
  (cond
    (empty? remotes) :no-remote
    (seq zone-only) :unpushed-branches
    (pos? dirty) :uncommitted
    :else nil))

(defn last-log-ok
  "Latest 'TIMESTAMP ok: ...' line of a backup log -> epoch seconds, or nil."
  [log-text]
  (some->> (lines log-text) reverse (filter #(re-find #"^\S+ ok:" %)) first
           (re-find #"^\S+") (java.time.OffsetDateTime/parse)
           (.toEpochSecond)))

(defn hub-relation
  "How a hub's head relates to zone's. known: zone has the hub's commit;
  hub-in-zone / zone-in-hub: ancestry each way."
  [known hub-in-zone zone-in-hub]
  (cond (not known) "has commits zone has never fetched (fetch, then compare)"
        hub-in-zone "is behind zone (push)"
        zone-in-hub "is ahead of zone (pull)"
        :else "has diverged from zone (each side has commits the other lacks)"))

;; ------------------------------------------------------------------ checks

(defn check-user-services []
  (let [expected (->> (fs/list-dir (str parked-dir "/default.target.wants"))
                      (map fs/file-name) sort)
        down (remove #(= "active" (:out (sh "systemctl" "--user" "is-active" %))) expected)]
    (if (empty? down)
      (pass-r (str (count expected) "/" (count expected) " user services active"))
      (fail-r (str (count down) " of " (count expected) " user services not active")
              :items (vec down)
              :fix "systemctl --user status <unit>; journalctl --user -u <unit>"))))

(defn check-system-units []
  (let [down (remove #(= "active" (:out (sh "systemctl" "is-active" %))) system-units)]
    (if (empty? down)
      (pass-r (str/join ", " system-units) )
      (fail-r "system units not active" :items (vec down)))))

(defn check-futon1b []
  (let [codes (into {} (for [port [7072 7073]]
                         [port (http-status (str "http://127.0.0.1:" port "/health"))]))]
    (if (every? #(= 200 %) (vals codes))
      (pass-r "liveness :7072 and main :7073 both 200")
      (fail-r "futon1b /health not 200" :items (mapv (fn [[p c]] (str ":" p " -> " c)) codes)))))

(defn check-agency []
  (let [{:keys [out exit]} (sh "curl" "-s" "-m" "4" "http://127.0.0.1:7070/health")
        body (when (zero? exit) (try (json/parse-string out true) (catch Exception _ nil)))
        degraded (get-in body [:queue-hardening :degraded])]
    (cond
      (not= "ok" (:status body)) (fail-r "Agency :7070 /health not ok" :items [(subs out 0 (min 200 (count out)))])
      (seq degraded) (fail-r "Agency queue hardening degraded" :items (mapv str degraded))
      :else (pass-r (str "status ok, " (:agents body) " agents")))))

(defn check-federation []
  (let [down (for [[peer url] federation-peers
                   :let [c (http-status url)] :when (not= 200 c)]
               (str peer " -> " c))]
    (if (empty? down)
      (pass-r (str "peers reachable: " (str/join ", " (keys federation-peers))))
      (fail-r "federation peers unreachable from zone" :items (vec down)))))

(defn check-firewall []
  (let [{:keys [out]} (sh "sudo" "-n" "ufw" "status" "verbose")
        at-boot (:out (sh "systemctl" "is-enabled" "ufw.service"))]
    (cond
      (not (str/includes? out "Status: active")) (fail-r "ufw not active" :items [out])
      (not= "enabled" at-boot) (fail-r (str "ufw.service is " at-boot ": the firewall will not come back after a reboot"))
      (not (re-find #"Default: deny \(incoming\)" out)) (fail-r "ufw default incoming is not deny")
      (re-find #"(?m)^(7072|7073)\S*\s+ALLOW" out) (fail-r "futon1b ports open in ufw" :fix "Joe opens ports, nobody else")
      :else (pass-r "active, default deny incoming, enabled at boot"))))

(defn check-disk-hardware []
  (let [ls (lines (try (slurp "/var/log/disk-watch.csv") (catch Exception _ "")))
        smart (sh "systemctl" "is-active" "smartd.service")]
    (if (< (count ls) 2)
      (fail-r "no disk-watch samples" :fix "systemctl status disk-watch.timer")
      (let [{:keys [problems detail]} (disk-watch-verdict (first ls) (last ls) (now-s))]
        (if (and (empty? problems) (= "active" (:out smart)))
          (pass-r detail)
          (fail-r "drive health" :items (cond-> problems (not= "active" (:out smart)) (conj "smartd not active"))
                  :detail detail))))))

(defn check-disk-space []
  (let [pct (some->> (:out (sh "df" "--output=pcent" "/")) lines last str/trim (re-find #"\d+") parse-long)]
    (if (and pct (< pct disk-pct-max))
      (pass-r (str "/ at " pct "%"))
      (fail-r (str "/ at " pct "% (limit " disk-pct-max "%)")
              :fix (when (fs/exists? recovery-image)
                     "Joe: delete /recovery/old-root.img (2 TB) once satisfied with the restore")))))

(defn check-old-image-readonly []
  (let [m (:out (sh "findmnt" "-n" "-o" "OPTIONS" "/mnt/old"))]
    (cond
      (str/blank? m) (pass-r "/mnt/old not mounted")
      (re-find #"(^|,)ro(,|$)" m) (pass-r "/mnt/old mounted read-only")
      :else (fail-r "/mnt/old is mounted READ-WRITE" :fix "umount /mnt/old; remount ro,norecovery"))))

(defn check-recovery-image []
  (if (fs/exists? recovery-image)
    (fail-r "2 TB recovery image still on disk"
            :fix "Joe: delete /recovery/old-root.img when satisfied (extract stays on metameso + lucy)")
    (pass-r "recovery image removed")))

(defn repo-state [dir]
  (let [git (fn [& args] (apply sh "git" "-C" (str dir) args))
        remotes (lines (:out (git "remote")))
        branches (lines (:out (git "for-each-ref" "--format=%(refname:short)" "refs/heads")))
        zone-only (vec (filter #(str/blank? (:out (git "branch" "-r" "--contains" %))) branches))]
    {:repo (str (fs/file-name dir)) :remotes remotes :zone-only zone-only
     :dirty (count (lines (:out (git "status" "--porcelain"))))}))

(defn main-checkouts
  "Git checkouts under dir, one per repository: a linked worktree shares its
  branches with the main checkout, so counting both would list the same
  branches twice. Dot-directories (test fixtures) are skipped."
  [dir]
  (->> (fs/list-dir dir)
       (filter #(fs/exists? (fs/path % ".git")))
       (remove #(let [n (str (fs/file-name %))] (or (str/starts-with? n ".") (repo-exclude n))))
       (filter #(fs/directory? (fs/path % ".git")))
       (sort-by str)))

;; Pushing is inbox zero's job (futon-sync check-clean, README-inbox-zero.md):
;; it fetches and checks the manifest repos hourly. The scan checks that inbox
;; zero is actually running and passing, and separately which repos holding
;; zone-only work it does not cover at all.

(defn manifest-paths []
  (let [base (str code-dir "/futon0/data")]
    (->> (:repos (json/parse-string (slurp (str base "/git_sources.json")) true))
         (map #(str (fs/normalize (fs/path base (:path %)))))
         set)))

(defn check-inbox-zero []
  (let [timer (:out (sh "systemctl" "--user" "is-active" "futon-sync-check-clean.timer"))
        props (into {} (for [l (lines (:out (sh "systemctl" "--user" "show" "futon-sync-check-clean.service"
                                                "-p" "Result,ExecMainExitTimestamp" "--timestamp=unix")))]
                         (str/split l #"=" 2)))
        ran (some->> (get props "ExecMainExitTimestamp") (re-find #"\d+") parse-long)]
    (cond
      (not= "active" timer) (fail-r "inbox-zero timer is not running, so nothing checks that repos are pushed"
                                    :fix "Joe: futon-sync-check-clean.timer is one of the parked timers")
      (nil? ran) (fail-r "inbox zero has not run since boot")
      (> (- (now-s) ran) (* 2 3600)) (fail-r (str "inbox zero last ran " (quot (- (now-s) ran) 3600) " h ago"))
      (not= "success" (get props "Result")) (fail-r "inbox zero's last run failed"
                                                     :fix "journalctl --user -u futon-sync-check-clean.service")
      :else (pass-r (str "last run passed " (quot (- (now-s) ran) 60) " min ago")))))

(defn check-inbox-zero-coverage []
  (let [manifest (manifest-paths)
        outside (->> (main-checkouts code-dir)
                     (remove #(manifest (str (fs/normalize %))))
                     (map repo-state))
        bad (for [s outside :let [k (repo-liability s)] :when (#{:no-remote :unpushed-branches} k)]
              (case k
                :no-remote (str (:repo s) ": no remote at all")
                :unpushed-branches (str (:repo s) ": " (count (:zone-only s)) " branch(es) on no remote"
                                        (when (<= (count (:zone-only s)) 3)
                                          (str " " (str/join " " (:zone-only s)))))))]
    (if (empty? bad)
      (pass-r "every repo holding zone-only work is in the inbox-zero manifest")
      (fail-r (str (count bad) " repos hold zone-only work outside inbox zero's manifest")
              :items (vec bad)
              :fix (str "Joe: add each to futon0/data/git_sources.json with a remote, or say it stays "
                        "hub-backup-only (zone-git-backup copies it nightly)")))))

(defn check-git-backup []
  (let [log (str home "/.local/state/zone-git-backup.log")
        t (last-log-ok (try (slurp log) (catch Exception _ "")))
        timer (:out (sh "systemctl" "--user" "is-active" "zone-git-backup.timer"))]
    (cond
      (not= "active" timer) (fail-r "zone-git-backup.timer not active")
      (nil? t) (fail-r "zone-git-backup has never succeeded" :fix (str "see " log))
      (> (- (now-s) t) backup-max-age-s) (fail-r (str "last good hub backup " (quot (- (now-s) t) 3600) " h ago")
                                                 :fix (str "see " log))
      :else (pass-r (str "last good hub backup " (quot (- (now-s) t) 3600) " h ago")))))

;; Inbox zero checks only each manifest repo's default branch, so a side branch
;; that never left zone is invisible to it. Joe 2026-10-09: these stay failing
;; until each is merged, pushed or retired.
(defn summarize-branches
  "One line per branch for a repo with a few; prefix counts for a repo with many
  (apm-lean keeps hundreds of exp/ run branches)."
  [repo described]
  (if (<= (count described) 5)
    (mapv #(str repo " " %) described)
    [(str repo ": " (count described) " branches ("
          (->> described (map #(if-let [[_ p] (re-find #"^([^/]+)/" %)] (str p "/*") %))
               frequencies (sort-by (comp - val))
               (map (fn [[k n]] (str n " " k)))
               (str/join ", "))
          ")")]))

(defn describe-branch [dir b]
  (let [default (not-empty (:out (sh "git" "-C" (str dir) "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
        unique (when default
                 (count (filter #(str/starts-with? % "+")
                                (lines (:out (sh "git" "-C" (str dir) "cherry" default b))))))]
    (str b (cond (nil? default) ""
                 (zero? unique) ": already on the default branch (safe to delete)"
                 :else (str ": " unique " commit(s) not on " default)))))

;; Inbox zero checks only each manifest repo's default branch, so a side branch
;; that never left zone is invisible to it. Joe 2026-10-09: these stay failing
;; until each is merged, pushed or retired.
(defn check-side-branches []
  (let [manifest (manifest-paths)
        per-repo (for [dir (main-checkouts code-dir)
                       :when (manifest (str (fs/normalize dir)))
                       :let [{:keys [repo zone-only]} (repo-state dir)]
                       :when (seq zone-only)]
                   [repo (if (<= (count zone-only) 5)
                           (mapv #(describe-branch dir %) zone-only)
                           zone-only)])
        total (reduce + (map (comp count second) per-repo))]
    (if (zero? total)
      (pass-r "no side branch of a manifest repo lives only on zone")
      (fail-r (str total " side branch(es) of manifest repos on no remote")
              :items (vec (mapcat (fn [[repo ds]] (summarize-branches repo ds)) per-repo))
              :fix "Joe: merge, push, or retire each (zone-git-backup copies them nightly meanwhile)"))))

(defn newest-offbox-evidence
  "Newest off-box copy of the evidence store on a hub: [epoch-s path], or nil.
  Counts the recovery extract and any later backup under ~/backups/evidence."
  [url]
  (let [[_ host port] (re-find #"ssh://([^:]+):(\d+)" url)
        {:keys [out]} (sh "ssh" "-o" "BatchMode=yes" "-o" "ConnectTimeout=5" "-p" port host
                          "find ~/recovery ~/backups/evidence -maxdepth 1 \\( -name 'zone-extract-*' -o -name 'evidence-*' \\) -printf '%T@ %p\\n' 2>/dev/null | sort -n | tail -1")]
    (when-let [[_ t p] (re-find #"^(\d+)\S* (\S+)" out)] [(parse-long t) p])))

(defn check-evidence-backup []
  (let [copies (into {} (for [[h url] hubs] [h (newest-offbox-evidence url)]))
        missing (for [[h c] copies :when (nil? c)] h)
        stale (for [[h [t _]] copies :when (and t (> (- (now-s) t) evidence-copy-max-age-s))]
                (str h ": " (quot (- (now-s) t) 86400) " days old"))
        ages (str/join ", " (for [[h [t p]] copies :when t]
                              (str h " " (quot (- (now-s) t) 3600) " h (" (fs/file-name p) ")")))]
    (cond
      (seq missing) (fail-r "no off-box evidence copy found" :items (vec missing))
      (seq stale) (fail-r "off-box evidence copy older than 7 days" :items (vec stale)
                          :fix "no recurring job yet; basis futon3c/scripts/backup_evidence.sh")
      :else (pass-r ages))))

(defn tmp-worktrees
  "Linked worktrees of repos under code-dir that live in /tmp: [repo path] pairs."
  [code-dir]
  (for [dir (main-checkouts code-dir)
        l (lines (:out (sh "git" "-C" (str dir) "worktree" "list" "--porcelain")))
        :let [[_ p] (re-find #"^worktree (/tmp/.*)$" l)]
        :when (and p (fs/exists? p))]
    [(str (fs/file-name dir)) p]))

;; /tmp is emptied at every boot (tmpfiles "D /tmp") and nothing backs it up. On
;; 2026-10-07 the boot after the drive failure took 278 mfuton worktrees and the
;; uncommitted progress tooling (rowtree.py, ACCEPTED.tsv) with it.
(defn check-work-in-tmp []
  (let [wts (tmp-worktrees code-dir)
        repos (->> (lines (:out (sh "find" "/tmp" "-maxdepth" "3" "-name" ".git" "-user" (System/getProperty "user.name"))))
                   (map #(str (fs/parent %)))
                   (remove (set (map second wts))))]
    (if (and (empty? wts) (empty? repos))
      (pass-r "no git work in /tmp")
      (fail-r (str (+ (count wts) (count repos)) " git checkout(s) in /tmp, which every boot erases")
              :items (vec (concat (map (fn [[r p]] (str r " worktree " p)) wts)
                                  (map #(str "repo " %) repos)))
              :fix "move under ~/worktrees/<repo>/ (git worktree move), or commit, push and remove"))))

(defn check-pass-store []
  (let [store (str home "/.password-store")
        n (count (filter #(str/ends-with? (str %) ".gpg") (file-seq (fs/file store))))
        key? (zero? (:exit (sh "gpg" "--batch" "--list-secret-keys" "E423CC2085636DA3C675702805B0D5246477D771")))
        unpushed (lines (:out (sh "git" "-C" store "log" "--oneline" "@{u}..")))]
    (cond
      (not key?) (fail-r "zone's gpg secret key E423CC20 missing")
      (zero? n) (fail-r "pass store empty or missing")
      (seq unpushed) (fail-r (str n " entries; " (count unpushed) " commit(s) not in zone's bare repo")
                             :items (vec unpushed) :fix "pass git push (after merging with the metameso hub)")
      :else (pass-r (str n " entries, key present, pushed")))))

(defn check-pass-hubs []
  (let [store (str home "/.password-store")
        heads (into {"zone" (:out (sh "git" "-C" store "rev-parse" "HEAD"))}
                    (for [[h url] (select-keys hubs ["metameso"])]
                      [h (first (str/split (:out (sh "git" "ls-remote" (str url home "/git/password-store.git")
                                                     "refs/heads/master"))
                                           #"\s"))]))
        missing (for [[h sha] heads :when (str/blank? sha)] h)
        ;; A hub head zone lacks, or zone lacks the hub's head: the stores diverged.
        diverged (for [[h sha] heads :when (and (not= h "zone") (not (str/blank? sha))
                                                (not= sha (heads "zone")))]
                   (let [git-ok? #(zero? (:exit (apply sh "git" "-C" store %&)))
                         known (git-ok? "cat-file" "-e" (str sha "^{commit}"))]
                     (str h " " (subs sha 0 7) " "
                          (hub-relation known
                                        (and known (git-ok? "merge-base" "--is-ancestor" sha "HEAD"))
                                        (and known (git-ok? "merge-base" "--is-ancestor" "HEAD" sha))))))]
    (cond
      (seq missing) (fail-r "pass hub unreachable" :items (vec missing))
      (seq diverged) (fail-r "pass hubs disagree with zone"
                             :items (vec diverged)
                             :fix "merge the two histories (pass git pull, then pass git push), then make zone a second hub")
      :else (pass-r "zone and metameso pass stores agree"))))

(defn check-parked-timers []
  (let [parked (->> (fs/list-dir (str parked-dir "/timers.target.wants")) (map fs/file-name) sort)
        back (filter #(= "active" (:out (sh "systemctl" "--user" "is-active" %))) parked)
        waiting (remove (set back) parked)]
    (if (empty? waiting)
      (pass-r "every parked timer has been decided")
      (fail-r (str (count waiting) " of " (count parked) " timers still parked since the outage")
              :items (vec waiting)
              :fix (str "Joe: re-enable each, or retire it by removing its symlink from "
                        parked-dir "/timers.target.wants")))))

(def checks
  [["user-services"       "the 18 user services enabled before the outage" check-user-services]
   ["system-units"        "caddy, ssh, ufw, smartd, disk-watch"            check-system-units]
   ["futon1b-health"      "futon1b /health on :7072 and :7073"             check-futon1b]
   ["agency-health"       "Agency :7070 /health"                           check-agency]
   ["federation"          "metameso and lucy Agency reachable"             check-federation]
   ["firewall"            "ufw on, deny incoming by default"              check-firewall]
   ["drive-health"        "disk-watch samples and smartd"                  check-disk-hardware]
   ["disk-space"          "/ below 80%"                                    check-disk-space]
   ["old-image-readonly"  "/mnt/old never writable"                        check-old-image-readonly]
   ["recovery-image"      "2 TB recovery image cleaned up"                 check-recovery-image]
   ["inbox-zero"          "inbox zero running and passing"                 check-inbox-zero]
   ["inbox-zero-coverage" "zone-only work is inside inbox zero's manifest" check-inbox-zero-coverage]
   ["side-branches"       "manifest repos' side branches are on a remote"  check-side-branches]
   ["work-in-tmp"         "no git checkouts in /tmp"                       check-work-in-tmp]
   ["git-hub-backup"      "nightly zone-git-backup ran in the last 26 h"   check-git-backup]
   ["evidence-backup"     "off-box evidence copy on both hubs, under 7 days" check-evidence-backup]
   ["pass-store"          "pass store and key on zone, pushed"             check-pass-store]
   ["pass-hubs-agree"     "zone and metameso pass stores agree"            check-pass-hubs]
   ["parked-timers"       "every parked timer decided by Joe"              check-parked-timers]])

(defn run-check [[id what f]]
  (merge {:id id :what what}
         (try (f) (catch Exception e (fail-r (str "check crashed: " (ex-message e)))))))

(defn print-table [results]
  (doseq [{:keys [id status summary items fix]} results]
    (println (format "%-4s  %-18s  %s" (if (= :pass status) "PASS" "FAIL") id summary))
    (doseq [i items] (println (str "                          - " i)))
    (when fix (println (str "                          fix: " fix))))
  (let [fails (count (filter #(= :fail (:status %)) results))]
    (println (format "\n%d checks, %d pass, %d fail" (count results) (- (count results) fails) fails))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [args (set *command-line-args*)
        only (some->> *command-line-args* (drop-while #(not= "--only" %)) second (#(str/split % #",")) set)
        results (mapv run-check (cond->> checks only (filter #(only (first %)))))]
    (if (args "--json")
      (doseq [r results] (println (json/generate-string r)))
      (print-table results))
    (System/exit (if (some #(= :fail (:status %)) results) 1 0))))
