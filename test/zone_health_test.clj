(require '[babashka.fs :as fs]
         '[babashka.process :as proc]
         '[clojure.string :as str]
         '[clojure.test :refer [deftest is run-tests testing]])

(load-file (str (fs/path (fs/parent (fs/parent (fs/real-path *file*)))
                         "scripts" "zone-health.bb")))

(def header "time,temp_c,written_tb,power_on_h,media_errors,pct_used,crit_warning,warn_temp_min")

(deftest disk-watch-verdict-catches-each-alert
  (let [now 1791507913]
    (testing "a fresh, cool, clean sample passes"
      (is (empty? (:problems (disk-watch-verdict header "1791507913,55,3.17,17,0,0,0,0" now)))))
    (testing "the 75 C alert threshold is inclusive, matching disk-watch"
      (is (= ["75 °C"] (:problems (disk-watch-verdict header "1791507913,75,3.17,17,0,0,0,0" now)))))
    (testing "media errors and critical warnings each fail"
      (is (= ["2 media errors"] (:problems (disk-watch-verdict header "1791507913,55,3.17,17,2,0,0,0" now))))
      (is (= ["critical warning 4"] (:problems (disk-watch-verdict header "1791507913,55,3.17,17,0,0,4,0" now)))))
    (testing "a stopped disk-watch timer shows up as a stale sample"
      (is (= ["last sample 60 min old"]
             (:problems (disk-watch-verdict header "1791504313,55,3.17,17,0,0,0,0" now)))))))

(deftest repo-liability-classes
  (is (= :no-remote (repo-liability {:remotes [] :zone-only [] :dirty 0})))
  (is (= :unpushed-branches (repo-liability {:remotes ["origin"] :zone-only ["main"] :dirty 0})))
  (is (= :uncommitted (repo-liability {:remotes ["origin"] :zone-only [] :dirty 3})))
  (is (nil? (repo-liability {:remotes ["origin"] :zone-only [] :dirty 0}))))

(deftest last-log-ok-takes-the-latest-success-only
  (let [log (str "2026-10-07T02:30:00+00:00 ok: 18 repos backed up to 2 hubs\n"
                 "2026-10-08T02:30:00+00:00 FAIL: lucy unreachable\n")]
    (testing "a later failure does not count as a success"
      (is (= (.toEpochSecond (java.time.OffsetDateTime/parse "2026-10-07T02:30:00+00:00"))
             (last-log-ok log)))))
  (is (nil? (last-log-ok "")))
  (is (nil? (last-log-ok "2026-10-08T02:30:00+00:00 FAIL: everything\n"))))

(deftest hub-relation-names-divergence
  (testing "the 2026-10-09 pass store: each side had commits the other lacked"
    (is (str/includes? (hub-relation true false false) "diverged")))
  (is (str/includes? (hub-relation true true false) "behind"))
  (is (str/includes? (hub-relation true false true) "ahead"))
  (is (str/includes? (hub-relation false false false) "never fetched")))

(deftest main-checkouts-counts-a-repo-once
  (let [dir (fs/create-temp-dir)
        git (fn [& args] (apply proc/shell {:out :string :err :string :dir (str dir)} "git" args))]
    (try
      (git "init" "-q" "-b" "main" "repo")
      (git "-C" "repo" "-c" "user.name=t" "-c" "user.email=t@t" "commit" "-q" "--allow-empty" "-m" "x")
      (git "-C" "repo" "worktree" "add" "-q" "../repo-wt" "-b" "side")
      (git "init" "-q" ".fixture")
      (git "init" "-q" "storage")
      (is (= ["repo"] (mapv #(str (fs/file-name %)) (main-checkouts (str dir))))
          "linked worktree, dot-dir fixture and excluded storage are all skipped")
      (finally (fs/delete-tree dir)))))

(let [{:keys [fail error]} (run-tests)]
  (System/exit (if (pos? (+ fail error)) 1 0)))
