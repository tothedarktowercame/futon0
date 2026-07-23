#!/usr/bin/env bb

(require '[clojure.test :refer [deftest is run-tests]])

(load-file (str (babashka.fs/parent (babashka.fs/parent *file*))
                "/current-usage-report.bb"))

(defn write-session! [path session-id rate-limits]
  (babashka.fs/create-dirs (babashka.fs/parent path))
  (let [timestamp (.toString (java.time.Instant/now))]
    (spit (str path)
          (str (cheshire.core/generate-string
                {:timestamp timestamp
                 :type "session_meta"
                 :payload {:id session-id}})
               "\n"
               (cheshire.core/generate-string
                {:timestamp timestamp
                 :type "event_msg"
                 :payload {:type "token_count"
                           :rate_limits rate-limits
                           :info {:total_token_usage {:input_tokens 10
                                                      :cached_input_tokens 2
                                                      :output_tokens 3
                                                      :reasoning_output_tokens 1
                                                      :total_tokens 13}}}})
               "\n"))))

(deftest resumed-old-directory-and-explicit-windows
  (let [root (babashka.fs/create-temp-dir {:prefix "usage-report-codex-"})
        old-session (babashka.fs/path root "2026" "01" "02" "weekly.jsonl")
        dual-session (babashka.fs/path root "2026" "07" "15" "dual.jsonl")]
    (try
      (write-session! old-session "weekly"
                      {:primary {:used_percent 17.0
                                 :window_minutes 10080
                                 :resets_at 1784666974}})
      (write-session! dual-session "dual"
                      {:primary {:used_percent 23.0
                                 :window_minutes 300
                                 :resets_at 1784131200}
                       :secondary {:used_percent 9.0
                                   :window_minutes 10080
                                   :resets_at 1784666974}})
      (with-redefs [codex-sessions-root (str root)]
        (let [sessions (vec (codex-active-sessions
                             (.minusSeconds (java.time.Instant/now) 60)))
              by-id (into {} (map (juxt :session-id identity) sessions))]
          (is (= #{"weekly" "dual"} (set (keys by-id))))
          (is (nil? (get-in by-id ["weekly" :rate-limits :five-hour])))
          (is (= 17.0 (get-in by-id ["weekly" :rate-limits :weekly :used-pct])))
          (is (= 23.0 (get-in by-id ["dual" :rate-limits :five-hour :used-pct])))
          (is (= 9.0 (get-in by-id ["dual" :rate-limits :weekly :used-pct])))))
      (finally
        (babashka.fs/delete-tree root)))))

(let [{:keys [fail error]} (run-tests)]
  (when (pos? (+ fail error))
    (System/exit 1)))
