(ns futon0.rhythm.affect-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [futon0.rhythm.affect :as affect]))

(defn- tmp-file [prefix suffix]
  (doto (java.io.File/createTempFile prefix suffix)
    (.deleteOnExit)))

(defn- write-json! [file value]
  (spit file (json/write-str value)))

(defn- read-json [file]
  (json/read-str (slurp file) :key-fn keyword))

(defn- jsonl-rows [file]
  (if (.exists (io/file file))
    (->> (str/split-lines (slurp file))
         (remove str/blank?)
         (mapv #(json/read-str % :key-fn keyword)))
    []))

(deftest documented-false-positives-do-not-fire
  (testing "please no longer stems to joy"
    (is (nil? (affect/detect-affect "please send the receipt"))))
  (testing "interesting no longer stems to attraction"
    (is (nil? (affect/detect-affect "interesting, park that for later"))))
  (testing "negated happy does not fire joy"
    (is (nil? (affect/detect-affect "I am not happy with that result")))))

(deftest true-affect-and-events-fire
  (is (= :joy (:type (affect/detect-affect "I am happy and delighted with this"))))
  (is (= :inspiration
         (:event-type
          (affect/detect-affect "This inspires a new proposal for the resolver"))))
  (is (= :recognition
         (:event-type
          (affect/detect-affect "yes exactly, that result is correct")))))

(deftest live-runner-appends-idempotently-and-refreshes-summary
  (let [entries-file (tmp-file "affect-entries" ".json")
        output-file (tmp-file "affect-output" ".jsonl")
        summary-file (tmp-file "affect-summary" ".json")
        _ (spit output-file "")
        entries [{:evidence/id "turn-1"
                  :evidence/at "2026-06-09T00:00:00Z"
                  :evidence/author "joe"
                  :evidence/session-id "fixture"
                  :evidence/type :coordination
                  :evidence/claim-type :observation
                  :evidence/body {:event "chat-turn"
                                  :role "user"
                                  :campaign-id "C-pudding-prover"
                                  :mission-id "M-pudding-peradams"
                                  :clocked-target "C-pudding-prover / M-pudding-peradams"
                                  :text "I am happy and delighted with this"}}
                 {:evidence/id "turn-2"
                  :evidence/at "2026-06-09T00:01:00Z"
                  :evidence/author "joe"
                  :evidence/session-id "fixture"
                  :evidence/type :coordination
                  :evidence/claim-type :observation
                  :evidence/body {:event "chat-turn"
                                  :role "user"
                                  :campaign-id "C-pudding-prover"
                                  :mission-id "M-pudding-peradams"
                                  :clocked-target "C-pudding-prover / M-pudding-peradams"
                                  :text "yes exactly, that result is correct"}}]]
    (write-json! entries-file {:entries entries})
    (affect/-main "--entries-file" (str entries-file)
                  "--output" (str output-file)
                  "--wm-summary-output" (str summary-file)
                  "--lookahead-minutes" "1"
                  "--live")
    (is (= 2 (count (jsonl-rows output-file))))
    (is (= 2 (:events_loaded (read-json summary-file))))
    (is (= 2 (:candidate_events (read-json summary-file))))
    (is (= #{"M-pudding-peradams"}
           (set (map :mission_id (jsonl-rows output-file)))))
    (is (= #{"C-pudding-prover / M-pudding-peradams"}
           (set (map :clocked_target (:recent (read-json summary-file))))))
    (affect/-main "--entries-file" (str entries-file)
                  "--output" (str output-file)
                  "--wm-summary-output" (str summary-file)
                  "--lookahead-minutes" "1"
                  "--live")
    (is (= 2 (count (jsonl-rows output-file))))
    (is (= #{"turn-1" "turn-2"}
           (set (map :transition_id (jsonl-rows output-file)))))))
