(ns futon0.rhythm.affect-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon0.rhythm.affect :as affect]))

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
