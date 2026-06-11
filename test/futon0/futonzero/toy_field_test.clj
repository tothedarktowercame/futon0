(ns futon0.futonzero.toy-field-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon0.futonzero.toy-field :as toy]))

(defn abs-diff [x]
  (Math/abs (double x)))

(defn closer? [truth before after]
  (< (abs-diff (- truth after)) (abs-diff (- truth before))))

(deftest successful-update-moves-estimates-toward-truth
  (let [before (toy/initial-learner-state)
        after (toy/learn before toy/legit-playout)]
    (is (toy/reward-admissible? toy/legit-playout))
    (is (= [] (:routed after)))
    (doseq [state [:s0 :s1 :s2]]
      (is (closer? (get-in toy/toy-field [:true-value state])
                   (get-in before [:value-estimates state])
                   (get-in after [:value-estimates state]))))
    (is (= 0.5 (get-in after [:value-estimates :s0])))
    (is (= 0.0 (get-in after [:value-estimates :terminal])))))

(deftest refusal-to-learn-from-laundered-reward
  (let [before (toy/initial-learner-state)
        after (toy/learn before toy/laundered-playout)]
    (is (false? (toy/reward-admissible? toy/laundered-playout)))
    (is (= (:value-estimates before) (:value-estimates after)))
    (is (= 1 (count (:routed after))))
    (is (= toy/laundered-playout
           (select-keys (first (:routed after))
                        [:path :return :witness-class :independent? :realised-source])))
    (is (= :rejected (:route/status (first (:routed after)))))
    (is (= #{:missing-witness :not-independent :not-measured}
           (set (:route/reasons (first (:routed after))))))))

(deftest reward-admissibility-guards-each-laundering-mode
  (testing "legit witnessed independent measured outcome"
    (is (true? (toy/reward-admissible? toy/legit-playout))))
  (testing "missing witness refuses"
    (is (false? (toy/reward-admissible?
                 (assoc toy/legit-playout :witness-class :none)))))
  (testing "operator gate is not fruit"
    (is (false? (toy/reward-admissible?
                 (assoc toy/legit-playout :witness-class :operator-gate)))))
  (testing "non-independent reward refuses"
    (is (false? (toy/reward-admissible?
                 (assoc toy/legit-playout :independent? false)))))
  (testing "target-absent fallback refuses"
    (is (false? (toy/reward-admissible?
                 (assoc toy/legit-playout :realised-source :target-absent-fallback)))))
  (testing "non-numeric return refuses (the 4th laundering mode)"
    (is (false? (toy/reward-admissible?
                 (assoc toy/legit-playout :return nil))))
    (is (= [:missing-return]
           (toy/rejection-reasons (assoc toy/legit-playout :return "1.0"))))))
