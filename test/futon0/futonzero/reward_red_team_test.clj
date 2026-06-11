(ns futon0.futonzero.reward-red-team-test
  (:require [clojure.test :refer [deftest is testing]]
            [futon0.futonzero.reward-red-team :as rt]))

(deftest legit-witnessed-outcome-is-still-accepted
  (let [cert (rt/certify rt/legit-outcome)]
    (is (true? (rt/reward-admissible? rt/legit-outcome)))
    (is (= :accepted (:status cert)))
    (is (nil? (:route/reasons cert)))))

(deftest every-red-team-case-is-rejected-with-explicit-reasons
  (doseq [{:keys [id outcome expected-reasons]} rt/red-team-cases]
    (testing (name id)
      (let [cert (rt/certify outcome)]
        (is (false? (rt/reward-admissible? outcome)))
        (is (= :rejected (:status cert)))
        (is (= :rejected (:route/status cert)))
        (is (= (set expected-reasons) (set (:route/reasons cert))))))))

(deftest red-team-summary-records-all-cases-as-passing
  (let [summary (rt/certify-red-team)]
    (is (= (count rt/red-team-cases) (count summary)))
    (is (every? :passes? summary))))

(deftest known-leniency-launder-by-omission-currently-passes
  ;; Characterization of the ninth shape: documents the deliberate shared leniency
  ;; (omitting :realised-read passes the settled check). When legacy pairs age out
  ;; and the verdict strictens to explicit-:settled-required, flip this to :rejected.
  (let [{:keys [outcome stricten-to]} (first rt/known-leniencies)
        cert (rt/certify outcome)]
    (is (true? (rt/reward-admissible? outcome))
        "currently lenient: omitting :realised-read passes (mirrors live verdict)")
    (is (= :accepted (:status cert)))
    (is (= [:not-settled] stricten-to)
        "documents what this should reject with once strictened")))
