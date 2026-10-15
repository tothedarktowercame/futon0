(ns futon0.futonzero.fidelity-test
  (:require [clojure.test :refer [deftest is]]
            [futon0.futonzero.fidelity :as sut]))

(deftest repository-contract-is-complete-and-source-pinned
  (let [result (sut/validate-contract (sut/read-contract))]
    (is (:valid? result))
    (is (= {:preserve 7 :adapt 3 :drop 3} (:counts result)))
    (is (every? (comp empty? :errors) (:entry-results result)))))

(deftest dropped-capability-cannot-disappear-without-a-reason
  (let [entry (-> (sut/read-contract) :capabilities last (assoc :rationale ""))]
    (is (= [":mission-control-writeback needs a disposition rationale"]
           (sut/entry-errors entry)))))

(deftest missing-source-is-an-invalid-contract-not-an-exception
  (let [contract (update (sut/read-contract) :capabilities
                         #(assoc % 0 (dissoc (first %) :source)))
        result (sut/validate-contract contract)]
    (is (false? (:valid? result)))
    (is (= [":structured-evidence-observation needs a pinned source"]
           (-> result :entry-results first :errors)))))
