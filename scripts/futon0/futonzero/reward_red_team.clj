(ns futon0.futonzero.reward-red-team
  "Synthetic FutonZero §4.5 reward red-team fixture.

  Pure offline cases where a fake win would be tempting. This extends the
  toy-field anti-laundering gate with explicit red-team discriminators; it does
  not read, call, or mutate any live field apparatus."
  (:require [futon0.futonzero.toy-field :as toy]))

(def legit-outcome
  (assoc toy/legit-playout
         :realised-read :settled
         :observational-component? true
         :self-referential? false))

(defn red-team-reasons
  "Additional §4.5 red-team rejection reasons beyond toy/rejection-reasons."
  [outcome]
  (cond-> []
    (and (contains? outcome :realised-read)
         (not= :settled (:realised-read outcome)))
    (conj :not-settled)

    (false? (:observational-component? outcome))
    (conj :no-observational-component)

    (true? (:self-referential? outcome))
    (conj :prior-value-tautology)))

(defn rejection-reasons
  "All explicit reasons this outcome may not train the learner."
  [outcome]
  (vec (distinct (concat (toy/rejection-reasons outcome)
                         (red-team-reasons outcome)))))

(defn reward-admissible?
  "Stricter red-team certifier: toy anti-laundering plus §4.5 discriminators."
  [outcome]
  (and (toy/reward-admissible? outcome)
       (empty? (red-team-reasons outcome))))

(defn certify
  "Return a typed acceptance or explicit routed rejection for OUTCOME."
  [outcome]
  (let [reasons (rejection-reasons outcome)]
    (if (empty? reasons)
      {:status :accepted
       :outcome outcome}
      {:status :rejected
       :route/status :rejected
       :route/reasons reasons
       :outcome outcome})))

(def red-team-cases
  [{:id :censored-fallback
    :why "Target vanished; predicted was copied into realised, producing suspicious perfect error."
    :outcome (assoc legit-outcome
                    :predicted 1.0
                    :realised 1.0
                    :error 0.0
                    :realised-source :target-absent-fallback)
    :expected-reasons [:not-measured]}
   {:id :transient-spike
    :why "Realised read taken before the field settled; apparent spike is not stable fruit."
    :outcome (assoc legit-outcome
                    :return 5.0
                    :realised-read :transient)
    :expected-reasons [:not-settled]}
   {:id :prior-value-tautology
    :why "Measurement reduces to the model grading its own predicted effects."
    :outcome (assoc legit-outcome
                    :observational-component? false
                    :self-referential? true)
    :expected-reasons [:no-observational-component :prior-value-tautology]}
   {:id :operator-gate-as-fruit
    :why "Operator approval is a gate, not evidence that the thing functioned."
    :outcome (assoc legit-outcome :witness-class :operator-gate)
    :expected-reasons [:missing-witness]}
   {:id :non-independent
    :why "Outcome not independently witnessed."
    :outcome (assoc legit-outcome :independent? false)
    :expected-reasons [:not-independent]}
   {:id :non-measured
    :why "Outcome source is model/copy apparatus, not a measured read."
    :outcome (assoc legit-outcome :realised-source :model-copy)
    :expected-reasons [:not-measured]}
   {:id :missing-witness
    :why "No build/witness discharge class is present."
    :outcome (assoc legit-outcome :witness-class :none)
    :expected-reasons [:missing-witness]}
   {:id :missing-return
    :why "No numeric realised return to back up."
    :outcome (assoc legit-outcome :return nil)
    :expected-reasons [:missing-return]}])

(defn certify-red-team []
  (mapv (fn [{:keys [outcome expected-reasons] :as case}]
          (assoc case
                 :certification (certify outcome)
                 :passes? (= (set expected-reasons)
                             (set (:route/reasons (certify outcome))))))
        red-team-cases))
