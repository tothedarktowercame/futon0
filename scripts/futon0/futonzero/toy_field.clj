(ns futon0.futonzero.toy-field
  "Synthetic FutonZero §4.2 toy field fixture.

  This namespace is deliberately pure and synthetic. It tests value-update
  arithmetic and anti-laundering failure modes over a tiny known graph; it does
  not model, call, or mutate the real field, WM, HTTP APIs, or substrate.")

(def toy-field
  {:doc "SYNTHETIC fixture only: a four-state chain with known transitions and values. Tests math, not real-field dynamics."
   :states [:s0 :s1 :s2 :terminal]
   :transitions {:s0 [:s1]
                 :s1 [:s2]
                 :s2 [:terminal]
                 :terminal []}
   :terminal :terminal
   :true-value {:s0 1.0
                :s1 1.0
                :s2 1.0
                :terminal 0.0}})

(def witnessed-reward-classes
  #{:build-discharge :witness-discharge})

(def measured-realised-source :measured)

(def default-alpha 0.5)

(defn initial-learner-state
  "Return a blank learner state for the toy field. Estimates intentionally start
  below the known value so a legitimate update can be checked for movement
  toward truth."
  []
  {:value-estimates (zipmap (:states toy-field) (repeat 0.0))
   :routed []})

(defn reward-admissible?
  "True only for anti-laundered reward observations: a witnessed reward class,
  independent evidence, and a measured realised source. Operator gates and
  fallback/target-absent values are not fruit and must not train the learner."
  [outcome]
  (and (contains? witnessed-reward-classes (:witness-class outcome))
       (true? (:independent? outcome))
       (= measured-realised-source (:realised-source outcome))
       (number? (:return outcome))))

(defn- update-toward [old target alpha]
  (+ old (* alpha (- target old))))

(defn value-update
  "Pure Monte-Carlo style update over the toy path. Every non-terminal state in
  the play-out path is backed up toward the observed return."
  ([value-estimates outcome]
   (value-update value-estimates outcome default-alpha))
  ([value-estimates {:keys [path return]} alpha]
   (reduce (fn [est state]
             (if (= state (:terminal toy-field))
               est
               (update est state #(update-toward (double (or % 0.0)) (double return) (double alpha)))))
           value-estimates
           path)))

(defn rejection-reasons
  "Explain why an outcome was routed instead of learned. The result is recorded
  with the routed item; failures are never silently swallowed."
  [outcome]
  (cond-> []
    (not (contains? witnessed-reward-classes (:witness-class outcome)))
    (conj :missing-witness)
    (not (true? (:independent? outcome)))
    (conj :not-independent)
    (not= measured-realised-source (:realised-source outcome))
    (conj :not-measured)
    (not (number? (:return outcome)))
    (conj :missing-return)))

(defn route-rejected [outcome]
  (assoc outcome
         :route/status :rejected
         :route/reasons (rejection-reasons outcome)))

(defn learn
  "Apply one synthetic play-out to the learner state. Admissible outcomes update
  estimates; laundered outcomes leave estimates unchanged and are appended to
  :routed with explicit rejection reasons."
  [state outcome]
  (if (reward-admissible? outcome)
    (update state :value-estimates value-update outcome)
    (update state :routed (fnil conj []) (route-rejected outcome))))

(def legit-playout
  {:path [:s0 :s1 :s2 :terminal]
   :return 1.0
   :witness-class :build-discharge
   :independent? true
   :realised-source :measured})

(def laundered-playout
  {:path [:s0 :s1 :s2 :terminal]
   :return 1.0
   :witness-class :none
   :independent? false
   :realised-source :target-absent-fallback})
