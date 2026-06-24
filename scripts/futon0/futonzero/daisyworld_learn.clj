(ns futon0.futonzero.daisyworld-learn
  "Daisyworld — Phase 3b: LEARN policies from coupled self-play (the R2 loop — does the prior improve?).

   Two worlds share ONE parameterized policy θ (self-play). Each tick, each world reads situation
   FEATURES, samples an action {:cool :warm :none} from softmax(θ·features), and both worlds advance
   via the coupled step (local 3×3 heat export). After each game, θ is updated by REINFORCE toward
   actions that produced better HOMEOSTASIS — the EXOGENOUS, grounded reward (a world's reg-err
   depends on the other's exports, so it can't be self-graded; Goodhart's door stays shut).

   This is R2 at toy scale: experience trains the policy. (The richer AlphaZero version distils a
   rollout/search policy into θ; here θ is trained by policy-gradient directly — cheaper, still a
   genuine self-play learning loop.) FALSIFIABLE TEST: does the TRAINED policy beat the FROZEN
   (untrained) one — and the hand HEURISTIC — on held-out coupled homeostasis?

   Run: bb --classpath scripts scripts/futon0/futonzero/daisyworld_learn.clj [train-games]")

(require '[futon0.futonzero.daisyworld :as dw]
         '[futon0.futonzero.daisyworld-coupled :as cpl]
         '[clojure.string :as str])

(def T* 22.8)
(def ACTIONS [:cool :warm :none])
(def ALPHA 0.03)        ; learning rate
(def KAPPA 0.5)         ; coupling

(defn- mean [xs] (if (seq xs) (/ (reduce + xs) (double (count xs))) 0.0))
(defn- mt [w] (mean (vals (:temp w))))

;; ---- situation features (normalised) ----
(defn- features [w import]
  (let [m (mt w) ts (vals (:temp w))
        spread (Math/sqrt (mean (map #(let [d (- % m)] (* d d)) ts)))
        imp (if import (mean (vals import)) 0.0)]
    [1.0 (/ (- m T*) 10.0) (/ spread 10.0) (/ imp 5.0)]))   ; bias, temp-dev, spread, coupling-pressure

;; ---- linear-softmax policy: theta = {action -> weight-vector} ----
(defn- dot [u v] (reduce + (map * u v)))
(defn- probs [theta f]
  (let [logits (into {} (map (fn [a] [a (dot (theta a) f)]) ACTIONS))
        mx (apply max (vals logits))
        es (into {} (map (fn [[a v]] [a (Math/exp (- v mx))]) logits))
        z (reduce + (vals es))]
    (into {} (map (fn [[a v]] [a (/ v z)]) es))))

(defn- sample [theta f ^java.util.Random rng]
  (let [p (probs theta f) r (.nextDouble rng)]
    (loop [cum 0.0 as ACTIONS]
      (let [a (first as) cum (+ cum (p a))]
        (if (or (>= cum r) (empty? (rest as))) a (recur cum (rest as)))))))

(defn- apply-action [{:keys [temp] :as w} a]
  (let [ext (fn [pick] (key ((if (= pick :hot) last first) (sort-by val temp))))]
    (case a
      :cool (assoc-in w [:daisy (ext :hot)]  {:breed :white :age 0})
      :warm (assoc-in w [:daisy (ext :cold)] {:breed :black :age 0})
      w)))

;; ---- one self-play game: shared theta, both worlds; returns per-world {:traj [[f a]...] :reg err} ----
(defn- play-game [theta seed-a seed-b ticks sa sb rng]
  (loop [wa (dw/init-world seed-a {}) wb (dw/init-world seed-b {})
         t 0 ta [] tb [] ea [] eb []]
    (if (> t ticks)
      [{:traj ta :reg (mean ea)} {:traj tb :reg (mean eb)}]
      (let [ia (cpl/import-field (:reflected wb) KAPPA)
            ib (cpl/import-field (:reflected wa) KAPPA)
            fa (features wa ia) fb (features wb ib)
            aa (sample theta fa rng) ab (sample theta fb rng)
            wa' (dw/step (apply-action wa aa) sa ia)
            wb' (dw/step (apply-action wb ab) sb ib)
            ra (- (Math/abs (- (mt wa') T*))) rb (- (Math/abs (- (mt wb') T*)))]  ; per-step reward
        (recur wa' wb' (inc t)
               (conj ta [fa aa ra]) (conj tb [fb ab rb])
               (conj ea (- ra)) (conj eb (- rb)))))))

;; ---- REINFORCE with per-step RETURN-TO-GO and a per-trajectory baseline (centered returns) ----
(defn- returns-to-go [traj gamma]
  (loop [rs (reverse (map #(nth % 2) traj)) g 0.0 acc ()]
    (if (empty? rs) (vec acc)
        (let [g' (+ (first rs) (* gamma g))] (recur (rest rs) g' (cons g' acc))))))

(defn- reinforce [theta traj gamma]
  (if (empty? traj) theta
      (let [gts (returns-to-go traj gamma)
            b (mean gts)]                              ; baseline = mean return (variance reduction)
        (reduce (fn [th i]
                  (let [[f a _] (nth traj i) adv (- (nth gts i) b) p (probs th f)]
                    (reduce (fn [th2 act]
                              (let [grad (* ALPHA adv (- (if (= act a) 1.0 0.0) (p act)))]
                                (update th2 act (fn [w] (mapv #(+ %1 (* grad %2)) w f)))))
                            th ACTIONS)))
                theta (range (count traj))))))

(defn- zero-theta [] (zipmap ACTIONS (repeat [0.0 0.0 0.0 0.0])))
(defn- heuristic-theta []   ; hand policy: cool when hot (temp-dev>0), warm when cold, slight none-bias
  {:cool [0.0 3.0 0.0 0.5] :warm [0.0 -3.0 0.0 0.0] :none [0.3 0.0 0.0 0.0]})

(defn- train [games ticks rng]
  (loop [theta (zero-theta) g 0 curve []]
    (if (>= g games)
      [theta curve]
      (let [[ra rb] (play-game theta (+ 100 g) (+ 900 g) ticks :change :default rng)
            ;; train shared θ from BOTH worlds' trajectories via per-step return-to-go (γ=0.9)
            theta (-> theta (reinforce (:traj ra) 0.9) (reinforce (:traj rb) 0.9))
            avg (/ (+ (:reg ra) (:reg rb)) 2.0)]
        (recur theta (inc g) (conj curve (format "%.2f" avg)))))))

(defn- eval-policy [theta games ticks rng]
  ;; held-out coupled games (seeds disjoint from training); mean reg-err over both worlds
  (mean (for [g (range games)]
          (let [[ra rb] (play-game theta (+ 5000 g) (+ 7000 g) ticks :change :default rng)]
            (/ (+ (:reg ra) (:reg rb)) 2.0)))))

(defn -main [& args]
  (let [games (if (seq args) (Integer/parseInt (first args)) 16)
        ticks 30
        rng (java.util.Random. 20260623)
        _ (println "training" games "self-play games (ticks/game" ticks ", grid" dw/W "x" dw/H ")…")
        [theta curve] (train games ticks rng)
        frozen (eval-policy (zero-theta) 5 ticks (java.util.Random. 42))
        trained (eval-policy theta 5 ticks (java.util.Random. 42))
        heur (eval-policy (heuristic-theta) 5 ticks (java.util.Random. 42))]
    (println "\n=== learning curve (avg reg-err per training game; lower=better homeostasis) ===")
    (println " " (str/join " " curve))
    (println "\n=== FALSIFIABLE TEST — held-out coupled homeostasis (mean reg-err, lower=better) ===")
    (println (format "  frozen (untrained θ=0) : %.2f" frozen))
    (println (format "  TRAINED (self-play)    : %.2f   <- learned from experience" trained))
    (println (format "  hand heuristic         : %.2f" heur))
    (println (format "\n  learned policy beats frozen: %s   matches/beats heuristic: %s"
                     (< trained frozen) (<= trained (+ heur 0.5))))
    (println "  trained θ:" (into {} (map (fn [[a w]] [a (mapv #(Double/parseDouble (format "%.2f" %)) w)]) theta)))))

(when (= *file* (System/getProperty "babashka.file")) (apply -main *command-line-args*))
