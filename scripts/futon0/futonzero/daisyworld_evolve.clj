(ns futon0.futonzero.daisyworld-evolve
  "Daisyworld — Phase 3b': LEARN by EVOLUTION (patterns reduced to their computational core; no agent-in-loop).

   A 'pattern' = a local condition→action rule (Alexander IF/HOWEVER/THEN, stripped to its game-theoretic core):
     IF      = my own 3×3  (local temp bucket + local daisy density)
     HOWEVER = the shadow world's 3×3 at the same location (what my neighbour-world is doing)
     THEN    = my response  (place :white / :black / :none)
   A GENOME is the lookup table over (own-temp × own-density × shadow-temp) → action  (3×3×3 = 27 entries).
   No LLM/agent reads or writes patterns — they are evolvable data. Pure evolutionary computing:
   self-play coupling = the adversary, homeostasis under coupling = grounded fitness (G-REWARD, exogenous so it
   can't be self-graded), mutation+selection = R2 (experience improves the policy). This is the fix for the 3b
   REINFORCE collapse: evolution over a population doesn't fall into the always-:warm corner.

   FALSIFIABLE TEST: does evolved fitness improve over generations, and beat random + the hand heuristic on
   held-out coupled homeostasis? Run: bb --classpath scripts scripts/futon0/futonzero/daisyworld_evolve.clj [gens]")

(require '[futon0.futonzero.daisyworld :as dw]
         '[futon0.futonzero.daisyworld-coupled :as cpl]
         '[clojure.string :as str])

(def T* 22.8) (def KAPPA 0.5) (def ACTS [:white :black :none])

(defn- mean [xs] (if (seq xs) (/ (reduce + xs) (double (count xs))) 0.0))
(defn- tbucket [t] (cond (< t 17.0) 0 (> t 28.0) 2 :else 1))          ; cold / ok / hot

(defn- blur [field]   ; 3×3 box-blur of a cell->number field (the local "what's here" reading)
  (into {} (for [c (keys field)]
             (let [ns (conj (dw/neighbours c) c)]
               [c (/ (reduce + (map #(get field % 0.0) ns)) (count ns))]))))

(defn- density-bucket [wd bd c]  ; own 3×3 daisy makeup: white-dom / black-dom / sparse
  (let [w (get wd c 0.0) b (get bd c 0.0)]
    (cond (and (< w 0.12) (< b 0.12)) 2  (> w b) 0  :else 1)))

(defn- gkey [ot od st] (+ (* 9 ot) (* 3 od) st))   ; 0..26

(defn- apply-genome [w shadow-temp genome]
  ;; precompute local fields once, then O(1) per cell; place the rule's daisy where it says white/black
  (let [temp (:temp w) daisy (:daisy w)
        wf (blur (into {} (map (fn [c] [c (if (= :white (:breed (daisy c))) 1.0 0.0)]) (keys temp))))
        bf (blur (into {} (map (fn [c] [c (if (= :black (:breed (daisy c))) 1.0 0.0)]) (keys temp))))
        st (blur shadow-temp)
        d' (reduce (fn [d c]
                     (let [k (gkey (tbucket (temp c)) (density-bucket wf bf c) (tbucket (st c)))]
                       (case (nth genome k)
                         :white (assoc d c {:breed :white :age 0})
                         :black (assoc d c {:breed :black :age 0})
                         d)))
                   daisy (keys temp))]
    (assoc w :daisy d')))

(defn- fitness1 [genome seed-a seed-b ticks]
  ;; self-play: ONE genome drives both coupled worlds; fitness = -mean reg-err (homeostasis), exogenous
  (loop [wa (dw/init-world seed-a {}) wb (dw/init-world seed-b {}) t 0 errs []]
    (if (> t ticks)
      (- (mean errs))
      (let [ia (cpl/import-field (:reflected wb) KAPPA) ib (cpl/import-field (:reflected wa) KAPPA)
            wa2 (apply-genome wa (:temp wb) genome)
            wb2 (apply-genome wb (:temp wa) genome)
            wa' (dw/step wa2 :change ia) wb' (dw/step wb2 :default ib)]
        (recur wa' wb' (inc t)
               (conj errs (Math/abs (- (mean (vals (:temp wa'))) T*)))
               )))))   ; (track world A; B symmetric under the shared genome)

;; fitness AVERAGED over several training seed-pairs (so a genome can't overfit one trajectory)
(def TRAIN-SEEDS [[1 2] [3 4]])
(defn- fitness [genome ticks] (mean (for [[sa sb] TRAIN-SEEDS] (fitness1 genome sa sb ticks))))
(defn- pick [^java.util.Random r coll] (let [v (vec coll)] (nth v (.nextInt r (count v)))))

;; ---- genomes ----
(defn- rand-genome [^java.util.Random r] (vec (repeatedly 27 #(nth ACTS (.nextInt r 3)))))
(defn- heuristic-genome []  ; cool when hot, warm when cold, else none — independent of shadow/density
  (vec (for [ot (range 3) _od (range 3) _st (range 3)]
         (case ot 2 :white 0 :black :none))))

(defn- crossover [^java.util.Random r g1 g2] (vec (map #(if (< (.nextDouble r) 0.5) %1 %2) g1 g2)))
(defn- mutate [^java.util.Random r g rate] (vec (map #(if (< (.nextDouble r) rate) (nth ACTS (.nextInt r 3)) %) g)))

;; ---- general coupled run; ga/gb = genome or nil (nil = autonomous "Classic" world) ----
(defn- cover [w] (/ (count (:daisy w)) (double (* dw/W dw/H))))
(defn- coupled-run [ga gb seed-a seed-b sa sb ticks]
  ;; start hot (luminosity 1.3) so the :change forcing drives the Classic toward collapse within a short run
  (loop [wa (dw/init-world seed-a {:luminosity 1.3}) wb (dw/init-world seed-b {:luminosity 1.3}) t 0 ea [] eb [] ca [] cb []]
    (if (> t ticks)
      {:a {:reg (mean ea) :cover ca} :b {:reg (mean eb) :cover cb}}
      (let [ia (cpl/import-field (:reflected wb) KAPPA) ib (cpl/import-field (:reflected wa) KAPPA)
            wa2 (if ga (apply-genome wa (:temp wb) ga) wa)
            wb2 (if gb (apply-genome wb (:temp wa) gb) wb)
            wa' (dw/step wa2 sa ia) wb' (dw/step wb2 sb ib)]
        (recur wa' wb' (inc t)
               (conj ea (Math/abs (- (mean (vals (:temp wa'))) T*)))
               (conj eb (Math/abs (- (mean (vals (:temp wb'))) T*)))
               (conj ca (cover wa')) (conj cb (cover wb')))))))
(defn- fitness-vc [genome ticks]   ; Pattern (A) maximises ITS OWN homeostasis next to a Classic neighbour (B)
  (- (mean (for [[sa sb] TRAIN-SEEDS] (:reg (:a (coupled-run genome nil sa sb :change :change ticks)))))))
(defn- collapse-tick [covers] (or (first (keep-indexed (fn [i c] (when (< c 0.1) i)) covers)) (count covers)))

(defn- evolve [gens pop-n ticks ^java.util.Random r fit]
  (loop [pop (vec (repeatedly pop-n #(rand-genome r))) g 0 curve []]
    (let [scored (sort-by (comp - second) (map (fn [gm] [gm (fit gm ticks)]) pop))
          best (first scored)
          elite (map first (take (max 2 (int (* 0.3 pop-n))) scored))]
      (if (>= g gens)
        [(first best) curve]
        (let [children (repeatedly (- pop-n (count elite))
                                   #(mutate r (crossover r (pick r elite) (pick r elite)) 0.12))]
          (recur (vec (concat elite children)) (inc g)
                 (conj curve (format "%.2f" (- (second best))))))))))   ; best reg-err this gen

(defn- eval-genome [genome ticks]   ; held-out coupled games (seeds disjoint from TRAIN-SEEDS), mean reg-err
  (mean (for [s (range 5)] (- (fitness1 genome (+ 5000 s) (+ 7000 s) ticks)))))

(defn- program-hypothesis [^java.util.Random r]
  ;; Does a PATTERN world (evolved to maximise ITS OWN survival next to a Classic) keep that Classic neighbour
  ;; alive LONGER than a Classic neighbour would? (Joe's "program your neighbour" / stewardship hypothesis.)
  (let [ticks 45
        [g _] (evolve 5 5 30 r fitness-vc)
        held [[5000 7000] [5001 7001] [5002 7002] [5003 7003]]
        bstat (fn [ga]   ; aliveness of the CLASSIC world B given neighbour ga (Pattern genome) or nil (Classic)
                (let [runs (map (fn [[sa sb]] (:b (coupled-run ga nil sa sb :change :change ticks))) held)]
                  [(mean (map #(mean (:cover %)) runs)) (mean (map #(collapse-tick (:cover %)) runs))]))
        [pc-cover pc-coll] (bstat g)
        [cc-cover cc-coll] (bstat nil)]
    (println "\n=== HYPOTHESIS: does a PATTERN neighbour keep the CLASSIC world alive longer? (both :change) ===")
    (println (format "  Classic B, neighbour = PATTERN : mean-cover %.3f   collapse-tick %.1f / %d" pc-cover pc-coll ticks))
    (println (format "  Classic B, neighbour = CLASSIC : mean-cover %.3f   collapse-tick %.1f / %d" cc-cover cc-coll ticks))
    (println (format "\n  Pattern keeps Classic MORE alive — more cover: %s | survives longer: %s"
                     (> pc-cover cc-cover) (> pc-coll cc-coll)))
    (println "  (if false on both, the Pattern world EXPLOITS its neighbour rather than stewarding it — also a finding)")))

(defn -main [& args]
  (let [r (java.util.Random. 20260623)]
    (if (= (first args) "program")
      (program-hypothesis r)
      (let [gens (if (seq args) (Integer/parseInt (first args)) 8)
            pop-n 6 ticks 16]
        (println "evolving" pop-n "genomes ×" gens "gens (ticks/game" ticks ", grid" dw/W "x" dw/H ")…")
        (let [[best curve] (evolve gens pop-n ticks r fitness)
              ev (eval-genome best ticks)
              rd (eval-genome (rand-genome (java.util.Random. 7)) ticks)
              he (eval-genome (heuristic-genome) ticks)]
          (println "\n=== evolution curve (best reg-err per generation; lower=better) ===")
          (println " " (str/join " " curve))
          (println "\n=== FALSIFIABLE TEST — held-out coupled homeostasis (mean reg-err, lower=better) ===")
          (println (format "  random genome   : %.2f" rd))
          (println (format "  EVOLVED genome  : %.2f   <- learned by pure evolution (no agent)" ev))
          (println (format "  hand heuristic  : %.2f" he))
          (println (format "\n  evolved beats random: %s   matches/beats heuristic: %s"
                           (< ev rd) (<= ev (+ he 0.5)))))))))

(when (= *file* (System/getProperty "babashka.file")) (apply -main *command-line-args*))
