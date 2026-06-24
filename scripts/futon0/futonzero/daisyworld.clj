(ns futon0.futonzero.daisyworld
  "Daisyworld — Phase 1: the SIMULATOR (G-SIM / the daisy domain's R4 forward-model).

   A faithful Clojure port of the JuliaDynamics Agents.jl Daisyworld example
   (https://juliadynamics.github.io/Agents.jl/v4.0/examples/daisyworld/), kept deliberately
   self-contained (clojure.core + java.util.Random only) so it runs under bb/clj with no deps and
   can later be wrapped as a FutonZero toy-field. Lovelock/Watson's homeostasis model: black/white
   daisies regulate planetary temperature via albedo; the planet self-regulates near the daisies'
   optimal temperature across a range of solar luminosity — THE morphogenesis/homeostasis toy.

   Phase 2 (separate): the FutonZero G(pi) player selects daisy-seeding POLICIES by rollout
   (R1 PUCT-prior + R2 return-channel + R5 EFE), reward = homeostasis (G-REWARD). This file is just
   the validated world the player will play on.

   Run:  bb futon0/scripts/futon0/futonzero/daisyworld.clj")

;; ---- parameters (defaults from the Agents.jl reference) ----
(def ^:const W 14)
(def ^:const H 14)
(def DEFAULTS
  {:max-age 25
   :white-albedo 0.75 :black-albedo 0.25 :surface-albedo 0.4
   :luminosity 1.0 :solar-change 0.005 :diffusion 0.5
   :init-white 0.2 :init-black 0.2})

(defn- cell [x y] [(mod x W) (mod y H)])
(defn neighbours [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not (and (zero? dx) (zero? dy)))]
    (cell (+ x dx) (+ y dy))))

;; ---- world state: {:temp {cell->°C} :daisy {cell->{:breed :age}} :lum L :tick t} ----
(defn init-world [seed cfg]
  (let [cfg (merge DEFAULTS cfg)
        rng (java.util.Random. seed)
        cells (for [x (range W) y (range H)] [x y])
        temp (zipmap cells (repeat 0.0))
        n (* W H)
        shuffled (mapv second (sort-by first (map (fn [c] [(.nextDouble rng) c]) cells)))
        nw (int (* (:init-white cfg) n)) nb (int (* (:init-black cfg) n))
        whites (take nw shuffled)
        blacks (take nb (drop nw shuffled))
        daisy (merge (zipmap whites (repeat {:breed :white :age 0}))
                     (zipmap blacks (repeat {:breed :black :age 0})))]
    {:temp temp :daisy daisy :lum (:luminosity cfg) :tick 0 :rng rng :cfg cfg}))

(defn- albedo [cfg breed]
  (case breed :white (:white-albedo cfg) :black (:black-albedo cfg) (:surface-albedo cfg)))

;; ---- local temperature update. `import` (cell->extra incoming heat, or nil) is the local heat
;; imported from a coupled shadow world; we also record `:reflected` (cell->reflected heat) for export. ----
(defn- update-temp [{:keys [temp daisy lum cfg] :as w} import]
  (let [out (reduce (fn [acc [c t]]
                      (let [a (albedo cfg (:breed (daisy c)))
                            incoming (+ lum (if import (get import c 0.0) 0.0))
                            absorbed (* (- 1.0 a) incoming)
                            heating (if (pos? absorbed) (+ (* 72.0 (Math/log absorbed)) 80.0) 80.0)]
                        (-> acc (assoc-in [:t c] (/ (+ t heating) 2.0))
                                (assoc-in [:r c] (* a incoming)))))   ; reflected = albedo * incoming
                    {:t {} :r {}} temp)]
    (assoc w :temp (:t out) :reflected (:r out))))

;; ---- diffusion: new = (1-r)*self + r*mean(8 neighbours) ----
(defn- diffuse [{:keys [temp cfg] :as w}]
  (let [r (:diffusion cfg)]
    (assoc w :temp
           (into {} (for [[c t] temp]
                      (let [ns (map temp (neighbours c))
                            m (/ (reduce + ns) (count ns))]
                        [c (+ (* (- 1.0 r) t) (* r m))]))))))

;; ---- reproduction: p = 0.1457 T - 0.0032 T^2 - 0.6443; seed same breed into a random empty neighbour ----
(defn- seed-prob [t] (max 0.0 (- (* 0.1457 t) (* 0.0032 t t) 0.6443)))
(defn- propagate [{:keys [temp daisy rng] :as w}]
  (loop [cs (keys daisy) d daisy]
    (if-let [c (first cs)]
      (let [p (seed-prob (temp c))]
        (if (< (.nextDouble rng) p)
          (let [empties (remove d (neighbours c))]
            (if (seq empties)
              (let [tgt (nth (vec empties) (.nextInt rng (count empties)))]
                (recur (rest cs) (assoc d tgt {:breed (:breed (daisy c)) :age 0})))
              (recur (rest cs) d)))
          (recur (rest cs) d)))
      (assoc w :daisy d))))

;; ---- ageing + death (age >= max-age) ----
(defn- age-and-die [{:keys [daisy cfg] :as w}]
  (assoc w :daisy
         (into {} (keep (fn [[c {:keys [breed age]}]]
                          (let [a (inc age)]
                            (when (< a (:max-age cfg)) [c {:breed breed :age a}])))
                        daisy))))

;; ---- solar activity scenarios ----
(defn- solar [{:keys [lum cfg tick] :as w} scenario]
  (let [dl (:solar-change cfg)]
    (assoc w :lum
           (case scenario
             :ramp (cond (<= 201 tick 400) (+ lum dl)
                         (<= 501 tick 750) (- lum (/ dl 2.0)) :else lum)
             :change (+ lum dl)
             lum))))

(defn step
  "Advance one tick. 3-arity passes a coupled-world `import` field (cell->extra incoming heat)."
  ([w scenario] (step w scenario nil))
  ([w scenario import]
   (-> w age-and-die (update-temp import) diffuse propagate
       (solar scenario) (update :tick inc))))

;; ---- observables ----
(defn stats [{:keys [temp daisy lum tick]}]
  (let [bs (count (filter #(= :black (:breed %)) (vals daisy)))
        ws (count (filter #(= :white (:breed %)) (vals daisy)))
        mt (/ (reduce + (vals temp)) (count temp))]
    {:tick tick :lum (Double/parseDouble (format "%.3f" lum))
     :mean-temp (Double/parseDouble (format "%.2f" mt)) :black bs :white ws
     :cover (Double/parseDouble (format "%.2f" (/ (+ bs ws) (double (* W H)))))}))

(defn run [{:keys [seed ticks scenario cfg] :or {seed 20260623 ticks 500 scenario :default cfg {}}}]
  (loop [w (init-world seed cfg) out []]
    (if (> (:tick w) ticks)
      out
      (recur (step w scenario)
             (if (zero? (mod (:tick w) 50)) (conj out (stats w)) out)))))

(defn -main [& args]
  (let [scenario (keyword (or (first args) "default"))]
    (println "Daisyworld (Phase-1 simulator) — scenario" scenario)
    (println "tick   lum   mean-temp  black white  cover")
    (doseq [s (run {:scenario scenario})]
      (println (format "%-6d %-5.3f %-9.2f  %-5d %-5d  %.2f"
                       (:tick s) (:lum s) (:mean-temp s) (:black s) (:white s) (:cover s))))))

(when (= *file* (System/getProperty "babashka.file")) (apply -main *command-line-args*))
