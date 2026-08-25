#!/usr/bin/env bb
;; grid.bb -- the 32 x 5 acceptance grid: companies against control-loop phases.
;;
;; NOT a survey. Nobody asks these companies anything. Each cell is a
;; COUNTERFACTUAL SIMULATION run from the company's own position at time T
;; against an outcome we already know: could this apparatus have got them to
;; that outcome faster or better, in a way the buyer would have paid for?
;;
;; The corpus was already built for this and did not say so. `:knowable-at-T`
;; is present on all 32 records and the encoding discipline required it to be
;; filled BEFORE the outcome was consulted -- which is exactly what makes it a
;; legitimate test input rather than a hindsight reconstruction. The outcome
;; fields are the oracle. `:fixture-access` asks in so many words whether a
;; newcomer could attempt the test without permission. That is a test suite
;; with the vocabulary of one.
;;
;; What a case OFFERS the simulation differs, so the grid records it per row:
;;   win  -- capture succeeded, so the target is fixed and the question is
;;           "faster? better?" (AppJet -> Etherpad, LangChain -> LangSmith)
;;   loss -- capture failed, so the question is "would we have changed it?"
;;   ncom -- sustained but never commercial: the PlanetMath shape, where the
;;           artifact-oracle held and the sale-oracle never did
;;   open -- still running, no settled endpoint to aim at
;;
;; A cell asks TWO questions, not one, because they come apart. PlanetMath's
;; artifact-oracle held for seventeen years (articles, edits, contributing
;; mathematicians) while its sale-oracle never did and its `:acceptance-event`
;; is `:none`. A single boolean per cell cannot record the case the corpus has
;; first-hand evidence for.
;;
;; Every cell starts :unknown, and :unknown is NOT red. Red means the
;; simulation ran and we could not. Unknown means it has not been run. 160
;; unexamined cells rendered as failures would claim 160 results nobody
;; produced. check.bb already runs strong Kleene three-valued logic over this
;; corpus for that reason; the grid speaks the same logic.
;;
;; Phases come from p4ng's control-stages.edn, read off the drawn control map,
;; so these columns and the loop in the paper cannot drift.
;;
;;   bb grid.bb          render
;;   bb grid.bb --seed   write grid.edn if it does not exist

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.pprint :as pp]
         '[clojure.string :as str])

(def root (.getParentFile (io/file *file*)))
(def records-dir (io/file root "records"))
(def grid-file (io/file root "grid.edn"))
(def p4ng (or (System/getenv "P4NG") "/home/joe/code/p4ng"))
(def stages-file (io/file p4ng "empirics-futon" "control-stages.edn"))

(defn read-edn [file]
  (edn/read-string (slurp file)))

(def known-win #{:scaled :acquired-sideways})
(def known-loss #{:failed :zombie})

(defn target
  "What this case gives the simulation to aim at. A settled capture outcome is
   a fixed target and turns the cell question into faster/better; an open one
   has no endpoint yet, so a simulation over it can only be speculative."
  [c]
  (let [cap (:capture-outcome c)]
    (cond
      (known-win cap)                    :win
      (known-loss cap)                   :loss
      (= :sustained-noncommercially cap) :ncom
      :else                              :open)))

(defn cases []
  (->> (file-seq records-dir)
       (filter #(str/ends-with? (.getName %) ".edn"))
       sort
       (mapcat read-edn)
       (map (fn [c]
              {:case (:case-id c) :org (:org c)
               :target (target c)
               ;; Can the simulation be attempted at all? The schema asks this
               ;; already, in test-suite words: could a newcomer attempt the
               ;; test without permission, and does passing it settle anything.
               :fixture (:fixture-access c)
               ;; The two schema fields the cell's two questions descend from,
               ;; carried so a claim can be checked against what the record
               ;; already says rather than asserted beside it.
               :artifact-oracle (get-in c [:artifact-oracle :hardness])
               :acceptance-event (:acceptance-event c)
               ;; A named successor is a trajectory with a "there" to get to
               ;; sooner -- the clearest form of the faster/better question.
               :trajectory (boolean (seq (get-in c [:lineage :successors])))}))
       vec))

(defn phases []
  (:stages (read-edn stages-file)))

(defn seed [cs ps]
  (vec (for [c cs p ps]
         {:case (:case c) :phase p :solved :unknown :paid :unknown :basis nil})))

(defn cell-state
  "Three-valued, and amber is a fourth reading of the two-valued pair rather
   than a state of its own: solved without payment is the PlanetMath shape."
  [{:keys [solved paid]}]
  (cond
    (= :n solved)              :red
    (and (= :y solved) (= :y paid)) :green
    (and (= :y solved) (= :n paid)) :amber
    :else                      :unknown))

(def glyph {:green "G" :amber "A" :red "R" :unknown "."})

(defn -main []
  (let [cs (cases)
        ps (phases)
        _ (when (and (some #{"--seed"} *command-line-args*) (not (.exists grid-file)))
            (spit grid-file
                  (str ";; The acceptance grid: one cell per (case, phase).\n"
                       ";; :solved -- could we solve a problem for them at this phase?\n"
                       ";; :paid   -- did money move for it?\n"
                       ";; :y | :n | :unknown.  :unknown is not :n -- see grid.bb.\n"
                       (with-out-str (pp/pprint (seed cs ps)))))
            (println "seeded" (str grid-file)))
        grid (if (.exists grid-file) (read-edn grid-file) (seed cs ps))
        by (into {} (map (juxt (juxt :case :phase) identity)) grid)
        states (for [c cs p ps] (cell-state (get by [(:case c) p] {})))
        tally (frequencies states)]
    (println (format "%d cases x %d phases = %d cells\n" (count cs) (count ps)
                     (* (count cs) (count ps))))
    (println (str (apply str (repeat 30 " ")) "tgt trj  "
                  (str/join " " (map #(subs % 0 3) ps))))
    (doseq [c cs]
      (println (format "%-28s  %-4s %-3s  %s"
                       (subs (:org c) 0 (min 28 (count (:org c))))
                       (name (:target c))
                       (if (:trajectory c) "->" "")
                       (str/join "   " (map #(glyph (cell-state (get by [(:case c) %] {})))
                                            ps)))))
    (println)
    (println (format "  targets: %s; %d with a named successor trajectory"
                     (str/join ", " (map (fn [[k n]] (str n " " (name k)))
                                         (sort-by (comp - val) (frequencies (map :target cs)))))
                     (count (filter :trajectory cs))))
    (println (format "  fixture: %d of %d publicly attemptable without permission"
                     (count (filter #(= :y (get-in % [:fixture :public])) cs)) (count cs)))
    (println)
    (doseq [k [:green :amber :red :unknown]]
      (when-let [n (get tally k)]
        (println (format "  %-8s %3d  %s" (name k) n
                         (case k
                           :green "got them there better, and it was worth paying for"
                           :amber "got them there better, nobody would have paid"
                           :red "simulated, and we could not"
                           :unknown "simulation not run")))))))

(-main)
