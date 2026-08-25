#!/usr/bin/env bb
;; grid.bb -- the 32 x 5 acceptance grid: companies against control-loop phases.
;;
;; A cell asks one question about one company at one phase of ITS OWN loop:
;; could we solve a problem for them there, and did money move. Two questions,
;; not one, because they come apart -- PlanetMath's artifact-oracle held for
;; seventeen years (articles, edits, contributing mathematicians) while its
;; sale-oracle never did, and its `:acceptance-event` is `:none`. A grid with a
;; single boolean per cell cannot record that, and it is the case the corpus
;; has first-hand evidence for.
;;
;; Every cell starts :unknown, and :unknown is NOT red. Red means asked and
;; answered no; :unknown means nobody has asked. Collapsing them would make a
;; grid of 160 unexamined cells look like 160 tested failures, which is the
;; same error as reporting a failed fetch as a count of zero. check.bb already
;; runs strong Kleene three-valued logic over this corpus for exactly this
;; reason; the grid speaks the same logic.
;;
;; Phases come from p4ng's control-stages.edn, which is read off the drawn
;; control map, so the columns here and the loop in the paper cannot drift.
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

(defn cases []
  (->> (file-seq records-dir)
       (filter #(str/ends-with? (.getName %) ".edn"))
       sort
       (mapcat read-edn)
       (map (fn [c] {:case (:case-id c) :org (:org c)
                     ;; The two schema fields the grid's two questions descend
                     ;; from. Carried so a cell claim can be checked against
                     ;; what the case record already says rather than asserted
                     ;; beside it.
                     :artifact-oracle (get-in c [:artifact-oracle :hardness])
                     :acceptance-event (:acceptance-event c)}))
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
    (println (str (apply str (repeat 34 " "))
                  (str/join " " (map #(subs % 0 3) ps))))
    (doseq [c cs]
      (println (format "%-32s  %s"
                       (subs (:org c) 0 (min 32 (count (:org c))))
                       (str/join "   " (map #(glyph (cell-state (get by [(:case c) %] {})))
                                            ps)))))
    (println)
    (doseq [k [:green :amber :red :unknown]]
      (when-let [n (get tally k)]
        (println (format "  %-8s %3d  %s" (name k) n
                         (case k
                           :green "solved and paid"
                           :amber "solved, nobody paid"
                           :red "asked, and we cannot"
                           :unknown "nobody has asked")))))))

(-main)
