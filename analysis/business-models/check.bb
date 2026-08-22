#!/usr/bin/env bb

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

;; Declarative predicate vocabulary used by conjectures.edn:
;;
;;   values:     literals, [:get k], [:get-in [k ...]]
;;   predicates: [:= a b], [:not= a b], [:in value #{...}], [:not p],
;;               [:and p ...], [:or p ...], [:non-empty? value],
;;               [:present? value], [:contains-node-type? node-type]
;;
;; Missing keys and the literal :unknown evaluate to a third truth value,
;; :indeterminate. Boolean operators use strong Kleene three-valued logic.
;; Conjecture domains are :cases, :predicted-cases, and :unverified-claims.
;; Quantifiers are :forall and :count-at-least. A :where predicate restricts
;; the domain; an indeterminate :where result is itself reported indeterminate.

(def unknown ::unknown)
(def truth-values #{true false :indeterminate})
(def ^:dynamic *item* nil)

(defn unknown? [x]
  (or (= x unknown) (= x :unknown)))

(defn lookup [m path]
  (let [missing (Object.)
        value (get-in m path missing)]
    (if (or (identical? missing value) (= :unknown value)) unknown value)))

(declare eval-pred)

(defn eval-value [form item]
  (if (and (vector? form) (keyword? (first form)))
    (case (first form)
      :get (lookup item [(second form)])
      :get-in (lookup item (second form))
      form)
    form))

(defn tri-not [x]
  (case x true false false true :indeterminate :indeterminate))

(defn tri-and [xs]
  (cond (some false? xs) false
        (some #{:indeterminate} xs) :indeterminate
        :else true))

(defn tri-or [xs]
  (cond (some true? xs) true
        (some #{:indeterminate} xs) :indeterminate
        :else false))

(defn compare-values [f a b]
  (let [a (eval-value a *item*)
        b (eval-value b *item*)]
    (if (or (unknown? a) (unknown? b)) :indeterminate (boolean (f a b)))))

(defn eval-pred [form item]
  (binding [*item* item]
    (let [[op & args] form]
      (case op
        := (compare-values = (first args) (second args))
        :not= (compare-values not= (first args) (second args))
        :in (compare-values contains? (second args) (first args))
        :not (tri-not (eval-pred (first args) item))
        :and (tri-and (map #(eval-pred % item) args))
        :or (tri-or (map #(eval-pred % item) args))
        :present? (let [v (eval-value (first args) item)]
                    (if (unknown? v) false true))
        :non-empty? (let [v (eval-value (first args) item)]
                      (cond (unknown? v) :indeterminate
                            (string? v) (not (str/blank? v))
                            (coll? v) (boolean (seq v))
                            (nil? v) false
                            :else true))
        :contains-node-type?
        (let [wanted (first args)
              primary (lookup item [:node-types :primary])
              secondary (lookup item [:node-types :secondary])]
          (cond
            (= wanted primary) true
            (and (not (unknown? secondary)) (coll? secondary)
                 (some #{wanted} secondary)) true
            (or (unknown? primary) (unknown? secondary)) :indeterminate
            :else false))
        (throw (ex-info (str "Unknown predicate operator " op) {:form form}))))))

(defn edn-files [dir]
  (let [f (io/file dir)]
    (if (.isDirectory f)
      (->> (.listFiles f)
           (filter #(and (.isFile %) (str/ends-with? (.getName %) ".edn")))
           (sort-by #(.getName %)))
      [])))

(defn records-in [value file]
  (cond
    (and (map? value) (contains? value :case-id)) [value]
    (and (map? value) (sequential? (:records value))) (:records value)
    (sequential? value) value
    :else (throw (ex-info "EDN file is not a case, sequence of cases, or {:records [...]}"
                          {:file (.getPath file)}))))

(defn load-records [dir]
  (reduce
   (fn [{:keys [records errors files]} file]
     (try
       (let [loaded (edn/read-string (slurp file))
             source-name (.getName file)
             rs (mapv #(assoc % ::source-file source-name)
                      (records-in loaded file))]
         {:records (into records rs) :errors errors :files (conj files file)})
       (catch Exception e
         {:records records
          :errors (conj errors {:file (.getPath file) :message (.getMessage e)})
          :files (conj files file)})))
   {:records [] :errors [] :files []}
   (edn-files dir)))

(defn domain-items [over records]
  (case over
    :cases (mapv (fn [r] {:case-id (:case-id r) :value r}) records)
    :predicted-cases
    (->> records
         ;; RECORDS-SPEC freezes batches E/F specifically. Other batches may
         ;; contain placeholder prediction maps, so source batch is part of
         ;; this domain's identity rather than a guess based on field values.
         (filter #(contains? #{"batch-e.edn" "batch-f.edn"}
                             (str/lower-case (::source-file %))))
         (mapv (fn [r] {:case-id (:case-id r) :value r})))
    :unverified-claims
    (->> records
         (mapcat (fn [r]
                   (for [claim (:claims r)
                         :when (= :unverified (:provenance claim))]
                     {:case-id (:case-id r) :value claim})))
         vec)
    (throw (ex-info (str "Unknown conjecture domain " over) {:over over}))))

(defn item-id [{:keys [case-id]}]
  (or case-id :missing-case-id))

(defn classify [{:keys [where pred]} domain-item]
  (let [item (:value domain-item)
        selected (if where (eval-pred where item) true)]
    (case selected
      false :excluded
      :indeterminate :indeterminate
      true (eval-pred pred item))))

(defn ids [items]
  (->> items (map item-id) distinct (sort-by str) vec))

(defn check-conjecture [conjecture records]
  (let [items (domain-items (:over conjecture) records)
        buckets (group-by #(classify conjecture %) items)
        passed (get buckets true [])
        failed (get buckets false [])
        indeterminate (get buckets :indeterminate [])
        quantifier (:quantifier conjecture)
        n (case quantifier
            :forall (+ (count passed) (count failed))
            :count-at-least (count passed))
        holds? (case quantifier
                 :forall (and (empty? failed) (empty? indeterminate))
                 ;; Unknown records are reported but cannot defeat a proven
                 ;; lower bound once enough known matches exist.
                 :count-at-least (>= n (long (:threshold conjecture))))
        counterexamples (case quantifier
                          :forall (ids failed)
                          :count-at-least (if holds? [] (ids (get buckets :excluded []))))
        ;; A boolean cannot distinguish "refuted by a counterexample" from
        ;; "not decidable because some records are :unknown". Conflating them
        ;; manufactures false findings in both directions, so report three.
        verdict (case quantifier
                  :forall (cond (seq failed)        :refuted
                                (seq indeterminate) :undecided
                                :else               :holds)
                  :count-at-least (if holds? :holds :refuted))]
    {:id (:id conjecture)
     :holds? holds?
     :verdict verdict
     :n n
     :counterexamples counterexamples
     :indeterminate (ids indeterminate)}))

(defn expectation-diff [expected result]
  (into (sorted-map)
        (keep (fn [[k wanted]]
                (let [actual (get result k unknown)]
                  (when (not= wanted actual)
                    [k {:expected wanted :actual actual}]))))
        expected))

(defn print-result! [conjecture result]
  (let [diff (expectation-diff (:expected conjecture) result)]
    (println (format "%-36s %-10s n=%d"
                     (name (:id conjecture))
                     (name (:verdict result))
                     (:n result)))
    (when (seq (:counterexamples result))
      (println "  counterexamples:" (str/join ", " (map name (:counterexamples result)))))
    (when (seq (:indeterminate result))
      (println "  indeterminate:" (str/join ", " (map name (:indeterminate result)))))
    (if (seq diff)
      (println "  EXPECTATION MISMATCH:" (pr-str diff))
      (println "  expectation: match"))
    diff))

(defn duplicate-case-ids [records]
  (->> records
       (group-by :case-id)
       (keep (fn [[case-id rs]] (when (> (count rs) 1) case-id)))
       (sort-by str)
       vec))

(defn -main [& _]
  (let [base (.getParentFile (io/file *file*))
        records-dir (io/file base "records")
        conjectures-file (io/file base "conjectures.edn")
        {:keys [records errors files]} (load-records records-dir)
        ;; Self-cases (batch G) are loaded and shape-checked like any other,
        ;; but MUST NOT enter the survey statistics: the corpus is the
        ;; instrument used to judge FUTON's own cases, so letting them into
        ;; the denominator would be layer-6 self-supply. Split here, not by
        ;; keeping them out of the directory -- they still get validated.
        survey (remove #(= :first-hand (:provenance-class %)) records)
        self-cases (filter #(= :first-hand (:provenance-class %)) records)
        conjectures (edn/read-string (slurp conjectures-file))
        duplicate-ids (duplicate-case-ids records)
        lints (filter #(= :lint (:kind %)) conjectures)
        world (remove #(= :lint (:kind %)) conjectures)]
    (println "Business-model conjecture checker")
    (println "records directory:" (.getPath records-dir))
    (println "files found:" (count files) "records loaded:" (count records)
             "| survey:" (count survey) "expected:" 30
             "missing:" (max 0 (- 30 (count survey)))
             "| self-cases (excluded from statistics):" (count self-cases))
    (when (empty? files)
      (println "NOTE: records directory is absent or contains no .edn files."))
    (doseq [{:keys [file message]} errors]
      (println "LOAD ERROR:" file "-" message))
    (when (seq duplicate-ids)
      (println "DUPLICATE CASE IDS:" (str/join ", " (map name duplicate-ids))))
    (println "\nLINT CONJECTURES  (shape discipline -- ALL records, self-cases included)")
    (let [lint-diffs (mapv (fn [c] (print-result! c (check-conjecture c records))) lints)]
      (println "\nWORLD CONJECTURES  (empirical -- SURVEY ONLY, self-cases excluded)")
      (let [world-diffs (mapv (fn [c] (print-result! c (check-conjecture c survey))) world)
            mismatches (+ (count (filter seq lint-diffs))
                          (count (filter seq world-diffs)))
            structural-errors (+ (count errors) (count duplicate-ids))]
        (println (str "\nsummary: " mismatches " expectation mismatch(es), "
                      structural-errors " load/identity error(s)"))
        (when (pos? (+ mismatches structural-errors))
          (System/exit 1))))))

(apply -main *command-line-args*)
