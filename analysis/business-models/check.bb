#!/usr/bin/env bb

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

;; Declarative predicate vocabulary used by conjectures.edn:
;;
;;   values:     literals, [:get k], [:get-in [k ...]]
;;   predicates: [:= a b], [:not= a b], [:in value #{...}], [:not p],
;;               [:and p ...], [:or p ...], [:non-empty? value],
;;               [:present? value], [:contains-node-type? node-type],
;;               [:some= value seq-value], [:all-in seq-value #{...}],
;;               [:exists-record other-path value], [:joined path pred]
;;
;; Missing keys, an explicit nil, and the literal :unknown evaluate to a third
;; truth value, :indeterminate. Boolean operators use strong Kleene
;; three-valued logic.
;;
;; :all-in checks every element of a sequential value against a closed list --
;; the shape a controlled vocabulary actually has. An EMPTY sequence passes:
;; [] means "no class assigned", which the v3 spec treats as a legitimate
;; answer, distinct from "a class not on the list".
;;
;; :some= tests membership in a SEQUENTIAL value. [:in ...] cannot: it compiles
;; to (contains? coll v), and on a vector contains? tests indices, so
;; (contains? [:capacity] :capacity) is false. :problem-class is a vector.
;;
;; :exists-record and :joined read across records, which the v3 demand-side
;; batch needs: a pairing claim is about two records at once. Both see the
;; whole corpus via *records*, not the conjecture's own filtered domain, so a
;; supply-side case can find the demand-side record that points at it.
;;
;; Conjecture domains are :cases (supply-side only), :demand-cases,
;; :predicted-cases, :unverified-claims, and :unverified-fields.
;; Quantifiers are :forall and :count-at-least. A :where predicate restricts
;; the domain; an indeterminate :where result is itself reported indeterminate.

(def unknown ::unknown)
(def truth-values #{true false :indeterminate})
(def ^:dynamic *item* nil)
;; The whole corpus, for the cross-record predicates. Bound once in -main so
;; that a conjecture whose DOMAIN is filtered can still join against records
;; outside it.
(def ^:dynamic *records* [])

(defn demand-record?
  "A v3 demand-side record: written from the customer's seat, so it carries
   none of the supply-side chain fields the world conjectures quantify over."
  [r]
  (= :customer (:entity-type r)))

(defn unknown? [x]
  (or (= x unknown) (= x :unknown)))

(defn lookup [m path]
  (let [missing (Object.)
        value (get-in m path missing)]
    (if (or (identical? missing value) (nil? value) (= :unknown value))
      unknown
      value)))

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
        :some=
        (let [needle (eval-value (first args) item)
              hay (eval-value (second args) item)]
          (cond (or (unknown? needle) (unknown? hay)) :indeterminate
                (set? hay) (boolean (contains? hay needle))
                (coll? hay) (boolean (some #(= needle %) hay))
                :else false))
        :all-in
        (let [v (eval-value (first args) item)
              allowed (second args)]
          (cond (unknown? v) :indeterminate
                (coll? v) (boolean (every? #(contains? allowed %) v))
                :else false))
        :exists-record
        (let [other-path (first args)
              wanted (eval-value (second args) item)]
          (if (unknown? wanted)
            :indeterminate
            (boolean (some (fn [r] (and (not (identical? r item))
                                        (= wanted (lookup r other-path))))
                           *records*))))
        :joined
        (let [wanted (lookup item (first args))]
          (if (unknown? wanted)
            :indeterminate
            (if-let [target (first (filter #(= wanted (:case-id %)) *records*))]
              (eval-pred (second args) target)
              false)))
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
    ;; :cases is the SUPPLY side and always was -- all 32 rows are written from
    ;; the seat of someone travelling the node. Naming that here rather than
    ;; leaving it implicit is what lets demand-side records share the directory
    ;; without turning every supply-side universal :undecided. A customer row
    ;; has no :chain and no :acceptance-event, so it would arrive as an
    ;; indeterminate, and :forall reads one indeterminate as "not decidable" --
    ;; which would report absence of scope as absence of a verdict.
    :cases (->> records (remove demand-record?)
                (mapv (fn [r] {:case-id (:case-id r) :value r})))
    :demand-cases (->> records (filter demand-record?)
                       (mapv (fn [r] {:case-id (:case-id r) :value r})))
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
    ;; v3 puts :provenance inside FIELDS (:capacity, :management-depth,
    ;; :failing-phase, :transition-slots), not only inside :claims. The
    ;; discharge lint walked :claims alone, so a v3 record could be unverified
    ;; in every field, carry no discharge anywhere, and pass clean. No v1/v2
    ;; record has a nested :provenance, so this domain is empty until the
    ;; demand-side batch lands.
    :unverified-fields
    (->> records
         (mapcat (fn [r]
                   (for [[k v] r
                         :when (and (map? v) (= :unverified (:provenance v)))]
                     {:case-id (keyword (str (name (:case-id r)) "/" (name k)))
                      :value v})))
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
        ;; Self-cases (batch G) are IN the survey. An earlier version held
        ;; them out as layer-6 self-supply; Joe corrected that 2026-08-22:
        ;; "throwing out Juxt because we know about it & have contributed
        ;; something useful to them and useful to us would be exactly the
        ;; wrong move." The reasoning was wrong because the terminal rule is
        ;; MECHANICAL -- follow the chain to an obligation or an interest --
        ;; so first-hand knowledge is better data, not biased data. Self-supply
        ;; would bite if the corpus scored FUTON leniently; it does not (both
        ;; land on :interest, as 30/30 others do). :provenance-class stays so
        ;; first-hand and desk-researched remain distinguishable.
        survey (remove demand-record? records)
        demand (filter demand-record? records)
        self-cases (filter #(= :first-hand (:provenance-class %)) records)
        conjectures (edn/read-string (slurp conjectures-file))
        duplicate-ids (duplicate-case-ids records)
        lints (filter #(= :lint (:kind %)) conjectures)
        world (remove #(= :lint (:kind %)) conjectures)]
    (println "Business-model conjecture checker")
    (println "records directory:" (.getPath records-dir))
    (println "files found:" (count files) "records loaded:" (count records)
             "| survey:" (count survey) "expected:" 32
             "missing:" (max 0 (- 32 (count survey)))
             "| of which first-hand:" (count self-cases))
    (println "demand-side records:" (count demand)
             "| paired:" (count (filter #(not (unknown? (lookup % [:for-case]))) demand))
             "| supply cases with a pair:"
             (count (filter (fn [c] (some #(= (:case-id c) (:for-case %)) demand))
                            survey)))
    (when (empty? files)
      (println "NOTE: records directory is absent or contains no .edn files."))
    (doseq [{:keys [file message]} errors]
      (println "LOAD ERROR:" file "-" message))
    (when (seq duplicate-ids)
      (println "DUPLICATE CASE IDS:" (str/join ", " (map name duplicate-ids))))
    (println "\nLINT CONJECTURES  (shape discipline -- ALL records, self-cases included)")
    (binding [*records* records]
     (let [lint-diffs (mapv (fn [c] (print-result! c (check-conjecture c records))) lints)]
      (println "\nWORLD CONJECTURES  (empirical -- all 32, first-hand included)")
      ;; Every conjecture now gets the FULL record set; domain-items does the
      ;; supply/demand split, so the filtering lives in one place and the
      ;; cross-record predicates can still see everything.
      (let [world-diffs (mapv (fn [c] (print-result! c (check-conjecture c records))) world)
            mismatches (+ (count (filter seq lint-diffs))
                          (count (filter seq world-diffs)))
            structural-errors (+ (count errors) (count duplicate-ids))]
        (println (str "\nsummary: " mismatches " expectation mismatch(es), "
                      structural-errors " load/identity error(s)"))
        (when (pos? (+ mismatches structural-errors))
          (System/exit 1)))))))

(apply -main *command-line-args*)
