#!/usr/bin/env bb

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.set :as set]
         '[clojure.string :as str])

(def root (.getParentFile (io/file *file*)))
(def records-dir (io/file root "records"))
(def lines-file (io/file root "budget-lines.edn"))
(def assignments-file (io/file root "budget-assignments.edn"))
(def graph-file (io/file root "graph.edn"))
(def snapshot-file (io/file root "graph-snapshot.edn"))

(def node-type-ids
  [:requirement :call :contest :benchmark :dependency :environment :market-exchange])
(def terminal-kind-ids
  [:interest :obligation-external :obligation-manufactured :none])

(defn read-edn [file]
  (try
    {:value (edn/read-string (slurp file))}
    (catch Exception e
      {:error (str (.getName file) ": " (.getMessage e))})))

(defn records-in [value]
  (cond
    (nil? value) []
    (and (map? value) (:case-id value)) [value]
    (and (map? value) (sequential? (:records value))) (:records value)
    (sequential? value) value
    :else []))

(defn load-records []
  (let [files (if (.isDirectory records-dir)
                (sort-by #(.getName %) (filter #(str/ends-with? (.getName %) ".edn")
                                               (file-seq records-dir)))
                [])
        loaded (map (fn [file] [file (read-edn file)]) files)]
    {:records (vec (mapcat (comp records-in :value second) loaded))
     :files (count files)
     :errors (vec (keep (comp :error second) loaded))}))

(defn load-budget-lines []
  (if-not (.isFile lines-file)
    {:lines {} :warning "budget-lines.edn absent"}
    (let [{:keys [value error]} (read-edn lines-file)]
      (cond
        error {:lines {} :warning error}
        (map? value) {:lines value}
        :else {:lines {} :warning "budget-lines.edn is not a map"}))))

(defn assignment-pairs [value]
  (let [value (if (and (map? value) (contains? value :assignments))
                (:assignments value)
                value)]
    (cond
      (map? value)
      (for [[case-id assignment] value]
        [case-id (if (map? assignment)
                   (or (:budget-line assignment) (:line assignment) :unclassified)
                   assignment)])

      (sequential? value)
      (for [assignment value :when (map? assignment)]
        [(:case-id assignment)
         (or (:budget-line assignment) (:line assignment) :unclassified)])

      :else [])))

(defn load-assignments []
  (if-not (.isFile assignments-file)
    {:assignments {} :warning "budget-assignments.edn absent"}
    (let [{:keys [value error]} (read-edn assignments-file)]
      (if error
        {:assignments {} :warning error}
        {:assignments (into {} (assignment-pairs value))}))))

(defn node-id [kind id] [kind id])
(defn sorted-vec [xs] (vec (sort-by pr-str xs)))

(defn case-node [record]
  {:id (node-id :case (:case-id record))
   :kind :case
   :case-id (:case-id record)
   :org (:org record)
   :artifact (get-in record [:chain :artifact])
   :sponsor (get-in record [:chain :sponsor])
   :payer (get-in record [:chain :payer])
   :beneficiary (:beneficiary record)
   :seat (:seat record)})

(defn problem-holder-node [record]
  {:id (node-id :problem-holder (:case-id record))
   :kind :problem-holder
   :case-id (:case-id record)
   :beneficiary (:beneficiary record)})

(defn edge [type case-id more]
  (merge {:id (into [:edge type case-id] (or (:id-suffix more) []))
          :type type
          :from (node-id :case case-id)}
         (dissoc more :id-suffix)))

(defn record-edges [record budget-line]
  (let [case-id (:case-id record)
        primary (get-in record [:node-types :primary])
        secondary (get-in record [:node-types :secondary])
        terminal (get-in record [:chain :terminal])
        beneficiary (:beneficiary record)]
    (concat
     [(edge :produces case-id
            {:to nil :target-kind :artifact-label
             :target-label (get-in record [:chain :artifact])})]
     (when primary
       [(edge :adopts case-id {:to (node-id :node-type primary)
                               :role :primary :id-suffix [:primary]})])
     (for [node-type secondary]
       (edge :adopts case-id {:to (node-id :node-type node-type)
                              :role :secondary :id-suffix [:secondary node-type]}))
     [(edge :pays case-id
            {:to nil :target-kind :case-sponsor
             :target-label (get-in record [:chain :sponsor])
             :acceptance-event (:acceptance-event record)})
      (edge :drawn-from case-id {:to (node-id :budget-line budget-line)})
      (edge :obliged-by case-id {:to (node-id :terminal-kind terminal)
                                 :terminal terminal})]
     (when (= :operating (:seat record))
       [(edge :operates case-id {:to (node-id :node-type primary)})])
     [(if (= :unknown beneficiary)
        (edge :serves case-id {:to nil :beneficiary :unknown :dangling? true})
        (edge :serves case-id {:to (node-id :problem-holder case-id)
                               :beneficiary beneficiary :dangling? false}))])))

(defn derive-graph [records lines assignments warnings]
  (let [record-ids (set (map :case-id records))
        assignment-lines (into {} (for [case-id record-ids]
                                    [case-id (get assignments case-id :unclassified)]))
        line-ids (conj (set (keys lines)) :unclassified)
        problem-records (remove #(= :unknown (:beneficiary %)) records)
        nodes (concat
               (map case-node records)
               (for [line-id line-ids]
                 {:id (node-id :budget-line line-id)
                  :kind :budget-line :budget-line line-id
                  :label (or (get-in lines [line-id :label])
                             (when (= line-id :unclassified) "Unclassified"))
                  :evidence (get-in lines [line-id :evidence])})
               (for [id node-type-ids]
                 {:id (node-id :node-type id) :kind :node-type :node-type id})
               (for [id terminal-kind-ids]
                 {:id (node-id :terminal-kind id) :kind :terminal-kind :terminal-kind id})
               (map problem-holder-node problem-records))
        edges (mapcat #(record-edges % (get assignment-lines (:case-id %))) records)
        unknown-assignment-ids (set/difference (set (keys assignments)) record-ids)]
    {:format-version 1
     :derived-from {:record-files (if (.isDirectory records-dir)
                                    (count (filter #(str/ends-with? (.getName %) ".edn")
                                                   (file-seq records-dir))) 0)
                    :record-count (count records)
                    :budget-lines-present? (.isFile lines-file)
                    :budget-assignments-present? (.isFile assignments-file)}
     :warnings (sorted-vec
                (concat warnings
                        (when (seq unknown-assignment-ids)
                          [(str "assignments for unknown cases: "
                                (str/join ", " (sort (map name unknown-assignment-ids))))])))
     :nodes (sorted-vec nodes)
     :edges (sorted-vec edges)}))

(defn frequencies-by [f xs]
  (into (sorted-map) (frequencies (map f xs))))

(defn occupancy [graph]
  (let [line-ids (for [{:keys [kind budget-line]} (:nodes graph)
                       :when (= kind :budget-line)] budget-line)
        drawn (group-by #(second (:to %))
                        (filter #(= :drawn-from (:type %)) (:edges graph)))]
    (into (sorted-map)
          (for [line-id line-ids]
            [line-id (sorted-vec (map #(second (:from %)) (get drawn line-id [])))]))))

(defn adjacency [graph]
  (let [ids (set (map :id (:nodes graph)))
        base (zipmap ids (repeat #{}))]
    (reduce (fn [adj {:keys [from to]}]
              (if (and (contains? ids from) (contains? ids to))
                (-> adj (update from conj to) (update to conj from))
                adj))
            base (:edges graph))))

(defn connected-components [graph]
  (let [adj (adjacency graph)]
    (loop [remaining (set (keys adj)) components []]
      (if-let [start (first remaining)]
        (let [component
              (loop [frontier [start] seen #{}]
                (if-let [node (peek frontier)]
                  (if (seen node)
                    (recur (pop frontier) seen)
                    (recur (into (pop frontier) (get adj node)) (conj seen node)))
                  seen))]
          (recur (set/difference remaining component)
                 (conj components (sorted-vec component))))
        (vec (sort-by (juxt (comp - count) pr-str) components))))))

(defn topology [graph]
  (let [occ (occupancy graph)
        serves (filter #(= :serves (:type %)) (:edges graph))]
    {:occupancy occ
     :attached-serves (set (map #(second (:from %)) (remove :dangling? serves)))
     :orphan-budget-lines (set (for [[line-id cases] occ :when (empty? cases)] line-id))
     :components (connected-components graph)}))

(defn snapshot [graph]
  (let [topo (topology graph)]
    {:format-version 1
     :nodes (:nodes graph)
     :edges (:edges graph)
     :occupancy (:occupancy topo)
     :attached-serves (sorted-vec (:attached-serves topo))
     :orphan-budget-lines (sorted-vec (:orphan-budget-lines topo))
     :components (:components topo)}))

(defn read-snapshot []
  (when (.isFile snapshot-file)
    (let [{:keys [value error]} (read-edn snapshot-file)]
      (if error {:read-error error} value))))

(defn component-changes [old-components new-components]
  (let [old (map set old-components)
        new (map set new-components)
        overlaps (fn [component candidates]
                   (count (filter #(seq (set/intersection component %)) candidates)))]
    {:merged (count (filter #(> (overlaps % old) 1) new))
     :split (count (filter #(> (overlaps % new) 1) old))}))

(defn topology-diff [old current]
  (let [old-occ (or (:occupancy old) {})
        new-occ (:occupancy current)
        lines (set/union (set (keys old-occ)) (set (keys new-occ)))
        occupancy-changes
        (into (sorted-map)
              (keep (fn [line]
                      (let [before (set (get old-occ line []))
                            after (set (get new-occ line []))
                            gained (set/difference after before)
                            lost (set/difference before after)]
                        (when (or (seq gained) (seq lost))
                          [line {:gained (sorted-vec gained) :lost (sorted-vec lost)}])))
                    lines))
        old-attached (set (:attached-serves old))
        new-attached (set (:attached-serves current))
        old-orphans (set (:orphan-budget-lines old))
        new-orphans (set (:orphan-budget-lines current))]
    {:occupancy occupancy-changes
     :serves-attached (sorted-vec (set/difference new-attached old-attached))
     :serves-detached (sorted-vec (set/difference old-attached new-attached))
     :newly-orphaned (sorted-vec (set/difference new-orphans old-orphans))
     :no-longer-orphaned (sorted-vec (set/difference old-orphans new-orphans))
     :components (component-changes (:components old) (:components current))}))

(defn fmt-items [xs]
  (if (seq xs) (str/join ", " (map name xs)) "none"))

(defn print-report [graph topo previous]
  (println "Business-model topology")
  (println (format "Sources: %d record file(s), %d record(s); budget lines %s; assignments %s"
                   (get-in graph [:derived-from :record-files])
                   (get-in graph [:derived-from :record-count])
                   (if (get-in graph [:derived-from :budget-lines-present?]) "present" "absent")
                   (if (get-in graph [:derived-from :budget-assignments-present?]) "present" "absent")))
  (doseq [warning (:warnings graph)] (println "WARNING:" warning))
  (println "\nNodes by kind:")
  (doseq [[kind n] (frequencies-by :kind (:nodes graph))]
    (println (format "  %-22s %d" (name kind) n)))
  (println "Edges by type:")
  (doseq [[type n] (frequencies-by :type (:edges graph))]
    (println (format "  %-22s %d" (name type) n)))
  (println "\nBudget-line occupancy (descending):")
  (doseq [[line cases] (sort-by (juxt (comp - count val) (comp name key)) (:occupancy topo))]
    (println (format "  %-32s %2d  %s" (name line) (count cases) (fmt-items cases))))
  (let [dangling (sort-by #(second (:from %))
                          (filter #(and (= :serves (:type %)) (:dangling? %)) (:edges graph)))]
    (println "\nDangling serves edges by case:")
    (if (seq dangling)
      (doseq [edge dangling] (println " " (name (second (:from edge)))))
      (println "  none")))
  (println "\nOrphan budget lines:" (fmt-items (sort (:orphan-budget-lines topo))))
  (let [components (:components topo)
        largest (first components)]
    (println (format "Connected components: %d; largest: %d node(s)"
                     (count components) (count largest)))
    (doseq [[i component] (map-indexed vector components)]
      (println (format "  %d (%d): %s" (inc i) (count component)
                       (str/join ", " (map pr-str component))))))
  (println "\nSnapshot diff:")
  (cond
    (nil? previous) (println "  first run — no previous snapshot")
    (:read-error previous) (println "  previous snapshot unreadable:" (:read-error previous))
    :else
    (let [diff (topology-diff previous topo)]
      (if (empty? (:occupancy diff))
        (println "  occupancy: unchanged")
        (doseq [[line {:keys [gained lost]}] (:occupancy diff)]
          (println (format "  occupancy %-24s gained [%s], lost [%s]"
                           (name line) (fmt-items gained) (fmt-items lost)))))
      (println "  serves attached:" (fmt-items (:serves-attached diff)))
      (println "  serves detached:" (fmt-items (:serves-detached diff)))
      (println "  newly orphaned:" (fmt-items (:newly-orphaned diff)))
      (println "  no longer orphaned:" (fmt-items (:no-longer-orphaned diff)))
      (println (format "  components merged: %d; split: %d"
                       (get-in diff [:components :merged])
                       (get-in diff [:components :split]))))))

(let [{:keys [records files errors]} (load-records)
      {:keys [lines warning]} (load-budget-lines)
      lines-warning warning
      {assignments :assignments assignments-warning :warning} (load-assignments)
      warnings (concat errors (keep identity [lines-warning assignments-warning])
                       (when (zero? files) ["no records/*.edn files found"]))
      graph (derive-graph records lines assignments warnings)
      topo (topology graph)
      previous (read-snapshot)
      current-snapshot (snapshot graph)]
  (spit graph-file (str (pr-str graph) "\n"))
  (spit snapshot-file (str (pr-str current-snapshot) "\n"))
  (print-report graph topo previous))
