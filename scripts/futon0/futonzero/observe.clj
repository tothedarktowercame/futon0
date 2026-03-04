(ns futon0.futonzero.observe
  "Read futon1a hyperedges and futon3c evidence to produce FunctioningRecords.

   Invariant: D-I5 (read-only observer — no writes to stack).
   Pattern:   agent/evidence-over-assertion
   Theory:    futon-theory/local-gain-persistence"
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import (java.net URLEncoder)
           (java.time Instant)))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:dynamic *futon1a-url* "http://localhost:7071")
(def ^:dynamic *futon3c-url* "http://localhost:7070")

;; ---------------------------------------------------------------------------
;; HTTP helpers
;; ---------------------------------------------------------------------------

(defn- http-get
  "GET url, return parsed JSON body or nil on error."
  [url]
  (try
    (let [resp (http/get url {:headers {"Accept" "application/json"}
                              :timeout 10000
                              :throw false})]
      (when (= 200 (:status resp))
        (json/parse-string (:body resp) true)))
    (catch Exception e
      (binding [*out* *err*]
        (println "HTTP GET failed:" url (.getMessage e)))
      nil)))

;; ---------------------------------------------------------------------------
;; futon1a EDN response parsing
;; ---------------------------------------------------------------------------

(defn- http-get-edn
  "GET url expecting EDN response (futon1a). Return raw string."
  [url]
  (try
    (let [resp (http/get url {:timeout 10000 :throw false})]
      (when (= 200 (:status resp))
        (:body resp)))
    (catch Exception e
      (binding [*out* *err*]
        (println "HTTP GET (EDN) failed:" url (.getMessage e)))
      nil)))

(defn- parse-edn-hyperedges
  "Minimally parse futon1a EDN response to extract hyperedge maps.
   futon1a returns EDN like {:hyperedges [{:hx/id ... :hx/type ... :hx/endpoints [...] :hx/props {...}}]}.
   We parse with regex since bb's EDN reader handles keywords fine."
  [edn-str]
  (when edn-str
    (try
      (let [parsed (read-string edn-str)]
        (:hyperedges parsed))
      (catch Exception e
        (binding [*out* *err*]
          (println "EDN parse failed:" (.getMessage e)))
        nil))))

;; ---------------------------------------------------------------------------
;; HX type → functioning type mapping
;; ---------------------------------------------------------------------------

(def hx-type-mapping
  "Map hyperedge type keywords to functioning type strings."
  {:math/post              "math/post-created"
   :math/iatc              "math/argumentation-linked"
   :math/scope             "math/scope-defined"
   :math/scope-binding     "math/scope-defined"
   :code/namespace         "code/namespace-created"
   :code/var               "code/var-defined"
   :code/ns-contains-var   "code/dependency-structured"
   :code/requires          "code/dependency-structured"
   :project/devmap         "project/devmap-structured"
   :project/component      "project/devmap-structured"
   :project/devmap-contains "project/devmap-structured"
   :project/tension        "project/tension-detected"
   :project/tension-on     "project/tension-detected"
   :project/trace-path     "project/gate-traversed"})

(defn- hx-type->functioning-type
  "Convert a hyperedge type keyword to a functioning type string.
   Falls back to generic pattern for unknown types."
  [hx-type]
  (or (get hx-type-mapping hx-type)
      (let [s (name hx-type)
            [prefix suffix] (str/split s #"/" 2)]
        (str prefix "/" (or suffix s) "-recorded"))))

(defn- hx-type->column
  "Derive column from hyperedge type keyword."
  [hx-type]
  (let [prefix (namespace hx-type)]
    (case prefix
      "math"      :math
      "code"      :code
      "project"   :project
      "invariant" :cross-column
      :other)))

;; ---------------------------------------------------------------------------
;; Evidence type → functioning type mapping
;; ---------------------------------------------------------------------------

(def evidence-type-mapping
  "Map (evidence-type, claim-type) pairs to functioning types."
  {[:pattern-selection :goal]       "discipline/psr-filed"
   [:pattern-selection :question]   "discipline/psr-filed"
   [:pattern-outcome   :evidence]   "discipline/pur-filed"
   [:reflection        :conclusion] "discipline/par-completed"
   [:coordination      :goal]       "project/session-started"
   [:coordination      :conclusion] "project/session-completed"
   [:coordination      :step]       "project/tool-invoked"
   [:correction        :correction] "discipline/correction-made"
   [:gate-traversal    nil]         "project/gate-traversed"})

(defn- evidence->functioning-type
  "Derive functioning type from an evidence entry."
  [entry]
  (let [etype  (keyword (name (or (:evidence/type entry) "unknown")))
        ctype  (keyword (name (or (:evidence/claim-type entry) "unknown")))
        direct (get evidence-type-mapping [etype ctype])]
    (or direct
        (get evidence-type-mapping [etype nil])
        (str "evidence/" (name etype) "-" (name ctype)))))

;; ---------------------------------------------------------------------------
;; FunctioningRecord construction
;; ---------------------------------------------------------------------------

(defn- make-functioning
  "Create a FunctioningRecord map."
  [agent ftype subject column timestamp evidence-ref session-id conversion]
  {:id           (str (hash [agent ftype subject timestamp]))
   :agent        agent
   :type         ftype
   :subject      subject
   :column       column
   :timestamp    timestamp
   :evidence-ref evidence-ref
   :session-id   session-id
   :conversion   conversion})

;; ---------------------------------------------------------------------------
;; Observe hyperedges (futon1a)
;; ---------------------------------------------------------------------------

(def observable-hx-types
  "Hyperedge types to query from futon1a."
  ["math/post" "math/iatc" "math/scope" "math/scope-binding"
   "code/namespace" "code/var" "code/ns-contains-var" "code/requires"
   "project/devmap" "project/component" "project/devmap-contains"
   "project/tension" "project/tension-on" "project/trace-path"
   "invariant/undocumented-entry-point" "invariant/uncovered-component"
   "invariant/orphan-namespace" "invariant/ungrounded-definition"])

(defn observe-hyperedges
  "Query futon1a for all observable hyperedge types, return FunctioningRecords.
   Agent attribution for batch-ingested HX is 'infrastructure'."
  []
  (let [results (atom [])]
    (doseq [hx-type observable-hx-types]
      (let [url  (str *futon1a-url* "/api/alpha/hyperedges?type=" hx-type "&limit=1000")
            body (http-get-edn url)
            hxes (parse-edn-hyperedges body)]
        (doseq [hx hxes]
          (let [hx-kw    (:hx/type hx)
                ftype    (hx-type->functioning-type (if (keyword? hx-kw) hx-kw (keyword hx-type)))
                column   (hx-type->column (if (keyword? hx-kw) hx-kw (keyword hx-type)))
                endpoint (first (:hx/endpoints hx))
                created  (get-in hx [:hx/props :created-at] (str (Instant/now)))]
            (swap! results conj
                   (make-functioning "infrastructure" ftype endpoint column
                                    created (:hx/id hx) nil nil))))))
    @results))

;; ---------------------------------------------------------------------------
;; Observe evidence (futon3c)
;; ---------------------------------------------------------------------------

(defn observe-evidence
  "Query futon3c evidence store for an agent's entries in a time window.
   Returns FunctioningRecords."
  [agent since]
  (let [params (cond-> (str "limit=1000&author=" (URLEncoder/encode agent "UTF-8"))
                 since (str "&since=" since))
        url    (str *futon3c-url* "/api/alpha/evidence?" params)
        resp   (http-get url)]
    (when (and resp (:ok resp))
      (mapv (fn [entry]
              (let [ftype    (evidence->functioning-type entry)
                    subject  (or (get-in entry [:evidence/subject :ref/id])
                                 (:evidence/id entry))
                    col      (if (str/starts-with? ftype "discipline/")
                               :discipline
                               :project)]
                (make-functioning
                  agent ftype subject col
                  (:evidence/at entry)
                  (:evidence/id entry)
                  (:evidence/session-id entry)
                  ;; ConversionContext filled in at profile aggregation
                  nil)))
            (:entries resp)))))

(defn observe-all-agents
  "Query evidence store for all distinct authors, return map of agent → records."
  [since]
  (let [url  (str *futon3c-url* "/api/alpha/evidence?limit=1000"
                  (when since (str "&since=" since)))
        resp (http-get url)]
    (when (and resp (:ok resp))
      (let [agents (->> (:entries resp)
                        (map :evidence/author)
                        (remove nil?)
                        distinct)]
        (into {}
              (map (fn [agent]
                     [agent (observe-evidence agent since)])
                   agents))))))

;; ---------------------------------------------------------------------------
;; Combined observation
;; ---------------------------------------------------------------------------

(defn observe-agent
  "Produce all FunctioningRecords for an agent: evidence-derived + HX-derived.
   HX-derived records are attributed to 'infrastructure' unless agent matches."
  [agent since]
  (let [evidence-frs (observe-evidence agent since)
        hx-frs       (observe-hyperedges)]
    (concat (or evidence-frs []) hx-frs)))
