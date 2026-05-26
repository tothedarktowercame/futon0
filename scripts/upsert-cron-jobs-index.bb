#!/usr/bin/env bb
;; upsert-cron-jobs-index.bb — mirror data/cron-jobs.edn into futon1a XTDB
;; as a typed hyperedge so the registry is runtime-queryable.
;;
;; Usage:
;;   bb scripts/upsert-cron-jobs-index.bb
;;
;; Reads the canonical EDN at /home/joe/code/futon0/data/cron-jobs.edn and
;; POSTs a hyperedge of type "arxana/index/cron-jobs" to futon1a (default
;; http://localhost:7071/api/alpha). Endpoints are per-job entity ids
;; (`cron-job:JOB-ID`), one for each :job-id in the registry; props carry
;; the full EDN content.
;;
;; The hyperedge type itself (`arxana/index/cron-jobs`) is the discoverable
;; entry point — a future Arxana browser chrome can render this without
;; needing to know any specific entity id.
;;
;; Re-run anytime data/cron-jobs.edn changes. Each run creates a fresh
;; hyperedge (typed-by-content); querying type returns all historical
;; snapshots, latest first by :hx/at. The latest is the authoritative
;; mirror.

(require '[babashka.http-client :as http]
         '[cheshire.core :as json]
         '[clojure.edn :as edn])

(def registry-path "/home/joe/code/futon0/data/cron-jobs.edn")
(def store-base
  (or (System/getenv "FUTON1A_BASE_URL") "http://localhost:7071/api/alpha"))
(def penholder
  (or (System/getenv "CRON_INDEX_PENHOLDER") "api"))
(def hyperedge-type "arxana/index/cron-jobs")

(defn -main [& _]
  (let [registry (edn/read-string (slurp registry-path))
        jobs (:jobs registry)
        endpoints (mapv #(str "cron-job:" (name (:job-id %))) jobs)
        payload {:penholder penholder
                 :hx/type hyperedge-type
                 :hx/endpoints endpoints
                 :props (assoc registry
                               :provenance/author "upsert-cron-jobs-index.bb"
                               :provenance/source-file registry-path
                               :provenance/upserted-at (str (java.time.Instant/now)))}
        url (str store-base "/hyperedge")
        resp (http/post url
                        {:headers {"Content-Type" "application/json"
                                   "Accept" "application/json"}
                         :body (json/generate-string payload)
                         :timeout 5000
                         :throw false})]
    (case (long (:status resp))
      (200 201)
      (do (println "OK upserted" hyperedge-type
                   "with" (count endpoints) "endpoints")
          (println "  " (:body resp))
          (System/exit 0))
      ;; else
      (do (binding [*out* *err*]
            (println "ERROR status=" (:status resp))
            (println "  body:" (:body resp)))
          (System/exit 1)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
