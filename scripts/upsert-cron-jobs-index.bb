#!/usr/bin/env bb
;; upsert-cron-jobs-index.bb — mirror data/cron-jobs.edn into the authoritative
;; Futon substrate
;; as a typed hyperedge so the registry is runtime-queryable.
;;
;; Usage:
;;   bb scripts/upsert-cron-jobs-index.bb
;;
;; Reads the canonical EDN at /home/joe/code/futon0/data/cron-jobs.edn and
;; POSTs a hyperedge of type "arxana/index/cron-jobs" through the configured
;; substrate API (default Futon1b at http://127.0.0.1:7073). Endpoints are per-job entity ids
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
         '[clojure.edn :as edn]
         '[clojure.string :as str])

(def registry-path "/home/joe/code/futon0/data/cron-jobs.edn")
(def substrate-base
  (-> (or (System/getenv "FUTON_SUBSTRATE_URL")
          "http://127.0.0.1:7073")
      (str/replace #"/+$" "")
      (str/replace #"/api/alpha$" "")))
(def penholder
  (or (System/getenv "CRON_INDEX_PENHOLDER") "api"))
(def hyperedge-type "arxana/index/cron-jobs")

(defn post!
  [path payload]
  (let [url (str substrate-base "/api/alpha" path)
        resp (http/post url
                        {:headers {"Content-Type" "application/edn"
                                   "Accept" "application/edn"
                                   "x-penholder" penholder}
                         :body (pr-str payload)
                         :timeout 5000
                         :throw false})]
    (if (<= 200 (long (:status resp)) 299)
      (:body resp)
      (throw (ex-info "authoritative substrate write failed"
                      {:url url :status (:status resp) :body (:body resp)})))))

(defn -main [& _]
  (let [registry (edn/read-string (slurp registry-path))
        jobs (:jobs registry)
        endpoints (mapv #(str "cron-job:" (name (:job-id %))) jobs)
        _entities (mapv (fn [job endpoint]
                          (post! "/entity"
                                 {:id endpoint
                                  :name (str "Cron job " (name (:job-id job)))
                                  :type :cron/job
                                  :source "futon0/data/cron-jobs.edn"
                                  :props job}))
                        jobs endpoints)
        payload {:hx/type hyperedge-type
                 :hx/endpoints endpoints
                 :hx/props (assoc registry
                                  :provenance/author "upsert-cron-jobs-index.bb"
                                  :provenance/source-file registry-path
                                  :provenance/upserted-at (str (java.time.Instant/now)))}
        result (post! "/hyperedge" payload)]
    (println "OK upserted" hyperedge-type
             "with" (count endpoints) "grounded endpoints")
    (println "  " result)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
