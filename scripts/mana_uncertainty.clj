;;; mana_uncertainty.clj — pure uncertain-pressure load/validate/merge for
;;; mana-snapshot.bb and its tests (C8, M-inbox-zero-claim-lifecycle N4).
;;;
;;; Side-effect-free by construction: no IO at load time, no -main. The
;;; producer (mana-snapshot.bb) load-files this; tests load it directly.

(ns mana-uncertainty
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def default-uncertain-pressure-path
  "/home/joe/code/storage/inbox-zero/uncertain-pressure.edn")

(defn canonical-root [p]
  (try (str (fs/real-path p))
       (catch Throwable _ (str (fs/normalize p)))))

(defn- nat? [x] (and (integer? x) (not (neg? x))))

(defn- valid-row?
  "One feed row must carry a non-blank root, non-negative counts with
  untracked <= dirty, a non-negative remainder, and a :paths vector whose
  length equals :dirty-count (the complete per-file drilldown)."
  [row]
  (and (map? row)
       (string? (:root row)) (not (str/blank? (:root row)))
       (nat? (:dirty-count row))
       (nat? (:untracked row)) (<= (:untracked row) (:dirty-count row))
       (nat? (:remainder row))
       (vector? (:paths row)) (= (count (:paths row)) (:dirty-count row))))

(defn validate-feed
  "Nil when the feed value is chronologically and structurally valid;
  otherwise a typed invalidity keyword. Future or missing timestamps,
  non-positive intervals, malformed rows and duplicate canonical roots
  are all invalid — never silently 'fresh'."
  [v now]
  (cond
    (not (map? v)) :not-a-map
    (not (vector? (:repos v))) :repos-not-vector
    (not (instance? java.util.Date (:at v))) :missing-timestamp
    (> (.getTime ^java.util.Date (:at v)) (long now)) :future-timestamp
    (not (and (integer? (:interval-ms v)) (pos? (:interval-ms v))))
    :invalid-interval
    (not (every? valid-row? (:repos v))) :invalid-row
    (not= (count (:repos v))
          (count (distinct (map #(canonical-root (:root %)) (:repos v)))))
    :duplicate-root
    :else nil))

(defn load-uncertainty
  "Read and validate the sweeper's uncertain-pressure EDN. Returns
   {:status :available :generated-at :stale? :age-minutes :drilldown
    :diagnostics-available? :repos-by-root {canonical-root row}}
   or {:status :missing} / {:status :malformed :reason <kw>}. Staleness
   uses the feed's own configured :interval-ms (older than 2x is stale),
   never a hard-coded constant."
  ([path] (load-uncertainty path (System/currentTimeMillis)))
  ([path now]
   (if-not (fs/exists? path)
     {:status :missing}
     (try
       (let [v (edn/read-string (slurp path))]
         (if-let [reason (validate-feed v now)]
           {:status :malformed :reason reason}
           (let [gen-ms (.getTime ^java.util.Date (:at v))
                 interval (long (:interval-ms v))
                 age-ms (max 0 (- (long now) gen-ms))]
             {:status :available
              :generated-at (:at v)
              :age-minutes (double (/ age-ms 60000.0))
              :stale? (> age-ms (* 2 interval))
              :drilldown (:drilldown v)
              :diagnostics-available? (boolean (:diagnostics-available? v))
              :repos-by-root
              (into {}
                    (map (fn [row]
                           [(canonical-root (:root row))
                            (select-keys row [:label :dirty-count :untracked
                                              :remainder :paths])]))
                    (:repos v))})))
       (catch Throwable _ {:status :malformed :reason :unreadable})))))

(defn merge-uncertainty
  "UNION join: snapshot per-repo entries gain :uncertain by canonical
  root; feed roots absent from the manifest are appended as
  :uncertain-only rows carrying identity only (label, abs-path, the
  uncertain block) — never invented pressure/count measurements. Repos
  with no row get NO :uncertain key (absence is unknown, not zero)."
  [per-repo uncertainty]
  (if-not (= :available (:status uncertainty))
    per-repo
    (let [by-root (:repos-by-root uncertainty)
          stale? (:stale? uncertainty)
          tag (fn [m u]
                (cond-> (assoc m :uncertain u)
                  stale? (assoc :uncertain-stale true)))
          merged (mapv (fn [r]
                         (if-let [u (get by-root (canonical-root (:abs-path r)))]
                           (tag r u)
                           r))
                       per-repo)
          manifest-roots (set (map #(canonical-root (:abs-path %)) per-repo))
          extras (->> by-root
                      (remove (fn [[root _]] (contains? manifest-roots root)))
                      (mapv (fn [[root u]]
                              (tag {:repo (or (:label u) root)
                                    :abs-path root
                                    :uncertain-only true}
                                   u))))]
      (into merged extras))))
