(ns futon0.usage-ledger
  "Provider-neutral, append-only usage observation ledger for M-usage-hacking.

  Schema (one map per line, EDN):
    :schema        \"futon0.usage-observation/1\"
    :provider      :claude | :codex | :zai
    :account       opaque account/plan identity (no credentials)
    :observed-at   instant string, e.g. \"2026-07-13T21:00:00Z\"
    :source        provenance string naming the surface observed
    :window        {:id ... :duration-h N} native window identity
    :free          percentage free (number) or :unavailable
    :next-reset    instant string or :unavailable
    :certainty     :exact | :provider-reported | :inferred | :unavailable
    :invocations   active invocation count at observation time
    :workload      :foreground | :unattended
    :outcome-links durable outcome evidence references, may be empty

  Fail closed: an observation lacking any required field, or whose value
  is :unavailable where a policy decision needs it, never becomes
  permission to spend (mission invariant 1, M-usage-hacking §6)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def schema-version "futon0.usage-observation/1")

(def required-keys
  [:schema :provider :observed-at :source :window :certainty
   :invocations :workload])

(defn- windows-valid? [w]
  (let [ok-free #(or (number? %) (= :unavailable %))
        ok-reset #(or (string? %) (= :unavailable %))]
    (cond
      (map? w) (and (ok-free (:free w)) (ok-reset (:next-reset w)))
      (vector? w) (and (seq w) (every? #(and (:id %) (windows-valid? %)) w))
      :else false)))

(defn valid-observation?
  "True only when every required key is present and correctly typed."
  [obs]
  (and (map? obs)
       (= schema-version (:schema obs))
       (every? #(some? (get obs %)) required-keys)
       (contains? #{:claude :codex :zai} (:provider obs))
       (string? (:observed-at obs))
       (or (string? (:observed-at obs))
           (instance? java.lang.String (:observed-at obs)))
       (windows-valid? (:window obs))
       (contains? #{:exact :provider-reported :inferred :unavailable}
                  (:certainty obs))
       (contains? #{:foreground :unattended} (:workload obs))))

(defn- check-validity [obs]
  (if (valid-observation? obs)
    obs
    (throw (ex-info "observation fails schema"
                    {:kind :invalid-observation :observation obs}))))

(defn append!
  "Append observations to the ledger file. Every record is validated
   first; one invalid record refuses the whole append (atomic by
   refusal — nothing is written). Append-only: never rewrites lines."
  [path observations]
  (let [checked (mapv check-validity observations)]
    (spit path (str/join "\n" (map pr-str checked)) :append true)
    (spit path "\n" :append true))
  (count observations))

(defn- read-forms
  "Read every top-level EDN form from text, preserving order."
  [text]
  (let [rdr (java.io.PushbackReader. (java.io.StringReader. text))]
    (loop [acc []]
      (let [form (try (edn/read {:eof ::eof} rdr)
                      (catch Exception _ ::eof))]
        (if (= ::eof form)
          acc
          (recur (conj acc form)))))))

(defn read-all
  "Read every valid observation in file order. The file may hold one EDN
   vector of observations or several top-level observation maps. A
   malformed record is returned as {:kind :malformed-ledger-record ...}
   rather than dropped."
  [path]
  (let [forms (read-forms (slurp path))]
    (if (and (= 1 (count forms)) (vector? (first forms)))
      (mapv #(if (valid-observation? %) %
                 {:kind :malformed-ledger-record :record %})
            (first forms))
      (mapv #(if (valid-observation? %) %
                 {:kind :malformed-ledger-record :record %})
            forms))))
