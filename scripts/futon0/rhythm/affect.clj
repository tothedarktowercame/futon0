(ns futon0.rhythm.affect
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.time Instant)))

(defn- home []
  (System/getProperty "user.home"))

(defn- path [& parts]
  (str (io/file (apply str (interpose "/" parts)))))

(def ^:private default-output
  (path (home) "code/storage/futon0/vitality/affect.jsonl"))

(def ^:private default-evidence-url
  (or (System/getenv "FUTON3C_EVIDENCE_URL")
      "http://localhost:7070/api/alpha/evidence"))

(def ^:private default-lookback-hours 24)
(def ^:private default-lookahead-minutes 10)
(def ^:private default-novelty-days 30)
(def ^:private default-max-pending 100)

(def ^:private affect-intents
  {:activation {:keywords #{"agitat" "amp" "anger" "angry" "annoy" "driv"
                            "determ" "fire" "furious" "irritat" "livid"
                            "mad" "motivat" "pressur" "rile" "resolv"
                            "resolut" "urgent" "urgency"}
                :phrases #{"amped up" "fed up" "fired up" "pissed off"
                           "riled up" "under pressure" "worked up"}}
   :attraction {:keywords #{"appeal" "attract" "captivat" "compel" "curios"
                            "draw" "engag" "fascinat" "interest" "intrigu"
                            "magnet" "pull" "tempt"}}
   :joy {:keywords #{"alive" "cheer" "content" "delight" "ease" "energise"
                     "energiz" "glad" "happy" "joy" "light" "play"
                     "pleas" "relax" "satisf" "uplift"}
         :phrases #{"open hearted" "open-hearted"}}
   :fatigue {:keywords #{"drain" "dull" "exhaust" "fatigu" "flat" "heavy"
                         "overload" "overwhelm" "sleepy" "sluggish" "spent"
                         "tired" "weary" "worn"}
             :phrases #{"burned out" "burnt out" "worn out"}}
   :anxiety {:keywords #{"afraid" "anxiety" "anxious" "apprehens" "concern"
                         "fear" "nervous" "panic" "pressur" "scared"
                         "stress" "tense" "uneasy" "worr"}
             :phrases #{"on edge"}}
   :withdrawal {:keywords #{"avoid" "block" "closed" "frozen" "guard"
                            "hesitat" "reluct" "reserved" "stuck" "withdraw"}
                :phrases #{"holding back" "shut down"}}
   :frustration {:keywords #{"annoy" "conflict" "exasperat" "frustrat"
                             "impatient" "irritat" "resent" "stymi"
                             "thwart" "torn"}}
   :sadness {:keywords #{"blue" "depress" "deflat" "disappoint" "discourag"
                         "down" "empty" "heavy" "hollow" "low" "sad"}
             :phrases #{"heavy-hearted"}}
   :numbness {:keywords #{"apathe" "blank" "detach" "disconnect" "indifferent"
                          "numb" "unmoved"}
              :phrases #{"blanked out" "checked out" "dead inside" "shut off"}}
   :orientation {:keywords #{"alert" "attentive" "aware" "certainty" "clarity"
                             "clear" "confus" "distract" "doubt" "focused"
                             "lost" "skeptic" "surpris" "uncertain" "unclear"}}
   :social {:keywords #{"accept" "appreciat" "belong" "connect" "dismiss"
                        "exclud" "ignored" "insecure" "lonely" "reject"
                        "seen" "secure" "support" "unseen" "valued"}}
   :regulation {:keywords #{"agitat" "balance" "calm" "equanim" "ground"
                            "overstimul" "restless" "settled" "unbalance"}}})

(defn- trim-base [s]
  (some-> s str/trim (str/replace #"/+$" "")))

(defn- parse-long-safe [value]
  (when (some? value)
    (try
      (Long/parseLong (str/trim (str value)))
      (catch Exception _ nil))))

(defn- encode-param [value]
  (URLEncoder/encode (str value) "UTF-8"))

(defn- query-string [params]
  (->> params
       (remove (comp nil? val))
       (map (fn [[k v]]
              (str (encode-param k) "=" (encode-param v))))
       (str/join "&")))

(defn- usage []
  (str/join
   \newline
   ["Usage:"
    "  clojure -M -m futon0.rhythm.affect --write"
    ""
    "Options:"
    "  --evidence-url <url>      Evidence Landscape query endpoint"
    "                            (default FUTON3C_EVIDENCE_URL or localhost:7070)"
    "  --entries-file <path>     Read evidence export from JSON/JSONL file"
    "  --git-repo <path>         Read commit history as fallback evidence"
    "  --author <name>           Filter by evidence author"
    "  --session-id <id>         Filter by evidence session id"
    "  --since <instant>         ISO-8601 lower bound"
    "  --until <instant>         ISO-8601 upper bound"
    "  --lookback-hours <n>      Default window when since/until absent"
    "  --lookahead-minutes <n>   Affect transition lookahead window"
    "  --novelty-days <n>        Novel-term freshness window"
    "  --limit <n>               Max entries to fetch from HTTP API"
    "  --max-transitions <n>     Trim emitted transitions"
    "  --max-terms <n>           Trim terms per transition"
    "  --output <path>           Output JSONL (default affect.jsonl)"
    "  --append                  Append to output instead of overwrite"
    "  --write                   Write JSONL to output path"
    "  --help                    Show this help"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:evidence-url default-evidence-url
               :lookback-hours default-lookback-hours
               :lookahead-minutes default-lookahead-minutes
               :novelty-days default-novelty-days
               :limit 1000
               :max-pending default-max-pending
               :output default-output}]
    (if (empty? args)
      opts
      (let [arg (first args)]
        (cond
          (= "--evidence-url" arg) (recur (nnext args) (assoc opts :evidence-url (second args)))
          (= "--entries-file" arg) (recur (nnext args) (assoc opts :entries-file (second args)))
          (= "--git-repo" arg) (recur (nnext args) (update opts :git-repos (fnil conj []) (second args)))
          (= "--author" arg) (recur (nnext args) (assoc opts :author (second args)))
          (= "--session-id" arg) (recur (nnext args) (assoc opts :session-id (second args)))
          (= "--since" arg) (recur (nnext args) (assoc opts :since (second args)))
          (= "--until" arg) (recur (nnext args) (assoc opts :until (second args)))
          (= "--lookback-hours" arg) (recur (nnext args) (assoc opts :lookback-hours (parse-long-safe (second args))))
          (= "--lookahead-minutes" arg) (recur (nnext args) (assoc opts :lookahead-minutes (parse-long-safe (second args))))
          (= "--novelty-days" arg) (recur (nnext args) (assoc opts :novelty-days (parse-long-safe (second args))))
          (= "--limit" arg) (recur (nnext args) (assoc opts :limit (parse-long-safe (second args))))
          (= "--max-transitions" arg) (recur (nnext args) (assoc opts :max-transitions (parse-long-safe (second args))))
          (= "--max-terms" arg) (recur (nnext args) (assoc opts :max-terms (parse-long-safe (second args))))
          (= "--output" arg) (recur (nnext args) (assoc opts :output (second args)))
          (= "--append" arg) (recur (rest args) (assoc opts :append? true))
          (= "--write" arg) (recur (rest args) (assoc opts :write? true))
          (= "--help" arg) (recur (rest args) (assoc opts :help? true))
          :else (throw (ex-info "Unknown argument" {:arg arg})))))))

(defn- parse-ts [value]
  (cond
    (instance? Instant value) value
    (string? value) (try
                      (Instant/parse value)
                      (catch Exception _ nil))
    :else nil))

(defn- now-minus-hours [hours]
  (.minusSeconds (Instant/now) (* 3600 (long hours))))

(defn- tokenize [text]
  (->> (re-seq #"[\p{L}\p{Nd}]+(?:'[\p{L}\p{Nd}]+)?" (or text ""))
       (map #(str/lower-case (apply str %)))
       vec))

(defn- matches-stem? [stem token]
  (cond
    (= stem token) true
    (<= (count stem) 3) false
    :else (str/starts-with? token stem)))

(defn- phrase-matches [phrases lower-text]
  (reduce (fn [acc phrase]
            (if (str/includes? lower-text (str/lower-case phrase))
              (inc acc)
              acc))
          0
          phrases))

(defn- detect-affect [text]
  (let [tokens (tokenize text)
        lower (str/lower-case (or text ""))]
    (->> affect-intents
         (keep (fn [[intent {:keys [keywords phrases]}]]
                 (let [kw-matches (reduce (fn [acc token]
                                            (if (some #(matches-stem? % token) keywords)
                                              (inc acc)
                                              acc))
                                          0
                                          tokens)
                       phrase-count (phrase-matches (or phrases #{}) lower)
                       total (+ kw-matches phrase-count)]
                   (when (pos? total)
                     {:type intent
                      :matches total
                      :conf (double (min 0.95 (+ 0.55 (* 0.07 total))))}))))
         (sort-by (juxt :matches :conf :type))
         last)))

(defn- extract-terms [text]
  (when text
    (->> (re-seq #"\b[a-zA-Z]{6,}\b" text)
         (map str/lower-case)
         distinct
         vec)))

(defn- novel-term? [history term ts novelty-days]
  (let [seen-at (get history term)
        cutoff (when seen-at
                 (.minusSeconds ts (* 86400 (long novelty-days))))]
    (or (nil? seen-at)
        (.isBefore seen-at cutoff))))

(defn- prune-expired [pending ts]
  (filterv (fn [{:keys [expires-at]}]
             (and expires-at (.isAfter expires-at ts)))
           pending))

(defn- emit-expired [state actor ts]
  (let [pending (get-in state [:pending actor] [])
        expired (filterv (fn [{:keys [expires-at terms]}]
                           (and expires-at
                                (not (.isAfter expires-at ts))
                                (seq terms)))
                         pending)
        remaining (filterv (fn [{:keys [expires-at]}]
                             (and expires-at (.isAfter expires-at ts)))
                           pending)
        transitions (mapv (fn [{:keys [affect terms event-id session-id author text ts]}]
                            {:timestamp (str ts)
                             :marker (name (:type affect))
                             :value (double (:conf affect))
                             :source "futon3c/evidence-affect-scan"
                             :transition_id event-id
                             :session_id session-id
                             :author author
                             :capacity_terms (vec (distinct terms))
                             :trigger_text text})
                          expired)]
    [(assoc-in state [:pending actor] remaining) transitions]))

(defn- flush-all-expired [state ts]
  (reduce (fn [[st out] actor]
            (let [[st* xs] (emit-expired st actor ts)]
              [st* (into out xs)]))
          [state []]
          (keys (:pending state))))

(defn- process-turn [state {:keys [actor ts text event-id session-id author]}
                     {:keys [lookahead-minutes novelty-days max-pending]}]
  (let [[state expired] (emit-expired state actor ts)
        affect (detect-affect text)
        history (get-in state [:term-history actor] {})
        novel (->> (extract-terms text)
                   (filterv #(novel-term? history % ts novelty-days)))
        state (if (seq novel)
                (reduce (fn [st term]
                          (assoc-in st [:term-history actor term] ts))
                        state
                        novel)
                state)
        state (if affect
                (let [expires-at (.plusSeconds ts (* 60 (long lookahead-minutes)))
                      item {:affect affect
                            :ts ts
                            :expires-at expires-at
                            :event-id event-id
                            :session-id session-id
                            :author author
                            :text text
                            :terms []}
                      current (-> (get-in state [:pending actor] [])
                                  (prune-expired ts)
                                  vec
                                  (conj item)
                                  (#(let [keep-from (max 0 (- (count %) (long max-pending)))]
                                      (subvec % keep-from))))]
                  (assoc-in state [:pending actor] current))
                state)
        state (if (seq novel)
                (update-in state [:pending actor]
                           (fn [items]
                             (mapv (fn [item]
                                     (if (.isAfter (:expires-at item) ts)
                                       (update item :terms into novel)
                                       item))
                                   (vec (or items [])))))
                state)]
    [state expired]))

(defn- build-query [opts]
  (let [{:keys [author session-id since until lookback-hours limit]} opts
        since (or since (when lookback-hours (str (now-minus-hours lookback-hours))))]
    (cond-> {"limit" (or limit 1000)}
      since (assoc "since" since)
      until (assoc "until" until)
      author (assoc "author" author)
      session-id (assoc "session-id" session-id))))

(defn- fetch-evidence-http [evidence-url opts]
  (let [base (trim-base evidence-url)
        query (build-query opts)
        url (str base "?" (query-string query))
        request (-> (HttpRequest/newBuilder (URI/create url))
                    (.header "Accept" "application/json")
                    (.GET)
                    .build)
        client (HttpClient/newHttpClient)
        response (.send client request (HttpResponse$BodyHandlers/ofString))
        status (.statusCode response)
        body (.body response)]
    (if (<= 200 status 299)
      (let [payload (json/read-str body :key-fn keyword)]
        (or (:entries payload)
            (when (vector? payload) payload)
            []))
      (throw (ex-info "Evidence API request failed"
                      {:status status :body body :url url})))))

(defn- git-log-cmd [opts]
  (let [{:keys [since until author limit]} opts]
    (cond-> ["git" "log" "--date=iso-strict"
             "--pretty=format:%H%x09%aI%x09%an%x09%s%x09%b"]
      since (conj (str "--since=" since))
      until (conj (str "--until=" until))
      author (conj (str "--author=" author))
      limit (conj (str (format "-n%d" (long limit)))))))

(defn- git-commit->entry [repo-root line]
  (let [[hash ts author subject body] (str/split line #"\t" 5)
        text (str/trim (str subject
                             (when (seq (str/trim (or body "")))
                               (str "\n" body))))]
    (when (and (seq hash) (seq ts) (seq text))
      {:evidence/id hash
       :evidence/at ts
       :evidence/author author
       :evidence/type :coordination
       :evidence/claim-type :observation
       :evidence/session-id (str "git/" (.getName (io/file repo-root)))
       :evidence/body {:event "git-commit"
                       :text text
                       :repo (.getName (io/file repo-root))
                       :hash hash}
       :evidence/tags [:git :commit]})))

(defn- load-git-entries [opts]
  (let [repos (:git-repos opts)]
    (->> repos
         (mapcat (fn [repo]
                   (let [{:keys [exit out err]} (apply shell/sh (concat (git-log-cmd opts) [:dir repo]))]
                     (when-not (zero? exit)
                       (throw (ex-info "git log failed" {:repo repo :err err})))
                     (->> (str/split-lines out)
                          (remove str/blank?)
                          (keep #(git-commit->entry repo %))))))
         (sort-by (juxt :evidence/at :evidence/id))
         vec)))

(defn- load-json-file [path]
  (let [payload (json/read-str (slurp path) :key-fn keyword)]
    (cond
      (vector? payload) payload
      (map? payload) (or (:entries payload) [])
      :else [])))

(defn- load-jsonl-file [path]
  (with-open [r (io/reader path)]
    (->> (line-seq r)
         (remove str/blank?)
         (map #(json/read-str % :key-fn keyword))
         vec)))

(defn- load-evidence-file [path]
  (try
    (load-json-file path)
    (catch Exception _
      (load-jsonl-file path))))

(defn- extract-body [entry]
  (or (:evidence/body entry)
      (:body entry)
      {}))

(defn- entry-text [entry]
  (let [body (extract-body entry)]
    (or (:text body)
        (:prompt body)
        (:response body)
        (:content body)
        (:message body)
        (:title body))))

(defn- turn-like-entry? [entry]
  (let [claim-type (or (:evidence/claim-type entry) (:claim-type entry))
        ev-type (or (:evidence/type entry) (:type entry))
        body (extract-body entry)
        event (:event body)]
    (and (string? (entry-text entry))
         (not (str/blank? (entry-text entry)))
         (or (= event "chat-turn")
             (= event :chat-turn)
             (contains? #{:question :observation :correction} claim-type)
             (= :forum-post ev-type)))))

(defn- entry->activity [entry]
  (let [body (extract-body entry)
        ts (parse-ts (or (:evidence/at entry) (:at entry)))
        text (entry-text entry)
        actor (or (:evidence/author entry) (:author entry) "unknown")
        event-id (or (:evidence/id entry) (:id entry))
        session-id (or (:evidence/session-id entry) (:session-id entry))]
    (when (and ts (string? text) (not (str/blank? text)))
      {:actor actor
       :author actor
       :ts ts
       :text text
       :event-id event-id
       :session-id session-id
       :body-event (:event body)})))

(defn- sort-activities [entries]
  (->> entries
       (map entry->activity)
       (remove nil?)
       (sort-by (juxt :ts :event-id))
       vec))

(defn- trim-transitions [entries {:keys [max-transitions max-terms]}]
  (let [entries (if (and max-transitions (pos? max-transitions))
                  (->> entries (take-last (long max-transitions)) vec)
                  (vec entries))]
    (if (and max-terms (pos? max-terms))
      (mapv #(update % :capacity_terms
                     (fn [terms]
                       (vec (take (long max-terms) (or terms [])))))
            entries)
      entries)))

(defn- write-jsonl [file rows append?]
  (with-open [w (io/writer file :append append?)]
    (doseq [row rows]
      (json/write row w)
      (.write w "\n"))))

(defn- load-evidence-entries [opts]
  (let [{:keys [entries-file evidence-url]} opts]
    (cond
      entries-file (load-evidence-file entries-file)
      (seq (:git-repos opts)) (load-git-entries opts)
      evidence-url (fetch-evidence-http evidence-url opts)
      :else (throw (ex-info "No evidence source configured"
                            {:hint "--entries-file or --evidence-url"})))))

(defn -main [& args]
  (let [{:keys [output write? append? help?] :as opts} (parse-args args)]
    (when help?
      (println (usage))
      (System/exit 0))
    (let [entries (load-evidence-entries opts)
          activities (->> entries
                          (filter turn-like-entry?)
                          sort-activities)
          [state transitions] (reduce (fn [[st out] activity]
                                        (let [[st* emitted] (process-turn st activity opts)]
                                          [st* (into out emitted)]))
                                      [{:pending {} :term-history {}} []]
                                      activities)
          flush-ts (.plusSeconds (Instant/now) (* 60 (long (:lookahead-minutes opts))))
          [_ flushed] (flush-all-expired state flush-ts)
          transitions (-> (into transitions flushed)
                          (trim-transitions opts))]
      (if write?
        (let [out (io/file output)]
          (.mkdirs (.getParentFile out))
          (write-jsonl out transitions append?)
          (println "wrote:" (.getAbsolutePath out))
          (println "entries:" (count entries) "turns:" (count activities) "transitions:" (count transitions)))
        (doseq [entry transitions]
          (println (json/write-str entry)))))))
