(ns futon0.rhythm.affect
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.net URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
           (java.time Instant)
           (java.util.regex Pattern)))

(defn- home []
  (System/getProperty "user.home"))

(defn- path [& parts]
  (str (io/file (apply str (interpose "/" parts)))))

(def ^:private default-output
  (path (home) "code/storage/futon0/vitality/affect.jsonl"))

(def ^:private default-wm-summary-output
  (path (home) "code/futon2/web/war-machine/resources/public/data/affect-events.json"))

(def ^:private default-evidence-url
  (or (System/getenv "FUTON3C_EVIDENCE_URL")
      "http://localhost:7070/api/alpha/evidence"))

(def ^:private default-lookback-hours 24)
(def ^:private default-lookahead-minutes 10)
(def ^:private default-overlap-minutes 15)
(def ^:private default-novelty-days 30)
(def ^:private default-max-pending 100)
(def ^:private default-summary-limit 8)
(def ^:private default-emotion-lexicon
  (path (home) "code/futon0/data/sentiment_emotion_lexicon.edn"))

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

(def ^:private false-positive-tokens
  #{"please" "pleas" "interesting" "interest"})

(def ^:private arrow-witness-markers
  #{"activation" "inspiration" "joy" "recognition" "regulation" "social"})

(def ^:private negation-tokens
  #{"not" "no" "never" "without" "hardly" "barely" "isn't" "isnt" "aren't"
    "arent" "wasn't" "wasnt" "weren't" "werent" "don't" "dont" "doesn't"
    "doesnt" "didn't" "didnt" "can't" "cant" "cannot" "won't" "wont"})

(def ^:private morphology-suffixes
  #{"s" "es" "ed" "er" "ers" "ing" "ion" "ions" "ive" "ively" "ment"
    "ments" "ness" "ful" "fully" "ous" "ously" "ity" "ities"})

(def ^:private default-event-intents
  {:inspiration
   {:event-type :inspiration
    :keywords #{"inspire" "inspired" "inspiring" "spark" "sparks" "sparked"
                "generative" "proposal" "propose" "idea" "extension"
                "extends" "opens"}
    :phrases #{"new proposal" "new idea" "generative spark" "this inspires"
               "that inspires" "opens a new" "suggests a new"
               "inspired a new" "inspired me to"}}
   :recognition
   {:event-type :recognition
    :keywords #{"accept" "accepted" "affirm" "affirmed" "confirm" "confirmed"
                "recognise" "recognize" "recognized" "yes" "right"
                "correct" "exactly" "solid"}
    :phrases #{"that's right" "that is right" "yes exactly" "looks right"
               "this works" "i accept" "i agree" "good catch"
               "solid work" "that lands"}}})

(defn- coerce-lexicon-set [xs]
  (->> xs (map (comp str/lower-case str)) (remove str/blank?) set))

(defn- normalize-event-intent [[k {:keys [event-type keywords phrases]}]]
  [k {:event-type (or event-type k)
      :keywords (coerce-lexicon-set (or keywords []))
      :phrases (coerce-lexicon-set (or phrases []))}])

(defn load-event-intents
  "Load the curated classical affect-event lexicon.
   G2 bound: this detector reads turn text. Resistance to authored/performed
   affect is out of scope for this classical layer."
  ([] (load-event-intents default-emotion-lexicon))
  ([file]
   (let [f (io/file file)]
     (if (.exists f)
       (->> (edn/read-string (slurp f))
            (map normalize-event-intent)
            (into {}))
       default-event-intents))))

(def ^:private event-intents
  (delay (merge default-event-intents (load-event-intents))))

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
    "  --incremental             Append only unseen transition ids"
    "  --overlap-minutes <n>     Incremental fetch overlap (default 15)"
    "  --refresh-wm-summary      Refresh WM affect-events.json from output"
    "  --wm-summary-output <p>   WM summary JSON path"
    "  --summary-limit <n>       Recent rows in WM summary (default 8)"
    "  --live                    Per-turn runner: --write --append"
    "                            --incremental --refresh-wm-summary"
    "  --append                  Append to output instead of overwrite"
    "  --write                   Write JSONL to output path"
    "  --help                    Show this help"
    ""]))

(defn- parse-args [args]
  (loop [args args
         opts {:evidence-url default-evidence-url
               :lookback-hours default-lookback-hours
               :lookahead-minutes default-lookahead-minutes
               :overlap-minutes default-overlap-minutes
               :novelty-days default-novelty-days
               :limit 1000
               :max-pending default-max-pending
               :output default-output
               :wm-summary-output default-wm-summary-output
               :summary-limit default-summary-limit}]
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
          (= "--incremental" arg) (recur (rest args) (assoc opts :incremental? true))
          (= "--overlap-minutes" arg) (recur (nnext args) (assoc opts :overlap-minutes (parse-long-safe (second args))))
          (= "--refresh-wm-summary" arg) (recur (rest args) (assoc opts :refresh-wm-summary? true))
          (= "--wm-summary-output" arg) (recur (nnext args) (assoc opts :wm-summary-output (second args)))
          (= "--summary-limit" arg) (recur (nnext args) (assoc opts :summary-limit (parse-long-safe (second args))))
          (= "--live" arg) (recur (rest args) (assoc opts
                                                     :write? true
                                                     :append? true
                                                     :incremental? true
                                                     :refresh-wm-summary? true))
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

(defn- token-negated? [tokens idx]
  (let [start (max 0 (- idx 3))]
    (boolean
     (some negation-tokens (subvec (vec tokens) start idx)))))

(defn- keyword-matches-token? [keyword token]
  (let [keyword (str/lower-case keyword)
        token (str/lower-case token)]
    (and (not (false-positive-tokens token))
         (or (= keyword token)
             (and (> (count keyword) 3)
                  (str/starts-with? token keyword)
                  (contains? morphology-suffixes (subs token (count keyword))))))))

(defn- token-keyword-match [keywords tokens idx]
  (let [token (nth tokens idx)]
    (when (and (not (token-negated? tokens idx))
               (some #(keyword-matches-token? % token) keywords))
      token)))

(defn- phrase-negated? [tokens phrase]
  (let [words (tokenize phrase)
        n (count words)]
    (boolean
     (some (fn [i]
             (and (= words (subvec (vec tokens) i (+ i n)))
                  (token-negated? tokens i)))
           (range 0 (inc (- (count tokens) n)))))))

(defn- phrase-match? [phrase lower-text tokens]
  (let [pattern (re-pattern (str "(?iu)(?<![\\p{L}\\p{Nd}_])"
                                 (Pattern/quote (str/lower-case phrase))
                                 "(?![\\p{L}\\p{Nd}_])"))]
    (and (re-find pattern lower-text)
         (not (phrase-negated? tokens phrase)))))

(defn- phrase-matches [phrases lower-text tokens]
  (->> phrases
       (filter #(phrase-match? % lower-text tokens))
       count))

(defn detect-affect
  "Detect one classical affect/event marker in TEXT, or nil.
   This is deliberately lexical and text-bound; authored/performed-affect
   resistance is out of scope for the classical layer."
  [text]
  (let [tokens (tokenize text)
        lower (str/lower-case (or text ""))
        intents (merge (into {} (map (fn [[k v]]
                                        [k (assoc v :event-type :affect-intent)])
                                      affect-intents))
                       @event-intents)]
    (->> intents
         (keep (fn [[intent {:keys [keywords phrases]}]]
                 (let [kw-triggers (keep-indexed (fn [idx _]
                                                   (token-keyword-match keywords tokens idx))
                                                 tokens)
                       phrase-count (phrase-matches (or phrases #{}) lower tokens)
                       kw-matches (count kw-triggers)
                       total (+ kw-matches phrase-count)]
                   (when (pos? total)
                     {:type intent
                      :event-type (get-in intents [intent :event-type] :affect-intent)
                      :matches total
                      :triggers (vec kw-triggers)
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
                             :event-type (name (:event-type affect))
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
      limit (conj (format "-n%d" (long limit))))))

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

(defn- load-affect-rows [file]
  (let [f (io/file file)]
    (if (.exists f)
      (load-jsonl-file file)
      [])))

(defn- transition-key [row]
  (or (:transition_id row)
      (:transition-id row)))

(defn- existing-transition-ids [file]
  (->> (load-affect-rows file)
       (keep transition-key)
       set))

(defn- latest-transition-ts [file]
  (->> (load-affect-rows file)
       (keep (comp parse-ts :timestamp))
       sort
       last))

(defn- incremental-since [output overlap-minutes]
  (when-let [latest (latest-transition-ts output)]
    (str (.minusSeconds latest (* 60 (long (or overlap-minutes default-overlap-minutes)))))))

(defn- prepare-incremental-opts [{:keys [incremental? since output overlap-minutes] :as opts}]
  (if (and incremental? (nil? since))
    (if-let [since* (incremental-since output overlap-minutes)]
      (assoc opts :since since*)
      opts)
    opts))

(defn- filter-new-transitions [existing-ids rows]
  (->> rows
       (remove #(contains? existing-ids (transition-key %)))
       vec))

(defn- arrow-candidate? [row]
  (and (contains? arrow-witness-markers (:marker row))
       (>= (double (or (:value row) 0.0)) 0.6)))

(defn- excerpt [text limit]
  (let [text (str/trim (str/replace (or text "") #"\s+" " "))
        limit (long (or limit 260))]
    (if (> (count text) limit)
      (subs text 0 limit)
      text)))

(defn- slim-summary-row [row]
  {:transition_id (transition-key row)
   :session_id (:session_id row)
   :source (:source row)
   :author (:author row)
   :timestamp (:timestamp row)
   :marker (:marker row)
   :value (:value row)
   :event_type (:event-type row)
   :candidate (arrow-candidate? row)
   :trigger_excerpt (excerpt (:trigger_text row) 260)})

(defn- marker-counts [rows]
  (->> rows
       (group-by :marker)
       (map (fn [[marker xs]]
              {:marker marker :count (count xs)}))
       (sort-by :marker)
       vec))

(defn affect-summary
  "Return the compact JSON shape consumed by the WM Affect Events pane."
  [rows {:keys [output summary-limit generated-at]}]
  (let [rows (vec rows)
        candidates (filter arrow-candidate? rows)
        latest-candidate (last (sort-by :timestamp candidates))
        recent (->> rows
                    (sort-by :timestamp)
                    reverse
                    (take (long (or summary-limit default-summary-limit)))
                    (mapv slim-summary-row))]
    {:generated_at (or generated-at (str (Instant/now)))
     :affect_path output
     :events_loaded (count rows)
     :candidate_events (count candidates)
     :marker_counts (marker-counts rows)
     :latest_candidate (some-> latest-candidate slim-summary-row)
     :recent recent}))

(defn refresh-wm-summary! [summary-output rows opts]
  (let [out (io/file summary-output)]
    (when-let [parent (.getParentFile out)]
      (.mkdirs parent))
    (spit out (str (json/write-str (affect-summary rows opts)) "\n"))
    (.getAbsolutePath out)))

(defn- load-evidence-entries [opts]
  (let [{:keys [entries-file evidence-url]} opts]
    (cond
      entries-file (load-evidence-file entries-file)
      (seq (:git-repos opts)) (load-git-entries opts)
      evidence-url (fetch-evidence-http evidence-url opts)
      :else (throw (ex-info "No evidence source configured"
                            {:hint "--entries-file or --evidence-url"})))))

(defn -main [& args]
  (let [{:keys [help?] :as parsed-opts} (parse-args args)
        {:keys [output write? append? incremental? refresh-wm-summary?
                wm-summary-output]
         :as opts} (prepare-incremental-opts parsed-opts)]
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
                          (trim-transitions opts))
          existing-ids (when incremental?
                         (existing-transition-ids output))
          write-transitions (if incremental?
                              (filter-new-transitions existing-ids transitions)
                              transitions)]
      (if write?
        (let [out (io/file output)]
          (.mkdirs (.getParentFile out))
          (write-jsonl out write-transitions (or append? incremental?))
          (println "wrote:" (.getAbsolutePath out))
          (println "entries:" (count entries)
                   "turns:" (count activities)
                   "transitions:" (count transitions)
                   "new:" (count write-transitions))
          (when refresh-wm-summary?
            (let [summary-path (refresh-wm-summary! wm-summary-output
                                                    (load-affect-rows output)
                                                    opts)]
              (println "wm-summary:" summary-path))))
        (doseq [entry transitions]
          (println (json/write-str entry)))))))
