(ns futon0.rhythm.affect-detector
  "Pure, replayable affect detector shared by the scanner and provenance gates."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.util.regex Pattern)))

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

(defn- default-emotion-lexicon []
  (str (io/file (System/getProperty "user.home")
                "code/futon0/data/sentiment_emotion_lexicon.edn")))

(defn- coerce-lexicon-set [xs]
  (->> xs (map (comp str/lower-case str)) (remove str/blank?) set))

(defn- normalize-event-intent [[k {:keys [event-type keywords phrases]}]]
  [k {:event-type (or event-type k)
      :keywords (coerce-lexicon-set (or keywords []))
      :phrases (coerce-lexicon-set (or phrases []))}])

(defn load-event-intents
  "Load the curated classical affect-event lexicon."
  ([] (load-event-intents (default-emotion-lexicon)))
  ([file]
   (let [f (io/file file)]
     (if (.exists f)
       (->> (edn/read-string (slurp f))
            (map normalize-event-intent)
            (into {}))
       default-event-intents))))

(def ^:private event-intents
  (delay (merge default-event-intents (load-event-intents))))

(defn- tokenize [text]
  (->> (re-seq #"[\p{L}\p{Nd}]+(?:'[\p{L}\p{Nd}]+)?" (or text ""))
       (map #(str/lower-case (apply str %)))
       vec))

(defn- token-negated? [tokens idx]
  (let [start (max 0 (- idx 3))]
    (boolean (some negation-tokens (subvec (vec tokens) start idx)))))

(defn- keyword-matches-token? [keyword token]
  (let [keyword (str/lower-case keyword)
        token (str/lower-case token)]
    (and (not (false-positive-tokens token))
         (or (= keyword token)
             (and (> (count keyword) 3)
                  (str/starts-with? token keyword)
                  (contains? morphology-suffixes
                             (subs token (count keyword))))))))

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
  "Detect one classical affect/event marker in text, or nil."
  [text]
  (let [tokens (tokenize text)
        lower (str/lower-case (or text ""))
        intents (merge (into {} (map (fn [[k v]]
                                       [k (assoc v :event-type :affect-intent)])
                                     affect-intents))
                       @event-intents)]
    (->> intents
         (keep (fn [[intent {:keys [keywords phrases]}]]
                 (let [kw-triggers (keep-indexed
                                    (fn [idx _]
                                      (token-keyword-match keywords tokens idx))
                                    tokens)
                       phrase-count (phrase-matches (or phrases #{}) lower tokens)
                       kw-matches (count kw-triggers)
                       total (+ kw-matches phrase-count)]
                   (when (pos? total)
                     {:type intent
                      :event-type (get-in intents [intent :event-type]
                                          :affect-intent)
                      :matches total
                      :triggers (vec kw-triggers)
                      :conf (double (min 0.95 (+ 0.55 (* 0.07 total))))}))))
         (sort-by (juxt :matches :conf :type))
         last)))
