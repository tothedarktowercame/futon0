#!/usr/bin/env bb

(require '[cheshire.core :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(import '(java.time Instant))

(defn usage []
  (println "Usage: recent-codex-session-selector.bb --since ISO --cwd PATH [--sessions-root PATH]")
  (println)
  (println "Prints matching Codex sessions as: session-id<TAB>file"))

(defn parse-args [args]
  (loop [opts {} remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--since" (recur (assoc opts :since (second remaining)) (nnext remaining))
        "--cwd" (recur (assoc opts :cwd (second remaining)) (nnext remaining))
        "--sessions-root" (recur (assoc opts :sessions-root (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- parse-instant [s]
  (try
    (Instant/parse s)
    (catch Exception _ nil)))

(defn- extract-message-text [message]
  (let [content (:content message)]
    (when (seq content)
      (->> content
           (map :text)
           (remove nil?)
           (apply str)
           str/trim))))

(defn- event-message-text [payload role]
  (let [event-type (:type payload)]
    (cond
      (and (= role "assistant") (= event-type "agent_message"))
      (or (:message payload) (:text payload))
      (and (= role "user") (= event-type "user_message"))
      (or (:message payload) (:text payload))
      :else nil)))

(defn- last-message-text [file role]
  (with-open [r (io/reader file)]
    (loop [lines (line-seq r)
           last-text nil]
      (if-let [line (first lines)]
        (let [m (try (json/parse-string line true)
                     (catch Exception _ nil))]
          (recur (rest lines)
                 (cond
                   (and m
                        (= "response_item" (:type m))
                        (= "message" (get-in m [:payload :type]))
                        (= role (get-in m [:payload :role])))
                   (or (extract-message-text (:payload m)) last-text)

                   (and m (= "event_msg" (:type m)))
                   (or (event-message-text (:payload m) role) last-text)

                   :else last-text)))
        last-text))))

(defn- session-preview [file]
  (let [text (or (last-message-text file "assistant")
                 (last-message-text file "user"))]
    (when text
      (let [preview (-> text
                        str/trim
                        (str/replace #"\s+" " "))]
        (subs preview 0 (min 200 (count preview)))))))

(defn- session-meta [file]
  (with-open [r (io/reader file)]
    (when-let [line (first (line-seq r))]
      (try
        (let [m (json/parse-string line true)]
          (when (= "session_meta" (:type m))
            {:file (.getAbsolutePath (io/file file))
             :session-id (get-in m [:payload :id])
             :timestamp (get-in m [:payload :timestamp])
             :cwd (get-in m [:payload :cwd])}))
        (catch Exception _ nil)))))

(defn- cwd-match? [target cwd]
  (or (= cwd target)
      (str/starts-with? cwd target)
      (str/starts-with? target cwd)))

(defn- matching-sessions [root since target-cwd]
  (let [since-ts (parse-instant since)]
    (->> (file-seq (io/file root))
         (filter #(.isFile ^java.io.File %))
         (filter #(str/ends-with? (.getName ^java.io.File %) ".jsonl"))
         (keep session-meta)
         (filter (fn [{:keys [timestamp cwd]}]
                   (and timestamp
                        (cwd-match? target-cwd cwd)
                        (when-let [ts (parse-instant timestamp)]
                          (if since-ts
                            (not (.isBefore ts since-ts))
                            true)))))
         (sort-by :timestamp))))

(defn -main [& args]
  (let [{:keys [help unknown since cwd sessions-root]} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      (or (nil? since) (nil? cwd)) (do (println "--since and --cwd are required") (usage) (System/exit 1))
      :else
      (let [root (or sessions-root
                     (str (io/file (System/getProperty "user.home") ".codex" "sessions")))]
        (doseq [{:keys [session-id file]} (matching-sessions root since cwd)]
          (let [preview (or (session-preview file) "")]
            (println (str (or session-id "") "\t" file "\t" preview))))))))

(apply -main *command-line-args*)
