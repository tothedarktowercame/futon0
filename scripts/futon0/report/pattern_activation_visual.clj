(ns futon0.report.pattern-activation-visual
  "2D hex-grid visualiser for pattern activation density.

   Sibling to war_machine_visual.clj. Each hex is a *pattern namespace*
   (e.g. `ants/`, `f3/`, `devmap-coherence/`); intensity encodes how
   often patterns in that namespace were retrieved during recent
   context-retrieval events.

   Two modes, toggled by a button:

     1. **Cumulative** — sum of all retrievals in the window (warm hue)
     2. **Decayed**    — sum with multiplicative daily decay so recent
                          activity dominates (cool hue). Decay rate
                          mirrors the futon2 pheromone decay shape
                          (multiplicative per tick). Default: 0.85/day.

   Data source: futon3c evidence log, context-retrieval events only.
   Launch:   clojure -M:pattern-activation [days]
   Defaults: 14-day window, decay rate 0.85, window 1000×700."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.string :as str])
  (:import [javax.swing JFrame JPanel JButton JLabel JToolBar
                        SwingUtilities BorderFactory]
           [java.awt Color Dimension Graphics Graphics2D Polygon
                     BasicStroke RenderingHints Font BorderLayout FlowLayout]
           [java.awt.event WindowAdapter ActionListener]
           [java.time LocalDate ZoneId]))

(def ^:private tz (ZoneId/of "Europe/London"))
(def ^:private sqrt3 (Math/sqrt 3.0))
(def ^:private default-decay-rate 0.85)

(def ^:private futon3c-url
  (or (System/getenv "FUTON3C_EVIDENCE_BASE")
      (System/getenv "FUTON3C_SERVER")
      (str "http://localhost:" (or (System/getenv "FUTON3C_PORT") "7070"))))

;; ---------- Fetch + aggregate ----------

(defn- fetch-events [limit]
  (let [url (str futon3c-url "/api/alpha/evidence?limit=" limit)
        resp (http/get url {:headers {"accept" "application/json"}
                            :throw false
                            :timeout 10000})]
    (when (= 200 (:status resp))
      (let [parsed (json/parse-string (:body resp) true)]
        (when (:ok parsed) (or (:entries parsed) []))))))

(defn- context-retrieval? [entry]
  (= "context-retrieval" (get-in entry [:evidence/body :event])))

(defn- entry-day [entry]
  (some-> (:evidence/at entry) (subs 0 10)))

(defn- pattern-namespace [pid]
  (if (str/includes? (str pid) "/")
    (first (str/split (str pid) #"/" 2))
    (str pid)))

(defn- retrieval-results [entry]
  (keep (fn [r] (or (:id r) (get r "id")))
        (get-in entry [:evidence/body :results])))

(defn- days-ago [day-str today]
  (try
    (let [d (LocalDate/parse day-str)]
      (.between java.time.temporal.ChronoUnit/DAYS d today))
    (catch Exception _ 0)))

(defn- aggregate
  "Returns {:namespaces {<ns> {:cumulative N :decayed D :recent-day <yyyy-mm-dd>}}
             :days-in-data N, :window-start, :window-end, :total-events}"
  [entries window-days decay-rate]
  (let [today (LocalDate/now tz)
        cutoff (.minusDays today (dec window-days))
        keep? (fn [entry]
                (when-let [d (entry-day entry)]
                  (try (not (.isBefore (LocalDate/parse d) cutoff))
                       (catch Exception _ false))))
        relevant (filter #(and (context-retrieval? %) (keep? %)) entries)]
    (loop [acc {} es relevant days-seen #{}]
      (if (empty? es)
        {:namespaces acc
         :days-seen days-seen
         :window-start (.toString cutoff)
         :window-end (.toString today)
         :total-events (count relevant)}
        (let [e (first es)
              day (entry-day e)
              ago (days-ago day today)
              weight (Math/pow decay-rate ago)
              pids (retrieval-results e)]
          (recur (reduce (fn [a pid]
                           (let [ns (pattern-namespace pid)]
                             (-> a
                                 (update-in [ns :cumulative] (fnil inc 0))
                                 (update-in [ns :decayed] (fnil + 0.0) weight)
                                 (update-in [ns :recent-day]
                                            (fn [r] (if (or (nil? r) (pos? (compare day r)))
                                                      day r))))))
                         acc pids)
                 (rest es)
                 (conj days-seen day)))))))

;; ---------- Hex geometry + spiral layout ----------

(defn- hex->pixel [q r size]
  [(* size sqrt3 (+ q (/ r 2.0)))
   (* size 1.5 r)])

(defn- hex-polygon ^Polygon [cx cy size]
  (let [poly (Polygon.)]
    (doseq [i (range 6)]
      (let [angle (+ (/ Math/PI 6.0) (* i (/ Math/PI 3.0)))
            x (+ cx (* size (Math/cos angle)))
            y (+ cy (* size (Math/sin angle)))]
        (.addPoint poly (int (Math/round x)) (int (Math/round y)))))
    poly))

(defn- spiral-coords
  "Generate axial (q,r) coordinates in a ring-expanding spiral around origin.
   Starting from the NE corner (ring, -ring) of each ring, walks six sides
   of length `ring` each: SE, SW, W, NW, NE, E."
  [n]
  (let [walk-dirs [[0 1] [-1 1] [-1 0] [0 -1] [1 -1] [1 0]]]
    (loop [out [[0 0]] ring 1]
      (if (>= (count out) n)
        (vec (take n out))
        (let [start-q ring
              start-r (- ring)
              ring-coords
              (loop [coords []
                     q start-q
                     r start-r
                     dir-idx 0]
                (if (= (count coords) (* 6 ring))
                  coords
                  (let [[dq dr] (nth walk-dirs dir-idx)]
                    (recur (conj coords [q r])
                           (+ q dq)
                           (+ r dr)
                           (if (zero? (mod (inc (count coords)) ring))
                             (mod (inc dir-idx) 6)
                             dir-idx)))))]
          (recur (into out ring-coords) (inc ring)))))))

(defn- assign-layout
  "Sort namespaces by total count descending; hottest goes in the centre,
   cooler namespaces spiral outward. Returns seq of {:ns :q :r :data}."
  [namespaces-map]
  (let [sorted (->> namespaces-map
                    (sort-by (fn [[_ d]] (- (:cumulative d 0))))
                    (map first))
        coords (spiral-coords (count sorted))]
    (mapv (fn [ns [q r]]
            {:ns ns :q q :r r :data (get namespaces-map ns)})
          sorted coords)))

;; ---------- Render ----------

(defn- lerp [a b t] (+ a (* (- b a) t)))

(defn- intensity->color
  "Return a Color with mode-specific hue, saturation varying with intensity."
  [mode intensity]
  (let [i (max 0.0 (min 1.0 intensity))
        hue (case mode
              :cumulative 0.08   ;; orange/warm
              :decayed    0.52)  ;; teal/cool
        sat (lerp 0.15 0.85 i)
        bri (lerp 0.35 0.95 i)]
    (Color/getHSBColor hue sat bri)))

(defn- namespace-size->font-size [hex-size]
  (max 9 (int (* hex-size 0.32))))

(defn- render-panel ^JPanel [state-atom]
  (proxy [JPanel] []
    (paintComponent [^Graphics g]
      (proxy-super paintComponent g)
      (let [g2 ^Graphics2D (.create g)
            w (.getWidth ^JPanel this)
            h (.getHeight ^JPanel this)
            {:keys [layout mode agg hex-size]} @state-atom
            maxv (case mode
                   :cumulative (apply max 1.0 (map #(get-in % [:data :cumulative] 0) layout))
                   :decayed    (apply max 1.0 (map #(get-in % [:data :decayed] 0.0) layout)))]
        (doto g2
          (.setRenderingHint RenderingHints/KEY_ANTIALIASING
                             RenderingHints/VALUE_ANTIALIAS_ON)
          (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING
                             RenderingHints/VALUE_TEXT_ANTIALIAS_ON))
        (.setColor g2 (Color. 18 22 32))
        (.fillRect g2 0 0 w h)
        (let [cx (/ w 2.0)
              cy (/ h 2.0)
              size (or hex-size 40)
              font-sm (Font. "Monospaced" Font/PLAIN (namespace-size->font-size size))]
          (doseq [{:keys [ns q r data]} layout]
            (let [value (case mode
                          :cumulative (:cumulative data 0)
                          :decayed    (:decayed data 0.0))
                  intensity (/ (double value) (double maxv))
                  [hx hy] (hex->pixel q r size)
                  x (+ cx hx)
                  y (+ cy hy)
                  poly (hex-polygon x y size)]
              (.setColor g2 (intensity->color mode intensity))
              (.fillPolygon g2 poly)
              (.setColor g2 (Color. 40 48 64))
              (.setStroke g2 (BasicStroke. 1.0))
              (.drawPolygon g2 poly)
              (.setFont g2 font-sm)
              (let [label (if (> (count ns) 14) (str (subs ns 0 13) "…") ns)
                    fm (.getFontMetrics g2)
                    lw (.stringWidth fm label)
                    label-color (if (> intensity 0.45) Color/BLACK Color/WHITE)]
                (.setColor g2 label-color)
                (.drawString g2 label (int (- x (/ lw 2))) (int (+ y 4)))
                (.setFont g2 (Font. "Monospaced" Font/PLAIN (max 8 (- (.getSize font-sm) 2))))
                (let [fm2 (.getFontMetrics g2)
                      count-str (case mode
                                  :cumulative (format "%d" value)
                                  :decayed    (format "%.1f" value))
                      cw (.stringWidth fm2 count-str)]
                  (.drawString g2 count-str
                               (int (- x (/ cw 2)))
                               (int (+ y (/ size 2) 2))))))))
        (.dispose g2)))))

;; ---------- UI ----------

(defn- build-toolbar [state-atom panel status-label]
  (let [bar (JToolBar.)
        btn-mode (JButton. "Cumulative")
        btn-refresh (JButton. "Refresh")
        update-btn (fn []
                     (.setText btn-mode
                               (case (:mode @state-atom)
                                 :cumulative "Mode: Cumulative"
                                 :decayed    "Mode: Decayed")))
        update-status (fn []
                        (let [{:keys [agg mode decay-rate window-days]} @state-atom]
                          (.setText status-label
                                    (format "  window=%dd  decay=%.2f/day  events=%d  namespaces=%d  mode=%s  "
                                            window-days
                                            (double decay-rate)
                                            (:total-events agg)
                                            (count (:namespaces agg))
                                            (name mode)))))]
    (update-btn)
    (.setFloatable bar false)
    (.addActionListener btn-mode
                        (reify ActionListener
                          (actionPerformed [_ _]
                            (swap! state-atom update :mode
                                   #(if (= % :cumulative) :decayed :cumulative))
                            (update-btn)
                            (update-status)
                            (.repaint panel))))
    (.addActionListener btn-refresh
                        (reify ActionListener
                          (actionPerformed [_ _]
                            (future
                              (let [entries (or (fetch-events 2000) [])
                                    agg (aggregate entries
                                                   (:window-days @state-atom)
                                                   (:decay-rate @state-atom))
                                    layout (assign-layout (:namespaces agg))]
                                (swap! state-atom assoc :agg agg :layout layout)
                                (SwingUtilities/invokeLater
                                 (fn []
                                   (update-status)
                                   (.repaint panel))))))))
    (.add bar btn-mode)
    (.add bar btn-refresh)
    (.add bar status-label)
    (update-status)
    bar))

(defn- visualize [window-days decay-rate]
  (let [entries (or (fetch-events 2000) [])
        agg (aggregate entries window-days decay-rate)
        layout (assign-layout (:namespaces agg))
        state (atom {:agg agg
                     :layout layout
                     :mode :cumulative
                     :window-days window-days
                     :decay-rate decay-rate
                     :hex-size 42})
        frame (JFrame. "Pattern Activation Density — 2D")
        panel (render-panel state)
        status (JLabel. "")
        toolbar (build-toolbar state panel status)]
    (.setPreferredSize panel (Dimension. 1000 700))
    (.setBorder panel (BorderFactory/createEmptyBorder 10 10 10 10))
    (doto frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setLayout (BorderLayout.))
      (.add toolbar BorderLayout/NORTH)
      (.add panel BorderLayout/CENTER)
      (.pack)
      (.setLocationRelativeTo nil)
      (.setVisible true))
    (when (empty? (:namespaces agg))
      (println (format "No retrieval data in the last %d days from %s"
                       window-days futon3c-url)))
    state))

(defn -main
  ([] (-main "14"))
  ([days-arg & _]
   (let [days (try (Integer/parseInt (str days-arg))
                   (catch Exception _ 14))]
     (SwingUtilities/invokeLater
      (fn [] (visualize days default-decay-rate))))))
