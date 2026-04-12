(ns futon0.report.war-machine-visual
  "War Machine Hex Petri Dish — strategic state visualiser.

   A hex-grid RPG-map-style display of the futon stack's strategic state.
   Each hex is a 'sprite' representing a repo, sorry, tick, or loop arrow.
   Color encodes type and workstream; intensity encodes activity (hotspot).
   Edges show temporal coupling between repos (cf. code-maat).

   Architecture follows ants/visual.clj:
     make-panel   → JPanel with custom paintComponent
     make-stats   → sidebar with metrics
     visualize    → orchestrate frame + data refresh

   But instead of a tick-by-tick simulation, this is a snapshot viewer:
   it reads scan data once (or refreshes on demand) and renders the
   strategic state as a hex map.

   Hex geometry: pointy-top hexagons, axial coordinates (q, r).
   Layout: workstream clusters with sorrys at boundaries.

   cf. code-maat: hotspot = churn × complexity
       here: hotspot = commit count (intensity)
   cf. code-compass: temporal coupling edges
       here: Jaccard co-change between repos

   Invariant: WM-I1 (read-only observer — display only).
   Pattern:   war-machine/spatial-over-tabular"
  (:require [futon0.report.war-machine :as wm]
            [clojure.string :as str])
  (:import [javax.swing JFrame JPanel JLabel SwingUtilities JButton JToolBar
                        JTextArea JScrollPane BorderFactory JCheckBox Timer]
           [java.awt Color Dimension Graphics Graphics2D Polygon
                     BasicStroke RenderingHints Font BorderLayout]
           [java.awt.event WindowAdapter ActionListener MouseAdapter ComponentAdapter]
           [java.awt.geom Path2D$Double]))

;; ---------------------------------------------------------------------------
;; Hex Geometry (pointy-top)
;;
;; Pointy-top hex: flat sides on left/right, points on top/bottom.
;; Axial coordinates (q, r) with offset conversion for rendering.
;;
;;   Width  = sqrt(3) * size
;;   Height = 2 * size
;;   Horizontal spacing = sqrt(3) * size
;;   Vertical spacing   = 1.5 * size
;;   Odd-row offset     = sqrt(3)/2 * size
;; ---------------------------------------------------------------------------

(def ^:private sqrt3 (Math/sqrt 3.0))

(defn- hex->pixel
  "Convert axial hex coordinates (q, r) to pixel center (x, y).
   Uses odd-r offset layout (odd rows shifted right)."
  [q r size]
  (let [x (+ (* size sqrt3 q)
              (if (odd? r) (* size (/ sqrt3 2.0)) 0.0))
        y (* size 1.5 r)]
    [(+ x (* size (/ sqrt3 2.0)))  ;; center offset
     (+ y size)]))                  ;; center offset

(defn- hex-polygon
  "Create a Polygon for a pointy-top hexagon centered at (cx, cy)."
  [cx cy size]
  (let [poly (Polygon.)]
    (doseq [i (range 6)]
      (let [angle (Math/toRadians (- (* 60.0 i) 30.0))
            px (+ cx (* size (Math/cos angle)))
            py (+ cy (* size (Math/sin angle)))]
        (.addPoint poly (int px) (int py))))
    poly))

(defn- hex-path
  "Create a Path2D for a pointy-top hexagon (smoother rendering)."
  [cx cy size]
  (let [path (Path2D$Double.)]
    (doseq [i (range 7)] ;; 7 to close the path
      (let [angle (Math/toRadians (- (* 60.0 (mod i 6)) 30.0))
            px (+ cx (* size (Math/cos angle)))
            py (+ cy (* size (Math/sin angle)))]
        (if (zero? i)
          (.moveTo path px py)
          (.lineTo path px py))))
    (.closePath path)
    path))

;; ---------------------------------------------------------------------------
;; Color Palettes
;;
;; Each sprite type has a color family. Intensity modulates alpha/brightness
;; to encode activity level (code-maat hotspot concept).
;; ---------------------------------------------------------------------------

(defn- workstream-color
  "Base color for a workstream."
  [ws]
  (case ws
    :stack       (Color. 74 144 226)   ;; blue
    :mathematics (Color. 34 197 94)    ;; green
    :portfolio   (Color. 234 179 8)    ;; gold
    :consulting  (Color. 239 68 68)    ;; red
    (Color. 156 163 175)))             ;; grey fallback

(defn- workstream-bg
  "Light background for workstream region."
  [ws]
  (case ws
    :stack       (Color. 219 234 254)  ;; light blue
    :mathematics (Color. 220 252 231)  ;; light green
    :portfolio   (Color. 254 249 195)  ;; light gold
    :consulting  (Color. 254 226 226)  ;; light red
    (Color. 243 244 246)))             ;; light grey

(defn- severity-color
  "Color for sorry severity."
  [severity]
  (case severity
    :critical (Color. 185 28 28)       ;; deep red
    :warning  (Color. 217 119 6)       ;; amber
    :info     (Color. 59 130 246)      ;; blue
    (Color. 107 114 128)))             ;; grey

(defn- health-color
  "Border color from health value [0,1]."
  [health]
  (let [h (max 0.0 (min 1.0 (double health)))
        r (int (* 255 (- 1.0 h)))
        g (int (* 255 h))]
    (Color. r g 80)))

(defn- activity-alpha
  "Alpha value from commit count. More commits = more opaque."
  [commits max-commits]
  (if (and max-commits (pos? max-commits))
    (let [ratio (/ (double commits) (double max-commits))]
      (int (+ 80 (* 175 (min 1.0 ratio)))))
    80))

(defn- mission-status-color
  "Color for mission by status."
  [status]
  (case (str status)
    "blocked"                     (Color. 239 68 68)    ;; red
    ("active" "in-progress" "in_progress") (Color. 59 130 246) ;; blue
    "testing"                     (Color. 168 85 247)   ;; purple
    "ready"                       (Color. 34 197 94)    ;; green
    "complete"                    (Color. 180 210 180)  ;; pale green
    "deferred"                    (Color. 200 180 140)  ;; tan
    (Color. 200 200 210)))                               ;; light grey for unknown

(defn- tick-color
  "Color for pocketwatch tick."
  [fired?]
  (if fired?
    (Color. 249 115 22)                ;; orange (firing!)
    (Color. 156 163 175)))             ;; grey (quiet)

(defn- loop-arrow-color
  "Color for loop health arrow."
  [health]
  (let [h (max 0.0 (min 1.0 (double health)))]
    (if (> h 0.5)
      (Color. 20 (int (+ 150 (* 105 h))) 100)   ;; green spectrum
      (Color. (int (+ 100 (* 155 (- 1.0 h)))) 80 80)))) ;; red spectrum

;; ---------------------------------------------------------------------------
;; Layout: tight hex cluster, most-connected nodes in the center
;;
;; All nodes packed into one adjacent hex cluster using hex-spiral placement.
;; Nodes sorted by connectivity (coupling edge count + commits) so the
;; most-connected repos get central positions.
;;
;; cf. code-maat: hotspot = most-changed files get visual prominence
;; ---------------------------------------------------------------------------

(defn- hex-spiral
  "Generate hex grid positions in a spiral from center outward.
   Returns a lazy seq of [q r] coordinates."
  []
  (concat
   [[0 0]]
   (mapcat
    (fn [ring]
      (let [;; Six directions for hex neighbours (pointy-top, odd-r offset)
            dirs [[1 0] [0 1] [-1 1] [-1 0] [0 -1] [1 -1]]]
        (for [side (range 6)
              step (range ring)
              :let [[dq dr] (nth dirs side)
                    ;; Start position for this ring
                    start-q ring
                    start-r 0
                    ;; Walk around the ring
                    [sq sr] (reduce (fn [[q r] _]
                                      [(+ q (first (nth dirs (mod (- side 1) 6))))
                                       (+ r (second (nth dirs (mod (- side 1) 6))))])
                                    [start-q start-r]
                                    (range side))
                    q (+ sq (* step dq))
                    r (+ sr (* step dr))]]
          [q r])))
    (range 1 20))))

(defn- hex-neighbours
  "Return the 6 hex grid neighbours of (q, r) in odd-r offset layout."
  [q r]
  (if (odd? r)
    [[(inc q) r] [q (inc r)] [(dec q) (inc r)]
     [(dec q) r] [(dec q) (dec r)] [q (dec r)]]
    [[(inc q) r] [(inc q) (inc r)] [q (inc r)]
     [(dec q) r] [q (dec r)] [(inc q) (dec r)]]))

(defn- assign-layout
  "Pack all nodes into a tight hex cluster.

   Placement order: most-connected repos first (center), then
   less-connected repos, then sorrys, then ticks.
   Each node is placed at the nearest free hex adjacent to an
   already-placed node, keeping the cluster tight."
  [graph-data]
  (let [{:keys [repos sorrys]} (:nodes graph-data)
        ticks (get-in graph-data [:dynamics :ticks])
        coupling (get-in graph-data [:edges :temporal-coupling] [])
        ;; Count coupling edges per repo
        edge-count (reduce (fn [m {:keys [from to]}]
                             (-> m
                                 (update from (fnil inc 0))
                                 (update to (fnil inc 0))))
                           {}
                           coupling)
        ;; Score: coupling edges * 10 + commits (most connected = highest)
        score (fn [node]
                (+ (* 10 (get edge-count (:id node) 0))
                   (or (:commits node) 0)))
        ;; Sort all nodes: repos by score desc, then sorrys, then ticks
        all-nodes (concat
                   (->> repos
                        (map #(assoc % :sprite-type :repo))
                        (sort-by score >))
                   (->> (or sorrys [])
                        (map #(assoc % :sprite-type :sorry)))
                   (->> (or ticks [])
                        (map #(assoc % :sprite-type :tick))))
        ;; Place nodes using greedy adjacency: each new node goes to the
        ;; free hex nearest to the cluster centroid that is adjacent to
        ;; an already-placed node.
        occupied (atom #{})
        cells (atom [])
        place! (fn [node q r]
                 (swap! occupied conj [q r])
                 (swap! cells conj {:node node :q q :r r}))]
    ;; Place first node at origin
    (when-let [first-node (first all-nodes)]
      (place! first-node 0 0))
    ;; Place remaining nodes
    ;; Affinity: prefer placing near same-type siblings (keeps ticks together,
    ;; sorrys together) while still growing the cluster outward.
    (let [type-anchors (atom {})] ;; sprite-type → first placed [q r]
      (doseq [node (rest all-nodes)]
        (let [st (:sprite-type node)
              ;; Find all free hexes adjacent to the current cluster
              frontier (->> @occupied
                            (mapcat (fn [[q r]] (hex-neighbours q r)))
                            distinct
                            (remove @occupied))
              ;; Anchor: prefer positions near same-type siblings
              anchor (get @type-anchors st)
              ;; Target: if we have a same-type anchor, bias toward it;
              ;; otherwise use cluster centroid
              [target-q target-r]
              (if anchor
                anchor
                [(if (seq @cells)
                   (/ (reduce + (map :q @cells)) (double (count @cells)))
                   0.0)
                 (if (seq @cells)
                   (/ (reduce + (map :r @cells)) (double (count @cells)))
                   0.0)])
              best (first (sort-by (fn [[q r]]
                                     (let [dq (- q (double target-q))
                                           dr (- r (double target-r))]
                                       (+ (* dq dq) (* dr dr))))
                                   frontier))]
          (when best
            (place! node (first best) (second best))
            ;; Record anchor for this type (first placement only)
            (when-not anchor
              (swap! type-anchors assoc st best))))))
    @cells))

(defn- assign-mission-layout
  "Pack missions into a tight hex cluster, grouped by repo.
   Same greedy adjacency algorithm as assign-layout but for missions.
   Missions are coloured by status, grouped by repo affinity."
  [mission-detail-data]
  (when mission-detail-data
    (let [missions (:missions mission-detail-data)
          dep-edges (:dependency-edges mission-detail-data)
          ;; Count dependency edges per mission (more connected = more central)
          edge-count (reduce (fn [m {:keys [from to]}]
                               (-> m
                                   (update from (fnil inc 0))
                                   (update to (fnil inc 0))))
                             {}
                             dep-edges)
          ;; Sort: in-progress first, then by dependency count, then by repo
          sorted (sort-by (fn [m]
                            [(case (:mission/status m)
                               "in-progress" 0 "active" 0
                               "testing" 1
                               "ready" 2
                               "blocked" 3
                               "complete" 4
                               5)
                             (- (get edge-count (:mission/id m) 0))
                             (:mission/repo m)])
                          missions)
          occupied (atom #{})
          cells (atom [])
          place! (fn [node q r]
                   (swap! occupied conj [q r])
                   (swap! cells conj {:node node :q q :r r}))]
      ;; Place first mission at origin
      (when-let [first-m (first sorted)]
        (place! (assoc first-m :sprite-type :mission) 0 0))
      ;; Place remaining
      (let [type-anchors (atom {})]
        (doseq [m (rest sorted)]
          (let [repo (:mission/repo m)
                frontier (->> @occupied
                              (mapcat (fn [[q r]] (hex-neighbours q r)))
                              distinct
                              (remove @occupied))
                anchor (get @type-anchors repo)
                [target-q target-r]
                (if anchor anchor
                    [(if (seq @cells) (/ (reduce + (map :q @cells)) (double (count @cells))) 0.0)
                     (if (seq @cells) (/ (reduce + (map :r @cells)) (double (count @cells))) 0.0)])
                best (first (sort-by (fn [[q r]]
                                       (let [dq (- q (double target-q))
                                             dr (- r (double target-r))]
                                         (+ (* dq dq) (* dr dr))))
                                     frontier))]
            (when best
              (place! (assoc m :sprite-type :mission) (first best) (second best))
              (when-not anchor
                (swap! type-anchors assoc repo best))))))
      @cells)))

(defn- layout-bounds
  "Compute the bounding box of hex cells."
  [cells]
  (if (empty? cells)
    {:q-min 0 :q-max 0 :r-min 0 :r-max 0 :q-span 1 :r-span 1}
    (let [qs (map :q cells)
          rs (map :r cells)
          q-min (apply min qs) q-max (apply max qs)
          r-min (apply min rs) r-max (apply max rs)]
      {:q-min q-min :q-max q-max :r-min r-min :r-max r-max
       :q-span (max 1 (- q-max q-min))
       :r-span (max 1 (- r-max r-min))})))

(defn- fit-hex-size
  "Compute hex size that fits all cells within (panel-w, panel-h)."
  [cells panel-w panel-h]
  (let [margin 50
        usable-w (- panel-w (* 2 margin))
        usable-h (- panel-h (* 2 margin))
        {:keys [q-span r-span]} (layout-bounds cells)
        size-from-w (/ (double usable-w) (* sqrt3 (+ q-span 1.5)))
        size-from-h (/ (double usable-h) (* 1.5 (+ r-span 1.5)))]
    (max 15.0 (min size-from-w size-from-h))))

(defn- layout-offset
  "Compute pixel offset to center the hex grid in the panel."
  [cells hex-size panel-w panel-h]
  (if (empty? cells)
    [0.0 0.0]
    (let [pixel-coords (map (fn [{:keys [q r]}] (hex->pixel q r hex-size)) cells)
          xs (map first pixel-coords)
          ys (map second pixel-coords)
          px-min (apply min xs) px-max (apply max xs)
          py-min (apply min ys) py-max (apply max ys)
          off-x (- (/ (double panel-w) 2.0) (/ (+ px-min px-max) 2.0))
          off-y (- (/ (double panel-h) 2.0) (/ (+ py-min py-max) 2.0))]
      [off-x off-y])))

(defn- cell-pixel
  "Get the pixel position of a cell given hex-size."
  [{:keys [q r]} hex-size]
  (hex->pixel q r hex-size))

(defn- hex-hit-test
  "Find which cell was clicked at pixel (mx, my)."
  [cells hex-size off-x off-y mx my]
  (let [adj-x (- mx off-x)
        adj-y (- my off-y)]
    (->> cells
         (filter (fn [cell]
                   (let [[cx cy] (cell-pixel cell hex-size)
                         dx (- adj-x cx)
                         dy (- adj-y cy)
                         dist (Math/sqrt (+ (* dx dx) (* dy dy)))]
                     (< dist (* hex-size 0.9)))))
         first)))

(defn- node-detail-text
  "Format a node's details for the detail panel."
  [node]
  (let [t (:sprite-type node)]
    (case t
      :repo (str "REPO: " (:label node) "\n"
                 "Workstream: " (name (or (:workstream node) :unknown)) "\n"
                 "Commits (window): " (:commits node 0) "\n"
                 "Active: " (:active? node))
      :sorry (str "SORRY: " (:label node) "\n"
                  "Severity: " (name (or (:severity node) :unknown)) "\n"
                  "Status: " (or (:status node) "?") "\n"
                  "Layer: " (or (:layer node) "?") "\n"
                  "Closes by: " (or (:closes-by node) "?"))
      :mission (str "MISSION: " (or (:mission/id node) (:label node)) "\n"
                    "Title: " (or (:mission/title node) "") "\n"
                    "Status: " (or (:mission/status node) (:status node)) "\n"
                    "Repo: " (or (:mission/repo node) (:repo node)))
      :tick (str "TICK: " (name (or (:id node) :unknown)) "\n"
                 "Fired: " (:fired? node) "\n"
                 (when (:fires node) (str "Fires: " (:fires node))))
      (str "Node: " (pr-str node)))))

;; ---------------------------------------------------------------------------
;; Sprite Rendering
;; ---------------------------------------------------------------------------

(defn- draw-hex-sprite
  "Draw a single hex sprite on the graphics context."
  [^Graphics2D g node cx cy size]
  (let [sprite-type (:sprite-type node)
        path (hex-path cx cy size)]
    (case sprite-type
      ;; --- Repo hex ---
      :repo
      (let [ws (or (:workstream node) :stack)
            commits (or (:commits node) 0)
            active? (:active? node)
            base-color (workstream-color ws)
            alpha (if active? (activity-alpha commits 50) 40)
            fill-color (Color. (.getRed base-color) (.getGreen base-color)
                               (.getBlue base-color) alpha)
            border (if active? (Color. 60 60 60) (Color. 180 180 180))]
        (.setColor g fill-color)
        (.fill g path)
        (.setColor g border)
        (.setStroke g (BasicStroke. (if active? 2.0 1.0)))
        (.draw g path)
        ;; Label
        (.setColor g (Color. 30 30 30))
        (.setFont g (Font. "SansSerif" Font/PLAIN (max 9 (int (* size 0.3)))))
        (let [label (or (:label node) "?")
              fm (.getFontMetrics g)
              tw (.stringWidth fm label)]
          (.drawString g label (int (- cx (/ tw 2))) (int (+ cy 4))))
        ;; Commit count badge
        (when (and active? (pos? commits))
          (.setFont g (Font. "SansSerif" Font/BOLD (max 8 (int (* size 0.22)))))
          (.setColor g (Color. 80 80 80))
          (.drawString g (str commits)
                       (int (- cx (* size 0.15)))
                       (int (+ cy (* size 0.45))))))

      ;; --- Sorry hex ---
      :sorry
      (let [severity (or (:severity node) :warning)
            base-color (severity-color severity)
            fill-color (Color. (.getRed base-color) (.getGreen base-color)
                               (.getBlue base-color) 200)]
        (.setColor g fill-color)
        (.fill g path)
        (.setColor g (Color. 120 0 0))
        (.setStroke g (BasicStroke. 2.5))
        (.draw g path)
        ;; Label
        (.setColor g Color/WHITE)
        (.setFont g (Font. "SansSerif" Font/BOLD (max 8 (int (* size 0.25)))))
        (let [label (str (when (:id node) (-> (name (:id node))
                                               (str/replace "SORRY-" "S:")
                                               (str/replace "sorry-" "s:"))))
              fm (.getFontMetrics g)
              tw (.stringWidth fm label)]
          (.drawString g label (int (- cx (/ tw 2))) (int (+ cy 4)))))

      ;; --- Tick hex ---
      :tick
      (let [fired? (:fired? node)
            base-color (tick-color fired?)
            alpha (if fired? 230 80)
            fill-color (Color. (.getRed base-color) (.getGreen base-color)
                               (.getBlue base-color) alpha)]
        (.setColor g fill-color)
        (.fill g path)
        (.setColor g (if fired? (Color. 200 50 0) (Color. 160 160 160)))
        (.setStroke g (BasicStroke. (if fired? 3.0 1.0)))
        (.draw g path)
        ;; Label
        (.setColor g (if fired? Color/WHITE (Color. 80 80 80)))
        (.setFont g (Font. "SansSerif" Font/BOLD (max 7 (int (* size 0.22)))))
        (let [label (str (when (:id node) (-> (name (:id node))
                                               (str/replace "-warning" "!")
                                               (str/replace "-" " "))))
              fm (.getFontMetrics g)
              tw (.stringWidth fm label)]
          (.drawString g label (int (- cx (/ tw 2))) (int (+ cy 4)))))

      ;; --- Mission hex ---
      :mission
      (let [status (or (:mission/status node) (:status node) "unknown")
            blocked? (= "blocked" status)
            base-color (mission-status-color status)
            alpha (if blocked? 200 140)
            fill-color (Color. (.getRed base-color) (.getGreen base-color)
                               (.getBlue base-color) alpha)]
        (.setColor g fill-color)
        (.fill g path)
        (.setColor g (if blocked? (Color. 180 0 0) (Color. 80 80 80)))
        (.setStroke g (BasicStroke. (if blocked? 2.5 1.5)))
        (.draw g path)
        ;; Label — truncate mission name for legibility
        (.setColor g (if blocked? Color/WHITE (Color. 30 30 30)))
        (.setFont g (Font. "SansSerif" Font/PLAIN (max 7 (int (* size 0.22)))))
        (let [raw-label (or (:mission/id node) (:label node) "?")
              label (-> raw-label str
                        (str/replace #"^M-" "")
                        (str/replace #"^mission-" ""))
              label (if (> (count label) 12)
                      (str (subs label 0 11) "..")
                      label)
              fm (.getFontMetrics g)
              tw (.stringWidth fm label)]
          (.drawString g label (int (- cx (/ tw 2))) (int (+ cy 4))))
        ;; Status badge
        (.setFont g (Font. "SansSerif" Font/BOLD (max 6 (int (* size 0.18)))))
        (.setColor g (if blocked? (Color. 255 200 200) (Color. 80 80 80)))
        (let [badge (case status
                      "blocked" "BLK"
                      ("active" "in-progress" "in_progress") "ACT"
                      "testing" "TST"
                      "ready" "RDY"
                      "complete" "OK"
                      "deferred" "DEF"
                      "?")
              fm (.getFontMetrics g)
              tw (.stringWidth fm badge)]
          (.drawString g badge (int (- cx (/ tw 2))) (int (+ cy (* size 0.4))))))

      ;; --- Default ---
      (do
        (.setColor g (Color. 200 200 200 100))
        (.fill g path)
        (.setColor g (Color. 150 150 150))
        (.setStroke g (BasicStroke. 1.0))
        (.draw g path)))))

;; ---------------------------------------------------------------------------
;; Edge Rendering (temporal coupling)
;; ---------------------------------------------------------------------------

(defn- draw-coupling-edges
  "Draw temporal coupling edges between repo hexes."
  [^Graphics2D g cells coupling-edges size]
  (let [cell-map (into {} (map (fn [cell]
                                 [(or (:id (:node cell)) (:mission/id (:node cell))) cell])
                               cells))]
    (doseq [{:keys [from to strength]} coupling-edges]
      (when-let [a (get cell-map from)]
        (when-let [b (get cell-map to)]
          (let [[ax ay] (cell-pixel a size)
                [bx by] (cell-pixel b size)
                s (double (or strength 0.5))
                alpha (int (+ 30 (* 180 (min 1.0 s))))
                width (float (+ 1.0 (* 3.0 (min 1.0 s))))]
            (.setColor g (Color. 100 100 200 alpha))
            (.setStroke g (BasicStroke. width))
            (.drawLine g (int ax) (int ay) (int bx) (int by))))))))

;; ---------------------------------------------------------------------------
;; Session Ant Rendering
;;
;; A session "ant" is drawn as a bright dot on the hex it's currently
;; visiting, with a fading trail showing where it's been.
;; ---------------------------------------------------------------------------

(def ^:private ant-colors
  "Distinct colours for multi-session replay ants."
  [(Color. 255 80 0)    ;; orange
   (Color. 0 180 255)   ;; cyan
   (Color. 200 50 200)  ;; magenta
   (Color. 50 200 50)   ;; lime
   (Color. 255 200 0)   ;; yellow
   (Color. 255 50 100)  ;; pink
   (Color. 100 200 200) ;; teal
   (Color. 200 150 50)]);; gold

(defn- draw-session-ant
  "Draw a session ant at its current position, with trail.
   `color-idx` selects from the ant colour palette.
   `trail` is a seq of {:repos [...] :age N} from recent steps.
   `current-repos` is the repos the ant is currently at."
  [^Graphics2D g cells hex-size current-repos trail color-idx]
  (let [ant-color (nth ant-colors (mod (or color-idx 0) (count ant-colors)))
        cell-map (into {} (map (fn [cell]
                                 [(or (:id (:node cell)) (:mission/id (:node cell))) cell])
                               cells))
        ;; Draw trail (fading circles at previously visited hexes)
        _ (doseq [{:keys [repos age]} trail]
            (doseq [repo repos]
              (when-let [cell (get cell-map repo)]
                (let [[cx cy] (cell-pixel cell hex-size)
                      alpha (int (max 20 (- 150 (* age 25))))
                      radius (int (* hex-size 0.25))]
                  (.setColor g (Color. (.getRed ant-color) (.getGreen ant-color)
                                       (.getBlue ant-color) alpha))
                  (.fillOval g (int (- cx radius)) (int (- cy radius))
                             (* 2 radius) (* 2 radius))))))
        ;; Draw current position (bright dot)
        _ (doseq [repo current-repos]
            (when-let [cell (get cell-map repo)]
              (let [[cx cy] (cell-pixel cell hex-size)
                    radius (int (* hex-size 0.35))]
                ;; Glow
                (.setColor g (Color. (.getRed ant-color) (.getGreen ant-color)
                                     (.getBlue ant-color) 100))
                (.fillOval g (int (- cx radius 3)) (int (- cy radius 3))
                           (+ (* 2 radius) 6) (+ (* 2 radius) 6))
                ;; Core dot
                (.setColor g ant-color)
                (.fillOval g (int (- cx radius)) (int (- cy radius))
                           (* 2 radius) (* 2 radius))
                ;; Bright center
                (.setColor g (Color. 255 255 200))
                (let [inner (int (* radius 0.4))]
                  (.fillOval g (int (- cx inner)) (int (- cy inner))
                             (* 2 inner) (* 2 inner))))))]))

;; ---------------------------------------------------------------------------
;; Legend
;; ---------------------------------------------------------------------------

(defn- draw-legend
  "Draw a compact legend in the top-left corner."
  [^Graphics2D g x y]
  (let [items [[(Color. 74 144 226) "Stack"]
               [(Color. 34 197 94) "Mathematics"]
               [(Color. 234 179 8) "Portfolio"]
               [(Color. 239 68 68) "Consulting"]
               [(Color. 185 28 28) "Sorry (critical)"]
               [(Color. 217 119 6) "Sorry (warning)"]
               [(Color. 59 130 246) "Mission (active)"]
               [(Color. 239 68 68) "Mission (blocked)"]
               [(Color. 34 197 94) "Mission (ready)"]
               [(Color. 249 115 22) "Tick (firing)"]
               [(Color. 156 163 175) "Tick (quiet)"]]
        box-size 12
        line-height 18]
    (.setFont g (Font. "SansSerif" Font/PLAIN 11))
    (doseq [[idx [color label]] (map-indexed vector items)]
      (let [iy (+ y (* idx line-height))]
        (.setColor g color)
        (.fillRect g x iy box-size box-size)
        (.setColor g (Color. 60 60 60))
        (.drawRect g x iy box-size box-size)
        (.drawString g label (+ x box-size 6) (+ iy 11))))))

;; ---------------------------------------------------------------------------
;; Stats Sidebar
;; ---------------------------------------------------------------------------

(defn- make-stats-panel
  "Build a sidebar panel showing key metrics."
  [data-atom]
  (let [width 200
        preferred (Dimension. width 600)
        panel (proxy [JPanel] []
                (getPreferredSize [] preferred)
                (paintComponent [^Graphics g]
                  (proxy-super paintComponent g)
                  (let [^Graphics2D g2 g
                        w (.getWidth this)
                        pad 12
                        y (atom 20)]
                    (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING
                                      RenderingHints/VALUE_ANTIALIAS_ON)
                    (.setColor g2 (Color. 250 250 252))
                    (.fillRect g2 0 0 w (.getHeight this))

                    (when-let [data @data-atom]
                      ;; Title
                      (.setFont g2 (Font. "SansSerif" Font/BOLD 14))
                      (.setColor g2 (Color. 30 30 30))
                      (.drawString g2 "War Machine" pad @y)
                      (swap! y + 24)

                      ;; Loop health
                      (when-let [lh (:loop-health data)]
                        (.setFont g2 (Font. "SansSerif" Font/BOLD 11))
                        (.setColor g2 (Color. 50 50 50))
                        (.drawString g2 "Loop Health" pad @y)
                        (swap! y + 16)
                        (.setFont g2 (Font. "SansSerif" Font/PLAIN 10))
                        (doseq [{:keys [label health count]} (:arrows lh)]
                          (let [color (loop-arrow-color health)
                                bar-w (- w (* 2 pad))
                                fill-w (int (* bar-w health))]
                            (.setColor g2 (Color. 230 230 230))
                            (.fillRect g2 pad @y bar-w 10)
                            (.setColor g2 color)
                            (.fillRect g2 pad @y fill-w 10)
                            (.setColor g2 (Color. 60 60 60))
                            (.drawString g2 (str label " " (format "%.0f%%" (* 100 health)))
                                         pad (+ @y 22))
                            (swap! y + 28)))
                        (swap! y + 8))

                      ;; Workstream balance
                      (when-let [pcts (get-in data [:graph :dynamics :commit-percentages])]
                        (.setFont g2 (Font. "SansSerif" Font/BOLD 11))
                        (.setColor g2 (Color. 50 50 50))
                        (.drawString g2 "Workstream Balance" pad @y)
                        (swap! y + 16)
                        (let [bar-w (- w (* 2 pad))]
                          (doseq [[ws pct] (sort-by val > pcts)]
                            (let [color (workstream-color ws)
                                  fill-w (int (* bar-w (min 1.0 (double pct))))]
                              (.setColor g2 (Color. 230 230 230))
                              (.fillRect g2 pad @y bar-w 12)
                              (.setColor g2 color)
                              (.fillRect g2 pad @y fill-w 12)
                              (.setColor g2 (Color. 40 40 40))
                              (.setFont g2 (Font. "SansSerif" Font/PLAIN 10))
                              (.drawString g2 (str (name ws) " " (format "%.0f%%" (* 100 (double pct))))
                                           (+ pad 4) (+ @y 10))
                              (swap! y + 18))))
                        (swap! y + 8))

                      ;; Argument
                      (when-let [sa (:support-attack data)]
                        (.setFont g2 (Font. "SansSerif" Font/BOLD 11))
                        (.setColor g2 (Color. 50 50 50))
                        (.drawString g2 "Argument" pad @y)
                        (swap! y + 16)
                        (.setFont g2 (Font. "SansSerif" Font/PLAIN 10))
                        (.setColor g2 (Color. 60 60 60))
                        (.drawString g2 (str "Support: " (format "%.0f%%" (* 100 (:support-coverage sa))))
                                     pad @y)
                        (swap! y + 14)
                        (.drawString g2 (str "Attack: " (format "%.0f%%" (* 100 (:attack-coverage sa))))
                                     pad @y)
                        (swap! y + 20))

                      ;; Mission triage
                      (when-let [mt (:mission-triage data)]
                        (.setFont g2 (Font. "SansSerif" Font/BOLD 11))
                        (.setColor g2 (Color. 50 50 50))
                        (.drawString g2 "Missions" pad @y)
                        (swap! y + 16)
                        (.setFont g2 (Font. "SansSerif" Font/PLAIN 10))
                        (.setColor g2 (Color. 60 60 60))
                        (.drawString g2 (str (:active mt) " active / "
                                             (:total mt) " total")
                                     pad @y)
                        (swap! y + 14)
                        (.drawString g2 (str (:abandoned-count mt) " abandoned")
                                     pad @y)
                        (swap! y + 14)
                        (.drawString g2 (str "Health: " (format "%.0f%%" (* 100 (:health mt))))
                                     pad @y)
                        (swap! y + 20))

                      ;; Portfolio Inference
                      (when-let [pf (:portfolio data)]
                        (when (:available? pf)
                          (.setFont g2 (Font. "SansSerif" Font/BOLD 11))
                          (.setColor g2 (Color. 50 50 50))
                          (.drawString g2 "Portfolio Inference" pad @y)
                          (swap! y + 16)
                          (when-let [s (:state pf)]
                            (.setFont g2 (Font. "SansSerif" Font/PLAIN 10))
                            (.setColor g2 (Color. 40 40 40))
                            (.drawString g2 (str "Mode: " (:mode s)
                                                 " | Urgency: " (format "%.2f" (double (or (:urgency s) 0))))
                                         pad @y)
                            (swap! y + 14)
                            (.drawString g2 (str "τ: " (format "%.2f" (double (or (:tau s) 0)))
                                                 " | Steps: " (:step-count s))
                                         pad @y)
                            (swap! y + 14))
                          (when-let [r (:recommendation pf)]
                            (.setFont g2 (Font. "SansSerif" Font/BOLD 10))
                            (.setColor g2 (Color. 0 100 180))
                            (.drawString g2 (str "→ " (:action r)) pad @y)
                            (swap! y + 14)
                            (when (:free-energy r)
                              (.setFont g2 (Font. "SansSerif" Font/PLAIN 9))
                              (.setColor g2 (Color. 80 80 80))
                              (.drawString g2 (str "G=" (format "%.4f" (double (:free-energy r))))
                                           pad @y)
                              (swap! y + 12)))
                          (swap! y + 8)))

                      ;; Observation vector summary
                      (when-let [obs (wm/observe data)]
                        (.setFont g2 (Font. "SansSerif" Font/BOLD 11))
                        (.setColor g2 (Color. 50 50 50))
                        (.drawString g2 "Observation Vector" pad @y)
                        (swap! y + 16)
                        (.setFont g2 (Font. "Monospaced" Font/PLAIN 9))
                        (.setColor g2 (Color. 60 60 60))
                        (doseq [[k v] (sort-by key obs)]
                          (.drawString g2 (format "%-20s %.2f" (name k) (double v))
                                       pad @y)
                          (swap! y + 12)))))))]
    (.setPreferredSize panel preferred)
    panel))

;; ---------------------------------------------------------------------------
;; Main Panel (hex map)
;; ---------------------------------------------------------------------------

(defn- make-hex-panel
  "Build the main hex map panel.
   Hex size is computed dynamically from panel dimensions so the
   entire grid fits regardless of window size.
   Click any hex to see its details in the detail-area.
   Session replay state is read from replay-atom.
   view-mode-atom: :stack (repos/sorrys/ticks) or :missions"
  [data-atom detail-area replay-atom view-mode-atom]
  (let [panel (proxy [JPanel] []
                (paintComponent [^Graphics g]
                  (proxy-super paintComponent g)
                  (let [^Graphics2D g2 g
                        pw (.getWidth this)
                        ph (.getHeight this)]
                    (.setRenderingHint g2 RenderingHints/KEY_ANTIALIASING
                                      RenderingHints/VALUE_ANTIALIAS_ON)
                    ;; Background
                    (.setColor g2 (Color. 248 250 252))
                    (.fillRect g2 0 0 pw ph)

                    (when-let [data @data-atom]
                      (let [view-mode (or @view-mode-atom :stack)
                            cells (if (= view-mode :missions)
                                    (or (assign-mission-layout (:mission-detail data)) [])
                                    (assign-layout (:graph data)))
                            coupling (if (= view-mode :missions)
                                       (get-in data [:mission-detail :dependency-edges] [])
                                       (get-in data [:graph :edges :temporal-coupling] []))
                            hex-size (fit-hex-size cells pw ph)
                            [off-x off-y] (layout-offset cells hex-size pw ph)]
                        ;; Apply centering offset
                        (.translate g2 (double off-x) (double off-y))
                        ;; Draw coupling edges (underneath hexes)
                        (draw-coupling-edges g2 cells coupling hex-size)
                        ;; Draw node hexes (tightly packed, full size)
                        (doseq [cell cells]
                          (let [[cx cy] (cell-pixel cell hex-size)]
                            (draw-hex-sprite g2 (:node cell) cx cy hex-size)))
                        ;; Draw session ants (one per selected session)
                        (when-let [replay @replay-atom]
                          (let [;; In mission view, use :missions field; in stack view, use :repos
                                pos-key (if (= view-mode :missions) :missions :repos)]
                            (doseq [[sid state] replay
                                    :when (map? state)]
                              (let [{:keys [steps step-idx color-idx]} state
                                    ;; Find current position — walk backward if current step has none
                                    current-pos
                                    (or (first (for [i (range step-idx -1 -1)
                                                     :let [ids (get (get steps i) pos-key)]
                                                     :when (seq ids)]
                                                 ids))
                                        [])
                                    trail (for [i (range (max 0 (- step-idx 6)) step-idx)
                                                :let [s (get steps i)
                                                      ids (get s pos-key)]
                                                :when (seq ids)]
                                            {:repos ids
                                             :age (- step-idx i)})]
                                (draw-session-ant g2 cells hex-size current-pos trail color-idx)))))
                        ;; Reset transform for legend
                        (.translate g2 (double (- off-x)) (double (- off-y)))
                        ;; Legend
                        (draw-legend g2 10 10))))))]
    ;; Click handler: hit-test hexes and show detail
    (.addMouseListener
     panel
     (proxy [MouseAdapter] []
       (mouseClicked [e]
         (when-let [data @data-atom]
           (let [graph (:graph data)
                 cells (assign-layout graph)
                 pw (.getWidth panel)
                 ph (.getHeight panel)
                 hex-size (fit-hex-size cells pw ph)
                 [off-x off-y] (layout-offset cells hex-size pw ph)
                 mx (.getX e)
                 my (.getY e)]
             (if-let [cell (hex-hit-test cells hex-size off-x off-y mx my)]
               (.setText detail-area (node-detail-text (:node cell)))
               (.setText detail-area "")))))))
    ;; Repaint on resize (including screen changes)
    (.addComponentListener
     panel
     (proxy [ComponentAdapter] []
       (componentResized [_] (.repaint panel))
       (componentMoved [_] (.repaint panel))))
    panel))

;; ---------------------------------------------------------------------------
;; Orchestrator
;; ---------------------------------------------------------------------------

(defn visualize
  "Launch the War Machine hex Petri dish visualiser.

   Runs scan functions, then displays the strategic state as a hex map.
   Click 'Refresh' to re-scan. The hex grid scales dynamically to fit
   the window — resize the window and the layout adapts.

   Options:
     :days      scan window (default 14)
     :blocking? block until window closes (default true)"
  ([] (visualize {}))
  ([{:keys [days blocking?]
     :or {days 14 blocking? true}}]
   (let [data-atom (atom nil)
         replay-atom (atom nil)
         view-mode-atom (atom :stack) ;; :stack or :missions
         scan! (fn []
                 (println "War Machine: scanning...")
                 (let [{:keys [data]} (wm/generate-war-machine days)]
                   (reset! data-atom data)
                   (println "War Machine: scan complete.")))
         finished (promise)]
     ;; Initial scan
     (scan!)
     ;; Build UI
     (SwingUtilities/invokeAndWait
      (fn []
        (let [;; Detail panel
              detail-area (doto (JTextArea. 4 30)
                            (.setEditable false)
                            (.setFont (Font. "Monospaced" Font/PLAIN 11))
                            (.setLineWrap true)
                            (.setWrapStyleWord true)
                            (.setText "(click a hex for details)"))
              detail-scroll (doto (JScrollPane. detail-area)
                              (.setPreferredSize (Dimension. 0 90))
                              (.setBorder (BorderFactory/createTitledBorder "Detail")))
              hex-panel (doto (make-hex-panel data-atom detail-area replay-atom view-mode-atom)
                          (.setPreferredSize (Dimension. 800 600)))
              stats-panel (make-stats-panel data-atom)
              ;; Toolbar
              toolbar (JToolBar.)
              refresh-btn (JButton. "Refresh")
              _ (.addActionListener refresh-btn
                                    (reify ActionListener
                                      (actionPerformed [_ _]
                                        (future
                                          (scan!)
                                          (SwingUtilities/invokeLater
                                           (fn []
                                             (.repaint hex-panel)
                                             (.repaint stats-panel)))))))
              _ (.add toolbar refresh-btn)
              ;; View toggle
              view-btn (JButton. "Missions")
              _ (.addActionListener view-btn
                                    (reify ActionListener
                                      (actionPerformed [_ _]
                                        (let [current @view-mode-atom
                                              next-mode (if (= current :stack) :missions :stack)]
                                          (reset! view-mode-atom next-mode)
                                          (.setText view-btn (if (= next-mode :stack) "Missions" "Stack"))
                                          (.repaint hex-panel)))))
              _ (.add toolbar view-btn)
              _ (.addSeparator toolbar)
              ;; Session replay — checkboxes for multi-session selection
              ;; replay-atom: {session-id {:steps :step-idx :color-idx} ...
              ;;               :playing? bool}
              sessions-vec (vec (get-in @data-atom [:sessions :sessions] []))
              session-panel (doto (JPanel.)
                              (.setLayout (java.awt.FlowLayout. java.awt.FlowLayout/LEFT 2 0)))
              _ (.add session-panel (JLabel. "Sessions: "))
              checkboxes (mapv
                          (fn [i session]
                            (let [sid (:session-id session)
                                  short-id (if (> (count sid) 12) (str (subs sid 0 8) "..") sid)
                                  label (str short-id " (" (:entry-count session) ")")
                                  cb (JCheckBox. label false)]
                              (.setFont cb (Font. "SansSerif" Font/PLAIN 10))
                              (.addActionListener cb
                                (reify ActionListener
                                  (actionPerformed [_ _]
                                    (if (.isSelected cb)
                                      ;; Add this session to replay
                                      (swap! replay-atom assoc sid
                                             {:steps (vec (:steps session))
                                              :step-idx 0
                                              :color-idx i
                                              :session-id sid})
                                      ;; Remove it
                                      (swap! replay-atom dissoc sid))
                                    (.repaint hex-panel))))
                              (.add session-panel cb)
                              cb))
                          (range)
                          sessions-vec)
              ;; Play/Step/Stop
              _ (.addSeparator toolbar)
              play-btn (JButton. "Play")
              step-btn (JButton. "Step")
              stop-btn (JButton. "Stop")
              _ (.add toolbar play-btn)
              _ (.add toolbar step-btn)
              _ (.add toolbar stop-btn)
              ;; Timer for auto-play
              timer-atom (atom nil)
              play-timer (Timer. 400
                                (reify ActionListener
                                  (actionPerformed [_ _]
                                    (when (:playing? @replay-atom)
                                      (let [any-advanced? (atom false)]
                                        ;; Advance all selected sessions
                                        (doseq [[sid state] @replay-atom
                                                :when (map? state)
                                                :let [{:keys [steps step-idx]} state
                                                      next-idx (inc step-idx)]
                                                :when (< next-idx (count steps))]
                                          (swap! replay-atom assoc-in [sid :step-idx] next-idx)
                                          (reset! any-advanced? true))
                                        (if @any-advanced?
                                          (.repaint hex-panel)
                                          ;; All sessions finished
                                          (do (swap! replay-atom assoc :playing? false)
                                              (when-let [t @timer-atom] (.stop t)))))))))
              _ (reset! timer-atom play-timer)
              _ (.addActionListener play-btn
                                    (reify ActionListener
                                      (actionPerformed [_ _]
                                        (swap! replay-atom assoc :playing? true)
                                        (.start play-timer))))
              _ (.addActionListener step-btn
                                    (reify ActionListener
                                      (actionPerformed [_ _]
                                        (.stop play-timer)
                                        (swap! replay-atom assoc :playing? false)
                                        (doseq [[sid state] @replay-atom
                                                :when (map? state)
                                                :let [{:keys [steps step-idx]} state
                                                      next-idx (inc step-idx)]
                                                :when (< next-idx (count steps))]
                                          (swap! replay-atom assoc-in [sid :step-idx] next-idx))
                                        (.repaint hex-panel))))
              _ (.addActionListener stop-btn
                                    (reify ActionListener
                                      (actionPerformed [_ _]
                                        (.stop play-timer)
                                        ;; Reset all to step 0
                                        (doseq [[sid state] @replay-atom
                                                :when (map? state)]
                                          (swap! replay-atom assoc-in [sid :step-idx] 0))
                                        (swap! replay-atom assoc :playing? false)
                                        (.repaint hex-panel))))
              ;; Top: toolbar + session checkboxes
              top-panel (doto (JPanel.)
                          (.setLayout (java.awt.GridLayout. 2 1 0 0))
                          (.add toolbar)
                          (.add session-panel))
              frame (doto (JFrame. "War Machine — Strategic Synthesis")
                      (.setLayout (BorderLayout.))
                      (.add top-panel BorderLayout/NORTH)
                      (.add hex-panel BorderLayout/CENTER)
                      (.add stats-panel BorderLayout/EAST)
                      (.add detail-scroll BorderLayout/SOUTH)
                      (.pack)
                      (.setExtendedState (bit-or (.getExtendedState (JFrame.))
                                                 JFrame/MAXIMIZED_BOTH))
                      (.setVisible true))
              ;; Maximize button for re-filling screen after moves
              maximize-btn (JButton. "Fill Screen")
              _ (.addActionListener maximize-btn
                                    (reify ActionListener
                                      (actionPerformed [_ _]
                                        (let [gc (.getGraphicsConfiguration frame)
                                              bounds (.getBounds gc)
                                              insets (.getScreenInsets (java.awt.Toolkit/getDefaultToolkit) gc)]
                                          (.setBounds frame
                                                      (+ (.x bounds) (.left insets))
                                                      (+ (.y bounds) (.top insets))
                                                      (- (.width bounds) (.left insets) (.right insets))
                                                      (- (.height bounds) (.top insets) (.bottom insets)))
                                          (.revalidate frame)
                                          (.repaint hex-panel)
                                          (.repaint stats-panel)))))
              _ (.add toolbar maximize-btn)]
          ;; Re-maximize when frame moves to a different screen
          (.addComponentListener frame
                                 (proxy [ComponentAdapter] []
                                   (componentMoved [_]
                                     ;; Re-fill the current screen's bounds
                                     (let [gc (.getGraphicsConfiguration frame)
                                           bounds (.getBounds gc)
                                           insets (.getScreenInsets (java.awt.Toolkit/getDefaultToolkit) gc)
                                           fw (.getWidth frame)
                                           fh (.getHeight frame)
                                           sw (- (.width bounds) (.left insets) (.right insets))
                                           sh (- (.height bounds) (.top insets) (.bottom insets))]
                                       ;; Only resize if the frame is smaller than the screen
                                       ;; (avoids fighting manual resizing)
                                       (when (or (< fw (* 0.9 sw)) (< fh (* 0.9 sh)))
                                         (.setBounds frame
                                                     (+ (.x bounds) (.left insets))
                                                     (+ (.y bounds) (.top insets))
                                                     sw sh)
                                         (.revalidate frame))))))
          (.addWindowListener frame
                              (proxy [WindowAdapter] []
                                (windowClosing [_]
                                  (when-not (realized? finished)
                                    (deliver finished true))))))))
     (when blocking? @finished)
     nil)))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(defn -main [& args]
  (let [days (if (seq args) (Integer/parseInt (first args)) 14)]
    (println "War Machine Visual: starting (days=" days ")")
    (println "  DISPLAY=" (System/getenv "DISPLAY"))
    (println "  Headless?" (java.awt.GraphicsEnvironment/isHeadless))
    (if (java.awt.GraphicsEnvironment/isHeadless)
      (do
        (println "ERROR: JVM is headless, cannot open Swing window.")
        (println "Run with: DISPLAY=:0 clojure -M:war-machine"))
      (do
        (visualize {:days days :blocking? true})
        (println "Window closed.")))))
