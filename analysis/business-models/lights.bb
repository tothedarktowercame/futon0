#!/usr/bin/env bb
;; lights.bb -- the 32 x 5 grid, rendered for the eye.
;;
;; WHAT THIS IS NOT. It is not the acceptance grid. grid.bb's cells are
;; simulation results -- could we have got this company there faster, and would
;; anyone have paid -- and all 160 of those are still :unknown because no
;; simulation has run. Lighting those would be inventing results.
;;
;; WHAT IT IS. The evidence the demand-side batch actually produced: for each
;; company, WHICH PHASE of its own loop the record locates the problem at, and
;; WHAT THE RECIPIENT DID when something was offered. Both are cited fields in
;; records/batch-H-demand.edn. So a lit cell means "we know where their problem
;; sits", not "we could fix it" -- and the dark rows are the honest majority.
;;
;; Colour carries the response and NEVER carries it alone: every lit cell also
;; shows a glyph and every state is named in the legend and in the table view
;; below the grid. Status hues are the fixed good/warning/critical steps, which
;; validate on both surfaces for CVD separation and normal-vision separation;
;; warning is under 3:1 on the light surface by design, which is why the label
;; and the table are not optional.
;;
;;   bb lights.bb            write grid-lights.html
;;   bb lights.bb --open     ...and print the path

(require '[clojure.edn :as edn]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def root (.getParentFile (io/file *file*)))
(def records-dir (io/file root "records"))
(def out-file (io/file root "grid-lights.html"))
(def p4ng (or (System/getenv "P4NG") "/home/joe/code/p4ng"))

(defn read-edn [f] (edn/read-string (slurp f)))

(defn all-records []
  (->> (file-seq records-dir)
       (filter #(str/ends-with? (.getName %) ".edn"))
       sort (mapcat read-edn)))

(defn phases []
  (:stages (read-edn (io/file p4ng "empirics-futon" "control-stages.edn"))))

;; Response states. `other` is a real slot, not a dumping ground: it holds the
;; classes that exist once each and would otherwise each demand a hue nobody
;; can tell apart from its neighbour.
;; Glyphs are deliberately Latin-1 or common Geometric Shapes. U+2715 MULTIPLICATION
;; X rendered as tofu when cairosvg rasterised the figure for print -- the web page
;; was fine, so the missing glyph would have shipped to the PDF only. U+00D7 is safe
;; everywhere.
(def states
  {:engaged                 {:label "Engaged" :short "Engaged" :glyph "●" :tone "good"}
   :declined-on-capacity    {:label "Declined — capacity" :short "Capacity" :glyph "◐" :tone "warning"}
   :declined-on-merit       {:label "Declined — merit" :short "Merit" :glyph "×" :tone "critical"}
   :silent                  {:label "Silent" :short "Silent" :glyph "·" :tone "muted"}
   :declined-ground-unknown {:label "Declined — ground unknown" :short "Ground?" :glyph "◌" :tone "muted"}
   :forked-after-engagement {:label "Forked after engagement" :short "Forked" :glyph "◆" :tone "muted"}
   :unknown                 {:label "No offer recorded" :short "" :glyph "" :tone "none"}})

(defn esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;")
      (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn rows []
  (let [rs (all-records)
        supply (remove #(= :customer (:entity-type %)) rs)
        demand (into {} (map (juxt :for-case identity))
                     (filter #(= :customer (:entity-type %)) rs))]
    (for [c supply
          :let [d (get demand (:case-id c))]]
      {:case (:case-id c)
       :org (:org c)
       ;; :unknown is a keyword, not a phase. Treating it as one lit every
       ;; row and turned "no public source located this" into a claim -- the
       ;; exact substitution this grid exists to avoid.
       :phase (let [p (get-in d [:failing-phase :phase])]
                (when (string? p) p))
       :why (get-in d [:failing-phase :why])
       :response (or (get-in d [:response-signal :class]) :unknown)
       :problem (get-in d [:problem-class])
       :paired (boolean d)})))

(defn cell [r p]
  (let [lit? (= p (:phase r))
        st (get states (:response r) (:unknown states))]
    (if-not lit?
      "<td class=\"c\"></td>"
      (format (str "<td class=\"c lit t-%s\" title=\"%s\">"
                   "<span class=\"g\">%s</span><span class=\"lbl\">%s</span></td>")
              (:tone st)
              (esc (str (:org r) " — " p " — " (:label st)
                        (when-let [w (:why r)] (str "\n\n" w))))
              (:glyph st) (esc (:short st))))))

(defn html [rs ps]
  (let [lit (filter :phase rs)
        by-state (frequencies (map :response lit))]
    (str "<!doctype html><meta charset=utf-8>
<title>Demand-side evidence grid — 32 companies x 5 control-loop phases</title>
<style>
 :root{--surface:#fcfcfb;--ink:#1a1a19;--ink-2:#5a5a52;--ink-3:#8a8a80;
       --rule:rgba(0,0,0,.12);--cell:rgba(0,0,0,.035);
       --good:#0ca30c;--warning:#fab219;--critical:#d03b3b;--muted:#8a8a80}
 @media (prefers-color-scheme:dark){:root{--surface:#1a1a19;--ink:#f2f2ef;
       --ink-2:#b8b8b0;--ink-3:#8a8a80;--rule:rgba(255,255,255,.16);
       --cell:rgba(255,255,255,.05);--muted:#9a9a90}}
 body{background:var(--surface);color:var(--ink);margin:0;padding:2.4rem 1.6rem 4rem;
      font:15px/1.5 Charter,Georgia,serif;max-width:1180px}
 h1{font-size:1.35rem;margin:0 0 .3rem} p.sub{color:var(--ink-2);margin:.2rem 0 1.6rem;max-width:60ch}
 table{border-collapse:separate;border-spacing:2px;font-size:.82rem}
 th{font-weight:600;color:var(--ink-2);text-align:left;padding:0 .5rem .4rem;
    font-size:.72rem;letter-spacing:.06em;text-transform:uppercase}
 th.org{width:20rem}
 td.org{color:var(--ink);padding-right:.6rem;white-space:nowrap;
        overflow:hidden;text-overflow:ellipsis;max-width:20rem}
 td.c{background:var(--cell);border-radius:4px;width:8.4rem;height:2.1rem;
      text-align:center;vertical-align:middle}
 td.lit{color:#1a1a19}
 td.lit .g{font-size:1rem;margin-right:.35rem;vertical-align:-.05em}
 td.lit .lbl{font-size:.7rem;white-space:nowrap}
 /* td.lit.t-* , not .t-* : td.c is (0,0,1,1) and a bare class is (0,0,1,0),
    so the neutral cell background won every lit cell and the whole grid
    rendered unfilled. Caught by looking at the render, not the validator. */
 td.lit.t-good{background:var(--good)}
 td.lit.t-critical{background:var(--critical);color:#fff}
 td.lit.t-warning{background:var(--warning)}
 td.lit.t-muted{background:var(--muted);color:#fff}
 tr.unpaired td.org{color:var(--ink-3)}
 .legend{display:flex;flex-wrap:wrap;gap:1.1rem;margin:1.4rem 0 .4rem;font-size:.78rem}
 .legend span.k{display:inline-block;width:.85rem;height:.85rem;border-radius:3px;
                margin-right:.4rem;vertical-align:-.1em;background:var(--cell)}
 /* Same specificity trap as the cells, one level along: scoping the fills to
    td.lit left the legend swatches unpainted. */
 .legend .k.t-good{background:var(--good)}
 .legend .k.t-critical{background:var(--critical)}
 .legend .k.t-warning{background:var(--warning)}
 .legend .k.t-muted{background:var(--muted)}
 .note{color:var(--ink-2);font-size:.8rem;max-width:66ch;margin-top:1.6rem}
 h2{font-size:.95rem;margin:2.6rem 0 .5rem}
 table.tv{font-size:.78rem;border-spacing:0}
 table.tv td,table.tv th{border-bottom:1px solid var(--rule);padding:.28rem .7rem .28rem 0;
   text-align:left;vertical-align:top}
</style>
<h1>Where the problem sits, and what the recipient did</h1>
<p class=sub>32 companies against the five phases of their own control loop. A lit
cell means the demand-side record <em>locates</em> the problem at that phase and
names what happened when something was offered — it does <strong>not</strong> mean
we could fix it. The acceptance grid is a different artefact and all 160 of its
cells are still unrun.</p>
<div class=legend>"
         (str/join
          (for [k [:engaged :declined-on-capacity :declined-on-merit
                   :silent :declined-ground-unknown :forked-after-engagement]
                :let [st (states k)]
                :when (pos? (get by-state k 0))]
            (format "<div><span class=\"k t-%s\"></span>%s %s (%d)</div>"
                    (:tone st) (:glyph st) (:label st) (get by-state k 0))))
         "</div><table><tr><th class=org>Company</th>"
         (str/join (for [p ps] (format "<th>%s</th>" p)))
         "</tr>"
         (str/join
          (for [r rs]
            (format "<tr class=\"%s\"><td class=org title=\"%s\">%s</td>%s</tr>"
                    (if (:phase r) "" "unpaired") (esc (:org r))
                    (esc (:org r)) (str/join (for [p ps] (cell r p))))))
         "</table>
<p class=note>" (count lit) " of " (count rs) " rows carry a phase claim; the rest
are recorded <code>:unknown</code> because no public source located the problem,
not because the problem is absent. Every value here is a cited or explicitly
unverified field in <code>records/batch-H-demand.edn</code>, and every unverified
one carries a discharge condition. Management depth — how many layers sit between
the person with the problem and the person who can sign — is
<code>:unknown</code> in all 32 and is not web-researchable.</p>
<h2>Table view</h2><table class=tv><tr><th>Company</th><th>Phase</th>
<th>Response</th><th>Problem class</th></tr>"
         (str/join
          (for [r rs]
            (format "<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>"
                    (esc (:org r)) (esc (or (:phase r) "—"))
                    (esc (:label (get states (:response r) (:unknown states))))
                    (esc (if-let [pc (:problem r)] (str/join ", " (map name pc)) "—")))))
         "</table>")))

(def svg-file (io/file root "grid-lights.svg"))

;; SVG rather than a screenshot: the paper's build converts every SVG figure to
;; PDF for pdflatex (p4ng/svg2pdf.py) and LaTeXML embeds the same file for the
;; web, so one vector source serves both and neither is a raster of a browser.
;; Drawn from the same rows the HTML uses, so the two cannot disagree.
(def tone-hex
  {"good" "#0ca30c" "warning" "#fab219" "critical" "#d03b3b"
   "muted" "#8a8a80" "none" "none"})

(def geom {:x0 250 :y0 46 :cw 96 :ch 15 :gap 3 :row 18})

(defn svg [rs ps]
  (let [{:keys [x0 y0 cw ch gap row]} geom
        lit (filter :phase rs)
        by-state (frequencies (map :response lit))
        width (+ x0 (* (count ps) (+ cw gap)) 8)
        height (+ y0 (* (count rs) row) 78)
        idx (into {} (map-indexed (fn [i p] [p i]) ps))]
    (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
         ;; width/height on the ROOT, not just a viewBox. tuftify.py sizes a
         ;; figure by scanning the first 2000 characters of the asset for
         ;; width="...", so a root carrying only a viewBox matched the first
         ;; cell rect instead -- 96px, under its margin-figure threshold, and
         ;; the paper rendered this grid at the size of a penny.
         (format (str "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" "
                      "viewBox=\"0 0 %d %d\" role=\"img\" aria-labelledby=\"t d\">")
                 width height width height)
         "<title id=\"t\">Where each case's problem sits, and what the recipient did</title>"
         "<desc id=\"d\">Thirty-two business-model cases against the five phases of the "
         "customer's own control loop. A filled cell means the demand-side record locates "
         "the problem at that phase; its glyph and colour name what the recipient did when "
         "something was offered. Fifteen of thirty-two rows carry a phase claim.</desc>"
         "<style>"
         ".o{font:10px Georgia,serif;fill:#1a1a19}"
         ".o.u{fill:#9a9a92}"
         ".h{font:9px Georgia,serif;fill:#5a5a52;letter-spacing:.08em}"
         ".g{font:10px Georgia,serif;text-anchor:middle;dominant-baseline:central}"
         ".lg{font:9.5px Georgia,serif;fill:#3a3a34}"
         "</style>"
         ;; phase headers
         (apply str (for [p ps]
                      (format "<text class=\"h\" x=\"%d\" y=\"%d\">%s</text>"
                              (+ x0 (* (idx p) (+ cw gap))) (- y0 8) (esc p))))
         ;; rows
         (apply str
           (for [[i r] (map-indexed vector rs)
                 :let [y (+ y0 (* i row))]]
             (str (format "<text class=\"o%s\" x=\"%d\" y=\"%d\" text-anchor=\"end\">%s</text>"
                          (if (:phase r) "" " u") (- x0 10) (+ y 11)
                          (esc (let [o (:org r)] (if (> (count o) 34) (str (subs o 0 33) "\u2026") o))))
                  (apply str
                    (for [p ps
                          :let [x (+ x0 (* (idx p) (+ cw gap)))
                                lit? (= p (:phase r))
                                st (get states (:response r) (:unknown states))
                                fill (if lit? (tone-hex (:tone st)) "#eeeeea")]]
                      (str (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" rx=\"3\" fill=\"%s\"/>"
                                   x y cw ch fill)
                           (when lit?
                             (format "<text class=\"g\" x=\"%d\" y=\"%d\" fill=\"%s\">%s %s</text>"
                                     (+ x (quot cw 2)) (+ y (quot ch 2))
                                     (if (#{"critical" "muted"} (:tone st)) "#ffffff" "#1a1a19")
                                     (:glyph st) (esc (:short st))))))))))
         ;; legend
         (let [ly (+ y0 (* (count rs) row) 24)]
           (apply str
             (for [[j k] (map-indexed vector
                           (filter #(pos? (get by-state % 0))
                                   [:engaged :declined-on-capacity :declined-on-merit
                                    :silent :declined-ground-unknown :forked-after-engagement]))
                   :let [st (states k) lx (+ 8 (* j 185))]]
               (str (format "<rect x=\"%d\" y=\"%d\" width=\"11\" height=\"11\" rx=\"2\" fill=\"%s\"/>"
                            lx (- ly 9) (tone-hex (:tone st)))
                    (format "<text class=\"lg\" x=\"%d\" y=\"%d\">%s %s (%d)</text>"
                            (+ lx 16) ly (:glyph st) (esc (:label st)) (get by-state k 0))))))
         (format (str "<text class=\"lg\" x=\"8\" y=\"%d\" fill=\"#5a5a52\">"
                      "%d of %d rows carry a phase claim; the rest are :unknown "
                      "because no source located the problem.</text>")
                 (+ y0 (* (count rs) row) 48) (count lit) (count rs))
         "</svg>")))

(defn -main []
  (let [rs (rows) ps (phases)]
    (spit out-file (html rs ps))
    (spit svg-file (svg rs ps))
    (println (format "wrote %s and %s -- %d rows, %d with a phase claim"
                     (.getName out-file) (.getName svg-file)
                     (count rs) (count (filter :phase rs))))))

(-main)
