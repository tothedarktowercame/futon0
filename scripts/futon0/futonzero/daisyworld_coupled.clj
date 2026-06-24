(ns futon0.futonzero.daisyworld-coupled
  "Daisyworld — Phase 3a: COUPLED worlds with LOCAL (3×3) heat export (Ostrom subsidiarity).

   Two worlds A and B, each with its own global sun (forcing). Instead of reflecting heat to space,
   each cell's REFLECTED heat is exported LOCALLY onto the 3×3 patch of the *shadow world* (a box-blur
   of the reflected field, scaled by coupling κ, lagged one step). So where you place reflective
   (white) daisies has a local consequence on the other world — a spatial heat-commons. This is the
   board for Phase 3b self-play (two G(π) players, R2-trained prior, exogenous homeostasis reward).

   Phase 3a here is the SIMULATOR + viz only (no players yet): validate the local-3×3 coupling and
   witness the two worlds co-evolve. Run:
     bb --classpath scripts scripts/futon0/futonzero/daisyworld_coupled.clj [ticks]")

(require '[futon0.futonzero.daisyworld :as dw] '[clojure.string :as str])

(def KAPPA 0.5)   ; coupling strength: fraction of reflected heat exported to the shadow world

(defn import-field
  "Local 3×3 box-blur of the shadow world's `reflected` field, scaled by κ → the heat imported
   onto each cell of this world. (Ostrom-local: a cell's reflection lands on the 3×3 around it.)"
  [reflected kappa]
  (when (seq reflected)
    (into {} (for [d (keys reflected)]
               (let [cells (conj (dw/neighbours d) d)]
                 [d (* kappa (/ (reduce + (map #(get reflected % 0.0) cells)) (count cells)))])))))

(defn coupled-step
  "Advance both worlds one tick, each importing the other's (lagged) reflected heat, 3×3-local."
  [wa wb kappa sa sb]
  [(dw/step wa sa (import-field (:reflected wb) kappa))
   (dw/step wb sb (import-field (:reflected wa) kappa))])

(defn- mean [xs] (/ (reduce + xs) (double (count xs))))
(defn- mt [w] (mean (vals (:temp w))))

;; ---- frame capture (per world) ----
(defn- tq [t] (max 0 (min 9 (int (/ (+ t 10) 5.0)))))
(defn- wframe [{:keys [temp daisy]}]
  (let [cells (for [y (range dw/H) x (range dw/W)] [x y])]
    {:g (apply str (map #(case (:breed (daisy %)) :black \B :white \W \.) cells))
     :h (apply str (map #(char (+ 48 (tq (temp %)))) cells))
     :mt (format "%.1f" (mt {:temp temp}))
     :b (count (filter #(= :black (:breed %)) (vals daisy)))
     :w (count (filter #(= :white (:breed %)) (vals daisy)))}))

(defn run-coupled [{:keys [seed-a seed-b ticks kappa sa sb]}]
  (loop [wa (dw/init-world seed-a {}) wb (dw/init-world seed-b {}) t 0 frames []]
    (if (> t ticks)
      frames
      (let [[wa' wb'] (coupled-step wa wb kappa sa sb)
            frames (if (zero? (mod t 4)) (conj frames [(wframe wa) (wframe wb) wa wb]) frames)]
        (recur wa' wb' (inc t) frames)))))

(defn- run-solo [{:keys [seed ticks scenario]}]
  ;; world B with NO coupling (kappa 0) — baseline to show coupling's effect on B
  (loop [w (dw/init-world seed {}) t 0 errs []]
    (if (> t ticks) (mean errs)
        (recur (dw/step w scenario) (inc t) (conj errs (Math/abs (- (mt w) 22.8)))))))

(defn- viz-html [frames]
  (let [fa (map first frames) fb (map second frames)
        j (fn [fs] (str "[" (str/join "," (map #(format "{\"g\":\"%s\",\"h\":\"%s\",\"mt\":%s,\"b\":%d,\"w\":%d}"
                                                         (:g %) (:h %) (:mt %) (:b %) (:w %)) fs)) "]"))]
    (str "<!doctype html><meta charset=utf-8><title>Coupled Daisyworlds — local 3×3 heat export</title>
<style>body{font:14px system-ui;background:#0d0f14;color:#dde;margin:0;padding:14px}
.row{display:flex;gap:24px}.col{text-align:center}canvas{image-rendering:pixelated;border:1px solid #333;width:300px;height:300px}
button,input{font:13px}h3{margin:6px}.k{color:#8ab}</style>
<h2>Coupled Daisyworlds — local 3×3 heat export (κ=" KAPPA ") &nbsp;<span class=k>each reflects onto the other's shadow patch</span></h2>
<div class=row>
 <div class=col><h3>World A <span class=k>(sun brightening)</span></h3><canvas id=a></canvas><div id=sa></div></div>
 <div class=col><h3>World B <span class=k>(shadow; constant sun)</span></h3><canvas id=b></canvas><div id=sb></div></div>
</div>
<div style='margin-top:10px'><button id=play>▶ play</button>
 <input id=sl type=range min=0 value=0 style='width:520px'> <span id=tk></span>
 &nbsp;<label><input type=checkbox id=heat> heat</label></div>
<script>
const W=" dw/W ",H=" dw/H ";
['a','b'].forEach(function(id){var cv=document.getElementById(id);cv.width=W;cv.height=H;});
const A=" (j fa) ",B=" (j fb) ";const N=Math.min(A.length,B.length);
const sl=document.getElementById('sl');sl.max=N-1;
const HC=['#08306b','#2171b5','#4292c6','#6baed6','#9ecae1','#c6dbef','#fdd0a2','#fdae6b','#fd8d3c','#e6550d'];
function draw(cv,f,heat){const x=cv.getContext('2d');for(let i=0;i<W*H;i++){const r=Math.floor(i/W),c=i%W;
 if(heat){x.fillStyle=HC[+f.h[i]];}else{const d=f.g[i];x.fillStyle=d=='B'?'#111':d=='W'?'#fff':'#243';}x.fillRect(c,r,1,1);}}
function show(i){const heat=document.getElementById('heat').checked;
 draw(document.getElementById('a'),A[i],heat);draw(document.getElementById('b'),B[i],heat);
 document.getElementById('sa').innerHTML='mean-temp '+A[i].mt+'° &nbsp; ●'+A[i].b+' ○'+A[i].w;
 document.getElementById('sb').innerHTML='mean-temp '+B[i].mt+'° &nbsp; ●'+B[i].b+' ○'+B[i].w;
 document.getElementById('tk').textContent='frame '+i+' / '+(N-1);}
sl.oninput=()=>show(+sl.value);let p=null;
document.getElementById('play').onclick=function(){if(p){clearInterval(p);p=null;this.textContent='▶ play';return;}
 this.textContent='⏸ pause';p=setInterval(()=>{let v=(+sl.value+1)%N;sl.value=v;show(v);},120);};
document.getElementById('heat').onchange=()=>show(+sl.value);show(0);
</script>")))

(defn -main [& args]
  (let [ticks (if (seq args) (Integer/parseInt (first args)) 150)
        sa :change sb :default]   ; A's sun brightens; B's is constant (so coupling's effect on B is legible)
    (println "running coupled (A:" sa " B:" sb ", κ=" KAPPA ")…")
    (let [frames (run-coupled {:seed-a 1 :seed-b 2 :ticks ticks :kappa KAPPA :sa sa :sb sb})
          last-b (-> frames last second :mt)
          b-coupled-err (mean (map #(Math/abs (- (Double/parseDouble (:mt (second %))) 22.8)) frames))
          b-solo-err (run-solo {:seed 2 :ticks ticks :scenario sb})
          out "/home/joe/code/futon0/data/daisyworld-coupled-viz.html"]
      (spit out (viz-html frames))
      (println "\n=== coupling effect on World B (constant sun) ===")
      (println "  B reg-err COUPLED to A (κ=" KAPPA "):" (format "%.2f" b-coupled-err)
               "  vs  B SOLO (κ=0):" (format "%.2f" b-solo-err))
      (println "  -> B is perturbed by A's local heat exports (coupling is live)")
      (println "  final B mean-temp:" last-b "  viz ->" out))))

(when (= *file* (System/getProperty "babashka.file")) (apply -main *command-line-args*))
