(ns futon0.futonzero.daisyworld-player
  "Daisyworld — Phase 2: the FutonZero G(pi) PLAYER + a viz (witness the evolutions).

   The Phase-1 simulator (daisyworld.clj) is the forward-model (R4). Here a player STEERS the world
   toward homeostasis by selecting INTERVENTION POLICIES via rollout — NOT argmax over the next step:
     - moves (R-actions): {:cool (seed white at the hottest cell) :warm (seed black at the coldest) :none}
     - forward model (R4): apply-action then daisyworld/step
     - EFE g(s) (R5): R5a pragmatic = |mean-temp - T*|  +  R5b ambiguity = temp spread (epistemic)
     - prior P(a|s) (R1): heuristic — cool when hot, warm when cold (PUCT branching weight)
     - G(pi) = sum_t gamma^t g(s_t) over a H-step rollout that CONTINUES under the prior policy
       (so an action's value is its multi-step consequence, not its immediate g — the policy, not the move)
     - select (R6): softmax over -G(pi)/tau with an abstain (:none) when policies tie
     - R2 hook: realized g after acting is returned for a prior-training channel (stubbed for v1)

   Validation: under :ramp (rising sun) the autonomous world over-warms and collapses; the G(pi) player
   should hold temperature near optimum longer. Emits daisyworld-viz.html (autonomous vs player, play/slider).

   Run:  bb futon0/scripts/futon0/futonzero/daisyworld_player.clj  [ticks]")

(require '[futon0.futonzero.daisyworld :as dw]
         '[clojure.string :as str])

(def T* 22.8)            ; daisy optimal temperature (peak of the seed-prob parabola)
(def LAMBDA 0.15)        ; R5b weight (ambiguity / temp spread)
(def GAMMA 0.9)          ; rollout discount
(def HORIZON 5)          ; rollout depth (policy length)
(def TAU 0.6)            ; R6 softmax temperature

(defn- mean [xs] (/ (reduce + xs) (double (count xs))))
(defn- g [{:keys [temp]}]
  (let [ts (vals temp) m (mean ts)
        spread (Math/sqrt (mean (map #(let [d (- % m)] (* d d)) ts)))]
    (+ (Math/abs (- m T*)) (* LAMBDA spread))))     ; R5a pragmatic + R5b ambiguity

(defn- extreme-cell [{:keys [temp]} pick]
  ;; hottest (pick max) or coldest (pick min) cell not already that breed
  (->> temp (sort-by val) ((if (= pick :hot) last first)) key))

(defn- apply-action [w a]
  (case a
    :cool (let [c (extreme-cell w :hot)]  (assoc-in w [:daisy c] {:breed :white :age 0}))
    :warm (let [c (extreme-cell w :cold)] (assoc-in w [:daisy c] {:breed :black :age 0}))
    w))

(def ACTIONS [:cool :warm :none])
(defn- forward [w a scenario] (dw/step (apply-action w a) scenario))

(defn- prior [w]
  ;; R1 PUCT prior: cool when hot, warm when cold, else none — softmax over heuristic logits
  (let [m (mean (vals (:temp w)))
        logits {:cool (* 0.4 (- m T*)) :warm (* 0.4 (- T* m)) :none 0.2}
        mx (apply max (vals logits))
        es (into {} (map (fn [[k v]] [k (Math/exp (- v mx))]) logits))
        z (reduce + (vals es))]
    (into {} (map (fn [[k v]] [k (/ v z)]) es))))

(defn- rollout-G [w first-a scenario]
  ;; G(pi) = discounted sum of g over an H-step policy: first-a, then CONTINUE under the prior (argmax)
  (loop [w (forward w first-a scenario) t 0 acc 0.0]
    (if (>= t HORIZON)
      acc
      (let [a (key (apply max-key val (prior w)))]
        (recur (forward w a scenario) (inc t) (+ acc (* (Math/pow GAMMA t) (g w))))))))

(defn- select-action [w scenario]
  ;; R6: softmax over -G(pi)/tau, PUCT-weighted by the prior; abstain (:none) if policies ~tie
  (let [pr (prior w)
        gs (into {} (map (fn [a] [a (rollout-G w a scenario)]) ACTIONS))
        best (apply min-key val gs) worst (apply max-key val gs)
        spread (- (val worst) (val best))
        scored (into {} (map (fn [[a gv]] [a (* (pr a) (Math/exp (- (/ gv TAU))))]) gs))]
    {:action (if (< spread 0.25) :none (key (apply max-key val scored)))   ; abstain when flat
     :G gs :realized (g w)}))                                              ; :realized = R2 return-channel hook

;; ---- frame capture for the viz ----
(defn- tq [t] (max 0 (min 9 (int (/ (+ t 10) 5.0)))))   ; temp -> 0..9 heat bucket
(defn- frame [{:keys [temp daisy lum tick]}]
  (let [cells (for [y (range dw/H) x (range dw/W)] [x y])
        grid (apply str (map (fn [c] (case (:breed (daisy c)) :black \B :white \W \.)) cells))
        heat (apply str (map (fn [c] (char (+ 48 (tq (temp c))))) cells))
        b (count (filter #(= :black (:breed %)) (vals daisy)))
        wht (count (filter #(= :white (:breed %)) (vals daisy)))]
    {:t tick :lum (format "%.3f" lum) :mt (format "%.1f" (mean (vals temp)))
     :b b :w wht :grid grid :heat heat}))

(defn- run-mode [{:keys [seed ticks scenario play?]}]
  (loop [w (dw/init-world seed {}) frames [] errs []]
    (if (> (:tick w) ticks)
      {:frames frames :reg-err (format "%.2f" (mean errs))}
      (let [sel (when play? (select-action w scenario))
            w' (if play? (forward w (:action sel) scenario) (dw/step w scenario))
            frames (if (zero? (mod (:tick w) 4)) (conj frames (frame w)) frames)]
        (recur w' frames (conj errs (Math/abs (- (mean (vals (:temp w))) T*))))))))

(defn- viz-html [auto play]
  (let [j (fn [fs] (str "[" (str/join ","
                 (map #(format "{\"t\":%d,\"lum\":%s,\"mt\":%s,\"b\":%d,\"w\":%d,\"g\":\"%s\",\"h\":\"%s\"}"
                               (:t %) (:lum %) (:mt %) (:b %) (:w %) (:grid %) (:heat %)) fs)) "]"))]
    (str "<!doctype html><meta charset=utf-8><title>Daisyworld — autonomous vs G(π) player</title>
<style>body{font:14px system-ui;background:#0d0f14;color:#dde;margin:0;padding:14px}
.row{display:flex;gap:24px}.col{text-align:center}canvas{image-rendering:pixelated;border:1px solid #333;width:300px;height:300px}
button,input{font:13px system-ui}h3{margin:6px}.k{color:#8ab}</style>
<h2>Daisyworld — autonomous (left) vs G(π)-steered player (right) &nbsp;<span class=k>:change scenario (sun brightens)</span></h2>
<div class=row>
 <div class=col><h3>autonomous</h3><canvas id=a width=30 height=30></canvas><div id=sa></div></div>
 <div class=col><h3>G(π) player</h3><canvas id=p width=30 height=30></canvas><div id=sp></div></div>
</div>
<div style='margin-top:10px'><button id=play>▶ play</button>
 <input id=sl type=range min=0 value=0 style='width:520px'> <span id=tk></span>
 &nbsp;<label><input type=checkbox id=heat> heat</label></div>
<p class=k>reg-err (mean |mean-temp − 22.8|): autonomous=" (:reg-err auto) " &nbsp; player=" (:reg-err play)
"&nbsp; — lower = better homeostasis</p>
<script>
const W=" dw/W ",H=" dw/H ";
['a','p'].forEach(function(id){var cv=document.getElementById(id);cv.width=W;cv.height=H;});
const A=" (j (:frames auto)) ",P=" (j (:frames play)) ";
const N=Math.min(A.length,P.length);const sl=document.getElementById('sl');sl.max=N-1;
const HC=['#08306b','#2171b5','#4292c6','#6baed6','#9ecae1','#c6dbef','#fdd0a2','#fdae6b','#fd8d3c','#e6550d'];
function draw(cv,f,heat){const x=cv.getContext('2d');for(let i=0;i<W*H;i++){const r=Math.floor(i/W),c=i%W;
 if(heat){x.fillStyle=HC[+f.h[i]];}else{const d=f.g[i];x.fillStyle=d=='B'?'#111':d=='W'?'#fff':'#243';}
 x.fillRect(c,r,1,1);} if(!heat){x.globalAlpha=1;}}
function show(i){const fa=A[i],fp=P[i];const heat=document.getElementById('heat').checked;
 draw(document.getElementById('a'),fa,heat);draw(document.getElementById('p'),fp,heat);
 document.getElementById('sa').innerHTML='lum '+fa.lum+' &nbsp; mean-temp '+fa.mt+'° &nbsp; ●'+fa.b+' ○'+fa.w;
 document.getElementById('sp').innerHTML='lum '+fp.lum+' &nbsp; mean-temp '+fp.mt+'° &nbsp; ●'+fp.b+' ○'+fp.w;
 document.getElementById('tk').textContent='tick '+fa.t;}
sl.oninput=()=>show(+sl.value);let playing=null;
document.getElementById('play').onclick=function(){if(playing){clearInterval(playing);playing=null;this.textContent='▶ play';return;}
 this.textContent='⏸ pause';playing=setInterval(()=>{let v=(+sl.value+1)%N;sl.value=v;show(v);},120);};
document.getElementById('heat').onchange=()=>show(+sl.value);show(0);
</script>")))

(defn -main [& args]
  (let [ticks (if (seq args) (Integer/parseInt (first args)) 200)
        scenario :change
        _ (println "running autonomous…")
        auto (run-mode {:seed 20260623 :ticks ticks :scenario scenario :play? false})
        _ (println "running G(π) player… (rollout depth" HORIZON ")")
        play (run-mode {:seed 20260623 :ticks ticks :scenario scenario :play? true})
        out "/home/joe/code/futon0/data/daisyworld-viz.html"]
    (spit out (viz-html auto play))
    (println "\n=== homeostasis under :change (reg-err = mean |mean-temp - 22.8|, lower=better) ===")
    (println "  autonomous :" (:reg-err auto) "   G(π) player :" (:reg-err play))
    (println "  player beats autonomous:" (< (Double/parseDouble (:reg-err play))
                                             (Double/parseDouble (:reg-err auto))))
    (println "  viz ->" out)))

(when (= *file* (System/getProperty "babashka.file")) (apply -main *command-line-args*))
