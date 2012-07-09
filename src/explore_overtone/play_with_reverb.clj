;;(ns explore-overtone.play-with-reverb)

;; use TouchOSC on iPhone to play with reverb parameters

(use 'overtone.live)
;;(use 'overtone.core)
;;(connect-external-server)
(use 'overtone.inst.sampled-piano)
;; (event-debug-on) 
;; (event-debug-off)
(println "get midi bridge output")
(def g-device (midi-out))

;;(def g-metronome (metronome 60))
(def g-reverb-fx (atom [nil]))
(def g-reverb-params
  (atom [{:name :roomsize :min 1.0 :max 150.0 :ctl 0 :val 10.0}
         {:name :revtime :min 0.1 :max 10.0 :ctl 1 :val 3.0}
         {:name :damping :min 0.1 :max 1.0 :ctl 2 :val 0.5}
         {:name :inputbw :min 0.1 :max 1.0 :ctl 3 :val 0.5}
         {:name :spread :min 1.0 :max 30.0 :ctl 4 :val 1.0}
         {:name :drylevel :min 0.0 :max 1.0 :ctl 5 :val 0.1}
         {:name :earlyreflevel :min 0.0 :max 1.0 :ctl 6 :val 0.1}
         {:name :taillevel :min 0.0 :max 1.0 :ctl 7 :val 0.5}
         {:name :lpf-freq :min 40.0 :max 4000.0 :ctl 8 :val 800.0}]))
(def g-piano-notes (atom {}))

(defn linear-map
  "given points (x0,y0), (x1,y1) calculate linear relation y given x"
  [x0 x1 y0 y1 x]
  (let [dydx (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ y0 (* dydx dx))))

(defn param-from-ctl [ctl-num]
  (let [param (first (filter #(= ctl-num (:ctl %)) @g-reverb-params))
        index (.indexOf @g-reverb-params param)]
    [param index]))

;; if you don't gate while you change, it creates horrible feedback/noise
(defn param-update-fx [param]
  (ctl @g-reverb-fx :gate 0.0)
  (ctl @g-reverb-fx (:name param) (:val param))
  (ctl @g-reverb-fx :gate 1.0))

(defn val2midi [param]
  (int (linear-map (:min param) (:max param) 0 127 (:val param))))

(defn midival2val [param midi-val]
  (linear-map 0 127 (:min param) (:max param) midi-val))

(defn param-update-view [param]
  (midi-control g-device (:ctl param) (val2midi param)))

(defn param-send-all []
  (doseq [param @g-reverb-params]
    (param-update-fx param)
    (param-update-view param)))

;; FIXME -- make a piano with velocity as a param.
(defn velocity2attack
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 127 0.2 0.05 v))

(defn velocity2level
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 127 0.0 0.8 v))

(defsynth fx-my-reverb
  [bus 0
   roomsize 10.0,
   revtime 3.0,
   damping 0.5,
   inputbw 0.5,
   spread 15.0,
   drylevel 1.0,
   earlyreflevel 0.7,
   taillevel 0.5,
   maxroomsize 300.0
   lpf-freq 800.0
   gate 1.0]
  (let [source (in bus)
        my-reverb (* gate
                     (g-verb (* gate source)
                             roomsize revtime damping inputbw spread
                             drylevel earlyreflevel taillevel maxroomsize))
        lpf-reverb (lpf my-reverb lpf-freq)]
    (replace-out bus (+ lpf-reverb source))))

(defn setup-fx [inst]
  (clear-fx inst)
  (reset! g-reverb-fx (inst-fx! inst fx-my-reverb))
  (param-send-all))
(setup-fx sampled-piano)

;; keep this handy if all goes haywire
;; (clear-fx sampled-piano)

(defn my-state-watcher
  [key identity old new]
  (dotimes [i (count new)]
    (if (not= (nth new i) (nth old i))
      (let [param (nth new i)]
        (println (format "%s %.2f" (:name param) (:val param)))
        (param-update-fx param)
        (param-update-view param)))))

(add-watch g-reverb-params :watcher my-state-watcher)

(defn dump-reverb-params []
  (doseq [param @g-reverb-params]
    (println (format "%s %.2f" (:name param) (:val param)))))
    
(on-event
 [:midi :note-on] 
 (fn [e] 
   (let [note (:note e)
         level (velocity2level (:velocity e))
         attack (velocity2attack (:velocity e))] 
     (reset! g-piano-notes (assoc @g-piano-notes note
                                  (sampled-piano :note note
                                                 :level level
                                                 :attack attack)))))
 ::my-midi-note-on-handler)

(on-event
 [:midi :note-off] 
 (fn [e] 
   (let [note (:note e)]
     ;; avoid race condition. wait for that note to appear
     ;; (HANG ALERT)
     (while (= nil (@g-piano-notes note))
       nil)
     (ctl (@g-piano-notes note) :gate 0.0)
     (reset! g-piano-notes (dissoc @g-piano-notes note))))
 ::my-midi-note-off-handler)

(on-event
 [:midi :control-change] 
 (fn [e] 
   (let [controller (:data1 e)
         data (:data2 e)
         [param param-index] (param-from-ctl controller)]
     ;;(println "!cc" controller data)
     (swap! g-reverb-params assoc param-index
            (assoc param :val (midival2val param data)))))
 ::my-midi-control-change-handler)


