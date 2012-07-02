(ns explore_overtone.play_with_echo)

;; use TouchOSC on iPhone to play with parameters of fx-my-echo.
;; "mix 2" panel
;;   lower 4 boxes == note events
;;   bottom blue dial -> note-offset
;;   middle red dial -> delay-time
;;   middle green dial -> decay-time

;; NOTE!  max-delay must be set at startup & not changed!

(use 'overtone.live)
;;(use 'overtone.core)
;;(connect-external-server)
(use 'overtone.inst.sampled-piano)

;; (event-debug-on) 
;; (event-debug-off) 

(def MY-STATE
  (atom {:note-offset 20
         :the-fx      nil
         :delay-time  1.0
         :decay-time  1.0
         :lpf-freq    800}))

(defn linear-map
  "given points (x0,y0), (x1,y1) calculate linear relation y given x"
  [x0 x1 y0 y1 x]
  (let [dydx (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ y0 (* dydx dx))))

(defn velocity2attack
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 127 0.2 0.05 v))

(defn velocity2level
  "sampled-piano uses attack & level, not velocity"
  [v]
  (linear-map 0 127 0.0 0.8 v))

(defsynth fx-my-echo
  [bus 0 max-delay 10.0 delay-time 1.0 decay-time 1.0 lpf-freq 800]
  (let [source (in bus)
        echo (comb-c source max-delay delay-time decay-time)]
    (replace-out bus (pan2 (rlpf (+ echo source) lpf-freq 0.5) 0))))

(defn ctl-fx []
  (ctl (:the-fx @MY-STATE) :delay-time (:delay-time @MY-STATE))
  (ctl (:the-fx @MY-STATE) :decay-time (:decay-time @MY-STATE))
  (ctl (:the-fx @MY-STATE) :lpf-freq (:lpf-freq @MY-STATE)))
  
(defn setup-fx [inst]
  (clear-fx inst)
  (swap! MY-STATE assoc :the-fx (inst-fx! inst fx-my-echo))
  (ctl-fx))

(setup-fx sampled-piano)

(defn my-state-watcher
  [key identity old new]
  (let [{:keys [note-offset delay-time decay-time lpf-freq]} new]
    (println
     (format "WATCH: note-offset %d delay-time %.2f decay-time %.2f lpf-freq %d"
             note-offset delay-time decay-time lpf-freq))
    (ctl-fx)))

(add-watch MY-STATE :watcher my-state-watcher)

(on-event
 [:midi :note-on] 
 (fn [e] 
   (let [note (+ (:note-offset @MY-STATE) (:note e)) 
         level (velocity2level (:velocity e))
         attack (velocity2attack (:velocity e))] 
     ;;(println "!non" note level attack)
     (sampled-piano :note note
                    :level level
                    :attack attack)))
 ::my-midi-note-on-handler)

;; controls all assume "mix 2" pane
(on-event
 [:midi :control-change] 
 (fn [e] 
   (let [controller (:data1 e)
         data (:data2 e)] 
     (println "!cc" controller data)
     (cond
      ;; bottom blue dial -> note-offset
      (= controller 0) 
      (swap! MY-STATE assoc :note-offset
             (int (/ data 3.0)))
      ;; middle red dial -> delay-time
      (= controller 1)
      (swap! MY-STATE assoc :delay-time
             (+ 0.01 (* 2 (/ data 127.0)))) 
      ;; middle green dial -> decay-time
      (= controller 2)
      (swap! MY-STATE assoc :decay-time
             (+ 0.01 (* 2 (/ data 127.0)))) 
      ;; lower-right dial -> lpf-freq
      (= controller 12)
      (swap! MY-STATE assoc :lpf-freq
             (int (+ 440 (* 50 data))))
      )))
 ::my-midi-control-change-handler)


