(ns explore_overtone.sawbble)
;;
;; just started with a saw wave and then added a detuned saw.
;; then made that detuned saw vary by lfo-level/freq
;; adding those together creates a nice synth bass.
;;
;; then sent it through a low-pass filter (moog-ff) that also varies
;; by its own lfo.
;;
;; then added some separation delay.
;;
;; kinda fun!

;; (use 'overtone.live)
(definst sawbble
  [note          60
   volume         1.0
   attack         0.01
   decay          0.3
   sustain        0.5
   release        0.01
   level          1.0
   curve         -4
   gate           1
   lfo-level      0.5
   lfo-freq       2.0
   lp-freq-lo  400.0
   lp-freq-hi 4000.0
   lp-lfo-freq   7.0
   lp-res        0.3
   note-slew    50.0
   sep-delay     0.1
   ]
  (let [note       (slew:kr note note-slew note-slew)
        note-freq  (midicps note)
        saw-out    (saw note-freq)
        lfo-out    (* lfo-level (sin-osc lfo-freq))
        saw-out2   (saw (+ note-freq lfo-out))
        ;; it really is amazing the difference you add two detuned saws...
        saws       (mix [saw-out saw-out2])
        lp-lfo-out (lin-lin (sin-osc lp-lfo-freq) -1 1 lp-freq-lo lp-freq-hi)
        lp-out     (moog-ff saws lp-lfo-out lp-res)
        lp-out     [(delay-c lp-out 1 0.0) (delay-c lp-out 1 sep-delay)]
        env-out    (env-gen (adsr attack decay sustain release level curve)
                            :gate gate :action FREE)
        ]
    (out 0 (* volume env-out lp-out))))

;; (sawbble 30) 
;; (stop)
;; (ctl sawbble :note 35)
;; (ctl sawbble :note 32)
;; (ctl sawbble :note 30)
;; (ctl sawbble :lfo-level 1.5 :lfo-freq 2.0)
;; (ctl sawbble :note-slew 15.0)
;; (ctl sawbble :lp-freq-lo 200.0 :lp-freq-hi 4000.0 :lp-lfo-freq 5.0)
;; (ctl sawbble :lp-res 0.75)
;; (ctl sawbble :sep-delay 0.01)
;; (ctl sawbble :gate 0)

(defn play-notes [the-inst level m notes]
  (dotimes [i (count notes)]
    (let [nx (at (m i)
                 (the-inst :note (note (notes i)) :level level))]
      (at (m (+ i 0.75)) (ctl nx :gate 0)))))

(defn slew-notes [the-inst level m notes]
  (let [nx (at (m 0) (the-inst :note 0 :level 0))]
    (dotimes [i (count notes)]
      (at (m i) (ctl nx :note (note (notes i)) :level level)))
    (at (m (+ (count notes) 1)) (ctl nx :gate 0))))

(defn play-some [the-player the-inst level]
  (let [m (metronome 140)
        notes (apply vector (flatten (repeat 3 [:c2 :g2 :e3 :e2 :f3 :f2 :g3 :c2 :c2 :c1])))]
    (the-player the-inst level m notes)))

(def my-sawbble
  (partial sawbble 
           :lfo-level 1.5 :lfo-freq 0.7
           :note-slew 50.0
           :lp-freq-lo 500.0 :lp-freq-hi 4000.0 :lp-lfo-freq 0.35
           :lp-res 0.2
           :sep-delay 0.05
           ))
           
;; staccatto
(play-some play-notes my-sawbble 1.0)
;; legato
(play-some slew-notes my-sawbble 1.0)

(stop)
