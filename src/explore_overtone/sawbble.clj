(ns explore_overtone.sawbble
  (:use [overtone.live]
        [overtone.gui.control]))

;; Rewriting this to go step-by-step into creating a saw synth

;; ======================================================================
;; a helpful controller for playing with the synths
(defn run-synth [the-synth]
  (let [the-instance (the-synth)
        the-watchers (map 
                      (fn [param] 
                        (add-watch
                         (:value param)
                         (keyword (:name param)) 
                         (fn [_ _ _ val] (ctl the-instance (keyword (:name param)) val))))
                      (:params the-synth))
        the-controller (synth-controller the-synth)]
    ;; pass them out of the fn to ensure they stay active
    (vector the-instance the-watchers the-controller)
    ))

;; ======================================================================
;; Step 1 - the basic synth 
;;
;; NOTE - to get this working with the GUI controller, I remove an
;; important bit of code from the env-gen call: :action FREE.  For a
;; "real" synth, you will want it to disappear wen the envelope goes
;; to 0.
;;
(defsynth saw-synth-1
  "a basic saw synth with an envelope"
  [pitch-midi         {:default 60  :min 40   :max 70 :step 1}
   adsr-attack-time   {:default 0.1 :min 0.05 :max 0.5 :step 0.05}
   adsr-decay-time    {:default 0.3 :min 0.05 :max 0.5 :step 0.05}
   adsr-sustain-level {:default 0.5 :min 0.0  :max 1.0 :step 0.01}
   adsr-release-time  {:default 0.1 :min 0.05 :max 0.5 :step 0.05}
   adsr-peak-level    {:default 0.7 :min 0.0  :max 1.0 :step 0.01}
   adsr-curve         {:default -4  :min -5   :max 5   :step 1} ;; what are valid values here?
   gate               {:default 1.0 :min 0.0  :max 1.0 :step 1}]
  (let [pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate)]
    (out 0 (pan2 (* env-out saw-out)))))

;; play with the synth (window may "pop under")
;; toggle the "gate" control to turn a note on & off
(run-synth saw-synth-1)

;; ======================================================================
;; Step 2 - use better scale than the chromatic scale

;; create a buffer
(def scale-buffer (buffer 16))
;; fill it with a scale
(doseq [[i n] (map-indexed
               vector
               (scale :d2 :minor (range 1 16)))]
  (buffer-set! scale-buffer i n))

;; synth
(defsynth saw-synth-2
  "a basic saw synth with an envelope"
  [pitch-index        {:default 0   :min 0    :max 15  :step 1}
   adsr-attack-time   {:default 0.1 :min 0.05 :max 0.5 :step 0.05}
   adsr-decay-time    {:default 0.3 :min 0.05 :max 0.5 :step 0.05}
   adsr-sustain-level {:default 0.5 :min 0.0  :max 1.0 :step 0.01}
   adsr-release-time  {:default 0.1 :min 0.05 :max 0.5 :step 0.05}
   adsr-peak-level    {:default 0.7 :min 0.0  :max 1.0 :step 0.01}
   adsr-curve         {:default -4  :min -5   :max 5   :step 1} ;; what are valid values here?
   gate               {:default 1.0 :min 0.0  :max 1.0 :step 1}]
  (let [pitch-midi (index:kr (:id scale-buffer) pitch-index)
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate)]
    (out 0 (pan2 (* env-out saw-out)))))

;; play with the synth & scale
(run-synth saw-synth-2)


;; ======================================================================
;; Step 3 - add detuned 2nd frequency
(defsynth saw-synth-3
  "a detuned saw synth with an envelope"
  [lfo-level          {:default 2.0 :min 0.0  :max 5.0  :step 0.05}
   lfo-freq           {:default 2.0 :min 0.0  :max 10.0 :step 0.1}
   pitch-index        {:default 0   :min 0    :max 15   :step 1}
   adsr-attack-time   {:default 0.1 :min 0.05 :max 0.5  :step 0.05}
   adsr-decay-time    {:default 0.3 :min 0.05 :max 0.5  :step 0.05}
   adsr-sustain-level {:default 0.5 :min 0.0  :max 1.0  :step 0.01}
   adsr-release-time  {:default 0.1 :min 0.05 :max 0.5  :step 0.05}
   adsr-peak-level    {:default 0.7 :min 0.0  :max 1.0  :step 0.01}
   adsr-curve         {:default -4  :min -5   :max 5    :step 1} ;; what are valid values here?
   gate               {:default 1.0 :min 0.0  :max 1.0  :step 1}]
  (let [pitch-midi (index:kr (:id scale-buffer) pitch-index)
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* lfo-level (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate)]
    (out 0 (pan2 (* env-out saws-out)))))

;; play with the synth, scale & detuning (lfo-level & freq)
(run-synth saw-synth-3)

;; ======================================================================
;; Step 4 - add separation delay between the stereo channels
(defsynth saw-synth-4
  "a detuned and stereo-separated saw synth with an envelope"  
  [separation-delay   {:default 0.1 :min 0.0  :max 1.0  :step 0.01}
   lfo-level          {:default 2.0 :min 0.0  :max 5.0  :step 0.05}
   lfo-freq           {:default 2.0 :min 0.0  :max 10.0 :step 0.1}
   pitch-index        {:default 0   :min 0    :max 15   :step 1}
   adsr-attack-time   {:default 0.1 :min 0.05 :max 0.5  :step 0.05}
   adsr-decay-time    {:default 0.3 :min 0.05 :max 0.5  :step 0.05}
   adsr-sustain-level {:default 0.5 :min 0.0  :max 1.0  :step 0.01}
   adsr-release-time  {:default 0.1 :min 0.05 :max 0.5  :step 0.05}
   adsr-peak-level    {:default 0.7 :min 0.0  :max 1.0  :step 0.01}
   adsr-curve         {:default -4  :min -5   :max 5    :step 1} ;; what are valid values here?
   gate               {:default 1.0 :min 0.0  :max 1.0  :step 1}]
  (let [pitch-midi (index:kr (:id scale-buffer) pitch-index)
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* lfo-level (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        saws-out-2ch [saws-out (delay-c saws-out 1.0 separation-delay)] ;; FIXME 1.0 
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate)]
    ;; NOTE -- no more pan2
    (out 0 (* env-out saws-out-2ch))))

;;  play with the stereo separation
(run-synth saw-synth-4)

;; ======================================================================
;; Step 5 - add low-pass filter to the output
(defsynth saw-synth-5
  "a detuned and stereo-separated saw synth with a low-pass-filter."  
  [lpf-freq           {:default 400 :min 100  :max 10000 :step 100}
   lpf-res            {:default 0.3 :min 0.0  :max 1.0   :step 0.05}
   separation-delay   {:default 0.1 :min 0.0  :max 1.0   :step 0.01}
   lfo-level          {:default 2.0 :min 0.0  :max 5.0   :step 0.05}
   lfo-freq           {:default 2.0 :min 0.0  :max 10.0  :step 0.1}
   pitch-index        {:default 0   :min 0    :max 15    :step 1}
   adsr-attack-time   {:default 0.1 :min 0.05 :max 0.5   :step 0.05}
   adsr-decay-time    {:default 0.3 :min 0.05 :max 0.5   :step 0.05}
   adsr-sustain-level {:default 0.5 :min 0.0  :max 1.0   :step 0.01}
   adsr-release-time  {:default 0.1 :min 0.05 :max 0.5   :step 0.05}
   adsr-peak-level    {:default 0.7 :min 0.0  :max 1.0   :step 0.01}
   adsr-curve         {:default -4  :min -5   :max 5     :step 1} ;; what are valid values here?
   gate               {:default 1.0 :min 0.0  :max 1.0   :step 1}]
  (let [pitch-midi (index:kr (:id scale-buffer) pitch-index)
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* lfo-level (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        saws-out-2ch [saws-out (delay-c saws-out 1.0 separation-delay)]
        lpf-out-2ch (moog-ff saws-out-2ch lpf-freq lpf-res)
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate)]
    (out 0 (* env-out lpf-out-2ch))))

;; play with the synth & lpf frequency
(run-synth saw-synth-5)

;; ======================================================================
;; Step 6 - add lfo on the low-pass filter
(defsynth saw-synth-6
  "a detuned and stereo-separated saw synth with a low-pass-filter and low-pass-filter LFO."  
  [lpf-lfo-freq       {:default 4    :min 0.0  :max 10.0  :step 0.01}
   lpf-min-freq       {:default 400  :min 100  :max 10000 :step 100}
   lpf-max-freq       {:default 4000 :min 100  :max 10000 :step 100}
   lpf-res            {:default 0.3  :min 0.0  :max 1.0   :step 0.05}
   separation-delay   {:default 0.1  :min 0.0  :max 1.0   :step 0.01}
   lfo-level          {:default 2.0  :min 0.0  :max 5.0   :step 0.05}
   lfo-freq           {:default 2.0  :min 0.0  :max 10.0  :step 0.1}
   pitch-index        {:default 0    :min 0    :max 15    :step 1}
   adsr-attack-time   {:default 0.1  :min 0.05 :max 0.5   :step 0.05}
   adsr-decay-time    {:default 0.3  :min 0.05 :max 0.5   :step 0.05}
   adsr-sustain-level {:default 0.5  :min 0.0  :max 1.0   :step 0.01}
   adsr-release-time  {:default 0.1  :min 0.05 :max 0.5   :step 0.05}
   adsr-peak-level    {:default 0.7  :min 0.0  :max 1.0   :step 0.01}
   adsr-curve         {:default -4   :min -5   :max 5     :step 1} ;; what are valid values here?
   gate               {:default 1.0  :min 0.0  :max 1.0   :step 1}]
  (let [pitch-midi (index:kr (:id scale-buffer) pitch-index)
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* lfo-level (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        saws-out-2ch [saws-out (delay-c saws-out 1.0 separation-delay)]
        lpf-freq (lin-lin (sin-osc lpf-lfo-freq) -1 1 lpf-min-freq lpf-max-freq)
        lpf-out-2ch (moog-ff saws-out-2ch lpf-freq lpf-res)
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate)]
    (out 0 (* env-out lpf-out-2ch))))

;; play with the synth, scale & lpf-lfo frequency
(run-synth saw-synth-6)

;; ======================================================================
;; and here is the full synth.  FIXME reconcile the parameters, etc.
(defsynth sawbble-synth
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

;; (def sawbble (sawbble-synth 30))
;; (stop)
;; (ctl sawbble :note 35)
;; (ctl sawbble :note 32)
;; (ctl sawbble :note 30)
;; (ctl sawbble :lfo-level 1.5 :lfo-freq 2.0)
;; (ctl sawbble :note-slew 15.0)
;; (ctl sawbble :lp-freq-lo 200.0 :lp-freq-hi 4000.0 :lp-lfo-freq 5.0)
;; (ctl sawbble :lp-res 0.75)
;; (ctl sawbble :sep-delay 0.02)
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
  (partial sawbble-synth 
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
