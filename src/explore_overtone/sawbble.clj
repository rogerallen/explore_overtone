(ns explore_overtone.sawbble
  (:use [overtone.live]))

;; Rewriting this to go step-by-step into creating a saw synth

;; ======================================================================
;; Step 1 - the basic synth 

(defsynth saw-synth-1
  "a basic synth with an envelope so that we could control via a midi
   keyboard.  When gated, or if the envelope goes to a level of 0, the
   instrument is freed."
  [pitch-midi         60
   adsr-attack-time   0.1
   adsr-decay-time    0.3
   adsr-sustain-level 0.5
   adsr-release-time  0.1
   adsr-peak-level    0.7
   adsr-curve         -4
   gate               1.0]
  (let [pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq) ;; saw is stereo
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate
                         :action FREE)]
    (out 0 (pan2 (* env-out saw-out)))))

;; ======================================================================
;; Step 1a - play with the synth

;; (def saw-1 (saw-synth-1 :pitch-midi 60))
;; (ctl saw-1 :pitch-midi 64)
;; (ctl saw-1 :pitch-midi 63)
;; (ctl saw-1 :pitch-midi 61)
;; (ctl saw-1 :gate 0)

;; ======================================================================
;; Step 2 - mouse synth with the mouse-y controlling the pitch
;; create a buffer
(def scale-buffer (buffer 16))
;; fill it with a scale
(doseq [[i n] (map-indexed
               vector
               (scale :d2 :minor (range 1 16)))]
  (buffer-set! scale-buffer i n))
;; synth
(defsynth saw-synth-2
  "a basic synth with an envelope and pitch that we control via mouse.
   When gated, or if the envelope goes to a level of 0, the instrument
   is freed."
  [adsr-attack-time   0.1
   adsr-decay-time    0.3
   adsr-sustain-level 0.5
   adsr-release-time  0.1
   adsr-peak-level    0.7
   adsr-curve         -4
   gate               1.0]
  (let [pitch-midi (index:kr (:id scale-buffer) (mouse-y 0 16))
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate
                         :action FREE)]
    (out 0 (pan2 (* env-out saw-out)))))

;; ======================================================================
;; Step 2a - play with the synth & scale

;; move your mouse up & down
;; (def saw-2 (saw-synth-2))
;; (ctl saw-2 :gate 0)

;; ======================================================================
;; Step 3 - add detuned 2nd frequency
(defsynth saw-synth-3
  "a detuned saw synth with an envelope and pitch that we control via
   mouse.  When gated, or if the envelope goes to a level of 0, the
   instrument is freed."
  [lfo-max-level      4.0
   lfo-freq           2.0
   adsr-attack-time   0.1
   adsr-decay-time    0.3
   adsr-sustain-level 0.5
   adsr-release-time  0.1
   adsr-peak-level    0.7
   adsr-curve         -4
   gate               1.0]
  (let [pitch-midi (index:kr (:id scale-buffer) (mouse-y 0 16))
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* (mouse-x 0 lfo-max-level) (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate
                         :action FREE)]
    (out 0 (pan2 (* env-out saws-out)))))

;; ======================================================================
;; Step 3a - play with the synth, scale & detuning

;; (def saw-3 (saw-synth-3)) ;; move your mouse up/down left/right
;; (ctl saw-3 :lfo-max-level 8.0)
;; (ctl saw-3 :lfo-freq 8.0)
;; (ctl saw-3 :gate 0)

;; ======================================================================
;; Step 4 - add separation delay between the stereo channels
(defsynth saw-synth-4
  "a detuned and stereo separated saw synth with an envelope and pitch
   that we control via mouse.  When gated, or if the envelope goes to
   a level of 0, the instrument is freed."  
  [max-separation-delay 0.5
   lfo-level            1.5
   lfo-freq             4.0
   adsr-attack-time     0.1
   adsr-decay-time      0.3
   adsr-sustain-level   0.5
   adsr-release-time    0.1
   adsr-peak-level      0.7
   adsr-curve           -4
   gate                 1.0]
  (let [pitch-midi (index:kr (:id scale-buffer) (mouse-y 0 16))
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* lfo-level (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        saws-out-2ch [saws-out (delay-c saws-out
                                        max-separation-delay
                                        (mouse-x 0 max-separation-delay))]
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate
                         :action FREE)]
    (out 0 (* env-out saws-out-2ch))))

;; ======================================================================
;; Step 4a - play with the synth, scale & stereo separation

;; (def saw-4 (saw-synth-4))
;; (ctl saw-4 :gate 0)

;; ======================================================================
;; Step 5 - add low-pass filter to the output
(defsynth saw-synth-5
  "a detuned and stereo separated saw synth with LPF, an envelope and pitch
   that we control via mouse.  When gated, or if the envelope goes to
   a level of 0, the instrument is freed."  
  [lpf-min-freq       400
   lpf-max-freq       10000
   lpf-res            0.3
   separation-delay   0.1
   lfo-level          1.5
   lfo-freq           4.0
   adsr-attack-time   0.1
   adsr-decay-time    0.3
   adsr-sustain-level 0.5
   adsr-release-time  0.1
   adsr-peak-level    0.7
   adsr-curve         -4
   gate               1.0]
  (let [pitch-midi (index:kr (:id scale-buffer) (mouse-y 0 16))
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* lfo-level (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        saws-out-2ch [saws-out (delay-c saws-out 1.0 separation-delay)]
        lpf-freq (mouse-x lpf-min-freq lpf-max-freq 1)
        lpf-out-2ch (moog-ff saws-out-2ch lpf-freq lpf-res)
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate
                         :action FREE)]
    (out 0 (* env-out lpf-out-2ch))))

;; ======================================================================
;; Step 5a - play with the synth, scale & lpf frequency

;; (def saw-5 (saw-synth-5))
;; (ctl saw-5 :separation-delay 0.2)
;; (ctl saw-5 :lpf-res 0.9)
;; (ctl saw-5 :gate 0)


;; ======================================================================
;; Step 6 - add lfo on the low-pass filter
(defsynth saw-synth-6
  "a detuned and stereo separated saw synth with LPF, an envelope and pitch
   that we control via mouse.  When gated, or if the envelope goes to
   a level of 0, the instrument is freed."  
  [lpf-min-freq       2000
   lpf-max-freq       8000
   lpf-lfo-max-freq   10
   lpf-res            0.3
   separation-delay   0.1
   lfo-level          1.5
   lfo-freq           4.0
   adsr-attack-time   0.1
   adsr-decay-time    0.3
   adsr-sustain-level 0.5
   adsr-release-time  0.1
   adsr-peak-level    0.7
   adsr-curve         -4
   gate               1.0]
  (let [pitch-midi (index:kr (:id scale-buffer) (mouse-y 0 16))
        pitch-freq (midicps pitch-midi)
        saw-out (saw pitch-freq)
        lfo-out (* lfo-level (sin-osc lfo-freq))
        detuned-out (saw (+ pitch-freq lfo-out))
        saws-out (mix [saw-out detuned-out])
        saws-out-2ch [saws-out (delay-c saws-out 1.0 separation-delay)]
        lpf-lfo-freq (mouse-x 0 lpf-lfo-max-freq)
        lpf-freq (lin-lin (sin-osc lpf-lfo-freq) -1 1 lpf-min-freq lpf-max-freq)
        lpf-out-2ch (moog-ff saws-out-2ch lpf-freq lpf-res)
        env-out (env-gen (adsr adsr-attack-time   adsr-decay-time
                               adsr-sustain-level adsr-release-time
                               adsr-peak-level    adsr-curve)
                         :gate gate
                         :action FREE)]
    (out 0 (* env-out lpf-out-2ch))))

;; ======================================================================
;; Step 6a - play with the synth, scale & lpf-lfo frequency

;; (def saw-6 (saw-synth-6))
;; (ctl saw-6 :lpf-min-freq 200)
;; (ctl saw-6 :separation-delay 0.05)
;; (ctl saw-6 :lpf-res 0.1)
;; (ctl saw-6 :lfo-freq 2.0)
;; (ctl saw-6 :gate 0)

;; ======================================================================
;; and here is the full synth.  FIXME reconcile the parameters
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
