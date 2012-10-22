(ns explore_overtone.guitar)
;; A Simple Guitar by Roger Allen.
;; ----------------------------------------------------------------------
(use 'overtone.live)

(def guitar-chord-frets
  ;; a map of chords to frets.
  ;; -1 indicates you don't play that string
  {:C   [ -1  3  2  0  1  0 ]
   :F   [ -1 -1  3  2  1  1 ]
   :G   [  3  2  0  0  0  3 ]
   :D   [ -1 -1  0  2  3  2 ]
   :A   [  0  0  2  2  2  0 ]
   :E   [  0  2  2  1  0  0 ]
   :Bb  [ -1 -1  3  3  3  1 ]
   :Eb  [ -1 -1  5  3  4  3 ]
   :Ab  [ -1 -1  6  5  4  4 ]
   :Db  [ -1 -1  3  1  2  1 ]
   :Gb  [ -1 -1  4  2  1  1 ]
   :B   [ -1 -1  4  4  4  2 ]
   
   :Cm  [ -1 -1  5  5  4  3 ]
   :Fm  [ -1 -1  3  1  1  1 ]
   :Gm  [ -1 -1  5  3  3  3 ]
   :Dm  [ -1  0  0  2  3  1 ]
   :Am  [  0  0  2  2  1  0 ]
   :Em  [  0  2  2  0  0  0 ]
   :Bbm [ -1 -1  3  3  2  1 ]
   :Ebm [ -1 -1  4  3  4  2 ]
   :Abm [ -1 -1  6  4  4  4 ]
   :Dbm [ -1 -1  2  1  2  0 ]
   :Gbm [ -1 -1  4  2  2  2 ]
   :Bm  [ -1 -1  4  4  3  2 ]
     
   :C7  [ -1  3  2  3  1 -1 ]
   :F7  [ -1 -1  1  2  1  1 ]
   :G7  [  3  2  0  0  0  1 ]
   :D7  [ -1 -1  0  2  1  2 ]
   :A7  [ -1 -1  2  2  2  3 ]
   :E7  [  0  2  0  1  0  0 ]
   :Bb7 [ -1 -1  3  3  3  4 ]
   :Eb7 [ -1 -1  1  3  2  3 ]
   :Ab7 [ -1 -1  1  1  1  2 ]
   :Db7 [ -1 -1  3  4  2  4 ]
   :Gb7 [ -1 -1  4  3  2  0 ]
   :B7  [ -1  2  1  2  0  2 ]

   :C9  [  3, -1,  2,  3,  3,  3 ]
   :F9  [ -1,  3, -1,  2,  4,  3 ]
   :G9  [ -1, -1,  0,  2,  0,  1 ]
   :D9  [ -1, -1,  4,  2,  1,  0 ]
   :A9  [  0,  0,  2,  4,  2,  3 ]
   :E9  [  0,  2,  0,  1,  3,  2 ]
   :Bb9 [ -1, -1,  0,  1,  1,  1 ]
   :Eb9 [ -1, -1,  1,  0,  2,  1 ]
   :Ab9 [ -1, -1,  1,  3,  1,  2 ]
   :Db9 [  4, -1,  3,  4,  4,  4 ]
   :Gb9 [ -1,  4, -1,  3,  5,  4 ]
   :B9  [  2, -1,  1,  2,  2,  2 ]

   :Gadd5 [  3  2  0  0  3  3 ]
   :Cadd9 [ -1  3  2  0  3  3 ]
   :Dsus4 [ -1 -1  0  2  3  3 ]
   })

;; the guitar string instrument is not freed.  Needs to be silenced with
;; a gate ->0 transition before a gate -> 1 transition activates it.
;; testing showed it needed something > 25 ms between these transitions.
;; output is sent on a bus, so that it can be mixed together with
;; the other strings.
(defsynth guitar-string-synth
  [out-bus   {:default 16 :rate :ar}
   note      {:default 60 :min 0 :max 127}
   dur       {:default 10.0}
   decay     {:default 30} ;; pluck decay
   coef      {:default 0.3 :min -1 :max 1} ;; pluck coef
   noise-amp {:default 0.8 :min 0.0 :max 1.0}
   gate      {:default 1}]
  (let [frq  (midicps note)
        nze  (* noise-amp (pink-noise))
        dly  (/ 1.0 frq)
        plk  (pluck nze gate (/ 1.0 8.0) dly decay coef)
        snd  (leak-dc (* plk (env-gen (asr 0.0001 1 0.1) :gate gate )) 0.995)]
    (out out-bus snd)))

;; the "amp" in the guitar that mixes the strings together
;; and adds optional distortion.
(defsynth guitar-synth [in-bus-0 {:default 16 :rate :ar}
                        in-bus-1 {:default 16 :rate :ar}
                        in-bus-2 {:default 16 :rate :ar}
                        in-bus-3 {:default 16 :rate :ar}
                        in-bus-4 {:default 16 :rate :ar}
                        in-bus-5 {:default 16 :rate :ar}
                        pre-amp  {:default 1.0}
                        amp      {:default 1.0}
                        distort  {:default 0.0 :min 0.0 :max 0.9999999999}]
  (let [src (* pre-amp (mix [(in in-bus-0 1) (in in-bus-1 1) (in in-bus-2 1)
                             (in in-bus-3 1) (in in-bus-4 1) (in in-bus-5 1)]))
        ;; distortion from fx-distortion2 
        k (/ (* 2 distort) (- 1 distort))
        dis (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))]
    (out 0 (pan2 (* amp dis) 0))))

;; create the individual string busses & synths -- defonce?
(def guitar-string-bus-0 (audio-bus))
(def guitar-string-bus-1 (audio-bus))
(def guitar-string-bus-2 (audio-bus))
(def guitar-string-bus-3 (audio-bus))
(def guitar-string-bus-4 (audio-bus))
(def guitar-string-bus-5 (audio-bus))
(def guitar-string-0 (guitar-string-synth :position :head guitar-string-bus-0 :gate 0))
(def guitar-string-1 (guitar-string-synth :position :head guitar-string-bus-1 :gate 0))
(def guitar-string-2 (guitar-string-synth :position :head guitar-string-bus-2 :gate 0))
(def guitar-string-3 (guitar-string-synth :position :head guitar-string-bus-3 :gate 0))
(def guitar-string-4 (guitar-string-synth :position :head guitar-string-bus-4 :gate 0))
(def guitar-string-5 (guitar-string-synth :position :head guitar-string-bus-5 :gate 0))
(def guitar-string-synths [guitar-string-0 guitar-string-1 guitar-string-2
                           guitar-string-3 guitar-string-4 guitar-string-5])
;; an array of 6 guitar strings: EADGBE
(def guitar-string-notes [(note :e2) (note :a2) (note :d3)
                          (note :g3) (note :b3) (note :e4)])

;; connect up the the full instrument
(def guitar (guitar-synth :position :tail
                          guitar-string-bus-0 guitar-string-bus-1 guitar-string-bus-2
                          guitar-string-bus-3 guitar-string-bus-4 guitar-string-bus-5))


;; Given a fret-offset, add to the base note index with special handling for -1
(defn- fret-to-note
  [base-note offset]
  (if (>= offset 0)
    (+ base-note offset)
    offset))

(defn pluck-string-at
  [string-index fret amp t]
  (let [the-note (fret-to-note (nth guitar-string-notes string-index) fret)] 
    ;; turn off the previous note
    (at t (ctl (guitar-string-synths string-index) :gate 0))
    ;; if part of the chord, turn on the current note
    ;; NOTE: there needs to be some time between these 
    (if (> the-note 0)
      (at (+ t 50) (ctl (guitar-string-synths string-index) :note the-note :amp amp :gate 1)))))

(defn pluck-string
  [string-index fret amp]
  (pluck-string-at string-index fret amp (now)))

;; strum a chord on the guitar
(defn strum-at
  ([the-time the-chord direction strum-time amp]
     ;; FIXME - deal with -1 fret offsets--shouldn't count as part of the delta time.
     (let [dt (* 1000 (/ strum-time 5))
           chord-frets (if (vector? the-chord)
                         the-chord ; treat the chord as a series of frets
                         (guitar-chord-frets the-chord))]
       (dotimes [i 6]
         (let [j (if (= direction :up) (- 5 i) i)]
           (pluck-string-at j (chord-frets j) amp (+ the-time (* i dt)))))))
  ([the-time the-chord direction strum-time]
     (strum-at the-time the-chord direction strum-time 0.5))
  ([the-time the-chord direction]
     (strum-at the-time the-chord direction 0.05 0.5))
  ([the-time the-chord]
     (strum-at the-time the-chord :down 0.05 0.5)))

(defn strum-now
  ([the-chord direction strum-time amp]
     (strum-at (now) the-chord direction strum-time amp))
  ([the-chord direction strum-time]
     (strum-now the-chord direction strum-time 0.5))
  ([the-chord direction]
     (strum-now the-chord direction 0.05 0.5))
  ([the-chord]
     (strum-now the-chord :down 0.05 0.5)))
