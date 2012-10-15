(ns explore_overtone.guitar)
;; A Simple Guitar by Roger Allen.
;; ----------------------------------------------------------------------
;; (use 'overtone.live)

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
;; (guitar-chord-offset-map :E9) (guitar-chord-offset-map :E)

;; the guitar string instrument is not freed.  Needs to be reset with
;; a gate ->0 transition before a gate -> 1 transition activates it.
;; testing showed it needed something > 25 ms between transitions, too.
(definst guitar-string-inst 
  [note  60
   amp   0.5
   dur   15.0
   decay 30
   coef  0.3
   gate  1]
  (let [freq  (midicps note)
        noize (* 0.8 (pink-noise))
        dly   (/ 1.0 freq)
        plk   (pluck noize gate (/ 1.0 40.0) dly decay coef)]
    (* amp (env-gen (perc 0.0001 dur) :gate gate ) plk)))

;; an array of 6 guitar strings: EADGBE
(def guitar-string-notes [(note :e2)
                          (note :a2)
                          (note :d3)
                          (note :g3)
                          (note :b3)
                          (note :e4)])

(def guitar-string-insts  [(guitar-string-inst :gate 0)
                           (guitar-string-inst :gate 0)
                           (guitar-string-inst :gate 0)
                           (guitar-string-inst :gate 0)
                           (guitar-string-inst :gate 0)
                           (guitar-string-inst :gate 0)])

;; given a fret-offset, add to the base note index with special handling for -1
(defn fret-to-note
  [base-note offset]
  (if (>= offset 0)
    (+ base-note offset)
    offset))

(defn pluck-string-at
  [string-index fret amp t]
  (let [the-note (fret-to-note (nth guitar-string-notes string-index) fret)] 
    ;; turn off the previous note
    (at t (ctl (guitar-string-insts string-index) :gate 0))
    ;; if part of the chord, turn on the current note
    ;; NOTE: there needs to be some time between these 
    (if (> the-note 0)
      (at (+ t 50) (ctl (guitar-string-insts string-index) :note the-note :amp amp :gate 1)))))

(defn pluck-string
  [string-index fret amp]
  (pluck-string-at string-index fret amp (now)))
;; (pluck-string 0 3 1.0)

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

;; ======================================================================
;; try out the guitar...
(strum-now :E)
(strum-now :E :up)
(strum-now :E :down 0.5)
(strum-now :E :up 0.5 0.25)

;; a little strumming pattern fun
;; http://www.youtube.com/watch?v=DV1ANPOYuH8
;; http://www.guitar.gg/strumming.html
(defn pat0 [metro cur-measure chord pattern]
  (let [cur-beat (* 4 cur-measure)]
    (doall
     (doseq [[b d] pattern]
       (strum-at (metro (+ b cur-beat)) chord d)))))
(defn dduud [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [1.5 :up] [2.5 :up]
          [3.0 :down] ]))
(defn dduudu [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [1.5 :up] [2.5 :up]
          [3.0 :down]
          [3.5 :up] ]))
(defn ddudu [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [2.5 :up] [3.0 :down] [3.5 :up] ]))
(defn ddduduud [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [2.0 :down] [2.25 :up] [2.5 :down] [2.75 :up]
          [3.25 :up] [3.5 :down]]))
(do ;; strumming practice
  (let [metro (metronome 100)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Gadd5 :Cadd9 :Cadd9
                                        :Dsus4 :Dsus4 :Gadd5 :Cadd9
                                        :Gadd5 :Cadd9])]
       (dduud metro i c))))
  )
(do ;; knocking on heaven's door
  (let [metro (metronome 100)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Cadd9 :Cadd9])]
       (dduudu metro i c))))
  )
(do ;; moar strumming practice
  (let [metro (metronome 180)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (ddudu metro i c))))
  )
(do ;; evin moar strumming practice
  (let [metro (metronome 90)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (ddduduud metro i c))))
  )

;; ======================================================================
;; FIXME -- this isn't quite working at the moment
;; now, add some distortion...
;; (def fxd (inst-fx! guitar-string-inst fx-distortion))
;; adjust a bit...
;; (ctl fxd :boost 10.5)
;; (ctl fxd :level 0.1)
;; (strum-now :E)



