;; A Guitar by Roger Allen.
(ns explore_overtone.guitar
  (:use [overtone.music pitch time]
        [overtone.studio inst]
        [overtone.sc envelope node server ugens]
        [overtone.sc.cgens mix]))

;; a map of chords to frets.
;; -1 indicates you don't play that string
(def guitar-chord-frets
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

   :C9  [  3 -1  2  3  3  3 ]
   :F9  [ -1  3 -1  2  4  3 ]
   :G9  [ -1 -1  0  2  0  1 ]
   :D9  [ -1 -1  4  2  1  0 ]
   :A9  [  0  0  2  4  2  3 ]
   :E9  [  0  2  0  1  3  2 ]
   :Bb9 [ -1 -1  0  1  1  1 ]
   :Eb9 [ -1 -1  1  0  2  1 ]
   :Ab9 [ -1 -1  1  3  1  2 ]
   :Db9 [  4 -1  3  4  4  4 ]
   :Gb9 [ -1  4 -1  3  5  4 ]
   :B9  [  2 -1  1  2  2  2 ]

   :Gadd5 [  3  2  0  0  3  3 ]
   :Cadd9 [ -1  3  2  0  3  3 ]
   :Dsus4 [ -1 -1  0  2  3  3 ]
   })

;; ======================================================================
;; an array of 6 guitar strings: EADGBE
(def guitar-string-notes [(note :e2) (note :a2) (note :d3)
                          (note :g3) (note :b3) (note :e4)])

(defn- fret-to-note
  "given a fret-offset, add to the base note index with special
  handling for -1"
  [base-note offset]
  (if (>= offset 0)
    (+ base-note offset)
    offset))

(defn- mkarg
  "useful for making arguments for the guitar strings"
  [s i]
  (keyword (format "%s-%d" s i)))

;; ======================================================================
;; Main helper functions.  Use pick or strum to play the guitar instrument.
(defn pick
  "pick the guitar instrument's string depending on the fret
   selected.  A fret value less than -1 will cause no event; -1 or
   greater causes the previous note to be silenced; 0 or greater will
   also cause a new note event."
  ([the-guitar string-index fret t]
     (let [the-note (fret-to-note (nth guitar-string-notes string-index) fret)] 
       ;; turn off the previous note
       (if (>= the-note -1)
         (at t (ctl the-guitar (mkarg "gate" string-index) 0)))
       ;; NOTE: there needs to be some time between these
       ;; FIXME: +50 seems conservative.  Find minimum.
       (if (>= the-note 0)
         (at (+ t 50) (ctl the-guitar
                           (mkarg "note" string-index) the-note
                           (mkarg "gate" string-index) 1)))))
  ([the-guitar string-index fret]
     (pick the-guitar string-index fret (now))))

;; ======================================================================
(defn strum
  "strum a chord on the guitar instrument in a direction (:up
   or :down) with a strum duration of strum-time at t."
  ([the-guitar the-chord direction strum-time t]
     ;; FIXME - deal with -1 fret offsets--shouldn't count as part of
     ;; the delta time.
     (let [dt (* 1000 (/ strum-time 5))
           chord-frets (if (vector? the-chord)
                         the-chord ; treat the chord as a series of frets
                         (guitar-chord-frets the-chord))]
       (dotimes [i 6]
         (let [j (if (= direction :up) (- 5 i) i)]
           (pick the-guitar j (chord-frets j) (+ t (* i dt)))))))
  ([the-guitar the-chord direction strum-time]
     (strum the-guitar the-chord direction strum-time (now)))
  ([the-guitar the-chord direction]
     (strum the-guitar the-chord direction 0.05 (now)))
  ([the-guitar the-chord]
     (strum the-guitar the-chord :down 0.05 (now))))

;; ======================================================================
;; The guitar instrument.
;; Note: the strings need to be silenced with a gate -> 0 transition
;; before a gate -> 1 transition activates it.  Testing showed it
;; needed > 25 ms between these transitions to be effective.
;; Use the strum and pick helpers to play the instrument.
(definst guitar [note-0 {:default 60 :min 0 :max 127} gate-0 {:default 0}
                 note-1 {:default 60 :min 0 :max 127} gate-1 {:default 0}
                 note-2 {:default 60 :min 0 :max 127} gate-2 {:default 0}
                 note-3 {:default 60 :min 0 :max 127} gate-3 {:default 0}
                 note-4 {:default 60 :min 0 :max 127} gate-4 {:default 0}
                 note-5 {:default 60 :min 0 :max 127} gate-5 {:default 0}
                 dur       {:default 10.0}
                 decay     {:default 30} ;; pluck decay
                 coef      {:default 0.3 :min -1 :max 1} ;; pluck coef
                 noise-amp {:default 0.8 :min 0.0 :max 1.0}
                 pre-amp   {:default 1.0}
                 amp       {:default 1.0}
                 distort   {:default 0.0 :min 0.0 :max 0.9999999999}]
  (let [strings (map #(let [frq  (midicps (first %))
                            nze  (* noise-amp (pink-noise))
                            plk  (pluck nze
                                        (second %)
                                        (/ 1.0 8.0)
                                        (/ 1.0 frq)
                                        decay
                                        coef)]
                        (leak-dc (* plk (env-gen (asr 0.0001 1 0.1)
                                                 :gate (second %)))
                                 0.995))
                     [[note-0 gate-0] [note-1 gate-1] [note-2 gate-2]
                      [note-3 gate-3] [note-4 gate-4] [note-5 gate-5]])
        src (* pre-amp (mix strings))
        ;; distortion from fx-distortion2 
        k   (/ (* 2 distort) (- 1 distort))
        dis (/ (* src (+ 1 k)) (+ 1 (* k (abs src))))]
    (* amp dis)))

