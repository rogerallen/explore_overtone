;; base routines for stringed instruments
(ns explore_overtone.stringed_player
  (:use [overtone.music pitch time]
        [overtone.studio inst]
        [overtone.sc envelope node server ugens]
        [overtone.sc.cgens mix]))

(defn- fret-to-note
  "given a fret-offset, add to the base note index with special
  handling for -1"
  [base-note offset]
  (if (>= offset 0)
    (+ base-note offset)
    offset))

(defn- mkarg
  "useful for making arguments for the instruments strings"
  [s i]
  (keyword (format "%s-%d" s i)))

;; ======================================================================
;; Main helper functions.  See guitar for how to use pick or strum to
;; play the instrument.
(defn pick-string
  "pick the instrument's string depending on the fret selected.  A
   fret value less than -1 will cause no event; -1 or greater causes
   the previous note to be silenced; 0 or greater will also cause a
   new note event."
  ([the-strings the-inst string-index fret t]
     (let [the-note (fret-to-note (nth the-strings string-index) fret)] 
       ;; turn off the previous note
       (if (>= the-note -1)
         (at t (ctl the-inst (mkarg "gate" string-index) 0)))
       ;; NOTE: there needs to be some time between these
       ;; FIXME: +50 seems conservative.  Find minimum.
       (if (>= the-note 0)
         (at (+ t 50) (ctl the-inst
                           (mkarg "note" string-index) the-note
                           (mkarg "gate" string-index) 1)))))
  ([the-chord-frets the-inst string-index fret]
     (pick-string the-chord-frets the-inst string-index fret (now))))

;; ======================================================================
(defn strum-strings
  "strum a chord on the instrument in a direction (:up or :down) with
   a strum duration of strum-time at t.  If the-chord is a vector, use
   it directly for fret indexes.  Code gets a bit complicated to deal
   with the case where strings are muted and don't count towards the
   strum-time."
  ([chord-fret-map the-strings the-inst the-chord direction strum-time t]
     (let [num-strings (count (chord-fret-map :A))
           ;; [-1 3 2 0 1 0]
           chord-frets (if (vector? the-chord)
                         ;; FIXME -- assert len(the-chord) is right?
                         the-chord ; treat the chord as a series of frets
                         (chord-fret-map the-chord))
           ;; account for unplayed strings for delta time calc
           ;; (0 0 1 2 3 4)
           fret-times (map first
                           (rest (reductions
                                  #(vector (if (>= (second %1) 0)
                                             (inc (first %1))
                                             (first %1))
                                           %2)
                                  [0 -1]
                                  chord-frets)))]
       (dotimes [i num-strings]
         (let [j (if (= direction :up) (- num-strings 1 i) i)
               max-t (apply max fret-times)
               dt (if (> max-t 0)
                    (* 1000 (/ strum-time max-t))
                    0)
               fret-delta (if (= direction :up)
                            (- max-t (nth fret-times i))
                            (nth fret-times i))]
           (pick-string the-strings the-inst j (nth chord-frets j) (+ t (* fret-delta dt)))))))
  ([chord-fret-map the-strings the-inst the-chord direction strum-time]
     (strum-strings chord-fret-map the-strings the-inst the-chord direction strum-time (now)))
  ([chord-fret-map the-strings the-inst the-chord direction]
     (strum-strings chord-fret-map the-strings the-inst the-chord direction 0.05 (now)))
  ([chord-fret-map the-strings the-inst the-chord]
     (strum-strings chord-fret-map the-strings the-inst the-chord :down 0.05 (now))))

