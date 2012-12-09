(ns explore-overtone.irrational-infinite-song)
(use 'overtone.live)
;;(use 'overtone.core)
;;(connect-external-server 57110)
;;(use 'overtone.inst.piano)
(use 'overtone.inst.sampled-piano) ;; requires 0.7.0. downloads 200MB

;; pi to 1000 digits
(def pi1000 '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4 3 3 8 3
  2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5 1 0 5 8 2 0 9 7 4 9 4 4 5
  9 2 3 0 7 8 1 6 4 0 6 2 8 6 2 0 8 9 9 8 6 2 8 0 3 4 8 2 5 3 4 2 1 1
  7 0 6 7 9 8 2 1 4 8 0 8 6 5 1 3 2 8 2 3 0 6 6 4 7 0 9 3 8 4 4 6 0 9
  5 5 0 5 8 2 2 3 1 7 2 5 3 5 9 4 0 8 1 2 8 4 8 1 1 1 7 4 5 0 2 8 4 1
  0 2 7 0 1 9 3 8 5 2 1 1 0 5 5 5 9 6 4 4 6 2 2 9 4 8 9 5 4 9 3 0 3 8
  1 9 6 4 4 2 8 8 1 0 9 7 5 6 6 5 9 3 3 4 4 6 1 2 8 4 7 5 6 4 8 2 3 3
  7 8 6 7 8 3 1 6 5 2 7 1 2 0 1 9 0 9 1 4 5 6 4 8 5 6 6 9 2 3 4 6 0 3
  4 8 6 1 0 4 5 4 3 2 6 6 4 8 2 1 3 3 9 3 6 0 7 2 6 0 2 4 9 1 4 1 2 7
  3 7 2 4 5 8 7 0 0 6 6 0 6 3 1 5 5 8 8 1 7 4 8 8 1 5 2 0 9 2 0 9 6 2
  8 2 9 2 5 4 0 9 1 7 1 5 3 6 4 3 6 7 8 9 2 5 9 0 3 6 0 0 1 1 3 3 0 5
  3 0 5 4 8 8 2 0 4 6 6 5 2 1 3 8 4 1 4 6 9 5 1 9 4 1 5 1 1 6 0 9 4 3
  3 0 5 7 2 7 0 3 6 5 7 5 9 5 9 1 9 5 3 0 9 2 1 8 6 1 1 7 3 8 1 9 3 2
  6 1 1 7 9 3 1 0 5 1 1 8 5 4 8 0 7 4 4 6 2 3 7 9 9 6 2 7 4 9 5 6 7 3
  5 1 8 8 5 7 5 2 7 2 4 8 9 1 2 2 7 9 3 8 1 8 3 0 1 1 9 4 9 1 2 9 8 3
  3 6 7 3 3 6 2 4 4 0 6 5 6 6 4 3 0 8 6 0 2 1 3 9 4 9 4 6 3 9 5 2 2 4
  7 3 7 1 9 0 7 0 2 1 7 9 8 6 0 9 4 3 7 0 2 7 7 0 5 3 9 2 1 7 1 7 6 2
  9 3 1 7 6 7 5 2 3 8 4 6 7 4 8 1 8 4 6 7 6 6 9 4 0 5 1 3 2 0 0 0 5 6
  8 1 2 7 1 4 5 2 6 3 5 6 0 8 2 7 7 8 5 7 7 1 3 4 2 7 5 7 7 8 9 6 0 9
  1 7 3 6 3 7 1 7 8 7 2 1 4 6 8 4 4 0 9 0 1 2 2 4 9 5 3 4 3 0 1 4 6 5
  4 9 5 8 5 3 7 1 0 5 0 7 9 2 2 7 9 6 8 9 2 5 8 9 2 3 5 4 2 0 1 9 9 5
  6 1 1 2 1 2 9 0 2 1 9 6 0 8 6 4 0 3 4 4 1 8 1 5 9 8 1 3 6 2 9 7 7 4
  7 7 1 3 0 9 9 6 0 5 1 8 7 0 7 2 1 1 3 4 9 9 9 9 9 9 8 3 7 2 9 7 8 0
  4 9 9 5 1 0 5 9 7 3 1 7 3 2 8 1 6 0 9 6 3 1 8 5 9 5 0 2 4 4 5 9 4 5
  5 3 4 6 9 0 8 3 0 2 6 4 2 5 2 2 3 0 8 2 5 3 3 4 4 6 8 5 0 3 5 2 6 1
  9 3 1 1 8 8 1 7 1 0 1 0 0 0 3 1 3 7 8 3 8 7 5 2 8 8 6 5 8 7 5 3 3 2
  0 8 3 8 1 4 2 0 6 1 7 1 7 7 6 6 9 1 4 7 3 0 3 5 9 8 2 5 3 4 9 0 4 2
  8 7 5 5 4 6 8 7 3 1 1 5 9 5 6 2 8 6 3 8 8 2 3 5 3 7 8 7 5 9 3 7 5 1
  9 5 7 7 8 1 8 5 7 7 8 0 5 3 2 1 7 1 2 2 6 8 0 6 6 1 3 0 0 1 9 2 7 8
  7 6 6 1 1 1 9 5 9 0 9 2 1 6 4 2 0 1 9 8 9))

;; A note on terminology...in this code a 'note' has pitch, duration &
;; velocity.  Overtone generally refers to this pitch as the note, but
;; the code here makes a distinction.
;;
;; inote = a note of indices, before they are translated to the proper
;; scale & otherwise made ready to play
;;
;; snote = a sequence note after translation & ready to play with an
;; associated beat to tell when it will be played.

;; (defn digits2inotes [digit-seq]
;;   "given a list of digits, make it into a list of index notes"
;;   (map #(hash-map :pitch-index %1 :velocity-index %2 :duration-index %3)
;;        (take-nth 3 digit-seq)
;;        (take-nth 3 (nthrest digit-seq 1))
;;        (take-nth 3 (nthrest digit-seq 2))))

;; change to play the first digits as pitches
(defn digits2inotes [digit-seq]
  "given a list of digits, make it into a list of index notes"
  (let [n (int (/ (count digit-seq) 3))]
    (map #(hash-map :pitch-index %1 :velocity-index %2 :duration-index %3)
         (take n digit-seq)
         (take n (drop n digit-seq))
         (take n (drop (* 2 n) digit-seq)))))

(defn index2pitch [tonic type index]
  "given a digit in range 0..9 find index in scale defined by
     tonic & type.  E.g. (index2pitch :c4 :major 1) -> 62"
  (nth (vec (scale tonic type (range 1 10))) (mod index 10)))

(defn index2velocity [index]
  "given a digit 'n' in range 0..9, find a velocity to play"
  (+ 80 (* 3 index)))

(defn index2duration [index]
  "given a digit 'n' in range 0..9, find a length in beats"
  ;; 1/f histogram of length
  ;;  0    1    2    3    4    5    6    7    8    9
  ([ 4.00 2.00 1.33 1.00 0.80 0.66 0.57 0.50 0.44 0.40] index))

(defn inote2snote [tonic type cur-inote]
  "given an index-note, create a sequence-note with a place for a beat."
  (hash-map
   :pitch (index2pitch tonic type (:pitch-index cur-inote))
   :velocity (index2velocity (:velocity-index cur-inote))
   :duration (index2duration (:duration-index cur-inote))
   :beat 0))

(defn duration2beat [cur-snote nxt-snote]
  "given 2 sequence notes, update the nxt beat"
  (hash-map
   :pitch (:pitch nxt-snote)
   :velocity (:velocity nxt-snote)
   :duration (:duration nxt-snote)
   :beat (+ (:duration cur-snote) (:beat cur-snote))))

(defn linear-map [x0 x1 y0 y1 x]
  "given x0 -> y0.  x1 -> y1.  x maps linearly to y"
  (let [dydx (/ (- y1 y0) (- x1 x0))
        dx (- x x0)]
    (+ y0 (* dydx dx))))
        
(defn velocity2attack [v]
  "sampled-piano uses attack & level, not velocity"
  (linear-map 0 127 0.2 0.05 v))

(defn velocity2level [v]
  "sampled-piano uses attack & level, not velocity"
  (linear-map 0 127 0.0 0.8 v))

(defn num-beats [snote-seq]
  "how long is a snote sequence? last duration + last beat"
  (let [last-snote (last snote-seq)]
    (+ (:beat last-snote) (:duration last-snote))))

(defn calc-seq [tonic type num-notes offset the-series]
  "calc some seq-notes in a certain key. doall to remove laziness. returns a list of
   (pitch velocity duration curbeat) values"
  (doall (reductions duration2beat
                     (map #(inote2snote tonic type %)
                          (take num-notes (nthrest (digits2inotes the-series) offset))))))

(defn play-seq [m beat snote-seq]
  "play a list of (pitch velocity duration curbeat) where snote-seq is offset by beat"
  (doseq [cur-snote snote-seq]
    (let [cur-pitch (:pitch cur-snote)
          cur-attack (velocity2attack (:velocity cur-snote))
          cur-level (velocity2level (:velocity cur-snote))
          cur-dur (:duration cur-snote)
          cur-beat (+ beat (:beat cur-snote))]
      ;;(println "note-on:" cur-beat cur-pitch )
      (at (m cur-beat) (def pk (sampled-piano :note cur-pitch
                                              :level cur-level
                                              :attack cur-attack)))
      ;;(println "note-off:" (+ cur-beat (* 0.9 cur-dur)))
      (at (m (+ cur-beat (* 1.6 cur-dur))) (ctl pk :gate 0)))))

;; (play-seq
;;  (metronome 120)
;;  0
;;  '({:pitch 60 :velocity 100 :duration 1 :beat 0}
;;    {:pitch 62 :velocity 100 :duration 1 :beat 1}
;;    {:pitch 65 :velocity 100 :duration 1 :beat 2}
;;    ))

(defn infinite-song [m beat tonic type]
  (println "infinite song" beat tonic type)
  (def seq1 (calc-seq tonic type
                     32
                     (mod beat (- (/ (count pi1000) 3) 32))
                     pi1000))
  (def seq-len (num-beats seq1))
  (def seq1-start (+ (m) 17))
  (def seq2-start (+ (m) seq-len 17))
  (play-seq m (m) seq1)
  (play-seq m seq1-start seq1)  
  (apply-at (m seq2-start) #'infinite-song m seq2-start tonic type []))
  
;; and now, we play...
(def metro (metronome 90))
(def pfx (inst-fx sampled-piano fx-freeverb))
;;(def lfx (inst-fx fx-freeverb fx-rlpf))
(def lfx (inst-fx sampled-piano fx-rlpf))
(do
  (ctl pfx :room-size 100)
  (ctl pfx :dampening 0.1)
  (ctl pfx :wet-dry   0.9) ;; dry = direct.  wet = reflections
  (ctl lfx :cutoff 1000)
  (ctl lfx :res 0.9)
  )
(infinite-song metro (metro) :c3 :pentatonic)
;;(stop)
;;(clear-fx sampled-piano)

;; need to do low-pass filter on the fx-freeverb output.
;; need to get rid of high-pitched ringing

;; should also try 'panning' the cutoff frequencies, etc. during music
