(ns irso.piso)
(use 'overtone.core)
(connect-external-server 57110)
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
  (cond ;; pick one below...
    ;;        0    1    2    3    4    5    6    7    8    9
    false ([ 4.00 2.00 1.33 1.00 0.80 0.66 0.57 0.50 0.44 0.40] index)  ;; 1/f
    false  ([ 4.00 2.00 1.50 1.00 0.75 0.75 0.50 0.50 0.50 0.25] index)  ;; 16x range
    true ([ 4.00 2.00 1.50 1.00 0.75 0.75 0.50 0.50 0.50 0.50] index)  ;; 8x range
    false ([ 4.00 2.00 1.50 1.00 0.75 0.75 0.50 0.50 0.50 0.25] index))) ;; 4x range

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
          cur-beat (+ beat (:beat cur-snote))
          k-beat 1.6]
      ;;(println "note-on:" beat cur-beat cur-pitch cur-snote)
      (at (m cur-beat) (def pk (sampled-piano :note cur-pitch
                                              :level cur-level
                                              :attack cur-attack)))
      ;;(println "note-off:" (+ cur-beat (* k-beat cur-dur)))
      (at (m (+ cur-beat (* k-beat cur-dur))) (ctl pk :gate 0)))))

;; ======================================================================
;; the song
(defn ^:dynamic play-repeated-snote-seq
  [m beat tonic type snote-seq num-play-rests irno-seq]
  "given snote-seq and a count of play/rest pairs to derive from number sequence irno-seq,
   play that sequence repeatedly."
  (let [snote-seq-len (num-beats snote-seq)
        subset-irno-seq (take (* 2 num-play-rests) irno-seq)
        ;; 3 1 4 1 5 9 -> repeat-counts = 3 4 5, rest-counts = 1 1 9
        repeat-counts (take-nth 2 subset-irno-seq)
        rest-count-sums (conj (take-nth 2 (drop 1 (reductions + subset-irno-seq))) 0)
        ;; seq-indexes tell when to play the seq as multiple of seq-len
        ;; 0,1,2,[3],4,5,6,7,[8],9,10,11,12,13,[14...17],18
        seq-indexes (flatten (map #(map (fn [x] (+ %2 x)) %1)
                                  (map range repeat-counts)
                                  rest-count-sums))]
    ;;(println "snote-seq indexes & len" seq-indexes snote-seq-len)
    (last  ; return latest beat
     (for [cur-index seq-indexes]
       (let [cur-beat (+ beat (* cur-index snote-seq-len))]
         ;;(println "play" cur-index ":" cur-beat)
         (play-seq m cur-beat snote-seq)
         (+ cur-beat snote-seq-len))))))
  
(defn ^:dynamic piso [m beat tonic type]
  (do
    ;;(piso0 m beat tonic type)))
    (def seq1 (calc-seq tonic type 13 0 pi1000))
    ;;(play-seq m beat seq1)))
    (def seq2 (calc-seq tonic type 17 13 pi1000))
    ;;(play-seq m beat seq2)))
    (def seq2-start (+ beat (* 3 (num-beats seq1))))
    (def seq3 (calc-seq tonic type 19 (+ 13 17) pi1000))
    ;;(play-seq m beat seq3)))
    (def seq3-start (+ beat (* 5 (num-beats seq2))))
    (play-repeated-snote-seq m beat tonic type seq1 3 pi1000)
    (play-repeated-snote-seq m seq2-start tonic type seq2 2 pi1000)
    (play-repeated-snote-seq m seq3-start tonic type seq3 2 pi1000)))

;; ======================================================================
;; Add effects to create the proper mood

;;(def fx0 (inst-fx sampled-piano fx-freeverb))
;;(ctl fx0 :room-size 1.5)
;;(ctl fx0 :dampening 0.5)
;;(ctl fx0 :wet-dry   0.5) ;; dry = direct.  wet = reflections
;;
;; hmm, freeverb seems to resolve eventually to a "ringing" tone that
;; is distracting.

;; try just reverb...
(def fx1 (inst-fx sampled-piano fx-reverb))

;; using lowpass filter to remove "ringing" tone.  
(defsynth fx-lpf
  [bus 0 freq 20000]
  (let [src (in bus)]
    (replace-out bus (lpf src freq))))
(def fx2 (inst-fx sampled-piano fx-lpf))
(ctl fx2 :freq      2400)

;;(clear-fx sampled-piano)

;; ======================================================================
;; and now, we play...
(def metro (metronome 80))
(piso metro (metro) :c3 :pentatonic)
;;(stop)

;; debugging
;; for ^:dynamic, see http://stackoverflow.com/questions/8875353/why-im-getting-cant-dynamically-bind-non-dynamic-var
;; (use 'clojure.tools.trace)
;; (dotrace [piso play-repeated-snote-seq] (piso metro (metro) :c3 :pentatonic))

;; should also try 'panning' the cutoff frequencies, etc. during music
