(ns explore_overtone.irrational_infinite_song)
(use 'overtone.core)
(connect-external-server 57110)
(use 'overtone.inst.piano)

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

;; use this to get a tuple (note velocity duration) of indexes
(defn s3v [s]
  "given a list s, make it into a list of 3-vectors"
  (map vector
       (doall (take-nth 3 s))
       (doall (take-nth 3 (nthrest s 1)))
       (doall (take-nth 3 (nthrest s 2)))))

(defn index2note [tonic type n]
  "given a digit 'n' in range 0..9 find index in scale defined by
     tonic & type.  E.g. (index2note :c4 :major 1) -> 62"
  (nth (vec (scale tonic type (range 1 10))) (mod n 10)))

(defn index2velocity [n]
  "given a digit 'n' in range 0..9, find a velocity to play"
  (+ 80 (* 3 n)))

(defn index2duration [n]
  "given a digit 'n' in range 0..9, find a length in beats"
  ;; 1/f histogram of length
  ;;  0    1    2    3    4    5    6    7    8    9
  ([ 4.00 2.00 1.33 1.00 0.80 0.66 0.57 0.50 0.44 0.40] n))

(defn iii2nvd0 [tonic type cur-3v]
  "given (n v d) indexes, fix (n v d 0) ready to play"
  (let [ret-3v-n (index2note tonic type (nth cur-3v 0))
        ret-3v-v (index2velocity (nth cur-3v 1))
        ret-3v-d (index2duration (nth cur-3v 2))]
    (list ret-3v-n ret-3v-v ret-3v-d 0)))

(defn duration2beat [cur-4v nxt-4v]
  "given 2 (n v d 0) tuples, sum the duration to give a beat for the nxt one"
  (let [ret-4v-n (nth nxt-4v 0)
        ret-4v-v (nth nxt-4v 1)
        ret-4v-d (nth nxt-4v 2)
        ret-4v-b (+ (nth cur-4v 2) (nth cur-4v 3))]
    (list ret-4v-n ret-4v-v ret-4v-d ret-4v-b)))

(defn num-beats [seq]
  "how long is a sequence? last duration + last beat"
  (let [last-4v (nth seq (dec (count seq)))]
    (+ (nth last-4v 2) (nth last-4v 3))))

(defn calc-seq [tonic type num-notes offset the-series]
  "calc some notes in a certain key. doall to remove laziness. returns a list of
   (note velocity duration curbeat) values"
  (doall (reductions duration2beat
                     (map #(iii2nvd0 tonic type %)
                          (take num-notes (nthrest (s3v the-series) offset))))))

(defn play-seq [m beat seq]
  "play a list of (note velocity duration curbeat) where seq is offset by beat"
  (doseq [cur-4v seq]
    (let [cur-note (nth cur-4v 0)
          cur-vel (nth cur-4v 1)
          cur-dur (nth cur-4v 2)
          cur-beat (+ beat (nth cur-4v 3))]
      ;;(println "play:" (+ beat cur-beat) cur-note cur-vel)
      (at (m cur-beat) (piano cur-note 1 cur-vel)))))

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
(def metro (metronome 120))
(infinite-song metro (metro) :e3 :pentatonic)
;;(stop) when you must.
