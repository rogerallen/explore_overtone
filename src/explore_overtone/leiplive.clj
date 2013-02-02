(ns explore-overtone.leiplive
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [explore-overtone.midi-persi :as mp]))

(defmethod play-note :leader
  [{:keys [pitch time duration]}]
  (let [synth-id (oss/ektara :note pitch :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defn add-durations
  "add durations to each note-on"
  [xs]
  (let [first-event (first xs)
        the-rest (rest xs)]
    (if (= (:command first-event) :note-on)
      (let [offs (filter #(= (:command %) :note-off) xs)
            note-to-find (:note first-event)
            off-event (first (drop-while #(not= (:note %) note-to-find) offs))
            duration (- (:timestamp off-event) (:timestamp first-event))]
        (cons (assoc first-event :duration duration) (add-durations the-rest)))
      (if (not (empty? the-rest))
        (cons first-event (add-durations (rest xs)))))))

;;(swank.core/break)
(defn for-leipzig
  "convert events from midipe to something leipzig can use."
  [raw-notes]
  (let [notes (add-durations raw-notes)
        first-timestamp (:timestamp (first notes))]
    (->> notes
         (filter #(= (:command %) :note-on))
         (map #(do
                 (assoc {}
                   :time     (/ (- (:timestamp %) first-timestamp) 1000.0)
                   :duration (/ (:duration %) 1000.0)
                   :pitch    (:note %)))))))

(defn get-melody [n]
  (->> (nth (mp/partition-by-timestamp (mp/get-list)) n)
       (for-leipzig)
       (where :part (is :leader))))

(defn round ;; FIXME clojure.math.numeric-tower?
  "round to nearest integer since int truncates."
  [x]
  (int (+ x 0.5)))

(defn quantize
  [bpm quanta t]
  (let [bps (/ bpm 60.0)]
    ;;(swank.core/break)
    (* quanta (round (/ (* bps t) quanta)))))

(defn quantize-notes
  "quantize a time to the nearest quanta of a beat."
  [bpm quanta notes]
  (map #(assoc %
          :time (quantize bpm quanta (/ (:time %) 1000.0))
          ;; don't let duration = 0
          :duration (max quanta (quantize bpm quanta (/ (:duration %) 1000.0))))
         notes))
;; (quantize-notes 60.0 0.5 [{:time 0 :duration 1000} {:time 1005 :duration 1495} {:time 2490 :duration 990}])
;; -> ({:duration 1.0, :time 0.0} {:duration 1.5, :time 1.0} {:duration 1.0, :time 2.5})

(defn click-track
  [speed measures]
  (->> (phrase [1 1 1 1] [60 60 60 60])
       (times measures)
       (where :time speed)
       (where :duration speed)
       (where :part (is :leader))))

(defn get-quant-melody [n the-bpm the-quanta]
  (->> (nth (mp/partition-by-timestamp (mp/get-list)) n)
       (for-leipzig)
       (quantize-notes the-bpm the-quanta)
       (where :part (is :leader))))

;; FIXME -- this is only a start.  Breaks down if there are rests
(defn print-phrase [xs]
  (println "(phrase" (apply vector (map :duration xs)))
  (println "       " (apply vector (map :pitch xs)) ")"))

(defn- from [base] (partial + base))
(defn- unfrom [base] (partial + (- base)))
;; ((unfrom 60) 63) -> 3
(defmacro defs {:private true} [names values]
  `(do ~@(map
     (fn [name value] `(def ~name ~value))
     names (eval values))))
(defs [unC unD unE unF unG unA unB]
  (map
    (comp unfrom (from 60) major)
    (range)))
;; (unC) -> -60
(defn sum-n [series n] (apply + (take n series)))
;; given intervals and sum-of-intervals, return the interval that produces that sum
;; 
(defn unscale-of ;;:natural?
  [intervals degree-sum]
  (count (take-while #(<= % degree-sum) (reductions + (cycle intervals)))))
(defn unscale [intervals] (partial unscale-of intervals))
(def unmajor (unscale [2 2 1 2 2 2 1]))
;;(unmajor 9) -> 5
;;(major 5) -> 9
;;((comp unmajor unC) 69) -> 5

(comment
  (mp/init!)  ;; only one time
  (mp/new!)   ;; each time you want to put work in a new file
  (mp/save!)  ;; to save work away
  (mp/record) ;; start recording midi events
  (mp/pause)  ;; pause recording midi events

  ;; listen to midi
  (def mpp (o/midi-poly-player (partial oss/ektara :gate 1)))

  ;; how many 'snippets' do you have?
  (count (mp/partition-by-timestamp (mp/get-list)))

  ;; play the first one
  (play (->>
         (get-melody 22)
         (times 2)))
  
  ;; play the first one, quantized at 92 bpm, played back faster 
  (play (->>
         (get-quant-melody 10 80 0.5)
         (times 2)
         (canon (comp (simple 8) (interval 7)))
         (where :time (bpm 180))
         (where :duration (bpm 180))))

  ;; transpose this
  (play (->>
         (get-quant-melody 22 80 0.25)
         ;; change from C major to intervals
         (where :pitch (comp unmajor unC))
         ;;(times 2)
         ;;(canon (comp (simple 8) (interval 7)))
         (where :pitch (comp D phrygian))
         (where :time (bpm 180))
         (where :duration (bpm 180))))

  (print-phrase (get-quant-melody 10 80 0.5))

  (def m1 (phrase [3 1 4  3 1 4  3 0.5 0.5  3 0.5 0.5  4.0]
                  [48 52 55 53 60 55 52 53 52 50 52 50 48] ))
  (play (->> m1
         (times 2)
         (canon (comp (simple 8) (interval 5)))
         (where :part (is :leader))
         (where :time (bpm 180))
         (where :duration (bpm 180))))
  
  (play (click-track (bpm 80) 8))
  (play (->>
         (get-quant-melody 3 92)
         (times 2)))

  ;; leipzig
  (C)
  ((comp C major) 22)
  (sharp 5)
)
