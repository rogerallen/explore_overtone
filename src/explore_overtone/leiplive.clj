(ns explore-overtone.leiplive
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [explore-overtone.midipe :as mp]))

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
  (->> (nth (mp/partition-by-timestamp) n)
       (for-leipzig)
       (where :part (is :leader))))

(defn round
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

(defn get-quant-melody [n the-bpm]
  (->> (nth (mp/partition-by-timestamp) n)
       (for-leipzig)
       (quantize-notes the-bpm 0.06125)
       (where :time (bpm the-bpm))
       (where :duration (bpm the-bpm))
       (where :part (is :leader))))

(comment
  (mp/new)
  (def mpp (o/midi-poly-player (partial oss/ektara :gate 1)))
  (count (mp/partition-by-timestamp))
  (get-melody 4)
  (play (click-track (bpm 92) 4))
  (play (->>
         (get-quant-melody 3 92)
         (times 2)))
  (play (->>
         (get-melody 4)
         (times 2)))
  (play (->>
         (concat (get-melody 4)
                 (get-melody 2))
         (sort-by :time)))

  (map #(* (/ 105 60.0) ; 105 bpm -> 1.75 bps
           (/ (:time %) 1000.0)) ; time in s
       (get-melody 4))

  (map #(:duration %) (get-quant-melody 3 92))
  
  (mp/save)
  (o/stop)
)
