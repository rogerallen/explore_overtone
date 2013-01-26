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

(defn unbias [orig] (partial + (- orig)))

(defn add-durations [xs]
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

(defn get-melody [n]
  (let [notes (add-durations (nth (mp/partition-by-timestamp) n))
        first-timestamp (:timestamp (first notes))]
    (->> notes
         (filter #(= (:command %) :note-on))
         (where :timestamp (unbias first-timestamp))
         (map #(assoc % :time (/ (:timestamp %) 1000.0)))
         (map #(assoc % :duration (/ (:duration %) 1000.0)))
         (map #(assoc % :pitch (:note %)))
         (where :part (is :leader)))))

(comment
  (mp/new)
  (def mpp (o/midi-poly-player (partial oss/ektara :gate 1)))
  (count (mp/partition-by-timestamp))
  (play (get-melody 5))
  
  (mp/save)
  (o/stop)
)
