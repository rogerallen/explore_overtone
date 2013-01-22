(ns explore-overtone.randcanon
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [clojure.pprint :as cpp]))

(defmethod play-note :leader
  [{:keys [pitch time duration]}]
  (let [synth-id (oss/ektara :note pitch :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :follower
  [{:keys [pitch time duration]}]
  (let [synth-id (oss/ektara :note (- pitch 12) :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defn run [[from & tos]]
  (if-let [to (first tos)]
    (let [up-or-down (if (<= from to)
                       (range from to)
                       (reverse (range (inc to) (inc from))))]
      (concat up-or-down (run tos)))
    [from]))

(def repeats (partial mapcat #(apply repeat %)))
(def runs (partial mapcat run))

(defn rand-beat-seq
  [num-notes num-beats min-beat]
  (if (> num-notes 1)
    (let [num-notes-rem (dec num-notes)
          min-beats-to-save (* num-notes-rem min-beat)
          cur-beat (min (- num-beats min-beats-to-save)
                        (/ (+ 1 (rand-int 4)) 4))
          num-beats-left (- num-beats cur-beat)]
      (if (= num-beats-left 0)
        [cur-beat]
        (concat [cur-beat]
                (rand-beat-seq num-notes-rem
                               num-beats-left
                               min-beat))))
    [num-beats]))
;; (rand-beat-seq 4 4 1/2)

(defn rand-pitch-seq
  [num-notes]
  (let [n (rand-int 2)]
    (cond
     (= n 0) (run [0 num-notes])
     (= n 1) (run [num-notes 0])
     )))

(defn rand-phrase
  [num-beats]
  (let [num-notes (+ 4 (rand-int 8))]
    (phrase (rand-beat-seq num-notes num-beats 1/4)
            (rand-pitch-seq num-notes))))

(defn get-new-melody [] ;; make a 12-beat random melody
  (->>
   (rand-phrase 4)
   (then (rand-phrase 4))
   (then (rand-phrase 4))
   (where :part (is :leader))))

;;(def melody (get-new-melody))

(defn canon-notes [speed key melody]
  (let [last-note (last melody)
        melody-len (+ (:duration last-note)
                      (:time last-note))
        canon-offset (- (* 2 melody-len)
                        (:duration last-note))]
    (->> melody
         (times 2) 
         (canon
          (comp (simple (+ 4 canon-offset))
                crab
                (partial where :part (is :follower))))
         ;; must sort the notes before playing
       (sort-by :time))))

(defn play-canon-notes [speed key melody]
  (->> (canon-notes speed key melody)
       (where :time speed)
       (where :duration speed)
       (where :pitch key)
       play))

(defn print-canon-notes [speed key melody]
  (->> (canon-notes speed key melody)
       cpp/pprint))

(comment
  (def melody (get-new-melody))
  (print-canon-notes (bpm 120) (comp D major) melody)
  (play-canon-notes (bpm 120) (comp D minor) melody)
  )

