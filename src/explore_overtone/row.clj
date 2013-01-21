(ns explore-overtone.row ; leipzig.example.row-row-row-your-boat
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [explore-overtone.sawbble :as eos]))

(defmethod play-note :leader
  [{:keys [pitch time duration]}]
  (let [synth-id (oss/ektara :note pitch :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :follower
  [{:keys [pitch time duration]}]
  (let [synth-id (eos/sawbble-synth :note (+ pitch 12)
                                    :lfo-freq 9.0
                                    :lpf-lfo-freq 3.0
                                    :amp 1.0
                                    :adsr-sustain-level 0.9
                                    :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :bass
  [{:keys [pitch time duration]}]
  (let [synth-id (oss/ektara :note (- pitch 12)
                             :amp 0.5
                             :distort 0.3
                             :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(def melody
               ; Row, row, row your boat,
  (->> (phrase [3/3 3/3 2/3 1/3 3/3]
               [  0   0   0   1   2])
    (then
               ; Gently down the stream,
       (phrase [2/3 1/3 2/3 1/3 6/3]
               [  2   1   2   3   4]))
    (then
               ; Merrily, merrily, merrily, merrily,
       (phrase (repeat 12 1/3) 
               (mapcat (partial repeat 3) [7 4 2 0])))
    (then
               ; Life is but a dream!
       (phrase [2/3 1/3 2/3 1/3 6/3] 
               [  4   3   2   1   0]))
    (where :part (is :leader))))

(def bass
  (->> (phrase [1  1 2]
               [0 -3 0])
     (where :part (is :bass))
     (times 4)))

(defn row-row [speed key]
  (->> melody
    (with bass)
    (times 2)
    (canon (comp (simple 4)
                 (partial where :part (is :follower))))
    (where :time speed)
    (where :duration speed)
    (where :pitch key)
    play))

(comment
  (row-row (bpm 120) (comp G sharp major))
  (row-row (bpm 120) (comp C minor))
  ;; below is only in my own mods
  (row-row (bpm 120) (comp G3 ♯ major))
  (row-row (bpm 90) (comp B4 ♭ minor))
)
