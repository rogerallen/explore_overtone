(ns explore-overtone.randcanon
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [oversampler.piano.inst :as op]
            [clojure.pprint :as cpp]))

(defmethod play-note :leader
  [{:keys [pitch time duration]}]
  (let [synth-id (op/sampled-piano :note pitch :level 0.4 :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :canon1
  [{:keys [pitch time duration]}]
  (let [synth-id (oss/ektara :note (- pitch 12) :amp 0.35 :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :canon2
  [{:keys [pitch time duration]}]
  (let [synth-id (op/sampled-piano :note pitch :level 0.4 :gate 1)]
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

(defn heads? []
  (= 0 (rand-int 2)))

(defn rotate-left [xs n]
  (let [cs (count xs)]
    (assert (>= n 0))
    (take cs (drop n (flatten (repeat xs))))))
(defn rotate-right [xs n]
  (let [cs (count xs)]
    (rotate-left xs (- cs n))))
;; (rotate-left [1 2 3 4 5] 1)
;; (rotate-right [1 2 3 4 5] 4)

(defn rand-beat-seq1
  [num-notes num-beats min-beat]
  ;;(println "rand-beat-seq" num-notes num-beats min-beat)
  (assert (>= (/ num-beats min-beat) num-notes))
  (if (> num-notes 1)
    (let [num-notes-rem (dec num-notes)
          min-beats-to-save (* num-notes-rem min-beat)
          max-beats-to-take (- num-beats min-beats-to-save)
          max-beatlets (int (/ max-beats-to-take min-beat))
          cur-beatlet (+ 1 (rand-int max-beatlets))
          cur-beat (* cur-beatlet min-beat)
          ;;cur-beat (min (- num-beats min-beats-to-save)
          ;;              (/ (+ 1 (rand-int 8)) 4))
          num-beats-left (- num-beats cur-beat)]
      (if (= num-beats-left 0)
        [cur-beat]
        (concat [cur-beat]
                (rand-beat-seq1 num-notes-rem
                                num-beats-left
                                min-beat))))
    [num-beats]))

(defn rand-beat-seq
  [num-notes num-beats min-beat]
  (let [r (rand-int num-notes)
        the-seq (rand-beat-seq1 num-notes num-beats min-beat) 
        final-seq (if (heads?)
                    (reverse the-seq)
                    the-seq)]
    (rotate-left final-seq r)))
;; (rand-beat-seq 9 4 1/2)
;; (assert (= 8 (count (rand-beat-seq 8 4 1/4))))
;; (assert (= 4 (reduce + (rand-beat-seq 4 4 1/4))))

(defn rand-pitch-seq
  [num-notes]
  (let [n (rand-int 2)]
    (cond
     (= n 0) (run [0 num-notes])
     (= n 1) (run [num-notes 0])
     )))

(defn rand-phrase
  [num-beats]
  (let [num-notes (+ 4 (rand-int 5))]
    (phrase (rand-beat-seq num-notes num-beats 1/2)
            (rand-pitch-seq num-notes))))

(defn get-new-melody [] ;; make a 12-beat random melody
  (->>
   (rand-phrase 4)
   (then (rand-phrase 4))
   (then (rand-phrase 4))
   (where :part (is :leader))))

(defn canon-notes [speed key melody]
  (let [last-note (last melody)
        melody-len (+ (:duration last-note)
                      (:time last-note))
        canon-offset (- (* 2 melody-len)
                        (:duration last-note))]
    ;; must sort the notes before playing
    (->>
     (concat
      (->> melody
           (times 2)
           (canon (comp
                   (simple (+ 4 canon-offset))
                   crab
                   (partial where :part (is :canon1)))))
      (->> melody
           (times 2)
           (canon (comp
                   (simple 8) ;; (+ 8 canon-offset))
                   mirror
                   (partial where :part (is :canon2)))))
      )
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
  (play-canon-notes (bpm 120) (comp D major) melody)
  (cpp/pprint melody)
  (print-canon-notes (bpm 120) (comp D major) melody)

  (def melody
    ({:part :leader, :duration 1N, :pitch 0, :time 0}
     {:part :leader, :duration 1/2, :pitch 1, :time 1N}
     {:part :leader, :duration 1/2, :pitch 2, :time 3/2}
     {:part :leader, :duration 1/2, :pitch 3, :time 2N}
     {:part :leader, :duration 3/2, :pitch 4, :time 5/2}
     {:part :leader, :duration 1/2, :pitch 5, :time 4N}
     {:part :leader, :duration 1/2, :pitch 4, :time 9/2}
     {:part :leader, :duration 1N, :pitch 3, :time 5N}
     {:part :leader, :duration 3/2, :pitch 2, :time 6N}
     {:part :leader, :duration 1/2, :pitch 1, :time 15/2}
     {:part :leader, :duration 1/2, :pitch 0, :time 8N}
     {:part :leader, :duration 1/2, :pitch 1, :time 17/2}
     {:part :leader, :duration 1/2, :pitch 2, :time 9N}
     {:part :leader, :duration 1/2, :pitch 3, :time 19/2}
     {:part :leader, :duration 1/2, :pitch 4, :time 10N}
     {:part :leader, :duration 1/2, :pitch 5, :time 21/2}
     {:part :leader, :duration 1/2, :pitch 6, :time 11N}
     {:part :leader, :duration 1/2, :pitch 7, :time 23/2})
    )
  )

