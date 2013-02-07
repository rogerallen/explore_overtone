(ns explore-overtone.leipzig-poly-player
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.inst.sampled-piano :as p]))

;; "un-scale" -- the inverse translations to go from raw pitches 60,
;; 62, etc.  to scale indices 0, 1, etc.  Then, you can "re-scale" in
;; leipzig to get into any scale you would like.  (some of this
;; duplicates private code in lepizig.scale.)
(defmacro defs {:private true} [names values]
  `(do ~@(map
     (fn [name value] `(def ~name ~value))
     names (eval values))))
(defn- from [base] (partial + base))
(defn- sum-n [series n] (apply + (take n series)))

(defn- unfrom [base] (partial + (- base)))
(defs [unC unD unE unF unG unA unB]
  (map
    (comp unfrom (from 60) major)
    (range)))
(defmulti unscale-of
  (fn [intervals degree-sum]
    (cond 
      ;; FIXME? (not= degree (floor degree)) :fraction
      (neg? degree-sum)            :negative
      :otherwise                   :natural)))
(defmethod unscale-of :natural [intervals degree-sum]
  (count (take-while #(<= % degree-sum) (reductions + (cycle intervals)))))
(defmethod unscale-of :negative [intervals degree-sum]
  (- (count (take-while #(<= % (- degree-sum)) (reductions + (cycle (reverse intervals)))))))
(defn unscale [intervals] (partial unscale-of intervals))

(def unMajor (unscale [2 2 1 2 2 2 1]))
(defs
  [unIonian unDorian unPhrygian unLydian unMixolydian unAeolian unLocrian]
  (map (partial mode unMajor) (range)))
(def unMinor unAeolian)

;;(major 5) -> 9
;;(unMajor 9) -> 5
;;(major -4) -> -7
;;(unMajor -7) -> -4
;;((comp unMajor unC) 50) -> 5

;; let's take a live midi input and rescale the white keys on the piano
;; into something you wouldn't normally expect...
(def mpp (o/midi-poly-player
          (fn [& {:keys [note velocity]}]
            ;; transpose the C major scale (white keys)
            (let [pitch-index ((comp unMajor unC) note)
                  ;; into D mixolydian
                  new-note ((comp D lydian) pitch-index)]
              (p/sampled-piano :note new-note
                               :velocity velocity)))))
;; (o/midi-player-stop)
