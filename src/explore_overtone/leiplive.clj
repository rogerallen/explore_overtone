(ns explore-overtone.leiplive
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon)
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [overtone.inst.synth :as osynth]
            [overtone.sc.sample :as osamp]
            [overtone.libs.freesound :as ofree]
            [explore-overtone.midi-persi :as mp]
            [oversampler.piano.inst :as piano]
            [oversampler.cello.inst :as cello]))

(defmethod play-note :piano
  [{:keys [pitch time duration]}]
  (let [synth-id (piano/sampled-piano :note pitch :gate 1 :play-buf-action o/NO-ACTION)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :cello
  [{:keys [pitch time duration]}]
  (let [synth-id (cello/sampled-cello :note (- pitch 12) :gate 1 :play-buf-action o/NO-ACTION)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :ektara
  [{:keys [pitch time duration]}]
  (let [synth-id (oss/ektara :note pitch :gate 1)]
    (o/at (+ time duration) (o/ctl synth-id :gate 0))))

(defmethod play-note :overpad
  [{:keys [pitch time duration]}]
  (osynth/overpad :note pitch))

(def snap (osamp/sample (ofree/freesound-path 87731)))
(defmethod play-note :click
  [{:keys [time]}]
  (snap))

;; ======================================================================
(defn click-track
  [speed measures]
  (->> (phrase [1 1 1 1] [60 60 60 60])
       (times measures)
       (where :time speed)
       (where :duration speed)
       (where :part (is :click))))

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

(defn get-melody [n inst-name]
  (->> (nth (mp/partition-by-timestamp (mp/get-list)) n)
       (for-leipzig)
       (where :part (is inst-name))))

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

(defn get-quant-melody [n the-bpm the-quanta inst-name]
  (->> (nth (mp/partition-by-timestamp (mp/get-list)) n)
       (for-leipzig)
       (quantize-notes the-bpm the-quanta)
       (where :part (is inst-name))))

;; FIXME -- this is only a start.  Breaks down if there are rests
(defn print-phrase [xs]
  (println "(phrase" (apply vector (map :duration xs)))
  (println "       " (apply vector (map :pitch xs)) ")"))

;; ======================================================================
;; alright, here are the inverse translations to go from raw pitches to scale indices
(defn- from [base] (partial + base))
(defn- unfrom [base] (partial + (- base)))

(defmacro defs {:private true} [names values]
  `(do ~@(map
     (fn [name value] `(def ~name ~value))
     names (eval values))))

(defs [unC unD unE unF unG unA unB]
  (map
    (comp unfrom (from 60) major)
    (range)))

(defn sum-n [series n] (apply + (take n series)))
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

;;(unMajor 9) -> 5
;;(major 5) -> 9
;;(major -4) -> -7
;;(unMajor -7) -> -4
;;((comp unMajor unC) 50) -> 5
;; ======================================================================

(comment
  (mp/init!)  ;; only one time
  (mp/new!)   ;; each time you want to put work in a new file
  (mp/save!)  ;; to save work away
  (mp/record) ;; start recording midi events
  (mp/pause)  ;; pause recording midi events

  ;; listen to midi
  (def mpp (o/midi-poly-player (partial oss/ektara :gate 1)))
  (def mpp (o/midi-poly-player (partial piano/sampled-piano
                                        :gate 1 :play-buf-action o/NO-ACTION)))
  (def mpp (o/midi-poly-player (fn [& {:keys [note velocity]}]
                                 (let [pitch-index ((comp unMajor unC) note)
                                       new-note ((comp D mixolydian) pitch-index)]
                                   (piano/sampled-piano :note new-note
                                                        :velocity velocity
                                                        :gate 1
                                                        :play-buf-action o/NO-ACTION)))))
  ;; (o/midi-player-stop)
  
  ;; how many 'snippets' do you have?
  (count (mp/partition-by-timestamp (mp/get-list)))

  ;; play the first one
  (play (->>
         (get-melody 0 :piano)
         (times 2)))
  
  ;; play the first one, quantized at 92 bpm, played back faster 
  (play (->>
         (get-quant-melody 10 80 0.5 :ektara)
         (times 2)
         (canon (comp (simple 8) (interval 7)))
         (where :time (bpm 180))
         (where :duration (bpm 180))))

  ;; transpose this
  (play (->>
         (get-quant-melody 1 80 0.25 :piano)
         ;; change from C major to intervals
         (where :pitch (comp unMajor unC))
         (where :pitch (comp G phrygian))
         (times 2)
         (canon (comp (simple 8.5)
                      (partial where :part (is :cello))))
         (where :time (bpm 120))
         (where :duration (bpm 120))))

  (print-phrase (get-quant-melody 10 80 0.5))

  (def m1 (phrase [3 1 4  3 1 4  3 0.5 0.5  3 0.5 0.5  4.0]
                  [48 52 55 53 60 55 52 53 52 50 52 50 48] ))
  (play (->> m1
         (times 2)
         (canon (comp (simple 8) (interval 5)))
         (where :part (is :piano))
         (where :time (bpm 180))
         (where :duration (bpm 180))))
  
  (play (click-track (bpm 80) 8))
  (play (->>
         (get-quant-melody 3 92)
         (times 2)))

)
