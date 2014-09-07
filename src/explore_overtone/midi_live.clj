(ns explore-overtone.midi-live
  (:require [overtone.live :as o]
            [overtone.synth.stringed :as oss]
            [overtone.inst.synth :as osynth]
            [oversampler.piano.inst :as piano]
            [oversampler.cello.inst :as cello]
            [explore-overtone.midi-persi :as mp]))

(defn play-piano
  [t note dur]
  (let [s (o/at t (piano/sampled-piano note))]
    (o/at (+ t dur) (o/ctl s :gate 0))))

(defn play-ektara
  [t note dur]
  (let [s (o/at t (oss/ektara note 1))]
    (o/at (+ t dur) (o/ctl s :gate 0))))

(defn seq-synth
  [synth t0 notes]
  (when-not (empty? notes)
    (synth (+ t0 (:timestamp (first notes)))
                (:note (first notes))
                (:duration (first notes)))
    (when-not (empty? (rest notes))
      (o/apply-by (+ t0 (:timestamp (first (rest notes))))
                  #'seq-synth [synth t0 (rest notes)]))))

(comment

  ;; midi-persi interaction
  (mp/init!)  ;; once a session

  (mp/new!)   ;; each time you want to put work in a new file
  (mp/record) ;; start recording midi events
  (mp/pause)  ;; pause recording midi events
  (mp/summary) ;; what does the file look like?
  (mp/save!)  ;; to save work away
  (mp/open! "140907_145403.clj")

  ;; connect a synth to the midi player
  (def mpp (o/midi-poly-player (partial oss/ektara :gate 1)))
  (def mpp (o/midi-poly-player (partial piano/sampled-piano
                                        :gate 1 :play-buf-action o/NO-ACTION)))
  (o/midi-player-stop) ;; stop the midi when you want

  ;; partition into sequences & play each
  (def nl (mp/partition-by-timestamp (mp/get-list)))
  (count nl)
  (seq-synth play-piano (o/now) (mp/make-notes (last nl)))
  (seq-synth play-ektara (o/now) (mp/make-notes (nth nl 0)))

  ;; FIXME
  ;; add a way to loop a sequence

)
