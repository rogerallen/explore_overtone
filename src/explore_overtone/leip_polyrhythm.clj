(ns explore-overtone.leip-polyrhythm
  (:require [overtone.live   :as o]
            [leipzig.live    :as ll]
            [leipzig.melody  :as lm]))

(def snare       (o/sample (o/freesound-path 26903)))
(def kick        (o/sample (o/freesound-path 2086)))
(def close-hihat (o/sample (o/freesound-path 802)))
(def open-hihat  (o/sample (o/freesound-path 26657)))
(def clap        (o/sample (o/freesound-path 48310)))
(def gshake      (o/sample (o/freesound-path 113625)))

(defmethod ll/play-note :default [{p :pitch}]
  (case p
    :snr (snare)
    :kck (kick)
    :chh (close-hihat)
    :ohh (open-hihat)
    :clp (clap)
    :shk (gshake)
    nil))

(defn my-phrase
  [rhy pat bpm]
  (->> (lm/phrase rhy pat)
       (lm/where :time (lm/bpm bpm))
       (lm/where :duration (lm/bpm bpm))))

(defn poly-rhythm
  [p0 p1 bpm]
  (let [r0   (repeat (count p0) 1)
        r1   (repeat (count p1) 1)
        ;; scale tempo up so that both rhythms match up
        bpm1 (* bpm (/ (count r1) (count r0)))
        phr0 (my-phrase r0 p0 bpm)
        phr1 (my-phrase r1 p1 bpm1)]
    (->> phr0 (lm/with phr1))))

;; basic 3 vs. 4 poly rhythm
(def poly-beats
  (poly-rhythm [:chh :--- :chh :--- :chh :---]
               [:clp :--- :clp :--- :clp :--- :clp :---]
               (* 2 80)))

;; start the jam
(ll/jam poly-beats)

;; play with this to add some more interesting beats
(def poly-beats
  (poly-rhythm [:chh :kck :chh :ohh :chh :kck]
               [:clp :chh :clp :chh :clp :--- :clp :---]
               (* 2 80)))

;; to turn off the melody
(def poly-beats nil)
