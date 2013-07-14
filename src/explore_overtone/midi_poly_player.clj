(ns explore-overtone.midi-poly-player
  (:use [overtone.live]))

;; define an inst to play with the midi keyboard

(definst steel-drum [note 60 amp 0.8]
  (let [freq (midicps note)]
    (* amp
       (env-gen (perc 0.01 0.2) 1 1 0 1 :action FREE)
       (+ (sin-osc (/ freq 2))
          (rlpf (saw freq) (* 1.1 freq) 0.4)))))

(def player (midi-poly-player steel-drum))

;; use this to stop the connection if you want to change sounds
(midi-player-stop)
