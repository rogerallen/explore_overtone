(ns explore-overtone.synth-bus
  (:use [overtone.live]))

;; a stereo synth
(defsynth foo
  [obus 0 note 60 gate 1]
  (let [f (midicps note)]
    (out obus (* (sin-osc [f (+ f 5)])
                (env-gen (adsr 0.1 0.1 1.0 0.1)
                         :gate gate :action FREE)))))

;; you can hear the stereo effect here
(def a (foo))
(ctl a :gate 0)

;; a stereo connection  >--f2b-->
(def f2b (audio-bus 2))

;; the simplest output passthru synth
(defsynth bar
  [obus 0 ibus 1]
  (out obus (in ibus 2)))

;; place b at the end of f2b                >--f2b-->[b]
(def b (bar :position :tail :ibus f2b))
;; place f at the start of f2b           [f]>--f2b-->[b]
(def f (foo :position :head :obus f2b))
(ctl f :gate 0)

;; play midi into b                    [mpp]>--f2b-->[b]
(def mpp (midi-poly-player (partial foo :position :head :obus f2b)))
(midi-player-stop)
