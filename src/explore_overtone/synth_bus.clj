(ns explore-overtone.synth-bus
  (:use [overtone.live]))

;; a stereo synth
(defsynth foo
  [obus 0 note 60 gate 1]
  (let [f (midicps note)]
    (out obus (* (saw [f (+ f 3)])
                (env-gen (adsr 0.1 0.1 1.0 0.1)
                         :gate gate :action FREE)))))

;; lpf passthru synth
(defsynth bar
  [obus 0 ibus 1]
  (out obus (lpf (in ibus 2) 2000)))

;; hpf passthrough synth
(defsynth qux
  [obus 0 ibus 1]
  (out obus (hpf (in ibus 2) 1000)))

;; ----------------------------------------------------------------------
;; first no effects...you can hear the stereo effect here
(def a (foo))
(ctl a :gate 0)

;; ----------------------------------------------------------------------
;; one effect...lpf makes for 'duller' sound
(def f2b (audio-bus 2))

;; place f at the start of f2b           [f]>--f2b-->
;; because you add this first, you don't need to say :position :head
(def f (foo :obus f2b))
;; place b at the end of f2b             [f]>--f2b-->[b]
;; because you add this 2nd, you don't need to say :position :tail
(def b (bar :ibus f2b))
(ctl f :gate 0)

;; play midi into b                    [mpp]>--f2b-->[b]
;; this time, you need to give the position since the
;; bus would normally make it :tail
(def mpp (midi-poly-player (partial foo :position :head :obus f2b)))
(midi-player-stop)

;; ----------------------------------------------------------------------
;; two effects...lpf + hpf makes for a 'thin' sound
(def b0 (audio-bus 2))
(def b1 (audio-bus 2))

;; place [x] at between b0 and b1           >--b0-->[x]>--b1-->
;; explicitly makes it :tail on :ibus, implicitly :head on obus
(def x (bar :position :tail :ibus b0 :obus b1))

;; add [y] after [x]                        >--b0-->[x]>--b1-->[y]
(def y (qux :position :tail :ibus b1))

;; place [w] at the start of b0          [w]>--b0-->[x]>--b1-->[y]
(def w (foo :position :head :obus b0))
(ctl w :gate 0)

;; place [w] at the start of b0        [mpp]>--b0-->[x]>--b1-->[y]
(def mpp (midi-poly-player (partial foo :position :head :obus b0)))
(midi-player-stop)
