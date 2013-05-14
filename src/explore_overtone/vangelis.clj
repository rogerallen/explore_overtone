(ns explore-overtone.vangelis
  (:use [overtone.live]
        [explore-overtone.sawbble])
  (:require [shadertone.tone :as t]
            [leipzig.live    :as ll]
            [leipzig.melody  :as lm]))

(defn spb [tempo beats]
  (let [bps (/ tempo 60)]
    (/ beats bps)))
(defn bps [tempo beats]
  (/ 1.0 (spb beats tempo)))

(def the-bpm 72)
(def the-spb (partial spb the-bpm))
(def the-bps (partial bps the-bpm))

;; ======================================================================
(defsynth van-echo
  "stereo echo effect"
  [out-bus    0
   in-bus     2
   echo-level 1.0
   max-delay  8.0
   delay-time 0.4
   decay-time 2.0]
  (let [source (in in-bus 2)
        effect (comb-n source max-delay delay-time decay-time)
        effect (* echo-level effect)]
    (out out-bus (+ effect source))))

(defsynth van-reverb
  "stereo reverb effect"
  [out-bus       0
   in-bus        2
   roomsize      80     ;; in m^2 DO NOT CHANGE!
   revtime       4.85   ;; in sec
   damping       0.41   ;; high freq rolloff 0=totally, 1=not at all
   inputbw       0.19   ;; ditto, but on the input signal
   spread        1.0    ;; stereo spread and diffusion of the reverb
   drylevel      -3     ;; amount of dry signal (dB)
   earlyreflevel -9     ;; amount of early reflection level (dB)
   taillevel     -11    ;; amount of tail level (dB)
   moog-freq     1000.0 ;;
   moog-res      0.5    ;;
   reverb-level  1.0]
  (let [source (in in-bus 2)
        effect (g-verb source
                       roomsize revtime damping inputbw spread
                       drylevel earlyreflevel taillevel)
        effect (* reverb-level effect)
        effect (moog-ff effect moog-freq moog-res)]
    (out out-bus (+ effect source))))

;; from doc.sccode.org/Classes/GVerb.html
;; bathroom
;;   [\roomsize, 5, \revtime, 0.6, \damping, 0.62, \inputbw, 0.48,
;;    \drylevel -6, \earlylevel, -11, \taillevel, -13]);
;; living room
;;   [\roomsize, 16, \revtime, 1.24, \damping, 0.10, \inputbw, 0.95,
;;    \drylevel -3, \earlylevel, -15, \taillevel, -17]);
;; church
;;   [\roomsize, 80, \revtime, 4.85, \damping, 0.41, \inputbw, 0.19,
;;    \drylevel -3, \earlylevel, -9, \taillevel, -11]);
;; cathedral
;;   [\roomsize, 243, \revtime, 1, \damping, 0.1, \inputbw, 0.34,
;;    \drylevel -3, \earlylevel, -11, \taillevel, -9]);
;; canyon
;;   [\roomsize, 300, \revtime, 103, \damping, 0.43, \inputbw, 0.51,
;;    \drylevel -5, \earlylevel, -26, \taillevel, -20]);

;; ======================================================================
;; creating this dataflow
;; [v]>--b0-->[y]>--b1-->[x]
(def b0 (audio-bus 2))
(def b1 (audio-bus 2))

(def vangelis (partial
               sawbble-synth
               :position            :head
               :amp-konst           1.0
               :separation-delay-ms 8.0
               :separation-phase    -1
               :lpf-lfo-freq        (the-bps 1)
               :lpf-min-freq-ratio  20.0
               :lpf-max-freq-ratio  40.0
               :lpf-res             0.4
               :lfo-level-ratio     0.5
               :lfo-freq            (the-bps 1)
               :adsr-attack-time    (the-spb 0.5)
               :adsr-decay-time     (the-spb 0.25)
               :adsr-sustain-level  0.2
               :adsr-release-time   (the-spb 3)
               :adsr-peak-level     0.4
               :adsr-curve          4
               :out-bus             b0))

(def y (van-echo   :position :tail :in-bus b0 :out-bus b1))
(def x (van-reverb :position :tail :in-bus b1)) ;; put the lowpass filter at the end.  :^)
(ctl x
     :spread       10.5
     :reverb-level 0.3
     :moog-freq    1000.0
     :moog-res     0.4)
(ctl y
     :delay-time (the-spb 0.5)
     :decay-time (the-spb 2)
     :echo-level 0.3)

(def v (midi-poly-player vangelis))
(midi-player-stop)
