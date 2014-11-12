(ns explore-overtone.fmsynth
  (:use [overtone.live]))

(defsynth fm0
  [note        60
   duration    0.3 ;; modify to use gate
   level       1.0
   attack      0.01
   release     0.01
   del-ms      4.0
   mod-ratio   3.0
   mod-amount  0.5
   mod-detune  0.0
   mod-attack  0.01
   mod-decay   0.1
   mod-sustain 0.2
   mod-release 0.01 ;; used?
   out-bus     0]
  (let [freq0 (midicps note)
        freq1 (+ (* mod-ratio freq0) mod-detune)
        sus1  (- duration mod-attack mod-release)
        env1  (env-gen (adsr mod-attack mod-decay mod-sustain mod-release))
        osc1  (* mod-amount env1 freq0 (sin-osc freq1))
        osc0  (* level (sin-osc (+ freq0 osc1)))
        sus0  (- duration attack release)
        env0  (env-gen (lin attack sus0 release) :action FREE)
        osc0  [osc0 (delay-l osc0 (/ del-ms 1000.0))]
        ]
    (out out-bus (* env0 osc0))))

;; this seems to sort-of match the bass at 6:44
;; FIXME--what is up with :del-ms?  AbstractMethodError?
(do (fm0 33 :level 1.5 :mod-amount 10)
    (fm0 (+ 12 33) :level 0.5 :mod-amount 10))


;; AbstractMethodError

(defsynth fm1
  [note        60
   duration    2.0 ;; modify to use gate
   level       1.0
   attack      0.75
   release     0.75
   del-ms      8.0
   mod-ratio   2.0
   mod-amount  1.0
   mod-detune  0.0 ;; in cents
   mod-attack  0.01
   mod-decay   0.1
   mod-sustain 0.2
   mod-release 0.01 ;; used?
   out-bus     0]
  (let [freq0 (midicps note)
        freq1 (* mod-ratio freq0 (pow 2 (/ mod-detune 1200)))
        sus1  (- duration mod-attack mod-release)
        env1  (env-gen (adsr mod-attack mod-decay mod-sustain mod-release))
        osc1  (* mod-amount env1 freq0 (sin-osc freq1))
        osc0  (* level (sin-osc (+ freq0 osc1)))
        sus0  (- duration attack release)
        env0  (env-gen (lin attack sus0 release) :action FREE)
        osc0  [osc0 (delay-l osc0 (/ del-ms 1000.0))]
        ]
    (out out-bus (* env0 osc0))))

(fm1 :note 57
     :del-ms 0
     :mod-amount 8.0
     :mod-detune -9
     :mod-attack 0.5 :mod-decay 0.5 :mod-sustain 0.8)
