(ns explore-overtone.growl
  (:use [overtone.live]
        [overtone.gui.control]
        ;;(require 'overtone.gui.scope) ;; if you don't need a trigger
        [explore-overtone.scope]))

(comment
  ;; watch what's going on
  (scope :width 700 :height 500)
  )

(defsynth growl
  [freq {:default 80 :min 40 :max 800 :step 1}
   cutoff {:default 4 :min 1 :max 10 :step 0.1}
   lfo-freq {:default 20 :min 0.1 :max 40.0 :step 0.1}
   lfo-amp {:default 2000 :min 10 :max 4000 :step 1}
   amp {:default 1.0 :min 0.0 :max 1.0 :step 0.01}
   out-bus 0]
  (let [src  (saw freq)
        lfo  (+ 2 (lf-tri:kr lfo-freq))
        cf   (* freq cutoff)
        filt (lpf src (+ cf (* lfo-amp lfo)))]
    (out out-bus (* amp (pan2 filt)))))

(live-synth-controller growl)
