(ns explore_overtone.ukelele_examples)
(use 'overtone.live)
(require '[explore_overtone.ukelele :as uke])

;; ======================================================================
;; try out the ukelele...hmm, maybe needs a high-pass filter?
(def u (uke/ukelele :dur 3.0 :decay 10))
(uke/strum u [0 0 0 0] :down 2.0) ;; my dog has fleas!
(uke/strum u :E :down 0.25)
(uke/strum u :E :up 0.75)
(uke/strum u :B :down 0.25)
(uke/strum u :A :up 0.5)
