(ns explore_overtone.guitar_examples)
(use 'overtone.live)
(require '[explore_overtone.guitar :as gtr])

;; ======================================================================
;; try out the guitar...
(def g (gtr/guitar))
(gtr/strum g :E :down 0.25)
(gtr/strum g :E :up 0.75)
(gtr/strum g :B :down 0.25)
(gtr/strum g :A :up 0.5)
;; bow down to the power chord!
(ctl g :pre-amp 4.0 :distort 0.99) 
(gtr/strum g [0 2 2 -1 -1 -1])
(gtr/strum g [3 5 5 -1 -1 -1])
;; mute all strings
(gtr/strum g [-1 -1 -1 -1 -1 -1])

;; ======================================================================
;; try out a bit of rhythmic accompanyment
;; http://www.youtube.com/watch?v=DV1ANPOYuH8
;; http://www.guitar.gg/strumming.html
;; FIXME -- these strumming patterns need a better treatment
(defn pat0 [metro cur-measure chord pattern]
  (let [cur-beat (* 4 cur-measure)]
    (doall
     (doseq [[b d] pattern]
       (gtr/strum g chord d 0.05 (metro (+ b cur-beat)))))))
(defn dduud [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [1.5 :up] [2.5 :up]
          [3.0 :down] ]))
(defn dduudu [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [1.5 :up] [2.5 :up]
          [3.0 :down]
          [3.5 :up] ]))
(defn ddudu [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [2.5 :up] [3.0 :down] [3.5 :up] ]))
(defn ddduduud [metro cur-measure chord]
  (pat0 metro cur-measure chord
        [ [0.0 :down] [1.0 :down]
          [2.0 :down] [2.25 :up] [2.5 :down] [2.75 :up]
          [3.25 :up] [3.5 :down]]))

;; now we play...
(ctl g :pre-amp 10.0 :amp 1.0 :distort 0.0)
(do ;; strumming practice
  (let [metro (metronome 100)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Gadd5 :Cadd9 :Cadd9
                                        :Dsus4 :Dsus4 :Gadd5 :Cadd9
                                        :Gadd5 :Cadd9])]
       (dduud metro i c))))
  )
(do ;; knocking on heaven's door
  (let [metro (metronome 100)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Am :Am
                                        :Gadd5 :Dsus4 :Cadd9 :Cadd9])]
       (dduudu metro i c))))
  )
(do ;; moar strumming practice
  (let [metro (metronome 180)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (ddudu metro i c))))
  )
(do ;; evan moar strumming practice
  (let [metro (metronome 90)
        now (metro)]
    (doall
     (doseq [[i c] (map-indexed vector [:Gadd5 :Cadd9 :Gadd5 :Cadd9])]
       (ddduduud metro i c))))
  )

;; ======================================================================
;; ac/dc's highway to hell intro.  turn it up! 
(defn ddd0 []
  (let [t (now) dt 250]
    (gtr/strum g [-1  0  2  2  2 -1] :down 0.01 (+ t (* 0 dt)))
    (gtr/strum g [-1  0  2  2  2 -1] :up   0.01 (+ t (* 1 dt)))
    (gtr/strum g [-1  0  2  2  2 -1] :down 0.01 (+ t (* 2 dt) 50))
    (gtr/strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 3.5 dt)))))
(defn ddd1 []
  (let [t (now) dt 250]
    (gtr/strum g [ 2 -1  0  2  3 -1] :down 0.01 (+ t (* 0 dt)))
    (gtr/strum g [ 2 -1  0  2  3 -1] :up   0.01 (+ t (* 1 dt)))
    (gtr/strum g [ 3 -1  0  0  3 -1] :down 0.01 (+ t (* 2 dt) 50))
    (gtr/strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 3.5 dt)))))
(defn ddd2 []
  (let [t (now) dt 250]
    (gtr/strum g [ 2 -1  0  2  3 -1] :down 0.01 (+ t (* 0 dt)))
    (gtr/strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 1.5 dt)))
    (gtr/strum g [-1  0  2  2  2 -1] :down 0.01 (+ t (* 2 dt)))
    (gtr/strum g [-1  0  2  2  2 -1] :up   0.01 (+ t (* 3 dt)))
    (gtr/strum g [-1 -1 -1 -1 -1 -1] :down 0.01 (+ t (* 4.5 dt)))))

(ctl g :pre-amp 5.0 :distort 0.96
     :lp-freq 5000 :lp-rq 0.25
     :rvb-mix 0.5 :rvb-room 0.7 :rvb-damp 0.4) 
(ddd0)
(ddd1) ;; repeat 3 times
(ddd2)
